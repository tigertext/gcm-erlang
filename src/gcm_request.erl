-module(gcm_request).

%% API
-export([send/2, send_from_project/2]).

-define(BASEURL, "https://fcm.googleapis.com/fcm/send").
-define(PROJECT_BASEURL, "https://fcm.googleapis.com").
-define(PROJECT_SEND_METHOD, "messages:send").
-define(TIMEOUT, 6000). %% 6 seconds
-define(CONNECT_TIMEOUT, 3000). %% 3 seconds

send({RegIds, Message, Message_Id}, {Key, ErrorFun}) ->
    lager:info("Message=~p; RegIds=~p~n", [Message, RegIds]),
    Body = [{<<"registration_ids">>, RegIds}|Message],
    Headers = [{"Authorization", string:concat("key=", Key)}],

    case json_post_request(?BASEURL, Headers, Body) of
      {ok, Json} ->
        Multicast = proplists:get_value(<<"multicast_id">>, Json),
        Success = proplists:get_value(<<"success">>, Json),
        Failure = proplists:get_value(<<"failure">>, Json),
        Canonical = proplists:get_value(<<"canonical_ids">>, Json),
        Results = proplists:get_value(<<"results">>, Json),
        Success =:= 1 andalso lager:info("Push sent success(RegIds=~p), message_id=~p,  multicast id=~p~n", [RegIds, Message_Id, Multicast]),
        case {Failure, Canonical} of
            {0, 0} -> false;
            {_Any, _Any} -> parse_results(Results, RegIds, ErrorFun, Message)
        end;
      OtherError -> OtherError
    end.

send_from_project({ProjectId, Auth, RegIds, Message}, {_Key, ErrorFun}) ->
    Url = build_project_url(ProjectId, ?PROJECT_SEND_METHOD),
    Data = proplists:get_value(<<"data">>, Message),
    NewData = [{filter(K), filter(V)} || {K, V} <- Data],
    TtlList =
        case proplists:get_value(<<"time_to_live">>, Message) of
            undefined ->
                [];
            _ ->
                [{<<"ttl">>, <<"86400s">>}]
        end,

    PriorityList = case proplists:get_value(<<"priority">>, Message) of
                    undefined ->
                        [];
                    Priority ->
                        [{<<"priority">>, Priority}]
                end,
    Android = [{<<"android">>, TtlList ++ PriorityList}],

    lager:info("[WIP] FCM Project sending push: Url=~p \n Data=~p \n Android=~p \n RegIds=~p", [Url, NewData, Android, RegIds]),
    [
        begin
            Body =[{<<"message">>, [{<<"token">>, RegId}, {<<"data">>, NewData}] ++ Android}],
            Headers = [{"Authorization", string:concat("Bearer ", binary_to_list(Auth))}],

            case json_post_request(Url, Headers, Body) of
                {ok, Json} ->
                    lager:info("FCM Project push sent: ~p~n", [Json]),
                    ok;
                {http_error, Code} = OtherError when Code >= 400 andalso Code =< 499 ->
                    lager:info("FCM Project push sent failed: ~p~n", [OtherError]),
                    OK;
                    %ErrorFun(<<"InvalidRegistration">>, RegId, Message);
                {http_error, Code} = OtherError when Code >= 500 andalso Code =< 599 ->
                    lager:info("FCM Project push sent failed: ~p~n", [OtherError]),
                    ErrorFun(<<"Unavailable">>, RegId, Message);
                OtherError ->
                    lager:info("FCM Project push sent failed: ~p~n", [OtherError]),
                    OtherError
            end
        end || RegId <- RegIds].

%%%===================================================================
%%% Internal functions
%%%===================================================================
json_post_request(BaseUrl, Headers, Body) ->
  Payload = jsx:encode(Body),
  Options = [{timeout, ?TIMEOUT}, {connect_timeout, ?CONNECT_TIMEOUT}],
  try httpc:request(post, {BaseUrl, Headers, "application/json", Payload}, Options, []) of
    {error, Reason} ->
      lager:error("error in request: ~p~n", [Reason]),
      {error, Reason};
    {ok, {{_, 200, _}, _Headers, Response}} ->
      {ok, jsx:decode(response_to_binary(Response))};
    {ok, {{_, Code, _}, _, _}} when Code >= 500 andalso Code =< 599 ->
      lager:error("Server error: ~p~n", [Code]),
      {http_error, Code};
    {ok, {{_, Code, _}, _, _}} when Code >= 400 andalso Code =< 499 ->
      lager:warning("Client error: ~p~n", [Code]),
      {http_error, Code};
    {ok, {{_, Code, _}, _, _}} ->
      lager:warning("Unknown error: ~p~n", [Code]),
      {http_error, Code};
    OtherError ->
      lager:error("Other error: ~p~n", [OtherError]),
      {http_error, OtherError}
  catch
    Exception ->
      lager:error("Exception error: ~p in call to URL: ~p~n", [Exception, BaseUrl]),
      {http_error, {exception, Exception}}
  end.

response_to_binary(Json) when is_binary(Json) -> Json;
response_to_binary(Json) when is_list(Json) -> list_to_binary(Json).

parse_results([Result | Results], [RegId | RegIds], ErrorFun, Message) ->
    case {
        proplists:get_value(<<"error">>, Result),
        proplists:get_value(<<"message_id">>, Result),
        proplists:get_value(<<"registration_id">>, Result)
    } of
        % First handle the normal successful response
        {_, MessageId, undefined} when MessageId =/= undefined ->
            lager:info("Message sent.~n", []),
            parse_results(Results, RegIds, ErrorFun, Message);
        % Next is when there's a new registration_id
        {_, MessageId, NewRegId} when MessageId =/= undefined, NewRegId =/= undefined ->
            ErrorFun(<<"NewRegistrationId">>, {RegId, NewRegId}, Message),
            parse_results(Results, RegIds, ErrorFun, Message);
        % Then, there might be an error...
        {Error, _, _} when Error =/= undefined ->
            ErrorFun(Error, RegId, Message),
            parse_results(Results, RegIds, ErrorFun, Message);
        % And last but not least, let's not forget what we don't know yet...
        _ ->
            lager:warning("Invalid results for registration_id [~p]: ~p", [RegId, Result]),
            parse_results(Results, RegIds, ErrorFun, Message)
    end;
parse_results([], [], _ErrorFun, _Message) ->
    ok.

build_project_url(ProjectId, Method) ->
    ?PROJECT_BASEURL ++ "/v1/projects/" ++ ProjectId ++ "/" ++ Method.

filter(V) when is_binary(V) ->
    V;
filter(V) when is_list(V) ->
    list_to_binary(V);
filter(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
filter(V) when is_integer(V) ->
    integer_to_binary(V);
filter(V) ->
    <<"">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Other possible errors:					%%
%%	<<"InvalidPackageName">>				%%
%%      <<"MissingRegistration">>				%%
%%	<<"MismatchSenderId">>					%%
%%	<<"MessageTooBig">>					%%
%%      <<"InvalidDataKey">>					%%
%%	<<"InvalidTtl">>					%%
%%								%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
