-module(gcm_request).

%% API
-export([send/2, send_from_project/2]).

%% test
-export([json_post_request/3, filter/1]).
-export([build_analytics_label/2, get_msg_type/1]).

-define(BASEURL, "https://fcm.googleapis.com/fcm/send").
-define(PROJECT_BASEURL, "https://fcm.googleapis.com").
-define(PROJECT_SEND_METHOD, "messages:send").
-define(ANALYTICS_LABEL, "gcm_erl_send").
-define(TIMEOUT, 6000). %% 6 seconds
-define(CONNECT_TIMEOUT, 3000). %% 3 seconds

send({RegIds, Message, Message_Id}, {Key, ErrorFun}) ->
    try
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
                {_, _} -> parse_results(Results, RegIds, ErrorFun, Message)
            end;
        OtherError -> OtherError
        end
    catch
        Error:Reason:StackTrace ->
            lager:error("Unexcepted exception, ~p:~p, function:~p, stack:~p", [Error, Reason, ?FUNCTION_NAME, StackTrace])
        end.

send_from_project({ProjectId, Auth, RegIds, Message}, {_Key, ErrorFun}) ->
    Data = proplists:get_value(<<"data">>, Message),
    SenderId = proplists:get_value(<<"sender_id">>, Data),
    ReceiverId = proplists:get_value(<<"receiver_id">>, Data),
    ResourceId = proplists:get_value(<<"resource_id">>, Data),
    try
        Url = build_project_url(ProjectId, ?PROJECT_SEND_METHOD),
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
        AnalyticsLabel = build_analytics_label(get_msg_type(Message), ?ANALYTICS_LABEL),
        Android = [{<<"android">>, TtlList ++ PriorityList ++ AnalyticsLabel}],

        lager:info("[WIP] FCM Project sending push: Url=~p \n Data=~p \n Android=~p \n RegIds=~p", [Url, NewData, Android, RegIds]),
        [
            begin
                Body =[{<<"message">>, [{<<"token">>, RegId}, {<<"data">>, NewData}] ++ Android}],
                Headers = [{"Authorization", string:concat("Bearer ", binary_to_list(Auth))}],

                case json_post_request(Url, Headers, Body) of
                    {ok, Json} ->
                        lager:info([{sender_id, SenderId}, {account_token, ReceiverId}, {resource_token, ResourceId}, {status_code, 200}],
                                   "FCM Project push sent: ~p~n", [Json]),
                        ok;
                    {http_error, Code} = OtherError when Code == 403 orelse Code == 404 ->
                        lager:info([{sender_id, SenderId}, {account_token, ReceiverId}, {resource_token, ResourceId}, {status_code, Code}], 
                                    "FCM Project push sent failed: ~p~n", [OtherError]),
                        spawn(fun() -> ErrorFun(<<"InvalidRegistration">>, RegId, Message) end),
                        ok;
                    {http_error, Code} = OtherError when Code >= 400 andalso Code =< 499 ->
                        lager:info([{sender_id, SenderId}, {account_token, ReceiverId}, {resource_token, ResourceId}, {status_code, Code}], 
                                     "FCM Project push sent failed: ~p~n", [OtherError]),
                        ok;
                    {http_error, Code} = OtherError when Code >= 500 andalso Code =< 599 ->
                        lager:info([{sender_id, SenderId}, {account_token, ReceiverId}, {resource_token, ResourceId}, {status_code, Code}], 
                                   "FCM Project push sent failed: ~p~n", [OtherError]),
                        spawn(fun() -> ErrorFun(<<"Unavailable">>, RegId, Message) end),
                        error;
                    OtherError ->
                        lager:info([{sender_id, SenderId}, {account_token, ReceiverId}, {resource_token, ResourceId}], 
                                   "FCM Project push sent failed: ~p~n", [OtherError]),
                        error
                end
            end || RegId <- RegIds]
    catch
        Error:Reason:StackTrace ->
            lager:error([{sender_id, SenderId}, {account_token, ReceiverId}, {resource_token, ResourceId}], "Unexcepted exception, ~p:~p, function:~p, stack:~p", [Error, Reason, ?FUNCTION_NAME, StackTrace]),
            [error]
        end.

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
    {ok, {{_, Code, _}, _, Response}} when Code >= 500 andalso Code =< 599 ->
      lager:error("Server error: ~p;Response:~p~n", [Code, Response]),
      {http_error, Code};
    {ok, {{_, Code, _}, _, Response}} when Code >= 400 andalso Code =< 499 ->
      lager:warning("Client error: ~p;Response:~p~n", [Code, Response]),
      {http_error, Code};
    {ok, {{_, Code, _}, _, Response}} ->
      lager:warning("Unknown error: ~p;Response:~p~n", [Code, Response]),
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
    try 
        list_to_binary(V)
    catch
        _:_ -> 
            jsx:encode(V)
    end;
filter(V) when is_atom(V) ->
    list_to_binary(atom_to_list(V));
filter(V) when is_integer(V) ->
    integer_to_binary(V);
filter(V) when is_map(V) ->
    maps:map(fun(_K,V1)->filter(V1)end, V);
filter(_V) ->
    <<"">>.

-spec build_analytics_label(binary() | list(), binary()) -> binary().
build_analytics_label(MsgType, Prefix) when is_list(MsgType) ->
    build_analytics_label(list_to_binary(MsgType), Prefix);
build_analytics_label(MsgType, Prefix) when is_binary(MsgType), is_binary(Prefix) ->
    Label = <<Prefix/binary, "%", MsgType/binary>>,
    [{<<"analytics_label">>, Label}].

-spec get_msg_type(list()) -> binary() | list().
get_msg_type(Message) when is_list(Message) ->
    case proplists:get_value(<<"type">>, Message, proplists:get_value(<<"msg_type">>, Message)) of
        undefined -> <<"undefined">>;
        Type -> Type
    end.

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
