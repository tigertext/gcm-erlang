-module(gcm_request).

%% API
-export([send/2]).

-define(BASEURL, "https://android.googleapis.com/gcm/send").
-define(TIMEOUT, 3000). %% 3 seconds
-define(CONNECT_TIMEOUT, 1000). %% 1 seconds

send({RegIds, Message, Message_Id}, {Key, ErrorFun}) ->
    lager:info("Message=~p; RegIds=~p~n", [Message, RegIds]),
    GCMRequest = jsx:encode([{<<"registration_ids">>, RegIds}|Message]),
    ApiKey = string:concat("key=", Key),

    try httpc:request(post, {?BASEURL, [{"Authorization", ApiKey}], "application/json", GCMRequest}, [{timeout, ?TIMEOUT}, {connect_timeout, ?CONNECT_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _Headers, GCMResponse}} ->
            Json = jsx:decode(response_to_binary(GCMResponse)),
            {Multicast, Success, Failure, Canonical, Results} = get_response_fields(Json),
            Success =:= 1 andalso lager:info("Push sent success(RegIds=~p), message_id=~p,  multicast id=~p~n", [RegIds, Message_Id, Multicast]),
            case to_be_parsed(Failure, Canonical) of
                true ->
                    parse_results(Results, RegIds, ErrorFun);
                false -> false
            end;
        {error, Reason} ->
            %% Some general error during the request.
            lager:error("error in request: ~p~n", [Reason]),
            {error, Reason};
        {ok, {{_, 400, _}, _, _}} ->
            %% Some error in the Json.
            {http_error, 400};
        {ok, {{_, 401, _}, _, _}} ->
            %% Some error in the authorization.
            lager:error("authorization error!", []),
            {http_error, 401};
        {ok, {{_, Code, _}, _, _}} when Code >= 500 andalso Code =< 599 ->
            %% TODO: retry with exponential back-off
            {http_error, 500};
        {ok, {{StatusLine, _, _}, _, _Body}} ->
            %% Request handled but some error like timeout happened.
            {http_error, StatusLine};
        OtherError ->
            %% Some other nasty error.
            lager:error("other error: ~p~n", [OtherError]),
            {http_error, {nasty, OtherError}}
    catch
        Exception ->
            lager:error("exception ~p in call to URL: ~p~n", [Exception, ?BASEURL]),
            {http_error, {exception, Exception}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
response_to_binary(Json) when is_binary(Json) ->
    Json;

response_to_binary(Json) when is_list(Json) ->
    list_to_binary(Json).

get_response_fields(Json) ->
    {
        proplists:get_value(<<"multicast_id">>, Json),
        proplists:get_value(<<"success">>, Json),
        proplists:get_value(<<"failure">>, Json),
        proplists:get_value(<<"canonical_ids">>, Json),
        proplists:get_value(<<"results">>, Json)
    }.

to_be_parsed(0, 0) -> false;
to_be_parsed(_Failure, _Canonical) -> true.

parse_results([Result | Results], [RegId | RegIds], ErrorFun) ->
    case {
        proplists:get_value(<<"error">>, Result),
        proplists:get_value(<<"message_id">>, Result),
        proplists:get_value(<<"registration_id">>, Result)
    } of
        % First handle the normal successful response
        {_, MessageId, undefined} when MessageId =/= undefined ->
            lager:info("Message sent.~n", []),
            parse_results(Results, RegIds, ErrorFun);
        % Next is when there's a new registration_id
        {_, MessageId, NewRegId} when MessageId =/= undefined, NewRegId =/= undefined ->
            ErrorFun(<<"NewRegistrationId">>, {RegId, NewRegId}),
            parse_results(Results, RegIds, ErrorFun);
        % Then, there might be an error...
        {Error, _, _} when Error =/= undefined ->
            ErrorFun(Error, RegId),
            parse_results(Results, RegIds, ErrorFun);
        % And last but not least, let's not forget what we don't know yet...
        _ ->
            lager:warning("Invalid results for registration_id [~p]: ~p", [RegId, Result]),
            parse_results(Results, RegIds, ErrorFun)
    end;
parse_results([], [], _ErrorFun) ->
    ok.

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
