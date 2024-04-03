%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2013, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(gcm).

-behaviour(gen_server).

%% API
-export([start/2, start/3, stop/1, start_link/2, start_link/3, push/3, push/4, push_from_project/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(BASEURL, "https://fcm.googleapis.com/fcm/send").
-define(GCM_TYPE, "fcm").

-record(state, {key, retry_after, error_fun}).

%%%===================================================================
%%% API
%%%===================================================================
start(Name, Key) ->
    start(Name, Key, fun handle_error/3).

start(Name, Key, ErrorFun) ->
    gcm_sup:start_child(Name, Key, ErrorFun).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Key) ->
    start_link(Name, Key, fun handle_error/3).

start_link(Name, Key, ErrorFun) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key, ErrorFun], []).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegIds, Message, Message_Id) ->
    ok = gen_server:call(Name, {send, RegIds, Message, Message_Id}).
push(Name, RegIds, Message) ->
    ok = gen_server:call(Name, {send, RegIds, Message, undefined}).

push_from_project(Name, ProjectId, RegIds, Message) ->
    ok = gen_server:call(Name, {send_from_project, ProjectId, RegIds, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Key, ErrorFun]) ->
    {ok, #state{key=Key, retry_after=0, error_fun=ErrorFun}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegIds, Message, Message_Id}, _From, #state{key=Key, error_fun=ErrorFun} = State) ->
    ok = cxy_ctl:execute_task(gcm, gcm_request, send, [{RegIds, Message, Message_Id}, {Key, ErrorFun}]),
    {reply, ok, State};

handle_call({send_from_project, ProjectId, RegIds, Message}, _From, #state{key=Key, error_fun=ErrorFun} = State) ->
    ok = cxy_ctl:execute_task(gcm, gcm_request, send_from_project, [{ProjectId, RegIds, Message}, {Key, ErrorFun}]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_error(<<"NewRegistrationId">>, {RegId, NewRegId}, Message) ->
    handle_error_generic(token_update, [?GCM_TYPE, RegId, NewRegId, Message]);

handle_error(Error, RegId, Message) when Error =:= <<"InvalidRegistration">>; Error =:= <<"NotRegistered">> ->
    % Invalid registration id in database.
    handle_error_generic(token_error, [?GCM_TYPE, RegId, Message]);

handle_error(<<"Unavailable">>, RegId, _Message) ->
    % The server couldn't process the request in time. Retry later with exponential backoff.
    lager:error("unavailable ~p~n", [RegId]),
    ok;

handle_error(<<"InternalServerError">>, RegId, _Message) ->
    % GCM had an internal server error. Retry later with exponential backoff.
    lager:error("internal server error ~p~n", [RegId]),
    ok;

handle_error(UnexpectedError, RegId, _Message) ->
    % There was an unexpected error that couldn't be identified.
    lager:error("unexpected error ~p in ~p~n", [UnexpectedError, RegId]),
    ok.

handle_error_generic(Function, Args) ->
    case application:get_env(gcm, feedback) of
        {ok, Funcs} ->
            case proplists:get_value(Function, Funcs) of
                {Mod, Func} ->
                    ok = erlang:apply(Mod, Func, Args);
                _ ->
                    no_push_feedback_channel
            end;
        _ ->
            no_push_feedback_channel
    end.
