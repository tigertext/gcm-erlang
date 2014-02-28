%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2013, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(gcm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

start_child(Name, ApiKey, ErrorFun) ->
    supervisor:start_child(?MODULE, [Name, ApiKey, ErrorFun]).

init([]) ->
    init_concurrency_limits(get_concurrency_limits()),
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(gcm, worker)]} }.

get_concurrency_limits() ->
    Config_Concurrency_Limits = application:get_env(gcm, cxy_limits, []),
    Reqd_Set = sets:from_list([gcm]),
    Specified_Set = sets:from_list([Type || {Type, _, _} <- Config_Concurrency_Limits]),
    case sets:is_subset(Reqd_Set, Specified_Set) of
        false -> Missing_Limits = sets:to_list(sets:subtract(Reqd_Set, Specified_Set)),
                 lager:error("Missing concurrency configuration limits for ~p", [Missing_Limits]),
                 [];
        true  -> Config_Concurrency_Limits
    end.

init_concurrency_limits(Concurrency_Limits) ->
    true = cxy_ctl:init(Concurrency_Limits),
    Limits = [get_cxy_props(Props) || Props <- cxy_ctl:concurrency_types()],
    lager:info("cxy_ctl limits set ~p", [Limits]),
    ok.

get_cxy_props(Props) -> [proplists:get_value(P, Props) || P <- [task_type, max_procs, max_history]].
