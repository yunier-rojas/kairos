%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright (C) 2021, Yunier Rojas García
%%% @doc
%%%
%%% @end
%%% Created : 19. Dec 2021 20:43
%%%-------------------------------------------------------------------

-module(kairos_sup).

-behaviour(supervisor).

%%%===================================================================
%%% API
%%%===================================================================

-export([start_link/0]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
-spec init(Args :: term()) ->
    {ok, {SupFlags :: supervisor:sup_flags(), [ChildSpec :: supervisor:child_spec()]}}
    | ignore.

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 10, 10}, Procs}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
