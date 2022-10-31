%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Set of functions that allow a process to register itself as a global
%%% process under a UUID and block until a result is sent to it.
%%% @end
%%% Created : 13. Dec 2021 23:27
%%%-------------------------------------------------------------------
-module(kairos_result).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-export([register/1]).
-export([wait_for/1]).
-export([send_to/2]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Registers a process to the given UUID so other processes can send the
%% result the result for the UUID.
%% @end
-spec register(kairos_utils:kairos_uuid()) ->
    ok | {error, Reason :: term()}.

register(UUID) ->
    syn:register(requests, UUID, self()).

%% @doc
%% Blocks the process until a result for the given UUID is sent to the process.
%% @end
-spec wait_for(kairos_utils:kairos_uuid()) ->
    {ok, any()}.

wait_for(UUID) ->
    receive
        {_Pid, UUID, Data} ->
            io:format("Received Data: ~p~n", [Data]),
            {ok, Data};
        Msg ->
            io:format("Ignoring: ~p~n", [Msg]),
            wait_for(UUID)
    end.

%% @doc
%% Sends any data to the process registered under the given UUID.
%% @end
-spec send_to(kairos_utils:kairos_uuid(), any()) ->
    ok.

send_to(UUID, Data) ->
    {Pid, _Meta} = syn:lookup(requests, UUID),
    Pid ! {self(), UUID, Data},
    ok.

%%%===================================================================
%%% Internals
%%%===================================================================

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

results_sent_to_owner_should_be_received_test() ->
    UUID = "ABC123",
    PID = self(),
    meck:new(syn),
    meck:expect(syn, register, fun(_, _, _) -> ok end),
    meck:expect(syn, lookup, fun(_, _) -> {PID, []} end),

    register(UUID),
    timer:apply_after(1000, ?MODULE, send_to, [UUID, ok]),
    {ok, Result} = wait_for(UUID),
    ?assertEqual(ok, Result),
    ?assert(meck:validate(syn)),
    meck:unload(syn).

-endif.
