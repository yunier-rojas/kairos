%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Load and facilitate Kairos configurations. Settings will be loaded
%%% from a file which filename can be declared in `KAIROS_CONFIG_FILE'
%%% environment variable. The default filename is `kairos.config' and
%%% should be available in the current path. An example of possible
%%% configuration can be found in at `kairos.config.example'.
%%% @end
%%% Created : 06. Dec 2021 22:29
%%%-------------------------------------------------------------------
-module(kairos_config).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

-export([get_routes/0]).

%%%===================================================================
%%% Definitions
%%%===================================================================

-type kairos_handler() :: module().

-type kairos_handler_options() :: list({atom(), any()}).
-export_type([kairos_handler_options/0]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Provides information about public endpoints that need to be intercepted
%% and downstream and callback configurations.
%% @end

-spec get_routes() ->
    list({cowboy_router:route_match(), kairos_handler(), kairos_handler_options()}).

get_routes() ->
    ReadConfigFile = read_config_file(),
    {ok, Config} = ReadConfigFile,
    {routes, Routes} = lists:keyfind(routes, 1, Config),
    Routes.

%%%===================================================================
%%% Internals
%%%===================================================================

%% @private
%% @doc
%% Reads and load the configuration file specified by `KAIROS_CONFIG_FILE'
%% environment variable.
%% @end

-spec read_config_file() ->
    {ok, term()}.

read_config_file() ->
    FileName = os:getenv("KAIROS_CONFIG_FILE", "kairos.config"),
    file:consult(FileName).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

get_routes_should_return_routes_from_config_test() ->
    meck:new(file, [unstick]),
    Routes = [{'_', kairos_async_status_handler, []}],
    meck:expect(file, consult, fun(_) -> {ok, [{routes, Routes}]} end),
    ConfigRoutes = get_routes(),
    ?assertEqual(ConfigRoutes, Routes),
    ?assert(meck:validate(file)),
    meck:unload(file).

-endif.
