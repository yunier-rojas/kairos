%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright (C) 2021, Yunier Rojas García
%%% @doc
%%% == Kairos HTTP Proxy Documentation ==
%%%
%%% === Introduction ===
%%% Kairos is a specialized HTTP proxy designed to facilitate the seamless transition of APIs from synchronous to
%%% asynchronous operations using webhooks, all while avoiding disruptive changes for the public.
%%%
%%% === Use Case ===
%%% The primary use case for Kairos arises when a public API initially relies on synchronous operations, where the
%%% client sends a request and waits for a response. Transitioning to asynchronous operations based on webhooks can
%%% be a lengthy process, and maintaining both versions of the API can be costly. Kairos mitigates these challenges
%%% by acting as an intermediary server that orchestrates the asynchronous endpoints, reducing the impact of the
%%% transition.
%%%
%%% === Public API ===
%%% Kairos binds to port 8080 for all endpoints defined in the configuration file (refer to kairos_config.erl).
%%% These endpoints should receive requests from the public API, which Kairos transforms into calls to the downstream
%%% asynchronous operations. Kairos then waits for the results of these operations before returning the response
%%% to the public.

%%% === Internal API ===
%%% Kairos also binds to port 8081, exposing the endpoint /callback/:uuid. This endpoint receives callback functions
%%% from the downstream asynchronous operations via webhooks. Any data sent to this endpoint will be forwarded to the
%%% waiting response associated with the corresponding :uuid.

%%% === Test API ===
%%% Kairos includes non-production endpoints for testing its functionalities. The testing server binds to port 8082
%%% and exposes the /webhook endpoint. This endpoint accepts a request with a header containing the URL of the callback
%%% endpoint and invokes the specified endpoint after a specified delay.

%%%
%%% @end
%%% Created : 11. Dec 2021 15:23
%%%-------------------------------------------------------------------
-module(kairos_app).

-behaviour(application).

%%%===================================================================
%%% API
%%%===================================================================

-export([start/2]).
-export([stop/1]).

%%%===================================================================
%%% Definitions
%%%===================================================================

-ifndef(PROD).
-define(DEF_DEV, dev_endpoint).
-else.
-define(DEF_DEV, do_nothing).
-endif.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. It also starts the top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(
    StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()
) ->
    {ok, pid()}
    | {ok, pid(), State :: term()}
    | {error, Reason :: term()}.

start(_Type, _Args) ->
    syn:add_node_to_scopes([requests]),
    PublicDispatch = cowboy_router:compile([
        {'_', kairos_config:get_routes()}
    ]),
    {ok, _} = cowboy:start_clear(
        public_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => PublicDispatch}}
    ),

    InternalDispatch = cowboy_router:compile([
        {'_', [{"/callback/:uuid", kairos_callback_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(
        internal_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => InternalDispatch}}
    ),

    % this is for development only
    ?DEF_DEV(),

    kairos_sup:start_link().

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(State :: term()) -> term().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifndef(PROD).
%%%===================================================================
%%% Development & testing functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function configures a test endpoint as proof of concept for webhook
%% based APIs with async operations.
%% @end
%%--------------------------------------------------------------------
-spec dev_endpoint() -> term().

dev_endpoint() ->
    TestDispatch = cowboy_router:compile([
        {'_', [
            {"/api/webhook/test", kairos_test_webhook_api_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(
        downstream_test_listener,
        [{port, 8082}],
        #{env => #{dispatch => TestDispatch}}
    ),
    ok.

-endif.

-ifdef(PROD).
%%%===================================================================
%%% Production functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function mocks dev_endpoint in production environment so no test
%% endpoints are exposed.
%% @end
%%--------------------------------------------------------------------
-spec do_nothing() -> term().

do_nothing() ->
    ok.

-endif.
