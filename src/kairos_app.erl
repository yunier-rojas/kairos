%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright (C) 2021, Yunier Rojas García
%%% @doc
%%% == Kairos ==
%%% Kairos is a very specific HTTP proxy. Its purpose is to allow APIs
%%% to migrate to webhook operations without introducing breaking changes
%%% to the public. It does that by transforming legacy requests into
%%% webhook operations and then waits and send the result when it is
%%% available.
%%%
%%% === Use case ===
%%% The main use case of this application is when a public API with
%%% normal synchronous operations (meaning the client makes a request
%%% and blocks until it get the result) introduce async operations based
%%% on webhooks. The process to deprecate the old operation can take a
%%% long time and maintaining both versions of the API can be costly.
%%%
%%% You can reduce the impact of this problem by introducing a Kairos
%%% server in front of the public API and serve the synchronous operations
%%% through Kairos by orchestrating the asynchronous endpoints.
%%%
%%% === Public API ===
%%% Kairos binds to port <code>8080</code> for all endpoints defined in
%%% the configuration file (See <code>kairos_config.erl</code>). Those
%%% endpoints should receive the public API call which will be transformed
%%% into calls to the async operations downstream and will wait for the
%%% result of those operation before returning the response to the public.
%%%
%%% === Internal API ===
%%% Kairos also binds to port <code>8081</code> exposing the endpoint
%%% <code>/callback/:uuid</code> which will receive the callback functions
%%% of the async operations downstream using webhooks. Whatever data is
%%% to this endpoint will be sent to waiting response with code
%%% <code>:uuid</code>.
%%%
%%% === Test API ===
%%% Kairos application includes a non-production endpoints to test the
%%% functionalities of Kairos. This testing server binds to port
%%% <code>8082</code> and expose the endpoint <code>/webhook</code>
%%% This endpoint receives a request with a header with the URL of the
%%% callback endpoint and calls the endpoint after some seconds.
%%%
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
