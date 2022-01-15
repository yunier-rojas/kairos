%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Cowboy handler to intercept and resolve public API requests. So this
%%% module will handle incoming HTTP requests from public clients to the
%%% legacy API and will translate them to the new async API.
%%% @end
%%% Created : 11. Dec 2021 15:23
%%%-------------------------------------------------------------------
-module(kairos_async_webhook_handler).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behavior(cowboy_handler).

%% NOTE: cowboy_handler behavior type specification raises warning
-dialyzer(no_undefined_callbacks).

%%%===================================================================
%%% cowboy_handler
%%%===================================================================

-export([init/2]).

%%%===================================================================
%%% cowboy_handler callbacks
%%%===================================================================

%% @private
%% @doc
%% HTTP handler for the incoming public API requests. `Req0' is the request
%% data collected by cowboy server and delegated to this handler. `State'
%% is information retrieved from the configuration with information about
%% how to transform the incoming request into a downstream call to the new
%% async operation. This handler registers a new request that is waiting for
%% a result, makes a request to the downstream server and waits for the result
%% to be ready for the client.
%% @end
-spec init(Req, State) ->
    {ok | module(), Req, any()}
    | {module(), Req, any(), any()}
when
    Req :: cowboy_req:req(),
    State :: list({atom(), any()}).

init(Req0, State) ->
    UUID = kairos_utils:gen_uuid(),
    ok = kairos_result:register(UUID),
    spawn_link(fun() -> kairos_downstream:make_request(Req0, UUID, State) end),
    {ok, {Payload, Headers}} = kairos_result:wait_for(UUID),
    Req = cowboy_req:reply(
        200,
        Headers,
        Payload,
        Req0
    ),
    {ok, Req, State}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-endif.
