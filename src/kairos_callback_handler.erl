%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Cowboy handler that accepts webhooks callbacks from the real async
%%% public API and sends the result to waiting legacy requests. So this
%%% module will handle incoming HTTP requests from the API and deliver
%%% the result to the waiting client.
%%% @end
%%% Created : 11. Dec 2021 15:23
%%%-------------------------------------------------------------------
-module(kairos_callback_handler).

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
%% data collected by cowboy server and delegated to this handler. This handler
%% receives a request from the async API with the result of the previous
%% request and send it to the waiting request so it can be delivered to the
%% client.
%% @end

-spec init(Req, any()) ->
    {ok | module(), Req, any()}
    | {module(), Req, any(), any()}
when
    Req :: cowboy_req:req().

init(Req0, State) ->
    {ok, Payload} = kairos_utils:read_body(Req0),
    logger:notice("Received payload: ~p~n", [Payload]),
    UUID = binary_to_list(cowboy_req:binding(uuid, Req0)),
    logger:notice("Received UUID: ~p~n", [UUID]),
    kairos_result:send_to(UUID, {Payload, cowboy_req:headers(Req0)}),
    Req = cowboy_req:reply(
        204,
        Req0
    ),
    {ok, Req, State}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-endif.
