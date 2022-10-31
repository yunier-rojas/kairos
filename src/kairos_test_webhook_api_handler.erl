%%%-------------------------------------------------------------------
%%% @author Yunier Rojas García
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Cowboy handler that simulate a async HTTP operation that deliver the
%%% result via webhook to a callback URL. So this module will handle
%%% incoming HTTP requests from Kairos interceptor endpoint and make a
%%% request to the callback URL seconds later. This way it mimics async
%%% operations that could potentially block the server for several seconds.
%%% @end
%%% Created : 11. Dec 2021 15:23
%%%-------------------------------------------------------------------
-module(kairos_test_webhook_api_handler).

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
%% HTTP handler for the incoming Kairos intercepted requests. `Req0' is the
%% request data collected by cowboy server and delegated to this handler. This
%% handler receives a request from the Kairos as the client may do if it were
%% making the request directly to this endpoint. It returns successfully but
%% seconds later, it makes a request to the callback function to deliver the
%% same payload it received.
%% @end

-spec init(Req, any()) ->
    {ok | module(), Req, any()}
    | {module(), Req, any(), any()}
when
    Req :: cowboy_req:req().

init(Req0, State) ->
    HeaderCallback = cowboy_req:header(<<"x-webhook-url">>, Req0),
    {ok, Payload} = kairos_utils:read_body(Req0),
    io:format("Callback: ~p~n", [HeaderCallback]),
    Req = cowboy_req:reply(
        202,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Ok">>,
        Req0
    ),
    timer:apply_after(1000, hackney, request, [post, HeaderCallback, [], Payload, []]),
    {ok, Req, State}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-endif.
