%%%-------------------------------------------------------------------
%%% @author yunier
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Wrapper around an HTTP client that transform an incoming request to
%%% to legacy operations into new async operations. This module expose
%%% tools to propagate the request to downstream HTTP server using
%%% configuration parameters for each endpoint.
%%% @end
%%% Created : 13. Dec 2021 22:02
%%%-------------------------------------------------------------------
-module(kairos_downstream).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================
-export([make_request/3]).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc
%% Wrapper around an HTTP client that transform an incoming request to
%% to legacy operations into new async operations. This module expose
%% tools to propagate the request to downstream HTTP server using
%% configuration parameters for each endpoint.
%% @end
-spec make_request(Req, kairos_utils:kairos_uuid(), kairos_config:kairos_handler_options()) ->
    {ok, {integer(), list(), binary()}}
when
    Req :: cowboy_req:req().

make_request(Req, UUID, ProxyOptions) ->
    {ok, {Method, Url, Headers, Payload, Options}} = build_downstream_request(
        Req, UUID, ProxyOptions
    ),

    {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(
        Method,
        Url,
        Headers,
        Payload,
        Options
    ),
    {ok, Body} = hackney:body(ClientRef),
    {ok, {StatusCode, RespHeaders, Body}}.

%%%===================================================================
%%% Internals
%%%===================================================================

%% @private
%% @doc
%% Builds the downstream URL from the incoming request URL. It uses
%% `Config' to override parts of the URL.
%% @end
-spec build_url(Req, map()) ->
    binary()
when
    Req :: cowboy_req:req().

build_url(Req, Config) ->
    iolist_to_binary(cowboy_req:uri(Req, Config)).

%% @private
%% @doc
%% Extract downstream options from the handler state and builds the downstream
%% URL from the incoming request URL.
%% @end
-spec build_downstream_url(Req, kairos_config:kairos_handler_options()) ->
    binary()
when
    Req :: cowboy_req:req().

build_downstream_url(Req, ProxyOptions) ->
    {downstream_url, DownOptions} = lists:keyfind(downstream_url, 1, ProxyOptions),
    build_url(Req, maps:from_list(DownOptions)).

%% @private
%% @doc
%% Extract callback options from the handler state and builds the callback URL
%% from the incoming request URL. This URL will be used to receive the result
%% of the webhook.
%% @end
-spec build_callback_url(Req, iolist(), kairos_config:kairos_handler_options()) ->
    binary()
when
    Req :: cowboy_req:req().

build_callback_url(Req, UUID, ProxyOptions) ->
    {callback_url, CallOptions} = lists:keyfind(callback_url, 1, ProxyOptions),
    binary:replace(build_url(Req, maps:from_list(CallOptions)), <<":uuid">>, list_to_binary(UUID)).

%% @private
%% @doc
%% Installs the callback URL into the downstream request.
%% @end
-spec install_callback_on_request(
    binary(), binary(), list({binary(), binary()}), binary(), tuple()
) ->
    {binary(), list({binary(), iodata()}), binary()}.

install_callback_on_request(
    DownUrl,
    CallbackUrl,
    IncomingHeaders,
    IncomingPayload,
    {header, WebhookHeader}
) ->
    io:format("Webhook installed on header: ~p~n", [WebhookHeader]),
    {DownUrl, [{list_to_binary(WebhookHeader), CallbackUrl} | IncomingHeaders], IncomingPayload}.

%% @private
%% @doc
%% Extract webhook options from the handler state and prepares the downstream request to the async operation.
%% @end
-spec prepare_request_with_callback(
    Req, binary(), binary(), kairos_config:kairos_handler_options()
) ->
    {binary(), list({binary(), iolist()}), binary()}
when
    Req :: cowboy_req:req().

prepare_request_with_callback(Req, DownUrl, CallbackUrl, ProxyOptions) ->
    {downstream_webhook, WebhookOptions} = lists:keyfind(downstream_webhook, 1, ProxyOptions),
    IncomingHeaders = maps:to_list(cowboy_req:headers(Req)),
    {ok, IncomingPayload} = kairos_utils:read_body(Req),
    install_callback_on_request(
        DownUrl, CallbackUrl, IncomingHeaders, IncomingPayload, WebhookOptions
    ).

%% @private
%% @doc
%% Build downstream URL replacing key parts of the incoming request URL.
%% Notice that the URL can be completely replace or it can only replace
%% a small part.
%% @end
-spec build_downstream_request(Req, iolist(), kairos_config:kairos_handler_options()) ->
    {ok, {atom(), binary(), list(tuple()), binary(), list()}}
when
    Req :: cowboy_req:req().

build_downstream_request(Req, UUID, ProxyOptions) ->
    % compute downstream request
    DownUrl = build_downstream_url(Req, ProxyOptions),
    io:format("Downstream URL: ~p~n", [DownUrl]),

    % compute callback URL
    CallbackUrl = build_callback_url(Req, UUID, ProxyOptions),
    io:format("Callback URL: ~p~n", [CallbackUrl]),

    % install webhook
    {ProxyUrl, ProxyHeader, ProxyPayload} = prepare_request_with_callback(
        Req, DownUrl, CallbackUrl, ProxyOptions
    ),

    % build request
    Method = binary_to_atom(string:lowercase(cowboy_req:method(Req))),
    Options = [],
    {ok, {Method, ProxyUrl, ProxyHeader, ProxyPayload, Options}}.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

-define(REQ, #{
    bindings => #{},
    body_length => 42,
    cert => undefined,
    has_body => true,
    headers =>
        #{
            <<"accept-encoding">> => <<"gzip,deflate">>,
            <<"connection">> => <<"Keep-Alive">>,
            <<"content-length">> => <<"42">>,
            <<"content-type">> => <<"application/json">>,
            <<"host">> => <<"localhost:8080">>,
            <<"user-agent">> => <<"Apache-HttpClient/4.5.13 (Java/11.0.12)">>
        },
    host => <<"localhost">>,
    host_info => undefined,
    method => <<"POST">>,
    path => <<"/webhook/">>,
    path_info => undefined,
    peer => {{127, 0, 0, 1}, 63863},
    pid => self(),
    port => 8080,
    qs => <<>>,
    ref => public_http_listener,
    scheme => <<"http">>,
    sock => {{127, 0, 0, 1}, 8080},
    streamid => 1,
    version => 'HTTP/1.1'
}).

-define(OPTIONS, [
    {downstream_url, [
        {port, 8082}
    ]},
    {downstream_webhook, {
        header, "X-Webhook-Url"
    }},
    {callback_url, [
        {port, 8081},
        {path, "/callback/:uuid"}
    ]}
]).

build_url_with_different_port_should_change_port_test() ->
    NewUrl = build_url(?REQ, #{port => 8082}),
    ?assertEqual(<<"http://localhost:8082/webhook/">>, NewUrl).

build_downstream_url_with_options_should_change_parts_test() ->
    NewUrl = build_downstream_url(?REQ, ?OPTIONS),
    ?assertEqual(<<"http://localhost:8082/webhook/">>, NewUrl).

build_callback_url_with_options_should_change_parts_test() ->
    NewUrl = build_callback_url(?REQ, "ABC123", ?OPTIONS),
    ?assertEqual(<<"http://localhost:8081/callback/ABC123">>, NewUrl).

install_callback_on_request_with_header_webhook_should_add_header_test() ->
    Header = "X-webhook-url",
    CallbackUrl = "http://localhost:8082/callback/",
    {_Url, Headers, _Payload} = install_callback_on_request(
        "http://localhost:8082/webhook/", CallbackUrl, [], <<>>, {header, Header}
    ),
    ?assertEqual([{list_to_binary(Header), CallbackUrl}], Headers).

prepare_request_with_callback_with_options_should_build_request_test() ->
    meck:new(cowboy_req),
    meck:expect(cowboy_req, headers, fun(_R) -> #{} end),
    meck:expect(cowboy_req, read_body, fun(R) -> {ok, <<"hello world">>, R} end),
    Header = "X-Webhook-Url",
    DownUrl = "http://localhost:8082/webhook/",
    CallbackUrl = "http://localhost:8081/callback/",
    {_Url, Headers, _Payload} = prepare_request_with_callback(?REQ, DownUrl, CallbackUrl, ?OPTIONS),
    ?assertEqual([{list_to_binary(Header), CallbackUrl}], Headers),
    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).

-endif.
