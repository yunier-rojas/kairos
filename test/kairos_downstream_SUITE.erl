-module(kairos_downstream_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Returns list of tuples to set default properties for the suite.
suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [
        test_downstream_request_with_header_webhook_should_set_the_callback_on_the_header
    ].

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

test_downstream_request_with_header_webhook_should_set_the_callback_on_the_header(_Config) ->
    UUID = "ABC123",
    Header = <<"X-Webhook-Url">>,
    CallbackUrl = <<"http://localhost:8081/callback/ABC123">>,

    meck:new(cowboy_req, [passthrough]),
    meck:expect(cowboy_req, read_body, fun(R) -> {ok, <<"hello world">>, R} end),

    meck:new(hackney),
    meck:expect(hackney, request, fun(
        _Method,
        _Url,
        Headers,
        _Payload,
        _Options
    ) ->
        {Header, WebHook} = lists:keyfind(Header, 1, Headers),
        ?assertEqual(WebHook, CallbackUrl),
        {ok, 200, [], 1}
    end),
    meck:expect(hackney, body, fun(_R) -> {ok, <<"hello world">>} end),

    {ok, _} = kairos_downstream:make_request(?REQ, UUID, ?OPTIONS),

    ?assert(meck:validate(cowboy_req)),
    meck:unload(cowboy_req).
