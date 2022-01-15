-module(kairos_utils_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Returns list of tuples to set default properties for the suite.
suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [
        test_gen_uuid_should_return_proper_type,
        test_read_body_with_no_acc_and_last_part_should_return_body,
        read_body_with_no_acc_and_parts_should_return_body_test
    ].

test_gen_uuid_should_return_proper_type(_Config) ->
    UUID = kairos_utils:gen_uuid(),
    [Head | _] = UUID,
    ?assert(is_integer(Head)).

test_read_body_with_no_acc_and_last_part_should_return_body(_Config) ->
    meck:new(cowboy_req),
    Req0 = first,
    Expected = <<"ok">>,
    Res = {ok, Expected, Req0},
    meck:expect(cowboy_req, read_body, fun(_) -> Res end),
    {ok, Test} = kairos_utils:read_body(Req0),
    ?assertEqual(Expected, Test),
    true = meck:validate(cowboy_req),
    meck:unload(cowboy_req).

read_body_with_no_acc_and_parts_should_return_body_test(_Config) ->
    meck:new(cowboy_req),
    Req0 = first,
    Expected = <<"hello world">>,
    Mock = fun
        (first) -> {more, <<"hello ">>, last};
        (last) -> {ok, <<"world">>, last}
    end,
    meck:expect(cowboy_req, read_body, Mock),
    {ok, Test} = kairos_utils:read_body(Req0),
    ?assertEqual(Expected, Test),
    true = meck:validate(cowboy_req),
    meck:unload(cowboy_req).
