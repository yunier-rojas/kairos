-module(kairos_result_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Returns list of tuples to set default properties for the suite.
suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [
        test_results_with_registered_process_should_receives_its_result
    ].

test_results_with_registered_process_should_receives_its_result(_Config) ->
    PID = self(),
    meck:new(syn),
    meck:expect(syn, register, fun(_, _, _) -> ok end),
    meck:expect(syn, lookup, fun(_, _) -> {PID, []} end),
    UUID = kairos_utils:gen_uuid(),
    kairos_result:register(UUID),
    timer:apply_after(1000, kairos_result, send_to, [UUID, ok]),
    {ok, Result} = kairos_result:wait_for(UUID),
    ?assertEqual(ok, Result),
    ?assert(meck:validate(syn)),
    meck:unload(syn).
