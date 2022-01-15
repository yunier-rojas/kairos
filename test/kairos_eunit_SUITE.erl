-module(kairos_eunit_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Returns list of tuples to set default properties for the suite.
suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [
        test_eunit_on_kairos_utils_should_succeed,
        test_eunit_on_kairos_result_should_succeed,
        test_eunit_on_kairos_downstream_should_succeed,
        test_eunit_on_kairos_config_should_succeed
    ].

test_eunit_on_kairos_utils_should_succeed(_Config) ->
    ok = eunit:test(kairos_utils).

test_eunit_on_kairos_result_should_succeed(_Config) ->
    ok = eunit:test(kairos_result).

test_eunit_on_kairos_downstream_should_succeed(_Config) ->
    ok = eunit:test(kairos_downstream).

test_eunit_on_kairos_config_should_succeed(_Config) ->
    ok = eunit:test(kairos_config).
