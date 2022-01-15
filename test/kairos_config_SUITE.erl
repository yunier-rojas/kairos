-module(kairos_config_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Returns list of tuples to set default properties for the suite.
suite() ->
    [{timetrap, {seconds, 5}}].

all() ->
    [
        test_config_should_read_from_file
    ].

test_config_should_read_from_file(_Config) ->
    ConfigFile = filename:append(filename:dirname(code:which(?MODULE)), "empty-kairos.config"),
    os:putenv("KAIROS_CONFIG_FILE", ConfigFile),
    Res = kairos_config:get_routes(),
    ?assertEqual([], Res).
