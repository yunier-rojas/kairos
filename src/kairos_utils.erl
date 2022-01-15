%%%-------------------------------------------------------------------
%%% @author yunier
%%% @copyright 2021, Yunier Rojas García
%%% @doc
%%% Collection of function used across the project.
%%% @end
%%% Created : 13. Dec 2021 21:10
%%%-------------------------------------------------------------------
-module(kairos_utils).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

-export([gen_uuid/0]).
-export([read_body/1]).

-type kairos_uuid() :: [integer()].
-export_type([kairos_uuid/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Generates a UUID V4.
%% @end
-spec gen_uuid() -> kairos_uuid().

gen_uuid() ->
    bin_to_hex_list(uuid:get_v4()).

%% @doc
%% Reads request payload using cowboy library.
%% @end
-spec read_body(Req0) -> {'ok', Data} when
    Req0 :: cowboy_req:req(),
    Data :: binary().

read_body(Req0) ->
    {ok, Data, _} = read_body(Req0, <<>>),
    {ok, Data}.

%%%===================================================================
%%% Internals
%%%===================================================================

%% @private
%% @doc
%% Transform binary data into a list of hexadecimal characters.
%% @end
-spec bin_to_hex_list(Bin) -> kairos_uuid() when
    Bin :: binary().

bin_to_hex_list(Bin) when is_binary(Bin) ->
    lists:flatten([integer_to_list(X, 16) || <<X>> <= Bin]).

%% @private
%% @doc
%% Recursively reads cowboy request payload until completes.
%% @end
-spec read_body(Req0, Acc) -> {'ok', Data, Req1} when
    Req0 :: cowboy_req:req(),
    Acc :: binary(),
    Data :: binary(),
    Req1 :: map().
read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, <<Acc/binary, Data/binary>>, Req};
        {more, Data, Req} -> read_body(Req, <<Acc/binary, Data/binary>>)
    end.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).

gen_uuid_with_mock_should_generate_valid_test() ->
    meck:new(uuid),
    UUID = <<185, 210, 164, 116, 59, 239, 68, 178, 171, 0, 167, 46, 97, 26, 154, 216>>,
    Expected = "B9D2A4743BEF44B2AB0A72E611A9AD8",
    meck:expect(uuid, get_v4, fun() -> UUID end),
    Test = gen_uuid(),
    ?assertEqual(Expected, Test),
    ?assert(meck:validate(uuid)),
    meck:unload(uuid).

-endif.
