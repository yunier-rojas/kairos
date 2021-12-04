%%%-------------------------------------------------------------------
%% @doc kairos public API
%% @end
%%%-------------------------------------------------------------------

-module(kairos_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kairos_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
