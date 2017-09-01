%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stopwatch_callback).

-include("types.hrl").

-callback tick(Name :: atom(), Timing :: stopwatch_timing()) -> ok.
