%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(metronome_callback).

-include("types.hrl").

-callback tick(Name :: atom(), Timing :: metronome_timing()) -> ok.
