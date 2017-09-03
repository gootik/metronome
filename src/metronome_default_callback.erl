%%%-------------------------------------------------------------------
%%% @doc Default callback handler that outputs an info message into the
%%%      error_logger.
%%% @end
%%%-------------------------------------------------------------------
-module(metronome_default_callback).

-behavior(metronome_callback).

-export([
    tick/2
]).

tick(Name, Timing) ->
    error_logger:info_msg("[Default Metronome Callback] - ~p took ~pms", [Name, Timing/1000]).