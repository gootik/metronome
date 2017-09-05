-module(metronome_test).

-compile([{parse_transform, metronome_transform}]).

-include_lib("eunit/include/eunit.hrl").
-include("metronome.hrl").

-behavior(metronome_callback).

-export([
    tick/2
]).

manual_test() ->
    {stopwatch_start, manual_test},
    Sample = lists:sum(lists:seq(0, 100)),
    {stopwatch_stop},

    ?assertEqual(5050, Sample),
    check_callback(manual_test).

simple_test() ->
    ?timed(?FUNCTION_NAME,
           begin
               Sample = lists:sum(lists:seq(0, 100))
           end),

    ?assertEqual(5050, Sample),

    check_callback(?FUNCTION_NAME).

multi_test() ->
    ?timed(first,
           begin
               Sample = lists:sum(lists:seq(0, 100))
           end),

    ?timed(second,
           begin
               Sample2 = lists:sum(lists:seq(0, 100))
           end),

    ?assertEqual(5050, Sample),
    ?assertEqual(5050, Sample2),

    check_callback(first),
    check_callback(second).

nested_test() ->
    ?timed(nested_first,
           begin
               Sample = lists:sum(lists:seq(0, 100)),
               ?timed(nested_second,
                      begin
                          Sample2 = lists:sum(lists:seq(0, 100))
                      end)
           end),

    ?assertEqual(5050, Sample),
    ?assertEqual(5050, Sample2),

    check_callback(nested_first),
    check_callback(nested_second).

%%==================================
%% Test callback handler
%%==================================
tick(Name, Timing) ->
    put(Name, Timing).

check_callback(Name) ->
    ?assert(get(Name) =/= undefined andalso get(Name) >= 0).