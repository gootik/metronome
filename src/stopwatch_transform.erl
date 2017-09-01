%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(stopwatch_transform).

-export([
    parse_transform/2
]).

parse_transform(AST, _Options) ->
    forms:map(fun do_transform/1, AST).

do_transform({tuple, L, [{atom, L, stopwatch_start}, {atom, L, StopwatchName}]} = _Form) ->
    push_sw_stack(StopwatchName),

    SwNum = get_inc_sw_count(),
    SwVar = list_to_atom("__SW_" ++ integer_to_list(SwNum)),

    {match, L,
     {var, L, SwVar},
     {call, L, {remote, L, {atom, L, erlang}, {atom, L, monotonic_time}}, []}};

do_transform({tuple, L, [{atom, L, stopwatch_stop}]} = _Form) ->
    SwName = pop_sw_stack(),
    SwNum = get_sw_count() - 1,
    SwVar = list_to_atom("__SW_" ++ integer_to_list(SwNum)),

    TimeDiff =
      {op, L, '-',
        {call, L, {remote, L, {atom, L, erlang}, {atom, L, monotonic_time}}, []},
        {var, L, SwVar}},

    Microseconds =
      {call, L,
       {remote, L, {atom, L, erlang}, {atom, L, convert_time_unit}},
       [TimeDiff, {atom, L, native}, {atom, L, microsecond}]},

    {call, L,
     {remote, L, {atom, L, ?stopwatch_callback_mod}, {atom, L, tick}},
     [{atom, L, SwName}, Microseconds]};

do_transform(Form) ->
    Form.

push_sw_stack(Name) ->
    CurStack = case erlang:get(sw_stack) of
                   undefined -> [];
                   Stack -> Stack
               end,
    erlang:put(sw_stack, [Name | CurStack]).

pop_sw_stack() ->
    CurStack = case erlang:get(sw_stack) of
                   undefined -> [undefined];
                   [] -> [undefined];
                   Stack -> Stack
               end,
    [Head | Rest] = CurStack,
    erlang:put(sw_stack, Rest),
    Head.

get_inc_sw_count() ->
    OldCount = case get(sw_count) of
                   undefined -> 0;
                   C -> C
               end,

    put(sw_count, OldCount + 1),
    OldCount.

get_sw_count() ->
    get(sw_count).