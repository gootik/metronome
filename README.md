stopwatch
=====

A simple library to add instrumentation to your Erlang code. This is a more
dynamic way of doing `timer:tc/3` or even a lazier version of it.

Stopwatch will measure runtime of parts of your code and calls a method on your
predefined module with the results. For example, if you compile your code with
`{d, 'stopwatch_callback_mod', 'sample_callback'}` and run the following:

```erlang
-include_lib("stopwatch/include/stopwatch.hrl").

?timed(total_sum,
  begin
    ?timed(sum_1, First = lists:sum(lists:seq(0, 100))),
    ?timed(sum_2, Second = lists:sum(lists:seq(0, 100))),
    _ = First + Second
  end).
```

You will see 3 calls to `sample_callback` module:
```erlang
sample_callback:tick(sum_1, Microseconds).
sample_callback:tick(sum_2, Microseconds).
sample_callback:tick(total_sum, Microseconds).
```
If you don't want to use the macros or if they are giving you trouble, you can
also explicitly add the start/stop points. The above code can be written like below
with no difference in execution:

```erlang
{stopwatch_start, total_sum},

{stopwatch_start, sum_1},
First = lists:sum(lists:seq(0, 100)),
{stopwatch_stop},

{stopwatch_start, sum_2},
Second = lists:sum(lists:seq(0, 100)),
{stopwatch_stop},

_ = First + Second,

{stopwatch_stop}.
```

### Usage
1. Add the package to your rebar.config
   ```erlang
   {deps, [
      {stopwatch, "0.1.0"}
   ]}.
   ```

2. Add the parse transform to your rebar.config
   ```erlang
   {erl_opts, [
     {parse_transform, stopwatch_transform}
   ]}.
   ```

3. [Optional] Configure a custom callback module by using overrides:
   ```erlang
   {overrides, [
     {override, stopwatch, [
       {erl_opts, [
         {d, 'stopwatch_callback_mod', 'my_sample_mod'
       }]
     }]}
   ]}.
   ```

4. Instrument your code:
   ```erlang
   {stopwatch_start, some_name},
     --- Intensive amount of work ---
   {stopwatch_stop}.
   ```
   or use the included macro:

   ```erlang
   -include_lib("stopwatch/stopwatch.hrl").

   ?timed(some_name,
     --- Intensive amount of work ---
    ).
   ```
### Why
I really wanted to learn more about parse transforms and also make my own life
easier. I usually mix this library with [statsderl](https://github.com/lpgauth/statsderl)
to make things really easy to report timing of different parts of my code.

### Custom callback
If you want a customized callback versus the provided one, you have to create a
module that implements the `stopwatch_callback` behavior. An example using `statsderl`
would look like this:

```erlang
-module(stopwatch_statsderl).

-behavior(stopwatch_callback).

-export([
  tick/2
]).

tick(Name, Microseconds) ->
  statsderl:timing(atom_to_list(Name), Microseconds, 1).

```