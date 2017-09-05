-define(timed(TimerName, Code),
    begin
        {stopwatch_start, TimerName},
        Result = Code,
        {stopwatch_stop},

        Result
    end).