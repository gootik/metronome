-define(timed(StopwatchName, Code),
    begin
        {stopwatch_start, StopwatchName},
        Result = Code,
        {stopwatch_stop},

        Result
    end).