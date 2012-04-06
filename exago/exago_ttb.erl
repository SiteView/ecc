-module(exago_ttb).

-export([enable/0, disable/0]).
-export([default_match_spec/0, trace_function/1]).

%% start (trace) -> seq_trace* -> end_of_trace
-spec(default_match_spec/0 :: () -> list()).
default_match_spec() -> [{'$1', [], []}].

-spec(enable/0 :: () -> ok).
enable()  -> 
    ttb:tracer(),
    ttb:p(all, [call]).

-spec(disable/0 :: () -> ok).
disable() -> 
    ttb:stop([format]).

-spec(trace_function/1 :: (tuple()) -> ok).
trace_function({Module, Function}) -> 
    ttb:tp(Module, Function, default_match_spec()).

%% ttb:format("nonode@nohost-ttb", [{handler, {fun (Fd, Trace, TraceInfo, State) -> ttb_parse:parser(Fd, Trace, TraceInfo, State) end, 0}}]).
