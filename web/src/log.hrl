-ifdef(debug).
-define(Log(X), log:log(X,?MODULE,?LINE)).
-define(log(X,Y), log:log(X,Y,?MODULE,?LINE)).
-else.
-define(Log(X), void).
-define(log(X,Y), void).
-endif.

-ifdef(debug).
-define(writelog(X), log:writelog(X)).
-else.
-define(writelog(X), void).
-endif.


%-ifdef(debug).
%-define(TimeSpan(Fun),  
%        statistics(wall_clock),
%        FunResult = Fun,  
%        {_, Time} = statistics(wall_clock),
%        io:format("[~pms]~n", [Time]), 
%        FunResult).
%-else.
%-define(TimeSpan(Fun), Fun).
%-endif.
%
-ifdef(debug).
-define(TimeSpan1(Mod,Fun,Args),         
        {Time,Result} = timer:tc(Mod,Fun,Args),
        io:format("[~p,~3..0w]~n", [Time div 1000,Time rem 1000]), 
        Result).
-else.
-define(TimeSpan1(Mod,Fun,Args), apply(Mod,Fun,Args)).
-endif.

