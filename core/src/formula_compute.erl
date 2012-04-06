-module(formula_compute).
-compile(export_all).

test(ExpressionStr) ->
    case formula_lex:string(ExpressionStr) of
	{ok,Tokens,_Line} ->
	    case formula:parse(Tokens) of
		{ok,Expression} ->
		    {ok,Expression};
		_ -> {error,"error in parse"}
	    end;
	_ ->
	    {error,"error in lex"}
    end.

compute(ExpressionStr,{_ItemFun,_Count} = Opt) ->
    %io:format("~p-n",[Opt]),
    Result = case test(ExpressionStr) of
	{ok,Expression} ->
	    execute(Expression,Opt);
	Error -> Error  
    end,
    Result.

execute({expression,{function,{_,_,Name}},ArgsExpressions},Opt) ->
    apply(formula_compute,Name,[ArgsExpressions,Opt]);   
execute({expression,{var,_,Var}},{ItemFun,_}) ->
    %io:format("Var is : ~p~n",[Var]),
    R = ItemFun(Var),
    %io:format("Formula execute result is: ~p~n",[R]),
    R.

sum([_Expression|_T] = ArgsExpressions,{ItemFun,_} = Opt) ->
    io:format("Fun is :~p ~p ~n",[ArgsExpressions,Opt]),
    lists:sum(execute(_Expression,Opt)).	      
    
    

avg([_Expression|_T] = ArgsExpressions,{ItemFun,Count} = Opt) ->
    %io:format("Fun is :~p ~p ~n",[ArgsExpressions,Opt]),
    R = sum(ArgsExpressions,Opt)/Count,
    R.
    

max([_Expression|_T] = ArgsExpressions,{ItemFun,_} = Opt) ->
    %io:format("Fun is :~p ~p ~n",[ArgsExpressions,Opt]),
    lists:max(execute(_Expression,Opt)).
    
    
    

min([_Expression|_T] = ArgsExpressions,{ItemFun,_} = Opt) ->
    %io:format("Fun is :~p ~p ~n",[ArgsExpressions,Opt]),
    R = lists:min(execute(_Expression,Opt)), 
    R.



