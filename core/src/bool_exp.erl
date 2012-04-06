-module(bool_exp).
-export([parse/1]).

parse(Tokens)->
	run(Tokens,0,0,0).
	
cal(V1,Op,V2)when Op=='and'->
	V1 and V2;
cal(V1,Op,V2)when Op=='or'->
	V1 or V2;
cal(_,_,_)->
	error.
	
op_level('and')->2;
op_level('or')->1;
op_level(_)->0.

run([],0,0,0)->false;
run([],V1,0,0)->V1;
run([],V1,Op,0)->
	error;
run([],V1,Op,V2)->
	run([],cal(V1,Op,V2),0,0);
run([C|T],0,0,0)->
	run(T,C,0,0);
run([C|T],V1,0,0)when C=='and' orelse C=='or'->
	run(T,V1,C,0);
run([C|T],V1,0,0)->
	run([C]++T,V1,'or',0);
run([C|T],V1,Op,0)->
	run(T,V1,Op,C);
run([C|T],V1,Op,V2) when C=='and' orelse C=='or'->
	[C2|T2] = T,
	L1 = op_level(C),
	L2 = op_level(Op),
	if
		L1>=L2->
			run(T2,cal(C2,C,V1),Op,V2);
		true->
			run(T2,C2,C,cal(V1,Op,V2))
	end;
run([C|T],V1,Op,V2)->
	DefOp = 'or',
	L1 = op_level(DefOp),
	L2 = op_level(Op),
	if
		L1>=L2->
			run(T,cal(C,DefOp,V1),Op,V2);
		true->
			run(T,C,DefOp,cal(V1,Op,V2))
	end.