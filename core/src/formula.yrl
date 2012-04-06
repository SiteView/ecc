Nonterminals 
expression expressions function literal.

Terminals '(' ')' ',' 
var atom.

Rootsymbol expression.

expression    -> literal : 
	      	 {expression,'$1'}.
expression    -> function '(' expressions ')' : 
	      	 {expression,'$1','$3'}.
function      -> atom : 
	      	 {function,'$1'}.     
expressions   -> expression : 
	      	 ['$1'].
expressions   -> expression ',' expressions : 
	      	 ['$1'] ++ '$3'.
literal       -> var : 
	      	 '$1'.