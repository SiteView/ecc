% Lexes 

Definitions.
WS   = ([\000-\s]|%.*)
TYPE = (total|sended|received)
FUN  = (max|min|avg|sum)

Rules.
{TYPE}      :   {token,{var,TokenLine,list_to_atom(TokenChars)}}.
{FUN}       :   {token,{atom,TokenLine,list_to_atom(TokenChars)}}.
[(),]       :   {token,{list_to_atom(TokenChars),TokenLine}}.
{WS}+       :   skip_token.

Erlang code.