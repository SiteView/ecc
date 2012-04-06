-module(rand).
-export([create/1]).

%0-9=>48-57
%A-Z=>65-90
%a-z=>97-122

-define(STRING, "0123456789abcdefghigklmnopqrstuvwxyzABCDEFGHIGKLMNOPQRSTUVWXYZ").

create(_, 0, Stack)->
    {ok, Stack};
create(Random, Len, Stack)->
    {Index, State} = random:uniform_s(length(?STRING), Random),
    create(State, Len -1, [lists:nth(Index, ?STRING)|Stack]).
	
create(Len)when Len >= 1 ->
    {X, Y, Z} = erlang:now(),
    case random:seed(X, Y, Z) of
            undefined ->
                    case random:seed(X, Y, Z) of
                        undefined ->
                            {error, random};
                        Random2->
                            create(Random2, Len, [])
                    end;
            Random1 ->
                    create(Random1, Len, [])
    end;
create(_)->
    {error, length}.
	