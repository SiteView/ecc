-define(Log(X), sec_restlog:log(X,?MODULE,?LINE)).
-define(log(X,Y), sec_restlog:log(X,Y,?MODULE,?LINE)).

-define(writelog(X,Y), sec_restlog:writelog(X,?MODULE,?LINE,Y)).
