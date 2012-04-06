-ifndef(_SVECC_LOG).
-define(_SVECC_LOG,1).

-ifdef(debug).
-define(DEBUG_LOG(Log), log:trace(?MODULE,?LINE,Log)).
-define(DEBUG_LOG2(Format,Params), log:trace(?MODULE,?LINE,Format,Params)).
-else.
-define(DEBUG_LOG(Log),void).
-define(DEBUG_LOG2(Format,Params),void).
-endif.

-define(SVLOG(Type,Format,Params),log_manager:log(Type,?MODULE,?LINE,Format,Params)).

-define(ERROR_LOG(Log),?SVLOG('Error',"~s",Log)).
-define(ERROR_LOG2(Format,Params),?SVLOG('Error',Format,Params)).

-endif.
