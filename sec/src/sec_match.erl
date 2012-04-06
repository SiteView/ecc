-module(sec_match).
-author('oldhand<oldhand@sina.com>').
-include("sec_log.hrl").

-include("xmerl.hrl").
-compile(export_all).


match(Facility, Severity, Host, Body) ->
  
         ?Log({"Result: ~p~n", [{Facility, Severity, Host, Body}]}),	        
  ok.