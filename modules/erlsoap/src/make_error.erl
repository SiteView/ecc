-module(make_error).
-compile(export_all).
-include("tt.hrl").

faultDetail(#'cwmp:Fault'{'FaultCode'=FaultCode,'FaultString'=FaultString, 'SetParameterValuesFault'=SetParameterValuesFault} )->
 FaultStr=if(FaultString==undefined)->
                  [];
               true->
	          "<FaultString>"++FaultString++"</FaultString>"
              end,
 Parstr=if(SetParameterValuesFault==undefined)->
                  [];
               true->
	        lists:concat(getParameterFault(SetParameterValuesFault))
              end,
 try
 "<detail>"
    "<cwmp:Fault>"
       "<FaultCode>"++FaultCode++"</FaultCode>"++FaultStr++Parstr++"</cwmp:Fault>"++
   "</detail>"
 catch
      _:_ ->
         "<detail>"
	    "<cwmp:Fault>"
	       "<FaultCode>Server error</FaultCode>"
	   "</detail>"
   end.


getParameterFault(Faults)->
      lists:map(
          fun(X) ->
	     ParameterName=X#'cwmp:Fault/SetParameterValuesFault'.'ParameterName',
             FaultCode=X#'cwmp:Fault/SetParameterValuesFault'.'FaultCode',
	     FaultString=X#'cwmp:Fault/SetParameterValuesFault'.'FaultString',
	     FaultString1=if(FaultString==undefined)->
                  [];
               true->
	           "<FaultString>"++FaultString++"</FaultString>"
              end,
	       "<SetParameterValuesFault>"++"<ParameterName>"++ParameterName++"</ParameterName>"++"<FaultCode>"++FaultCode++"</FaultCode>"++FaultString1++"</SetParameterValuesFault>"
	       end,Faults).

test()->
    A=[#'cwmp:Fault/SetParameterValuesFault'{'ParameterName'="test", 'FaultCode'="9000", 'FaultString'="fffff"},#'cwmp:Fault/SetParameterValuesFault'{'ParameterName'="test", 'FaultCode'="9000"}],
    B=#'cwmp:Fault'{'FaultCode'="1000", 'FaultString'="ddd"},
    C=faultDetail(B),
    io:format("a is ~p\n",[C]).
             
	   

          
        






     
          


