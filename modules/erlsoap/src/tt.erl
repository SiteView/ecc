-module(tt).
-compile(export_all).
-include("m.hrl").

handler([H],[B])->
    io:format("header is ~p\n",[H]),
        handler(B);
handler(_,[B])->
        handler(B);
handler(undefined,undefined)->
   {ok,undefined}.
handler(#'cwmp:GetParameterNames'{'ParameterPath'=P, 'NextLevel'=B})->
  C=[#'cwmp:ParameterInfoStruct'{'Name'="t", 'Writable'=true}],
  [D]=C,
   A=#'cwmp:ParameterInfoList'{ anyAttribs= [{{"xsi:type", []},"SOAP-ENC:Array"},{{"SOAP-ENC:arrayType", []},"cwmp:EventStruct[11]"}],'ParameterInfoStruct'=C}, 
  {ok,[#'cwmp:GetParameterNames'{'ParameterPath'="123", 'NextLevel'=true}]}.
  %%{ok,#'soap:Header'{choice=[#'cwmp:ID'{'P:mustUnderstand'="1",'#text'="1234"}]},[#'cwmp:GetParameterNamesResponse'{'ParameterList'=A}]}.
   %% Eror=[#'cwmp:Fault/SetParameterValuesFault'{'ParameterName'="test", 'FaultCode'="9000", 'FaultString'="server error"}],
   %Er=#'cwmp:Fault'{'FaultCode'="9001",'FaultString'="error test", 'SetParameterValuesFault'=Eror},
   %%{error,"Client","rrr",Er},


%%handler(#'cwmp:DeleteObject'{'ObjectName'=A, 'ParameterKey'=B})->
 %%  {ok,#'soap:Header'{choice=[#'cwmp:ID'{mustUnderstand="1",'#text'="1234"}]},[#'cwmp:DeleteObjectResponse'{'Status'="1"}]}.

%handler(#'cwmp:HelloObject'{'Hellostr'=Str})->
  %%     {ok, [#'cwmp:HelloResponse'{'HelloReturn' =#'cwmp:HelloResult'{'Word'=Str}}]}.








     
 