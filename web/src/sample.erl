-module(sample).
-include("log.hrl").
-include("config.hrl").
-include("xmerl.hrl").

-export([func_get_sample/4]).
-export([func_get_sample_rollup/5]).


%http://192.168.4.119:8080/xn/rest/1.0/sample(id=sdfsdf)	
func_get_sample(Host,_Req,Path,Raw_path) -> 
	?Log({"~p~n",[{Host,Path,Raw_path}]}),
	web_common:respond(unknown).
	
	
%http://192.168.4.119:8080/xn/rest/1.0/sample(id=sfsfd)/rollup(sfsafsfd)	
func_get_sample_rollup(Host,_Req,_Path,_SubPath,_Raw_path) -> 
    ?Log({"~p~n",[{Host,_Path,_SubPath,_Raw_path}]}),
	web_common:respond(unknown).
	

	
% http://192.168.4.119:8080/xn/ato1m/1.0/content	 -> 404: Resource not found
