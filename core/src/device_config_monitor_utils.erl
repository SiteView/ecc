-module(device_config_monitor_utils).

-compile(export_all).
-include("monitor.hrl").

-define(TEMPLATEPATH,"templates.deviceConfig/deviceConfig.conf").

get_config_string(Oid)->
	case file:consult(?TEMPLATEPATH) of
		{ok,R}->
			Fun = 
            fun(Element)->
                {_,Poid,_Configstr}=Element,
                case string:substr(Poid,1,7) =:= string:substr(Oid,1,7) of
                    true->true;
                    _->false
                end
            end,
            SubString = lists:filter(R,Fun),
            element(3,lists:nth(1,SubString)) ;
		_->
			""
	end.
    
compare_Config(Oconfig,Nconfig)->
    
    OResult = clear(Oconfig),
    NResult = clear(Nconfig),
    OResult1= split(OResult,"\r\n"),
    NResult1 = split(NResult,"\r\n"),
  
    TrimOResult = [trim(A)||A<-OResult1,trim(A)=/=[]],
    TrimNResult = [trim(B)||B<-NResult1,trim(B)=/=[]],
    
    [S||S <-TrimNResult,lists:member(S,TrimOResult)=:=false].
    
clear_word(Str,Word)->
    Index = string:str(Str,Word),
    case Index>0 of
    true->
        NStr = string:substr(Str,1,Index-1) ++ string:substr(Str,Index+length(Word),length(Str)),
        clear_word(NStr,Word);
    _->
        Str
    end.

trim(String)->
    string:strip(String,both,$ ). 

split(S,SpWord)->
    SpLength = string:len(SpWord),
    split_t(S,SpWord,SpLength).
split_t(S,SpWord,SpWordLength)->    
    case string:str(S,SpWord) of
        0 -> [S];
        Index-> 
        case string:substr(S,1,Index-1) of
        []->
            split_t(string:substr(S,Index+SpWordLength,length(S)),SpWord,SpWordLength);
        Ss->
            [Ss]++split_t(string:substr(S,Index+SpWordLength,length(S)),SpWord,SpWordLength)
        end
    end.
    
clear(Config)->
    clear_word(clear_word(clear_word(clear_word(clear_word(clear_word(clear_word(clear_word(clear_word(Config,"!"),"#"),"\b"),"More"),"--"),"\t\n"),"\t"),"\e"),"[42D").



