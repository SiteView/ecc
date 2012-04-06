%%
%% ldap Monitor
%%

%% @author lei.lin@dragonflow.com
%% @copyright 2009 siteview
%% @version 1.0
%% @doc ldap monitor
-module(ldap_monitor,[BASE]).
-compile(export_all).
-extends(atomic_monitor).
-include("monitor.hrl").
-include("monitor_template.hrl").


%% @spec new() -> Obj
%% @type Obj = term()
%% @doc create a new instance for directory monitor
new() ->
    Base = atomic_monitor:new(),
    Base:set_attribute(pRoundTripTime, "n/a"),
    Base:set_attribute(pStatus, 0),
    {?MODULE,Base}.
    
%% @spec update() -> Result
%% @type Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
update() ->
    {ok,{_,URLProvider}} = THIS:get_property(pURLProvider),
    {ok,{_,SecurityPrincipal}} = THIS:get_property(pSecurityPrincipal),
    {ok,{_,SecurityCredential}} = THIS:get_property(pSecurityCredential),
    {ok,{_,MatchString}} = THIS:get_property(pMatchString),
    {ok,{_,LdapQuery}} = THIS:get_property(pLdapQuery),
    {ok,{_,LdapFilter}} = THIS:get_property(pLdapFilter),
    [Scode,Sstring,Res,Mtime] = authenticate(URLProvider,SecurityPrincipal,SecurityCredential,LdapQuery,LdapFilter),
    Time = sv_datetime:microSecondsToStrSeconds(Mtime),
    case Scode of
    "200" ->
        TString = Res ++ " " ++ Time ++ " sec";
    _ ->
        TString = Scode ++ " " ++ Sstring
    end,
    if  MatchString /= "" ->
        case regexp:match(Res,MatchString) of
        {match,_,_} ->
            String = "matched: " ++ TString,
            EScode = Scode,
            set_content_match(Res);
        nomatch ->
            EScode = "-999",
            String = "content match error, " ++ TString;
        {error,_Error} ->
            EScode = "-999",
            String = "invalid regular expression" ++ TString        
        end;
    true ->
        set_content_match(Res),
        EScode = Scode,
        String = TString
    end,
    case EScode of
    "200" ->
        Len = length(String), 
        if Len > 200 ->        
            THIS:set_attribute(?STATE_STRING, string:substr(String,200)),
	        THIS:set_attribute(pRoundTripTime, Time),
            THIS:set_attribute(pStatus, 200);
        true ->
            THIS:set_attribute(?STATE_STRING, String),
	        THIS:set_attribute(pRoundTripTime, Time),
            THIS:set_attribute(pStatus, 200)   
        end;   
    Code ->
        THIS:set_attribute(?STATE_STRING, String),
	    THIS:set_attribute(pStatus, list_to_integer(Code)),
        THIS:set_attribute(pRoundTripTime, "n/a"),
		THIS:set_attribute(?NO_DATA, true)         
    end.

%% @spec authenticate(Url,SecPri,SecCre,LdapQu,LdapFilt) -> [StatuCode,StatusString,Result,Time]
%%  Result = term()
%% @doc update is the run function called by schedule to test the  directory monitor
%%return [statucode,statusstring,result,miltime]
authenticate(UrlPro,SecPri,SecCre,LdapQu,LdapFilt) ->
    {Time, Value} = timer:tc(THIS, getLdapAttr, [UrlPro,SecPri,SecCre,"","",LdapQu,LdapFilt]),    
    lists:append(Value,[Time]).
  
%% @spec getLdapAttr(UrlPro,SecPri,SecCre,_Str1,Str2,LdapQu,LdapFilt) -> [Code,Explain,Value]
%%  Code = integer()
%%  Explain = string()
%%  Vlaue = string()
%% @doc update is the run function called by schedule to test the  directory monitor 
getLdapAttr(UrlPro,SecPri,SecCre,_Str1,Str2,LdapQu,LdapFilt) -> 
    HostandPort = string:tokens(string:substr(UrlPro,length("ldap://")+1),":"), % is List   
    case length(HostandPort) of
    1 ->
        [Host] = HostandPort,
        Port = 389;
    _ ->
        [Host,Port_str] = HostandPort,
	Port = list_to_integer(Port_str)
    end,
    case eldap:open([Host],[{port,Port}]) of
    {ok,Handle} ->
         if  SecCre /= "" ->
            BindStatu =  eldap:simple_bind(Handle,SecPri,SecCre);
        true ->
            BindStatu =  eldap:simple_bind(Handle,anon,anon)     
        end,        
        case BindStatu of
        ok ->
            Index1 = string:str(SecPri,","),
            Base = {base,string:substr(SecPri,Index1+1)},
            Scope = {scope, eldap:wholeSubtree()},
            [Name,Value] = string:tokens(string:substr(SecPri,1,Index1-1),"="),
            Filter = {filter, eldap:equalityMatch(Name, Value)},
           case  eldap:search(Handle, [Base,Scope,Filter]) of
           {ok,Res} ->
                if  LdapQu /= ""  ->
                    if  LdapFilt /= "" ->
                        FiltLen = length(LdapFilt),
                        [FT|_] = string:tokens(LdapFilt,","),
                        Temp = string:tokens(FT,"="),                    
                        case length(Temp) of
                        2 ->
                            if FiltLen > 1 ->
                                [FilterTemp,_] = string:tokens(LdapFilt,","),
                                [FName,FValue] = string:tokens(FilterTemp,"="),
                                QuBase = {base,LdapQu},
                                QuFilter = {filter, eldap:equalityMatch(FName, FValue)},
                                case  eldap:search(Handle, [QuBase,Scope,QuFilter]) of
                                {ok,QuRes} ->
                                    eldap:close(Handle),
                                    ["200",Str2,make_string(QuRes)];
                                _ ->
                                    eldap:close(Handle),
                                    ["-99","General error. ",""]                            
                                end; 
                            true ->
                                [FName,FValue] = string:tokens(LdapFilt,"="),
                                QuBase = {base,LdapQu},
                                QuFilter = {filter, eldap:equalityMatch(FName, FValue)},                           
                                case  eldap:search(Handle, [QuBase,Scope,QuFilter]) of
                                {ok,QuRes} ->
                                    eldap:close(Handle),
                                    ["200",Str2,make_string(QuRes)];
                                _ ->
                                    eldap:close(Handle),
                                    ["-99","General error. ",""]                            
                                end          
                            end;
                        _ ->
                            ["-96","Invalid Filter.",""]
                        end;                        
                    true ->
                        Index2 = string:str(LdapQu,","),
                        [FName,FValue] = string:tokens(string:substr(SecPri,1,Index2-1),"="),
                        QuBase = {base,LdapQu},
                        QuFilter = {filter, eldap:equalityMatch(FName, FValue)}, 
                        case  eldap:search(Handle, [QuBase,Scope,QuFilter]) of
                        {ok,QuRes} ->
                            eldap:close(Handle),
                            ["200",Str2,make_string(QuRes)];
                        _ ->
                            eldap:close(Handle),
                            ["-99","General error. ",""]                            
                        end                    
                    end;
                true ->
                    eldap:close(Handle),
                    ["200",Str2,make_string(Res)] 
                end;
            _ ->
                eldap:close(Handle),
                ["-99","General error. ",""]
            end;                
        {error,Cause1} ->
            eldap:close(Handle),
            case Cause1  of
            invalidCredentials ->
                ["-65","not able to authenticate this user","invalidCredentials"];
            anonymous_auth ->
                ["-99","General error. ","anonymous_auth"];
            _ ->
                ["-99","General error. ",atom_to_list(Cause1)]
            end     
        end;
    {error,Cause2} -> 
        case Cause2 of
        "connect failed" ->
            ["-997","not able to reach server ",Cause2];
        _ ->
            ["-99","General error",Cause2]
        end            
    end.
    

make_string(T) ->
    case T of
    {eldap_search_result,L1,_L2} ->
        case length(L1) of
        0 ->
            "";
        _ ->
            make_string_util_1(L1)
        end;
    _ ->
        ""
    end.

make_string_util_1(List) ->
    make_string_util(List,length(List),"").
make_string_util(_Li,0,R) -> R;
make_string_util(Li,Num,Re) ->
    [A|B] = Li,
    {_Atom,Str,List} = A,
    make_string_util(B,Num-1,Re ++ Str ++ make_string_util_2(List) ++ "  ").    

make_string_util_2(List) ->
    make_string_util_2_t(List,length(List),"").
make_string_util_2_t(_Li,0,R) -> "  " ++ "{" ++ R ++ "}";
make_string_util_2_t(Li,Num,Re) ->
    [A|B] = Li,
    {Name,Val} = A,
    if Name == "userPassword" ->
        [Temp] = Val, 
        TempValue = base64:encode_to_string(Temp),
        Value = [TempValue];
    true ->
        Value = Val  
    end,
    make_string_util_2_t(B,Num-1,Re ++" "++ Name ++ ":" ++ string:join(Value,",")).     


%% @spec set_content_match(string()) -> ok
%% @doc  To obtain threshold and the match
set_content_match(Date) ->
    case  THIS:get_property(error_classifier) of
	{ok,{error_classifier,[{pMatchValue,_T1,Content1}]}} ->
		case regexp:match(Date,Content1) of
        {match,_,_} ->
            THIS:set_attribute(pMatchValue,Content1);
        _ ->
            case THIS:get_property(warning_classifier) of
            {ok,{warning_classifier,[{pMatchValue,_T2,Content2}]}} ->
                case regexp:match(Date,Content2) of
                {match,_,_} ->
                    THIS:set_attribute(pMatchValue,Content2);
                _ ->
                    case THIS:get_property(good_classifier) of 
                    {ok,{good_classifier,[{pMatchValue,_T3,Content3}]}} ->
                        case regexp:match(Date,Content3) of
                        {match,_,_} -> 
                            THIS:set_attribute(pMatchValue,Content3);
                        _ ->
                            THIS:set_attribute(pMatchValue,"")
                        end;
                    _ ->                     
                        THIS:set_attribute(pMatchValue,"")
                    end
                end;
            _ ->
                case THIS:get_property(good_classifier) of 
                {ok,{good_classifier,[{pMatchValue,_T3,Content3}]}} ->
                    case regexp:match(Date,Content3) of
                    {match,_,_} ->
                        THIS:set_attribute(pMatchValue,Content3);
                    _ ->
                        THIS:set_attribute(pMatchValue,"")
                    end;
                _ ->
                    THIS:set_attribute(pMatchValue,"")
                end               
            end             
        end;
    _ ->
        case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,[{pMatchValue,_T2,Content2}]}} ->
            case regexp:match(Date,Content2) of
            {match,_,_} ->
                THIS:set_attribute(pMatchValue,Content2);
            _ ->
                case THIS:get_property(good_classifier) of 
                {ok,{good_classifier,[{pMatchValue,_T3,Content3}]}} ->
                    case regexp:match(Date,Content3) of
                    {match,_,_} ->
                        THIS:set_attribute(pMatchValue,Content3);
                    _ ->
                        THIS:set_attribute(pMatchValue,"")            
                    end;
                _ ->
                    THIS:set_attribute(pMatchValue,"") 
                end                 
            end;
        _ ->
            case THIS:get_property(good_classifier) of 
		    {ok,{good_classifier,[{content_match,_T3,Content3}]}} ->
			    case regexp:match(Date,Content3) of
                {match,_,_} ->
                    THIS:set_attribute(content_match,Content3);
                _ ->
                    THIS:set_attribute(content_match,"")
                end;       
            _ ->
                THIS:set_attribute(content_match,"")             
            end    
        end      
    end.
 
    
%% @spec verify(Params) -> {ok, []} | {error, Reason}
%%  Params = [term()]
%%  Reason = string()
%% @doc verify is the function called by schedule to verify the parameter of monitor inputed by user    
verify(Params)->
	Errs = 
	case proplists:get_value(pURLProvider,Params) of
    ""->
		[{pURLProvider,"URLProvider  missing"}];
    Host->
	    case string:rstr(Host," ") of
		    0->
			    [];
			_->
			    [{host,"no spaces are allowed"}]
	    end
	end ++
	case proplists:get_value(pSecurityPrincipal,Params) of
    ""->
		[{pSecurityPrincipal,"pSecurityPrincipal  missing"}];
    _SecurityPrincipal ->
	    []
	end ++    
	case proplists:get_value(pLdapFilter,Params) of
    ""->
		[];
    LdapFilter->
        [A|_] = string:tokens(LdapFilter,","),
	    case length(string:tokens(A,"=")) of
		    2 ->
			    [];
			_->
			    [{pLdapFilter,"Invalid Filter."}]
	    end
	end,    
    if length(Errs)>0 ->
	    {error,Errs};
    true ->
	    {ok,""}
	end.

getHostname()->
	case THIS:get_property(pURLProvider) of
		{ok,{_,V}}->
			V;
		_->
			BASE:getHostname()
	end.


%% @spec get_template_property() -> list()
%% @doc get_template_property is the function called by schedule to determinate two sections of value:
get_template_property() ->
    BASE:get_template_property() ++
    [
        #property{name=pURLProvider,title="LDAP service provider",type=text,order=1,description = "the LDAP server to connect to (example: ldap://ldap.this-company.com:389)"},
        #property{name=pSecurityPrincipal,title="Security Principal",type=text,order=2,description="the LDAP query to perform (example: uid=testuser,ou=TEST,o=this-company.com)"},
        #property{name=pSecurityCredential,title="Security Credentials",type=password,order=3,description="the password used for authentication. If left blank it defaults to Anonymous."},
        #property{name=pMatchString,title="Content Match",type=text,advance=true,order=1,description="optional, match against query result, using a string or a <a href=/SiteView/docs/regexp.htm>regular expression</a> or <a href=/SiteView/docs/XMLMon.htm>XML names</a>."},
        #property{name=pLdapQuery,title="Object Query",type=text,advance=true,order=2,description="Be able to look at a separate object than the user dn object. This field has to be set if using a filter."},
        #property{name=pLdapFilter,title="LDAP Filter",type=text,advance=true,order=3,description="To be able to perform a search with filter"},
        #property{name=pStatus,title="status",type=numeric,configurable=false,state=true},
        #property{name=pRoundTripTime,title="roundTripTime",type=numeric,configurable=false,state=true,baselinable=true},       
        #property{name=pMatchValue,title="matchValue",type=text,configurable=false,state=true}
    ]. 
    
%% @spec get_classifier(Param) -> List
%% Param = atom()
%% List = [Tuple]
%% Tuple = {Status, Logic, Value}
%% Status = 'error'|'warning'| 'good' 
%% Logic = '!=' | '==' | '>' | '<' | 'contain'
%% Value = term()
%% @doc get_classifier is run function called by schedule to decide the condition of good, warning and error   
get_classifier(error)->  
	Cls = case THIS:get_property(error_classifier) of
				{ok,{error_classifier,Classifier}}->
					Classifier;
				_->
					[{pStatus,'!=',200}]
			end;
get_classifier(warning)->
	Cls =case THIS:get_property(warning_classifier) of
		{ok,{warning_classifier,Classifier}}->
			Classifier;
		_->
			[]
	end;
	
get_classifier(good)->
	Cls = case THIS:get_property(good_classifier) of
		{ok,{good_classifier,Classifier}}->
			Classifier;
		_->
			[{pStatus,'==',200}]
	end.    
    