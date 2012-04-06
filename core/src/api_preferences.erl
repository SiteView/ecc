%% 
%% @doc api of preference
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>
-module(api_preferences).
-extends(api_siteview).
-compile(export_all).
-include("monitor.hrl").

-export([get_prefs/2,get_all/1,set_prefs/3,remove_prefs/2,test/2,verify/3,get_prefs_list/2,set_prefs_list/2,remove_prefs_list/2,get_all_mail/0,get_all_message/0]).

-define(MAIL_SERVER,mailServer).
-define(MAIL_USER,mailUser).
-define(MAIL_PASSWORD,mailPassword).
-define(AUTO_MAIL,autoEmail).
-define(AUTO_DAILY,autoDaily).
-define(AUTO_START,autoStartup).
-define(FROM_ADDRESS,fromAddress).
-define(MAIL_SERVER_BACKUP,mailServerBackup).

-define(TIMEOUT,5).

%% @spec get_prefs(File,Key)->({error,Reason}|{ok,[{Key,Value}]})
%% where
%%	File = atom()
%%	Key = atom()
%%	Value = term()
%%	Reason = atom()
%% @doc get a single preferences value from a File
get_prefs(File,Key)->
	preferences:get(File,Key).

%% @spec get_all(File)->({error,Reason}|{ok,[{Key,Value}]})
%% where
%%	File = atom()
%%	Key = atom()
%%	Value = term()
%%	Reason = atom()
%% @doc get all preferences from a File
get_all(File)->
	preferences:all(File).

%% @spec set_prefs(File,Key,Val)->({error,Reason}|{ok,Result})
%% where
%%	File = atom()
%%	Key = atom()
%%	Val = term()
%%	Result = atom()
%%	Reason = atom()
%% @doc set preference to a File
set_prefs(File,Key,Val)->
	Ret = preferences:set(File,Key,Val),
	case Key of
		'reportset.ini' ->
			updateindex_store(Val);
		'topnreportset.ini'->
			updateindex_store(Val);
		_ ->
			pass
	end,
	Ret.
	
updateindex_store([])->
	ok;
updateindex_store([H|E])->
	Id=element(1, H),
	Data=element(2,H),
	T1=proplists:get_value("Title", Data, ""),
    T2=string:tokens(T1, "|"),
    Title=lists:nth(1, T2),
	Description=proplists:get_value("Descript", Data, ""),
    Newdata=[{id,Id },{title,Title},{description,Description}],
	%index_store:update(report,Newdata),
    updateindex_store(E).
%% @spec remove_prefs(File,Key)->({error,Reason}|{ok,Result})
%% where
%%	File = atom()
%%	Key = atom()
%%	Val = term()
%%	Result = atom()
%%	Reason = atom()
%% @doc remove a preference from File
remove_prefs(File,Key)->
	index_store:remove(report,atom_to_list(Key)),
	preferences:remove(File,Key).
    
    
%% @spec set_prefs_list(File,KeysValues)->({error,Reason}|{ok})
%% where
%%	File = atom()
%%    KeysValues = list() =[{Key1,Value1},{Key2,Value2},...]  
%%	Key = atom()
%%	Val = term()
%%	Result = atom()
%%	Reason = atom()
%%   @doc set a list of preference to a File
set_prefs_list(File,[])-> {ok};
set_prefs_list(File,[{Key,Val}|B])-> 
	Reason = preferences:set(File,Key,Val),
    case Reason of
    {error,Reason}  -> {error,Reason};
    _   ->
        set_prefs_list(File,B)
    end.

%% @spec remove_prefs_list(File,Keys)->({error,Reason}|{ok})
%% where
%%	File = atom()
%%	Keys = list() = [Key1,Key2,...]
%%   Key = atom()
%%	Val = term()
%%	Result = atom()
%%	Reason = atom()
%% @doc remove a list of preference from File
remove_prefs_list(File,[])-> {ok};
remove_prefs_list(File,[Key|B])-> 
	Reason = preferences:remove(File,Key),
    case Reason of
    {error,Reason}  -> {error,Reason};
    _   ->
        remove_prefs_list(File,B)
    end.


%% @spec get_prefs(File,Keys)->({error,Reason}| KeyValues)
%% where
%%	File = atom()
%%	Keys = list() = [key1,key2,...]
%%	KeyValues = list() = [{key1,value1},{key2,value2},...]
%%	Reason = atom()
%% @doc get  preferences values from a File
get_prefs_list(File,Keys)   ->
    get_prefs_list(File,Keys,[]).
    
get_prefs_list(File,[],Values)->    Values;
get_prefs_list(File,[Key|Keys],Values) ->
    case preferences:get(File,Key) of
    {ok,KeyValue} ->
        NewValues = Values ++ KeyValue,
        get_prefs_list(File,Keys,NewValues);
    {error,Reason}  ->
        {error,Reason}
    end.


mail_test(To, Attachment)->
	Sever =
	case get_prefs(email_settings,?MAIL_SERVER) of
		{ok,[{_,V1}|_]}->
			V1;
		_->
			""
	end,
	User = 
	case get_prefs(email_settings,?MAIL_USER) of
		{ok,[{_,V2}|_]}->
			V2;
		_->
			""
	end,
	Password=
	case get_prefs(email_settings,?MAIL_PASSWORD) of
		{ok,[{_,V4}|_]}->
			V4;
		_->
			""
	end,
	From = 
	case get_prefs(email_settings,?FROM_ADDRESS) of
		{ok,[{_,V5}|_]}->
			V5;
		_->
			""
	end,
	case mail:smtp_mail_send(Sever,25,User,Password,From,To,"mail test","This is a test.  This is only a test.", Attachment,?TIMEOUT) of
		ok->
			{ok,"ok"};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end.

report_mail_to(To, Subject, Content, Attachment)->
	Sever =
	case get_prefs(email_settings,?MAIL_SERVER) of
		{ok,[{_,V1}|_]}->
			V1;
		_->
			""
	end,
	User = 
	case get_prefs(email_settings,?MAIL_USER) of
		{ok,[{_,V2}|_]}->
			V2;
		_->
			""
	end,
	Password=
	case get_prefs(email_settings,?MAIL_PASSWORD) of
		{ok,[{_,V4}|_]}->
			V4;
		_->
			""
	end,
	From = 
	case get_prefs(email_settings,?FROM_ADDRESS) of
		{ok,[{_,V5}|_]}->
			V5;
		_->
			""
	end,
	case mail:smtp_mail_send(Sever, 25, User, Password, From, To, Subject, Content, Attachment,?TIMEOUT) of
		ok->
			{ok,"ok"};
		{error,Err}->
			{error,Err};
		Err2->
			{error,Err2}
	end.

%% @spec verify(Mod,Prop,Value)->({error,Reason}|{ok,Result})
%% where
%%	Mod = atom()
%%	Prop = atom()
%%	Value = term()
%%	Result = string()
%%	Reason = string()
%% @doc verify preferences
verify(Mod,Prop,Value)->
	case Mod:verify(Prop,[{Prop,Value}]) of
		{ok,_}->
			true;
		_->
			false
	end.

%% @spec test(Mod,Params)->({error,Reason}|{ok,Result})
%% where
%%	Mod = atom()
%%	Params = list()
%%	Val = term()
%%	Result = atom()
%%	Reason = atom()
%% @doc test preferences  ,params is a key-value tuple list,it is parameter of test.
test(Mod,Params)->
    Result = Mod:test(Params),
	case Result of
		{ok,Ret}->
			{ok,Ret};
		{error,Err}->
			{error,Err};
		Else->
			{error,Else}
	end.

get_all_mail()->
    case get_all('additional_email_settings') of
        {ok,AddtionEmailSettings}   ->  
                [{S#'additional_email_settings'.name,S#'additional_email_settings'.email}|| {ID,S}<-AddtionEmailSettings];
        _   ->[]
    end.
get_all_message()->
    case get_all('additional_sms_settings') of
        {ok,AddtionsmsSettings}   ->  
                [{S#'additional_sms_settings'.name,S#'additional_sms_settings'.email} || {ID,S}<-AddtionsmsSettings];
        _   ->[]
    end.
