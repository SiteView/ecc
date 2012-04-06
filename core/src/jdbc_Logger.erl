%% @author Cheng kaiyang <kaiyang.cheng@dragonflow.com>
%% @copyright 2009 dragonflow, Inc.
%% @version 1.0
%% @doc jdbc database logger. 
-module(jdbc_Logger).
-compile(export_all).
-behaviour(gen_server).

-export([log/2,test/1, test/0]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,terminate/2,init/1]).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include("db_monitor_base.hrl").

-define(SERVER,'elecc_jdbc_Logger').
-define(LOG_JDBC_URL,logJdbcURL).
-define(LOG_JDBC_DRIVER,logJdbcDriver).
-define(LOG_JDBC_USER,logJdbcUser).
-define(LOG_JDBC_PASSWORD,logJdbcPassword).
-define(LOG_JDBC_URL_BACKUP,logJdbcURLBackup).
-define(LOG_JDBC_ENCODE,logjdbcencode).
-define(LOG_JDBC_AVAILABLE,logjdbcavailable).
-define(RUN_STATE,runstate).
-define(DEFENCODE,httputils:pageEncode()).
-define(DefaultTable,"CREATE TABLE SiteViewLog (datex VARCHAR(255), serverName VARCHAR(255), class VARCHAR(255), sample VARCHAR(255), category VARCHAR(255), groupName VARCHAR(255), monitorName VARCHAR(255), status VARCHAR(255), monitorID VARCHAR(255), value1 VARCHAR(255), value2 VARCHAR(255), value3 VARCHAR(255), value4 VARCHAR(255), value5 VARCHAR(255), value6 VARCHAR(255), value7 VARCHAR(255), value8 VARCHAR(255), value9 VARCHAR(255), value10 VARCHAR(255))").
-define(DefaultInsert,"INSERT INTO SiteViewLog VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)").


start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
    
init(Args) ->
    {ok,Args}.
    
stop() ->
    gen_server:cast(?MODULE, stop).

%% @spec log(MonitorLog) -> ok
%% where
%% MonitorLog = record()
%% StateString = string()
%% ErrorInfo = string()
%% @create a database tabel and insert content to this table,the table format is depends on logJdbcInsert and logJdbcCreate.
log(App,MonitorLog) ->
    gen_server:cast(?MODULE, {log,App,MonitorLog}).

test() ->
    St = case api_preferences:get_all(log) of
			{ok,Ret}->
				Ret;
			_->
				[]
    end,
    Args = [  
                    {logJdbcURL,proplists:get_value(?LOG_JDBC_URL,St,"")},  
                    {logJdbcDriver,proplists:get_value(?LOG_JDBC_DRIVER,St,"")},
                    {logJdbcUser,proplists:get_value(?LOG_JDBC_USER,St,"")},
                    {logJdbcPassword,proplists:get_value(?LOG_JDBC_PASSWORD,St,"")},
                    {logJdbcURLBackup,proplists:get_value(?LOG_JDBC_URL_BACKUP,St,"")}
                 ],
    test(Args).

%% @spec test(St) -> Result
%% where
%% St = list()
%% Result = [{ok,Good}|{error,Error}]
%% Good = list()
%% Error = list()
%% @test the appointed jdbc can be reached.
test(St) ->
    Args = [
                    {logJdbcURL,proplists:get_value(?LOG_JDBC_URL,St,"")},  
                    {logJdbcDriver,proplists:get_value(?LOG_JDBC_DRIVER,St,"")},
                    {logJdbcUser,proplists:get_value(?LOG_JDBC_USER,St,"")},
                    {logJdbcPassword,proplists:get_value(?LOG_JDBC_PASSWORD,St,"")},
                    {logJdbcURLBackup,proplists:get_value(?LOG_JDBC_URL_BACKUP,St,"")}
                ],
    T1 = httputils:timeMillis(),
    Response = send_request("setupConnection",Args),
    T2 = httputils:timeMillis(),
    io:format("response:~p, ~p(s)~n", [Response, round((T2 - T1)/1000)]),
    Available = case Response of
        {error,_} ->
            false;
        _ ->
            true
    end,
    api_preferences:set_prefs(log,?LOG_JDBC_AVAILABLE,Available),
    Response.



handle_cast({log,App,MonitorLog},State) ->
    St = case api_preferences:get_all(log) of
			{ok,Ret}->
				Ret;
			_->
				[]
    end,
    Args = [  
                    {logJdbcURL,proplists:get_value(?LOG_JDBC_URL,St,"")},  
                    {logJdbcDriver,proplists:get_value(?LOG_JDBC_DRIVER,St,"")},
                    {logJdbcUser,proplists:get_value(?LOG_JDBC_USER,St,"")},
                    {logJdbcPassword,proplists:get_value(?LOG_JDBC_PASSWORD,St,"")},
                    {logJdbcURLBackup,proplists:get_value(?LOG_JDBC_URL_BACKUP,St,"")},
                    {logJdbcInsert,proplists:get_value(logJdbcInsert,St,?DefaultInsert)},
                    {logJdbcCreate,proplists:get_value(logJdbcCreate,St,?DefaultTable)}
                 ],
    Available = proplists:get_value(?LOG_JDBC_AVAILABLE,St,false),
    if
        (not Available) ->
            nothing;
        true ->
            Encode = proplists:get_value(?LOG_JDBC_ENCODE,St,"GBK"),
            MonitorState = create_content(App,MonitorLog,Encode),
            send_request("log",Args++MonitorState)
    end,
    {noreply,State};
handle_cast(stop, S) ->
    {stop, normal, S};
handle_cast(_, State) ->
    {noreply, State}.

create_content(App,#monitorlog{id = ID,name = Name,time = Time,category = Category,
	desc = Desc,measurement = Measurement,class=Class,groupname=GN},Encode) ->
    T = process_time(Time),
    % MonitorInfo = case dbcs_monitor:get_monitor(ID) of
        % {error,_}->
            % [];
        % Other ->
            % Other
    % end,
    % Gid = proplists:get_value(parent,MonitorInfo),
    % GN = if
        % Gid=/=undefined ->
            % GInfo = dbcs_group:get_group(Gid),
            % proplists:get_value(name,GInfo,"");
        % true ->
            % ""
    % end,
    Result = 
    [
        {date,T},
        {serverName,App},
        {class,atom_to_list(Class)},
        {sample,""},
        {category,atom_to_list(Category)},
        {groupName,iconv:convert(?DEFENCODE,Encode,GN)},
        {monitorName,iconv:convert(?DEFENCODE,Encode,Name)},
        {status,iconv:convert(?DEFENCODE,Encode,Desc)},
        {monitorID,atom_to_list(ID)}
    ],
	% io:format("~p:~p~n",[?MODULE,Result]),
    filter(Result++process_measure(Measurement,[],1),[]);
create_content(_,_,_) ->[].

process_measure([],Result,_) ->Result;
process_measure([{_,V}|R],Result,N)->
    if
        N>10 ->
            Result;
        true ->
            NewV = if
                is_list(V) ->
                    lists:flatten(V);
                true ->
                    lists:flatten(io_lib:format("~p",[V]))
            end,
            process_measure(R,Result++[{list_to_atom("value"++integer_to_list(N)),NewV}],N+1)
    end;
process_measure([_|R],Result,N) ->
    process_measure(R,Result,N).

filter([],Result)->Result;
filter([{K,V}|R],Result) ->
    NewV = if
        length(V)>255 ->
            string:sub_string(V,1,255);
        true ->
            V
    end,
    filter(R,Result++[{K,NewV}]);
filter([_|R],Result)->
    filter(R,Result).

process_time(Time) ->
    case Time of
        {{Y,M,D},{HH,MM,SS}}->
                lists:flatten(io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w",[Y,M,D,HH,MM,SS]));
            _->
                ""
	end.

handle_call(_, _, State) ->
    {noreply, State}.
                
send_request(Oper,Args) ->
    %%build request args
    Request = {"com.dragonflow.erlangecc.log.jdbcLogger", Oper, Args},
    
    %%get java node
    Java_Node = siteview:get_java_node(),
    
    %%send request to java and receive the result
    rpc(?REG_NAME, Java_Node, Request).



rpc(RegName, Node, Msg) ->
	%%ping the java node
	Ping = net_adm:ping(Node),
    if
        Ping==pang ->
            {error,"Connect Java Node Error! "};
        true ->
            %%send request to java node
            {RegName, Node} ! Msg,	

            %%try to get result
            receive
                %%some thing error
                {error, _From, Ret} ->
                    lists:nth(1,Ret);
                %%get  result	
                {ok, _From, Ret} ->
                    lists:nth(1,Ret);
                Any ->
                    Any
                %%time out	
                after 60000 ->
                    {error,"time out"}
            end
    end.

handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
