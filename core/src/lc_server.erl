-module(lc_server).
-behaviour(gen_server).
-include("monitor.hrl").

-record(state, {license=[]}).

-define(SERVER,'elecc_lc_server').

-export([start_link/0, getLicense/0, getMachineCode/0, takeEffectLicense/0, setLicense/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         
%% Mac
-define(DIR_LIB, "priv/lib/").
-define(FILE_MACCODE, "sysid.txt").
         

%% gen_server callbacks

init(Opts) ->
    State = takeEffectLicense([]),
	{ok,State}.
    
handle_call({getLicense}, _From, State) ->
    {reply,State#state.license,State};
handle_call({takeEffectLicense}, _From, State) ->
    NState = takeEffectLicense(State),
    {reply, NState#state.license, NState};
handle_call({getMachineCode}, _From, State) ->
    Fun = 
        fun() ->
            proc_getMachineCode(_From)
        end,
    erlang:spawn(Fun),
    {reply, ok, State};
handle_call({setLicense, LicenseStr}, _From, State) ->
    Fun = 
        fun() ->
            proc_setLicense(LicenseStr, _From)
        end,
    erlang:spawn(Fun),
    {reply, ok, State};
handle_call(Other, _From, State) ->
    {reply, {error, notsupport},State}.


handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).

 
%% api 

%% 从genserver进程中获取到license
getLicense() ->
    call({getLicense}).

%% 设置license到genserver进程
takeEffectLicense()->
    call({takeEffectLicense}).

%% 上传license文件到服务端目录
setLicense(LicenseStr) ->
    call({setLicense, LicenseStr}),
    receive
        {ok, "setLicenseOk"} ->
            {ok, "setLicenseOk"};
        _ ->
            {error, "setLicenseError"}
    after 10000 ->
        ""
    end.
    

%% 获取机器码
getMachineCode() ->
    call({getMachineCode}),
    receive
        Code when erlang:is_list(Code) ->
            Code;
        _ ->
            ""
    after 10000 ->
        ""
    end.


stop() ->
    cast(stop).
    
    
%% tool
valideLicense(Lc, St) ->
    L =
    case license:isvalidlicense("",Lc,0,"false") of
        {ok, match} ->
            io:format("******************~n"),
            io:format("validlicense ok...~n"),
            io:format("******************~n"),
            Lc;
        Other ->
            io:format("LicenseStr: ~p~n", [Lc]),
            io:format("LicenseOther: ~p~n", [Other]),
            io:format("******************~n"),
            io:format("validlicense error~n"),
            io:format("******************~n"),
            erlang:list_to_binary("")
    end,
    case St of
        S = #state{} ->
            S#state{license=L};
        _ ->
            #state{license=L}
    end.

%% 获取机器码
proc_getMachineCode(_From) ->
    LibPath = ?DIR_LIB,
    Result =
    case os:type() of
        {win32,nt} ->
            %%filename:nativename(
            ExePath = LibPath ++ "getSysID.exe",
            OsExePath = filename:nativename(ExePath),
            os:cmd(OsExePath);
        _ ->
            ExePath = LibPath ++ "sysid",
            OsExePath = filename:nativename(ExePath),
            os:cmd(OsExePath)
    end,
    MachineCode =
    case filelib:is_file(?FILE_MACCODE) of
        true ->
            case file:read_file(?FILE_MACCODE) of
                {ok, Binary} ->
                    erlang:binary_to_list(Binary);        
                _ ->
                    ""
            end;
        _ ->
            ""
    end,
    {Pid, _} = _From,
    Pid ! MachineCode.

%% 上传license文件
proc_setLicense(LicenseStr, _From) ->
    Result =
    case file:write_file(?ECCLICFILENAME, erlang:list_to_binary(LicenseStr)) of
        ok ->
            {ok, "setLicenseOk"};
        _ ->
            {error, "setLicenseError"}
    end,
    {Pid, _} = _From,
    Pid ! Result.

%% 初始化license服务模块
takeEffectLicense(St) ->
    %% 启动license 接口
    license:start(),
    Lc =
    case license:getlicstring(?ECCLICFILENAME) of
        {ok,License} ->
            License;
        Other ->
            io:format("takeEffectLicense: ~p~n", [Other]),
            erlang:list_to_binary("")
    end,
    State = valideLicense(Lc, St).
    