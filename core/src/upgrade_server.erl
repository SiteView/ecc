-module(upgrade_server).
-behaviour(gen_server).
-record(state, {
            childproc=[]     %%Upgrade the task sub-process[{taskid, #childproc{}}|T]
            }).
%% Upgrade the task sub-process            
-record(childproc, {
            taskid,         %%task id 
            pid,            %% process pid
            upgradeset,     %%Upgrade settings
            upids           %%Which corresponds to this task[{Commandkey,device_id}|T]
            }).
-define(SERVER,'ecc_upgrade_server').
-include("monitor.hrl").
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-compile(export_all).
         
%% gen_server callbacks

init([]) ->
    State = #state{},
    {ok, State}.

handle_call({submitupgrade,Host,UpSetInfo=#tr069_upgradeset{}}, _From, State) ->       %%Submit to upgrade settings
    {CallPid,_} = _From,
    case spawn_upgrade_proc(UpSetInfo,Host, self(), CallPid) of
        {error, _} ->
            {reply, {error,upgrade_command_fail}, State};
        Pid when erlang:is_pid(Pid) ->
            %%Childproces = State#state.childproc,
            %%TaskId = erlang:ref_to_list(erlang:make_ref()),
            %%NChildProc = Childproces ++ [{TaskId,#childproc{taskid=TaskId,pid=Pid,upgradeset=UpSetInfo}}],
            %%{reply, upgrade_command_fail, State#state{childproc=NChildProc}};
            {reply, {ok, Pid}, State};
        _ ->
            {reply, {error,upgrade_command_fail}, State}
    end;
handle_call(_Info, _From, State) ->
    {reply, other_info, State}.
    
handle_cast(stop, State) ->
    {stop,api_stop_upgrade_server, State};
handle_cast(_Info, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    catch start_link([]).

start_link(Opts) when is_list(Opts) ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, State} ->
            process_flag(trap_exit,true),   %%After the system is set to start the process
            {ok, State};
        {stop, Reason} ->
            {error, Reason};
        {error,{already_started,_}} ->
            {ok, already_started};
        Other ->
            {error, Other}
    end.
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).
    

stop() ->
    cast(stop).

%% Submit to upgrade set
submit_upgrade(Host,UpSetInfo=#tr069_upgradeset{}) ->
    call({submitupgrade,Host,UpSetInfo}),
    loop_upgrade_caller().
    

%% Receive back a message
loop_upgrade_caller() ->
    receive
        {ok, Upids} ->
            {ok, Upids};
        {error,Other, Upids} ->
            {error,Other, Upids};
        {error,Reason} ->
            {error,Reason}
    after 0 ->
        loop_upgrade_caller()
    end.
    
    
%% Internal processing function********

build_devid(M=#tr069_device{}) ->
    Devid =
    M#tr069_device.manufacturer ++ "_" ++
    M#tr069_device.oui ++ "_" ++
    M#tr069_device.serialnumber,
    textutils:space2spechar(Devid).

%% Start a single child process to handle the task to upgrade a single individual
spawn_upgrade_proc(UpSetInfo=#tr069_upgradeset{},Host, GenPid, CallPid) ->
    Pid = spawn_link(fun() -> process_upchildproc(UpSetInfo,Host, GenPid, CallPid) end),
    Pid;
spawn_upgrade_proc(T,Host, GenPid, CallPid) ->
    {error, param_error}.

buildcommandkeyanddevid([]) ->
    [];
buildcommandkeyanddevid([{H,Device=#tr069_device{}}|T]) ->
    %%io:format("H = ~p~n", [H]),
    CommandKey = textutils:guid(),
    [{CommandKey, H}] ++
    buildcommandkeyanddevid(T);  
buildcommandkeyanddevid([H|T]) ->
    buildcommandkeyanddevid(T).

%% In the child process to send a mission to upgrade acs, and will send the results back to the calling process  and genserver
process_upchildproc(UpSetInfo=#tr069_upgradeset{},Host, GenPid, CallPid) ->
    Devids = UpSetInfo#tr069_upgradeset.devices,
    Menufacturer = UpSetInfo#tr069_upgradeset.manufacturer,
    Productclass = UpSetInfo#tr069_upgradeset.productclass,
    Fileversion = UpSetInfo#tr069_upgradeset.version,
    Filename = UpSetInfo#tr069_upgradeset.filename,
    Is_rightrow = UpSetInfo#tr069_upgradeset.is_rightrow,
    Begintime =  
        case UpSetInfo#tr069_upgradeset.begintime of
            [] ->
                0;
            VVV ->
                erlang:list_to_integer(VVV)
        end,
    %%io:format("Devids = ~p~n", [Devids]),
    Ids = buildcommandkeyanddevid(Devids),
    %%io:format("Ids = ~p~n", [Ids]),
    OutTimeSpan = 1200000,      %% timeout
    %% **timeout control**
    api_tr069:delete_upstatus_upgradingSatus(Host, OutTimeSpan),    %% When submitted to the command to remove all state and status updates for the time being to upgrade the state has timed out
    %% **Number of devices control**
    AMountStr =
    case api_tr069:get_upgrade_amount(Host) of
        {ok, UpAmount=#tr069_upgradeamount{}} ->
            UpAmount#tr069_upgradeamount.amount;
        _ ->
            ?DEFAULT_UPAMOUNT
    end,
    AMount = erlang:list_to_integer(AMountStr),
    %%SameCount = api_tr069:stat_upgrading(Host),
    %%io:format("AMount = ~p~n", [AMount]),
    %%io:format("SameCount = ~p~n", [SameCount]),
    %%
    Result = 
    case Is_rightrow of
        "true" ->
            %% Is set to upgrade the status of being
            %%io:format("Ids = ~p~n", [Ids]),
            setnowupgradestatus(Host,Ids,Fileversion,Devids),
            %%
            CallPid ! {ok, wait_upgrade},
            GenPid ! {ok, wait_upgrade},
            try batch_download(Host,Ids,Menufacturer,Productclass,Fileversion,Filename,AMount) of
                RR1 ->
                    RR1
            catch
                _:_ ->
                    {error, submit_fail}
            after
                %% Wait 20 minutes, or remove non-final state,%% commit command is completed, remove all of the specified id is being upgraded, upgrade the status of the waiting
                %% **timeout control**
                %%io:format("submit after~n"),
                %%io:format("Ids = ~p~n", [Ids]),
                spawn_delupstatus_proc(Host, Ids, OutTimeSpan)
            end;
        _ ->
            Now = sv_datetime:now(),
            if 
                Begintime >= Now ->
                    %% Is set to upgrade the status of being
                    setwaitupgradestatus(Host,Ids,Fileversion,Devids),
                    CallPid ! {ok, wait_upgrade},
                    GenPid ! {ok, wait_upgrade},
                    Btime = Begintime - Now,
                    platform:sleep(Btime),
                    setnowupgradestatus(Host,Ids,Fileversion,Devids),
                    try batch_download(Host,Ids,Menufacturer,Productclass,Fileversion,Filename,AMount) of
                        RR ->
                            RR
                    catch
                        _:_ ->
                            {error, submit_fail}
                    after
                        %% Wait 20 minutes, or remove non-final state,%% commit command is completed, remove all of the specified id is being upgraded, upgrade the status of the waiting
                        %% **timeout control**
                        spawn_delupstatus_proc(Host, Ids, OutTimeSpan)
                    end;
                true ->
                    CallPid ! {error, begintime_less_now},
                    GenPid ! {error, begintime_less_now}
            end
    end,
    ok.

%% Construct the device id stringö[{commandkey,id}|T]
%%buildcommandkeyanddevid([]) ->
%%    [];
%%buildcommandkeyanddevid([H|T]) ->
%%    CommandKey = erlang:ref_to_list(erlang:make_ref()),
%%    [{CommandKey, H}] ++
%%%    buildcommandkeyanddevid(T).

%% All the device id is set to wait for the upgrade status
setwaitupgradestatus(Host,[],Fileversion, Devids) ->
    {ok, setok};
setwaitupgradestatus(Host,[{CommandKey, Id}|T],Fileversion, Devids) ->
    case lists:keysearch(Id, 1, Devids) of
        {value, {Id, Dev=#tr069_device{}}} ->
            DateTime = erlang:integer_to_list(sv_datetime:now()),
            Ip = Dev#tr069_device.ip,
            Mac = Dev#tr069_device.serialnumber,
            Group = Dev#tr069_device.label,
            Manufacturer = Dev#tr069_device.manufacturer,
            Oui = Dev#tr069_device.oui,
            Productclass = Dev#tr069_device.productclass,
            Anothorname = Dev#tr069_device.description,
            %%io:format("Manufacturer = ~p~n", [Manufacturer]),
            %%io:format("Oui = ~p~n", [Oui]),
            %%io:format("Productclass = ~p~n", [Productclass]),
            %%io:format("Anothorname = ~p~n", [Anothorname]),
            api_tr069:save_upstatusdb(Host,CommandKey, Id, "1", Fileversion, DateTime,"","",  Ip, Mac, Group, Manufacturer, Oui, Productclass, Anothorname);
        _ ->
            []
    end,
    setwaitupgradestatus(Host,T,Fileversion, Devids);
setwaitupgradestatus(Host,[H|T],Fileversion,Devids) ->
    setwaitupgradestatus(Host,T,Fileversion,Devids).
    
%% All the device id is set to wait for the upgrade status
setnowupgradestatus(Host,[],Fileversion, Devids) ->
    {ok, setok};
setnowupgradestatus(Host,[{CommandKey, Id}|T],Fileversion, Devids) ->
    case lists:keysearch(Id, 1, Devids) of
        {value, {Id, Dev=#tr069_device{}}} ->
            DateTime = erlang:integer_to_list(sv_datetime:now()),
            Ip = Dev#tr069_device.ip,
            Mac = Dev#tr069_device.serialnumber,
            Group = Dev#tr069_device.label,
            Manufacturer = Dev#tr069_device.manufacturer,
            Oui = Dev#tr069_device.oui,
            Productclass = Dev#tr069_device.productclass,
            Anothorname = Dev#tr069_device.description,
            %%io:format("Manufacturer = ~p~n", [Manufacturer]),
            %%io:format("Oui = ~p~n", [Oui]),
            %%io:format("Productclass = ~p~n", [Productclass]),
            %%io:format("Anothorname = ~p~n", [Anothorname]),
            api_tr069:save_upstatusdb(Host,CommandKey, Id, "2", Fileversion, DateTime,DateTime,"",  Ip, Mac, Group, Manufacturer, Oui, Productclass, Anothorname);
        _ ->
            []
    end,
    setnowupgradestatus(Host,T,Fileversion, Devids);
setnowupgradestatus(Host,[H|T],Fileversion,Devids) ->
    setnowupgradestatus(Host,T,Fileversion,Devids).

%% Batch command to submit multiple equipment upgrade
batch_download(Host,DeviceIds,Menufacturer,Productclass,Fileversion,Filename,AMount) ->
    Upsite = api_tr069:get_site(Host),
    DelaySeconds = "0",
    SuccessURL = "",
    FailureURL = "",
    Result =
    case Upsite of
        Us = #tr069_upgradesite{} ->
            Username = Us#tr069_upgradesite.username,
            Password = Us#tr069_upgradesite.password,
            Manufacturer_t = textutils:biastoEfficfilename(Menufacturer),
            Productclass_t = textutils:biastoEfficfilename(Productclass),
            FileVersion_t = textutils:biastoEfficfilename(Fileversion),
            Filename_t = textutils:biastoEfficfilename(Filename),
            UpFile = api_tr069:get_file_bymenu_prd_ver_name(Host,Menufacturer, Productclass, Fileversion, Filename),
            URL1 =  Manufacturer_t ++ "/" ++ Productclass_t ++ "/" ++ FileVersion_t ++ "/" ++ Filename_t,
            %%"http://" ++ Us#tr069_upgradesite.devaccessaddr++"/" ++
            URL = 
                case string:str(Us#tr069_upgradesite.devaccessaddr, "http://") of
                    1 ->
                       Us#tr069_upgradesite.devaccessaddr++"/" ++URL1;
                    _ ->
                        "http://" ++ Us#tr069_upgradesite.devaccessaddr++"/" ++URL1
                end,
            %%io:format("URL = ~p~n", [URL]),
            case UpFile of
                [] ->
                    {error, upfile_empty};
                [VSS=#tr069_upgradefile{}] ->
                    FileType = VSS#tr069_upgradefile.filetype,
                    FileSize = VSS#tr069_upgradefile.filesize,
                    TargetFileName = VSS#tr069_upgradefile.filename,
                    case DeviceIds of
                        [] ->
                            {error,device_empty};
                        V when erlang:is_list(V) ->
                                %%io:format("DeviceIds"),
                                try exe_download(Host,V,FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion,[], AMount) of
                                    VV1 ->
                                        VV1
                                catch
                                    _:_ ->
                                        {error, other_error}
                                end;
                        _ ->
                            {error,device_error}
                    end;
                _ ->
                    {error, upfile_read_error}
                    
            end;    
        _ ->
            {error, site_empty_error}
    end,
    Result.
    

exe_download(Host,[],FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion,Upids,AMount) ->
    {ok, Upids};
exe_download(Host,[{CommandKey,H}|T],FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion, Upids,AMount) ->
    SameCount = api_tr069:stat_upgrading(Host),
    if
        SameCount < AMount ->
            io:format("ok begin~n"),
            io:format("SameCount = ~p~n", [SameCount]),
            io:format("AMount = ~p~n", [AMount]),
            io:format("H = ~p~n", [H]),
            io:format("URL = ~p~n", [URL]),
            io:format("TargetFileName = ~p~n", [TargetFileName]),
            case api_tr069:download(Host,H,CommandKey,FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds,SuccessURL,FailureURL) of
                {ok,Results} ->
                    io:format("Results = ~p~n", [Results]),
                    DateTime = erlang:integer_to_list(sv_datetime:now()),
                    case api_tr069:get_upstatus(Host,CommandKey) of
                        {ok, Stas=#tr069_upgradestatus{}} ->
                            case Stas#tr069_upgradestatus.upgradestatus of
                                "1" ->
                                    api_tr069:save_upstatusdb(Host,CommandKey, H, "2", Fileversion, DateTime, DateTime, "");
                                _ ->
                                    []
                            end;
                        _ ->
                            []
                    end,
                    exe_download(Host,T,FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion,Upids++[{CommandKey,H}],AMount);
                Ots ->
                    io:format("Ots = ~p~n", [Ots]),
                    DateTime = erlang:integer_to_list(sv_datetime:now()),
                    api_tr069:save_upstatusdb(Host,CommandKey,H, "3", "", DateTime, "", ""),
                    exe_download(Host,T,FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion,Upids,AMount)
            end;
        true ->
            io:format("already to maxupdevice~n"),
            io:format("SameCount = ~p~n", [SameCount]),
            io:format("AMount = ~p~n", [AMount]),
            io:format("H = ~p~n", [H]),
            platform:sleep(240000), %%240000
            exe_download(Host,[{CommandKey,H}|T],FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion,Upids,AMount)
    end;
exe_download(Host,[H|T],FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion, Upids,AMount) ->
    exe_download(Host,T,FileType,URL,Username,Password,FileSize,TargetFileName,DelaySeconds, SuccessURL, FailureURL,Fileversion,Upids,AMount).

%% Remove the state process to handle
proc_delupstatus_proc(Ids ,Host, TimeSpan) ->
    platform:sleep(TimeSpan),
    del_download_timeout(Ids ,Host).
    
%% Heavy task in one submission, from this mission to upgrade the status of all devices into a wait start time, if not 20 minutes into the upgrade is complete or upgrade fails
%% So that the mission failed, the upgrade task to upgrade all the equipment status data will be deleted
del_download_timeout([], Host) ->
    [];
del_download_timeout([{CommandKey,H}|T], Host) ->
    api_tr069:delete_upstatus_nottoendSatus(Host, CommandKey),
    del_download_timeout(T, Host);
del_download_timeout([K|T], Host) ->
    del_download_timeout(T, Host).
    
    
%% Start a single child process to handle the task to upgrade a single individual
spawn_delupstatus_proc(Host, Ids, TimeSpan) ->
    Pid = spawn(fun() -> proc_delupstatus_proc(Ids ,Host, TimeSpan) end),
    Pid.

    
    
    