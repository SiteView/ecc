-module(snmp_trap_data).
-behaviour(gen_server).
-record(state, {parent,file,mdate,iodevice}).
-define(SERVER,'ecc_snmp_trap_data').
-define(LOGNAME, "logs/snmptrap.log").
-include_lib("kernel/include/file.hrl").

-export([log/1, stop/0, getFileLength/0, exists/0, nowDT/0, getlocalAddr/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         

%% gen_server callbacks

init([]) ->
    {Year,Month,Day} = date(),
    case file:open(?LOGNAME, [write,append]) of
        {ok, S} ->
            State = #state{file=?LOGNAME,mdate={Year,Month,Day}, iodevice=S},
            {ok, State};
        {error, Reason} ->
            {stop, Reason};
        Other ->
            Other
    end.
    
handle_call({write_snmp_trap, Content}, _From, State) ->
    Io_Device = State#state.iodevice,
    %%io:format("*********IO_Device~n~p~n", [Io_Device]),
    io:format(Io_Device, nowDT()++"\t"++Content++"~n", []),
    Reply = ok,
    {reply, Reply, State}.
    
handle_cast(stop, State) ->
    {stop,close_file, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    file:close(State#state.iodevice),
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    case gen_server:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {ok, State} ->
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
    
    
log(Content) ->
    case start_link() of
        {ok, _} ->
            call({write_snmp_trap, Content});
            
            %%{ok, State#state.iodevice};
            
        {stop, Reason} ->
            {error, Reason};
        Other ->
            {error, Other}
    end.

getFileLength() ->
    case file:read_file_info(?LOGNAME) of
            {ok, FileInfo} ->
                Length = FileInfo#file_info.size;
            {error, Reason} ->
                Length = -1;
            _ ->
                Length = -1
    end,
    Length.
    
    
exists() ->
    case file:read_file_info(?LOGNAME) of
            {ok, FileInfo} ->
                isExists;
            {error, Reason} ->
                isNotExists;
            _ ->
                isNotExists
    end.
    
nowDT() ->
     {Hour, Minute, Second} = time(),
     {Year, Month, Day} = date(),
     integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second)
     ++ " " ++ integer_to_list(Month) ++ "/" ++ integer_to_list(Day) ++ "/" ++ integer_to_list(Year).
    
getlocalAddr() ->
    {ok, Host} = inet:gethostname(),
    case inet:getaddr(Host, inet) of
        {ok, Address} ->
            Address;
        {error, Reason} ->
            {127,0,0,1};
        _ ->
            {127,0,0,1}
    end.

stop() ->
    cast(stop).
            
    
    
    
