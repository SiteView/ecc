-module(tr069_CpeCache).
-define(CpeCache,'tr069_device_cache').
-define(Author,<<"ning.cai@dragonflow.com">>).

-include("monitor.hrl").
-behaviour(gen_server).
-record(state, {
                    update_cep=dict:new()       %% 待修改的cpe, 如果没有制定值，初始化为一个hash表
                    }).
-define(SERVER,'ecc_tr069_devicecache_server').
-include("config.hrl"). 
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
         

%% gen_server callbacks

init([]) ->
    ets:new(?CpeCache, [set,public,named_table]),
    {ok, #state{}}.

handle_call({getCpeByCache, Host, CpeId}, _From, State) ->
    %%io:format("Host !!!!!!!!!!!!!!!!!!!!~n~p~n", [Host]),
    HostName = dbcs_tr069:domain(Host),
    %%HostName = Host,
    Reply =
    case ets:member(?CpeCache, HostName) of
        true ->
            [{HostName, Dict}] = ets:lookup(?CpeCache, HostName),
            case dict:find(CpeId, Dict) of
                {ok, Value} ->
                    Value;
                _ ->
                    []
            end;
        _ ->
            CacheObject = {HostName, dict:new()},
            ets:insert(?CpeCache, CacheObject),
            []
    end,
    %%io:format("HostName !!!!!!!!!!!!!!!!!!!~n~p~n", [HostName]),
    %%io:format("Cpe !!!!!!!!!!!!!!!!!!!~n~p~n", [Reply]),
    {reply, Reply, State};
handle_call({saveCpeByCache, Host, Cpe}, _From, State) ->
    %%io:format("Host !!!!!!!!!!!!!!!!!!!!~n~p~n", [Host]),
    HostName = dbcs_tr069:domain(Host),
    %%HostName = Host,
    CpeId = tr069_tool:genDeviceId(Cpe),
    Dict =
    case ets:member(?CpeCache, HostName) of
        true ->
            [{HostName, Dict1}] = ets:lookup(?CpeCache, HostName),
            Dict1;
        _ ->
            dict:new()
    end,
    %%io:format("HostName !!!!!!!!!!!!!!!!!!!~n~p~n", [HostName]),
    %%io:format("Cpe !!!!!!!!!!!!!!!!!!!~n~p~n", [Cpe]),
    NDict = dict:store(CpeId, Cpe, Dict),
    CacheObject = {HostName, NDict},
    ets:insert(?CpeCache, CacheObject),
    Reply = {ok, "cache cpe ok"},
    {reply, Reply, State};
handle_call({deleteCpeByCache, Host, CpeId}, _From, State) ->
    %%io:format("Host !!!!!!!!!!!!!!!!!!!!~n~p~n", [Host]),
    HostName = dbcs_tr069:domain(Host),
    %%HostName = Host,
    Dict =
    case ets:member(?CpeCache, HostName) of
        true ->
            [{HostName, Dict1}] = ets:lookup(?CpeCache, HostName),
            Dict1;
        _ ->
            dict:new()
    end,
    %%io:format("HostName !!!!!!!!!!!!!!!!!!!~n~p~n", [HostName]),
    NDict = dict:erase(CpeId, Dict),
    CacheObject = {HostName, NDict},
    ets:insert(?CpeCache, CacheObject),
    Reply = {ok, "delete cpe ok"},
    {reply, Reply, State};
handle_call({member, Host, CpeId}, _From, State) ->
    HostName = dbcs_tr069:domain(Host),
    HostName = dbcs_tr069:domain(Host),
    %%HostName = Host,
    Reply =
    case ets:member(?CpeCache, HostName) of
        true ->
            [{HostName, Dict}] = ets:lookup(?CpeCache, HostName),
            dict:is_key(CpeId, Dict);
        _ ->
            CacheObject = {HostName, dict:new()},
            ets:insert(?CpeCache, CacheObject),
            false
    end,
    %%io:format("HostName !!!!!!!!!!!!!!!!!!!~n~p~n", [HostName]),
    {reply, Reply, State};
handle_call(_Info, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop,close_file, State};
handle_cast(_Info, State) ->
    {noreply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
    
terminate(_Reason, State) ->
    ok.
    
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% API

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    case erlang:whereis(?SERVER) of
        undefined ->        %% 没有启动
            gen_server:start_link({local, ?SERVER}, ?MODULE, [], []);
        _ ->                %% 已经启动
            {error, "existed"}
    end.
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
    
cast(Req) ->
    gen_server:cast(?SERVER, Req).
    
    
    


%% ** 缓存数据操作接口 **     通过genserver

% 查询 
getCpeByCache(Host, CpeId) ->
    call({getCpeByCache, Host, CpeId}).

% 保存
saveCpeByCache(Host, Cpe) ->
    call({saveCpeByCache, Host, Cpe}).

% 删除
deleteCpeByCache(Host, CpeId) ->
    call({deleteCpeByCache, Host, CpeId}).
    
member(Host, CpeId) ->
    call({member, Host, CpeId}).

%% **