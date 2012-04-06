-module(nmap_manage).
-compile(export_all).

start(IPString,N,NmapString,App,User)when is_list(App),is_list(User)->
    DetsStr = App++":"++User,
    pool_manage:do_action(IPString,N,NmapString,DetsStr).

start(IPString, User)->
    App = dbcs_base:get_app(),
    start(IPString, 20, "  -T3 -sS -O -F ", atom_to_list(App), User).

stop()->
    pool_manage :stop_all(),
    process_pool:stop_all().
    
read_all()-> pool_manage:all_result().

%%前面是地址段，第二个数字式一次的地址数，第三个参数是，nmap配置参数，后面是APP和user  如果同时扫描，那么应该传入不同的user来区分.
%%如果只是tcp的扫描，如下，1,2,3 可以把一个扫描的地址设置的多一些，这样会比较快 10-15之间比较好。
%%如果加入了udp的扫描，可以把每次的扫描的地址设置的少一些，这样会快一点 1-5之间比较好。 
%% -sS , -sT  tcp的扫描 -sU  udp的扫描 -O 系统扫描 -F快速扫描
test1()->
    start("192.168.0.1-255",15," -T4 -F  -O ","localhost","henry"). 
    
test2()->
    start("192.168.0.1-255",5," -T3 -sS  -O ","localhost","henry"). 

test3()->
    start("192.168.0.1-255",20,"  -T3 -sS -O -F " ,"localhost","henry"). 
%%块
test4()->
    start("192.168.0.1-255",20,"  -T5 -sP" ,"localhost","henry"). 
    
test5()->
    start("192.168.0.1-255",20,"  -T5 -sS -O" ,"localhost","henry"). 
    
get_result(User)->
    App = dbcs_base:get_app(),
    process_pool:get("match_user_result", atom_to_list(App)++":"++User).

%%扫描的进度，当返回1.0的时候扫描结束.
get_rate(User)->
    App = dbcs_base:get_app(),
    N1 = pool_manage:call_n(),
    N2 = length(process_pool:get("match_user",atom_to_list(App)++":"++User)), 
    round(N2/N1*100).
    
get_statestring(User) ->
    Result = get_result(User),
    parse_result(Result, []).
    
parse_result([], Result) ->lists:reverse(Result);
parse_result([F|R], Result) ->
    IP = proplists:get_value(ip, F, "unkown"),
    Ports = proplists:get_value(service, F, []),
    OS = proplists:get_value(os, F, []),
    S = "Host: "++IP++"(" ++ proplists:get_value(type,OS,"") ++ ") port: " ++ integer_to_list(length(Ports)),
    parse_result(R, [S|Result]).
    
    