-module(df_util).

-compile(export_all).
-include_lib("df_snmp_record.hrl").


for_up(Max, Max, F) -> [F(Max)];
for_up(I, Max, F)   -> [F(I)|for_up(I+1, Max, F)].


fib(0) -> 1;
fib(1) -> 1;
fib(N) -> N*fib(N-1).



iP_Num(IP) when is_list(IP) ->
    iP_Num(list_to_tuple(IP));

iP_Num({A,B,C,D}) ->
    A*256*256*256+B*256*256+C*256+D;
    

iP_Num(_) -> 0.
    
num_IP(Num) ->
    D = Num rem 256,
    C = Num div 256 rem 256,
    B = Num div (256*256) rem 256,
    A = Num div (256*256*256) rem 256,
    {A,B,C,D}.

start() ->
    %%getIP_list({{192,168,0,254},{255,255,0,0}}).
    getIP_list({{222,240,176,195},{255,255,255,248}}).



timeSub({A1,A2,A3},{B1,B2,B3}) ->
    {B1-A1,B2-A2,B3-A3}.


getIP_lists(IPMsk_list) ->
    lists:umerge(lists:map(fun(IM) -> getIP_list(IM) end,IPMsk_list)).

getWebMsk({IP,Msk}) ->
    IP_num  = iP_Num(IP),
    Msk_num = iP_Num(Msk),
    Web_ad  = IP_num band Msk_num,
    All     = iP_Num({255,255,255,255}) bxor Msk_num,
    {num_IP(Web_ad),All}.
    
getWeb_ad({IP,Msk}) ->
    IP_num  = iP_Num(IP),
    Msk_num = iP_Num(Msk),
    Web_ad  = IP_num band Msk_num,
    {num_IP(Web_ad),Msk}.
    
getIP_list2({Web_ad,All}) ->
    for_up(Web_ad+1,All+Web_ad-1,fun(X) -> num_IP(X) end).

getIP_list1({IP,Msk}) ->
    IP_num  = iP_Num(IP),
    Msk_num = iP_Num(Msk),
    Web_ad  = IP_num band Msk_num,
    %%io:format("IP=~p  Msk=~p Web=~p~n",[IP_num,Msk_num,num_IP(Web_ad)]),
    case iP_Num({255,255,255,255}) bxor Msk_num of 
        0 -> {0,{num_IP(Web_ad)}};
        1 -> {1,{num_IP(Web_ad),num_IP(Web_ad+1)}};
        R -> IP_list = for_up(Web_ad+1,R+Web_ad-1,fun(X) -> num_IP(X) end) ,
             {ok,IP_list,{num_IP(Web_ad),num_IP(Web_ad+R)}}
    end.
    
getIP_list({IP,Msk}) ->
    case getIP_list1({IP,Msk}) of
        {ok,IP_list,_} -> lists:usort(IP_list);
        _               -> []
    end.


c10_16(Num) when is_integer(Num),Num>=0,Num<16 ->
    case Num of
        0  -> "0";
        1  -> "1";
        2  -> "2";
        3  -> "3";
        4  -> "4";
        5  -> "5";
        6  -> "6";
        7  -> "7";
        8  -> "8";
        9  -> "9";
        10 -> "A";
        11 -> "B";
        12 -> "C";
        13 -> "D";
        14 -> "E";
        15 -> "F"
    end;
c10_16(_) -> wrong.

ready(I) ->
    Server = {192,168,0,I},
    #snmpPara{
                    server    = Server,
                    port      = 5000+I,
                    community = "public1",
                    timeout   = 2000,
                    retry     = 2
              }.

    
    
getDevidSP([Devid|Devid_list],SP_list) ->
    Type = Devid#deviceInfo.devType,
    Flag = Devid#deviceInfo.snmpFlag,
    if
        Flag=:=1 ->
            if
                Type=:=0;Type=:=1;Type=:=2 ->
                    getDevidSP(Devid_list,[Devid#deviceInfo.sp|SP_list]);
                true ->
                    getDevidSP(Devid_list,SP_list)
            end;
        true -> 
            getDevidSP(Devid_list,SP_list)
    end.    
    
   
    
    




