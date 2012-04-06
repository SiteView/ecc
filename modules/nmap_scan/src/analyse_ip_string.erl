-module(analyse_ip_string).
-compile(export_all).
%%输入字符串"192.168.0.1-192"or"192.168.0.1-2.192"or"192.168.0.1-169.2.192"or"192.168.0.1-192.169.2.192"。
%%将会被解析成一个list，包含的是这个字符串中对应的IPAddress如:["192.168.0.1","192.168.0.2","192.168.0.3",------,""192.168.0.192""].



analy_IPString(IPValue)->
    IPStrings = string:tokens(IPValue,","),
    case length(IPStrings)>0 of
        true->
            analy_IPString2(IPStrings,[]);
        _->
        {error,unknow_string}
    end.
analy_IPString2([],Result)-> Result;
analy_IPString2([IPString|NEXT],Result)->
    analy_IPString2(NEXT,analy_IP(IPString)++Result).
analy_IP(IPString)->
    IPStringlist = string:tokens(IPString,"-"),
    case length(IPStringlist) of
    0->[];
    1->IPStringlist;
    2->
        IP1 = lists:nth(1,IPStringlist),
        IP2 = lists:nth(2,IPStringlist),
        getIPString(IP1,IP2);
    _->{error,unknow_ip_string}
    end.

getIPString(IP1,IP2)->
    IP1list = string:tokens(IP1,"."),
    case length(IP1list) =:= 4 of 
    true->
        [A1,B1,C1,D1] = IP1list,
        IP2list = string:tokens(IP2,"."),
        case length(IP2list) of
        1->
            [D2] = IP2list,
            getIPString_int({list_to_integer(A1),list_to_integer(B1),list_to_integer(C1),list_to_integer(D1)},{list_to_integer(A1),list_to_integer(B1),list_to_integer(C1),list_to_integer(D2)});
        2-> 
            [C2,D2] = IP2list,
            getIPString_int({list_to_integer(A1),list_to_integer(B1),list_to_integer(C1),list_to_integer(D1)},{list_to_integer(A1),list_to_integer(B1),list_to_integer(C2),list_to_integer(D2)});
        3->    
            [B2,C2,D2] = IP2list,
            getIPString_int({list_to_integer(A1),list_to_integer(B1),list_to_integer(C1),list_to_integer(D1)},{list_to_integer(A1),list_to_integer(B2),list_to_integer(C2),list_to_integer(D2)});
        4->
            [A2,B2,C2,D2] = IP2list,
            getIPString_int({list_to_integer(A1),list_to_integer(B1),list_to_integer(C1),list_to_integer(D1)},{list_to_integer(A2),list_to_integer(B2),list_to_integer(C2),list_to_integer(D2)});
        _->
            {error,unknow_ip_string}
        end;    
    _-> {error,unknow_ip_string}
    end.
    
validateAddress({A1,B1,C1,D1},{A2,B2,C2,D2})->
    case A2<A1 andalso A1>255 andalso A1<0 andalso A2>255 andalso A2<0 of
    true-> {error,unknow_ip_string};
    _->
        case A2=:=A1 of
        true->
            case  B2<B1 andalso B1>255 andalso B1<0 andalso B2>255 andalso B2<0 of
            true->
                {error,unknow_ip_string};
            _->
                case B2=:=B1 of
                true->
                    case C2<C1 andalso C1>255 andalso C1<0 andalso C2>255 andalso C2<0 of
                    true->
                        {error,unknow_ip_string};
                    _->
                        case C2=:=C1 of
                         true->
                            case D2<D1 andalso D1>255 andalso D1<0 andalso D2>255 andalso D2<0 of
                            true->
                                {error,unknow_ip_string};
                            _-> ok
                            end;
                        _-> ok
                        end
                    end;
                _->ok
                end
            end;
         _->ok
        end    
    end.
getIPString_int({A1,B1,C1,D1},{A2,B2,C2,D2})->
    case  validateAddress({A1,B1,C1,D1},{A2,B2,C2,D2}) of
    ok->
        case A1 =:= A2 of
        true->
            case B1 =:= B2 of
            true->
                case C1 =:= C2 of
                true->
                    analy({A1,B1,C1,D1},{A2,B2,C2,D2},4,[]);
                _->
                    analy({A1,B1,C1,D1},{A2,B2,C2,D2},3,[])
                end;    
            _->
                analy({A1,B1,C1,D1},{A2,B2,C2,D2},2,[])
            end;
        _->
            analy({A1,B1,C1,D1},{A2,B2,C2,D2},1,[])
        end;
    Other->Other
    end.
analy({A1,B1,C1,D1},{A2,B2,C2,D2},S,Result)->
    NewResult = Result ++[integer_to_list(A1)++"."++integer_to_list(B1)++"."++integer_to_list(C1)++"."++integer_to_list(D1)],
    case S of
    1->
        case A1 =:=A2 andalso B1 =:=B2 andalso C1 =:=C2 andalso D1 =:=D2  of
            true -> NewResult;
            _->
             {A,B,C,D} = get_a_IP({A1,B1,C1,D1}),
            analy({A,B,C,D},{A2,B2,C2,D2},S,NewResult)
        end;
    2->
        case B1 =:=B2 andalso C1 =:=C2 andalso D1 =:=D2 of
            true -> NewResult;
            _->
             {A,B,C,D} = get_a_IP({A1,B1,C1,D1}),
            analy({A,B,C,D},{A2,B2,C2,D2},S,NewResult)
        end;
    3->
        case C1 =:=C2 andalso D1 =:=D2 of
            true -> NewResult;
            _->
            {A,B,C,D} = get_a_IP({A1,B1,C1,D1}),
            analy({A,B,C,D},{A2,B2,C2,D2},S,NewResult)
        end;
    _->
        case D1 =:=D2 of
            true -> NewResult;
            _->
            {A,B,C,D} = get_a_IP({A1,B1,C1,D1}),
            analy({A,B,C,D},{A2,B2,C2,D2},S,NewResult)
        end
    end.
get_a_IP({A1,B1,C1,D1})->
    case D1+1 of
    256->
        case C1 +1 of
        256->
            case B1+1 of
            256->
                {A1+1,1,1,1};
            _-> 
                {A1,B1+1,1,1}
            end;    
        _->
        {A1,B1,C1+1,1}
        end;
    _->
        {A1,B1,C1,D1+1}
    end.    
        
    