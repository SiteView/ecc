-module(process_xml).
-compile(export_all).

process_xml(XmlString) ->
    case erlsom:simple_form(XmlString) of
    %case XmlString of
    {_,Xml,_} ->
        case Xml of 
        {"PerformanceMonitor",_,Xml1} ->      
            if length(Xml1) > 0 ->
                process_xml_u(Xml1);
            true ->
                []
            end;
        _ ->
            []
        end;
    _ ->
        []
    end.
    
    
process_xml_u(XmlList) ->
    process_xml_t(XmlList,length(XmlList),[]).
process_xml_t(_Xl,0,E) -> E;
process_xml_t(XmlL,Num,En) ->
    [A|B] = XmlL,
    case A of
    {"Node",[{"name",NodeName}],List} ->
        TuList = process_xml_util_1(List,NodeName),    
        process_xml_t(B,Num-1,lists:append(En,TuList)); 
    _ ->
        process_xml_t(B,Num-1,En)        
    end.


process_xml_util_1(List,NodeName) ->
    process_xml_util_1_t(List,length(List),NodeName,[]).
process_xml_util_1_t(_L,0,_Node,E) -> E;
process_xml_util_1_t(Li,Num,Node,En) ->
    [A|B] = Li,
    case A of
    {"Server",[{"name",ServerName}],List} ->
        TuList = process_xml_util_2(List,Node,ServerName),
        process_xml_util_1_t(B,Num-1,Node,lists:append(En,TuList));
    _ ->
        process_xml_util_1_t(B,Num-1,Node,En)    
    end.
    
    
process_xml_util_2(List,NodeName,ServerName) ->
    process_xml_util_2_t(List,length(List),NodeName,ServerName,[]).
process_xml_util_2_t(_L,0,_N,_S,E) -> E;
process_xml_util_2_t(Li,Num,Node,Server,En) ->
    [A|B] = Li,
    case A of
    {Module,ModuleName,List} ->       
        Bool = estimate_end(List),
        if Bool ->
           if ModuleName == [] ->
                TuList = process_xml_util_util(List,Node++"/"++Server++"/"++Module),       
                process_xml_util_2_t(B,Num-1,Node,Server,lists:append(En,TuList));
            true ->
                [{_,MN}] = ModuleName,
                TuList = process_xml_util_util(List,Node++"/"++Server++"/"++MN),
                process_xml_util_2_t(B,Num-1,Node,Server,lists:append(En,TuList))             
            end;            
        true ->    
            if ModuleName == [] ->
                TuList = process_xml_util_3(List,Node,Server,Module),       
                process_xml_util_2_t(B,Num-1,Node,Server,lists:append(En,TuList));
            true ->
                [{_,MN}] = ModuleName,
                TuList = process_xml_util_3(List,Node,Server,MN),
                process_xml_util_2_t(B,Num-1,Node,Server,lists:append(En,TuList))             
            end
        end;        
    _->   
        process_xml_util_2_t(B,Num-1,Node,Server,En)
    end.

process_xml_util_3(List,NodeName,ServerName,ModuleName) ->
    process_xml_util_3_t(List,length(List),NodeName,ServerName,ModuleName,[]). 
process_xml_util_3_t(_L,0,_N,_S,_M,E) -> E;    
process_xml_util_3_t(Li,Num,Node,Server,Module,En) ->    
    [A|B] = Li,
    case A of    
    {SubModule,SubModuleName,List} ->
        Bool = estimate_end(List),
        if Bool ->
            if SubModuleName == [] ->
                TuList = process_xml_util_util(List,Node++"/"++Server++"/"++Module++"/"++SubModule),      
                process_xml_util_3_t(B,Num-1,Node,Server,Module,lists:append(En,TuList));
            true ->
                [{_,SubN}] = SubModuleName,
                TuList = process_xml_util_util(List,Node++"/"++Server++"/"++Module++"/"++SubN),   
                process_xml_util_3_t(B,Num-1,Node,Server,Module,lists:append(En,TuList))             
            end;
        true ->            
            if SubModuleName == [] ->
                TuList = process_xml_util_4(List,Node,Server,Module,SubModule,""),       
                process_xml_util_3_t(B,Num-1,Node,Server,Module,lists:append(En,TuList));
            true ->
                [{_,SubN}] = SubModuleName,
                TuList = process_xml_util_4(List,Node,Server,Module,SubModule,SubN),
                process_xml_util_3_t(B,Num-1,Node,Server,Module,lists:append(En,TuList))             
            end
        end;    
    _ ->  
        process_xml_util_3_t(B,Num-1,Node,Server,Module,En)
    end.
    
    
process_xml_util_4(List,NodeName,ServerName,ModuleName,SubModule,SubModuleName) ->
    process_xml_util_4_t(List,length(List),NodeName,ServerName,ModuleName,SubModule,SubModuleName,[]).
process_xml_util_4_t(_Li,0,_Node,_Server,_Module,_SubM,_SubMN,E) ->E;
process_xml_util_4_t(Li,Num,Node,Server,Module,SubM,SubMN,En) ->
    [A|B] = Li,
    case A of    
    {N,N1,List} ->
        Bool = estimate_end(List),        
        if Bool ->
            if N1 == [] ->
                TuList = process_xml_util_util(List,Node++"/"++Server++"/"++Module++"/"++SubM++"/"++SubMN++"/"++N),
                process_xml_util_4_t(B,Num-1,Node,Server,Module,SubM,SubMN,lists:append(En,TuList));                  
            true ->
                [{_,N3}] = N1, 
                TuList = process_xml_util_util(List,Node++"/"++Server++"/"++Module++"/"++SubM++"/"++SubMN++"/"++N3),
                process_xml_util_4_t(B,Num-1,Node,Server,Module,SubM,SubMN,lists:append(En,TuList))
            end;
        true ->
            if N1 == [] ->
                TuList = process_xml_util_5(List,Node++"/"++Server++"/"++Module++"/"++SubM++"/"++SubMN++"/"++N), 
                process_xml_util_4_t(B,Num-1,Node,Server,Module,SubM,SubMN,lists:append(En,TuList));
            true ->
                [{_,N3}] = N1, 
                TuList = process_xml_util_5(List,Node++"/"++Server++"/"++Module++"/"++SubM++"/"++SubMN++"/"++N3),
                process_xml_util_4_t(B,Num-1,Node,Server,Module,SubM,SubMN,lists:append(En,TuList))
            end                
        end;
    _->
        process_xml_util_4_t(B,Num-1,Node,Server,Module,SubM,SubMN,En)
    end.        
      

process_xml_util_util(List,Name) ->
    process_xml_util_util_t(List,length(List),Name,[]).
process_xml_util_util_t(_L,0,_N,E) -> E;
process_xml_util_util_t(Li,Num,Na,En) ->
    [A|B] = Li,
    {Flag,List,_} = A,
    case Flag of
    "PerfStatInfo" ->

        case lists:keysearch("mean",1,List) of
        {value,{"mean",Value1}} ->
            L1 = [{Na ++ "(Statistical)" ++ "/Mean",Value1}];
        _ ->
            L1 = []
        end,
        case lists:keysearch("num",1,List) of
        {value,{"num",Value2}} ->
            L2 = [{Na ++ "(Statistical)" ++ "/Num",Value2}];
        _ ->
            L2 = []
        end,
        case lists:keysearch("sum_of_squares",1,List) of
        {value,{"sum_of_squares",Value3}} ->
            L3 = [{Na ++ "(Statistical)" ++ "/Sum of Squares",Value3}];
        _ ->
            L3 = []
        end,
        case lists:keysearch("total",1,List) of
        {value,{"total",Value4}} ->
            L4 = [{Na ++ "(Statistical)" ++ "/Total",Value4}];
        _ ->
            L4 = []
        end,
        L = lists:append(En,lists:append(L1,lists:append(L2,lists:append(L3,L4)))),        
        process_xml_util_util_t(B,Num-1,Na,lists:append(En,L));
    "PerfLoadInfo" -> 

        case lists:keysearch("mean",1,List) of
        {value,{"mean",Value1}} ->
            L1 = [{Na ++ "(Load)" ++ "/Mean",Value1}];
        _ ->
            L1 = []
        end,
        case lists:keysearch("timeSinceCreate",1,List) of
        {value,{"timeSinceCreate",Value2}} ->
            L2 = [{Na ++ "(Load)" ++ "/Time Since Create",Value2}];
        _ ->
            L2 = []
        end,
        case lists:keysearch("integral",1,List) of
        {value,{"integral",Value3}} ->
            L3 = [{Na ++ "(Load)" ++ "/Integral",Value3}];
        _ ->
            L3 = []
        end,
        case lists:keysearch("currentValue",1,List) of
        {value,{"currentValue",Value4}} ->
            L4 = [{Na ++ "(Load)" ++ "/CurrentValue",Value4}];
        _ ->
            L4 = []
        end,
        L = lists:append(En,lists:append(L1,lists:append(L2,lists:append(L3,L4)))),        
        process_xml_util_util_t(B,Num-1,Na,lists:append(En,L));
    "PerfNumericInfo" ->

        case lists:keysearch("val",1,List) of
        {value,{"val",Value1}} ->
            L = [{Na,Value1}];
        _ ->
            L = []
        end,
        process_xml_util_util_t(B,Num-1,Na,lists:append(En,L))
    end.    
        
        


process_xml_util_5(List,Name) ->
    process_xml_util_5_t(List,length(List),Name,[]).
process_xml_util_5_t(_L,0,_Name,E) -> E;
process_xml_util_5_t(Li,Num,Na,En) ->
    [A|B] = Li,
    {N,N1,List} = A,
    Bool = estimate_end(List),
    if Bool ->
        TuList = process_xml_util_util(List,Na++"/"++N);
    true ->    
        TuList = process_xml_util_6(List,Na++"/"++N)  
    end,
    process_xml_util_5_t(B,Num-1,Na,lists:append(En,TuList)).
    

process_xml_util_6(List,Name) ->
    process_xml_util_6_t(List,length(List),Name,[]).
process_xml_util_6_t(_L,0,_Name,E) -> E;
process_xml_util_6_t(Li,Num,Na,En) ->
    [A|B] = Li,
    {N,N1,List} = A,
    Bool = estimate_end(List),
    if Bool ->
        TuList = process_xml_util_util(List,Na++"/"++N);
    true ->    
        TuList = process_xml_util_7(List,Na++"/"++N)  
    end,
    process_xml_util_6_t(B,Num-1,Na,lists:append(En,TuList)).


process_xml_util_7(List,Name) ->
    process_xml_util_7_t(List,length(List),Name,[]).
process_xml_util_7_t(_L,0,_Name,E) -> E;
process_xml_util_7_t(Li,Num,Na,En) ->
    [A|B] = Li,
    {N,N1,List} = A,
    Bool = estimate_end(List),
    if Bool ->
        TuList = process_xml_util_util(List,Na++"/"++N);
    true ->    
        TuList = process_xml_util_8(List,Na++"/"++N)  
    end,
    process_xml_util_7_t(B,Num-1,Na,lists:append(En,TuList)).

process_xml_util_8(List,Name) ->
    process_xml_util_8_t(List,length(List),Name,[]).
process_xml_util_8_t(_L,0,_Name,E) -> E;
process_xml_util_8_t(Li,Num,Na,En) ->
    [A|B] = Li,
    {N,N1,List} = A,
    Bool = estimate_end(List),
    if Bool ->
        TuList = process_xml_util_util(List,Na++"/"++N);
    true ->    
        TuList = []  
    end,
    process_xml_util_8_t(B,Num-1,Na,lists:append(En,TuList)).

estimate_end(List) ->
    case lists:keysearch("PerfStatInfo",1,List) of
    {value,_V1} ->
        true;
    _ ->
        case lists:keysearch("PerfNumericInfo",1,List) of
        {value,_V2} ->
            true;
        _ ->
            case lists:keysearch("PerfLoadInfo",1,List) of  
            {value,_V3} ->
                true;
            _ ->
                false
            end
        end      
    end.    
    
    
