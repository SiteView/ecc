-module(proxy).


-include("config.hrl").

   
-compile(export_all).  

tablelist(ValueList) ->
     case hd(ValueList) of
	 HeaderList when HeaderList /= []->
	   case lists:nthtail(1,ValueList) of
		 Lists when length(Lists) > 0 ->			    
		    {ok,listmap(Lists,HeaderList,[])};
		 _ -> {error,"Error Empty!"}
	   end;
	 _ -> {error,"Error Format!"}
     end.
  
listmap([L|R],HeaderList,Acc) when is_tuple(HeaderList) andalso is_tuple(L) andalso tuple_size(HeaderList) =:= tuple_size(L) ->
    listmap(R,HeaderList,[elementmap(L,HeaderList,[],1)|Acc]);
      

listmap([_|R],HeaderList,Acc)  -> listmap(R,HeaderList,Acc);

listmap([],_,Acc)  -> lists:reverse(Acc).

elementmap(_,HeaderList,Acc,Num)  when tuple_size(HeaderList)+1 =:= Num -> lists:reverse(Acc);
elementmap(DataList,HeaderList,Acc,Num) ->
     NewAcc = {list_to_atom(element(Num,HeaderList)),element(Num,DataList)},
     elementmap(DataList,HeaderList,[NewAcc|Acc],Num+1).
     
 to_integer(String) ->
      case string:to_integer(String) of
            {Value,_} when is_integer(Value) -> Value;
	    _ -> 0
      end.  

sleep(Time)->
    receive 
	after Time ->   ok
    end.  
    
dotround(String,Div,Bit) when is_list(String) ->     
      case string:to_integer(String) of
            {Value,_} when is_integer(Value) -> dotround(Value,Div,Bit);
	    _ -> 0
      end;      
dotround(Value,Div,Bit) when is_integer(Value) orelse is_float(Value) -> 
     N = math:pow(10,Bit),
     round(Value*N/Div) / N;   
     
dotround(_,_,_)  -> 0.    

myround(String,Bit) when is_list(String) ->
      case string:to_float(String) of
            {Value,_} when is_float(Value) -> myround(Value,Bit);
	    _ -> 
	      case string:to_integer(String) of
		    {Value,_} when is_integer(Value) -> myround(Value,Bit);
		    _ -> 0
	      end
      end;
            
myround(Value,Bit) when is_integer(Value) orelse is_float(Value) -> 
     N = math:pow(10,Bit),
     round(Value*N) / N;   
     
myround(_,_)  -> 0. 

sumkey(Key,[L|R],Acc) ->
      case lists:keysearch(Key,1,L) of
         {value,{Key,Value}} when is_integer(Value) -> sumkey(Key,R,Value+Acc);
         {value,{Key,Value}} -> sumkey(Key,R,to_integer(Value)+Acc);
	 _ -> sumkey(Key,R,Acc)
      end; 	 
sumkey(_,[],Acc) -> Acc.
     
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     


cpu_count(Host) ->
   agent:getdatabykey(Host,'cpu_count').


os(Host) ->
  case agent:getdatabykey(Host,'os_ver') of  
	{ok,{simple_tuple,{OS,V1,V2}}} -> {ok,lists:flatten([OS,V2,".",V1])};
	ErrorReason -> ErrorReason
  end. 
  
datetime(Host) ->
  case agent:getdatabykey(Host,'datetime') of  
	{ok,{simple_tuple,{Date,Time}}} -> {ok,[{'date',Date},{'time',Time}]};
	ErrorReason -> ErrorReason
  end. 
  
uptime(Host) ->
  case agent:getdatabykey(Host,'uptime') of  
	{ok,{simple_tuple,{Day,Hour}}} -> {ok,lists:flatten([Day,"d,",Hour,"h"])};
	{ok,{simple_tuple,{Hour}}} -> {ok,lists:flatten(["0d,",Hour,"h"])};
	ErrorReason -> ErrorReason
  end.  
  
processes_count(Host) ->
  case agent:getdatabykey(Host,'processes_count') of  
	{ok,{simple_integer,Count}} -> {ok,Count};
	ErrorReason -> ErrorReason
  end.  
  
logins_count(Host) ->
  case agent:getdatabykey(Host,'logins_count') of  
	{ok,{simple_integer,Count}} -> {ok,Count};
	ErrorReason -> ErrorReason
  end. 

established(Host) ->
  case agent:getdatabykey(Host,'established') of  
	{ok,{simple_tuple,{Established,Time_Wait,Close_Wait}}} -> {ok,[{established,Established},{time_wait,Time_Wait},{close_wait,Close_Wait}]};
	ErrorReason -> ErrorReason
  end.
  
diskspace(Host) ->
  case agent:getdatabykey(Host,'diskspace') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	    case  tablelist(ValueList) of
	          {ok,DiskList}  ->	
                       %~ io:format("DiskList:~p~n",[DiskList]),		  
                      Fun = fun(X) ->		            
		            Filesystem =  case lists:keysearch('Filesystem',1,X) of
				                {value,{'Filesystem',FilesystemValue}} -> FilesystemValue;
						_ -> "N/A"
				          end,
			    Used =  case lists:keysearch('Used',1,X) of
				                {value,{'Used',UsedValue}} -> UsedValue;
						_ -> 
						  case lists:keysearch('used',1,X) of
							{value,{'used',UsedValue}} -> UsedValue;
							_ -> "0"
						  end
				          end,	
			    Capacity =  case lists:keysearch('Capacity',1,X) of
						{value,{'Capacity',CapacityValue}} -> CapacityValue;
						_ -> 
						  case lists:keysearch('capacity',1,X) of
							{value,{'capacity',CapacityValue}} -> CapacityValue;
							_ -> "0"
						  end
					  end,
			    Mounted_on =  case lists:keysearch('Mounted on',1,X) of
				                {value,{'Mounted on',Mounted_onValue}} -> Mounted_onValue;
						_ -> "N/A"
				          end,				
                            Space = case to_integer(Capacity) of
			         0 -> 0;
				 _ -> round(to_integer(Used) * 100 / to_integer(Capacity))
			    end,
			    
			    [{'Filesystem',Filesystem},
			     {'Mounted on',Mounted_on},
			     {'Used',to_integer(Used)},
			     {'Capacity',to_integer(Capacity)},
			     {'Space',Space}]					  
		            end,
	           {ok,lists:map(Fun,DiskList)};	
		 _ -> {error,"Error Format!"} 
	    end;	
	ErrorReason -> ErrorReason
  end. 
  
cpu(Host) -> 
  case agent:getdatabykey(Host,'cpu') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 	
             %~ io:format("ValueList:~p~n", [ValueList]), 	
	     case tablelist(ValueList) of
	         {ok,[[_,{'%USR',Usr},{'%SYS',Sys},{'%WIO',Wio},{'%IDLE',Idle}]]}  -> 		      
		      {ok,{{idle,to_integer(Idle)},{usr,to_integer(Usr)},{sys,to_integer(Sys)},{wio,to_integer(Wio)}}};
		 {ok,[[{usr,Usr},{sys,Sys},{wt,Wio},{idl,Idle}]]} ->
		      {ok,{{idle,to_integer(Idle)},{usr,to_integer(Usr)},{sys,to_integer(Sys)},{wio,to_integer(Wio)}}};	
		 {ok,[[{'%usr',Usr},{'%sys',Sys},{'%wio',Wio},{'%idle',Idle}]]} ->
		      {ok,{{idle,to_integer(Idle)},{usr,to_integer(Usr)},{sys,to_integer(Sys)},{wio,to_integer(Wio)}}};			      
		 ErrorReason -> {error,ErrorReason} 
	     end;
	{ok,{simple_tuple,{Usr,Sys,Idle,Wio}}}  -> 
	    {ok,{{idle,to_integer(Idle)},{usr,to_integer(Usr)},{sys,to_integer(Sys)},{wio,to_integer(Wio)}}};
	{ok,{simple_tuple,{Usr,Sys,Idle}}}  -> 
	    {ok,{{idle,to_integer(Idle)},{usr,to_integer(Usr)},{sys,to_integer(Sys)},{wio,0}}};    
	ErrorReason -> ErrorReason
  end.
  
 
memory(Host) ->
   os_memory(Host,agent:getdatabykey(Host,'OS')).
   
   
os_memory(Host,{ok,"AIX"}) ->
    case  agent:getdatabykey(Host,'swapspace') of
          {ok,{table,[_,{SwapSpace,SwapPerent}]}} ->
	     Swapspace = to_integer(SwapSpace),
	     Swapperent = to_integer(SwapPerent),
	     case agent:getdatabykey(Host,'lsattr') of
		  {ok,{simple_integer,Total_Memory}} ->
			  Total_memory	= round(Total_Memory / 1024),	  
			  case agent:getdatabykey(Host,'memory') of
				{ok,{table,ValueList}} when is_list(ValueList) ->	
					case tablelist(ValueList) of
					     {ok,[Memory]} ->
					         case lists:keysearch(fre,1,Memory) of
						       {value,{fre,Used_Physical_Memory}} ->
						            Used_physical_memory = to_integer(Used_Physical_Memory),
							    Used_physical_memory_mb = Total_memory - round(Used_physical_memory * 4 /1024),
							    Used_swap_space = Swapperent*Swapspace/100,
							    {ok,[{physical_memory,Total_memory},
							         {used_physical_memory,Used_physical_memory_mb},
								 {swap_space,Swapspace},
								 {used_swap_space,Used_swap_space},
								 {virtual_memory,Total_memory+Swapspace},
								 {used_virtual_memory,round(Used_physical_memory_mb+Used_swap_space)}]};
						       _ -> {error,"Error format used_physical_memory!"}
						 end;
					     _ -> {error,"Error read used_physical_memory!"}   					     
				        end;
					
				ErrorReason -> ErrorReason
			  end;
		  ErrorReason -> ErrorReason
	     end;	
	  ErrorReason -> ErrorReason
    end;
os_memory(Host,{ok,"Linux"}) ->
    case  agent:getdatabykey(Host,'swapspace') of         
	  {ok,{simple_tuple,{SwapSpace,Used_swap_space}}} ->
	      case agent:getdatabykey(Host,'memory') of
	           {ok,{simple_tuple,{Physical_memory,Used_physical_memory}}} ->		          
			   PhysicalMemory_ = dotround(Physical_memory,1024,2),
			   SwapSpace_ = dotround(SwapSpace,1024,2),
			   Used_physical_memory_ = dotround(Used_physical_memory,1024,2),
			   Used_swap_space_ = dotround(Used_swap_space,1024,2),
			  
			   {ok,[{physical_memory,PhysicalMemory_},
				 {used_physical_memory,Used_physical_memory_},
				 {swap_space,SwapSpace_},
				 {used_swap_space,Used_swap_space_},
				 {virtual_memory,dotround(PhysicalMemory_+SwapSpace_,1,2)},
				 {used_virtual_memory,dotround(Used_physical_memory_+Used_swap_space_,1,2)}]};
		   ErrorReason -> ErrorReason
	      end;
	  ErrorReason -> ErrorReason
    end;    
os_memory(Host,{ok,"SunOS"}) ->
    case  agent:getdatabykey(Host,'swapspace') of         
	  {ok,{simple_tuple,{Used_swap_space,Free_swap_space}}} ->	      
	      case agent:getdatabykey(Host,'lsattr') of
	           {ok,{simple_tuple,{Physical_memory}}} ->	
			    case  agent:getdatabykey(Host,'vmstat') of
                                 {ok,{table,VmstatList}} when is_list(VmstatList) -> 
				     case tablelist(VmstatList) of
					{ok,Vmstat} ->
					    Vmstat_tail = lists:nth(length(Vmstat),Vmstat),
					    Free =  case lists:keysearch('free',1,Vmstat_tail) of
				                       {value,{'free',PiValue}} -> PiValue;
							_ -> "0"
						  end,
					    %~ io:format("Redhat4:~p~n",[{Used_swap_space,Free_swap_space,Physical_memory,Free}]), 	  
					    Free_memory = dotround(to_integer(Free),1024,2),
					    PhysicalMemory_ = to_integer(Physical_memory),
					    SwapSpace_ = dotround(to_integer(Used_swap_space)+to_integer(Free_swap_space),1024,2),
					    Used_physical_memory_ = myround(PhysicalMemory_ - Free_memory,2),
					    Used_swap_space_ = dotround(Used_swap_space,1024,2),
					    {ok,[{physical_memory,PhysicalMemory_},
						 {used_physical_memory,Used_physical_memory_},
						 {swap_space,SwapSpace_},
						 {used_swap_space,Used_swap_space_},
						 {virtual_memory,dotround(PhysicalMemory_+SwapSpace_,1,2)},
						 {used_virtual_memory,dotround(Used_physical_memory_+Used_swap_space_,1,2)}]};
					 _ -> {error,"Error read vmstat!"}   					     
				     end;
 				
			         ErrorReason -> ErrorReason
			   end;  
		   ErrorReason -> ErrorReason
	      end;
	  ErrorReason -> ErrorReason
    end;   

os_memory(Host,{ok,"HP-UX"}) ->
    case  agent:getdatabykey(Host,'swapspace') of         
	  {ok,{simple_tuple,{Swap_space,Used_swap_space,_Free_swap_space}}} ->
              %~ io:format("HP-UX:~p~n",[{Swap_space,Used_swap_space,Free_swap_space}]), 	  
	      case agent:getdatabykey(Host,'lsattr') of
	           {ok,{simple_integer,Physical_memory}} ->	
		            %~ io:format("HP-UX:~p~n",[Physical_memory]),
			    case  agent:getdatabykey(Host,'memory') of
                                 {ok,{table,VmstatList}} when is_list(VmstatList) -> 
				   %~ io:format("HP-UX:~p~n",[VmstatList]),
				     case tablelist(VmstatList) of
					{ok,Vmstat} ->
					    Vmstat_tail = lists:nth(length(Vmstat),Vmstat),
					    Free =  case lists:keysearch('free',1,Vmstat_tail) of
				                       {value,{'free',PiValue}} -> PiValue;
							_ -> "0"
						  end,
					   
					    Free_memory = dotround(to_integer(Free),1024,2),
					    PhysicalMemory_ = dotround(Physical_memory,1024,2),
					    SwapSpace_ = dotround(Swap_space,1024,2),
					    Used_physical_memory_ = myround(PhysicalMemory_ - Free_memory,2),
					    Used_swap_space_ = dotround(Used_swap_space,1024,2),
					    {ok,[{physical_memory,PhysicalMemory_},
						 {used_physical_memory,Used_physical_memory_},
						 {swap_space,SwapSpace_},
						 {used_swap_space,Used_swap_space_},
						 {virtual_memory,dotround(PhysicalMemory_+SwapSpace_,1,2)},
						 {used_virtual_memory,dotround(Used_physical_memory_+Used_swap_space_,1,2)}]};
					 _ -> {error,"Error read vmstat!"}   					     
				     end;
 				
			         ErrorReason -> ErrorReason
			   end;  
		   ErrorReason -> ErrorReason
	      end;
	  ErrorReason -> ErrorReason
    end;   
os_memory(_,_) -> {error,"Memory OS Error"}.

swap(Host) ->
   os_swap(Host,agent:getdatabykey(Host,'OS')).
   
   
os_swap(Host,{ok,"AIX"}) ->
    case  agent:getdatabykey(Host,'vmstat') of
         {ok,{table,SwapList}} when is_list(SwapList) ->
	    case tablelist(SwapList) of
		     {ok,[Swap]} ->		         
			 Pi =  case lists:keysearch('pi',1,Swap) of
				                {value,{'pi',PiValue}} -> PiValue;
						_ -> "0"
				          end,
			 Po =  case lists:keysearch('po',1,Swap) of
				                {value,{'po',PoValue}} -> PoValue;
						_ -> "0"
				          end,
			 Fi =  case lists:keysearch('fi',1,Swap) of
				                {value,{'fi',FiValue}} -> FiValue;
						_ -> "0"
				          end,
			 Fo =  case lists:keysearch('fo',1,Swap) of
				                {value,{'fo',FoValue}} -> FoValue;
						_ -> "0"
				          end,
			  {ok,[{'page in',to_integer(Pi)},{'page out',to_integer(Po)},{'swap in',to_integer(Fi)},{'swap out',to_integer(Fo)}]};
		     _ -> {error,"Error read Swap!"}   					     
	    end;
	  ErrorReason -> ErrorReason
    end;  
    
os_swap(Host,{ok,"Linux"}) ->
  case agent:getdatabykey(Host,'vmstat') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	     case tablelist(ValueList) of
	        {ok,VmstatList}  ->
		    Vmstat = lists:nth(length(VmstatList),VmstatList),
		    %~ io:format("IoList:~p~n",[{Vmstat,VmstatList}]),
		    Si =  case lists:keysearch('si',1,Vmstat) of
				                {value,{'si',PiValue}} -> PiValue;
						_ -> "0"
				          end,
		    So =  case lists:keysearch('so',1,Vmstat) of
					{value,{'so',PoValue}} -> PoValue;
					_ -> "0"
				  end, 
		    case	 page(Host) of
		        {ok,Pagedata} ->   {ok,Pagedata ++ [{'swap in',to_integer(Si)},{'swap out',to_integer(So)}]};			   
			ErrorReason -> ErrorReason
		    end;		   
		 _ -> {error,"Error Format!"} 
	     
	     end;	     
	ErrorReason -> ErrorReason
  end;      
os_swap(_,_) -> {error,"OS Error"}.

page(Host) ->  
   case agent:getdatabykey(Host,'page') of  
	{ok,{simple_tuple,{Pagein,Pageout}}} ->  
	      sleep(1000),
	      case agent:getdatabykey(Host,'page') of  
		{ok,{simple_tuple,{Pagein1,Pageout1}}} -> 	
		
		  {ok,[{'page in', to_integer(Pagein1) - to_integer(Pagein)},		      
		      {'page out', to_integer(Pageout1) - to_integer(Pageout)}]
		    };
		  ErrorReason -> ErrorReason
	      end;
	  ErrorReason -> ErrorReason
    end.

iostat(Host) ->
  os_iostat(Host,agent:getdatabykey(Host,'OS')).
  
os_iostat(Host,{ok,"AIX"}) ->
  %~ case agent:getdatabykey(Host,'iostat') of  
	%~ {ok,{table,ValueList}} when is_list(ValueList) -> 
	     %~ case tablelist(ValueList) of
	        %~ {ok,IoList}  ->
                   %~ Rps = iostat_sumkey('RPS',IoList,0),
		   %~ Wps = iostat_sumkey('WPS',IoList,0),
		   %~ {ok,[{'RPS',Rps},{'WPS',Wps}]};		   
		 %~ _ -> {error,"Error Format!"} 
	     
	     %~ end;	     
	%~ ErrorReason -> ErrorReason
  %~ end;  
   case agent:getdatabykey(Host,'iostat') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	     case tablelist(ValueList) of
	        {ok,IoList}  ->
		   %~ io:format("IoList:~p~n",[IoList]),
		   NewIoList = iostat(IoList,[]),                  
		   R_s = iostat_sumkey('RPS',NewIoList,0),
		   W_s = iostat_sumkey('WPS',NewIoList,0),		  
		   {ok,[{'RPS',R_s},{'WPS',W_s},iostat_util('DISK','ACT',NewIoList,[])]} ;
		 _ -> {error,"Error Format!"} 
	     
	     end;	     
	ErrorReason -> ErrorReason
   end;    
os_iostat(Host,{ok,"Linux"}) ->
  case agent:getdatabykey(Host,'iostat') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	     case tablelist(ValueList) of
	        {ok,IoList}  ->
		   %~ io:format("IoList:~p~n",[IoList]),
		   NewIoList = iostat(IoList,[]),                  
		   R_s = iostat_sumkey('rkB/s',NewIoList,0),
		   W_s = iostat_sumkey('wkB/s',NewIoList,0),		  
		   {ok,[{'RPS',R_s},{'WPS',W_s},iostat_util('Device:','%util',NewIoList,[])]} ;
		 _ -> {error,"Error Format!"} 
	     
	     end;	     
	ErrorReason -> ErrorReason
  end;    
os_iostat(Host,{ok,"SunOS"}) ->
  case agent:getdatabykey(Host,'iostat') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	     case tablelist(ValueList) of
	        {ok,IoList}  ->
		    %~ io:format("IoList:~p~n",[IoList]),
		   NewIoList = iostat(IoList,[]),                  
		   R_s = iostat_sumkey('kr/s',NewIoList,0),
		   W_s = iostat_sumkey('kw/s',NewIoList,0),		  
		   {ok,[{'RPS',R_s},{'WPS',W_s},iostat_util('device','actv',NewIoList,[])]} ;
		 _ -> {error,"Error Format!"} 
	     
	     end;	     
	ErrorReason -> ErrorReason
  end;   
os_iostat(_,_) -> {error,"OS Error"}.

iostat([L|R],Acc) ->
        case lists:keysearch('Device:',1,L) of
		{value,{'Device:',"Device:"}} -> iostat(R,[]);
		_ -> iostat(R,[L|Acc])
	end;
iostat([],Acc) -> lists:reverse(Acc).

iostat_sumkey(Key,[L|R],Acc) ->
      case lists:keysearch(Key,1,L) of
         {value,{Key,Value}} -> iostat_sumkey(Key,R,to_integer(Value)+Acc);
	 _ -> iostat_sumkey(Key,R,Acc)
      end; 	 
iostat_sumkey(_,[],Acc) -> Acc.

iostat_util(Name,Key,[L|R],Acc) ->
      case {lists:keysearch(Name,1,L),lists:keysearch(Key,1,L)} of
         {{value,{Name,NameValue}},{value,{Key,Value}}} -> iostat_util(Name,Key,R,[{NameValue,myround(Value,2)}|Acc]);
	 _ -> iostat_util(Name,Key,R,Acc)
      end; 	 
iostat_util(_,_,[],Acc) -> lists:reverse(Acc).


network(Host) ->
   os_network(Host,agent:getdatabykey(Host,'OS')).
   
os_network(Host,{ok,"AIX"}) ->  
    case get_network(Host) of
          {ok,Networklist} ->
	      sleep(1000),
	      case get_network(Host) of
		  {ok,Networklist1} ->	
		  %~ io:format("os:~p~n",[calculatenetwork(Networklist,Networklist1,[])]),
		  {ok,[{'Ipkts/s', sumkey('Ipkts',Networklist1,0) - sumkey('Ipkts',Networklist,0)},		      
		      {'Opkts/s', sumkey('Opkts',Networklist1,0) - sumkey('Opkts',Networklist,0)},
		      {'Ierrs/s', sumkey('Ierrs',Networklist1,0)  - sumkey('Ierrs',Networklist,0)},
		      {'Oerrs/s', sumkey('Oerrs',Networklist1,0)  - sumkey('Oerrs',Networklist,0)},
		      calculatenetwork(Networklist,Networklist1,[])
		      ]
		    };
		  ErrorReason -> ErrorReason
	      end;
	  ErrorReason -> ErrorReason
    end;
    
os_network(Host,{ok,"Linux"}) ->  
     case agent:getdatabykey(Host,'network') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	      %~ io:format("os:~p~n",[ValueList]),
	     case tablelist(ValueList) of	            
	           {ok,Networklist} -> 	
                         %~ io:format("os:~p~n",[Networklist]), 
			sleep(1000),
			case agent:getdatabykey(Host,'network') of  
				{ok,{table,ValueList1}} when is_list(ValueList1) -> 
				      %~ io:format("os:~p~n",[ValueList1]),
				     case tablelist(ValueList1) of	            
					   {ok,Networklist1} -> 	
						 %~ io:format("os:~p~n",[Networklist1]),		   
						{ok,[{'Ipkts/s', sumkey('Ipkts',Networklist1,0) - sumkey('Ipkts',Networklist,0)},		      
						      {'Opkts/s', sumkey('Opkts',Networklist1,0) - sumkey('Opkts',Networklist,0)},
						      {'Ierrs/s', sumkey('Ierrs',Networklist1,0)  - sumkey('Ierrs',Networklist,0)},
						      {'Oerrs/s', sumkey('Oerrs',Networklist1,0)  - sumkey('Oerrs',Networklist,0)},
						      calculatenetwork(Networklist,Networklist1,[])
						      ]
						    }; 
					   _ -> {error,"Error read network"}   	
				     end;
				ErrorReason -> ErrorReason
			     end; 
	           _ -> {error,"Error read network"}   	
	     end;
	ErrorReason -> ErrorReason
     end;
os_network(Host,{ok,"SunOS"}) ->  
     case agent:getdatabykey(Host,'network') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	       %~ io:format("os:~p~n",[ValueList]),
	     case tablelist(ValueList) of	            
	           {ok,Networklist} -> 	
                          %~ io:format("os:~p~n",[Networklist]), 
			sleep(1000),
			case agent:getdatabykey(Host,'network') of  
				{ok,{table,ValueList1}} when is_list(ValueList1) -> 
				       %~ io:format("os:~p~n",[ValueList1]),
				     case tablelist(ValueList1) of	            
					   {ok,Networklist1} -> 	
						  %~ io:format("os:~p~n",[Networklist1]),		   
						{ok,[{'Ipkts/s', sumkey('Ipkts',Networklist1,0) - sumkey('Ipkts',Networklist,0)},		      
						      {'Opkts/s', sumkey('Opkts',Networklist1,0) - sumkey('Opkts',Networklist,0)},
						      {'Ierrs/s', sumkey('Ierrs',Networklist1,0)  - sumkey('Ierrs',Networklist,0)},
						      {'Oerrs/s', sumkey('Oerrs',Networklist1,0)  - sumkey('Oerrs',Networklist,0)},
						      calculatenetwork(Networklist,Networklist1,[])
						      ]
						    }; 
					   _ -> {error,"Error read network"}   	
				     end;
				ErrorReason -> ErrorReason
			     end; 
	           _ -> {error,"Error read network"}   	
	     end;
	ErrorReason -> ErrorReason
     end;
os_network(Host,{ok,"HP-UX"}) ->  
  case agent:getdatabykey(Host,'network') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	        %~ io:format("os:~p~n",[ValueList]),
	     case tablelist(ValueList) of	            
	           {ok,Networklist} -> 	
                          %~ io:format("os:~p~n",[Networklist]), 
			sleep(1000),
			case agent:getdatabykey(Host,'network') of  
				{ok,{table,ValueList1}} when is_list(ValueList1) -> 
				        %~ io:format("os:~p~n",[ValueList1]),
				     case tablelist(ValueList1) of	            
					   {ok,Networklist1} -> 	
						  %~ io:format("os:~p~n",[Networklist1]),		   
						{ok,[{'Ipkts/s', sumkey('Ipkts',Networklist1,0) - sumkey('Ipkts',Networklist,0)},		      
						      {'Opkts/s', sumkey('Opkts',Networklist1,0) - sumkey('Opkts',Networklist,0)},
						      {'Ierrs/s', sumkey('Ierrs',Networklist1,0)  - sumkey('Ierrs',Networklist,0)},
						      {'Oerrs/s', sumkey('Oerrs',Networklist1,0)  - sumkey('Oerrs',Networklist,0)},
						      calculatenetwork(Networklist,Networklist1,[])
						      ]
						    }; 
					   _ -> {error,"Error read network"}   	
				     end;
				ErrorReason -> ErrorReason
			     end; 
	           _ -> {error,"Error read network"}   	
	     end;
	ErrorReason -> ErrorReason
     end;  
os_network(_,_) -> {error,"OS Error"}.

%~ linux_network([L|R]) ->
   %~ HeadList = tuple_to_list(L).
   
%~ linux_network_head(["face"|R],Acc) -> linux_network_head(R,[RAcc])


%~ sumkey(Key,[L|R],Acc) ->
      %~ case lists:keysearch(Key,1,L) of
         %~ {value,{Key,Value}} when is_integer(Value) -> sumkey(Key,R,Value+Acc);
         %~ {value,{Key,Value}} -> sumkey(Key,R,to_integer(Value)+Acc);
	 %~ _ -> sumkey(Key,R,Acc)
      %~ end; 	 
%~ sumkey(_,[],Acc) -> Acc.   
   
calculatenetwork([L|R],Networklist,Acc) ->
      case lists:keysearch('Name',1,L) of
	{value,{'Name',NameValue}} -> 
	    NewAcc = {NameValue,[{'pkts/s',calculatenetworkkey('Ipkts',L,NameValue,Networklist)+calculatenetworkkey('Opkts',L,NameValue,Networklist)},
				 {'errs/s',calculatenetworkkey('Ierrs',L,NameValue,Networklist)+calculatenetworkkey('Oerrs',L,NameValue,Networklist)}]},
	    calculatenetwork(R,Networklist,[NewAcc|Acc]);
	_ -> calculatenetwork(R,Networklist,Acc)
      end;
calculatenetwork([],_,Acc) -> lists:reverse(Acc).




calculatenetworkkey(Key,Network,Name,Networklist) ->
	case lists:keysearch(Key,1,Network) of
		{value,{Key,Value}}  when is_integer(Value) -> calculatenetworkkey(Key,Name,Networklist) - Value;
		{value,{Key,Value}} -> calculatenetworkkey(Key,Name,Networklist) - to_integer(Value);
		_ -> 0
	end.
	
calculatenetworkkey(Key,Name,[L|R]) ->   
     case {lists:keysearch(Key,1,L),lists:keysearch('Name',1,L)} of
             {{value,{Key,Value}},{value,{'Name',Name}}} when is_integer(Value) -> Value;
	     {{value,{Key,Value}},{value,{'Name',Name}}} -> to_integer(Value);
	   _ ->  calculatenetworkkey(Key,Name,R)    
     end; 
calculatenetworkkey(_,_,[]) -> 0.


 
get_network(Host) ->
  case agent:getdatabykey(Host,'network') of  
	{ok,{table,ValueList}} when is_list(ValueList) -> 
	     %~ io:format("os:~p~n",[tablelist(ValueList)]),
	     case tablelist(ValueList) of	            
	           {ok,Networklist} -> 		       
			{ok,network_link(Networklist,[])};
	           _ -> {error,"Error read network"}   	
	     end;
	ErrorReason -> ErrorReason
  end.  
  
network_link([L|R],Acc) ->           
    case lists:keysearch('Network',1,L) of
         {value,{'Network',"link#"++_}} ->
				Name =  case lists:keysearch('Name',1,L) of
				                {value,{'Name',NameValue}} -> NameValue;
						_ -> "N/A"
				          end,
		                Ipkts =  case lists:keysearch('Ipkts',1,L) of
				                {value,{'Ipkts',IpktsValue}} -> IpktsValue;
						_ -> 0
				          end,
				Ierrs =  case lists:keysearch('Ierrs',1,L) of
				                {value,{'Ierrs',IerrsValue}} -> IerrsValue;
						_ -> 0
				          end,
				Opkts =  case lists:keysearch('Opkts',1,L) of
				                {value,{'Opkts',OpktsValue}} -> OpktsValue;
						_ -> 0
				          end,
				Oerrs =  case lists:keysearch('Oerrs',1,L) of
				                {value,{'Oerrs',OerrsValue}} -> OerrsValue;
						_ -> 0
				          end,
				 NetWork = [{'Name',Name},
				      {'Ipkts',to_integer(Ipkts)},
				      {'Ierrs',to_integer(Ierrs)},
				      {'Opkts',to_integer(Opkts)},
				      {'Oerrs',to_integer(Oerrs)}],
				 network_link(R,[NetWork|Acc]);
          _ -> network_link(R,Acc)
    end;
network_link([],Acc) -> lists:reverse(Acc).
















    
    
    
    
    
    
    
    

  %~ (agent@alienware)1> diskspace:{ok,{table,[{"Filesystem","1024-blocks","Used","Available",
                       %~ "Capacity","Mounted on"},
                      %~ {"/dev/hd4","65536","24164","41372","37%","/"},
                      %~ {"/dev/hd2","1376256","1038952","337304","76%","/usr"},
                      %~ {"/dev/hd9var","65536","47952","17584","74%","/var"},
                      %~ {"/dev/hd3","65536","57780","7756","89%","/tmp"},
                      %~ {"/dev/hd1","65536","392","65144","1%","/home"},
                      %~ {"/dev/hd10opt","65536","50972","14564","78%","/opt"},
                      %~ {"/dev/testlv","1048576","520","1048056","1%",
                       %~ "/test"}]}}
%~ (agent@alienware)1> cpu:{ok,{table,[{"CPU","%USR","%SYS","%WIO","%IDLE"},
                %~ {"0","6","10","0","84"}]}}
%~ (agent@alienware)1> os_ver:{ok,{simple_tuple,{"AIX","3","5"}}}
%~ (agent@alienware)1> datetime:{ok,{simple_tuple,{"2010-12-23","19:17:46"}}}
%~ (agent@alienware)1> Processes_count:{ok,{simple_integer,78}}
%~ (agent@alienware)1> Logins_count:{ok,{simple_integer,11}}
%~ (agent@alienware)1> Uptime:{ok,{simple_tuple,{"8","16"}}}
%~ (agent@alienware)1> Lsattr:{ok,{simple_integer,1048576}}
%~ (agent@alienware)1> Acks:{ok,{simple_tuple,{"84310290","1"}}}
%~ (agent@alienware)1> Network:{ok,{table,[{"Name","Mtu","Network","Address","Ipkts","Ierrs","Opkts",
                     %~ "Oerrs","Coll"},
                    %~ {"en0","1500","link#2","0.9.6b.de.1e.48","7644156","0",
                     %~ "805582","0","0"},
                    %~ {"en0","1500","192.168.0","192.168.0.68","7644156","0",
                     %~ "805582","0","0"},
                    %~ {"lo0","16896","127","127.0.0.1","437","0","440","0",
                     %~ "0"}]}}
%~ (agent@alienware)1> Packets:{ok,{simple_tuple,{"84320646","1"}}}
%~ (agent@alienware)1> Established:{ok,{simple_tuple,{"6","0","0"}}}
%~ (agent@alienware)1> Iostat:{ok,{table,[{"DISK","BREAD","BWRTN","RPS","WPS"},
                   %~ {"hdisk0","155.9","1.3K","0.0","0.3"},
                   %~ {"hdisk1","0.0","129.3","0.0","0.0"},
                   %~ {"cd0","0.0","0.0","0.0","0.0"}]}}
%~ (agent@alienware)1> Swapspace:{ok,{table,[{"Total","Used"},{"512MB","1%"}]}}
%~ (agent@alienware)1> Memory:{ok,{table,[{"r","b","avm","fre","re","pi","po","fr","sr","cy","in",
                    %~ "sy","cs","us","sy","id","wa"},
                   %~ {"2","0","87831","156113","0","0","0","0","0","0","8",
                    %~ "18338","248","19","15","66","0"}]}}