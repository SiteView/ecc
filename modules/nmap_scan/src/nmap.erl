%% @doc interface of nmap
%% @version{1.0}
%% @copyright 2009 dragonflow.com
%% @author Shi xianfang <xianfang.shi@dragonflow.com>

-module(nmap).
-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-import(xmerl_xs, 
        [ xslapply/2, value_of/1, select/2, built_in_rules/2 ]).


-define(NMAP_CMD,get_nmap_cmdline()).

get_nmap_cmdline()->
	case os:type() of
		{win32,_}->
			"tools\\nmap-5.10BETA2\\nmap";
		_->
			"tools/nmap-5.10BETA2/nmap"
	end.
	
get_out_filename()->
	lists:flatten(io_lib:format("logs/scan_~s.xml",[string:join(string:tokens(pid_to_list(self()),"<>."),"_")])).
	
	
scan(Params)->
	CmdLine = parse_params(Params),
	File = get_out_filename(),
	os:cmd(?NMAP_CMD ++ " " ++ CmdLine ++ " -oX " ++ File),
    case filelib:is_file(File) of
    true->
        case xmerl_scan:file(File) of
            {error,_}->
                file:delete(File),
                {error,open_xml_file_error};
            {Ret,_}->
                L = parse_return(xmerl_xpath:string("//host[status[@state='up']]",Ret)),
                file:delete(File),
                L
        end;
    false->[]
    end.
	
parse_params([])->"";
parse_params([P|T])->
	P ++ " " ++ parse_params(T).

parse_return([])->[];
parse_return([E=#xmlElement{name=host}|T])->
	% [{E#xmlElement.name,parse_attributes(E#xmlElement.attributes) ++ parse_host_childs(E#xmlElement.content)}] ++
	template(E) ++ 
	parse_return(T);
parse_return([E=#xmlElement{}|T])->
	parse_return(E#xmlElement.content) ++
	parse_return(T);
parse_return([_|T])->
	parse_return(T).


parse_attributes([])->[];
parse_attributes([E=#xmlAttribute{}|T])->
	[{E#xmlAttribute.name,E#xmlAttribute.value}] ++ parse_attributes(T).
	

parse_host_childs([])->[];
% parse_host_childs([E=#xmlText{}|T])->
%	[E#xmlText.value] ++ parse_host_childs(T);
parse_host_childs([E=#xmlElement{}|T])->
	[{E#xmlElement.name,parse_attributes(E#xmlElement.attributes)}] ++ parse_host_childs(T);
parse_host_childs([_|T])->
	parse_host_childs(T).

template(E=#xmlElement{name=osclass})->
		{osclass,[
				{type,element(3,select("string(@type)",E))},
				{vendor,element(3,select("string(@vendor)",E))},
				{osfamily,element(3,select("string(@osfamily)",E))
						++case select("string(@osgen)",E) of
							{_,_,V}->
								" " ++ V;
							_->
								""
						end
				},
				{accuracy,element(3,select("string(@accuracy)",E))}
				]
		};
template(E=#xmlElement{name=port})->
		{port,[
				{portid,element(3,select("string(@portid)",E))},
				{protocol,element(3,select("string(@protocol)",E))},
				{service,element(3,select("string(service/@name)",E))},
				{product,element(3,select("string(service/@product)",E))},
				{version,element(3,select("string(service/@version)",E))}
				]
		};
template(E=#xmlElement{name=host})->
	[	[
			{ip,element(3,select("string(address[@addrtype='ipv4']/@addr)",E))},
			{ports,xslapply(fun template/1,select("ports/port[state/@state='open']",E))
			      ++ xslapply(fun template/1,select("ports/port[state/@state='open|filtered']",E))},
			{os,xslapply(fun template/1,select("os/osclass[1]",E))}
		]	
	];
template(E) -> built_in_rules(fun template/1, E).
