%% ---
%% xml2list
%%
%%---
-module(xml2list).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").

to_objlist(Xml)->
	{XMLContent,_} = xmerl_scan:string(xmerl_ucs:to_utf8(Xml)),
	process_obj(XMLContent).

to_params(Xml)->
	{XMLContent,_} = xmerl_scan:string(xmerl_ucs:to_utf8(Xml)),
	process_element(XMLContent).

process_obj(E=#xmlElement{name='obj'})->
	process_type(E#xmlElement.attributes) ++ process_property(E#xmlElement.content,false);
process_obj(_)->
	{error,xml_ecc_format}.


process_type([])->[];
process_type([A=#xmlAttribute{name='type'}|_])->
		[{class,list_to_atom(A#xmlAttribute.value)}];
process_type([_|T])->
		process_type(T).

process_element(E=#xmlElement{})->
	process_property(E#xmlElement.content,false);
process_element(_)->
	{error,xml_format_error}.

process_property([],_)->[];
process_property([P=#xmlElement{}|T],_)->
	[{P#xmlElement.name,process_sub_property(P#xmlElement.content,true)}]++process_property(T,false);
process_property([P=#xmlText{}|T],Flag)->
	case Flag of
		true->
			P#xmlText.value ++ process_property(T,false);
		false->
			process_property(T,false)
	end;
process_property([_|T],_)->process_property(T,false);
process_property(_,_)->[].


process_sub_property([],_)->[];
process_sub_property([P=#xmlText{}],_)->
	case string:to_float(P#xmlText.value) of
		{F,[]}->
			F;
		_->
			case string:to_integer(P#xmlText.value) of
				{I,[]}->
					I;
				_->
					list_to_atom(P#xmlText.value)
			end
	end;
process_sub_property([P=#xmlElement{}],_)->
	{P#xmlElement.name,process_sub_property(P#xmlElement.content,true)};
process_sub_property([P=#xmlElement{}|T],_)->
	[{P#xmlElement.name,process_sub_property(P#xmlElement.content,true)}]++process_property(T,false);
process_sub_property([_|T],_)->process_property(T,false);
process_sub_property(_,_)->[].
