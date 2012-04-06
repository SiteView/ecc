
-module(index_store).
	
-behaviour(gen_server).

-compile(export_all).

-include("monitor.hrl").
-include("monitor_template.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% gen_server callbacks
 -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
		 
-record(state, {parent,files=[]}).

-define(INDEX_FILE,"logs/index.db").

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

init([Opts])->
	{ok,#state{files=Opts}}.

stop() ->
    gen_server:cast(?MODULE, stop).
	
create()->
	gen_server:cast(?MODULE,create).
	
find(Tag)->
	gen_server:call(?MODULE,{find,Tag,dbcs_base:get_app()}).
	
update(machine,Data=#machine{})->
	gen_server:cast(?MODULE,{update,machine,Data,dbcs_base:get_app()});
update(monitor,Data)->
	gen_server:cast(?MODULE,{update,monitor,Data,dbcs_base:get_app()});
update(rule,Data)->
	gen_server:cast(?MODULE,{update,rule,Data,dbcs_base:get_app()});
update(report,Data)->
	gen_server:cast(?MODULE,{update,report,Data,dbcs_base:get_app()});
update(_,_)->{error,parameter_error}.

remove(machine,Id)->
	gen_server:cast(?MODULE,{remove,machine,Id,dbcs_base:get_app()});
remove(monitor,Id)->
	gen_server:cast(?MODULE,{remove,monitor,Id,dbcs_base:get_app()});
remove(rule,Id)->
	gen_server:cast(?MODULE,{remove,rule,Id,dbcs_base:get_app()});
remove(report,Id)->
	gen_server:cast(?MODULE,{remove,report,Id,dbcs_base:get_app()});
remove(_,_)->{error,parameter_error}.



handle_call({find,TagStr,App}, _, State)->
	case dets:open_file(index_file, [{type,bag},{keypos,1},{file,?INDEX_FILE}]) of
		{ok,_}->
			Tag = string:to_upper(TagStr),
			Ret = qlc:q([{Key,Id,Entity,Prop}||
					{Key,Id,Entity,Prop,Ap}<-dets:table(index_file),Ap=:= App andalso case re:run(Key,Tag) of {match,_}->true;_->false end]),
			R = qlc:e(Ret),
			dets:close(index_file),
			{reply, {ok,R}, State};
		Else->
			{reply, Else, State}
	end;
handle_call(Req, _, State) ->
    {reply, {error,unknown_request}, State}.
	
handle_cast({remove,Type,Id,App}, S) ->
	case dets:open_file(index_file, [{type,bag},{keypos,1},{file,?INDEX_FILE}]) of
		{ok,_}->
			dets:match_delete(index_file, {'_', Id, Type, '_',App}),
			dets:close(index_file);
		_->
			error
	end,
	{noreply,S};
			
handle_cast({update,Type,Data,App}, S) ->
	case dets:open_file(index_file, [{type,bag},{keypos,1},{file,?INDEX_FILE}]) of
		{ok,_}->
			case Type of
				machine->
					dets:match_delete(index_file, {'_', Data#machine.id, '_', '_',App}),
					dets:insert(index_file,[{string:to_upper(Data#machine.host),Data#machine.id,machine,host,App},
											{string:to_upper(Data#machine.name),Data#machine.id,machine,name,App}]);
				monitor->
					Id = proplists:get_value(id,Data),
					Class = proplists:get_value(class,Data),
					dets:match_delete(index_file,{'_', Id, '_', '_',App}),
					M = Class:new(),
					Mt = M:get_template_property(),
					M:delete(),
					lists:foldl(fun(X,R)->
						case lists:member(X#property.type,[server,text]) of
							true->
								case proplists:get_value(X#property.name,Data,"") of
									""->
										R;
									Pv->
										dets:insert(index_file, {string:to_upper(Pv),Id,monitor,X#property.name,App}),
										R
								end;
							_->
								R
						end
					end, [], Mt);
				rule->
					Name = proplists:get_value(name,Data,""),
					Id = proplists:get_value(id,Data),
					case Name of
						""->
							pass;
						_->
							dets:insert(index_file,[{string:to_upper(Name),Id,rule,name,App}])
					end;
				report->
					Title = proplists:get_value(title,Data,""),
					Desc = proplists:get_value(description,Data,""),
					Id = proplists:get_value(id,Data),
					case Title of
						""->
							pass;
						_->
							dets:insert(index_file,[{string:to_upper(Title),Id,report,title,App}])
					end,
					case Desc of
						""->
							pass;
						_->
							dets:insert(index_file,[{string:to_upper(Desc),Id,report,description,App}])
					end;
				_->
					pass
			end,
			dets:close(index_file);
		_->
			error
	end,
	{noreply,S};
	
handle_cast(create, S) ->
	file:delete(?INDEX_FILE),
	case dets:open_file(index_file, [{type,bag},{keypos,1},{file,?INDEX_FILE}]) of
		{ok,_}->
			Apps = app:all(),
			lists:map(fun(App)->
			
			dbcs_base:set_app(App,true),
			% 索引主机
			Machs = dbcs_machine:get_all(),
			lists:foldl(fun(X,R)->
					dets:insert(index_file,[{string:to_upper(X#machine.host),X#machine.id,machine,host,App},
											{string:to_upper(X#machine.name),X#machine.id,machine,name,App}]),
					R
				end,[],Machs),
			% 索引监测器
			Monitors = dbcs_monitor:get_all(),
			lists:foldl(fun(X,R)->
					timer:sleep(10),
					Id = proplists:get_value(id,X),
					Class = proplists:get_value(class,X),
					M = Class:new(),
					Mt = M:get_template_property(),
					M:delete(),
					lists:foldl(fun(X1,R1)->
						case lists:member(X1#property.type,[server,text]) of
							true->
								case proplists:get_value(X1#property.name,X,"") of
									""->
										R1;
									Pv->
										dets:insert(index_file, {string:to_upper(Pv),Id,monitor,X1#property.name,App}),
										R1
								end;
							_->
								R1
						end
					end, [], Mt),
					R
				end, [], Monitors),
				
			% 索引告警
			Alerts =  dbcs_rule:get_all(),
			lists:foldl(fun(X,R)->
					Name = proplists:get_value(name,X,""),
					Id = proplists:get_value(id,X),
					case Name of
						""->
							pass;
						_->
							dets:insert(index_file,[{Name,Id,rule,name,App}])
					end,
					R
				end, [], Alerts),
				
			%索引报告
			Reports = dbcs_report:get_all(),
			lists:foldl(fun(X,R)->
					Title = proplists:get_value(title,X,""),
					Desc = proplists:get_value(description,X,""),
					Id = proplists:get_value(id,X),
					case Title of
						""->
							pass;
						_->
							dets:insert(index_file,[{string:to_upper(Title),Id,report,title,App}])
					end,
					case Desc of
						""->
							pass;
						_->
							dets:insert(index_file,[{string:to_upper(Desc),Id,report,description,App}])
					end,
					R
				end, [], Reports),
				timer:sleep(1000)
			end,Apps),
			dets:close(index_file),
			{noreply,S};
		_->
			{noreply,S}
	end;
handle_cast(stop, S) ->
    {stop, normal, S}.
	
handle_info(_, State) ->
    {noreply, State}.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.