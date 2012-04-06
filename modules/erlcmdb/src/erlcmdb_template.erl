-module(erlcmdb_template).
-include("erlcmdb.hrl").
-compile(export_all).

start()->
	case lists:member(?MODULE,ets:all()) of
		true->
			{error,already_started};
		_->
			ets:new(?MODULE,[bag,named_table,public]),
			case file:consult(get_conf_file()) of
				{ok,Ci}->
					ets:insert(?MODULE,Ci),
					ok;
				{error,Err}->
					{error,Err};
				_->
					{error,read_file_error}
			end
	end.

stop()->
	ets:delete(?MODULE).
	
	
get_template(Type)->
	Ts = get_all_template(),
	case [X||X<-Ts,X#erlcmdb_ci.type==Type] of
		[]->
			undefined;
		[T|_]->
			T
	end.
	
get_child_template(Type)->
	Ts = get_all_template(),
	[X||X<-Ts,X#erlcmdb_ci.parent==Type].
	
get_root_template()->
	"ci".
	

get_conf_file()->
	"conf/erlcmdb.ci".
	
get_all_template()->
	ets:tab2list(?MODULE).
