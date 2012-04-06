%% ---
%%siteview object static
%%
%%---
-module(siteview_object_static).
-compile(export_all).
-extends(propertied_object_static).
-include("monitor.hrl").


create_object(Data)->
	case is_list(Data) of
		false->
			{error,data_error};
		true->
			case lists:keysearch(class,1,Data) of
				{value,{class,Class}}->
					create_object(Class,Data);
				_->
					{error,not_found_class}
			end
	end.
	

create_object(Mod,Data)->
	try Mod:new(Data)
	catch
		_:_->{error,create_class_error}
	end.

