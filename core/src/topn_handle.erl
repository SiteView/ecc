-module(topn_handle).
-compile(export_all).
-define(ATTRIBUTE, attribute).
-define(PROPERTY, property).

loop() ->
    receive
        {From, stop} ->
            From ! {self(), {ok, stoped}};
        {From, {set_attribute, Key, Value}} ->
            Ars =
            case get(?ATTRIBUTE) of
                undefined ->
                    [{Key, Value}];
                Attrs when erlang:is_list(Attrs) ->
                    lists:keystore(Key, 1, Attrs, {Key, Value});
                _ ->
                    [{Key, Value}]
            end,
            put(?ATTRIBUTE, Ars),
            From ! {self(),{ok,set_attribute_ok}},
            loop();
        {From, {get_attribute, Key}} ->
            Ars = 
            case get(?ATTRIBUTE) of
                undefined ->
                    [];
                Attrs when erlang:is_list(Attrs) ->
                    Attrs;
                _ ->
                    []
            end,
            From ! {self(), get_attribute(Key,Ars)},
            loop();
        {From, {get_attributes}} ->
            Ars = 
            case get(?ATTRIBUTE) of
                undefined ->
                    [];
                Attrs when erlang:is_list(Attrs) ->
                    Attrs;
                _ ->
                    []
            end,
            From ! {self(), {ok, Ars}},
            loop();
        {From, {set_property,Key, Value}} ->
            Ars =
            case get(?PROPERTY) of
                undefined ->
                    [{Key, Value}];
                Attrs when erlang:is_list(Attrs) ->
                    lists:keystore(Key, 1, Attrs, {Key, Value});
                _ ->
                    [{Key, Value}]
            end,
            put(?PROPERTY, Ars),
            From ! {self(),{ok,set_property_ok}},
            loop();
        {From, {get_property, Key}} ->
            Ars = 
            case get(?PROPERTY) of
                undefined ->
                    [];
                Attrs when erlang:is_list(Attrs) ->
                    Attrs;
                _ ->
                    []
            end,
            From ! {self(), get_attribute(Key,Ars)},
            loop();
        {From, {get_properties}} ->
            Ars = 
            case get(?PROPERTY) of
                undefined ->
                    [];
                Attrs when erlang:is_list(Attrs) ->
                    Attrs;
                _ ->
                    []
            end,
            From ! {self(), {ok, Ars}},
            loop();
        _ ->
            loop()
    end.
    
    
get_attribute(Name,Data)->
	case lists:keysearch(Name,1,Data) of
		false->
			{error,{Name,not_found}};
		{value,Val}->
			{ok,Val};
		Else->
			{error,Else}
	end.