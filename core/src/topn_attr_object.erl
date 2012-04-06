-module(topn_attr_object, [Tid]).
-define(TIMEOUT,5000).
-compile(export_all).



new() ->
    Id = erlang:spawn_link(fun() -> topn_handle:loop() end),
    {?MODULE,Id}.
    
getTid() ->
    Tid.
    
set_attribute(Key, Value) ->
    Tid ! {self(), {set_attribute, Key, Value}},
    receive
        {Tid,Ret}->
            Ret
    after ?TIMEOUT ->
        {error, timeout}
    end.
    
get_attribute(Key) ->
    Tid ! {self(), {get_attribute, Key}},
    receive
        {Tid, Ret} ->
            Ret
    after ?TIMEOUT ->
        {error, timeout}
    end.
    
get_attributes() ->
    Tid ! {self(), {get_attributes}},
    receive
        {Tid, Ret} ->
            Ret
    after ?TIMEOUT ->
        {error, timeout}
    end.
    
set_property(Key, Value) ->
    Tid ! {self(), {set_property,Key, Value}},
    receive
        {Tid, Ret} ->
            Ret
    after ?TIMEOUT ->
        {error, timeout}
    end.
    
get_property(Key) ->
    Tid ! {self(), {get_property, Key}},
    receive
        {Tid, Ret} ->
            Ret
    after ?TIMEOUT ->
        {error, timeout}
    end.
    
get_properties() ->
    Tid ! {self(), {get_properties}},
    receive
        {Tid, Ret} ->
            Ret
    after ?TIMEOUT ->
        {error, timeout}
    end.
    
