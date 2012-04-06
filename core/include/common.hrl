%%name regedit
%%
name_register(Name) ->
    try register(Name, self()) of
	true -> true
    catch
	error:_ ->
	    {false, whereis(Name)}
    end.

