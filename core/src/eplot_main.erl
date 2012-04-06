
%% http://github.com/psyeugenic/eplot

-module(eplot_main).
%% -compile(export_all).
-export([init/0, run/0, run/1, png/3, eview/2,getimage/2]).



run(_) ->
	run().

init()->
	egd_chart:init().

run()->
	iconv:start(), 
	Options = get_config(),
	Inputs  = proplists:get_value(input_data, Options),	
    Data   = parse_data_files(Inputs, proplists:get_value(x_data_type, Options, datetime)),
	Func    = proplists:get_value(run_function, Options),
	run_function(Func, Data, Options).

getimage( Data, Options)->
	png(proplists:get_value(save_file, Options), Data, Options).

run_function(eview, Data, Options)->
	eview(Data, Options);
run_function(png, Data, Options)->	
	png(proplists:get_value(save_file, Options), Data, Options).


png(Output, Data, Options0) ->
    Options = merge_options(Options0, get_config()),
	some_check(Data, Options),
    B       = graph_binary(proplists:get_value(plot, Options), Data, Options),
    egd:save(B, Output),
    ok.

eview(Data, Options0) ->
    Options = proplists:delete(type, merge_options(Options0, get_config())),
	some_check(Data, Options),
    W       = proplists:get_value(width, Options),
    H       = proplists:get_value(height, Options),
    P       = eview:start({W+10,H+32}),
    B       = graph_binary(proplists:get_value(plot, Options), Data, [{type, raw_bitmap}] ++ Options),
    P ! {self(), bmp_image, B, {W,H}},
    receive {P, done} -> ok end.


some_check([{_,[]}], Options)->
	throw("The plotting data is empty! Please check it!");
some_check(_Data, Options)->
	Plot= proplists:get_value(plot, Options),
	XDataType= proplists:get_value(x_data_type, Options),
	case Plot=/=bar2d andalso XDataType=:=topN_text of
		true->
			throw("Data type of X-axis is topN_text, that can only plot a bar picture!");
		_->
			ok
	end.
	

graph_binary(plot2d,Data, Options) ->
    egd_chart:graph(Data, Options);
graph_binary(bar2d, Data, Options) ->
    egd_chart:bar2d(Data, Options);
graph_binary(Type, _, _) -> io:format("Bad engine: ~p~n", [Type]), exit(unnormal).

parse_data_files(Filenames, XDataType) -> parse_data_files(Filenames, [], XDataType).
parse_data_files([], Out, XDataType) -> lists:reverse(Out);
parse_data_files([Filename|Filenames], Out, XDataType) ->
    Data = parse_data_file(Filename, XDataType),
    parse_data_files(Filenames, [{Filename, Data}|Out], XDataType).


merge_options([], Out) -> Out;
merge_options([{Key, Value}|Opts], Out) ->
    case proplists:get_value(Key, Out) of
	undefined -> merge_options(Opts, [{Key, Value}|Out]);
	_         -> merge_options(Opts, [{Key, Value}|proplists:delete(Key, Out)])
    end.

data_speedup([]) -> [];
data_speedup([{Filename,[{X,Y}|T]}|Data]) -> 
    Speedup = data_speedup(T, Y, [{X,1}]),
    [{Filename, Speedup}|data_speedup(Data)].


data_speedup([], _, Out) -> lists:reverse(Out);
data_speedup([{X,Y}|T], F, Out) -> data_speedup(T, F, [{X,F/Y}|Out]).


parse_data_file(Filename, XDataType) when XDataType =:= topN_text ->
	eplot_analyse:get_topN_text(Filename);
parse_data_file(Filename, XDataType) when XDataType =:= datetime ->
	eplot_analyse:translate_xdatetime_to_number(Filename);
parse_data_file(Filename, XDataType) ->
    {ok, Fd} = file:open(Filename, [read]),
    parse_data_file(Fd, io:get_line(Fd, ""), []).


parse_data_file(Fd, eof, Out) -> file:close(Fd), lists:reverse(Out);
parse_data_file(Fd, String, Out) ->
    % expected string is 'number()<whitespace(s)>number()'
    Tokens = string:tokens(String, " \t\n\r"),
    Item = tokens2item(Tokens),
    parse_data_file(Fd, io:get_line(Fd, ""), [Item|Out]).
    
tokens2item(Tokens) ->
    Is = lists:map(fun
	(String) ->
	    list_to_term(String)
	end, Tokens),
    [X,Y|_] = Is,
    {X,Y}.

list_to_term(String) ->
    case catch list_to_number(String) of
	{'EXIT', _} -> list_to_atom(String);
	I -> I
    end.

list_to_number(String) ->
    case catch list_to_integer(String) of
	I when is_integer(I) -> I;
	_ -> list_to_float(String)
    end.

get_config() ->
%%     Home = os:cmd("echo -n $HOME"),
%%     Path = filename:join([Home, ".eplot"]),
	Path= ".",
    File = filename:join([Path, "eplot.config"]),
    case file:consult(File) of
	{ok, Terms} -> Terms;
	{error, enoent} -> make_config(Path, File)
    end.

make_config(Path, File) ->
    Defaults = [{width, 800}, {height, 480}],
    try
	file:make_dir(Path),
    	{ok, Fd} = file:open(File, [write]),
    	[io:format(Fd, "~p.~n", [Opt]) || Opt <- Defaults],
    	file:close(Fd),
	Defaults
    catch
	A:B ->
	    io:format("Error writing config. ~p ~p~n", [A,B]),
	    Defaults
    end.
    

test1(Fname) ->
    {ok, Terms} = file:consult(Fname),
    Text = proplists:get_value(title1, Terms),
   
	GbBin  = iolist_to_binary(Text),
	GetCode= egd_chart:gbkToUnicode(Text),
    UniBin = iconv:convertBinary("gbk", "unicode", GbBin),
    UniList = unicode:characters_to_list(UniBin),
    io:format(" bin : ~ts~n bin : ~p~n",[UniBin, UniBin]),
    io:format(" list: ~ts~n list: ~p~n", [UniList, UniList]).



