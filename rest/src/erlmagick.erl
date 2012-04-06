%%%----------------------------------------------------------------------
%%% File    : iconv.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Interface to libiconv
%%% Created : 16 Feb 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2008   Process-one
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%                         
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(erlmagick).
-author('alexey@process-one.net').

-behaviour(gen_server).

-export([start/0, start_link/0, transformImage/1, rotateImage/1, captcha/1]).

%% Internal exports, call-back functions.
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2]).


start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {_, Path} = file:get_cwd(),
    io:format("--->~p~n", [Path]),
	erl_ddll:start(),
    case erl_ddll:load_driver(Path++"/priv/lib", erlmagick_drv) of
	ok -> ok;
	{error, already_loaded} -> ok;
	{error,ErrorDesc} -> io:format("===>~p~n", [erl_ddll:format_error(ErrorDesc)]),exit({error, could_not_load_driver});
	_ ->  exit({error, could_not_load_driver})
    end,
    Port = open_port({spawn, erlmagick_drv}, []),
%    ets:new(magick_table, [set, public, named_table]),
%    ets:insert(magick_table, {port, Port}),
    {ok, Port}.


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

port()->
    gen_server:call(?MODULE, port, 1000).

handle_call(port, _, Port)->
    {reply, Port, Port};
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'EXIT', Port, Reason}, Port) ->
    {stop, {port_died, Reason}, Port};
handle_info({'EXIT', _Pid, _Reason}, Port) ->
    {noreply, Port};
handle_info(_, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, Port) ->
    Port ! {self, close},
    ok.

%test1()->
%    [{port, Port} | _] = ets:lookup(magick_table, port),
%    Bin = term_to_binary({true, [{h, [1,300]}, {t, [30000]}]}),
%    Result = port_control(Port, 1, Bin),
%    binary_to_term(Result).
%
%test2()->
%    [{port, Port} | _] = ets:lookup(magick_table, port),
%    Bin = term_to_binary([]),
%    Result = port_control(Port, 2, Bin),
%    binary_to_term(Result).

%{image, format, width, height}
transformImage({Image, Format, Width, Height}) when is_integer(Width),is_integer(Height) ->
    %[{port, Port} | _] = ets:lookup(magick_table, port),
    Port = port(),
    Bin = term_to_binary({Image, Format, Width, Height}),
    Result = port_control(Port, 0, Bin),
    binary_to_term(Result);
	
transformImage({Image, Format, Width, Height}) when is_list(Width),is_list(Height) ->
    transformImage({Image, Format, list_to_integer(Width), list_to_integer(Height)}).

%{image, format, rotate}
rotateImage({Image, Format, Width, Height, Rotate})when is_integer(Width),is_integer(Height), is_integer(Rotate) ->
    %[{port, Port} | _] = ets:lookup(magick_table, port),
    Port = port(),
    Bin = term_to_binary({Image, Format, Width, Height, Rotate}),
    Result = port_control(Port, 1, Bin),
    binary_to_term(Result);
rotateImage({Image, Format, Width, Height, Rotate})when is_list(Width),is_list(Height), is_list(Rotate) ->
    rotateImage({Image, Format, list_to_integer(Width), list_to_integer(Height), list_to_integer(Rotate)}).

captcha(Text)->    
	Width = 131,
	Height = 62,
	Port = port(),
	Bin = term_to_binary({Text, Width, Height}),
	Result = port_control(Port, 2, Bin),
	binary_to_term(Result).
