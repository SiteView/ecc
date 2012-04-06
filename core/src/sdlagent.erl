-module(sdlagent).
-compile(export_all).
-behaviour(gen_server).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).
-define(SERVER, ?MODULE).
-record(state, {parent, master_agent}).
%% -include_lib("modules/esdl-1.0.1/include/sdl.hrl").
%% -include_lib("modules/esdl-1.0.1/include/sdl_audio.hrl").
-include("sdl.hrl").
-include("sdl_audio.hrl").

playaudio(FileName, AudioSleepTime) ->
    %%sdl:init(?SDL_INIT_AUDIO),
    start_link(?SDL_INIT_AUDIO),
    %%io:format("FileName = ~p~n", [FileName]),
    {ASpec,Sample} = sdl_audio:loadWAV(FileName),
    Obtained = sdl_audio:openAudio(ASpec, true),
    %%io:format("Driver: ~s\n", [sdl_audio:audioDrivername()]),
    %%io:format("Obtained: ~p\n", [Obtained]),
    sdl_audio:play_audio(Sample, 1),
    sdl_audio:pauseAudio(false),
    if
        AudioSleepTime > 0 ->
            timer:sleep(AudioSleepTime),
            sdl_audio:pauseAudio(true),
            sdl_audio:freeWAV(Sample),
            sdl_audio:closeAudio();
        true ->
            ok
    end,
    sdl:getError().    
    

init([Parent, Opts]) ->
    %%process_flag(trap_exit, true),
    io:format("start sdl!!!!!!!!!!!!!!!!!!~n"),
    try sdl:init(Opts) of
        _ ->
            {ok, #state{parent = Parent}}
    catch
        _:Reason ->
            sdl:quit(),
            {stop, Reason}
    end.

start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).
    
stop() ->
    cast(stop).
    
%%-------------------------------------------------------------------

handle_call(Req, From, State) ->
    {reply, {error, {unknown_request, Req}}, State}.
    
handle_cast(Msg, State) ->
    {noreply, State}.
    
handle_info(Info, State) ->
    {noreply, State}.
    
terminate(_Reason, _State) ->
    ok.


code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};

% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.
    
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).