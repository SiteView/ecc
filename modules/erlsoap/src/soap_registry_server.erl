%%%-------------------------------------------------------------------
%%% File	: soap_registry_server.erl
%%% Author	: Anton Fedorov <datacompboy@call2ru.com>
%%% Original Author : Erik Reitsma <elnerei@tina29.etm.ericsson.se>
%%% Description : Server for SOAP XSDs
%%%
%%% Created :  8 Dec 2003 by Erik Reitsma <elnerei@tina29.etm.ericsson.se>
%%% Updated : 18 sep 2006 by Anton Fedorov <datacompboy@call2ru.com>
%%% Updated : 16 June 2007 by Willem de Jong <w.a.de.jong@gmail.com>
%%%           Added support for WSDL files.
%%%-------------------------------------------------------------------
-module(soap_registry_server).
-vsn("0.5").

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/1,
        start/1,
        start_link/0,
        start/0,
        stop/0]).
-export([add_xsd/2,
        get_xsd/1,
        set_xsd/2, 
        add_wsdl/2]).
-export([status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("soap.hrl").

-record(state, {wsdlModel, envmodel, xsdlist}).

-define(SERVER,?MODULE).

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server with "soap-envelope.xsd" as envelope model
%%--------------------------------------------------------------------
start_link() ->
    EnvelopeFile = case application:get_env(erlsoap,soap_envelope_xsd) of
		{ok,EnvFile} -> EnvFile;
		undefined -> "soap-envelope-1.1.xsd"
	end,
	start_link(EnvelopeFile).

%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server with given file as envelope model
%%--------------------------------------------------------------------
start_link(EnvModel) ->
	R = gen_server:start_link({local, ?SERVER}, ?MODULE, [EnvModel], []),
    io:format("server started~nModel:~p~nResult: ~P~n", [EnvModel,R, 10]),
    R.

%%--------------------------------------------------------------------
%% Function: start/0
%% Description: Starts the server with "soap-envelope.xsd" as envelope model
%%--------------------------------------------------------------------
start() ->
	EnvelopeFile = case application:get_env(erlsoap,soap_envelope_xsd) of
		{ok,EnvFile} -> EnvFile;
		undefined -> "soap-envelope-1.2.xsd"
	end,
	start(EnvelopeFile).

%%--------------------------------------------------------------------
%% Function: start/1
%% Description: Starts the server with given file as envelope model
%%--------------------------------------------------------------------
start(EnvModel) ->
	R = gen_server:start({local, ?SERVER}, ?MODULE, [EnvModel], []),
    io:format("server started~nModel:~p~nResult: ~P~n", [EnvModel,R, 10]),
    R.

%%====================================================================
%% Server functions
%%====================================================================

add_xsd(URL,XSDFile) ->
	gen_server:call(?SERVER,{add_xsd,URL,XSDFile}).

add_wsdl(URL,WSDLFile) ->
	gen_server:call(?SERVER,{add_wsdl,URL,WSDLFile}).

get_xsd(URL) ->
	gen_server:call(?SERVER,{get_xsd,URL}).

set_xsd(URL,Model) ->
	gen_server:call(?SERVER,{set_xsd,URL,Model}).

stop() ->
	gen_server:cast(?SERVER,stop).

status() ->
    case whereis(?SERVER) of
        undefined ->
            stopped;
        _Else ->
            started
    end.     

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State} 		 |
%%			{ok, State, Timeout} |
%%			ignore				 |
%%			{stop, Reason}
%%--------------------------------------------------------------------
save_data(Data) -> 
	 Filename="d:\\data.txt",
        {ok, FileIo} = file:open(Filename, [raw, write]), 
        file:write(FileIo, Data),
        file:close(FileIo).


init([EnvelopeFile]) ->
    {WsdlModel, EnvModel} = erlsoap_lib:prepareWsdlModel(EnvelopeFile), 
	{ok, #state{wsdlModel = WsdlModel, envmodel = EnvModel, xsdlist=ets:new(operation_table,[])}}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}		   |
%%			{reply, Reply, State, Timeout} |
%%			{noreply, State}			   |
%%			{noreply, State, Timeout}	   |
%%			{stop, Reason, Reply, State}   | (terminate/2 is called)
%%			{stop, Reason, State}			 (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({add_xsd,URL,XSDFile}, _From, State) ->
	case (catch erlsom:add_xsd_file(XSDFile,[{'prefix',"cwmp"}],State#state.envmodel)) of
		{'EXIT', Reason} ->
			{reply, {error, Reason}, State};
		{ok,Model} ->
			ets:insert(State#state.xsdlist,{URL,Model}),
			{reply,ok,State}
	end;

handle_call({add_wsdl,URL,WSDLFile}, _From, State) ->
	case (catch erlsoap_lib:compileWsdl(WSDLFile,State#state.wsdlModel, State#state.envmodel)) of
		{'EXIT', Reason} -> 
  			io:format("compiling WSDL failed: ~P~n", [Reason, 10]), 
            {reply, {error, Reason}, State};
        #wsdl{model = Model} -> 
            ets:insert(State#state.xsdlist,{URL,Model}),
		    {reply,ok,State}
	end;

handle_call({get_xsd,URL}, _From, State) ->
	case ets:lookup(State#state.xsdlist,URL) of
	[] ->
		{reply,{error,not_found},State};
	[{URL,Model}] ->
		{reply,{ok,Model},State}
	end;

handle_call({set_xsd,URL,Model}, _From, State) ->
	ets:insert(State#state.xsdlist,{URL,Model}),
 	{reply,ok,State};

handle_call(_Call, _From, State) ->
	{reply, {error,not_implemented}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}		  |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, State}			 (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}		  |
%%			{noreply, State, Timeout} |
%%			{stop, Reason, State}			 (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
	ets:delete(State#state.xsdlist),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
