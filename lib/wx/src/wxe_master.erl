%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%%-------------------------------------------------------------------
%%% File    : wxe_server.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : 
%%%
%%% Created : 17 Jan 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

%% @hidden
-module(wxe_master).
-behaviour(gen_server).

%% API
-export([start/1, init_port/1, init_opengl/0, fetch_msgs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {cb_port,  %% Callback port and to erlang messages goes via it.
		users,    %% List of wx servers, needed ??
		driver,   %% Driver name so wx_server can create it's own port
		msgs=[]   %% Early messages (such as openfiles on OSX)
	       }).

-include("wxe.hrl").
-include("gen/wxe_debug.hrl").

-define(DRIVER, "wxe_driver").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(SilentStart) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(SilentStart) ->
    gen_server:start({local, ?MODULE}, ?MODULE, [SilentStart], []).

%%--------------------------------------------------------------------
%% Function: init_port(SilentStart) -> {UserPort,CallBackPort} | error(Error)
%% Description: Creates the port
%%--------------------------------------------------------------------
init_port(SilentStart) ->
    case whereis(?MODULE) of
	undefined ->
	    case start(SilentStart) of
		{ok,Pid} -> Pid;
		{error,{already_started,Pid}} -> Pid;
		{error, {Reason,Stack}} ->
		    erlang:raise(error, Reason, Stack)
	    end;
	Pid ->
	    Pid
    end,
    {Driver, CBport} = gen_server:call(?MODULE, init_port, infinity),
    Port = open_port({spawn,Driver},[binary]),
    receive wx_port_initiated -> ok end,
    {Port, CBport}.


%%--------------------------------------------------------------------
%% Initalizes the opengl library
%%--------------------------------------------------------------------
init_opengl() ->
    case get(wx_init_opengl) of
        true -> {ok, "already  initialized"};
        _ ->
            GLLib = wxe_util:wxgl_dl(),
            Res = wxe_util:call(?WXE_INIT_OPENGL, <<(list_to_binary(GLLib))/binary, 0:8>>),
            element(1, Res) =:= ok andalso put(wx_init_opengl, true),
            Res
    end.

%%--------------------------------------------------------------------
%% Fetch early messages, hack to get start up args on mac
%%--------------------------------------------------------------------
fetch_msgs() ->
    gen_server:call(?MODULE, fetch_msgs, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([SilentStart]) ->
    DriverName = ?DRIVER,
    PrivDir = wxe_util:priv_dir(?DRIVER, SilentStart),
    erlang:group_leader(whereis(init), self()),
    case catch erlang:system_info(smp_support) of
	true -> ok;
	_ -> 
	    wxe_util:opt_error_log(SilentStart,
                                   "WX ERROR: SMP emulator required"
                                   " (start with erl -smp)",
                                   []),
	    erlang:error(not_smp)
    end,

    case erl_ddll:load_driver(PrivDir,DriverName) of
	ok -> ok;
	{error, What} ->
	    wxe_util:opt_error_log(SilentStart,
                                   "WX Failed loading ~p@~p ~n",
                                   [DriverName,PrivDir]),
	    Str = erl_ddll:format_error(What),
	    erlang:error({load_driver,Str})
    end,
    process_flag(trap_exit, true),
    DriverWithArgs = DriverName ++ " " ++ code:priv_dir(wx),

    try
	Port = open_port({spawn, DriverWithArgs},[binary]),
	wx_debug_info = ets:new(wx_debug_info, [named_table]),
	wx_non_consts = ets:new(wx_non_consts, [named_table]),
	true = ets:insert(wx_debug_info, wxdebug_table()),
	spawn_link(fun() -> debug_ping(Port) end),
	receive
	    {wx_consts, List} ->
		true = ets:insert(wx_non_consts, List)
	end,
	{ok, #state{cb_port=Port, driver=DriverName, users=gb_sets:empty()}}
    catch _:Err ->
	    error({Err, "Could not initiate graphics"})
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(init_port, From, State=#state{driver=Driver,cb_port=CBPort, users=Users}) ->
    {reply, {Driver,CBPort}, State#state{users=gb_sets:add(From,Users)}};
handle_call(fetch_msgs, _From, State=#state{msgs=Msgs}) ->
    {reply, lists:reverse(Msgs), State#state{msgs=[]}};
handle_call(_Request, _From, State) ->
    %%io:format("Unknown request ~p sent to ~p from ~p ~n",[_Request, ?MODULE, _From]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    %%io:format("Unknown message ~p sent to ~p~n",[_Msg, ?MODULE]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({wxe_driver, error, Msg}, State) ->
    error_logger:error_report([{wx, error}, {message, lists:flatten(Msg)}]),
    {noreply, State};
handle_info({wxe_driver, internal_error, Msg}, State) ->
    error_logger:error_report([{wx, internal_error}, {message, lists:flatten(Msg)}]),
    {noreply, State};
handle_info({wxe_driver, debug, Msg}, State) ->
    io:format("WX DBG: ~s~n", [Msg]),
    {noreply, State};
handle_info({wxe_driver, open_file, File}, State=#state{msgs=Msgs}) ->
    {noreply, State#state{msgs=[File|Msgs]}};
handle_info(_Info, State) ->
    io:format("Unknown message ~p sent to ~p~n",[_Info, ?MODULE]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    erlang:display({?MODULE, killed, process_info(self(),trap_exit),_Reason}),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%%%%%%%%%%%% INTERNAL %%%%%%%%%%%%%%%%%%%%%%%%

debug_ping(Port) ->
    timer:sleep(1*333),    
    _R = (catch erlang:port_call(Port, 0, [])),
%%    io:format("Erlang ping ~p ~n", [_R]),
    debug_ping(Port).
