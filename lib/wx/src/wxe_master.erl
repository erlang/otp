%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
-moduledoc false.
-behaviour(gen_server).

%% API
-export([start/1, init_env/1, init_opengl/0, fetch_msgs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {subscribers=[],  %% WxApp messages listeners
		msgs=[]   %% WxApp messages (open_file and new_file on MacOSX)
	       }).

-include("wxe.hrl").

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
init_env(SilentStart) ->
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
    gen_server:call(?MODULE, init_env, infinity),  %% sync
    wxe_util:make_env().


%%--------------------------------------------------------------------
%% Initializes the opengl library
%%--------------------------------------------------------------------
init_opengl() ->
    case get(wx_init_opengl) of
        true -> {ok, "already  initialized"};
        _ ->
            Opaque = gl:lookup_func(functions),
            Debug = gl:lookup_func(function_names),
            {ok, wxe_util:init_opengl(Opaque, Debug)}
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
    erlang:group_leader(whereis(init), self()),
    case catch erlang:system_info(smp_support) of
	true -> ok;
	_ ->
	    wxe_util:opt_error_log(SilentStart, "WX ERROR: SMP emulator required", []),
	    erlang:error({error, not_smp})
    end,
    process_flag(trap_exit, true),
    case wxe_util:init_nif(SilentStart) of
        ok -> ok;
        {error, {Reason, String}} = Err ->
            wxe_util:opt_error_log(SilentStart,
                                   "WX ERROR: Could not load library: ~p~n~s",
                                   [Reason, String]),
            erlang:error(Err)
    end,
    try
	spawn_link(fun() -> debug_ping() end),
        wxe_util:setup_consts(),
	{ok, #state{}}
    catch _:Error:ST ->
            Str = io_lib:format("Error: ~p @ ~p~n",[Error, ST]),
            logger:log(error, Str, #{domain => [wx]}),
	    error({error, {Error, "Could not initiate graphics"}})
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
handle_call(init_env, _From, State) ->
    {reply, ok, State};
handle_call(fetch_msgs, _From, State=#state{msgs=Msgs}) ->
    NewFiles = [Data || {Type, Data} <- lists:reverse(Msgs), Type == new_file],
    {reply, NewFiles, State#state{msgs=[]}};
handle_call(subscribe_msgs, {Pid, _Tag}, State=#state{subscribers=Subs}) ->
    monitor(process, Pid),
    lists:foreach(fun(Msg) -> Pid ! Msg end, lists:reverse(State#state.msgs)),
    {reply, ok, State#state{subscribers=[Pid|Subs], msgs=[]}};
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
    logger:log(error, "wx: ~s", [Msg], #{domain => [wx]}),
    {noreply, State};
handle_info({wxe_driver, internal_error, Msg}, State) ->
    logger:log(error, "wx: ~s", [Msg], #{domain => [wx]}),
    {noreply, State};
handle_info({wxe_driver, debug, Msg}, State) ->
    logger:log(notice, "wx: ~s", [Msg], #{domain => [wx]}),
    {noreply, State};
handle_info({wxe_driver, Cmd, File}, State = #state{subscribers=Subs, msgs=Msgs}) 
  when Cmd =:= open_file; Cmd =:= new_file; Cmd =:= print_file; 
       Cmd =:= open_url; Cmd =:= reopen_app ->
    lists:foreach(fun(Pid) -> Pid ! {Cmd, File} end, Subs),
    {noreply, State#state{msgs=[{Cmd, File}|Msgs]}};
handle_info({'DOWN', _Ref, process, Pid, _Info}, State) ->
    Subs = State#state.subscribers -- [Pid],
    {noreply, State#state{subscribers=Subs}};
handle_info(Info, State) ->
    logger:log(notice, "wx: Unexpected Msg: ~p", [Info], #{domain => [wx], line=>?LINE, file=>?MODULE_STRING}),
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

debug_ping() ->
    timer:sleep(1*333),
    wxe_util:debug_ping(),
    debug_ping().
