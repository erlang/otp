%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%% File    : wx_object.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Frame work for erlang sub-classes.
%%%
%%% Created : 25 Nov 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
%%
%% @doc wx_object - Generic wx object behaviour
%%
%% This is a behaviour module that can be used for "sub classing" 
%% wx objects. It works like a regular gen_server module and creates
%% a server per object.  
%%
%% NOTE: Currently no form of inheritance is implemented.
%% 
%% 
%% The user module should export:
%%   
%%   init(Args) should return <br/>
%%     {wxObject, State} | {wxObject, State, Timeout} |
%%         ignore | {stop, Reason}
%%
%%   handle_call(Msg, {From, Tag}, State) should return <br/>
%%    {reply, Reply, State} | {reply, Reply, State, Timeout} |
%%        {noreply, State} | {noreply, State, Timeout} |
%%        {stop, Reason, Reply, State}  
%%
%%   Asynchronous window event handling: <br/>
%%   handle_event(#wx{}, State)  should return <br/>
%%    {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State} 
%%
%%   Info is message e.g. {'EXIT', P, R}, {nodedown, N}, ...  <br/>
%%   handle_info(Info, State)  should return , ...  <br/>
%%    {noreply, State} | {noreply, State, Timeout} | {stop, Reason, State} 
%% 
%%   When stop is returned in one of the functions above with Reason =
%% normal | shutdown | Term, terminate(State) is called. It lets the
%% user module clean up, it is always called when server terminates or
%% when wxObject() in the driver is deleted. If the Parent process
%% terminates the Module:terminate/2 function is called. <br/>
%% terminate(Reason, State)
%%
%%
%% Example:  
%% 
%% ```
%% -module(myDialog).
%% -export([new/2, show/1, destroy/1]).  %% API 
%% -export([init/1, handle_call/3, handle_event/2, 
%%          handle_info/2, code_change/3, terminate/2]).
%%          new/2, showModal/1, destroy/1]).  %% Callbacks
%% 
%% %% Client API
%% new(Parent, Msg) ->
%%    wx_object:start(?MODULE, [Parent,Id], []).
%%
%% show(Dialog) ->
%%    wx_object:call(Dialog, show_modal).
%%
%% destroy(Dialog) -> 
%%    wx_object:call(Dialog, destroy).
%%
%% %% Server Implementation ala gen_server
%% init([Parent, Str]) ->
%%    Dialog = wxDialog:new(Parent, 42, "Testing", []),
%%    ... 
%%    wxDialog:connect(Dialog, command_button_clicked), 
%%    {Dialog, MyState}.
%%
%% handle_call(show, _From, State) ->
%%    wxDialog:show(State#state.win),
%%    {reply, ok, State};
%% ...
%% handle_event(#wx{}, State) ->
%%    io:format("Users clicked button~n",[]),
%%    {noreply, State};
%% ...
%% '''

-module(wx_object).
-include("wxe.hrl").
-include("../include/wx.hrl").

%% API
-export([start/3, start/4,
	 start_link/3, start_link/4,
	 stop/1, stop/3,
	 call/2, call/3,
	 cast/2,
	 reply/2,
	 get_pid/1,
	 set_pid/2
	]).

%% -export([behaviour_info/1]).
-callback init(Args :: term()) ->
    {#wx_ref{}, State :: term()} | {#wx_ref{}, State :: term(), timeout() | 'hibernate'} |
    {'stop', Reason :: term()} | 'ignore'.
-callback handle_event(Request :: #wx{}, State :: term()) ->
    {'noreply', NewState :: term()} |
    {'noreply', NewState :: term(), timeout() | 'hibernate'} |
    {'stop', Reason :: term(), NewState :: term()}.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {'reply', Reply :: term(), NewState :: term()} |
    {'reply', Reply :: term(), NewState :: term(), timeout() | 'hibernate'} |
    {'noreply', NewState :: term()} |
    {'noreply', NewState :: term(), timeout() | 'hibernate'} |
    {'stop', Reason :: term(), Reply :: term(), NewState :: term()} |
    {'stop', Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {'noreply', NewState :: term()} |
    {'noreply', NewState :: term(), timeout() | 'hibernate'} |
    {'stop', Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout() | term(), State :: term()) ->
    {'noreply', NewState :: term()} |
    {'noreply', NewState :: term(), timeout() | 'hibernate'} |
    {'stop', Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: ('normal' | 'shutdown' | {'shutdown', term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {'down', term()}), State :: term(),
                      Extra :: term()) ->
    {'ok', NewState :: term()} | {'error', Reason :: term()}.


%% System exports
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 format_status/2]).

%% Internal exports
-export([init_it/6]).

-import(error_logger, [format/2]).

%%%=========================================================================
%%%  API
%%%=========================================================================
%% @hidden
%% behaviour_info(callbacks) ->
%%     [{init,1},
%%      {handle_call,3},
%%      {handle_info,2},
%%      {handle_event,2},
%%      {terminate,2},
%%      {code_change,3}];
%% behaviour_info(_Other) ->
%%     undefined.


%%  -----------------------------------------------------------------
%% @spec (Mod, Args, Options) -> wxWindow:wxWindow() 
%%   Mod = atom() 
%%   Args = term()
%%   Options = [{timeout, Timeout} | {debug, [Flag]}]
%%   Flag = trace | log | {logfile, File} | statistics | debug
%% @doc Starts a generic wx_object server and invokes Mod:init(Args) in the
%% new process.
start(Mod, Args, Options) ->
    gen_response(gen:start(?MODULE, nolink, Mod, Args, [get(?WXE_IDENTIFIER)|Options])).
	    
%% @spec (Name, Mod, Args, Options) -> wxWindow:wxWindow() 
%%   Name = {local, atom()}
%%   Mod = atom() 
%%   Args = term()
%%   Options = [{timeout, Timeout} | {debug, [Flag]}]
%%   Flag = trace | log | {logfile, File} | statistics | debug
%% @doc Starts a generic wx_object server and invokes Mod:init(Args) in the
%% new process.
start(Name, Mod, Args, Options) ->
    gen_response(gen:start(?MODULE, nolink, Name, Mod, Args, [get(?WXE_IDENTIFIER)|Options])).

%% @spec (Mod, Args, Options) -> wxWindow:wxWindow() 
%%   Mod = atom() 
%%   Args = term()
%%   Options = [{timeout, Timeout} | {debug, [Flag]}]
%%   Flag = trace | log | {logfile, File} | statistics | debug
%% @doc Starts a generic wx_object server and invokes Mod:init(Args) in the
%% new process.
start_link(Mod, Args, Options) ->
    gen_response(gen:start(?MODULE, link, Mod, Args, [get(?WXE_IDENTIFIER)|Options])).

%% @spec (Name, Mod, Args, Options) -> wxWindow:wxWindow() 
%%   Name = {local, atom()}
%%   Mod = atom() 
%%   Args = term()
%%   Options = [{timeout, Timeout} | {debug, [Flag]}]
%%   Flag = trace | log | {logfile, File} | statistics | debug
%% @doc Starts a generic wx_object server and invokes Mod:init(Args) in the
%% new process.
start_link(Name, Mod, Args, Options) ->
    gen_response(gen:start(?MODULE, link, Name, Mod, Args, [get(?WXE_IDENTIFIER)|Options])).
    
gen_response({ok, Pid}) ->
    receive {ack, Pid, Ref = #wx_ref{}} -> Ref end;
gen_response(Reply) ->
    Reply.

%% @spec (Ref::wxObject()|atom()|pid()) -> ok
%% @doc Stops a generic wx_object server with reason 'normal'.
%% Invokes terminate(Reason,State) in the server. The call waits until
%% the process is terminated. If the process does not exist, an
%% exception is raised.
stop(Ref = #wx_ref{state=Pid}) when is_pid(Pid) ->
    try
	gen:stop(Pid)
    catch _:ExitReason ->
	    erlang:error({ExitReason, {?MODULE, stop, [Ref]}})
    end;
stop(Name) when is_atom(Name) orelse is_pid(Name) ->
    try
	gen:stop(Name)
    catch _:ExitReason ->
	    erlang:error({ExitReason, {?MODULE, stop, [Name]}})
    end.

%% @spec (Ref::wxObject()|atom()|pid(), Reason::term(), Timeout::timeout()) -> ok
%% @doc Stops a generic wx_object server with the given Reason.
%% Invokes terminate(Reason,State) in the server. The call waits until
%% the process is terminated. If the call times out, or if the process
%% does not exist, an exception is raised.
stop(Ref = #wx_ref{state=Pid}, Reason, Timeout) when is_pid(Pid) ->
    try
	gen:stop(Pid, Reason, Timeout)
    catch _:ExitReason ->
	    erlang:error({ExitReason, {?MODULE, stop, [Ref, Reason, Timeout]}})
    end;
stop(Name, Reason, Timeout) when is_atom(Name) orelse is_pid(Name) ->
    try
	gen:stop(Name, Reason, Timeout)
    catch _:ExitReason ->
	    erlang:error({ExitReason, {?MODULE, stop, [Name, Reason, Timeout]}})
    end.

%% @spec (Ref::wxObject()|atom()|pid(), Request::term()) -> term()
%% @doc Make a call to a wx_object server.
%% The call waits until it gets a result.
%% Invokes handle_call(Request, From, State) in the server
call(Ref = #wx_ref{state=Pid}, Request) when is_pid(Pid) ->
    try 
	{ok,Res} = gen:call(Pid, '$gen_call', Request, infinity),
	Res
    catch _:Reason ->
	    erlang:error({Reason, {?MODULE, call, [Ref, Request]}})
    end;
call(Name, Request) when is_atom(Name)  orelse is_pid(Name) ->
    try
        {ok,Res} = gen:call(Name, '$gen_call', Request, infinity),
        Res
    catch _:Reason ->
            erlang:error({Reason, {?MODULE, call, [Name, Request]}})
    end.
    
%% @spec (Ref::wxObject()|atom()|pid(), Request::term(), Timeout::integer()) -> term()
%% @doc Make a call to a wx_object server with a timeout.
%% Invokes handle_call(Request, From, State) in server
call(Ref = #wx_ref{state=Pid}, Request, Timeout) when is_pid(Pid) ->
    try
	{ok,Res} = gen:call(Pid, '$gen_call', Request, Timeout),
	Res
    catch _:Reason ->
	    erlang:error({Reason, {?MODULE, call, [Ref, Request, Timeout]}})
    end;
call(Name, Request, Timeout) when is_atom(Name) orelse is_pid(Name) ->
    try
        {ok,Res} = gen:call(Name, '$gen_call', Request, Timeout),
        Res
    catch _:Reason ->
            erlang:error({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

%% @spec (Ref::wxObject()|atom()|pid(), Request::term()) -> ok
%% @doc Make a cast to a wx_object server.
%% Invokes handle_cast(Request, State) in the server

cast(#wx_ref{state=Pid}, Request) when is_pid(Pid) ->
    Pid ! {'$gen_cast',Request},
    ok;
cast(Name, Request) when is_atom(Name) orelse is_pid(Name) ->
    Name ! {'$gen_cast',Request},
    ok.

%% @spec (Ref::wxObject()) -> pid()
%% @doc Get the pid of the object handle.
get_pid(#wx_ref{state=Pid}) when is_pid(Pid) ->
    Pid.

%% @spec (Ref::wxObject(), pid()) -> wxObject()
%% @doc Sets the controlling process of the object handle.
set_pid(#wx_ref{}=R, Pid) when is_pid(Pid) ->
    R#wx_ref{state=Pid}.

%% -----------------------------------------------------------------
%% Send a reply to the client.
%% -----------------------------------------------------------------
%% @spec (From::tuple(), Reply::term()) -> pid()
%% @doc Get the pid of the object handle.
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================
%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
%% @hidden
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name, Mod, Args, [WxEnv|Options]) ->
    case WxEnv of
	undefined -> ok;
	_ -> wx:set_env(WxEnv)
    end,
    put('_wx_object_', {Mod,'_wx_init_'}),
    Debug = debug_options(Name, Options),
    case catch Mod:init(Args) of
	{#wx_ref{} = Ref, State} ->
	    init_it2(Ref, Starter, Parent, Name, State, Mod, infinity, Debug);
	{#wx_ref{} = Ref, State, Timeout} ->
	    init_it2(Ref, Starter, Parent, Name, State, Mod, Timeout, Debug);
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.
%% @hidden
init_it2(Ref, Starter, Parent, Name, State, Mod, Timeout, Debug) ->
    ok = wxe_util:register_pid(Ref),
    case ?CLASS_T(Ref#wx_ref.type, wxWindow) of
	false -> 
	    Reason = {Ref, "not a wxWindow subclass"},
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	true ->
	    proc_lib:init_ack(Starter, {ok, self()}),
	    proc_lib:init_ack(Starter, Ref#wx_ref{state=self()}),
	    loop(Parent, Name, State, Mod, Timeout, Debug)
    end.    

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------
%% @hidden
loop(Parent, Name, State, Mod, Time, Debug) ->
    put('_wx_object_', {Mod,State}),
    Msg = receive
	      Input ->
		  Input
	  after Time ->
		  timeout
	  end,
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time]);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, State, Debug);
	{'_wxe_destroy_', _Me} ->
	    terminate(wx_deleted, Name, Msg, Mod, State, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, Name, State, Mod);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {in, Msg}),
	    handle_msg(Msg, Parent, Name, State, Mod, Debug1)
    end.

%%% ---------------------------------------------------
%%% Message handling functions
%%% ---------------------------------------------------
%% @hidden
dispatch({'$gen_cast', Msg}, Mod, State) ->
    Mod:handle_cast(Msg, State);
dispatch(Msg = #wx{}, Mod, State) ->
    Mod:handle_event(Msg, State);
dispatch(Info, Mod, State) ->
    Mod:handle_info(Info, State).
%% @hidden
handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod) ->
    case catch Mod:handle_call(Msg, From, State) of
	{reply, Reply, NState} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, infinity, []);
	{reply, Reply, NState, Time1} ->
	    reply(From, Reply),
	    loop(Parent, Name, NState, Mod, Time1, []);
	{noreply, NState} ->
	    loop(Parent, Name, NState, Mod, infinity, []);
	{noreply, NState, Time1} ->
	    loop(Parent, Name, NState, Mod, Time1, []);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} =
		(catch terminate(Reason, Name, Msg, Mod, NState, [])),
	    reply(From, Reply),
	    exit(R);
	Other -> handle_common_reply(Other, Name, Msg, Mod, State, [])
    end;
handle_msg(Msg, Parent, Name, State, Mod) ->
    Reply = (catch dispatch(Msg, Mod, State)),
    handle_no_reply(Reply, Parent, Name, Msg, Mod, State, []).

%% @hidden
handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, Debug) ->
    case catch Mod:handle_call(Msg, From, State) of
	{reply, Reply, NState} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{reply, Reply, NState, Time1} ->
	    Debug1 = reply(Name, From, Reply, NState, Debug),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{noreply, NState} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {noreply, NState}),
	    loop(Parent, Name, NState, Mod, infinity, Debug1);
	{noreply, NState, Time1} ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      Name, {noreply, NState}),
	    loop(Parent, Name, NState, Mod, Time1, Debug1);
	{stop, Reason, Reply, NState} ->
	    {'EXIT', R} = 
		(catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
	    _ = reply(Name, From, Reply, NState, Debug),
	    exit(R);
	Other ->
	    handle_common_reply(Other, Name, Msg, Mod, State, Debug)
    end;
handle_msg(Msg, Parent, Name, State, Mod, Debug) ->
    Reply = (catch dispatch(Msg, Mod, State)),
    handle_no_reply(Reply, Parent, Name, Msg, Mod, State, Debug).
%% @hidden
handle_no_reply({noreply, NState}, Parent, Name, _Msg, Mod, _State, []) ->
    loop(Parent, Name, NState, Mod, infinity, []);
handle_no_reply({noreply, NState, Time1}, Parent, Name, _Msg, Mod, _State, []) ->
    loop(Parent, Name, NState, Mod, Time1, []);
handle_no_reply({noreply, NState}, Parent, Name, _Msg, Mod, _State, Debug) ->
    Debug1 = sys:handle_debug(Debug, fun print_event/3,
			      Name, {noreply, NState}),
    loop(Parent, Name, NState, Mod, infinity, Debug1);
handle_no_reply({noreply, NState, Time1}, Parent, Name, _Msg, Mod, _State, Debug) ->
    Debug1 = sys:handle_debug(Debug, fun print_event/3,
			      Name, {noreply, NState}),
    loop(Parent, Name, NState, Mod, Time1, Debug1);
handle_no_reply(Reply, _Parent, Name, Msg, Mod, State, Debug) ->
    handle_common_reply(Reply, Name, Msg, Mod, State,Debug).

%% @hidden
-spec handle_common_reply(_, _, _, _, _, _) -> no_return().
handle_common_reply(Reply, Name, Msg, Mod, State, Debug) ->
    case Reply of
	{stop, Reason, NState} ->
	    terminate(Reason, Name, Msg, Mod, NState, Debug);
	{'EXIT', What} ->
	    terminate(What, Name, Msg, Mod, State, Debug);
	_ ->
	    terminate({bad_return_value, Reply}, Name, Msg, Mod, State, Debug)
    end.

%% @hidden
reply(Name, {To, Tag}, Reply, State, Debug) ->
    reply({To, Tag}, Reply),
    sys:handle_debug(Debug, fun print_event/3,
		     Name, {out, Reply, To, State}).


%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
%% @hidden
system_continue(Parent, Debug, [Name, State, Mod, Time]) ->
    loop(Parent, Name, State, Mod, Time, Debug).

%% @hidden
-spec system_terminate(_, _, _, [_]) -> no_return().
system_terminate(Reason, _Parent, Debug, [Name, State, Mod, _Time]) ->
    terminate(Reason, Name, [], Mod, State, Debug).

%% @hidden
system_code_change([Name, State, Mod, Time], _Module, OldVsn, Extra) ->
    case catch Mod:code_change(OldVsn, State, Extra) of
	{ok, NewState} -> {ok, [Name, NewState, Mod, Time]};
	Else -> Else
    end.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{'$gen_call', {From, _Tag}, Call} ->
	    io:format(Dev, "*DBG* ~p got call ~p from ~w~n",
		      [Name, Call, From]);
	{'$gen_cast', Cast} ->
	    io:format(Dev, "*DBG* ~p got cast ~p~n",
		      [Name, Cast]);
	_ ->
	    io:format(Dev, "*DBG* ~p got ~p~n", [Name, Msg])
    end;
print_event(Dev, {out, Msg, To, State}, Name) ->
    io:format(Dev, "*DBG* ~p sent ~p to ~w, new state ~w~n",
	      [Name, Msg, To, State]);
print_event(Dev, {noreply, State}, Name) ->
    io:format(Dev, "*DBG* ~p new state ~w~n", [Name, State]);
print_event(Dev, Event, Name) ->
    io:format(Dev, "*DBG* ~p dbg  ~p~n", [Name, Event]).

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------
%% @hidden
terminate(Reason, Name, Msg, Mod, State, Debug) ->
    case catch Mod:terminate(Reason, State) of
	{'EXIT', R} ->
	    error_info(R, Name, Msg, State, Debug),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		wx_deleted ->
		    exit(normal);
		_ ->
		    error_info(Reason, Name, Msg, State, Debug),
		    exit(Reason)
	    end
    end.
%% @hidden
error_info(_Reason, application_controller, _Msg, _State, _Debug) ->
    ok;
error_info(Reason, Name, Msg, State, Debug) ->
    Reason1 = 
	case Reason of
	    {undef,[{M,F,A,L}|MFAs]} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A,L}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				Reason;
			    false ->
				{'function not exported',[{M,F,A,L}|MFAs]}
			end
		end;
	    _ ->
		Reason
	end,    
    format("** wx object server ~p terminating \n"
           "** Last message in was ~p~n"
           "** When Server state == ~p~n"
           "** Reason for termination == ~n** ~p~n",
	   [Name, Msg, State, Reason1]),
    sys:print_log(Debug),
    ok.

%%% ---------------------------------------------------
%%% Misc. functions.
%%% ---------------------------------------------------
%% @hidden
opt(Op, [{Op, Value}|_]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options); 
opt(_, []) ->
    false.
%% @hidden
debug_options(Name, Opts) ->
    case opt(debug, Opts) of
	{ok, Options} -> dbg_opts(Name, Options);
	_ -> []
    end.
%% @hidden
dbg_opts(Name, Opts) ->
    case catch sys:debug_options(Opts) of
	{'EXIT',_} ->
	    format("~p: ignoring erroneous debug options - ~p~n",
		   [Name, Opts]),
	    [];
	Dbg ->
	    Dbg
    end.

%% @hidden
%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, Debug, [Name, State, Mod, _Time]] = StatusData,
    StatusHdr = "Status for wx object ",
    Header = if
		 is_pid(Name) ->
		     lists:concat([StatusHdr, pid_to_list(Name)]);
		 is_atom(Name); is_list(Name) ->
		     lists:concat([StatusHdr, Name]);
		 true ->
		     {StatusHdr, Name}
	     end,
    Log = sys:get_debug(log, Debug, []),
    Specfic = 
	case erlang:function_exported(Mod, format_status, 2) of
	    true ->
		case catch Mod:format_status(Opt, [PDict, State]) of
		    {'EXIT', _} -> [{data, [{"State", State}]}];
		    Else -> Else
		end;
	    _ ->
		[{data, [{"State", State}]}]
	end,
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent},
	     {"Logged events", Log}]} |
     Specfic].
