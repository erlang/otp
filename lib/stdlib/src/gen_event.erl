%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
%%
-module(gen_event).

%%% 
%%% A general event handler.
%%% Several handlers (functions) can be added.
%%% Each handler holds a state and will be called
%%% for every event received of the handler.
%%% 

%%% Modified by Magnus.
%%%       Take care of fault situations and made notify asynchronous.
%%% Re-written by Joe with new functional interface !
%%% Modified by Martin - uses proc_lib, sys and gen!


-export([start/0, start/1, start/2,
         start_link/0, start_link/1, start_link/2,
         stop/1, stop/3,
	 notify/2, sync_notify/2,
	 add_handler/3, add_sup_handler/3, delete_handler/3, swap_handler/3,
	 swap_sup_handler/3, which_handlers/1, call/3, call/4, wake_hib/5]).

-export([init_it/6,
	 system_continue/3,
	 system_terminate/4,
	 system_code_change/4,
	 system_get_state/1,
	 system_replace_state/2,
	 format_status/2]).

-export_type([handler/0, handler_args/0, add_handler_ret/0,
              del_handler_ret/0]).

-import(error_logger, [error_msg/2]).

-record(handler, {module             :: atom(),
		  id = false,
		  state,
		  supervised = false :: 'false' | pid()}).

%%%=========================================================================
%%%  API
%%%=========================================================================

%% gen_event:start(Handler) -> {ok, Pid} | {error, What}
%%   gen_event:add_handler(Handler, Mod, Args) -> ok | Other
%%      gen_event:notify(Handler, Event) -> ok
%%      gen_event:call(Handler, Mod, Query) -> {ok, Val} | {error, Why}
%%      gen_event:call(Handler, Mod, Query, Timeout) -> {ok, Val} | {error, Why}
%%   gen_event:delete_handler(Handler, Mod, Args) -> Val
%%   gen_event:swap_handler(Handler, {OldMod, Args1}, {NewMod, Args2}) -> ok
%%   gen_event:which_handler(Handler) -> [Mod]
%% gen_event:stop(Handler) -> ok 

-callback init(InitArgs :: term()) ->
    {ok, State :: term()} |
    {ok, State :: term(), hibernate} |
    {error, Reason :: term()}.
-callback handle_event(Event :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.
-callback handle_call(Request :: term(), State :: term()) ->
    {ok, Reply :: term(), NewState :: term()} |
    {ok, Reply :: term(), NewState :: term(), hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}.
-callback handle_info(Info :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {ok, NewState :: term(), hibernate} |
    {swap_handler, Args1 :: term(), NewState :: term(),
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler.
-callback terminate(Args :: (term() | {stop, Reason :: term()} |
                             stop | remove_handler |
                             {error, {'EXIT', Reason :: term()}} |
                             {error, term()}),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}),
                      State :: term(), Extra :: term()) ->
    {ok, NewState :: term()}.
-callback format_status(Opt, StatusData) -> Status when
      Opt :: 'normal' | 'terminate',
      StatusData :: [PDict | State],
      PDict :: [{Key :: term(), Value :: term()}],
      State :: term(),
      Status :: term().

-optional_callbacks(
    [handle_info/2, terminate/2, code_change/3, format_status/2]).

%%---------------------------------------------------------------------------

-type handler()          :: atom() | {atom(), term()}.
-type handler_args()     :: term().
-type add_handler_ret()  :: ok | term() | {'EXIT',term()}.
-type del_handler_ret()  :: ok | term() | {'EXIT',term()}.

-type emgr_name() :: {'local', atom()} | {'global', atom()}
                   | {'via', atom(), term()}.
-type debug_flag() :: 'trace' | 'log' | 'statistics' | 'debug'
                    | {'logfile', string()}.
-type option() :: {'timeout', timeout()}
                | {'debug', [debug_flag()]}
                | {'spawn_opt', [proc_lib:spawn_option()]}
                | {'hibernate_after', timeout()}.
-type emgr_ref()  :: atom() | {atom(), atom()} |  {'global', atom()}
                   | {'via', atom(), term()} | pid().
-type start_ret() :: {'ok', pid()} | {'error', term()}.

%%---------------------------------------------------------------------------

-define(NO_CALLBACK, 'no callback module').

%% -----------------------------------------------------------------
%% Starts a generic event handler.
%% start()
%% start(MgrName | Options)
%% start(MgrName, Options)
%% start_link()
%% start_link(MgrName | Options)
%% start_link(MgrName, Options)
%%    MgrName ::= {local, atom()} | {global, atom()} | {via, atom(), term()}
%%    Options ::= [{timeout, Timeout} | {debug, [Flag]} | {spawn_opt,SOpts}]
%%       Flag ::= trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%% -----------------------------------------------------------------

-spec start() -> start_ret().
start() ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], []).

-spec start(emgr_name() | [option()]) -> start_ret().
start(Name) when is_tuple(Name) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], []);
start(Options) when is_list(Options) ->
    gen:start(?MODULE, nolink, ?NO_CALLBACK, [], Options).

-spec start(emgr_name(), [option()]) -> start_ret().
start(Name, Options) ->
    gen:start(?MODULE, nolink, Name, ?NO_CALLBACK, [], Options).

-spec start_link() -> start_ret().
start_link() ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], []).

-spec start_link(emgr_name() | [option()]) -> start_ret().
start_link(Name) when is_tuple(Name) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], []);
start_link(Options) when is_list(Options) ->
    gen:start(?MODULE, link, ?NO_CALLBACK, [], Options).

-spec start_link(emgr_name(), [option()]) -> start_ret().
start_link(Name, Options) ->
    gen:start(?MODULE, link, Name, ?NO_CALLBACK, [], Options).

%% -spec init_it(pid(), 'self' | pid(), emgr_name(), module(), [term()], [_]) -> 
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name0, _, _, Options) ->
    process_flag(trap_exit, true),
    Name = gen:name(Name0),
    Debug = gen:debug_options(Name, Options),
	HibernateAfterTimeout = gen:hibernate_after(Options),
    proc_lib:init_ack(Starter, {ok, self()}),
    loop(Parent, Name, [], HibernateAfterTimeout, Debug, false).

-spec add_handler(emgr_ref(), handler(), term()) -> term().
add_handler(M, Handler, Args) -> rpc(M, {add_handler, Handler, Args}).

-spec add_sup_handler(emgr_ref(), handler(), term()) -> term().
add_sup_handler(M, Handler, Args)  ->
    rpc(M, {add_sup_handler, Handler, Args, self()}).

-spec notify(emgr_ref(), term()) -> 'ok'.
notify(M, Event) -> send(M, {notify, Event}).

-spec sync_notify(emgr_ref(), term()) -> 'ok'.
sync_notify(M, Event) -> rpc(M, {sync_notify, Event}).

-spec call(emgr_ref(), handler(), term()) -> term().
call(M, Handler, Query) -> call1(M, Handler, Query).

-spec call(emgr_ref(), handler(), term(), timeout()) -> term().
call(M, Handler, Query, Timeout) -> call1(M, Handler, Query, Timeout).

-spec delete_handler(emgr_ref(), handler(), term()) -> term().
delete_handler(M, Handler, Args) -> rpc(M, {delete_handler, Handler, Args}).

-spec swap_handler(emgr_ref(), {handler(), term()}, {handler(), term()}) ->
	    'ok' | {'error', term()}.
swap_handler(M, {H1, A1}, {H2, A2}) -> rpc(M, {swap_handler, H1, A1, H2, A2}).

-spec swap_sup_handler(emgr_ref(), {handler(), term()}, {handler(), term()}) ->
	    'ok' | {'error', term()}.
swap_sup_handler(M, {H1, A1}, {H2, A2}) ->
    rpc(M, {swap_sup_handler, H1, A1, H2, A2, self()}).

-spec which_handlers(emgr_ref()) -> [handler()].
which_handlers(M) -> rpc(M, which_handlers).

-spec stop(emgr_ref()) -> 'ok'.
stop(M) ->
    gen:stop(M).

stop(M, Reason, Timeout) ->
    gen:stop(M, Reason, Timeout).

rpc(M, Cmd) ->
    {ok, Reply} = gen:call(M, self(), Cmd, infinity),
    Reply.

call1(M, Handler, Query) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd) of
	{ok, Res} ->
	    Res
    catch
	exit:Reason ->
	    exit({Reason, {?MODULE, call, [M, Handler, Query]}})
    end.

call1(M, Handler, Query, Timeout) ->
    Cmd = {call, Handler, Query},
    try gen:call(M, self(), Cmd, Timeout) of
	{ok, Res} ->
	    Res
    catch
	exit:Reason ->
	    exit({Reason, {?MODULE, call, [M, Handler, Query, Timeout]}})
    end.

send({global, Name}, Cmd) ->
    catch global:send(Name, Cmd),
    ok;
send({via, Mod, Name}, Cmd) ->
    catch Mod:send(Name, Cmd),
    ok;
send(M, Cmd) ->
    M ! Cmd,
    ok.

loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true) ->
     proc_lib:hibernate(?MODULE, wake_hib, [Parent, ServerName, MSL, HibernateAfterTimeout, Debug]);
loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, _) ->
    fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false).

wake_hib(Parent, ServerName, MSL, HibernateAfterTimeout, Debug) ->
    fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true).

fetch_msg(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib) ->
    receive
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [ServerName, MSL, HibernateAfterTimeout, Hib],Hib);
	{'EXIT', Parent, Reason} ->
	    terminate_server(Reason, Parent, MSL, ServerName);
	Msg when Debug =:= [] ->
	    handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, []);
	Msg ->
	    Debug1 = sys:handle_debug(Debug, fun print_event/3,
				      ServerName, {in, Msg}),
	    handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug1)
    after HibernateAfterTimeout ->
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, true)
    end.

handle_msg(Msg, Parent, ServerName, MSL, HibernateAfterTimeout, Debug) ->
    case Msg of
	{notify, Event} ->
	    {Hib,MSL1} = server_notify(Event, handle_event, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {sync_notify, Event}} ->
	    {Hib, MSL1} = server_notify(Event, handle_event, MSL, ServerName),
	    reply(Tag, ok),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{'EXIT', From, Reason} ->
	    MSL1 = handle_exit(From, Reason, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, false);
	{_From, Tag, {call, Handler, Query}} ->
	    {Hib, Reply, MSL1} = server_call(Handler, Query, MSL, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {add_handler, Handler, Args}} ->
	    {Hib, Reply, MSL1} = server_add_handler(Handler, Args, MSL),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {add_sup_handler, Handler, Args, SupP}} ->
	    {Hib, Reply, MSL1} = server_add_sup_handler(Handler, Args, MSL, SupP),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {delete_handler, Handler, Args}} ->
	    {Reply, MSL1} = server_delete_handler(Handler, Args, MSL,
						  ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, false);
	{_From, Tag, {swap_handler, Handler1, Args1, Handler2, Args2}} ->
	    {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						     Args2, MSL, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, {swap_sup_handler, Handler1, Args1, Handler2, Args2,
		     Sup}} ->
	    {Hib, Reply, MSL1} = server_swap_handler(Handler1, Args1, Handler2,
						Args2, MSL, Sup, ServerName),
	    reply(Tag, Reply),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib);
	{_From, Tag, stop} ->
	    catch terminate_server(normal, Parent, MSL, ServerName),
	    reply(Tag, ok);
	{_From, Tag, which_handlers} ->
	    reply(Tag, the_handlers(MSL)),
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false);
	{_From, Tag, get_modules} ->
	    reply(Tag, get_modules(MSL)),
	    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, false);
	Other  ->
	    {Hib, MSL1} = server_notify(Other, handle_info, MSL, ServerName),
	    loop(Parent, ServerName, MSL1, HibernateAfterTimeout, Debug, Hib)
    end.

terminate_server(Reason, Parent, MSL, ServerName) ->
    stop_handlers(MSL, ServerName),
    do_unlink(Parent, MSL),
    exit(Reason).

reply({From, Ref}, Msg) ->
    From ! {Ref, Msg},
    ok.

%% unlink the supervisor process of all supervised handlers.
%% We do not want a handler supervisor to EXIT due to the
%% termination of the event manager (server).
%% Do not unlink Parent !
do_unlink(Parent, MSL) ->
    lists:foreach(fun(Handler) when Handler#handler.supervised =:= Parent ->
			  true;
		     (Handler) when is_pid(Handler#handler.supervised) ->
			  unlink(Handler#handler.supervised),
			  true;
		     (_) ->
			  true
		  end,
		  MSL).

%% First terminate the supervised (if exists) handlers and
%% then inform other handlers.
%% We do not know if any handler really is interested but it
%% may be so !
handle_exit(From, Reason, MSL, SName) ->
    MSL1 = terminate_supervised(From, Reason, MSL, SName),
    {_,MSL2}=server_notify({'EXIT', From, Reason}, handle_info, MSL1, SName),
    MSL2.

terminate_supervised(Pid, Reason, MSL, SName) ->
    F = fun(Ha) when Ha#handler.supervised =:= Pid ->
		do_terminate(Ha#handler.module,
			     Ha,
			     {stop,Reason},
			     Ha#handler.state,
			     {parent_terminated, {Pid,Reason}},
			     SName,
			     shutdown),
		false;
	   (_) ->
		true
	end,
    lists:filter(F, MSL).

%%-----------------------------------------------------------------
%% Callback functions for system messages handling.
%%-----------------------------------------------------------------
system_continue(Parent, Debug, [ServerName, MSL, HibernateAfterTimeout, Hib]) ->
    loop(Parent, ServerName, MSL, HibernateAfterTimeout, Debug, Hib).

-spec system_terminate(_, _, _, [_]) -> no_return().
system_terminate(Reason, Parent, _Debug, [ServerName, MSL, _HibernateAfterTimeout, _Hib]) ->
    terminate_server(Reason, Parent, MSL, ServerName).

%%-----------------------------------------------------------------
%% Module here is sent in the system msg change_code.  It specifies
%% which module should be changed.
%%-----------------------------------------------------------------
system_code_change([ServerName, MSL, HibernateAfterTimeout, Hib], Module, OldVsn, Extra) ->
    MSL1 = lists:zf(fun(H) when H#handler.module =:= Module ->
			    {ok, NewState} =
				Module:code_change(OldVsn,
						   H#handler.state, Extra),
			    {true, H#handler{state = NewState}};
		       (_) -> true
		    end,
		    MSL),
    {ok, [ServerName, MSL1, HibernateAfterTimeout, Hib]}.

system_get_state([_ServerName, MSL, _HibernateAfterTimeout, _Hib]) ->
    {ok, [{Mod,Id,State} || #handler{module=Mod, id=Id, state=State} <- MSL]}.

system_replace_state(StateFun, [ServerName, MSL, HibernateAfterTimeout, Hib]) ->
    {NMSL, NStates} =
		lists:unzip([begin
				 Cur = {Mod,Id,State},
				 try
				     NState = {Mod,Id,NS} = StateFun(Cur),
				     {HS#handler{state=NS}, NState}
				 catch
				     _:_ ->
					 {HS, Cur}
				 end
			     end || #handler{module=Mod, id=Id, state=State}=HS <- MSL]),
    {ok, NStates, [ServerName, NMSL, HibernateAfterTimeout, Hib]}.

%%-----------------------------------------------------------------
%% Format debug messages.  Print them as the call-back module sees
%% them, not as the real erlang messages.  Use trace for that.
%%-----------------------------------------------------------------
print_event(Dev, {in, Msg}, Name) ->
    case Msg of
	{notify, Event} ->
	    io:format(Dev, "*DBG* ~tp got event ~tp~n", [Name, Event]);
	{_,_,{call, Handler, Query}} ->
	    io:format(Dev, "*DBG* ~tp(~tp) got call ~tp~n",
		      [Name, Handler, Query]);
	_ ->
	    io:format(Dev, "*DBG* ~tp got ~tp~n", [Name, Msg])
    end;
print_event(Dev, Dbg, Name) ->
    io:format(Dev, "*DBG* ~tp : ~tp~n", [Name, Dbg]).


%% server_add_handler(Handler, Args, MSL) -> {Ret, MSL'}.
%%   where MSL = [#handler{}]
%%   Ret goes to the top level MSL' is the new internal state of the
%%   event handler

server_add_handler({Mod,Id}, Args, MSL) ->
    Handler = #handler{module = Mod,
		       id = Id},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_handler(Mod, Args, MSL) ->
    Handler = #handler{module = Mod},
    server_add_handler(Mod, Handler, Args, MSL).

server_add_handler(Mod, Handler, Args, MSL) ->
    case catch Mod:init(Args) of
        {ok, State} ->
	    {false, ok, [Handler#handler{state = State}|MSL]};
        {ok, State, hibernate} ->
	    {true, ok, [Handler#handler{state = State}|MSL]};
        Other ->
            {false, Other, MSL}
    end.

%% Set up a link to the supervising process.
%% (Ought to be unidirected links here, Erl5.0 !!)
%% NOTE: This link will not be removed then the
%% handler is removed in case another handler has
%% own link to this process.
server_add_sup_handler({Mod,Id}, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       id = Id,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL);
server_add_sup_handler(Mod, Args, MSL, Parent) ->
    link(Parent),
    Handler = #handler{module = Mod,
		       supervised = Parent},
    server_add_handler(Mod, Handler, Args, MSL).

%% server_delete_handler(HandlerId, Args, MSL) -> {Ret, MSL'}

server_delete_handler(HandlerId, Args, MSL, SName) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, delete, SName, normal),
	     MSL1};
	error ->
	    {{error, module_not_found}, MSL}
    end.

%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SN) -> MSL'
%% server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SN) -> MSL'

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, SName) ->
    {State2, Sup, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					      SName, Handler2, false),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{Hib, ok, MSL2} ->
	    {Hib, ok, MSL2};
	{Hib, What, MSL2} ->
	    {Hib, {error, What}, MSL2}
    end.

server_swap_handler(Handler1, Args1, Handler2, Args2, MSL, Sup, SName) ->
    {State2, _, MSL1} = split_and_terminate(Handler1, Args1, MSL,
					    SName, Handler2, Sup),
    case s_s_h(Sup, Handler2, {Args2, State2}, MSL1) of
	{Hib, ok, MSL2} ->
	    {Hib, ok, MSL2};
	{Hib, What, MSL2} ->
	    {Hib, {error, What}, MSL2}
    end.

s_s_h(false, Handler, Args, MSL) ->
    server_add_handler(Handler, Args, MSL);
s_s_h(Pid, Handler, Args, MSL) ->
    server_add_sup_handler(Handler, Args, MSL, Pid).

split_and_terminate(HandlerId, Args, MSL, SName, Handler2, Sup) ->
    case split(HandlerId, MSL) of
	{Mod, Handler, MSL1} ->
	    OldSup = Handler#handler.supervised,
	    NewSup = if
			 not Sup -> OldSup;
			 true    -> Sup
		     end,
	    {do_terminate(Mod, Handler, Args,
			  Handler#handler.state, swapped, SName,
			  {swapped, Handler2, NewSup}),
	     OldSup,
	     MSL1};
	error ->
            {error, false, MSL}
    end.

%% server_notify(Event, Func, MSL, SName) -> MSL'

server_notify(Event, Func, [Handler|T], SName) ->
    case server_update(Handler, Func, Event, SName) of
	{ok, Handler1} ->
	    {Hib, NewHandlers} = server_notify(Event, Func, T, SName),
	    {Hib, [Handler1|NewHandlers]};
	{hibernate, Handler1} ->
	    {_Hib, NewHandlers} = server_notify(Event, Func, T, SName),
	    {true, [Handler1|NewHandlers]};
	no ->
	    server_notify(Event, Func, T, SName)
    end;
server_notify(_, _, [], _) ->
    {false, []}.

%% server_update(Handler, Func, Event, ServerName) -> Handler1 | no

server_update(Handler1, Func, Event, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:Func(Event, State) of
	{ok, State1} ->
	    {ok, Handler1#handler{state = State1}};
	{ok, State1, hibernate} ->
	    {hibernate, Handler1#handler{state = State1}};
	{swap_handler, Args1, State1, Handler2, Args2} ->
	    do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName);
	remove_handler ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    no;
        {'EXIT', {undef, [{Mod1, handle_info, [_,_], _}|_]}} ->
            error_logger:warning_msg("** Undefined handle_info in ~tp~n"
                                     "** Unhandled message: ~tp~n", [Mod1, Event]),
           {ok, Handler1};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Event, SName, crash),
	    no
    end.

do_swap(Mod1, Handler1, Args1, State1, Handler2, Args2, SName) ->
    %% finalise the existing handler
    State2 = do_terminate(Mod1, Handler1, Args1, State1,
			  swapped, SName,
			  {swapped, Handler2, Handler1#handler.supervised}),
    {Mod2, Handler} = new_handler(Handler2, Handler1),
    case catch Mod2:init({Args2, State2}) of
	{ok, State2a} ->
	    {ok, Handler#handler{state = State2a}};
	Other ->
	    report_terminate(Handler, crash, {error, Other}, SName, false),
	    no
    end.

new_handler({Mod,Id}, Handler1) ->
    {Mod, #handler{module = Mod,
		   id = Id,
		   supervised = Handler1#handler.supervised}};
new_handler(Mod, Handler1) ->
    {Mod, #handler{module = Mod,
		   supervised = Handler1#handler.supervised}}.


-spec split(handler(), [#handler{}]) ->
	{atom(), #handler{}, [#handler{}]} | 'error'.

split(Ha, MSL) -> split(Ha, MSL, []).

split({Mod,Id}, [Ha|T], L) when Ha#handler.module =:= Mod,
                                Ha#handler.id =:= Id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Mod, [Ha|T], L) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    {Mod, Ha, lists:reverse(L, T)};
split(Ha, [H|T], L) ->
    split(Ha, T, [H|L]);
split(_, [], _) ->
    error.

%% server_call(Handler, Query, MSL, ServerName) ->
%%    {Reply, MSL1}

server_call(Handler, Query, MSL, SName) ->
    case search(Handler, MSL) of
	{ok, Ha} ->
	    case server_call_update(Ha, Query, SName) of
		{no, Reply} ->
		    {false, Reply, delete(Handler, MSL)};
		{{ok, Ha1}, Reply} ->
		    {false, Reply, replace(Handler, MSL, Ha1)};
		{{hibernate, Ha1}, Reply} ->
		    {true, Reply, replace(Handler, MSL, Ha1)}
	    end;
	false ->
	    {false, {error, bad_module}, MSL}
    end.

search({Mod, Id}, [Ha|_MSL]) when Ha#handler.module =:= Mod,
				  Ha#handler.id =:= Id ->
    {ok, Ha};
search(Mod, [Ha|_MSL]) when Ha#handler.module =:= Mod,
			    not Ha#handler.id ->
    {ok, Ha};
search(Handler, [_|MSL]) ->
    search(Handler, MSL);
search(_, []) ->
    false.

delete({Mod, Id}, [Ha|MSL]) when Ha#handler.module =:= Mod,
                                 Ha#handler.id =:= Id ->
    MSL;
delete(Mod, [Ha|MSL]) when Ha#handler.module =:= Mod,
                           not Ha#handler.id ->
    MSL;
delete(Handler, [Ha|MSL]) ->
    [Ha|delete(Handler, MSL)];
delete(_, []) ->
    [].

replace({Mod, Id}, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                         Ha#handler.id =:= Id ->
    [NewHa|MSL];
replace(Mod, [Ha|MSL], NewHa) when Ha#handler.module =:= Mod,
                                   not Ha#handler.id ->
    [NewHa|MSL];
replace(Handler, [Ha|MSL], NewHa) ->
    [Ha|replace(Handler, MSL, NewHa)];
replace(_, [], NewHa) ->
    [NewHa].

%% server_call_update(Handler, Query, ServerName) ->
%%    {{Handler1, State1} | 'no', Reply}

server_call_update(Handler1, Query, SName) ->
    Mod1 = Handler1#handler.module,
    State = Handler1#handler.state,
    case catch Mod1:handle_call(Query, State) of
	{ok, Reply, State1} ->
	    {{ok, Handler1#handler{state = State1}}, Reply};
	{ok, Reply, State1, hibernate} ->
	    {{hibernate, Handler1#handler{state = State1}},
	     Reply};
	{swap_handler, Reply, Args1, State1, Handler2, Args2} ->
	    {do_swap(Mod1,Handler1,Args1,State1,Handler2,Args2,SName), Reply};
	{remove_handler, Reply} ->
	    do_terminate(Mod1, Handler1, remove_handler, State,
			 remove, SName, normal),
	    {no, Reply};
	Other ->
	    do_terminate(Mod1, Handler1, {error, Other}, State,
			 Query, SName, crash),
	    {no, {error, Other}}
    end.

do_terminate(Mod, Handler, Args, State, LastIn, SName, Reason) ->
    case erlang:function_exported(Mod, terminate, 2) of
	true ->
	    Res = (catch Mod:terminate(Args, State)),
	    report_terminate(Handler, Reason, Args, State, LastIn, SName, Res),
	    Res;
	false ->
	    report_terminate(Handler, Reason, Args, State, LastIn, SName, ok),
	    ok
    end.

report_terminate(Handler, crash, {error, Why}, State, LastIn, SName, _) ->
    report_terminate(Handler, Why, State, LastIn, SName);
report_terminate(Handler, How, _, State, LastIn, SName, _) ->
    %% How == normal | shutdown | {swapped, NewHandler, NewSupervisor}
    report_terminate(Handler, How, State, LastIn, SName).

report_terminate(Handler, Reason, State, LastIn, SName) ->
    report_error(Handler, Reason, State, LastIn, SName),
    case Handler#handler.supervised of
	false ->
	    ok;
	Pid ->
	    Pid ! {gen_event_EXIT,handler(Handler),Reason},
	    ok
    end.

report_error(_Handler, normal, _, _, _)             -> ok;
report_error(_Handler, shutdown, _, _, _)           -> ok;
report_error(_Handler, {swapped,_,_}, _, _, _)      -> ok;
report_error(Handler, Reason, State, LastIn, SName) ->
    Reason1 =
	case Reason of
	    {'EXIT',{undef,[{M,F,A,L}|MFAs]}} ->
		case code:is_loaded(M) of
		    false ->
			{'module could not be loaded',[{M,F,A,L}|MFAs]};
		    _ ->
			case erlang:function_exported(M, F, length(A)) of
			    true ->
				{undef,[{M,F,A,L}|MFAs]};
			    false ->
				{'function not exported',[{M,F,A,L}|MFAs]}
			end
		end;
	    {'EXIT',Why} ->
		Why;
	    _ ->
		Reason
	end,
    Mod = Handler#handler.module,
    FmtState = case erlang:function_exported(Mod, format_status, 2) of
		   true ->
		       Args = [get(), State],
		       case catch Mod:format_status(terminate, Args) of
			   {'EXIT', _} -> State;
			   Else -> Else
		       end;
		   _ ->
		       State
	       end,
    error_msg("** gen_event handler ~p crashed.~n"
	      "** Was installed in ~tp~n"
	      "** Last event was: ~tp~n"
	      "** When handler state == ~tp~n"
	      "** Reason == ~tp~n",
	      [handler(Handler),SName,LastIn,FmtState,Reason1]).

handler(Handler) when not Handler#handler.id ->
    Handler#handler.module;
handler(Handler) ->
    {Handler#handler.module, Handler#handler.id}.

the_handlers(MSL) ->
    [handler(Handler) || Handler <- MSL].

%% stop_handlers(MSL, ServerName) -> []

stop_handlers([Handler|T], SName) ->
    Mod = Handler#handler.module,
    do_terminate(Mod, Handler, stop, Handler#handler.state,
		 stop, SName, shutdown),
    stop_handlers(T, SName);
stop_handlers([], _) ->
    [].

%% Message from the release_handler.
%% The list of modules got to be a set, i.e. no duplicate elements!
get_modules(MSL) ->
    Mods = [Handler#handler.module || Handler <- MSL],
    ordsets:to_list(ordsets:from_list(Mods)).

%%-----------------------------------------------------------------
%% Status information
%%-----------------------------------------------------------------
format_status(Opt, StatusData) ->
    [PDict, SysState, Parent, _Debug, [ServerName, MSL, _HibernateAfterTimeout, _Hib]] = StatusData,
    Header = gen:format_status_header("Status for event handler",
                                      ServerName),
    FmtMSL = [case erlang:function_exported(Mod, format_status, 2) of
		  true ->
		      Args = [PDict, State],
		      case catch Mod:format_status(Opt, Args) of
			  {'EXIT', _} -> MSL;
			  Else -> MS#handler{state = Else}
		      end;
		  _ ->
		      MS
	      end || #handler{module = Mod, state = State} = MS <- MSL],
    [{header, Header},
     {data, [{"Status", SysState},
	     {"Parent", Parent}]},
     {items, {"Installed handlers", FmtMSL}}].
