%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
-module(gen).
-moduledoc false.
-compile({inline,[get_node/1]}).

%%%-----------------------------------------------------------------
%%% This module implements the really generic stuff of the generic
%%% standard behaviours (e.g. gen_server, gen_fsm).
%%%
%%% The standard behaviour should export init_it/6.
%%%-----------------------------------------------------------------
-export([start/5, start/6, debug_options/2, hibernate_after/1,
	 name/1, unregister_name/1, get_proc_name/1, get_parent/0,
	 call/3, call/4, reply/2,
         send_request/3, send_request/5,
         wait_response/2, receive_response/2, check_response/2,
         wait_response/3, receive_response/3, check_response/3,
         reqids_new/0, reqids_size/1,
         reqids_add/3, reqids_to_list/1,
         stop/1, stop/3]).

-export([init_it/6, init_it/7]).

-export([format_status_header/2, format_status/4]).

-export(['@wait_response_recv_opt'/3]).

-define(MAX_INT_TIMEOUT, 4294967295).
-define(default_timeout, 5000).

-include("logger.hrl").

%%-----------------------------------------------------------------

-export_type([reply_tag/0,
              request_id/0,
              request_id_collection/0]).

-type linkage()    :: 'monitor' | 'link' | 'nolink'.
-type emgr_name()  :: {'local', atom()}
                    | {'global', term()}
                    | {'via', Module :: module(), Name :: term()}.

-type start_ret()  :: {'ok', pid()}
                    | {'ok', {pid(), reference()}}
                    | 'ignore'
                    | {'error', term()}.

-type option()     :: {'timeout', timeout()}
		    | {'debug', [sys:debug_option()]}
		    | {'hibernate_after', timeout()}
		    | {'spawn_opt', [proc_lib:spawn_option()]}.

-type server_ref() :: pid() | atom() | {atom(), node()}
                    | {global, term()} | {via, module(), term()}.

-opaque reply_tag() :: % As accepted by reply/2
          reference()
        | nonempty_improper_list('alias', reference())
        | nonempty_improper_list(
            nonempty_improper_list('alias', reference()), term()).

-opaque request_id() :: reference().

-opaque request_id_collection() :: map().

-type response_timeout() ::
        0..?MAX_INT_TIMEOUT | 'infinity' | {abs, integer()}.

%%-----------------------------------------------------------------
%% Starts a generic process.
%% start(GenMod, LinkP, Mod, Args, Options)
%% start(GenMod, LinkP, Name, Mod, Args, Options)
%%    GenMod = atom(), callback module implementing the 'real' fsm
%%    LinkP = link | nolink
%%    Name = {local, atom()} | {global, term()} | {via, atom(), term()}
%%    Args = term(), init arguments (to Mod:init/1)
%%    Options = [{timeout, Timeout} | {debug, [Flag]} | {spawn_opt, OptionList}]
%%      Flag = trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} | {ok, Pid, Reference} | ignore |{error, Reason} |
%%          {error, {already_started, Pid}} |
%%    The 'already_started' is returned only if Name is given 
%%-----------------------------------------------------------------

-spec start(module(), linkage(), emgr_name(), module(), term(), [option()]) ->
	start_ret().

start(GenMod, LinkP, Name, Mod, Args, Options) ->
    case where(Name) of
	undefined ->
	    do_spawn(GenMod, LinkP, Name, Mod, Args, Options);
	Pid ->
	    {error, {already_started, Pid}}
    end.

-spec start(module(), linkage(), module(), term(), [option()]) -> start_ret().

start(GenMod, LinkP, Mod, Args, Options) ->
    do_spawn(GenMod, LinkP, Mod, Args, Options).

%%-----------------------------------------------------------------
%% Spawn the process (and link) maybe at another node.
%% If spawn without link, set parent to ourselves 'self'!!!
%%-----------------------------------------------------------------
do_spawn(GenMod, link, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(?MODULE, init_it,
			[GenMod, self(), self(), Mod, Args, Options], 
			Time,
			spawn_opts(Options));
do_spawn(GenMod, monitor, Mod, Args, Options) ->
    Time = timeout(Options),
    Ret = proc_lib:start_monitor(?MODULE, init_it,
                                 [GenMod, self(), self(), Mod, Args, Options], 
                                 Time,
                                 spawn_opts(Options)),
    monitor_return(Ret);
do_spawn(GenMod, _, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(?MODULE, init_it,
		   [GenMod, self(), 'self', Mod, Args, Options],
		   Time,
		   spawn_opts(Options)).

do_spawn(GenMod, link, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(?MODULE, init_it,
			[GenMod, self(), self(), Name, Mod, Args, Options],
			Time,
			spawn_opts(Options));
do_spawn(GenMod, monitor, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    Ret = proc_lib:start_monitor(?MODULE, init_it,
                                 [GenMod, self(), self(), Name, Mod, Args, Options],
                                 Time,
                                 spawn_opts(Options)),
    monitor_return(Ret);
do_spawn(GenMod, _, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(?MODULE, init_it,
		   [GenMod, self(), 'self', Name, Mod, Args, Options],
		   Time,
		   spawn_opts(Options)).


%%
%% Adjust monitor returns for OTP gen behaviours...
%%
%% If an OTP behaviour is introduced that 'init_ack's
%% other results, this has code has to be moved out
%% into all behaviours as well as adjusted...
%%
monitor_return({{ok, Pid}, Mon}) when is_pid(Pid), is_reference(Mon) ->
    %% Successful start_monitor()...
    {ok, {Pid, Mon}};
monitor_return({Error, Mon}) when is_reference(Mon) ->
    %% Failure; wait for spawned process to terminate
    %% and release resources, then return the error...
    receive
        {'DOWN', Mon, process, _Pid, _Reason} ->
            ok
    end,
    Error.

%%-----------------------------------------------------------------
%% Initiate the new process.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%%-----------------------------------------------------------------
init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
    init_it2(GenMod, Starter, Parent, self(), Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    case register_name(Name) of
	true ->
	    init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options);
	{false, Pid} ->
	    proc_lib:init_fail(
              Starter, {error, {already_started, Pid}}, {exit, normal})
    end.

init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    GenMod:init_it(Starter, Parent, Name, Mod, Args, Options).

%%-----------------------------------------------------------------
%% Makes a synchronous call to a generic process.
%% Request is sent to the Pid, and the response must be
%% {Tag, Reply}.
%%-----------------------------------------------------------------

%%% New call function which uses the new monitor BIF
%%% call(ServerId, Label, Request)

call(Process, Label, Request) -> 
    call(Process, Label, Request, ?default_timeout).

%% Optimize a common case.
call(Process, Label, Request, Timeout) when is_pid(Process),
  Timeout =:= infinity orelse is_integer(Timeout) andalso Timeout >= 0 ->
    do_call(Process, Label, Request, Timeout);
call(Process, Label, Request, Timeout)
  when Timeout =:= infinity; is_integer(Timeout), Timeout >= 0 ->
    Fun = fun(Pid) -> do_call(Pid, Label, Request, Timeout) end,
    do_for_proc(Process, Fun).

-dialyzer({no_improper_lists, do_call/4}).

do_call(Process, _Label, _Request, _Timeout) when Process =:= self() ->
    exit(calling_self);
do_call(Process, Label, Request, infinity)
  when (is_pid(Process)
        andalso (node(Process) == node()))
       orelse (element(2, Process) == node()
               andalso is_atom(element(1, Process))
               andalso (tuple_size(Process) =:= 2)) ->
    Mref = erlang:monitor(process, Process),
    %% Local without timeout; no need to use alias since we unconditionally
    %% will wait for either a reply or a down message which corresponds to
    %% the process being terminated (as opposed to 'noconnection')...
    Process ! {Label, {self(), Mref}, Request},
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, Reason} ->
            exit(Reason)
    end;
do_call(Process, Label, Request, Timeout) when is_atom(Process) =:= false ->
    Mref = erlang:monitor(process, Process, [{alias,demonitor}]),

    Tag = [alias | Mref],

    %% OTP-24:
    %% Using alias to prevent responses after 'noconnection' and timeouts.
    %% We however still may call nodes responding via process identifier, so
    %% we still use 'noconnect' on send in order to try to send on the
    %% monitored connection, and not trigger a new auto-connect.
    %%
    erlang:send(Process, {Label, {self(), Tag}, Request}, [noconnect]),

    receive
        {[alias | Mref], Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, noconnection} ->
            Node = get_node(Process),
            exit({nodedown, Node});
        {'DOWN', Mref, _, _, Reason} ->
            exit(Reason)
    after Timeout ->
            erlang:demonitor(Mref, [flush]),
            receive
                {[alias | Mref], Reply} ->
                    {ok, Reply}
            after 0 ->
                    exit(timeout)
            end
    end.

get_node(Process) ->
    %% We trust the arguments to be correct, i.e
    %% Process is either a local or remote pid,
    %% or a {Name, Node} tuple (of atoms) and in this
    %% case this node (node()) _is_ distributed and Node =/= node().
    case Process of
	{_S, N} when is_atom(N) ->
	    N;
	_ when is_pid(Process) ->
	    node(Process)
    end.

-spec send_request(Name::server_ref(), Tag::term(), Request::term()) ->
          request_id().
send_request(Process, Tag, Request) when is_pid(Process) ->
    do_send_request(Process, Tag, Request);
send_request(Process, Tag, Request) ->
    Fun = fun(Pid) -> do_send_request(Pid, Tag, Request) end,
    try do_for_proc(Process, Fun)
    catch exit:Reason ->
            %% Make send_request async and fake a down message
            Mref = erlang:make_ref(),
            self() ! {'DOWN', Mref, process, Process, Reason},
            Mref
    end.

-spec send_request(Name::server_ref(), Tag::term(), Request::term(),
                   Label::term(), ReqIdCol::request_id_collection()) ->
          request_id_collection().
send_request(Process, Tag, Request, Label, ReqIdCol) when is_map(ReqIdCol) ->
    maps:put(send_request(Process, Tag, Request), Label, ReqIdCol).

-dialyzer({no_improper_lists, do_send_request/3}).

do_send_request(Process, Tag, Request) ->
    ReqId = erlang:monitor(process, Process, [{alias, demonitor}]),
    _ = erlang:send(Process, {Tag, {self(), [alias|ReqId]}, Request}, [noconnect]),
    ReqId.

-spec '@wait_response_recv_opt'(term(), term(), term()) -> ok.
'@wait_response_recv_opt'(Process, Tag, Request) ->
    %% Enables reference optimization in wait_response/2 and
    %% receive_response/2
    %%
    %% This never actually runs and is only used to trigger the optimization,
    %% see the module comment in beam_ssa_recv for details.
    _ = wait_response(send_request(Process, Tag, Request), infinity),
    _ = receive_response(send_request(Process, Tag, Request), infinity),
    ok.

%%
%% Wait for a reply to the client.
%% Note: if timeout is returned monitors are kept.

-spec wait_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Resp :: {reply, Reply::term()} | {error, {Reason::term(), server_ref()}},
      Result :: Resp | 'timeout'.

wait_response(ReqId, Timeout) ->
    TMO = timeout_value(Timeout),
    receive
        {[alias|ReqId], Reply} ->
            erlang:demonitor(ReqId, [flush]),
            {reply, Reply};
        {'DOWN', ReqId, _, Object, Reason} ->
            {error, {Reason, Object}}
    after TMO ->
            timeout
    end.

-spec wait_response(ReqIdCol, Timeout, Delete) -> Result when
      ReqIdCol :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Resp :: {reply, Reply::term()} | {error, {Reason::term(), server_ref()}},
      Result :: {Resp, Label::term(), NewReqIdCol::request_id_collection()} |
                'no_request' | 'timeout'.

wait_response(ReqIdCol, Timeout, Delete) when map_size(ReqIdCol) == 0,
                                              is_boolean(Delete) ->
    _ = timeout_value(Timeout),
    no_request;
wait_response(ReqIdCol, Timeout, Delete) when is_map(ReqIdCol),
                                              is_boolean(Delete) ->
    TMO = timeout_value(Timeout),
    receive
        {[alias|ReqId], _} = Msg when is_map_key(ReqId, ReqIdCol) ->
            collection_result(Msg, ReqIdCol, Delete);
        {'DOWN', ReqId, _, _, _} = Msg when is_map_key(ReqId, ReqIdCol) ->
            collection_result(Msg, ReqIdCol, Delete)
    after TMO ->
            timeout
    end.

-spec receive_response(ReqId, Timeout) -> Result when
      ReqId :: request_id(),
      Timeout :: response_timeout(),
      Resp :: {reply, Reply::term()} | {error, {Reason::term(), server_ref()}},
      Result :: Resp | 'timeout'.

receive_response(ReqId, Timeout) ->
    TMO = timeout_value(Timeout),
    receive
        {[alias|ReqId], Reply} ->
            erlang:demonitor(ReqId, [flush]),
            {reply, Reply};
        {'DOWN', ReqId, _, Object, Reason} ->
            {error, {Reason, Object}}
    after TMO ->
            erlang:demonitor(ReqId, [flush]),
            receive
                {[alias|ReqId], Reply} ->
                    {reply, Reply}
            after 0 ->
                    timeout
            end
    end.

-spec receive_response(ReqIdCol, Timeout, Delete) -> Result when
      ReqIdCol :: request_id_collection(),
      Timeout :: response_timeout(),
      Delete :: boolean(),
      Resp :: {reply, Reply::term()} | {error, {Reason::term(), server_ref()}},
      Result :: {Resp, Label::term(), NewReqIdCol::request_id_collection()}
              | 'no_request' | 'timeout'.

receive_response(ReqIdCol, Timeout, Delete) when map_size(ReqIdCol) == 0,
                                                 is_boolean(Delete) ->
    _ = timeout_value(Timeout),
    no_request;
receive_response(ReqIdCol, Timeout, Delete) when is_map(ReqIdCol),
                                                 is_boolean(Delete) ->
    TMO = timeout_value(Timeout),
    receive
        {[alias|ReqId], _} = Msg when is_map_key(ReqId, ReqIdCol) ->
            collection_result(Msg, ReqIdCol, Delete);
        {'DOWN', Mref, _, _, _} = Msg when is_map_key(Mref, ReqIdCol) ->
            collection_result(Msg, ReqIdCol, Delete)
    after TMO ->
            maps:foreach(fun (ReqId, _Label) when is_reference(ReqId) ->
                                 erlang:demonitor(ReqId, [flush]);
                             (_, _) ->
                                 error(badarg)
                         end, ReqIdCol),
            flush_responses(ReqIdCol),
            timeout
    end.

-spec check_response(Msg::term(), ReqIdOrReqIdCol) -> Result when
      ReqIdOrReqIdCol :: request_id() | request_id_collection(),
      ReqIdResp :: {reply, Reply::term()} |
                   {error, {Reason::term(), server_ref()}},
      ReqIdColResp :: {{reply, Reply::term()}, Label::term()} |
                             {{error, {Reason::term(), server_ref()}}, Label::term()},
      Result :: ReqIdResp | ReqIdColResp | 'no_reply'.

check_response(Msg, ReqId) when is_reference(ReqId) ->
    case Msg of
        {[alias|ReqId], Reply} ->
            erlang:demonitor(ReqId, [flush]),
            {reply, Reply};
        {'DOWN', ReqId, _, Object, Reason} ->
            {error, {Reason, Object}};
        _ ->
            no_reply
    end;
check_response(_, _) ->
    error(badarg).

-spec check_response(Msg, ReqIdCol, Delete) -> Result when
      Msg :: term(),
      ReqIdCol :: request_id_collection(),
      Delete :: boolean(),
      Resp :: {reply, Reply::term()} | {error, {Reason::term(), server_ref()}},
      Result :: {Resp, Label::term(), NewReqIdCol::request_id_collection()}
              | 'no_request' | 'no_reply'.

check_response(_Msg, ReqIdCol, Delete) when map_size(ReqIdCol) == 0,
                                            is_boolean(Delete) ->
    no_request;
check_response(Msg, ReqIdCol, Delete) when is_map(ReqIdCol),
                                           is_boolean(Delete) ->
    case Msg of
        {[alias|ReqId], _} = Msg when is_map_key(ReqId, ReqIdCol) ->
            collection_result(Msg, ReqIdCol, Delete);
        {'DOWN', Mref, _, _, _} = Msg when is_map_key(Mref, ReqIdCol) ->
            collection_result(Msg, ReqIdCol, Delete);
        _ ->
            no_reply
    end.

collection_result({[alias|ReqId], Reply}, ReqIdCol, Delete) ->
    _ = erlang:demonitor(ReqId, [flush]),
    collection_result({reply, Reply}, ReqId, ReqIdCol, Delete);
collection_result({'DOWN', ReqId, _, Object, Reason}, ReqIdCol, Delete) ->
    collection_result({error, {Reason, Object}}, ReqId, ReqIdCol, Delete).

collection_result(Resp, ReqId, ReqIdCol, false) ->
    {Resp, maps:get(ReqId, ReqIdCol), ReqIdCol};
collection_result(Resp, ReqId, ReqIdCol, true) ->
    {Label, NewReqIdCol} = maps:take(ReqId, ReqIdCol),
    {Resp, Label, NewReqIdCol}.

flush_responses(ReqIdCol) ->
    receive
        {[alias|Mref], _Reply} when is_map_key(Mref, ReqIdCol) ->
            flush_responses(ReqIdCol)
    after 0 ->
            ok
    end.

timeout_value(infinity) ->
    infinity;
timeout_value(Timeout) when 0 =< Timeout, Timeout =< ?MAX_INT_TIMEOUT ->
    Timeout;
timeout_value({abs, Timeout}) when is_integer(Timeout) ->
    case Timeout - erlang:monotonic_time(millisecond) of
        TMO when TMO < 0 ->
            0;
        TMO when TMO > ?MAX_INT_TIMEOUT ->
            error(badarg);
        TMO ->
            TMO
    end;
timeout_value(_) ->
    error(badarg).

-spec reqids_new() ->
          NewReqIdCol::request_id_collection().

reqids_new() ->
    maps:new().

-spec reqids_size(request_id_collection()) ->
          non_neg_integer().
reqids_size(ReqIdCol) when is_map(ReqIdCol) ->
    maps:size(ReqIdCol);
reqids_size(_) ->
    error(badarg).

-spec reqids_add(ReqId::request_id(), Label::term(),
                 ReqIdCol::request_id_collection()) ->
          NewReqIdCol::request_id_collection().

reqids_add(ReqId, _, ReqIdCol) when is_reference(ReqId),
                                    is_map_key(ReqId, ReqIdCol) ->
    error(badarg);
reqids_add(ReqId, Label, ReqIdCol) when is_reference(ReqId),
                                        is_map(ReqIdCol) ->
    maps:put(ReqId, Label, ReqIdCol);
reqids_add(_, _, _) ->
    error(badarg).

-spec reqids_to_list(ReqIdCol::request_id_collection()) ->
          [{ReqId::request_id(), Label::term()}].

reqids_to_list(ReqIdCol) when is_map(ReqIdCol) ->
    maps:to_list(ReqIdCol);
reqids_to_list(_) ->
    error(badarg).

%%
%% Send a reply to the client.
%%
reply({_To, [alias|Alias] = Tag}, Reply) when is_reference(Alias) ->
    Alias ! {Tag, Reply}, ok;
reply({_To, [[alias|Alias] | _] = Tag}, Reply) when is_reference(Alias) ->
    Alias ! {Tag, Reply}, ok;
reply({To, Tag}, Reply) ->
    try To ! {Tag, Reply}, ok catch _:_ -> ok end.

%%-----------------------------------------------------------------
%% Syncronously stop a generic process
%%-----------------------------------------------------------------
stop(Process) ->
    stop(Process, normal, infinity).

stop(Process, Reason, Timeout)
  when Timeout =:= infinity; is_integer(Timeout), Timeout >= 0 ->
    Fun = fun(Pid) -> proc_lib:stop(Pid, Reason, Timeout) end,
    do_for_proc(Process, Fun).

%%-----------------------------------------------------------------
%% Map different specifications of a process to either Pid or
%% {Name,Node}. Execute the given Fun with the process as only
%% argument.
%% -----------------------------------------------------------------

%% Local or remote by pid
do_for_proc(Pid, Fun) when is_pid(Pid) ->
    Fun(Pid);
%% Local by name
do_for_proc(Name, Fun) when is_atom(Name) ->
    case whereis(Name) of
	Pid when is_pid(Pid) ->
	    Fun(Pid);
	undefined ->
	    exit(noproc)
    end;
%% Global by name
do_for_proc(Process, Fun)
  when ((tuple_size(Process) == 2 andalso element(1, Process) == global)
	orelse
	  (tuple_size(Process) == 3 andalso element(1, Process) == via)) ->
    case where(Process) of
	Pid when is_pid(Pid) ->
	    Node = node(Pid),
	    try Fun(Pid)
	    catch
		exit:{nodedown, Node} ->
		    %% A nodedown not yet detected by global,
		    %% pretend that it was.
		    exit(noproc)
	    end;
	undefined ->
	    exit(noproc)
    end;
%% Local by name in disguise
do_for_proc({Name, Node}, Fun) when Node =:= node() ->
    do_for_proc(Name, Fun);
%% Remote by name
do_for_proc({_Name, Node} = Process, Fun) when is_atom(Node) ->
    if
	node() =:= nonode@nohost ->
	    exit({nodedown, Node});
	true ->
	    Fun(Process)
    end.


%%%-----------------------------------------------------------------
%%%  Misc. functions.
%%%-----------------------------------------------------------------
where({global, Name}) -> global:whereis_name(Name);
where({via, Module, Name}) -> Module:whereis_name(Name);
where({local, Name})  -> whereis(Name);
where(ServerName) ->
    error(badarg, [ServerName]).

register_name({local, Name} = LN) ->
    try register(Name, self()) of
	true -> true
    catch
	error:_ ->
	    {false, where(LN)}
    end;
register_name({global, Name} = GN) ->
    case global:register_name(Name, self()) of
	yes -> true;
	no -> {false, where(GN)}
    end;
register_name({via, Module, Name} = GN) ->
    case Module:register_name(Name, self()) of
	yes ->
	    true;
	no ->
	    {false, where(GN)}
    end.

name({local,Name}) -> Name;
name({global,Name}) -> Name;
name({via,_, Name}) -> Name;
name(Pid) when is_pid(Pid) -> Pid.

unregister_name({local,Name}) ->
    try unregister(Name) of
	_ -> ok
    catch
	_:_ -> ok
    end;
unregister_name({global,Name}) ->
    _ = global:unregister_name(Name),
    ok;
unregister_name({via, Mod, Name}) ->
    _ = Mod:unregister_name(Name),
    ok;
unregister_name(Pid) when is_pid(Pid) ->
    ok.

get_proc_name(Pid) when is_pid(Pid) ->
    Pid;
get_proc_name({local, Name}) ->
    case process_info(self(), registered_name) of
	{registered_name, Name} ->
	    Name;
	{registered_name, _Name} ->
	    exit(process_not_registered);
	[] ->
	    exit(process_not_registered)
    end;
get_proc_name({global, Name}) ->
    case global:whereis_name(Name) of
	undefined ->
	    exit(process_not_registered_globally);
	Pid when Pid =:= self() ->
	    Name;
	_Pid ->
	    exit(process_not_registered_globally)
    end;
get_proc_name({via, Mod, Name}) ->
    case Mod:whereis_name(Name) of
	undefined ->
	    exit({process_not_registered_via, Mod});
	Pid when Pid =:= self() ->
	    Name;
	_Pid ->
	    exit({process_not_registered_via, Mod})
    end.

get_parent() ->
    case get('$ancestors') of
	[Parent | _] when is_pid(Parent) ->
	    Parent;
	[Parent | _] when is_atom(Parent) ->
	    name_to_pid(Parent);
	_ ->
	    exit(process_was_not_started_by_proc_lib)
    end.

name_to_pid(Name) ->
    case whereis(Name) of
	undefined ->
	    case global:whereis_name(Name) of
		undefined ->
		    exit(could_not_find_registered_name);
		Pid ->
		    Pid
	    end;
	Pid ->
	    Pid
    end.


timeout(Options) ->
    case lists:keyfind(timeout, 1, Options) of
	{_,Time} ->
	    Time;
	false ->
	    infinity
    end.

spawn_opts(Options) ->
    case lists:keyfind(spawn_opt, 1, Options) of
	{_,Opts} ->
	    Opts;
	false ->
	    []
    end.

hibernate_after(Options) ->
	case lists:keyfind(hibernate_after, 1, Options) of
		{_,HibernateAfterTimeout} ->
			HibernateAfterTimeout;
		false ->
			infinity
	end.

debug_options(Name, Opts) ->
    case lists:keyfind(debug, 1, Opts) of
	{_,Options} ->
	    try sys:debug_options(Options)
	    catch _:_ ->
		    error_logger:format(
		      "~tp: ignoring erroneous debug options - ~tp~n",
		      [Name,Options]),
		    []
	    end;
	false ->
	    []
    end.

format_status_header(TagLine, Pid) when is_pid(Pid) ->
    lists:concat([TagLine, " ", pid_to_list(Pid)]);
format_status_header(TagLine, RegName) when is_atom(RegName) ->
    lists:concat([TagLine, " ", RegName]);
format_status_header(TagLine, Name) ->
    {TagLine, Name}.

-spec format_status(Mod :: module(), Opt :: terminate | normal, Status, Args) ->
          ReturnStatus when
      Status :: #{ atom() => term() },
      ReturnStatus :: #{ atom() => term(), '$status' => term()},
      Args :: list(term()) | undefined.
format_status(Mod, Opt, Status, Args) ->
    case {erlang:function_exported(Mod, format_status, 1),
          erlang:function_exported(Mod, format_status, 2)} of
        {true, _} ->
            try Mod:format_status(Status) of
                NewStatus when is_map(NewStatus) ->
                    MergedStatus = maps:merge(Status, NewStatus),
                    case maps:size(MergedStatus) =:= maps:size(NewStatus) of
                        true ->
                            MergedStatus;
                        false ->
                            Status#{ 'EXIT' => atom_to_list(Mod) ++ ":format_status/1 returned a map with unknown keys" }
                    end;
                _ ->
                    Status#{ 'EXIT' => atom_to_list(Mod) ++ ":format_status/1 did not return a map" }
            catch
                _:_ ->
                    Status#{ 'EXIT' => atom_to_list(Mod) ++ ":format_status/1 crashed" }
            end;
        {false, true} when is_list(Args) ->
            try Mod:format_status(Opt, Args) of
                Result ->
                    Status#{ '$status' => Result }
            catch
                throw:Result ->
                    Status#{ '$status' => Result };
                _:_ ->
                    Status#{ 'EXIT' => atom_to_list(Mod) ++ ":format_status/2 crashed" }
            end;
        {false, _} ->
            Status
    end.
