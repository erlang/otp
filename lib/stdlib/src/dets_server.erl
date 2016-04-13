%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(dets_server).

%% Disk based linear hashing lookup dictionary. Server part.

-behaviour(gen_server).

%% External exports.
-export([all/0, close/1, get_pid/1, open_file/1, open_file/2, pid2name/1,
         users/1, verbose/1]).

%% Internal.
-export([start_link/0, start/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

%% record for not yet handled reqeusts to open or close files
-record(pending, {tab, ref, pid, from, reqtype, clients}). % [{From,Args}]

%% state for the dets server
-record(state, {store, parent, pending}). % [pending()]

-include("dets.hrl").

-define(REGISTRY, dets_registry).  % {Table, NoUsers, TablePid}
-define(OWNERS, dets_owners).      % {TablePid, Table}
-define(STORE, dets).              % {User, Table} and {{links,User}, NoLinks}

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

-compile({inline, [{pid2name_1,1}]}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Internal.
start_link() ->
    gen_server:start_link({local, ?SERVER_NAME}, dets_server, [self()], []).

start() -> 
    ensure_started().

stop() ->
    case whereis(?SERVER_NAME) of
	undefined ->
	    stopped;
	_Pid ->
            gen_server:call(?SERVER_NAME, stop, infinity)
    end.

all() ->
    call(all).

close(Tab) ->
    call({close, Tab}).

get_pid(Tab) ->
    ets:lookup_element(?REGISTRY, Tab, 3).

open_file(File) ->
    call({open, File}).

open_file(Tab, OpenArgs) ->
    call({open, Tab, OpenArgs}).

pid2name(Pid) ->
    ensure_started(),
    pid2name_1(Pid).

users(Tab) ->
    call({users, Tab}).

verbose(What) ->
    call({set_verbose, What}).

call(Message) ->
    ensure_started(),
    gen_server:call(?SERVER_NAME, Message, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(Parent) ->
    Store = init(),
    {ok, #state{store=Store, parent=Parent, pending = []}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(all, _From, State) ->
    F = fun(X, A) -> [element(1, X) | A] end,
    {reply, ets:foldl(F, [], ?REGISTRY), State};
handle_call({close, Tab}, From, State) ->
    request([{{close, Tab}, From}], State);
handle_call({open, File}, From, State) ->
    request([{{open, File}, From}], State);
handle_call({open, Tab, OpenArgs}, From, State) ->
    request([{{open, Tab, OpenArgs}, From}], State);
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({set_verbose, What}, _From, State) ->
    set_verbose(What),
    {reply, ok, State};
handle_call({users, Tab}, _From, State) ->
    Users = ets:select(State#state.store, [{{'$1', Tab}, [], ['$1']}]),
    {reply, Users, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({pending_reply, {Ref, Result0}}, State) ->
    {value, #pending{tab = Tab, pid = Pid, from = {FromPid,_Tag}=From, 
                     reqtype = ReqT, clients = Clients}} =
        lists:keysearch(Ref, #pending.ref, State#state.pending),
    Store = State#state.store,
    Result = 
	case {Result0, ReqT} of
	    {ok, add_user} ->
		do_link(Store, FromPid),
		true = ets:insert(Store, {FromPid, Tab}),
		ets:update_counter(?REGISTRY, Tab, 1),
		{ok, Tab};
	    {ok, internal_open} ->
		link(Pid),
		do_link(Store, FromPid),
		true = ets:insert(Store, {FromPid, Tab}),
                %% do_internal_open() has already done the following:
                %% true = ets:insert(?REGISTRY, {Tab, 1, Pid}),
                %% true = ets:insert(?OWNERS, {Pid, Tab}),
		{ok, Tab};
            {Reply, internal_open} ->
                %% Clean up what do_internal_open() did:
                true = ets:delete(?REGISTRY, Tab),
                true = ets:delete(?OWNERS, Pid),
                Reply;
	    {Reply, _} -> % ok or Error
		Reply
	end,
    gen_server:reply(From, Result),
    NP = lists:keydelete(Pid, #pending.pid, State#state.pending),
    State1 = State#state{pending = NP},
    request(Clients, State1);
handle_info({'EXIT', Pid, _Reason}, State) ->
    Store = State#state.store,
    case pid2name_1(Pid) of
	{ok, Tab} -> 
	    %% A table was killed.
            true = ets:delete(?REGISTRY, Tab),
            true = ets:delete(?OWNERS, Pid),
            Users = ets:select(State#state.store, [{{'$1', Tab}, [], ['$1']}]),
            true = ets:match_delete(Store, {'_', Tab}),
            lists:foreach(fun(User) -> do_unlink(Store, User) end, Users),
            {noreply, State};
	undefined ->
	    %% Close all tables used by Pid.
            F = fun({FromPid, Tab}, S) ->
                        {_, S1} = handle_close(S, {close, Tab}, 
                                               {FromPid, notag}, Tab),
                        S1
                end,
            State1 = lists:foldl(F, State, ets:lookup(Store, Pid)),
            {noreply, State1}
    end;
handle_info(_Message, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

ensure_started() ->
    case whereis(?SERVER_NAME) of
	undefined -> 
	    DetsSup = {dets_sup, {dets_sup, start_link, []}, permanent,
		      1000, supervisor, [dets_sup]},
	    _ = supervisor:start_child(kernel_safe_sup, DetsSup),
	    DetsServer = {?SERVER_NAME, {?MODULE, start_link, []},
			  permanent, 2000, worker, [?MODULE]},
            _ = supervisor:start_child(kernel_safe_sup, DetsServer),
	    ok;
	_ -> ok
    end.

init() ->
    set_verbose(verbose_flag()),
    process_flag(trap_exit, true),
    ?REGISTRY = ets:new(?REGISTRY, [set, named_table]),
    ?OWNERS = ets:new(?OWNERS, [set, named_table]),
    ets:new(?STORE, [duplicate_bag]).

verbose_flag() ->
    case init:get_argument(dets) of
	{ok, Args} ->
	    lists:member(["verbose"], Args);
	_ ->
	    false
    end.

set_verbose(true) ->
    put(verbose, yes);
set_verbose(_) ->
    erase(verbose).

%% Inlined.
pid2name_1(Pid) ->
    case ets:lookup(?OWNERS, Pid) of
        [] -> undefined;
        [{_Pid,Tab}] -> {ok, Tab}
    end.

request([{Req, From} | L], State) ->
    Res = case Req of
              {close, Tab} -> 
                  handle_close(State, Req, From, Tab);
              {open, File} ->
                  do_internal_open(State, From, [File, get(verbose)]);
              {open, Tab, OpenArgs} ->
                  do_open(State, Req, From, OpenArgs, Tab)
          end,
    State2 = case Res of
                 {pending, State1} -> 
                     State1;
                 {Reply, State1} ->
		     gen_server:reply(From, Reply),
                     State1
             end,
    request(L, State2);
request([], State) ->
    {noreply, State}.

%% -> {pending, NewState} | {Reply, NewState}
do_open(State, Req, From, Args, Tab) ->
    case check_pending(Tab, From, State, Req) of
        {pending, NewState} -> {pending, NewState};
        false ->
            case ets:lookup(?REGISTRY, Tab) of
                [] -> 
                    A = [Tab, Args, get(verbose)],
                    do_internal_open(State, From, A);
                [{Tab, _Counter, Pid}] ->
                    pending_call(Tab, Pid, make_ref(), From, Args, 
                                 add_user, State)
            end
    end.

%% -> {pending, NewState} | {Reply, NewState}
do_internal_open(State, From, Args) ->
    case supervisor:start_child(dets_sup, [self()]) of 
        {ok, Pid} ->
            Ref = make_ref(),
            Tab = case Args of
                      [T, _, _] -> T;
                      [_, _] -> Ref
                  end,
            %% Pretend the table is open. If someone else tries to
            %% open the file it will always become a pending
            %% 'add_user' request. If someone tries to use the table
            %% there will be a delay, but that is OK.
            true = ets:insert(?REGISTRY, {Tab, 1, Pid}),
            true = ets:insert(?OWNERS, {Pid, Tab}),
            pending_call(Tab, Pid, Ref, From, Args, internal_open, State);
        Error ->
            {Error, State}
    end.

%% -> {pending, NewState} | {Reply, NewState}
handle_close(State, Req, {FromPid,_Tag}=From, Tab) ->
    case check_pending(Tab, From, State, Req) of
        {pending, NewState} -> {pending, NewState};
        false ->
            Store = State#state.store,
            case ets:match_object(Store, {FromPid, Tab}) of
                [] -> 
                    ?DEBUGF("DETS: Table ~w close attempt by non-owner~w~n",
                            [Tab, FromPid]),
                    {{error, not_owner}, State};
                [_ | Keep] ->
                    case ets:lookup(?REGISTRY, Tab) of
                        [{Tab, 1, Pid}] ->
                            do_unlink(Store, FromPid),
                            true = ets:delete(?REGISTRY, Tab),
                            true = ets:delete(?OWNERS, Pid),
                            true = ets:match_delete(Store, {FromPid, Tab}),
                            unlink(Pid),
                            pending_call(Tab, Pid, make_ref(), From, [], 
                                         internal_close, State);
                        [{Tab, _Counter, Pid}] ->
			    do_unlink(Store, FromPid),
			    true = ets:match_delete(Store, {FromPid, Tab}),
                            true = ets:insert(Store, Keep),
			    ets:update_counter(?REGISTRY, Tab, -1),
                            pending_call(Tab, Pid, make_ref(), From, [],
                                         remove_user, State)
                    end
            end
    end.

%% Links with counters
do_link(Store, Pid) ->
    Key = {links, Pid},
    case ets:lookup(Store, Key) of
	[] ->
	    true = ets:insert(Store, {Key, 1}),
	    link(Pid);
	[{_, C}] ->
	    true = ets:delete(Store, Key),
	    true = ets:insert(Store, {Key, C+1})
    end.

do_unlink(Store, Pid) ->
    Key = {links, Pid},
    case ets:lookup(Store, Key) of
	[{_, C}] when C > 1 ->
	    true = ets:delete(Store, Key),
	    true = ets:insert(Store, {Key, C-1});
	_ ->
	    true = ets:delete(Store, Key),
	    unlink(Pid)

    end.

pending_call(Tab, Pid, Ref, {FromPid, _Tag}=From, Args, ReqT, State) ->
    Server = self(),
    F = fun() -> 
                Res = case ReqT of 
                          add_user -> 
                              dets:add_user(Pid, Tab, Args);
                          internal_open -> 
                              dets:internal_open(Pid, Ref, Args);
                          internal_close ->
                              dets:internal_close(Pid);
                          remove_user ->
                              dets:remove_user(Pid, FromPid)
                      end,
                Server ! {pending_reply, {Ref, Res}}
        end,
    _ = spawn(F),
    PD = #pending{tab = Tab, ref = Ref, pid = Pid, reqtype = ReqT,
                  from = From, clients = []},
    P = [PD | State#state.pending],
    {pending, State#state{pending = P}}.

check_pending(Tab, From, State, Req) ->
    case lists:keysearch(Tab, #pending.tab, State#state.pending) of
        {value, #pending{tab = Tab, clients = Clients}=P} ->
            NP = lists:keyreplace(Tab, #pending.tab, State#state.pending, 
                                  P#pending{clients = Clients++[{Req,From}]}),
            {pending, State#state{pending = NP}};
        false ->
            false
    end.
