%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2024. All Rights Reserved.
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

-module(diameter_util).

%%
%% Utility functions.
%%

%% generic
-export([name/1,
         consult/2,
         run/1,
         fold/3,
         foldl/3,
         scramble/1,
         choose/1,
         tmpdir/0,
         mktemp/1,
         peer/1,
         unique_string/0,
         have_sctp/0,
         eprof/1]).

%% diameter-specific
-export([lport/2,
         listen/2, listen/3,
         connect/3, connect/4,
         disconnect/4,
         info/0]).

-define(L, atom_to_list).

-define(LOG(F, A), log(?LINE, F, A)).


%% ---------------------------------------------------------------------------

eprof(start) ->
    eprof:start(),
    eprof:start_profiling([self()]);

eprof(stop) ->
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop().

%% ---------------------------------------------------------------------------
%% name/2
%%
%% Construct and deconstruct lists of atoms as atoms to work around
%% group names in common_test being restricted to atoms.

name(Names)
  when is_list(Names) ->
    list_to_atom(string:join([atom_to_list(A) || A <- Names], ","));

name(A)
  when is_atom(A) ->
    [list_to_atom(S) || S <- string:tokens(atom_to_list(A), ",")].

%% ---------------------------------------------------------------------------
%% consult/2
%%
%% Extract info from the app/appup file (presumably) of the named
%% application.

consult(Name, Suf)
  when is_atom(Name), is_atom(Suf) ->
    case code:lib_dir(Name, ebin) of
        {error = E, Reason} ->
            {E, {Name, Reason}};
        Dir ->
            consult(filename:join([Dir, ?L(Name) ++ "." ++ ?L(Suf)]))
    end.

consult(Path) ->
    case file:consult(Path) of
        {ok, Terms} ->
            Terms;
        {error, Reason} ->
            {error, {Path, Reason}}
    end.
%% Name/Path in the return value distinguish the errors and allow for
%% a useful badmatch.

%% ---------------------------------------------------------------------------
%% run/1
%%
%% Evaluate functions in parallel and raise an error exception if any
%% fail to return.

run(L) ->
    Ref = make_ref(),
    AccF = fun(I, [F|T]) ->
                   Ref == (catch element(1, I))
                       orelse error(#{failed => F, reason => I}),
                   T
           end,
    Pid = self(),
    Funs = [fun() -> down(Pid, self()), {Ref, eval(F)} end || F <- L],
    [] = fold(AccF, L, Funs).

%% down/2

down(Parent, Worker)
  when is_pid(Parent) ->
    spawn(fun() ->
                  monitor(process, Worker),
                  down(monitor(process, Parent), Worker)
          end);

%% Die with the worker, kill the worker if the parent dies.
down(ParentMRef, WorkerPid) ->
    ?LOG("down -> await worker (~p) termination", [WorkerPid]),
    receive
        {'EXIT', TCPid, {timetrap_timeout = R, TCTimeout, TCStack}} ->
            ?LOG("down -> test case timetrap timeout when"
                 "~n   (test case) Pid:     ~p"
                 "~n   (test case) Timeout: ~p"
                 "~n   (test case) Stack:   ~p", [TCPid, TCTimeout, TCStack]),
            exit(WorkerPid, kill),
            %% So many wrapper levels, make sure we go with a bang
            exit({TCPid, R, TCStack});
        {'DOWN', ParentMRef, process, PPid, PReason} ->
            ?LOG("down -> parent process (~p) died: "
                 "~n   Reason: ~p", [PPid, PReason]),
            exit(WorkerPid, kill);
        {'DOWN', _, process, WorkerPid, WReason} ->
            ?LOG("down -> worker process (~p) died: "
                 "~n   Reason: ~p", [WorkerPid, WReason]),
            ok
    end.

%% ---------------------------------------------------------------------------
%% fold/3
%%
%% Parallel fold. Results are folded in the order received.

fold(Fun, Acc0, L)
  when is_list(L) ->
    fold(Fun, Acc0, lists:foldl(fun(F,A) ->
                                        {P,M} = spawn_eval(F),
                                        A#{M => P}
                                end,
                                #{},
                                L));

fold(_, Acc, Map)
  when 0 == map_size(Map) ->
    Acc;

fold(Fun, Acc, #{} = Map) ->
    receive
        {'DOWN', MRef, process, _, Info} when is_map_key(MRef, Map) ->
            fold(Fun, Fun(Info, Acc), maps:remove(MRef, Map))
    end.

%% spawn_eval/1

spawn_eval(F) ->
    spawn_monitor(fun() -> exit(eval(F)) end).

%% ---------------------------------------------------------------------------
%% foldl/3
%%
%% Parallel fold. Results are folded in order of the function list.

foldl(Fun, Acc0, L) ->
    lists:foldl(fun(R,A) -> acc(Fun, R, A) end,
                Acc0,
                [M || F <- L, {_,M} <- [spawn_eval(F)]]).

%% acc/3

acc(Fun, MRef, Acc) ->
    receive {'DOWN', MRef, process, _, Info} -> Fun(Info, Acc) end.

%% ---------------------------------------------------------------------------
%% scramble/1
%%
%% Sort a list into random order.

scramble(L) ->
    [X || {_,X} <- lists:sort([{rand:uniform(), T} || T <- L])].

%% ---------------------------------------------------------------------------
%% choose/1
%%
%% Return a random element from a list.

choose([_|_] = List) ->
    hd(lists:nthtail(rand:uniform(length(List)) - 1, List)).

%% ---------------------------------------------------------------------------
%% tmpdir/0

tmpdir() ->
    case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        Dir ->
            Dir
    end.

%% mktemp/1

mktemp(Prefix) ->
    Suf = integer_to_list(erlang:monotonic_time()),
    Tmp = filename:join(tmpdir(), Prefix ++ "." ++ Suf),
    ok = file:make_dir(Tmp),
    Tmp.

%% ---------------------------------------------------------------------------
%% peer/1
%%
%% Start a peer that dies with the calling process, but start it from
%% a spawned process to make it insensitive to the exit reason of the
%% calling process.

peer(#{name := _} = Opts) ->
    Ref = make_ref(),
    Pid = self(),
    {_, MRef} = spawn_monitor(fun() -> peer(Opts, Pid, Ref) end),
    receive
        {Ref, T} ->
            erlang:demonitor(MRef, [flush]),
            T;
        {'DOWN', MRef, process, _, T} ->
            {error, T}
    end;

peer(#{} = Opts) ->
    peer(Opts#{name => peer:random_name()}).

%% peer/3

peer(Opts, Pid, Ref) ->
    Cookie = atom_to_list(erlang:get_cookie()),
    Args = [str(B) || B <- maps:get(args, Opts, [])],
    Pid ! {Ref, peer:start_link(Opts#{args => ["-setcookie", Cookie | Args]})},
    MRef = monitor(process, Pid),
    receive {'DOWN', MRef, process, _, _} -> ok end.

%% str/1
%%
%% Turn possible iodata() into string().

str(Bytes) ->
    binary_to_list(iolist_to_binary(Bytes)).

%% ---------------------------------------------------------------------------
%% unique_string/0

unique_string() ->
    integer_to_list(erlang:unique_integer()).

%% ---------------------------------------------------------------------------
%% have_sctp/0

have_sctp() ->
    have_sctp(erlang:system_info(system_architecture)).

%% Don't run SCTP on platforms where it's either known to be flakey or
%% isn't available.

have_sctp("sparc-sun-solaris2.10") ->
    false;

have_sctp(_) ->
    case gen_sctp:open() of
        {ok, Sock} ->
            gen_sctp:close(Sock),
            true;
        _ ->
            false
    end.

%% ---------------------------------------------------------------------------
%% eval/1
%%
%% Evaluate a function in one of a number of forms.

eval({F, infinity}) ->
    eval(F);
eval({F, Tmo})
  when is_integer(Tmo) ->
    {ok, _} = timer:exit_after(Tmo, timeout),
    eval(F);

eval({M,[F|A]})
  when is_atom(F) ->
    apply(M,F,A);

eval({M,F,A}) ->
    apply(M,F,A);

eval([F|A])
  when is_function(F) ->
    apply(F,A);

eval(L)
  when is_list(L) ->
    [eval(F) || F <- L];

eval(F)
  when is_function(F,0) ->
    F().

%% ---------------------------------------------------------------------------
%% lport/2
%%
%% Lookup the port number of a tcp/sctp listening transport.

lport(Prot, {Node, Ref}) ->
    rpc:call(Node, ?MODULE, lport, [Prot, Ref]);

lport(Prot, Ref) ->
    [_] = diameter_reg:wait({'_', listener, {Ref, '_'}}),
    [N || M <- tmod(Prot), {listen, N, _} <- M:ports(Ref)].

%% ---------------------------------------------------------------------------
%% listen/2-3
%%
%% Add a listening transport on the loopback address and a free port.

listen(SvcName, Prot) ->
    listen(SvcName, Prot, []).

listen(SvcName, Prot, Opts) ->
    SvcName = diameter:service_info(SvcName, name),  %% assert
    Ref = add_transport(SvcName, {listen, opts(Prot, listen) ++ Opts}),
    true = transport(SvcName, Ref),                  %% assert
    Ref.

%% ---------------------------------------------------------------------------
%% connect/2-3
%%
%% Add a connecting transport on and connect to a listening transport
%% with the specified reference.

connect(Client, Prot, LRef) ->
    connect(Client, Prot, LRef, []).

connect(Client, ProtOpts, LRef, Opts) ->
    Prot = head(ProtOpts),
    [PortNr] = lport(Prot, LRef),
    Client = diameter:service_info(Client, name),  %% assert
    true = diameter:subscribe(Client),
    Ref = add_transport(Client, {connect, opts(ProtOpts, PortNr) ++ Opts}),
    true = transport(Client, Ref),                 %% assert

    diameter_lib:for_n(fun(_) -> ok = up(Client, Ref, Prot, PortNr) end,
                       proplists:get_value(pool_size, Opts, 1)),
    Ref.

head([T|_]) ->
    T;
head(T) ->
    T.

up(Client, Ref, Prot, PortNr) ->
    receive
        {diameter_event, Client, {up, Ref, _, _, _}} -> ok
    after 10000 ->
            {Client, Prot, PortNr, process_info(self(), messages)}
    end.

transport(SvcName, Ref) ->
    [Ref] == [R || [{ref, R} | _] <- diameter:service_info(SvcName, transport),
                   R == Ref].

%% ---------------------------------------------------------------------------
%% disconnect/4
%%
%% Remove the client transport and expect the server transport to go
%% down.

disconnect(Client, Ref, Server, LRef) ->
    true = diameter:subscribe(Server),
    ok = diameter:remove_transport(Client, Ref),
    receive
        {diameter_event, Server, {down, LRef, _, _}} ->
            ok
    after 10000 ->
            {Client, Ref, Server, LRef, process_info(self(), messages)}
    end.

%% ---------------------------------------------------------------------------

-define(ADDR, {127,0,0,1}).

add_transport(SvcName, T) ->
    {ok, Ref} = diameter:add_transport(SvcName, T),
    Ref.

tmod(tcp) ->
    [diameter_tcp];
tmod(sctp) ->
    [diameter_sctp];
tmod(any) ->
    [diameter_sctp, diameter_tcp].

opts([Prot | Opts], T) ->
    tmo(T, lists:append([[{transport_module, M}, {transport_config, C ++ Opts}]
                         || M <- tmod(Prot),
                            C <- [cfg(M,T) ++ cfg(M) ++ cfg(T)]]));

opts(Prot, T) ->
    opts([Prot], T).

tmo(listen, Opts) ->
    Opts;
tmo(_, Opts) ->
    tmo(Opts).

%% Timeout on all but the last alternative.
tmo([_,_] = Opts) ->
    Opts;
tmo([M, C | Opts]) ->
    {transport_config = K, Cfg} = C,
    [M, {K, Cfg, 5000} | tmo(Opts)].

%% Listening SCTP socket need larger-than-default buffers to avoid
%% resends on some platforms (eg. SLES 11).
cfg(diameter_sctp, listen) ->
    [{recbuf, 1 bsl 16}, {sndbuf, 1 bsl 16}];

cfg(_, _) ->
    [].

cfg(M)
  when M == diameter_tcp;
       M == diameter_sctp ->
    [{ip, ?ADDR}, {port, 0}];

cfg(listen) ->
    [{accept, M} || M <- [{256,0,0,1}, ["256.0.0.1", ["^.+$"]]]];
cfg(PortNr) ->
    [{raddr, ?ADDR}, {rport, PortNr}].

%% ---------------------------------------------------------------------------
%% info/0

info() ->
    [_|_] = Svcs = diameter:services(),  %% assert
    run([[fun info/1, S] || S <- Svcs]).

info(S) ->
    [_|_] = Keys = diameter:service_info(S, keys),
    [] = run([[fun info/2, K, S] || K <- Keys]).

info(Key, SvcName) ->
    [{Key, _}] = diameter:service_info(SvcName, [Key]).

log(LINE, F, A) ->
    ct:log("[DUTIL:~w,~p] " ++ F ++ "~n", [LINE,self()|A]).
