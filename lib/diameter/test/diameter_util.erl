%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(diameter_util).

%%
%% Utility functions.
%%

%% generic
-export([consult/2,
         run/1,
         fold/3,
         foldl/3,
         scramble/1]).

%% diameter-specific
-export([lport/2,
         lport/3,
         listen/2, listen/3,
         connect/3, connect/4,
         disconnect/4]).

%% common_test-specific
-export([write_priv/3,
         read_priv/2,
         map_priv/3]).

-define(L, atom_to_list).

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
%% Evaluate functions in parallel and return a list of those that
%% failed to return. The fun takes a boolean (did the function return
%% or not), the function that was evaluated, the return value or exit
%% reason and the prevailing accumulator.

run(L) ->
    fold(fun cons/4, [], L).

cons(true, _, _, Acc) ->
    Acc;
cons(false, F, RC, Acc) ->
    [{F, RC} | Acc].

%% ---------------------------------------------------------------------------
%% fold/3
%%
%% Parallel fold. Results are folded in the order received.

fold(Fun, Acc0, L)
  when is_function(Fun, 4) ->
    Ref = make_ref(),
    %% Spawn a middleman to collect down messages from processes
    %% spawned for each function so as not to assume that all DOWN
    %% messages are ours.
    MRef = run1([fun fold/4, Ref, Fun, Acc0, L], Ref),
    {Ref, RC} = down(MRef),
    RC.

fold(Ref, Fun, Acc0, L) ->
    recv(run(Ref, L), Ref, Fun, Acc0).

run(Ref, L) ->
    [{run1(F, Ref), F} || F <- L].

run1(F, Ref) ->
    {_, MRef} = spawn_monitor(fun() -> exit({Ref, eval(F)}) end),
    MRef.

recv([], _, _, Acc) ->
    Acc;
recv(L, Ref, Fun, Acc) ->
    {MRef, R} = down(),
    {MRef, F} = lists:keyfind(MRef, 1, L),
    recv(lists:keydelete(MRef, 1, L),
         Ref,
         Fun,
         acc(R, Ref, F, Fun, Acc)).

acc({Ref, RC}, Ref, F, Fun, Acc) ->
    Fun(true, F, RC, Acc);
acc(Reason, _, F, Fun, Acc) ->
    Fun(false, F, Reason, Acc).

down(MRef) ->
    receive {'DOWN', MRef, process, _, Reason} -> Reason end.

down() ->
    receive {'DOWN', MRef, process, _, Reason} -> {MRef, Reason} end.

%% ---------------------------------------------------------------------------
%% foldl/3
%%
%% Parallel fold. Results are folded in order of the function list.

foldl(Fun, Acc0, L)
  when is_function(Fun, 4) ->
    Ref = make_ref(),
    recvl(run(Ref, L), Ref, Fun, Acc0).

recvl([], _, _, Acc) ->
    Acc;
recvl([{MRef, F} | L], Ref, Fun, Acc) ->
    R = down(MRef),
    recvl(L, Ref, Fun, acc(R, Ref, F, Fun, Acc)).

%% ---------------------------------------------------------------------------
%% scramble/1
%%
%% Sort a list into random order.

scramble(L) ->
    foldl(fun(true, _, S, false) -> S end,
          false,
          [[fun s/1, L]]).

s(L) ->
    random:seed(now()),
    s([], L).

s(Acc, []) ->
    Acc;
s(Acc, L) ->
    {H, [T|Rest]} = lists:split(random:uniform(length(L)) - 1, L),
    s([T|Acc], H ++ Rest).

%% ---------------------------------------------------------------------------
%% eval/1
%%
%% Evaluate a function in one of a number of forms.

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
    run(L);

eval(F)
  when is_function(F,0) ->
    F().

%% ---------------------------------------------------------------------------
%% write_priv/3
%%
%% Write an arbitrary term to a named file.

write_priv(Config, Name, Term) ->
    write(path(Config, Name), Term).

write(Path, Term) ->
    ok = file:write_file(Path, term_to_binary(Term)).

%% read_priv/2
%%
%% Read a term from a file.

read_priv(Config, Name) ->
    read(path(Config, Name)).

read(Path) ->
    {ok, Bin} = file:read_file(Path),
    binary_to_term(Bin).
    
%% map_priv/3
%%
%% Modify a term in a file and return both old and new values.

map_priv(Config, Name, Fun1) ->
    map(path(Config, Name), Fun1).

map(Path, Fun1) ->
    T0 = read(Path),
    T1 = Fun1(T0),
    write(Path, T1),
    {T0, T1}.

path(Config, Name)
  when is_atom(Name) ->
    path(Config, ?L(Name));
path(Config, Name) ->
    Dir = proplists:get_value(priv_dir, Config),
    filename:join([Dir, Name]).

%% ---------------------------------------------------------------------------
%% lport/2-3
%%
%% Lookup the port number of a tcp/sctp listening transport.

lport(M, Ref) ->
    lport(M, Ref, 1).

lport(M, Ref, Tries) ->
    lp(tmod(M), Ref, Tries).

lp(M, Ref, T) ->
    L = [N || {listen, N, _} <- M:ports(Ref)],
    if [] /= L orelse T =< 1 ->
            L;
       true ->
            receive after 50 -> ok end,
            lp(M, Ref, T-1)
    end.

%% ---------------------------------------------------------------------------
%% listen/2-3
%%
%% Add a listening transport on the loopback address and a free port.

listen(SvcName, Prot) ->
    listen(SvcName, Prot, []).

listen(SvcName, Prot, Opts) ->
    add_transport(SvcName, {listen, opts(Prot, listen) ++ Opts}).

%% ---------------------------------------------------------------------------
%% connect/2-3
%%
%% Add a connecting transport on and connect to a listening transport
%% with the specified reference.

connect(Client, Prot, LRef) ->
    connect(Client, Prot, LRef, []).

connect(Client, Prot, LRef, Opts) ->
    [PortNr] = lport(Prot, LRef, 20),
    Ref = add_transport(Client, {connect, opts(Prot, PortNr) ++ Opts}),
    true = diameter:subscribe(Client),
    ok = receive
             {diameter_event, Client, {up, Ref, _, _, _}} -> ok
         after 2000 ->
                 {Client, Prot, PortNr, process_info(self(), messages)}
         end,
    Ref.

%% ---------------------------------------------------------------------------
%% disconnect/4
%%
%% Remove the client transport and expect the server transport to go
%% down.

disconnect(Client, Ref, Server, LRef) ->
    true = diameter:subscribe(Server),
    ok = diameter:remove_transport(Client, Ref),
    ok = receive
             {diameter_event, Server, {down, LRef, _, _}} -> ok
         after 2000 ->
                 {Client, Ref, Server, LRef, process_info(self(), messages)}
         end.

%% ---------------------------------------------------------------------------

-define(ADDR, {127,0,0,1}).

add_transport(SvcName, T) ->
    {ok, Ref} = diameter:add_transport(SvcName, T),
    Ref.

tmod(tcp) ->
    diameter_tcp;
tmod(sctp) ->
    diameter_sctp.

opts(Prot, T) ->
    [{transport_module, tmod(Prot)},
     {transport_config, [{ip, ?ADDR}, {port, 0} | opts(T)]}].

opts(listen) ->
    [];
opts(PortNr) ->
    [{raddr, ?ADDR}, {rport, PortNr}].
