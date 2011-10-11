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

-export([consult/2,
         run/1,
         fold/3,
         foldl/3,
         scramble/1,
         write_priv/3,
         read_priv/2]).

-define(L, atom_to_list).

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

%% eval/1

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

%% write_priv/3

write_priv(Config, Name, Term) ->
    Dir = proplists:get_value(priv_dir, Config),
    Path = filename:join([Dir, Name]),
    ok = file:write_file(Path, term_to_binary(Term)).

%% read_priv/2

read_priv(Config, Name) ->
    Dir = proplists:get_value(priv_dir, Config),
    Path = filename:join([Dir, Name]),
    {ok, Bin} = file:read_file(Path),
    binary_to_term(Bin).
