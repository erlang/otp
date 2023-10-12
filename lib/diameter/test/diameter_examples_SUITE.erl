%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2022. All Rights Reserved.
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

%%
%% Test example code under ../examples/code.
%%

-module(diameter_examples_SUITE).

%% testcase, no common_test dependency
-export([run/0,
         run/1]).

%% common_test wrapping
-export([suite/0,
         all/0,
         dict/1,
         code/1]).

%% rpc calls
-export([install/1,
         start/1,
         traffic/1]).

-include("diameter.hrl").

%% ===========================================================================

-define(util, diameter_util).

%% The order here is significant and causes the server to listen
%% before the clients connect.
-define(NODES, [server, client]).

%% @inherits dependencies between example dictionaries. This is needed
%% in order compile them in the right order. Can't compile to erl to
%% find out since @inherits is a beam dependency.
-define(INHERITS, [{rfc4006_cc,  [rfc4005_nas]},
                   {rfc4072_eap, [rfc4005_nas]},
                   {rfc4740_sip, [rfc4590_digest]}]).

%% Common dictionaries to inherit from examples.
-define(DICT0, [rfc3588_base, rfc6733_base]).

%% Transport protocols over which the example Diameter nodes are run.
-define(PROTS, [sctp || ?util:have_sctp()] ++ [tcp]).

-define(L, atom_to_list).
-define(A, list_to_atom).

%% ===========================================================================
%% common_test wrapping

suite() ->
    [{timetrap, {seconds, 75}}].

all() ->
    [dict, code].

dict(Config) ->
    run(dict, Config).

code(Config) ->
    run(code, Config).

%% ===========================================================================

%% run/0

run() ->
    run(all()).

%% run/1

run({dict, Dir}) ->
    compile_dicts(Dir);
%% The example code doesn't use the example dictionaries, so a
%% separate testcase.

run({code, Dir}) ->
    run_code(Dir);

run(List)
  when is_list(List) ->
    Tmp = ?util:mktemp("diameter_examples"),
    try
        run(List, Tmp)
    after
        file:del_dir_r(Tmp)
    end.

%% run/2

%% Eg. erl -noinput -s diameter_examples_SUITE run code -s init stop ...
run(List, Dir)
  when is_list(List) ->
    ?util:run([{[fun run/1, {F, Dir}], 60000} || F <- List]);

run(F, Config) ->
    run([F], proplists:get_value(priv_dir, Config)).

%% ===========================================================================
%% compile_dicts/1
%%
%% Compile example dictionaries in examples/dict.

compile_dicts(Dir) ->
    Out = mkdir(Dir, "dict"),
    Dirs = [filename:join(H ++ ["examples", "dict"])
            || H <- [[code:lib_dir(diameter)], [here(), ".."]]],
    [] = [{F,D,RC} || {_,F} <- sort(find_files(Dirs, ".*\\.dia$")),
                      D <- ?DICT0,
                      RC <- [make(F, D, Out)],
                      RC /= ok].

sort([{_,_} | _] = Files) ->
    lists:sort(fun({A,_},{B,_}) ->
                       sort([filename:rootname(F) || F <- [A,B]])
               end,
               Files);

sort([A,B] = L) ->
    [DA,DB] = [dep([D],[]) || D <- L],
    case {[A] -- DB, [B] -- DA} of
        {[], [_]} ->  %% B depends on A
            true;
        {[_], []} ->  %% A depends on B
            false;
        {[_],[_]} ->  %% or not
            length(DA) < length(DB)
    end.

%% Recursively accumulate inherited dictionaries.
dep([D|Rest], Acc) ->
    dep(dep(D), Rest, Acc);
dep([], Acc) ->
    Acc.

dep([{Dict, _} | T], Rest, Acc) ->
    dep(T, [Dict | Rest], [Dict | Acc]);
dep([], Rest, Acc) ->
    dep(Rest, Acc).

make(Path, Dict0, Out)
  when is_atom(Dict0) ->
    make(Path, atom_to_list(Dict0), Out);

make(Path, Dict0, Out) ->
    Dict = filename:rootname(filename:basename(Path)),
    {Mod, Pre} = make_name(Dict),
    {"diameter_gen_base" ++ Suf = Mod0, _} = make_name(Dict0),
    Name = Mod ++ Suf,
    try
        ok = to_erl(Path, [{name, Name},
                           {prefix, Pre},
                           {outdir, Out},
                           {inherits, "common/" ++ Mod0}
                           | [{inherits, D ++ "/" ++ M ++ Suf}
                              || {D,M} <- dep(Dict)]]),
        ok = to_beam(filename:join(Out, Name))
    catch
        throw: {_,_} = E ->
            E
    end.

to_erl(File, Opts) ->
    case diameter_make:codec(File, Opts) of
        ok ->
            ok;
        No ->
            throw({make, No})
    end.

to_beam(Name) ->
    case compile:file(Name ++ ".erl", [return]) of
        {ok, _, _} ->
            ok;
        No ->
            throw({compile, No})
    end.

dep(Dict) ->
    case lists:keyfind(list_to_atom(Dict), 1, ?INHERITS) of
        {_, Is} ->
            lists:map(fun inherits/1, Is);
        false ->
            []
    end.

inherits(Dict)
  when is_atom(Dict) ->
    inherits(atom_to_list(Dict));

inherits(Dict) ->
    {Name, _} = make_name(Dict),
    {Dict, Name}.

make_name(Dict) ->
    {R, [$_|N]} = lists:splitwith(fun(C) -> C /= $_ end, Dict),
    {string:join(["diameter_gen", N, R], "_"), "diameter_" ++ N}.

%% ===========================================================================
%% compile_code/1
%%
%% Compile example code under examples/code.

compile_code(Tmpdir) ->
    {ok, Pid, Node} = slave(peer:random_name(), here()),
    try
        {ok, _Ebin} = rpc:call(Node, ?MODULE, install, [Tmpdir])
    after
        peer:stop(Pid)
    end.

%% Compile in another node since the code path is modified.
install(Tmpdir) ->
    {Top, Dia, Ebin} = install(here(), Tmpdir),

    %% Prepend the created directory just so that code:lib_dir/1 finds
    %% it when compile:file/2 tries to resolve include_lib.
    true = code:add_patha(Ebin),
    Dia = code:lib_dir(diameter),  %% assert

    Src = filename:join([Top, "examples", "code"]),
    Files = find_files([Src], ".*\\.erl$"),
    [] = [{F,T} || {_,F} <- Files,
                   T <- [compile:file(F, [warnings_as_errors,
                                          return_errors,
                                          {outdir, Ebin}])],
                   ok /= element(1, T)],
    {ok, Ebin}.

%% Copy include files into a temporary directory and adjust the code
%% path in order for example code to be able to include them with
%% include_lib. This is really only required when running in the reop
%% since generated includes, that the example code wants to
%% include_lib, are under src/gen and there's no way to get get the
%% preprocessor to find these otherwise. Generated hrls are only be
%% under include in an installation. ("Installing" them locally is
%% anathema.)
install(Dir, Tmpdir) ->
    Top = top(Dir, code:lib_dir(diameter)),

    %% Create a new diameter/include in priv_dir. Copy all includes
    %% there, from below ../include and ../src/gen if they exist (in
    %% the repo).
    Tmp = filename:join([Tmpdir, "diameter"]),
    TmpInc = filename:join([Tmp, "include"]),
    TmpEbin = filename:join([Tmp, "ebin"]),
    [] = [{T,E} || T <- [Tmp, TmpInc, TmpEbin],
                   {error, E} <- [file:make_dir(T)]],

    Inc = filename:join([Top, "include"]),
    Gen = filename:join([Top, "src", "gen"]),
    Files = find_files([Inc, Gen], ".*\\.hrl$"),
    [] = [{F,E} || {_,F} <- Files,
                   B <- [filename:basename(F)],
                   D <- [filename:join([TmpInc, B])],
                   {error, E} <- [file:copy(F,D)]],
    {Top, Tmp, TmpEbin}.

find_files(Dirs, RE) ->
    lists:foldl(fun(D,A) -> fold_files(D, RE, A) end,
                orddict:new(),
                Dirs).

fold_files(Dir, RE, Acc) ->
    filelib:fold_files(Dir, RE, false, fun store/2, Acc).

store(Path, Dict) ->
    orddict:store(filename:basename(Path), Path, Dict).

%% ===========================================================================

%% enslave/1
%%
%% Start two nodes: one for the server, one for the client.

enslave(Prefix) ->
    [{S,N} || D <- [here()],
              S <- ?NODES,
              M <- [lists:append(["diameter", Prefix, ?L(S)])],
              {ok, _, N} <- [slave(M,D)]].

slave(Name, Dir) ->
    Args = ["-pa", Dir, filename:join([Dir, "..", "ebin"])],
    {ok, _Pid, _Node} = ?util:peer(#{name => Name, args => Args}).

here() ->
    filename:dirname(code:which(?MODULE)).

top(Dir, LibDir) ->
    File = filename:join([Dir, "depend.sed"]),  %% only in the repo
    case filelib:is_regular(File) of
        true  -> filename:join([Dir, ".."]);
        false -> LibDir
    end.

%% start/2

start({server, Prot, Ebin}) ->
    true = code:add_patha(Ebin),
    ok = diameter:start(),
    ok = server:start(),
    {ok, Ref} = server:listen({Prot, any, 3868}),
    [_] = ?util:lport(Prot, Ref),
    ok;

start({client = Svc, Prot, Ebin}) ->
    true = code:add_patha(Ebin),
    ok = diameter:start(),
    true = diameter:subscribe(Svc),
    ok = client:start(),
    {ok, Ref} = client:connect({Prot, loopback, loopback, 3868}),
    receive
        #diameter_event{info = {up, Ref, _, _, _}} ->
            ok
    after
        2000 ->
            timeout
    end;

start([Prot, Ebin | Nodes]) ->
    [] = [RC || {S,N} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, start, [{S, Prot, Ebin}])],
                RC /= ok].

%% traffic/1
%%
%% Send successful messages from client to server.

traffic(server) ->
    ok;

traffic(client) ->
    {_, MRef} = spawn_monitor(fun() -> exit(call(100)) end),
    receive {'DOWN', MRef, process, _, Reason} -> Reason end;

traffic({Prot, Ebin}) ->
    Nodes = enslave(?L(Prot)),
    [] = start([Prot, Ebin | Nodes]),
    [] = [RC || {T,N} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, traffic, [T])],
                RC /= ok].

%% run_code/1

run_code(Dir) ->
    true = is_alive(),  %% need distribution for peer nodes
    {ok, Ebin} = compile_code(mkdir(Dir, "code")),
    ?util:run([[fun traffic/1, {T, Ebin}] || T <- ?PROTS]).

%% call/1

call(0) ->
    ok;

call(N) ->
    {ok, _} = client:call(),
    call(N-1).

%% mkdir/2

mkdir(Top, Dir) ->
    Tmp = filename:join(Top, Dir),
    ok = file:make_dir(Tmp),
    Tmp.
