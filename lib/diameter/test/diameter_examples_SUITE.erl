%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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

%%
%% Test example code under ../examples/code.
%%

-module(diameter_examples_SUITE).

-export([suite/0,
         all/0]).

%% testcases
-export([compile/1,
         enslave/1,
         start/1,
         traffic/1,
         stop/1]).

-export([install/1,
         call/1]).

-include("diameter.hrl").

%% ===========================================================================

-define(util, diameter_util).

%% The order here is significant and causes the server to listen
%% before the clients connect.
-define(NODES, [compile, server, client]).

%% Options to ct_slave:start/2.
-define(TIMEOUTS, [{T, 15000} || T <- [boot_timeout,
                                       init_timeout,
                                       start_timeout]]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 45}}].

all() ->
    [compile,
     enslave,
     start,
     traffic,
     stop].

%% ===========================================================================

%% compile/1

compile(Config) ->
    Node = slave(hd(?NODES), here()),
    [] = rpc:call(Node,
                  ?MODULE,
                  install,
                  [proplists:get_value(priv_dir, Config)]).

%% Compile on another node since the code path may be modified.
install(PrivDir) ->
    Top = install(here(), PrivDir),
    Src = filename:join([Top, "examples", "code"]),
    Files = find_files(Src, ".*\\.erl"),
    [] = [{F,E} || F <- Files,
                   {error, _, _} = E <- [compile:file(F, [warnings_as_errors,
                                                          return_errors])]].

%% Copy include files into a temporary directory and adjust the code
%% path in order for example code to be able to include them with
%% include_lib. This is really only required when running in the reop
%% since generated includes, that the example code wants to
%% include_lib, are under src/gen and there's no way to get get the
%% preprocessor to find these otherwise. Generated hrls are only be
%% under include in an installation. ("Installing" them locally is
%% anathema.)
install(Dir, PrivDir) ->
    %% Remove the path added by slave/1 (needed for the rpc:call/4 in
    %% compile/1 to find ?MODULE) so the call to code:lib_dir/2 below
    %% returns the installed path.
    [Ebin | _] = code:get_path(),
    true = code:del_path(Ebin),
    Top = top(Dir, code:lib_dir(diameter)),

    %% Create a new diameter/include in priv_dir, copy all includes
    %% there: first the installed includes, then any we find in
    %% ../include and ../src/gen.
    Tmp = filename:join([PrivDir, "diameter"]),
    TmpInc = filename:join([PrivDir, "diameter", "include"]),
    TmpEbin = filename:join([PrivDir, "diameter", "ebin"]),
    [] = [{T,E} || T <- [Tmp, TmpInc, TmpEbin],
                   {error, E} <- [file:make_dir(T)]],

    Inc = filename:join([Top, "include"]),
    Gen = filename:join([Top, "src", "gen"]),
    Files = lists:append([find_files(F, ".*\\.hrl") || F <- [Inc, Gen]]),
    [] = [{F,E} || F <- Files,
                   B <- [filename:basename(F)],
                   D <- [filename:join([TmpInc, B])],
                   {error, E} <- [file:copy(F,D)]],

    %% Prepend the created directory just so that code:lib_dir/1 finds
    %% it when compile:file/2 tries to resolve include_lib.
    true = code:add_patha(TmpEbin),
    Tmp = code:lib_dir(diameter),  %% assert
    %% Return the top directory containing examples/code.
    Top.

find_files(Dir, RE) ->
    filelib:fold_files(Dir, RE, false, fun cons/2, []).

cons(H,T) ->
    [H|T].

%% enslave/1
%%
%% Start two nodes: one for the server, one for the client.

enslave(Config) ->
    Dir = here(),
    Nodes = [{N, slave(N, Dir)} || N <- tl(?NODES)],
    ?util:write_priv(Config, nodes, Nodes).

slave(Name, Dir) ->
    {ok, Node} = ct_slave:start(Name, ?TIMEOUTS),
    ok = rpc:call(Node,
                  code,
                  add_pathsa,
                  [[Dir, filename:join([Dir, "..", "ebin"])]]),
    Node.

here() ->
    filename:dirname(code:which(?MODULE)).

top(Dir, LibDir) ->
    File = filename:join([Dir, "depend.sed"]),  %% only in the repo
    case filelib:is_regular(File) of
        true  -> filename:join([Dir, ".."]);
        false -> LibDir
    end.

%% start/1

start(server) ->
    ok = diameter:start(),
    ok = server:start(),
    {ok, Ref} = server:listen(tcp),
    [_] = ?util:lport(tcp, Ref, 20),
    ok;

start(client) ->
    ok = diameter:start(),
    true = diameter:subscribe(client),
    ok = client:start(),
    {ok, Ref} = client:connect(tcp),
    receive #diameter_event{info = {up, Ref, _, _, _}} -> ok end;

start(Config) ->
    Nodes = ?util:read_priv(Config, nodes),
    [] = [RC || {T,N} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, start, [T])],
                RC /= ok].

%% traffic/1
%%
%% Send successful messages from client to server.

traffic(server) ->
    ok;

traffic(client) ->
    {_, MRef} = spawn_monitor(fun() -> call(100) end),
    receive {'DOWN', MRef, process, _, Reason} -> Reason end;

traffic(Config) ->
    Nodes = ?util:read_priv(Config, nodes),
    [] = [RC || {T,N} <- Nodes,
                RC <- [rpc:call(N, ?MODULE, traffic, [T])],
                RC /= ok].

call(0) ->
    exit(ok);

call(N) ->
    {ok, _} = client:call(),
    call(N-1).

%% stop/1

stop(Name)
  when is_atom(Name) ->
    {ok, _Node} = ct_slave:stop(Name),
    ok;

stop(_Config) ->
    [] = [RC || N <- ?NODES, RC <- [stop(N)], RC /= ok].
