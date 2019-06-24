%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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
-module(erl_init).

%% Initial process of an Erlang system.

-export([start/2]).

%% This gets the module name given by the +i option (default 'init')
%% and the list of command line arguments

-spec start(Mod, BootArgs) -> no_return() when
      Mod :: module(),
      BootArgs :: [binary()].
start(Mod, BootArgs) ->
    %% Load the static nifs
    zlib:on_load(),
    erl_tracer:on_load(),
    prim_buffer:on_load(),
    prim_file:on_load(),
    %% socket:on_load(), prim_net:on_load(),
    conditional_load(socket, [socket, prim_net]),
    %% Proceed to the specified boot module
    run(Mod, boot, BootArgs).

run(M, F, A) ->
    case erlang:function_exported(M, F, 1) of
	false ->
	    erlang:display({fatal,error,module,M,"does not export",F,"/1"}),
	    halt(1);
	true ->
            M:F(A)
    end.

conditional_load(CondMod, Mods2Load) ->
    Loaded = erlang:loaded(),
    %% erlang:display({?MODULE, conditional_load, Loaded}),
    conditional_load(CondMod, Loaded, Mods2Load).

conditional_load(_CondMod, [], _Mods2LOad) ->
    ok;
conditional_load(CondMod, [CondMod|_], Mods2Load) ->
    on_load(Mods2Load);
conditional_load(CondMod, [_|T], Mods2Load) ->
    conditional_load(CondMod, T, Mods2Load).

on_load([]) ->
    ok;
on_load([Mod|Mods]) ->
    Mod:on_load(),
    on_load(Mods).


    


