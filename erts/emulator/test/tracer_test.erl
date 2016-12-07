%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

-module(tracer_test).

%%%
%%% Test tracer
%%%

-export([enabled/3, trace/5]).
-export([load/1, load/2]).
-on_load(load/0).

enabled(_, _, _) ->
    erlang:nif_error(nif_not_loaded).

trace(_, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

load() ->
    case whereis(tracer_test_config) of
        undefined ->
            ok;
        Pid ->
            Pid ! {get, self()},
            receive
                {Conf, Postfix} ->
                    load(Conf, Postfix);
                Conf ->
                    load(Conf)
            end
    end.

load(DataDir) ->
    load(DataDir, "").
load(DataDir, Postfix) ->
    SoFile = atom_to_list(?MODULE) ++ Postfix,
    erlang:load_nif(filename:join(DataDir, SoFile) , 0).
