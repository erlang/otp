%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

%% A NIF resource that sends a message in the destructor.

-module(erts_test_destructor).

-export([init/1, send/2]).

init(Config) ->
    case is_loaded() of
        false ->
            Path = proplists:get_value(data_dir, Config),
            erlang:load_nif(filename:join(Path,?MODULE), []);
        true ->
            ok
    end.

is_loaded() ->
    false.

%% Create a resource which sends Msg to Pid when destructed.
send(_Pid, _Msg) ->
    erlang:nif_error("NIF not loaded").
