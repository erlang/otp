%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(otp_ring0).

%% Purpose : Start up of erlang system.

-export([start/2]).

-spec start(_, term()) -> term().
start(_Env, Argv) ->
    run(init, boot, Argv).

run(M, F, A) ->
    case erlang:function_exported(M, F, 1) of
	false ->
	    erlang:display({fatal,error,module,M,"does not export",F,"/1"}),
	    halt(1);
	true ->
            M:F(A)
    end.
