%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
