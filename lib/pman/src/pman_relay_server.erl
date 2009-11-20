%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Relay server code.
%%%----------------------------------------------------------------------

-module(pman_relay_server).

%%-compile(export_all).
-export([init/1]).



init(P) ->
    process_flag(trap_exit, true),

    loop(P).

loop(P) ->
    receive
	{ok_to_trace, PidSender, PidToTrace} ->
	    case catch erlang:trace(PidToTrace, false, [send]) of
		1 ->
		    PidSender ! {ok_to_trace, self()},
		    loop(P);
		_Otherwise ->
		    PidSender ! {not_ok_to_trace, self()}
	    end;

	{P, M,F,A} ->
	    case catch apply(M, F, A) of
		1 -> ok;
		_Other ->  P ! {print, "** Illegal trace request **\n", []}
	    end,
	    loop(P);
	{'EXIT', P, _Reason} ->
	    exit(normal);
	Other ->             %% Here is the normal case for trace i/o
	    P ! Other, 
	    loop(P)
    end.
