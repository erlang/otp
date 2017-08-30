%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
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
-module(erts_literal_area_collector).

-export([start/0]).

%% Currently we only allow two outstanding literal
%% copying jobs that garbage collect in order to
%% copy the literals. Maybe we could allow more
%% than two outstanding processes, but for now we
%% play it safe...
-define(MAX_GC_OUTSTND, 2).

%%
%% The erts_literal_area_collector is started at
%% VM boot by the VM. It is a spawned as a system
%% process, i.e, the whole VM will terminate if
%% this process terminates.
%%
start() ->
    process_flag(trap_exit, true),
    msg_loop(undefined, 0, 0, []).

%%
%% The VM will send us a 'copy_literals' message
%% when it has a new literal area that needs to
%% be handled is added. We will also be informed
%% about more areas when we call
%% erts_internal:release_literal_area_switch().
%%
msg_loop(Area, Outstnd, GcOutstnd, NeedGC) ->
    receive

	%% A new area to handle has arrived...
	copy_literals when Outstnd == 0 ->
	    switch_area();

	%% Process (_Pid) has completed the request...
	{copy_literals, {Area, _GcAllowed, _Pid}, ok} when Outstnd == 1 ->
	    switch_area(); %% Last process completed...
	{copy_literals, {Area, false, _Pid}, ok} ->
	    msg_loop(Area, Outstnd-1, GcOutstnd, NeedGC);
	{copy_literals, {Area, true, _Pid}, ok} when NeedGC == [] ->
	    msg_loop(Area, Outstnd-1, GcOutstnd-1, []);
	{copy_literals, {Area, true, _Pid}, ok} ->
	    send_copy_req(hd(NeedGC), Area, true),
	    msg_loop(Area, Outstnd-1, GcOutstnd, tl(NeedGC));

	%% Process (Pid) failed to complete the request
	%% since it needs to garbage collect in order to
	%% complete the request...
	{copy_literals, {Area, false, Pid}, need_gc} when GcOutstnd < ?MAX_GC_OUTSTND ->
	    send_copy_req(Pid, Area, true),
	    msg_loop(Area, Outstnd, GcOutstnd+1, NeedGC);
	{copy_literals, {Area, false, Pid}, need_gc} ->
	    msg_loop(Area, Outstnd, GcOutstnd, [Pid|NeedGC]);

	%% Not handled message regarding the area that we
	%% currently are working with. Crash the VM so
	%% we notice this bug...
	{copy_literals, {Area, _, _}, _} = Msg when erlang:is_reference(Area) ->
	    exit({not_handled_message, Msg});

	%% Unexpected garbage message. Get rid of it...
	_Ignore ->
	    msg_loop(Area, Outstnd, GcOutstnd, NeedGC)

    end.

switch_area() ->
    Res = erts_internal:release_literal_area_switch(),
    erlang:garbage_collect(), %% Almost no live data now...
    case Res of
	false ->
	    %% No more areas to handle...
	    msg_loop(undefined, 0, 0, []);
	true ->
	    %% Send requests to all processes to copy
	    %% all live data they have referring to the
	    %% literal area that is to be released...
	    Area = make_ref(),
	    Outstnd = send_copy_reqs(erlang:processes(), Area, false),
	    msg_loop(Area, Outstnd, 0, [])
    end.

send_copy_reqs(Ps, Area, GC) ->
    send_copy_reqs(Ps, Area, GC, 0).

send_copy_reqs([], _Area, _GC, N) ->
    N;
send_copy_reqs([P|Ps], Area, GC, N) ->
    send_copy_req(P, Area, GC),
    send_copy_reqs(Ps, Area, GC, N+1).

send_copy_req(P, Area, GC) ->
    erts_internal:request_system_task(P, normal, {copy_literals, {Area, GC, P}, GC}).
