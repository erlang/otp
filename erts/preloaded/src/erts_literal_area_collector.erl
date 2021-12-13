%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2021. All Rights Reserved.
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

-export([start/0, send_copy_request/3, release_area_switch/0]).

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
    msg_loop(undefined, {0, []}, 0, []).

%%
%% The VM will send us a 'copy_literals' message
%% when it has a new literal area that needs to
%% be handled is added. We will also be informed
%% about more areas when we call release_area_switch().
%%
msg_loop(Area, {Ongoing, NeedIReq} = OReqInfo, GcOutstnd, NeedGC) ->
    %% 'Ongoing' is the sum of currently outstanding requests
    %% and currently delayed requests allowing GC.

    HibernateTmo =
        if Area =:= undefined ->
                60_000;
           true ->
                infinity
        end,

    receive

	%% A new area to handle has arrived...
	copy_literals when Ongoing == 0 ->
	    switch_area();

	%% Process (_Pid) has completed the request...
	{copy_literals, {Area, _GcAllowed, _Pid}, ok} when Ongoing == 1,
                                                           NeedIReq == [] ->
	    switch_area(); %% Last process completed...
	{copy_literals, {Area, false, _Pid}, ok} ->
	    msg_loop(Area, check_send_copy_req(Area, Ongoing-1, NeedIReq),
                     GcOutstnd, NeedGC);
	{copy_literals, {Area, true, _Pid}, ok} when NeedGC == [] ->
	    msg_loop(Area, check_send_copy_req(Area, Ongoing-1, NeedIReq),
                     GcOutstnd-1, []);
	{copy_literals, {Area, true, _Pid}, ok} ->
	    send_copy_req(hd(NeedGC), Area, true),
	    msg_loop(Area, {Ongoing-1, NeedIReq}, GcOutstnd, tl(NeedGC));

	%% Process (Pid) failed to complete the request
	%% since it needs to garbage collect in order to
	%% complete the request...
	{copy_literals, {Area, false, Pid}, need_gc} when GcOutstnd < ?MAX_GC_OUTSTND ->
	    send_copy_req(Pid, Area, true),
	    msg_loop(Area, OReqInfo, GcOutstnd+1, NeedGC);
	{copy_literals, {Area, false, Pid}, need_gc} ->
	    msg_loop(Area, check_send_copy_req(Area, Ongoing, NeedIReq),
                     GcOutstnd, [Pid|NeedGC]);

	%% Not handled message regarding the area that we
	%% currently are working with. Crash the VM so
	%% we notice this bug...
	{copy_literals, {Area, _, _}, _} = Msg when erlang:is_reference(Area) ->
	    exit({not_handled_message, Msg});

        {change_prio, From, Ref, Prio} ->
            change_prio(From, Ref, Prio),
	    msg_loop(Area, OReqInfo, GcOutstnd, NeedGC);

	%% Unexpected garbage message. Get rid of it...
	_Ignore ->
	    msg_loop(Area, OReqInfo, GcOutstnd, NeedGC)

    after HibernateTmo ->
            %% We hibernate in order to clear the heap completely.
            erlang:hibernate(?MODULE, start, [])
    end.

switch_area() ->
    Res = erts_literal_area_collector:release_area_switch(),
    case Res of
	false ->
	    %% No more areas to handle...
	    msg_loop(undefined, {0, []}, 0, []);
	true ->
	    %% Send requests to OReqLim processes to copy
	    %% all live data they have referring to the
	    %% literal area that is to be released.
            %% Continue sending requests for all other
            %% processes when responses comes back until
            %% all processes have been handled...
	    Area = make_ref(),
            Pids = erlang:processes(),
            OReqLim = erlang:system_info(outstanding_system_requests_limit),
	    msg_loop(Area, send_copy_reqs(Pids, Area, OReqLim), 0, [])
    end.

check_send_copy_req(_Area, Ongoing, []) ->
    {Ongoing, []};
check_send_copy_req(Area, Ongoing, [Pid|Pids]) ->
    send_copy_req(Pid, Area, false),
    {Ongoing+1, Pids}.

send_copy_reqs(Ps, Area, OReqLim) ->
    send_copy_reqs(Ps, Area, OReqLim, 0).

send_copy_reqs([], _Area, _OReqLim, N) ->
    {N, []};
send_copy_reqs(Ps, _Area, OReqLim, N) when N >= OReqLim ->
    {N, Ps};
send_copy_reqs([P|Ps], Area, OReqLim, N) ->
    send_copy_req(P, Area, false),
    send_copy_reqs(Ps, Area, OReqLim, N+1).

send_copy_req(P, Area, GC) ->
    erts_literal_area_collector:send_copy_request(P, Area, GC).

-spec release_area_switch() -> boolean().

release_area_switch() ->
    erlang:nif_error(undef). %% Implemented in beam_bif_load.c

-spec send_copy_request(To, AreaId, GcAllowed) -> 'ok' when
      To :: pid(),
      AreaId :: term(),
      GcAllowed :: boolean().

send_copy_request(_To, _AreaId, _GcAllowed) ->
    erlang:nif_error(undef). %% Implemented in beam_bif_load.c

change_prio(From, Ref, Prio) ->
    try
        OldPrio = process_flag(priority, Prio),
        _ = From ! {Ref, OldPrio},
        ok
    catch
        _:_ ->
            _ = From ! {Ref, error},
            ok
    end.
