%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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

-module(erts_dirty_process_signal_handler).

-export([start/0]).

%%
%% The erts_dirty_process_signal_handler is started at
%% VM boot by the VM. It is a spawned as a system
%% process, i.e, the whole VM will terminate if
%% this process terminates.
%%
start() ->
    process_flag(trap_exit, true),
    msg_loop().

msg_loop() ->
    _ = receive
	    Request ->
                try
                    handle_request(Request)
                catch
                    _ : _ ->
                        %% Ignore all failures;
                        %% someone passed us garbage...
                        ok
                end
	end,
    msg_loop().

handle_request(Pid) when is_pid(Pid) ->
    handle_incoming_signals(Pid, 0);
handle_request({Requester, Target, Prio,
                {SysTaskOp, ReqId, Arg} = Op} = Request) ->
    case handle_sys_task(Requester, Target, SysTaskOp, ReqId, Arg) of
        true ->
            ok;
        false ->
            %% Target has stopped executing dirty since the
            %% initial request was made. Dispatch the
            %% request to target and let it handle it itself...
            case erts_internal:request_system_task(Requester,
                                                   Target,
                                                   Prio,
                                                   Op) of
                ok ->
                    ok;
                dirty_execution ->
                    %% Ahh... It began executing dirty again...
                    handle_request(Request)
            end
    end;
handle_request(_Garbage) ->
    ignore.

%%
%% ----------------------------------------------------------------------------
%%

handle_incoming_signals(Pid, 5) ->
    self() ! Pid; %% Work with other requests for a while...    
handle_incoming_signals(Pid, N) ->
    case erts_internal:dirty_process_handle_signals(Pid) of
        more -> handle_incoming_signals(Pid, N+1);
        _Res -> ok
    end.

handle_sys_task(Requester, Target, check_process_code, ReqId, Module) ->
    case erts_internal:is_process_executing_dirty(Target) of
        false ->
            false;
        true ->
            _ = check_process(Requester, Target, ReqId, Module),
            true
    end.

check_process(Requester, Target, ReqId, Module) ->
    Result = erts_internal:check_dirty_process_code(Target, Module),
    Requester ! {check_process_code, ReqId, Result}.
