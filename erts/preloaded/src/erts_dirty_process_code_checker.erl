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

-module(erts_dirty_process_code_checker).

-export([start/0]).

%%
%% The erts_dirty_process_code_checker is started at
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
		handle_request(Request)
	end,
    msg_loop().

check_process(Requester, Target, ReqId, Module) ->
    Result = erts_internal:check_dirty_process_code(Target, Module),
    Requester ! {check_process_code, ReqId, Result}.
					  
handle_request({Requester,
		Target,
		Prio,
		{check_process_code,
		 ReqId,
		 Module,
		 _Flags} = Op}) ->
    %%
    %% Target may have stopped executing dirty since the
    %% initial request was made. Check its current state
    %% and try to send the request if possible; otherwise,
    %% check the dirty executing process and send the result...
    %%
    try
	case erts_internal:is_process_executing_dirty(Target) of
	    true ->
		check_process(Requester, Target, ReqId, Module);
	    false ->
		case erts_internal:request_system_task(Requester,
						       Target,
						       Prio,
						       Op) of
		    ok ->
			ok;
		    dirty_execution ->
			check_process(Requester, Target, ReqId, Module)
		end
	end
    catch
	_ : _ ->
	    ok %% Ignore all failures; someone passed us garbage...
    end;
handle_request(_Garbage) ->
    ignore.

		    
	    
