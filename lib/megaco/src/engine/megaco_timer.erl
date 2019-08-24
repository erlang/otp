%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

%%
%%----------------------------------------------------------------------
%% Purpose: Timer handling
%%----------------------------------------------------------------------

-module(megaco_timer).

%% Application internal export
-export([
	 init/1,
	 restart/1,
	 verify/1
        ]).


-include_lib("megaco/include/megaco.hrl").


%%-----------------------------------------------------------------

%% init(Timer) -> {TimeoutTime, NewTimer}
%% Timer       = megaco_timer()
%% NewTimer    = megaco_timer() 
%% TimeoutTime = infinity | integer()
%% 
init(SingleWaitFor) when SingleWaitFor =:= infinity ->
    {SingleWaitFor, timeout};
init(SingleWaitFor) when is_integer(SingleWaitFor) and (SingleWaitFor >= 0) ->
    {SingleWaitFor, timeout};
init(Timer) when is_record(Timer, megaco_incr_timer) ->
    return_incr(Timer).


%% Returns {WaitFor, NewTimer} | {WaitFor, timeout}
restart(#megaco_incr_timer{wait_for    = Old,
			   factor      = Factor,
			   incr        = Incr,
			   max_retries = MaxRetries} = Timer) ->
    New    = wait_for(Old, Factor, Incr),
    Max    = decr(MaxRetries),
    Timer2 = Timer#megaco_incr_timer{wait_for    = New,
				     max_retries = Max},
    return_incr(Timer2);
restart({Timer, timeout}) when is_record(Timer, megaco_incr_timer) ->
    restart(Timer).

wait_for(Old, Factor, Incr) ->
    New = (Old * Factor) + Incr,
    if 
	New < 0 ->
	    0;
	true ->
	    New
    end.

verify(#megaco_incr_timer{wait_for    = WaitFor,
			  factor      = Factor,
			  incr        = Incr,
			  max_retries = MaxRetries}) ->
    (megaco_config_misc:verify_strict_uint(WaitFor) and
     megaco_config_misc:verify_strict_uint(Factor)  and
     megaco_config_misc:verify_strict_int(Incr)     and
     verify_max_retries(MaxRetries));
verify(Timer) ->
    megaco_config_misc:verify_uint(Timer).

verify_max_retries(infinity_restartable) ->
    true;
verify_max_retries(Val) ->
    megaco_config_misc:verify_uint(Val).


%%-----------------------------------------------------------------


return_incr(#megaco_incr_timer{wait_for    = WaitFor,
			       max_retries = infinity} = Timer) ->
    {WaitFor, Timer};

return_incr(#megaco_incr_timer{wait_for    = WaitFor,
			       max_retries = infinity_restartable} = Timer) ->
    {WaitFor, {Timer, timeout}};

return_incr(#megaco_incr_timer{wait_for    = WaitFor,
			       max_retries = Int} = Timer) 
  when is_integer(Int) and (Int > 0) ->
    {WaitFor, Timer};

return_incr(#megaco_incr_timer{wait_for    = WaitFor,
			       max_retries = 0} = _Timer) ->
    {WaitFor, timeout}.


decr(infinity = V)             -> V;
decr(infinity_restartable = V) -> V;
decr(Int) when is_integer(Int) -> Int - 1.


