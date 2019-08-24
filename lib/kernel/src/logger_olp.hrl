%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2015. All Rights Reserved.
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

%%%-----------------------------------------------------------------
%%% Overload protection configuration

%%! *** NOTE *** 
%%! It's important that:
%%! SYNC_MODE_QLEN =< DROP_MODE_QLEN =< FLUSH_QLEN
%%! and that DROP_MODE_QLEN >= 2.
%%! Otherwise the process could end up in drop mode with no new
%%! log requests to process. This would cause all future requests
%%! to be dropped (no switch to async mode would ever take place).

%% This specifies the message_queue_len value where the log
%% requests switch from asynchronous casts to synchronous calls.
-define(SYNC_MODE_QLEN, 10).
%% Above this message_queue_len, log requests will be dropped,
%% i.e. no log requests get sent to the process.
-define(DROP_MODE_QLEN, 200).
%% Above this message_queue_len, the process will flush its mailbox
%% and only leave this number of messages in it.
-define(FLUSH_QLEN, 1000).

%% Never flush more than this number of messages in one go, or the
%% process will be unresponsive for seconds (keep this number as large
%% as possible or the mailbox could grow large).
-define(FLUSH_MAX_N, 5000).

%% BURST_LIMIT_MAX_COUNT is the max number of log requests allowed
%% to be written within a BURST_LIMIT_WINDOW_TIME time frame.
-define(BURST_LIMIT_ENABLE, true).
-define(BURST_LIMIT_MAX_COUNT, 500).
-define(BURST_LIMIT_WINDOW_TIME, 1000).

%% This enables/disables the feature to automatically terminate the
%% process if it gets too loaded (and can't keep up).
-define(OVERLOAD_KILL_ENABLE, false).
%% If the message_queue_len goes above this size even after
%% flushing has been performed, the process is terminated.
-define(OVERLOAD_KILL_QLEN, 20000).
%% If the memory usage exceeds this level, the process is terminated.
-define(OVERLOAD_KILL_MEM_SIZE, 3000000).

%% This is the default time to wait before restarting and accepting
%% new requests. The value 'infinity' disables restarts.
-define(OVERLOAD_KILL_RESTART_AFTER, 5000).

%% This is the time in milliseconds after last load message received
%% that we notify the callback about being idle.
-define(IDLE_DETECT_TIME, 100).

%%%-----------------------------------------------------------------
%%% Overload protection macros

-define(timestamp(), erlang:monotonic_time(microsecond)).

-define(get_mode(Tid),
        case ets:lookup(Tid, mode) of
            [{mode,M}] -> M;
            _          -> async
        end).

-define(set_mode(Tid, M),
        begin ets:insert(Tid, {mode,M}), M end).

-define(change_mode(Tid, M0, M1),
        if M0 == M1 ->
                M0;
           true ->
                ets:insert(Tid, {mode,M1}),
                M1
        end).

-define(max(X1, X2),
        if 
            X2 == undefined -> X1;
            X2 > X1 -> X2;
            true -> X1
        end).

-define(diff_time(OS_T1, OS_T0), OS_T1-OS_T0).

%%%-----------------------------------------------------------------
%%% These macros enable statistics counters in the state of the
%%% process, which is useful for analysing the overload protection
%%% behaviour. These counters should not be included in code to be
%%% officially released (as some counters will grow very large over
%%% time).

%% -define(SAVE_STATS, true).
-ifdef(SAVE_STATS).
  -define(merge_with_stats(STATE),
          begin
              TIME = ?timestamp(),
              STATE#{start => TIME, time => {TIME,0},
                     flushes => 0, flushed => 0, drops => 0,
                     burst_drops => 0, casts => 0, calls => 0,
                     writes => 0, max_qlen => 0, max_time => 0,
                     max_mem => 0, freq => {TIME,0,0}} end).

  -define(update_max_qlen(QLEN, STATE),
          begin #{max_qlen := QLEN0} = STATE,
                STATE#{max_qlen => ?max(QLEN0,QLEN)} end).

  -define(update_max_mem(MEM, STATE),
          begin #{max_mem := MEM0} = STATE,
                STATE#{max_mem => ?max(MEM0,MEM)} end).

  -define(update_calls_or_casts(CALL_OR_CAST, INC, STATE),
          case CALL_OR_CAST of
              cast ->
                  #{casts := CASTS0} = STATE,
                  STATE#{casts => CASTS0+INC};
              call ->
                  #{calls := CALLS0} = STATE,
                  STATE#{calls => CALLS0+INC}
          end).

  -define(update_max_time(TIME, STATE),
          begin #{max_time := TIME0} = STATE,
                STATE#{max_time => ?max(TIME0,TIME)} end).

  -define(update_other(OTHER, VAR, INCVAL, STATE),
          begin #{OTHER := VAR} = STATE,
                STATE#{OTHER => VAR+INCVAL} end).

  -define(update_freq(TIME,STATE),
          begin
              case STATE of
                  #{freq := {START, 49, _}} ->
                      STATE#{freq => {TIME, 0, trunc(1000000*50/(?diff_time(TIME,START)))}};
                  #{freq := {START, N, FREQ}} ->
                      STATE#{freq => {START, N+1, FREQ}}
              end end).

  -define(update_time(TIME,STATE),
          begin #{start := START} = STATE,
                STATE#{time => {TIME,trunc((?diff_time(TIME,START))/1000000)}} end).

-else.                                          % DEFAULT!
  -define(merge_with_stats(STATE), STATE).
  -define(update_max_qlen(_QLEN, STATE), STATE).
  -define(update_max_mem(_MEM, STATE), STATE).
  -define(update_calls_or_casts(_CALL_OR_CAST, _INC, STATE), STATE).
  -define(update_max_time(_TIME, STATE), STATE).
  -define(update_other(_OTHER, _VAR, _INCVAL, STATE), STATE).
  -define(update_freq(_TIME, STATE), STATE).
  -define(update_time(_TIME, STATE), STATE).
-endif.

%%%-----------------------------------------------------------------
%%% These macros enable callbacks that make it possible to analyse the
%%% overload protection behaviour from outside the process (including
%%% dropped requests on the client side). An external callback module
%%% (?OBSERVER_MOD) is required which is not part of the kernel
%%% application. For this reason, these callbacks should not be
%%% included in code to be officially released.

%%-define(OBSERVER_MOD, logger_test).
-ifdef(OBSERVER_MOD).
  -define(start_observation(NAME), ?OBSERVER:start_observation(NAME)).
  -define(observe(NAME,EVENT), ?OBSERVER:observe(NAME,EVENT)).

-else.                                          % DEFAULT!
  -define(start_observation(_NAME), ok).
  -define(observe(_NAME,_EVENT), ok).
-endif.
