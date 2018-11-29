
%%%-----------------------------------------------------------------
%%% Overload protection configuration

%%! *** NOTE *** 
%%! It's important that:
%%! SYNC_MODE_QLEN =< DROP_MODE_QLEN =< FLUSH_QLEN
%%! and that DROP_MODE_QLEN >= 2.
%%! Otherwise the handler could end up in drop mode with no new
%%! log requests to process. This would cause all future requests
%%! to be dropped (no switch to async mode would ever take place).

%% This specifies the message_queue_len value where the log
%% requests switch from asynchronous casts to synchronous calls.
-define(SYNC_MODE_QLEN, 10).
%% Above this message_queue_len, log requests will be dropped,
%% i.e. no log requests get sent to the handler process.
-define(DROP_MODE_QLEN, 200).
%% Above this message_queue_len, the handler process will flush
%% its mailbox and only leave this number of messages in it.
-define(FLUSH_QLEN, 1000).

%% Never flush more than this number of messages in one go,
%% or the handler will be unresponsive for seconds (keep this
%% number as large as possible or the mailbox could grow large).
-define(FLUSH_MAX_N, 5000).

%% BURST_LIMIT_MAX_COUNT is the max number of log requests allowed
%% to be written within a BURST_LIMIT_WINDOW_TIME time frame.
-define(BURST_LIMIT_ENABLE, true).
-define(BURST_LIMIT_MAX_COUNT, 500).
-define(BURST_LIMIT_WINDOW_TIME, 1000).

%% This enables/disables the feature to automatically get the
%% handler terminated if it gets too loaded (and can't keep up).
-define(OVERLOAD_KILL_ENABLE, false).
%% If the message_queue_len goes above this size even after
%% flushing has been performed, the handler is terminated.
-define(OVERLOAD_KILL_QLEN, 20000).
%% If the memory usage exceeds this level
-define(OVERLOAD_KILL_MEM_SIZE, 3000000).

%% This is the default time that the handler will wait before
%% restarting and accepting new requests. The value 'infinity'
%% disables restarts.
-define(OVERLOAD_KILL_RESTART_AFTER, 5000).
%%-define(OVERLOAD_KILL_RESTART_AFTER, infinity).

%% The handler sends asynchronous write requests to the process
%% controlling the i/o device, but every once in this interval
%% will the write request be synchronous, so that the i/o device
%% process doesn't get overloaded. This gives the handler time
%% to keep up with its mailbox in overload situations, even if
%% the i/o is slow.
-define(CONTROLLER_SYNC_INTERVAL, 20).
%% The handler will not perform a file sync operation if the
%% mailbox size is greater than this number. This is to ensure
%% the handler process doesn't get overloaded while waiting for
%% an expensive file sync operation to finish.
-define(FILESYNC_OK_QLEN, 2). 
%% Do a file/disk_log sync operation every integer() millisec
%% (if necessary) or set to 'no_repeat' to only do file sync when
%% the handler is idle. Note that file sync is not guaranteed to
%% happen automatically if this operation is disabled.
-define(FILESYNC_REPEAT_INTERVAL, 5000).
%%-define(FILESYNC_REPEAT_INTERVAL, no_repeat).

%% This is the time after last message received that we think/hope
%% that the handler has an empty mailbox (no new log request has
%% come in).
-define(IDLE_DETECT_TIME_MSEC, 100).
-define(IDLE_DETECT_TIME_USEC, 100000).

%% Default disk log option values
-define(DISK_LOG_TYPE, wrap).
-define(DISK_LOG_MAX_NO_FILES, 10).
-define(DISK_LOG_MAX_NO_BYTES, 1048576).

%%%-----------------------------------------------------------------
%%% Utility macros

-define(name_to_reg_name(MODULE,Name),
        list_to_atom(lists:concat([MODULE,"_",Name]))).

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

-define(min(X1, X2),
        if X2 == undefined -> X1;
           X2 < X1 -> X2;
           true -> X1
        end).

-define(max(X1, X2),
        if 
            X2 == undefined -> X1;
            X2 > X1 -> X2;
            true -> X1
        end).

-define(diff_time(OS_T1, OS_T0), OS_T1-OS_T0).

%%%-----------------------------------------------------------------
%%% The test hook macros make it possible to observe and manipulate
%%% internal handler functionality. When enabled, these macros will
%%% slow down execution and therefore should not be include in code
%%% to be officially released.

%%-define(TEST_HOOKS, true).
-ifdef(TEST_HOOKS).
  -define(TEST_HOOKS_TAB, logger_h_test_hooks).

  -define(init_test_hooks(),
          _ = case ets:whereis(?TEST_HOOKS_TAB) of
                  undefined -> ets:new(?TEST_HOOKS_TAB, [named_table,public]);
                  _         -> ok
              end,
          ets:insert(?TEST_HOOKS_TAB, {internal_log,{logger,internal_log}}),
          ets:insert(?TEST_HOOKS_TAB, {file_write,ok}),
          ets:insert(?TEST_HOOKS_TAB, {file_datasync,ok}),
          ets:insert(?TEST_HOOKS_TAB, {disk_log_write,ok}),
          ets:insert(?TEST_HOOKS_TAB, {disk_log_sync,ok})).

  -define(set_internal_log(MOD_FUNC),
          ets:insert(?TEST_HOOKS_TAB, {internal_log,MOD_FUNC})).

  -define(set_result(OPERATION, RESULT),
          ets:insert(?TEST_HOOKS_TAB, {OPERATION,RESULT})).

  -define(set_defaults(),
          ets:insert(?TEST_HOOKS_TAB, {internal_log,{logger,internal_log}}),
          ets:insert(?TEST_HOOKS_TAB, {file_write,ok}),
          ets:insert(?TEST_HOOKS_TAB, {file_datasync,ok}),
          ets:insert(?TEST_HOOKS_TAB, {disk_log_write,ok}),
          ets:insert(?TEST_HOOKS_TAB, {disk_log_sync,ok})).

  -define(internal_log(TYPE, TERM),
          try ets:lookup(?TEST_HOOKS_TAB, internal_log) of
              [{_,{LMOD,LFUNC}}] -> apply(LMOD, LFUNC, [TYPE,TERM]);
              _ -> logger:internal_log(TYPE, TERM)
          catch _:_ -> logger:internal_log(TYPE, TERM) end).

  -define(file_write(DEVICE, DATA),
          try ets:lookup(?TEST_HOOKS_TAB, file_write) of
              [{_,ok}]    -> file:write(DEVICE, DATA);
              [{_,ERROR}] -> ERROR
          catch _:_       -> file:write(DEVICE, DATA) end).

  -define(file_datasync(DEVICE),
          try ets:lookup(?TEST_HOOKS_TAB, file_datasync) of
              [{_,ok}]    -> file:datasync(DEVICE);
              [{_,ERROR}] -> ERROR
          catch _:_       -> file:datasync(DEVICE) end).

  -define(disk_log_write(LOG, MODE, DATA),
          try ets:lookup(?TEST_HOOKS_TAB, disk_log_write) of
              [{_,ok}]    -> disk_log_write(LOG, MODE, DATA);
              [{_,ERROR}] -> ERROR
          catch _:_       -> disk_log_write(LOG, MODE, DATA) end).

  -define(disk_log_sync(LOG),
          try ets:lookup(?TEST_HOOKS_TAB, disk_log_sync) of
              [{_,ok}]    -> disk_log:sync(LOG);
              [{_,ERROR}] -> ERROR
          catch _:_       -> disk_log:sync(LOG) end).

  -define(DEFAULT_CALL_TIMEOUT, 5000).

-else.                                          % DEFAULTS!
  -define(TEST_HOOKS_TAB, undefined).
  -define(init_test_hooks(), ok).
  -define(set_internal_log(_MOD_FUNC), ok).
  -define(set_result(_OPERATION, _RESULT), ok).
  -define(set_defaults(), ok).
  -define(internal_log(TYPE, TERM), logger:internal_log(TYPE, TERM)).
  -define(file_write(DEVICE, DATA), file:write(DEVICE, DATA)).
  -define(file_datasync(DEVICE), file:datasync(DEVICE)).
  -define(disk_log_write(LOG, MODE, DATA), disk_log_write(LOG, MODE, DATA)).
  -define(disk_log_sync(LOG), disk_log:sync(LOG)).
  -define(DEFAULT_CALL_TIMEOUT, 10000).
-endif.

%%%-----------------------------------------------------------------
%%% These macros enable statistics counters in the state of the
%%% handler which is useful for analysing the overload protection
%%% behaviour. These counters should not be included in code to be
%%% officially released (as some counters will grow very large
%%% over time).

%%-define(SAVE_STATS, true).
-ifdef(SAVE_STATS).
  -define(merge_with_stats(STATE),
          STATE#{flushes => 0, flushed => 0, drops => 0,
                 burst_drops => 0, casts => 0, calls => 0,
                 max_qlen => 0, max_time => 0}).

  -define(update_max_qlen(QLEN, STATE),
          begin #{max_qlen := QLEN0} = STATE,
                STATE#{max_qlen => ?max(QLEN0,QLEN)} end).

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
          
-else.                                          % DEFAULT!
  -define(merge_with_stats(STATE), STATE).
  -define(update_max_qlen(_QLEN, STATE), STATE).
  -define(update_calls_or_casts(_CALL_OR_CAST, _INC, STATE), STATE).
  -define(update_max_time(_TIME, STATE), STATE).
  -define(update_other(_OTHER, _VAR, _INCVAL, STATE), STATE).
-endif.

%%%-----------------------------------------------------------------
%%% These macros enable callbacks that make it possible to analyse
%%% the overload protection behaviour from outside the handler
%%% process (including dropped requests on the client side).
%%% An external callback module (?OBSERVER_MOD) is required which
%%% is not part of the kernel application. For this reason, these
%%% callbacks should not be included in code to be officially released.

%%-define(OBSERVER_MOD, logger_test).
-ifdef(OBSERVER_MOD).
  -define(start_observation(NAME), ?OBSERVER:start_observation(NAME)).
  -define(observe(NAME,EVENT), ?OBSERVER:observe(NAME,EVENT)).

-else.                                          % DEFAULT!
  -define(start_observation(_NAME), ok).
  -define(observe(_NAME,_EVENT), ok).
-endif.
%%! <---
