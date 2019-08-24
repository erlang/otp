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

%% Default disk log option values
-define(DISK_LOG_TYPE, wrap).
-define(DISK_LOG_MAX_NO_FILES, 10).
-define(DISK_LOG_MAX_NO_BYTES, 1048576).

%%%-----------------------------------------------------------------
%%% Utility macros

-define(name_to_reg_name(MODULE,Name),
        list_to_atom(lists:concat([MODULE,"_",Name]))).

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
-endif.
