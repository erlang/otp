%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%% the log level is used as argument to any CT logging function
-define(MIN_IMPORTANCE, 0 ).
-define(LOW_IMPORTANCE, 25).
-define(STD_IMPORTANCE, 50).
-define(HI_IMPORTANCE,  75).
-define(MAX_IMPORTANCE, 99).

%% verbosity thresholds to filter out logging printouts
-define(MIN_VERBOSITY, 0  ).  %% turn logging off
-define(LOW_VERBOSITY, 25 ).
-define(STD_VERBOSITY, 50 ).
-define(HI_VERBOSITY,  75 ).
-define(MAX_VERBOSITY, 100).

%% name of process executing the CT Hook init and terminate function
-define(CT_HOOK_INIT_PROCESS, ct_util_server).
-define(CT_HOOK_TERMINATE_PROCESS, ct_util_server).

%% Backward compatibility for test_server test suites.
%% DO NOT USE IN NEW TEST SUITES.
-define(line,).
-define(t,test_server).
-define(config,test_server:lookup_config).
