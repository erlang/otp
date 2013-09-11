%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").

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
