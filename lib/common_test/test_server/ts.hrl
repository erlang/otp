%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

%% Defines ripped out from test_server (these must remain the same
%% as in test_server).

-define(logdir_ext, ".logs").
-define(suitelog_name, "suite.log").
-define(last_file, "last_name").
-define(last_link, "last_link").
-define(last_test, "last_test").
-define(run_summary, "suite.summary").
-define(cover_total,"total_cover.log").
-define(variables, "variables").
-define(cross_variables, "variables-cross").
-define(LF, [10]).                              % Newline in VxWorks script
-define(CHAR_PER_LINE, 60).                     % Characters per VxWorks script building line
-define(CROSS_COOKIE, "cross").                 % cookie used when cross platform testing
-define(TS_PORT, 7887).
-define(TEST_SERVER_SCRIPT, "test_server_vx.script").

