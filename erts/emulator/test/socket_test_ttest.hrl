%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2018-2018. All Rights Reserved.
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

-ifndef(socket_test_ttest).
-define(socket_test_ttest, true).

-define(TTEST_TAG,          42).
-define(TTEST_TYPE_REQUEST, 101).
-define(TTEST_TYPE_REPLY,   102).

-define(SECS(I), timer:seconds(I)).

-define(SLEEP(T), receive after T -> ok end).

-endif. % -ifdef(socket_test_ttest).
