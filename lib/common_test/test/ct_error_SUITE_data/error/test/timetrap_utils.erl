%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(timetrap_utils).

-export([timetrap_val/1,
	 timetrap_exit/1,
	 timetrap_timeout/2]).

timetrap_val(Val) ->
    Val.

timetrap_exit(Reason) ->
    exit(Reason).

timetrap_timeout(Sleep, Val) ->
    ct:sleep(Sleep),
    Val.
