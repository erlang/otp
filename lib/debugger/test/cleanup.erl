%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(cleanup).

-export([all/0,groups/0,init_per_group/2,end_per_group/2, cleanup/1]).

-include_lib("common_test/include/ct.hrl").

all() -> 
    [cleanup].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cleanup(_) ->
    Mods = int:interpreted(),
    ok = int:n(Mods),
    case whereis(interpret) of
	undefined ->
	    ok;
	Pid ->
	    exit(Pid, kill)
    end,
    case whereis(int_db) of
	undefined ->
	    ok;
	Pid2 ->
	    exit(Pid2, kill)
    end,
    ok.
