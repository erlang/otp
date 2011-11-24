%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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

%%
-module(cleanup).

-export([all/0,groups/0,init_per_group/2,end_per_group/2, cleanup/1]).

-include_lib("test_server/include/test_server.hrl").

all() -> 
[cleanup].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


cleanup(suite) -> [];
cleanup(_) ->
    ?line Mods = int:interpreted(),
    ?line ok = int:n(Mods),
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
