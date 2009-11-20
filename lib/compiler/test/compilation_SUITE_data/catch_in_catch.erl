%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2009. All Rights Reserved.
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
-module(catch_in_catch).

-export([?MODULE/0,do_start/1]).

?MODULE() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, do_start, [x]),
    receive
	{'EXIT',Pid,good_exit} -> ok;
	Other ->
	    io:format("Unexpected: ~p\n", [Other]),
	    error
    after 32000 ->
	    io:format("No message received\n"),
	    error
    end.

do_start(Param) ->
    init(Param),
    exit(good_exit).

init(Param) ->
    process_flag(trap_exit, true),
    %% The catches were improperly nested, causing a "No catch found" crash.
    (catch begin
           foo(Param),
           (catch exit(bar))
       end
    ),
    ignore.

foo(_) ->
    ok.
