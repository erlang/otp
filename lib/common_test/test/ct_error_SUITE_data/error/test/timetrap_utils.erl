%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2012. All Rights Reserved.
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
