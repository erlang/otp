%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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

%% To able to generate nice crash reports we need a catch on the highest level.
%% This code can't be purged so a code change is not possible.
%% And hence this a simple module.

-module(mnesia_sp).

-export([init_proc/4]).

init_proc(Who, Mod, Fun, Args) ->
    mnesia_lib:verbose("~p starting: ~p~n", [Who, self()]),
    case catch apply(Mod, Fun, Args) of
	{'EXIT', Reason} ->
	    mnesia_monitor:terminate_proc(Who, Reason, Args),
	    exit(Reason);
	Other ->
	    Other
    end.




