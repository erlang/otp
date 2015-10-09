%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(random_kill_SUITE).
-compile([export_all]).
%%-define(line_trace,1).
-include("test_server.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) -> [run].

-define(iterations,25). %% Kill this many processes, 
                        %% possibly with reboots in between

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

run(suite) -> [];
run(Config) ->
    registered(?iterations).

registered(0) ->
    ok;
registered(N) ->
    random:seed(3461*N,1159*N,351*N),
    Pid = select_victim(registered),
    test_server:resume_point(?MODULE,registered,[N-1]),
    test_server:format("About to kill pid ~p (~p)\n~p",
		       [Pid,process_info(Pid,registered_name),info(Pid)]),
    %%exit(Pid,kill),
    registered(N-1).

info(Pid) ->
    Rest0 = tl(pid_to_list(Pid)),
    {P1,Rest1} = get_until($.,Rest0),
    {P2,Rest2} = get_until($.,Rest1),
    {P3,_}     = get_until($>,Rest2),
    c:i(list_to_integer(P1),list_to_integer(P2),list_to_integer(P3)).

get_until(Ch,L) ->
    get_until(Ch,L,[]).
get_until(Ch,[],Acc) ->
    {lists:reverse(Acc),[]};
get_until(Ch,[Ch|T],Acc) ->
    {lists:reverse(Acc),T};
get_until(Ch,[H|T],Acc) ->
    get_until(Ch,T,[H|Acc]).

select_victim(registered) ->
    Pids =
	lists:map(fun(Server)-> whereis(Server) end,registered()),
    ImmunePids =
	[self()|lists:map(fun(Job)-> element(2,Job) end,test_server:jobs())],
    SuitablePids =
	lists:filter(fun(Pid)-> case lists:member(Pid,ImmunePids) of
				    true -> false;
				    false -> true
				end
		     end, Pids),
    Selected = random:uniform(length(SuitablePids)),
    io:format("Selected ~p if ~p",[Selected,length(SuitablePids)]),
    lists:nth(Selected,SuitablePids).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					 
