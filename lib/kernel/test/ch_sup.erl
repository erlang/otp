%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(ch_sup).
-behaviour(supervisor).

%% External exports
-export([start/2, start_phase/3, stop/1, config_change/3]).

%% Internal exports
-export([init/1]).

start(_Type, {_AppN, Low, High}) ->
    Name = list_to_atom(lists:concat([ch_sup, Low])),
    {ok, P} = supervisor:start_link({local, Name}, ch_sup,
				    lists:seq(Low, High)),
    {ok, P, []}.

stop(_) -> ok.
 
start_phase(_Phase, _Type, _Args) ->
    ok.

init(Nos) ->
    SupFlags = {one_for_one, 12, 60},
    Chs = lists:map(fun(No) ->
			   {list_to_atom(lists:concat([ch,No])),
			    {ch, start_link, [{ch, No}]},
			    permanent, 2000, worker, [ch]}
		    end,
		    Nos),
    {ok, {SupFlags, Chs}}.

config_change(Changed, New, Removed) ->
    (catch global:send(conf_change,{cc, [{Changed, New, Removed}]})),
    ok.
