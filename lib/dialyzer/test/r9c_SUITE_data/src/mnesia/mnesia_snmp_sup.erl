%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_snmp_sup.erl,v 1.1 2008/12/17 09:53:39 mikpe Exp $
%%
-module(mnesia_snmp_sup).

-behaviour(supervisor).

-export([start/0, init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% top supervisor callback functions

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sub supervisor callback functions

init([]) ->
    Flags = {simple_one_for_one, 0, timer:hours(24)}, % Trust the top supervisor
    MFA = {mnesia_snmp_hook, start, []},
    Modules = [?MODULE, mnesia_snmp_hook, supervisor],
    KillAfter = mnesia_kernel_sup:supervisor_timeout(timer:seconds(3)),
    Workers = [{?MODULE, MFA, transient, KillAfter, worker, Modules}],
    {ok, {Flags, Workers}}.
