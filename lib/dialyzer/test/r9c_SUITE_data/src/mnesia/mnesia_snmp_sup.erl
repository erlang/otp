%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
