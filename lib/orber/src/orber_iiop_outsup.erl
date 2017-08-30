%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_iiop_outsup.erl
%% 
%% Description:
%%    This file contains the outgoing IIOP communication supervisor which
%%    holds all active "proxies"
%%
%%-----------------------------------------------------------------
-module(orber_iiop_outsup).

-behaviour(supervisor).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/2, connect/7]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/2
%%-----------------------------------------------------------------
start(sup, Opts) ->
    supervisor:start_link({local, orber_iiop_outsup}, orber_iiop_outsup,
			  {sup, Opts});
start(_A1, _A2) -> 
    ok.


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({sup, _Opts}) ->
    SupFlags = {simple_one_for_one, 500, 100},
    ChildSpec = [
		 {name2, {orber_iiop_outproxy, start, []}, temporary, 
		  10000, worker, [orber_iiop_outproxy]}
		],
    {ok, {SupFlags, ChildSpec}};
init(_Opts) ->
    {ok, []}.


%%-----------------------------------------------------------------
%% Func: terminate/1
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: connect/6
%%-----------------------------------------------------------------
connect(Host, Port, SocketType, SocketOptions, Parent, Key, NewKey) ->
    supervisor:start_child(orber_iiop_outsup, 
			   [{connect, Host, Port, SocketType, 
			     SocketOptions, Parent, Key, NewKey}]).

