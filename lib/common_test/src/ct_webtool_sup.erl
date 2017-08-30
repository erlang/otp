%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(ct_webtool_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0,stop/1]).

%% supervisor callbacks
-export([init/1]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local,ct_websup},ct_webtool_sup, []).

stop(Pid)->
   exit(Pid,normal).
%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init(_StartArgs) ->
    %%Child1 = 
    %%Child2 ={webcover_backend,{webcover_backend,start_link,[]},permanent,2000,worker,[webcover_backend]},
    %%{ok,{{simple_one_for_one,5,10},[Child1]}}.
    {ok,{{one_for_one,100,10},[]}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------



















