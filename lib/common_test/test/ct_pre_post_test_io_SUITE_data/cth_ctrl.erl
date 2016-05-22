%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(cth_ctrl).

-export([proceed/0,
	 init/2, terminate/1]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% API
%%%===================================================================

proceed() ->
    ?MODULE ! proceed,
    ok.

%%--------------------------------------------------------------------
%% Hook functions
%%--------------------------------------------------------------------
init(_Id, _Opts) ->
    case lists:keyfind(sasl, 1, application:which_applications()) of
	false ->
	    exit(sasl_not_started);
	_Else ->
	    ok
    end,
    WhoAmI = self(),
    WhoAmI = whereis(?CT_HOOK_INIT_PROCESS),
    DispPid = spawn_link(fun() -> dispatcher(WhoAmI) end),
    register(?MODULE, DispPid),
    ct:pal("~n~n+++ Startup of ~w on ~p finished, "
	   "call ~w:proceed() to run tests...~n",
	   [?MODULE,node(),?MODULE]),
    start_external_logger(cth_logger),
    receive
	{?MODULE,proceed} -> ok
    after
	10000 -> ok
    end,
    {ok,[],ct_last}.

terminate(_State) ->
    WhoAmI = whereis(?CT_HOOK_TERMINATE_PROCESS),
    WhoAmI = self(),
    ct:pal("~n~n+++ Tests finished, call ~w:proceed() to shut down...~n",
	   [?MODULE]),
    receive
	{?MODULE,proceed} -> ok
    after
       10000 -> ok
    end,
    stop_external_logger(cth_logger),
    stop_dispatcher(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_external_logger(Name) ->
    case whereis(Name) of
	undefined -> ok;
	Pid -> exit(Pid, kill)
    end,
    spawn(fun() -> init_logger(Name) end).

stop_external_logger(Name) ->
    catch exit(whereis(Name), kill).

init_logger(Name) ->
    register(Name, self()),
    logger_loop(1).

logger_loop(N) ->
    ct:log("Logger iteration: ~p", [N]),
    error_logger:error_report(N),
    timer:sleep(100),
    logger_loop(N+1).

%%%-----------------------------------------------------------------

dispatcher(SendTo) ->
    receive Msg -> SendTo ! {?MODULE,Msg} end,
    dispatcher(SendTo).

stop_dispatcher() ->
    catch exit(whereis(?MODULE), kill).
	    

