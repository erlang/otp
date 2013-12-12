%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
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
	10000 ->
	    ok
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
       10000 ->
	    ok
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
    timer:sleep(250),
    logger_loop(N+1).

%%%-----------------------------------------------------------------

dispatcher(SendTo) ->
    receive Msg -> SendTo ! {?MODULE,Msg} end,
    dispatcher(SendTo).

stop_dispatcher() ->
    catch exit(whereis(?MODULE), kill).
	    

