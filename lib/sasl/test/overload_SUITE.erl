%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011. All Rights Reserved.
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

-module(overload_SUITE).
-include("test_server.hrl").

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> [info, set_config_data, set_env_vars, request, timeout].
all(suite) -> all().

init_per_testcase(_Case,Config) ->
    restart_sasl(),
    Config.

end_per_testcase(Case,Config) ->
    try apply(?MODULE,Case,[cleanup,Config])
    catch error:undef -> ok
    end,
    ok.

%%%-----------------------------------------------------------------
info(suite) -> [];
info(_Config) ->
    ?line Info = overload:get_overload_info(),
    ?line [{total_intensity,0.0},
	   {accept_intensity,0.0},
	   {max_intensity,0.8},
	   {weight,0.1},
	   {total_requests,0},
	   {accepted_requests,0}] = Info.

%%%-----------------------------------------------------------------
set_config_data(suite) -> [];
set_config_data(_Config) ->
    ?line InfoDefault = overload:get_overload_info(),
    ?line ok = check_info(0.8,0.1,InfoDefault),
    ?line ok = overload:set_config_data(0.5,0.4),
    ?line Info1 = overload:get_overload_info(),
    ?line ok = check_info(0.5,0.4,Info1),
    ok.

%%%-----------------------------------------------------------------
set_env_vars(suite) -> [];
set_env_vars(_Config) ->
    ?line InfoDefault = overload:get_overload_info(),
    ?line ok = check_info(0.8,0.1,InfoDefault),
    ?line ok = application:set_env(sasl,overload_max_intensity,0.5),
    ?line ok = application:set_env(sasl,overload_weight,0.4),
    ?line ok = application:stop(sasl),
    ?line ok = application:start(sasl),
    ?line Info1 = overload:get_overload_info(),
    ?line ok = check_info(0.5,0.4,Info1),
    ok.
set_env_vars(cleanup,_Config) ->
    application:unset_env(sasl,overload_max_intensity),
    application:unset_env(sasl,overload_weight),
    ok.

%%%-----------------------------------------------------------------
request(suite) -> [];
request(_Config) ->
    %% Find number of request that can be done with default settings
    %% and no delay
    ?line overload:set_config_data(0.8, 0.1),
    ?line NDefault = do_many_requests(0),
    ?line restart_sasl(),
    ?line ?t:format("NDefault: ~p",[NDefault]),
 
    %% Check that the number of requests increases when max_intensity
    %% increases
    ?line overload:set_config_data(2, 0.1),
    ?line NLargeMI = do_many_requests(0),
    ?line restart_sasl(),
    ?line ?t:format("NLargeMI: ~p",[NLargeMI]),
    ?line true = NLargeMI > NDefault,

    %% Check that the number of requests decreases when weight
    %% increases
    ?line overload:set_config_data(0.8, 1),
    ?line NLargeWeight = do_many_requests(0),
    ?line restart_sasl(),
    ?line ?t:format("NLargeWeight: ~p",[NLargeWeight]),
    ?line true = NLargeWeight < NDefault,

    %% Check that number of requests increases when delay between
    %% requests increases.
    %% (Keeping same config and comparing to large weight in order to
    %% minimize the time needed for this case.)
    ?line overload:set_config_data(0.8, 1),
    ?line NLargeTime = do_many_requests(500),
    ?line restart_sasl(),
    ?line ?t:format("NLargeTime: ~p",[NLargeTime]),
    ?line true = NLargeTime > NLargeWeight,
    ok.

%%%-----------------------------------------------------------------
timeout(suite) -> [];
timeout(_Config) ->
    ?line overload:set_config_data(0.8, 1),
    ?line _N = do_many_requests(0),
    
    %% Check that the overload alarm is raised
    ?line [{overload,_}] = alarm_handler:get_alarms(),

    %% Fake a clear timeout in overload.erl and check that, since it
    %% came very soon after the overload situation, the alarm is not
    %% cleared
    ?line overload ! timeout,
    ?line timer:sleep(1000),
    ?line [{overload,_}] = alarm_handler:get_alarms(),

    %% A bit later, try again and check that this time the alarm is
    %% cleared
    ?line overload ! timeout,
    ?line timer:sleep(1000),
    ?line [] = alarm_handler:get_alarms(),

    ok.


%%%-----------------------------------------------------------------
%%% INTERNAL FUNCTIONS

%%%-----------------------------------------------------------------
%%% Call overload:request/0 up to 30 times with the given time delay
%%% between. Stop when 'reject' is returned.
do_many_requests(T) ->
    30 - do_requests(30,T).

do_requests(0,_) ->
    ?t:fail(never_rejected);
do_requests(N,T) ->
    case overload:request() of
	accept ->
	    timer:sleep(T),
	    do_requests(N-1,T);
	reject ->
	    N
    end.

%%%-----------------------------------------------------------------
%%% Restart the sasl application
restart_sasl() ->
    application:stop(sasl),
    application:start(sasl),
    ok.

%%%-----------------------------------------------------------------
%%% Check that max_intensity and weight is set as expected
check_info(MI,W,Info) -> 
    case {lists:keyfind(max_intensity,1,Info), lists:keyfind(weight,1,Info)} of
	{{_,MI},{_,W}} -> ok;
	_ -> ?t:fail({unexpected_info,MI,W,Info})
    end.
    

