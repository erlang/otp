%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2017. All Rights Reserved.
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
-module(cth_log_redirect).

%%% @doc Common Test Framework functions handling test specifications.
%%%
%%% <p>This module redirects sasl and error logger info to common test log.</p>
%%% @end


%% CTH Callbacks
-export([id/1, init/2,
	 pre_init_per_suite/3, pre_end_per_suite/3, post_end_per_suite/4,
	 pre_init_per_group/4, post_init_per_group/5,
	 pre_end_per_group/4, post_end_per_group/5,
	 pre_init_per_testcase/4, post_init_per_testcase/5,
	 pre_end_per_testcase/4, post_end_per_testcase/5]).

%% Event handler Callbacks
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/1, terminate/2, code_change/3]).

%% Other
-export([handle_remote_events/1]).

-include("ct.hrl").

-behaviour(gen_event).

-record(eh_state, {log_func,
		   curr_suite,
		   curr_group,
		   curr_func,
		   parallel_tcs = false,
		   handle_remote_events = false}).

id(_Opts) ->
    ?MODULE.

init(?MODULE, _Opts) ->
    ct_util:mark_process(),
    error_logger:add_report_handler(?MODULE),
    tc_log_async.

pre_init_per_suite(Suite, Config, State) ->
    set_curr_func({Suite,init_per_suite}, Config),
    {Config, State}.

pre_end_per_suite(Suite, Config, State) ->
    set_curr_func({Suite,end_per_suite}, Config),
    {Config, State}.

post_end_per_suite(_Suite, Config, Return, State) ->
    set_curr_func(undefined, Config),
    {Return, State}.

pre_init_per_group(_Suite, Group, Config, State) ->
    set_curr_func({group,Group,init_per_group}, Config),
    {Config, State}.

post_init_per_group(_Suite, Group, Config, Result, tc_log_async) when is_list(Config) ->
    case lists:member(parallel,proplists:get_value(
				 tc_group_properties,Config,[])) of
	true ->
	    {Result, {set_log_func(tc_log),Group}};
	false ->
	    {Result, tc_log_async}
    end;
post_init_per_group(_Suite, _Group, _Config, Result, State) ->
    {Result, State}.

pre_init_per_testcase(_Suite, TC, Config, State) ->
    set_curr_func(TC, Config),
    {Config, State}.

post_init_per_testcase(_Suite, _TC, _Config, Return, State) ->
    {Return, State}.

pre_end_per_testcase(_Suite, _TC, Config, State) ->
    {Config, State}.

post_end_per_testcase(_Suite, _TC, _Config, Result, State) ->
    %% Make sure that the event queue is flushed
    %% before ending this test case.
    gen_event:call(error_logger, ?MODULE, flush, 300000),
    {Result, State}.

pre_end_per_group(_Suite, Group, Config, {tc_log, Group}) ->
    set_curr_func({group,Group,end_per_group}, Config),
    {Config, set_log_func(tc_log_async)};
pre_end_per_group(_Suite, Group, Config, State) ->
    set_curr_func({group,Group,end_per_group}, Config),
    {Config, State}.

post_end_per_group(_Suite, _Group, Config, Return, State) ->
    set_curr_func({group,undefined}, Config),
    {Return, State}.

%% Copied and modified from sasl_report_tty_h.erl
init(_Type) ->
    {ok, #eh_state{log_func = tc_log_async}}.

handle_event({_Type,GL,_Msg}, #eh_state{handle_remote_events = false} = State)
  when node(GL) /= node() ->
    {ok, State};
handle_event(Event, #eh_state{log_func = LogFunc} = State) ->
    case whereis(sasl_sup) of
	undefined ->
	    sasl_not_started;
	_Else ->
	    {ok, ErrLogType} = application:get_env(sasl, errlog_type),
	    SReport = sasl_report:format_report(group_leader(), ErrLogType,
						tag_event(Event, local)),
	    if is_list(SReport) ->
		    SaslHeader = format_header(State),
		    case LogFunc of
			tc_log ->
			    ct_logs:tc_log(sasl, ?STD_IMPORTANCE,
					   SaslHeader, SReport, [], []);
			tc_log_async ->
			    ct_logs:tc_log_async(sasl, ?STD_IMPORTANCE,
						 SaslHeader, SReport, [])
		    end;
	       true -> %% Report is an atom if no logging is to be done
		    ignore
	    end
    end,
    %% note that error_logger (unlike sasl) expects UTC time
    EReport = error_logger_tty_h:write_event(
		tag_event(Event, utc), io_lib),
    if is_list(EReport) ->
	    ErrHeader = format_header(State),
	    case LogFunc of
		tc_log ->
		    ct_logs:tc_log(error_logger, ?STD_IMPORTANCE,
				   ErrHeader, EReport, [], []);
		tc_log_async ->
		    ct_logs:tc_log_async(error_logger, ?STD_IMPORTANCE,
					 ErrHeader, EReport, [])
	    end;
       true -> %% Report is an atom if no logging is to be done
	    ignore
    end,
    {ok, State}.

handle_info({'EXIT',User,killed}, State) ->
    case whereis(user) of
	%% init:stop/1/2 has been called, let's finish!
	undefined ->
	    remove_handler;
	User ->
	    remove_handler;
	_ ->
	    {ok,State}
    end;

handle_info(_, State) -> 
    {ok,State}.

handle_call(flush,State) ->
    {ok, ok, State};

handle_call({set_curr_func,{group,Group,Conf},Config},
	    State) when is_list(Config) ->
    Parallel = case proplists:get_value(tc_group_properties, Config) of
		   undefined -> false;
		   Props -> lists:member(parallel, Props)
	       end,
    {ok, ok, State#eh_state{curr_group = Group,
			    curr_func = Conf,
			    parallel_tcs = Parallel}};
handle_call({set_curr_func,{group,Group,Conf},_SkipOrFail}, State) ->
    {ok, ok, State#eh_state{curr_group = Group,
			    curr_func = Conf,
			    parallel_tcs = false}};
handle_call({set_curr_func,{group,undefined},_Config}, State) ->
    {ok, ok, State#eh_state{curr_group = undefined,
			    curr_func = undefined,
			    parallel_tcs = false}};
handle_call({set_curr_func,{Suite,Conf},_Config}, State) ->
    {ok, ok, State#eh_state{curr_suite = Suite,
			    curr_func = Conf,
			    parallel_tcs = false}};
handle_call({set_curr_func,undefined,_Config}, State) ->
    {ok, ok, State#eh_state{curr_suite = undefined,
			    curr_func = undefined,
			    parallel_tcs = false}};
handle_call({set_curr_func,TC,_Config}, State) ->
    {ok, ok, State#eh_state{curr_func = TC}};

handle_call({set_logfunc,NewLogFunc}, State) ->
    {ok, NewLogFunc, State#eh_state{log_func = NewLogFunc}};

handle_call({handle_remote_events,Bool}, State) ->
    {ok, ok, State#eh_state{handle_remote_events = Bool}};

handle_call(_Query, _State) ->
    {error, bad_query}.

terminate(_) ->
    error_logger:delete_report_handler(?MODULE),
    [].

terminate(_Arg, _State) ->
    ok.

tag_event(Event, utc) ->
    {calendar:universal_time(), Event};
tag_event(Event, _) ->
    {calendar:local_time(), Event}.

set_curr_func(CurrFunc, Config) ->
    gen_event:call(error_logger, ?MODULE, {set_curr_func, CurrFunc, Config}).

set_log_func(Func) ->
    gen_event:call(error_logger, ?MODULE, {set_logfunc, Func}).

handle_remote_events(Bool) ->
    gen_event:call(error_logger, ?MODULE, {handle_remote_events, Bool}).

%%%-----------------------------------------------------------------

format_header(#eh_state{curr_suite = undefined,
			curr_group = undefined,
			curr_func = undefined}) ->
    io_lib:format("System report", []);

format_header(#eh_state{curr_suite = Suite,
			curr_group = undefined,
			curr_func = undefined}) ->
    io_lib:format("System report during ~w", [Suite]);

format_header(#eh_state{curr_suite = Suite,
			curr_group = undefined,
			curr_func = TcOrConf}) ->
    io_lib:format("System report during ~w:~tw/1",
		  [Suite,TcOrConf]);

format_header(#eh_state{curr_suite = Suite,
			curr_group = Group,
			curr_func = Conf}) when Conf == init_per_group;
						Conf == end_per_group ->
    io_lib:format("System report during ~w:~w/2 for ~tw",
		  [Suite,Conf,Group]);

format_header(#eh_state{curr_suite = Suite,
			curr_group = Group,
			parallel_tcs = true}) ->
    io_lib:format("System report during ~tw in ~w",
		  [Group,Suite]);

format_header(#eh_state{curr_suite = Suite,
			curr_group = Group,
			curr_func = TC}) ->
    io_lib:format("System report during ~w:~tw/1 in ~tw",
		  [Suite,TC,Group]).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
