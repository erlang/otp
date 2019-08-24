%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2018. All Rights Reserved.
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

%%% Common Test Framework functions handling test specifications.
%%%
%%% This module redirects sasl and error logger info to common test log.

%% CTH Callbacks
-export([id/1, init/2,
	 pre_init_per_suite/3, pre_end_per_suite/3, post_end_per_suite/4,
	 pre_init_per_group/4, post_init_per_group/5,
	 pre_end_per_group/4, post_end_per_group/5,
	 pre_init_per_testcase/4, post_init_per_testcase/5,
	 pre_end_per_testcase/4, post_end_per_testcase/5]).

%% Logger handler and gen_server callbacks
-export([log/2,
         init/1,
	 handle_cast/2, handle_call/3,
	 terminate/1, terminate/2]).

%% Other
-export([handle_remote_events/1]).

-include("ct.hrl").
-include("../../kernel/src/logger_internal.hrl").

-behaviour(gen_server).

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
    ok = start_log_handler(),
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
    gen_server:call(?MODULE, flush, 300000),
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

start_log_handler() ->
    case whereis(?MODULE) of
        undefined ->
            ChildSpec =
                #{id=>?MODULE,
                  start=>{gen_server,start_link,[{local,?MODULE},?MODULE,[],[]]},
                  restart=>transient,
                  shutdown=>2000,
                  type=>worker,
                  modules=>[?MODULE]},
            {ok,_} = supervisor:start_child(logger_sup,ChildSpec),
            ok;
        _Pid ->
            ok
    end,
    ok = logger:add_handler(?MODULE,?MODULE,
                            #{level=>info,
                              formatter=>{?DEFAULT_FORMATTER,
                                          ?DEFAULT_FORMAT_CONFIG}}).

init([]) ->
    {ok, #eh_state{log_func = tc_log_async}}.

log(#{msg:={report,Msg},meta:=#{domain:=[otp,sasl]}}=Log,Config) ->
    case whereis(sasl_sup) of
	undefined ->
	    ok; % sasl application is not started
	_Else ->
            Level =
                case application:get_env(sasl, errlog_type) of
                    {ok,error} ->
                        error;
                    {ok,_} ->
                        info;
                    undefined ->
                        info
                end,
            case Level of
                error ->
                    case Msg of
                        #{label:={_,progress}} ->
                            ok;
                        _ ->
                            do_log(add_log_category(Log,sasl),Config)
                    end;
                _ ->
                    do_log(add_log_category(Log,sasl),Config)
            end
    end;
log(#{meta:=#{domain:=[otp]}}=Log,Config) ->
    do_log(add_log_category(Log,error_logger),Config);
log(#{meta:=#{domain:=_}},_) ->
    ok;
log(Log,Config) ->
    do_log(add_log_category(Log,error_logger),Config).

add_log_category(#{meta:=Meta}=Log,Category) ->
    Log#{meta=>Meta#{?MODULE=>#{category=>Category}}}.

do_log(Log,Config) ->
    gen_server:call(?MODULE,{log,Log,Config}).

handle_cast(_, State) ->
    {noreply,State}.

handle_call({log,#{meta:=#{gl:=GL}},_}, _From,
            #eh_state{handle_remote_events=false}=State)
  when node(GL) /= node() ->
    {reply, ok, State};

handle_call({log,
             #{meta:=#{?MODULE:=#{category:=Category}}}=Log,
             #{formatter:={Formatter,FConfig}}},
            _From,
            #eh_state{log_func=LogFunc}=State) ->
    Header = format_header(State),
    String = Formatter:format(Log,FConfig),
    case LogFunc of
        tc_log ->
            ct_logs:tc_log(Category, ?STD_IMPORTANCE,
                           Header, "~ts", [String], []);
        tc_log_async ->
            ct_logs:tc_log_async(sasl, ?STD_IMPORTANCE,
                                 Header, "~ts", [String])
    end,
    {reply,ok,State};

handle_call(flush,_From,State) ->
    {reply, ok, State};

handle_call({set_curr_func,{group,Group,Conf},Config},_From,State)
  when is_list(Config) ->
    Parallel = case proplists:get_value(tc_group_properties, Config) of
		   undefined -> false;
		   Props -> lists:member(parallel, Props)
	       end,
    {reply, ok, State#eh_state{curr_group = Group,
                               curr_func = Conf,
                               parallel_tcs = Parallel}};
handle_call({set_curr_func,{group,Group,Conf},_SkipOrFail}, _From, State) ->
    {reply, ok, State#eh_state{curr_group = Group,
                               curr_func = Conf,
                               parallel_tcs = false}};
handle_call({set_curr_func,{group,undefined},_Config}, _From, State) ->
    {reply, ok, State#eh_state{curr_group = undefined,
                               curr_func = undefined,
                               parallel_tcs = false}};
handle_call({set_curr_func,{Suite,Conf},_Config}, _From, State) ->
    {reply, ok, State#eh_state{curr_suite = Suite,
                               curr_func = Conf,
                               parallel_tcs = false}};
handle_call({set_curr_func,undefined,_Config}, _From, State) ->
    {reply, ok, State#eh_state{curr_suite = undefined,
                               curr_func = undefined,
                               parallel_tcs = false}};
handle_call({set_curr_func,TC,_Config}, _From, State) ->
    {reply, ok, State#eh_state{curr_func = TC}};

handle_call({set_logfunc,NewLogFunc}, _From, State) ->
    {reply, NewLogFunc, State#eh_state{log_func = NewLogFunc}};

handle_call({handle_remote_events,Bool}, _From, State) ->
    {reply, ok, State#eh_state{handle_remote_events = Bool}}.

terminate(_) ->
    _ = logger:remove_handler(?MODULE),
    _ = supervisor:terminate_child(logger_sup,?MODULE),
    _ = supervisor:delete_child(logger_sup,?MODULE),
    ok.

terminate(_Arg, _State) ->
    ok.

set_curr_func(CurrFunc, Config) ->
    gen_server:call(?MODULE, {set_curr_func, CurrFunc, Config}).

set_log_func(Func) ->
    gen_server:call(?MODULE, {set_logfunc, Func}).

handle_remote_events(Bool) ->
    gen_server:call(?MODULE, {handle_remote_events, Bool}).

%%%-----------------------------------------------------------------

format_header(#eh_state{curr_suite = undefined,
			curr_group = undefined,
			curr_func = undefined}) ->
    lists:flatten(io_lib:format("System report", []));

format_header(#eh_state{curr_suite = Suite,
			curr_group = undefined,
			curr_func = undefined}) ->
    lists:flatten(io_lib:format("System report during ~w", [Suite]));

format_header(#eh_state{curr_suite = Suite,
			curr_group = undefined,
			curr_func = TcOrConf}) ->
    lists:flatten(io_lib:format("System report during ~w:~tw/1",
                                [Suite,TcOrConf]));

format_header(#eh_state{curr_suite = Suite,
			curr_group = Group,
			curr_func = Conf}) when Conf == init_per_group;
						Conf == end_per_group ->
    lists:flatten(io_lib:format("System report during ~w:~w/2 for ~tw",
                                [Suite,Conf,Group]));

format_header(#eh_state{curr_suite = Suite,
			curr_group = Group,
			parallel_tcs = true}) ->
    lists:flatten(io_lib:format("System report during ~tw in ~w",
                                [Group,Suite]));

format_header(#eh_state{curr_suite = Suite,
			curr_group = Group,
			curr_func = TC}) ->
    lists:flatten(io_lib:format("System report during ~w:~tw/1 in ~tw",
                                [Suite,TC,Group])).
