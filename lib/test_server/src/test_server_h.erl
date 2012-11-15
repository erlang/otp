%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
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

-module(test_server_h).
-behaviour(gen_event).

%% API
-export([install/0, restore/0]).
-export([testcase/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {kernel, sasl, testcase}).

%%====================================================================
%% API
%%====================================================================

install() ->
    case gen_event:add_handler(error_logger, ?MODULE, []) of
	ok ->
	    error_logger:delete_report_handler(sasl_report_tty_h),
	    gen_event:delete_handler(error_logger, error_logger_tty_h, []),
	    ok;
	Error ->
	    Error
    end.

restore() ->
    gen_event:add_handler(error_logger, error_logger_tty_h, []),
    error_logger:add_report_handler(sasl_report_tty_h, all),
    gen_event:delete_handler(error_logger, ?MODULE, []).

testcase(Testcase) ->
    gen_event:call(error_logger, ?MODULE, {set_testcase, Testcase}, 10*60*1000).

%%====================================================================
%% gen_event callbacks
%%====================================================================

init([]) ->

    %% error_logger_tty_h initialization
    User = set_group_leader(),

    %% sasl_report_tty_h initialization
    Type = all,

    {ok, #state{kernel={User, []}, sasl=Type}}.

set_group_leader() ->
    case whereis(user) of
	User when is_pid(User) ->
	    link(User),
	    group_leader(User, self()),
	    User;
	_ ->
	    false
    end.

handle_event({_Type, GL, _Msg}, State) when node(GL)/=node() ->
    {ok, State};
handle_event({Tag, _GL, {_Pid, Type, _Report}} = Event, State) ->
    SASL = lists:keyfind(sasl, 1, application:which_applications()),
    case report_receiver(Tag, Type) of
	sasl when SASL /= false ->
	    {ok,ErrLogType} = application:get_env(sasl, errlog_type),
	    SReport = sasl_report:format_report(group_leader(), ErrLogType,
						tag_event(Event)),
	    if is_list(SReport) ->
		    tag(State#state.testcase),
		    sasl_report_tty_h:handle_event(Event,
						   State#state.sasl);
	       true -> %% Report is an atom if no logging is to be done
		    ignore
	    end;
	sasl -> %% SASL not running
	    ignore;
	kernel ->
	    tag(State#state.testcase),
	    error_logger_tty_h:handle_event(Event, State#state.kernel);
	none ->
	    ignore
    end,
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({set_testcase, Testcase}, State) ->
    {ok, ok, State#state{testcase=Testcase}};
handle_call(_Query, _State) ->
    {error, bad_query}.

handle_info({emulator,GL,_Chars}=Event, State) when node(GL)==node() ->
    tag(State#state.testcase),
    error_logger_tty_h:handle_info(Event, State#state.kernel),
    {ok, State};
handle_info(_Msg, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

report_receiver(error_report, supervisor_report) -> sasl;
report_receiver(error_report, crash_report) -> sasl;
report_receiver(info_report, progress) -> sasl;
report_receiver(error, _) -> kernel;
report_receiver(error_report, _) -> kernel;
report_receiver(warning_msg, _) -> kernel;
report_receiver(warning_report, _) -> kernel;
report_receiver(info, _) -> kernel;
report_receiver(info_msg, _) -> kernel;
report_receiver(info_report,Tuple)
  when is_tuple(Tuple) andalso
       (element(1,Tuple)==ct_connection orelse
	element(1,Tuple)==conn_log) ->
    none;
report_receiver(info_report, _) -> kernel;
report_receiver(_, _) -> none.

tag({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    io:format(user, "~n=TESTCASE: ~p:~p/~p", [M,F,A]);
tag(Testcase) ->
    io:format(user, "~n=TESTCASE: ~p", [Testcase]).

tag_event(Event) ->
    {calendar:local_time(), Event}.
