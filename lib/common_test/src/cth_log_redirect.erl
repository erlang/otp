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
-module(cth_log_redirect).

%%% @doc Common Test Framework functions handling test specifications.
%%%
%%% <p>This module redirects sasl and error logger info to common test log.</p>
%%% @end


%% CTH Callbacks
-export([id/1, init/2, pre_init_per_testcase/3, post_end_per_testcase/4]).

%% Event handler Callbacks
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-record(state, { }).

id(_Opts) ->
    ?MODULE.

init(?MODULE, _Opts) ->
    error_logger:add_report_handler(?MODULE),
    #state{  }.

pre_init_per_testcase(_Testcase, Config, State) ->
    {Config, State}.

post_end_per_testcase(_TestCase, _Config, Result, State) ->
    {Result, State}.

%% Copied and modified from sasl_report_tty_h.erl
init(Type) ->
    {ok, Type}.

handle_event({_Type, GL, _Msg}, State) when node(GL) /= node() ->
    {ok, State};
handle_event(Event, Type) ->
    case proplists:get_value(sasl, application:which_applications()) of
	undefined ->
	    sasl_not_started;
	_Else ->
	    {ok, ErrLogType} = application:get_env(sasl, errlog_type),
	    SReport = sasl_report:format_report(group_leader(), ErrLogType,
						tag_event(Event)),
	    if is_list(SReport) ->
		    ct:log(sasl, SReport, []);
	       true -> %% Report is an atom if no logging is to be done
		    ignore
	    end
    end,
    EReport = error_logger_tty_h:write_event(
		tag_event(Event),io_lib),
    if is_list(EReport) ->
	    ct:log(error_logger, EReport, []);
       true -> %% Report is an atom if no logging is to be done
	    ignore
    end,
    {ok, Type}.

handle_info(_,State) -> {ok, State}.

handle_call(_Query, _Type) -> {error, bad_query}.

terminate(_Reason, _Type) ->
    [].

tag_event(Event) ->
    {calendar:local_time(), Event}.
