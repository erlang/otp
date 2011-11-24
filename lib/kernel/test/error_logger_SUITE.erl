%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
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
-module(error_logger_SUITE).

-include_lib("test_server/include/test_server.hrl").

%%-----------------------------------------------------------------
%% We don't have to test the normal behaviour here, i.e. the tty
%% handler.
%% We will add an own error handler in order to verify that the
%% error_logger deliver the expected events.
%%-----------------------------------------------------------------

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 error_report/1, info_report/1, error/1, info/1,
	 emulator/1, tty/1, logfile/1, add/1, delete/1]).

-export([generate_error/0]).

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).


suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [error_report, info_report, error, info, emulator, tty,
     logfile, add, delete].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------

error_report(suite) -> [];
error_report(doc) -> [];
error_report(Config) when is_list(Config) ->
    ?line error_logger:add_report_handler(?MODULE, self()),
    Rep1 = [{tag1,"data1"},{tag2,data2},{tag3,3}],
    Rep2 = [testing,"testing",{tag1,"tag1"}],
    Rep3 = "This is a string !",
    Rep4 = {this,is,a,tuple},
    ?line ok = error_logger:error_report(Rep1),
    reported(error_report, std_error, Rep1),
    ?line ok = error_logger:error_report(Rep2),
    reported(error_report, std_error, Rep2),
    ?line ok = error_logger:error_report(Rep3),
    reported(error_report, std_error, Rep3),
    ?line ok = error_logger:error_report(Rep4),
    reported(error_report, std_error, Rep4),

    ?line ok = error_logger:error_report(test_type, Rep1),
    reported(error_report, test_type, Rep1),
    ?line ok = error_logger:error_report(test_type, Rep2),
    reported(error_report, test_type, Rep2),
    ?line ok = error_logger:error_report(test_type, Rep3),
    reported(error_report, test_type, Rep3),
    ?line ok = error_logger:error_report(test_type, Rep4),
    reported(error_report, test_type, Rep4),

    ?line ok = error_logger:error_report("test_type", Rep1),
    reported(error_report, "test_type", Rep1),
    ?line ok = error_logger:error_report({test,type}, Rep2),
    reported(error_report, {test,type}, Rep2),
    ?line ok = error_logger:error_report([test,type], Rep3),
    reported(error_report, [test,type], Rep3),
    ?line ok = error_logger:error_report(1, Rep4),
    reported(error_report, 1, Rep4),

    ?line my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

info_report(suite) -> [];
info_report(doc) -> [];
info_report(Config) when is_list(Config) ->
    ?line error_logger:add_report_handler(?MODULE, self()),
    Rep1 = [{tag1,"data1"},{tag2,data2},{tag3,3}],
    Rep2 = [testing,"testing",{tag1,"tag1"}],
    Rep3 = "This is a string !",
    Rep4 = {this,is,a,tuple},
    ?line ok = error_logger:info_report(Rep1),
    reported(info_report, std_info, Rep1),
    ?line ok = error_logger:info_report(Rep2),
    reported(info_report, std_info, Rep2),
    ?line ok = error_logger:info_report(Rep3),
    reported(info_report, std_info, Rep3),
    ?line ok = error_logger:info_report(Rep4),
    reported(info_report, std_info, Rep4),

    ?line ok = error_logger:info_report(test_type, Rep1),
    reported(info_report, test_type, Rep1),
    ?line ok = error_logger:info_report(test_type, Rep2),
    reported(info_report, test_type, Rep2),
    ?line ok = error_logger:info_report(test_type, Rep3),
    reported(info_report, test_type, Rep3),
    ?line ok = error_logger:info_report(test_type, Rep4),
    reported(info_report, test_type, Rep4),

    ?line ok = error_logger:info_report("test_type", Rep1),
    reported(info_report, "test_type", Rep1),
    ?line ok = error_logger:info_report({test,type}, Rep2),
    reported(info_report, {test,type}, Rep2),
    ?line ok = error_logger:info_report([test,type], Rep3),
    reported(info_report, [test,type], Rep3),
    ?line ok = error_logger:info_report(1, Rep4),
    reported(info_report, 1, Rep4),

    ?line my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

error(suite) -> [];
error(doc) -> [];
error(Config) when is_list(Config) ->
    ?line error_logger:add_report_handler(?MODULE, self()),
    Msg1 = "This is a plain text string~n",
    Msg2 = "This is a text with arguments ~p~n",
    Arg2 = "This is the argument",
    Msg3 = {erroneous,msg},

    ?line ok = error_logger:error_msg(Msg1),
    reported(error, Msg1, []),
    ?line ok = error_logger:error_msg(Msg2, Arg2),
    reported(error, Msg2, Arg2),
    ?line ok = error_logger:error_msg(Msg3),
    reported(error, Msg3, []),

    ?line ok = error_logger:error_msg(Msg1, []),
    reported(error, Msg1, []),
    ?line ok = error_logger:error_msg(Msg2, Arg2),
    reported(error, Msg2, Arg2),
    ?line ok = error_logger:error_msg(Msg3, []),
    reported(error, Msg3, []),

    ?line ok = error_logger:format(Msg1, []),
    reported(error, Msg1, []),
    ?line ok = error_logger:format(Msg2, Arg2),
    reported(error, Msg2, Arg2),
    ?line ok = error_logger:format(Msg3, []),
    reported(error, Msg3, []),

    ?line my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

info(suite) -> [];
info(doc) -> [];
info(Config) when is_list(Config) ->
    ?line error_logger:add_report_handler(?MODULE, self()),
    Msg1 = "This is a plain text string~n",
    Msg2 = "This is a text with arguments ~p~n",
    Arg2 = "This is the argument",
    Msg3 = {erroneous,msg},

    ?line ok = error_logger:info_msg(Msg1),
    reported(info_msg, Msg1, []),
    ?line ok = error_logger:info_msg(Msg2, Arg2),
    reported(info_msg, Msg2, Arg2),
    ?line ok = error_logger:info_msg(Msg3),
    reported(info_msg, Msg3, []),

    ?line ok = error_logger:info_msg(Msg1, []),
    reported(info_msg, Msg1, []),
    ?line ok = error_logger:info_msg(Msg2, Arg2),
    reported(info_msg, Msg2, Arg2),
    ?line ok = error_logger:info_msg(Msg3, []),
    reported(info_msg, Msg3, []),

    ?line my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

emulator(suite) -> [];
emulator(doc) -> [];
emulator(Config) when is_list(Config) ->
    ?line error_logger:add_report_handler(?MODULE, self()),
    spawn(?MODULE, generate_error, []),
    reported(emulator),
    ?line my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

generate_error() ->
    erlang:error({badmatch,4}).

%%-----------------------------------------------------------------
%% We don't enables or disables tty error logging here. We do not
%% want to interact with the test run.
%%-----------------------------------------------------------------

tty(suite) -> [];
tty(doc) -> [];
tty(Config) when is_list(Config) ->
    ?line {'EXIT', _Reason} = (catch error_logger:tty(dummy)),
    ok.

%%-----------------------------------------------------------------
%% If where already exists a logfile we skip this test case !!
%%-----------------------------------------------------------------

logfile(suite) -> [];
logfile(doc) -> [];
logfile(Config) when is_list(Config) ->
    ?line case error_logger:logfile(filename) of
	      {error, no_log_file} -> % Ok, we continues.
		  do_logfile();
	      _ ->
		  ok
	  end.

do_logfile() ->
    ?line {error, _} = error_logger:logfile(close),
    ?line {error, _} = error_logger:logfile({open,{error}}),
    ?line ok = error_logger:logfile({open, "dummy_logfile.log"}),
    ?line "dummy_logfile.log" = error_logger:logfile(filename),
    ?line ok = error_logger:logfile(close),
    ?line {'EXIT',_} = (catch error_logger:logfile(dummy)),
    ok.

%%-----------------------------------------------------------------

add(suite) -> [];
add(doc) -> [];
add(Config) when is_list(Config) ->
    ?line {'EXIT',_} = (catch error_logger:add_report_handler("dummy")),
    ?line {'EXIT',_} = error_logger:add_report_handler(non_existing),
    ?line my_error = error_logger:add_report_handler(?MODULE, [error]),
    ok.

%%-----------------------------------------------------------------

delete(suite) -> [];
delete(doc) -> [];
delete(Config) when is_list(Config) ->
    ?line {'EXIT',_} = (catch error_logger:delete_report_handler("dummy")),
    ?line {error,_} = error_logger:delete_report_handler(non_existing),
    ok.

%%-----------------------------------------------------------------
%% Check that the report has been received.
%%-----------------------------------------------------------------
reported(Tag, Type, Report) ->
    receive
	{Tag, Type, Report} ->
	    test_server:messages_get(),
	    ok
    after 1000 ->
	    test_server:fail(no_report_received)
    end.

reported(emulator) ->
    receive
	{error, "~s~n", String} when is_list(String) ->
	    test_server:messages_get(),
	    ok
    after 1000 ->
	    test_server:fail(no_report_received)
    end.

%%-----------------------------------------------------------------
%% The error_logger handler (gen_event behaviour).
%% Sends a notification to the Tester process about the events
%% generated by the Tester process.
%%-----------------------------------------------------------------
init(Tester) when is_pid(Tester) ->
    {ok, Tester};
init(Config) when is_list(Config) ->
    my_error.

handle_event({Tag, _GL, {_EPid, Type, Report}}, Tester) ->
    Tester ! {Tag, Type, Report},
    {ok, Tester};
handle_event(_Event, Tester) ->
    {ok, Tester}.

handle_info({emulator, _GL, String}, Tester) ->
    Tester ! {emulator, String},
    {ok, Tester};
handle_info(_, Tester) ->
    {ok, Tester}.

handle_call(_Query, Tester) -> {ok, {error, bad_query}, Tester}.

terminate(_Reason, _Tester) ->
    my_yes.
