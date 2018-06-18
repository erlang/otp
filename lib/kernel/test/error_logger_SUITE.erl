%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(error_logger_SUITE).

-include_lib("common_test/include/ct.hrl").

%%-----------------------------------------------------------------
%% We don't have to test the normal behaviour here, i.e. the tty
%% handler.
%% We will add an own error handler in order to verify that the
%% error_logger deliver the expected events.
%%-----------------------------------------------------------------

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
         off_heap/1,
	 error_report/1, info_report/1, error/1, info/1,
	 emulator/1, via_logger_process/1, other_node/1,
         tty/1, logfile/1, add/1, delete/1, format_depth/1]).

-export([generate_error/2]).

-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [off_heap, error_report, info_report, error, info, emulator,
     via_logger_process, other_node, tty, logfile, add, delete,
     format_depth].

groups() -> 
    [].

init_per_suite(Config) ->
    logger:add_handler(error_logger,error_logger,
                       #{level=>info,filter_default=>log}),
    Config.

end_per_suite(_Config) ->
    logger:remove_handler(error_logger),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------

off_heap(_Config) ->
    %% The error_logger process may receive a huge amount of
    %% messages. Make sure that they are stored off heap to
    %% avoid exessive GCs.
    MQD = message_queue_data,
    {MQD,off_heap} = process_info(whereis(error_logger), MQD),
    ok.

%%-----------------------------------------------------------------

error_report(Config) when is_list(Config) ->
    error_logger:add_report_handler(?MODULE, self()),
    Rep1 = [{tag1,"data1"},{tag2,data2},{tag3,3}],
    Rep2 = [testing,"testing",{tag1,"tag1"}],
    Rep3 = "This is a string !",
    Rep4 = {this,is,a,tuple},
    ok = error_logger:error_report(Rep1),
    reported(error_report, std_error, Rep1),
    ok = error_logger:error_report(Rep2),
    reported(error_report, std_error, Rep2),
    ok = error_logger:error_report(Rep3),
    reported(error_report, std_error, Rep3),
    ok = error_logger:error_report(Rep4),
    reported(error_report, std_error, Rep4),

    ok = error_logger:error_report(test_type, Rep1),
    reported(error_report, test_type, Rep1),
    ok = error_logger:error_report(test_type, Rep2),
    reported(error_report, test_type, Rep2),
    ok = error_logger:error_report(test_type, Rep3),
    reported(error_report, test_type, Rep3),
    ok = error_logger:error_report(test_type, Rep4),
    reported(error_report, test_type, Rep4),

    ok = error_logger:error_report("test_type", Rep1),
    reported(error_report, "test_type", Rep1),
    ok = error_logger:error_report({test,type}, Rep2),
    reported(error_report, {test,type}, Rep2),
    ok = error_logger:error_report([test,type], Rep3),
    reported(error_report, [test,type], Rep3),
    ok = error_logger:error_report(1, Rep4),
    reported(error_report, 1, Rep4),

    my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

info_report(Config) when is_list(Config) ->
    error_logger:add_report_handler(?MODULE, self()),
    Rep1 = [{tag1,"data1"},{tag2,data2},{tag3,3}],
    Rep2 = [testing,"testing",{tag1,"tag1"}],
    Rep3 = "This is a string !",
    Rep4 = {this,is,a,tuple},
    ok = error_logger:info_report(Rep1),
    reported(info_report, std_info, Rep1),
    ok = error_logger:info_report(Rep2),
    reported(info_report, std_info, Rep2),
    ok = error_logger:info_report(Rep3),
    reported(info_report, std_info, Rep3),
    ok = error_logger:info_report(Rep4),
    reported(info_report, std_info, Rep4),

    ok = error_logger:info_report(test_type, Rep1),
    reported(info_report, test_type, Rep1),
    ok = error_logger:info_report(test_type, Rep2),
    reported(info_report, test_type, Rep2),
    ok = error_logger:info_report(test_type, Rep3),
    reported(info_report, test_type, Rep3),
    ok = error_logger:info_report(test_type, Rep4),
    reported(info_report, test_type, Rep4),

    ok = error_logger:info_report("test_type", Rep1),
    reported(info_report, "test_type", Rep1),
    ok = error_logger:info_report({test,type}, Rep2),
    reported(info_report, {test,type}, Rep2),
    ok = error_logger:info_report([test,type], Rep3),
    reported(info_report, [test,type], Rep3),
    ok = error_logger:info_report(1, Rep4),
    reported(info_report, 1, Rep4),

    my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

error(Config) when is_list(Config) ->
    error_logger:add_report_handler(?MODULE, self()),
    Msg1 = "This is a plain text string~n",
    Msg2 = "This is a text with arguments ~p~n",
    Arg2 = "This is the argument",
    Msg3 = {erroneous,msg},

    ok = error_logger:error_msg(Msg1),
    reported(error, Msg1, []),
    ok = error_logger:error_msg(Msg2, Arg2),
    reported(error, Msg2, Arg2),
    ok = error_logger:error_msg(Msg3),
    reported(error, Msg3, []),

    ok = error_logger:error_msg(Msg1, []),
    reported(error, Msg1, []),
    ok = error_logger:error_msg(Msg2, Arg2),
    reported(error, Msg2, Arg2),
    ok = error_logger:error_msg(Msg3, []),
    reported(error, Msg3, []),

    ok = error_logger:format(Msg1, []),
    reported(error, Msg1, []),
    ok = error_logger:format(Msg2, Arg2),
    reported(error, Msg2, Arg2),
    ok = error_logger:format(Msg3, []),
    reported(error, Msg3, []),

    my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

info(Config) when is_list(Config) ->
    error_logger:add_report_handler(?MODULE, self()),
    Msg1 = "This is a plain text string~n",
    Msg2 = "This is a text with arguments ~p~n",
    Arg2 = "This is the argument",
    Msg3 = {erroneous,msg},

    ok = error_logger:info_msg(Msg1),
    reported(info_msg, Msg1, []),
    ok = error_logger:info_msg(Msg2, Arg2),
    reported(info_msg, Msg2, Arg2),
    ok = error_logger:info_msg(Msg3),
    reported(info_msg, Msg3, []),

    ok = error_logger:info_msg(Msg1, []),
    reported(info_msg, Msg1, []),
    ok = error_logger:info_msg(Msg2, Arg2),
    reported(info_msg, Msg2, Arg2),
    ok = error_logger:info_msg(Msg3, []),
    reported(info_msg, Msg3, []),

    my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

%%-----------------------------------------------------------------

emulator(Config) when is_list(Config) ->
    error_logger:add_report_handler(?MODULE, self()),
    Msg = "Error in process ~p on node ~p with exit value:~n~p~n",
    Error = {badmatch,4},
    Stack = [{module, function, 2, []}],
    Pid = spawn(?MODULE, generate_error, [Error, Stack]),
    reported(error, Msg, [Pid, node(), {Error, Stack}]),
    my_yes = error_logger:delete_report_handler(?MODULE),
    ok.

generate_error(Error, Stack) ->
    erlang:raise(error, Error, Stack).

%%-----------------------------------------------------------------

via_logger_process(Config) ->
    case os:type() of
        {win32,_} ->
            {skip,"Skip on windows - cant change file mode"};
        _ ->
            error_logger:add_report_handler(?MODULE, self()),
            Dir = filename:join(?config(priv_dir,Config),"dummydir"),
            Msg = "File operation error: eacces. Target: " ++
                Dir ++ ". Function: list_dir. ",
            ok = file:make_dir(Dir),
            ok = file:change_mode(Dir,8#0222),
            error = erl_prim_loader:list_dir(Dir),
            ok = file:change_mode(Dir,8#0664),
            _ = file:del_dir(Dir),
            reported(error_report, std_error, Msg),
            my_yes = error_logger:delete_report_handler(?MODULE),
            ok
    end.

%%-----------------------------------------------------------------

other_node(_Config) ->
    error_logger:add_report_handler(?MODULE, self()),
    {ok,Node} = test_server:start_node(?FUNCTION_NAME,slave,[]),
    ok = rpc:call(Node,logger,add_handler,[error_logger,error_logger,
                                           #{level=>info,filter_default=>log}]),
    rpc:call(Node,error_logger,error_report,[hi_from_remote]),
    reported(error_report,std_error,hi_from_remote),
    test_server:stop_node(Node),
    ok.


%%-----------------------------------------------------------------
%% We don't enables or disables tty error logging here. We do not
%% want to interact with the test run.
%%-----------------------------------------------------------------

tty(Config) when is_list(Config) ->
    {'EXIT', _Reason} = (catch error_logger:tty(dummy)),
    ok.

%%-----------------------------------------------------------------
%% If where already exists a logfile we skip this test case !!
%%-----------------------------------------------------------------

logfile(Config) when is_list(Config) ->
    case error_logger:logfile(filename) of
	{error, no_log_file} -> % Ok, we continues.
	    do_logfile();
	_ ->
	    ok
    end.

do_logfile() ->
    {error, _} = error_logger:logfile(close),
    {error, _} = error_logger:logfile({open,{error}}),
    ok = error_logger:logfile({open, "dummy_logfile.log"}),
    "dummy_logfile.log" = error_logger:logfile(filename),
    ok = error_logger:logfile(close),
    {'EXIT',_} = (catch error_logger:logfile(dummy)),
    ok.

%%-----------------------------------------------------------------

add(Config) when is_list(Config) ->
    {'EXIT',_} = (catch error_logger:add_report_handler("dummy")),
    {'EXIT',_} = error_logger:add_report_handler(non_existing),
    my_error = error_logger:add_report_handler(?MODULE, [error]),
    ok.

%%-----------------------------------------------------------------

delete(Config) when is_list(Config) ->
    {'EXIT',_} = (catch error_logger:delete_report_handler("dummy")),
    {error,_} = error_logger:delete_report_handler(non_existing),
    ok.

%%-----------------------------------------------------------------

format_depth(_Config) ->
    ok = application:set_env(kernel,error_logger_format_depth,30),
    30 = error_logger:get_format_depth(),
    ok = application:set_env(kernel,error_logger_format_depth,3),
    10 = error_logger:get_format_depth(),
    ok = application:set_env(kernel,error_logger_format_depth,11),
    11 = error_logger:get_format_depth(),
    ok = application:set_env(kernel,error_logger_format_depth,unlimited),
    unlimited = error_logger:get_format_depth(),
    ok = application:unset_env(kernel,error_logger_format_depth),
    unlimited = error_logger:get_format_depth(),
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
	    ct:fail({no_report_received,test_server:messages_get()})
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
