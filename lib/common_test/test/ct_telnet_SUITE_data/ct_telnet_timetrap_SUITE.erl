-module(ct_telnet_timetrap_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(name, telnet_server_conn1).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

suite() -> [{require,?name,{unix,[telnet]}},
	    {require,ct_conn_log},
 	    {ct_hooks, [{cth_conn_log,[]}]},
	    {timetrap,{seconds,7}}].

all() ->
    [expect_timetrap,
     expect_success].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_,Config) ->
    ct:log("init_per_testcase: opening telnet connection...",[]),
    {ok,_} = ct_telnet:open(?name),
    ct:log("...done",[]),
    Config.

end_per_testcase(_,_Config) ->
    ct:log("end_per_testcase: closing telnet connection...",[]),
    _ = ct_telnet:close(?name),
    ct:log("...done",[]),
    ok.


%% OTP-10648
%% This test case should fail with timetrap timeout.
%%
%% The long timetrap timeout and timeout option in the expect call
%% also causes the telnet client to hang so long that the attempt at
%% closing it (in end_per_testcase) will time out (close timeout is 5
%% sec) without a timetrap timeout occuring in end_per_testcase.
%%
%% The point is to see that the connection is thoroughly removed and
%% unregistered anyway so that the next test case can successfully
%% open a connection with the same name.
%%
%% Note!!! that if end_per_testcase reaches a timetrap timeout before
%% the connection is closed, then the connection will survive until
%% the hanging expect times out, after which it will be closed in a
%% seemingly normal way due to the close message which was sent by the
%% close attempt. This could happen any time during the subsequent
%% test cases and cause confusion... There is however not much to do
%% about this, except writing test cases with timetrap timeout longer
%% than the close timeout (as this test case does)
expect_timetrap(_) ->
    {error,timeout} = ct_telnet:expect(?name, ["ayt"], [{timeout,20000}]),
    ok.

%% This should succeed
expect_success(_) ->
    ok = ct_telnet:send(?name, "echo ayt"),
    {ok,["ayt"]} = ct_telnet:expect(?name, ["ayt"]),
    ok.
