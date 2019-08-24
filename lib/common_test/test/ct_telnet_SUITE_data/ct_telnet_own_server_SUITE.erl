-module(ct_telnet_own_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%% telnet control characters
-define(SE,	240).
-define(NOP,	241).
-define(DM,	242).
-define(BRK,	243).
-define(IP,	244).
-define(AO,	245).
-define(AYT,	246).
-define(EC,	247).
-define(EL,	248).
-define(GA,	249).
-define(SB,	250).
-define(WILL,	251).
-define(WONT,	252).
-define(DO,	253).
-define(DONT,	254).
-define(IAC,	255).

-define(SHORT_TIME,2000).

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() ->
    [
     {require,telnet_server_conn1,{unix,[telnet]}},
     {require,ct_conn_log},
     {ct_hooks, [{cth_conn_log,[]}]}
    ].

all() ->
    [
     expect,
     expect_repeat,
     expect_sequence,
     expect_wait_until_prompt,
     expect_error_prompt,
     expect_error_timeout1,
     expect_error_timeout2,
     expect_error_timeout3,
     total_timeout_less_than_idle,
     no_prompt_check,
     no_prompt_check_repeat,
     no_prompt_check_sequence,
     no_prompt_check_timeout,
     ignore_prompt,
     ignore_prompt_repeat,
     ignore_prompt_sequence,
     ignore_prompt_timeout,
     large_string,
     server_speaks,
     server_disconnects,
     newline_ayt,
     newline_break,
     newline_string
    ].

groups() ->
    [].

init_per_suite(Config) ->
    ct:pal("Will use these log hook options: ~p",
	   [ct:get_config(ct_conn_log,[])]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Simple expect
expect(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo ayt"),
    {ok,["ayt"]} = ct_telnet:expect(Handle, "ayt"),
    ok = ct_telnet:send(Handle, "echo ayt"),
    {ok,["ayt"]} = ct_telnet:expect(Handle, ["ayt"]),
    ok = ct_telnet:close(Handle),
    ok.

%% Expect with repeat option
expect_repeat(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_ml xy xy"),
    {ok,[["xy"],["xy"]],done} = ct_telnet:expect(Handle, ["xy"],[{repeat,2}]),
    ok = ct_telnet:close(Handle),
    ok.

%% Expect with sequence option
expect_sequence(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_ml ab cd ef"),
    {ok,[["ab"],["cd"],["ef"]]} = ct_telnet:expect(Handle,
						   [["ab"],["cd"],["ef"]],
						   [sequence]),
    ok = ct_telnet:close(Handle),
    ok.

%% Check that expect can wait for delayed prompt
expect_wait_until_prompt(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    Timeouts = [{idle_timeout,5000},{total_timeout,7000}],

    ok = ct_telnet:send(Handle, "echo_delayed_prompt 3000 xxx"),
    {ok,["xxx"]} =
	ct_telnet:expect(Handle, "xxx",
			 [wait_for_prompt|Timeouts]),
    ok = ct_telnet:send(Handle, "echo_delayed_prompt 3000 yyy zzz"),
    {ok,[["yyy"],["zzz"]]} =
	ct_telnet:expect(Handle, ["yyy","zzz"],
			 [{wait_for_prompt,"> "}|Timeouts]),
    ok.

%% Check that expect returns when a prompt is found, even if pattern
%% is not matched.
expect_error_prompt(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo xxx> yyy"),
    {error,{prompt,"> "}} = ct_telnet:expect(Handle, ["yyy"]),
    ok = ct_telnet:close(Handle),
    ok.

%% Check that expect returns after idle timeout, and even if the
%% expected pattern is received - as long as not newline or prompt is
%% received it will not match.
expect_error_timeout1(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx"),
    {error,timeout} = ct_telnet:expect(Handle, ["xxx"], [{timeout,?SHORT_TIME}]),
    ok = ct_telnet:close(Handle),
    ok.

expect_error_timeout2(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx"),
    {error,timeout} = ct_telnet:expect(Handle, ["xxx"], [{idle_timeout,1000},
							 {total_timeout,infinity}]),
    ok = ct_telnet:close(Handle),
    ok.

%% Check that if server loops and pattern not matching, the operation
%% can be aborted
expect_error_timeout3(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_loop 5000 xxx"),

    T0 = now(),
    {error,timeout} = ct_telnet:expect(Handle, ["yyy"],
				       [{idle_timeout,infinity},
					{total_timeout,2001}]),
    Diff = trunc(timer:now_diff(now(),T0)/1000),
    {_,true} = {Diff, (Diff >= 2000) and (Diff =< 4000)},

    ok = ct_telnet:send(Handle, "echo ayt"),
    {ok,["ayt"]} = ct_telnet:expect(Handle, ["ayt"]),
    ok = ct_telnet:close(Handle),
    ok.
    
%% OTP-12335: If total_timeout < idle_timeout, expect will never timeout
%% until after idle_timeout, which is incorrect.
total_timeout_less_than_idle(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx"),

    T0 = now(),
    {error,timeout} = ct_telnet:expect(Handle, ["yyy"],
				       [{idle_timeout,5000},
					{total_timeout,2001}]),
    Diff = trunc(timer:now_diff(now(),T0)/1000),
    {_,true} = {Diff, (Diff >= 2000) and (Diff =< 4000)},

    ok = ct_telnet:send(Handle, "echo ayt"),
    {ok,["ayt"]} = ct_telnet:expect(Handle, ["ayt"]),
    ok = ct_telnet:close(Handle),
    ok.

%% expect with ignore_prompt option should not return even if a prompt
%% is found. The pattern after the prompt (here "> ") can be matched.
ignore_prompt(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo xxx> yyy"),
    {ok,["yyy"]} = ct_telnet:expect(Handle, ["yyy"], [ignore_prompt]),
    ok = ct_telnet:close(Handle),
    ok.

%% expect with ignore_prompt and repeat options.
ignore_prompt_repeat(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_ml yyy> yyy>"),
    {ok,[["yyy"],["yyy"]],done} = ct_telnet:expect(Handle, ["yyy"],
						   [{repeat,2},
						    ignore_prompt]),
    ok = ct_telnet:close(Handle),
    ok.

%% expect with ignore_prompt and sequence options.
ignore_prompt_sequence(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_ml xxx> yyy> zzz> "),
    {ok,[["xxx"],["yyy"],["zzz"]]} = ct_telnet:expect(Handle,
						      [["xxx"],["yyy"],["zzz"]],
						      [sequence,
						       ignore_prompt]),
    ok = ct_telnet:close(Handle),
    ok.

%% Check that expect returns after idle timeout when ignore_prompt
%% option is used.
%% As for expect without the ignore_prompt option, it a newline or a
%% prompt is required in order for the pattern to match.
ignore_prompt_timeout(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo xxx"),
    {error,timeout} = ct_telnet:expect(Handle, ["yyy"], [ignore_prompt,
							 {timeout,?SHORT_TIME}]),
    ok = ct_telnet:send(Handle, "echo xxx"), % sends prompt and newline
    {ok,["xxx"]} = ct_telnet:expect(Handle, ["xxx"], [ignore_prompt,
						      {timeout,?SHORT_TIME}]),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx\n"), % no prompt, but newline
    {ok,["xxx"]} = ct_telnet:expect(Handle, ["xxx"], [ignore_prompt,
						      {timeout,?SHORT_TIME}]),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx"), % no prompt, no newline
    {error,timeout} = ct_telnet:expect(Handle, ["xxx"], [ignore_prompt,
							 {timeout,?SHORT_TIME}]),
    ok = ct_telnet:close(Handle),
    ok.

%% no_prompt_check option shall match pattern both when prompt is sent
%% and when it is not.
no_prompt_check(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo xxx"),
    {ok,["xxx"]} = ct_telnet:expect(Handle, ["xxx"], [no_prompt_check]),
    ok = ct_telnet:send(Handle, "echo_no_prompt yyy"),
    {ok,["yyy"]} = ct_telnet:expect(Handle, ["yyy"], [no_prompt_check]),
    ok = ct_telnet:close(Handle),
    ok.

%% no_prompt_check and repeat options
no_prompt_check_repeat(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_ml xxx xxx"),
    {ok,[["xxx"],["xxx"]],done} = ct_telnet:expect(Handle,["xxx"],
						   [{repeat,2},
						    no_prompt_check]),
    ok = ct_telnet:send(Handle, "echo_ml_no_prompt yyy yyy"),
    {ok,[["yyy"],["yyy"]],done} = ct_telnet:expect(Handle,["yyy"],
						   [{repeat,2},
						    no_prompt_check]),
    ok = ct_telnet:close(Handle),
    ok.

%% no_prompt_check and sequence options
no_prompt_check_sequence(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_ml_no_prompt ab cd ef"),
    {ok,[["ab"],["cd"],["ef"]]} = ct_telnet:expect(Handle,
						   [["ab"],["cd"],["ef"]],
						   [sequence,
						    no_prompt_check]),
    ok = ct_telnet:close(Handle),
    ok.

%% Check that expect returns after idle timeout when no_prompt_check
%% option is used.
no_prompt_check_timeout(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo xxx"),
    {error,timeout} = ct_telnet:expect(Handle, ["yyy"], [no_prompt_check,
							 {timeout,?SHORT_TIME}]),
    ok = ct_telnet:close(Handle),
    ok.

%% Check that it's possible to receive multiple chunks of data sent from
%% the server with one get_data call
large_string(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    String = "abcd efgh ijkl mnop qrst uvwx yz ",
    BigString = lists:flatmap(fun(S) -> S end,
			      [String || _ <- lists:seq(1,10)]),
    VerifyStr = [C || C <- BigString, C/=$ ],

    {ok,Data} = ct_telnet:cmd(Handle, "echo_sep "++BigString),
    ct:log("[CMD] Received ~w chars: ~s", [length(lists:flatten(Data)),Data]),
    VerifyStr = [C || C <- lists:flatten(Data), C/=$ , C/=$\r, C/=$\n, C/=$>],

    %% Test #1: With a long sleep value, all data gets gets buffered and
    %% ct_telnet can receive it with one single request to ct_telnet_client.
    %% Test #2: With a short sleep value, ct_telnet needs multiple calls to
    %% ct_telnet_client to collect the data. This iterative operation should
    %% yield the same result as the single request case.

    ok = ct_telnet:send(Handle, "echo_sep "++BigString),
    ct:sleep(1000),
    {ok,Data1} = ct_telnet:get_data(Handle),
    ct:log("[GET DATA #1] Received ~w chars: ~s",
	   [length(lists:flatten(Data1)),Data1]),
    VerifyStr = [C || C <- lists:flatten(Data1), C/=$ , C/=$\r, C/=$\n, C/=$>],

    ok = ct_telnet:send(Handle, "echo_sep "++BigString),
    %% On some slow machines, 50 ms might not be enough to get the
    %% first packet of data. We will therefore keep trying for a
    %% second before we give up this...
    F = fun RepeatUntilData(N) ->
		ct:sleep(50),
		case ct_telnet:get_data(Handle) of
		    {ok,[]} when N>1 ->
			RepeatUntilData(N-1);
		    Other ->
			Other
		end
	end,
    {ok,Data2} = F(20),
    ct:log("[GET DATA #2] Received ~w chars: ~s", [length(lists:flatten(Data2)),Data2]),
    VerifyStr = [C || C <- lists:flatten(Data2), C/=$ , C/=$\r, C/=$\n, C/=$>],

    ok = ct_telnet:close(Handle),
    ok.

%% The server says things. Manually check that it gets printed correctly
%% in the general IO log.
%%
%% In this test case we simulate data sent spontaneously from the
%% server. We use ct_telnet_client:send_data instead of ct_telnet:send
%% to avoid flushing of buffers in the client, and we use
%% echo_no_prompt since the server would normally not send a prompt in
%% this case.
server_speaks(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),

    Backdoor = ct_gen_conn:get_conn_pid(Handle),
    ok = ct_telnet_client:send_data(Backdoor,
				    "echo_no_prompt This is the first message"),
    ok = ct_telnet_client:send_data(Backdoor,
				    "echo_no_prompt This is the second message"),
    %% Let ct_telnet_client get an idle timeout. This should print the
    %% two messages to the log. Note that the buffers are not flushed here!
    ct:sleep(15000),
    ok = ct_telnet_client:send_data(Backdoor,
				    "echo_no_prompt This is the third message"),
    {ok,_} = ct_telnet:expect(Handle, ["first.*second.*third"],
			      [no_prompt_check, sequence]),
    {error,timeout} = ct_telnet:expect(Handle, ["the"], [no_prompt_check,
							 {timeout,?SHORT_TIME}]),
    ok = ct_telnet_client:send_data(Backdoor,
				    "echo_no_prompt This is the fourth message"),
    %% give the server time to respond
    ct:sleep(2000),
    %% closing the connection should print last message in log
    ok = ct_telnet:close(Handle),
    ok.  

%% Let the server close the connection. Make sure buffered data gets printed
%% to the general IO log.
server_disconnects(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "disconnect_after 1500"),
    %% wait until the get_data operation (triggered by send/2) times out
    %% before sending the msg
    ct:sleep(500),
    ok = ct_telnet:send(Handle, "echo_no_prompt This is the message"),
    %% when the server closes the connection, the last message should be
    %% printed in the log
    ct:sleep(3000),
    _ = ct_telnet:close(Handle),
    ok.

%% Test option {newline,false} to send telnet command sequence.
newline_ayt(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, [?IAC,?AYT], [{newline,false}]),
    {ok,["yes"]} = ct_telnet:expect(Handle, ["yes"]),
    ok = ct_telnet:close(Handle),
    ok.

%% Test option {newline,false} to send telnet command sequence.
newline_break(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, [?IAC,?BRK], [{newline,false}]),
    %% '#' is the prompt in break mode
    {ok,["# "]} = ct_telnet:expect(Handle, ["# "], [no_prompt_check]),
    {ok,R} = ct_telnet:cmd(Handle, "q", [{newline,false}]),
    "> " = lists:flatten(R),
    ok = ct_telnet:close(Handle),
    ok.

%% Test option {newline,String} to specify an own newline, e.g. "\r\n"
newline_string(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo hello-", [{newline,"own_nl\n"}]),
    {ok,["hello-own_nl"]} = ct_telnet:expect(Handle, ["hello-own_nl"]),
    ok = ct_telnet:close(Handle),
    ok.
