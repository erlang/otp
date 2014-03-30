-module(ct_telnet_own_server_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

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
    [expect,
     expect_repeat,
     expect_sequence,
     expect_error_prompt,
     expect_error_timeout1,
     expect_error_timeout2,
     expect_error_timeout3,
     no_prompt_check,
     no_prompt_check_repeat,
     no_prompt_check_sequence,
     no_prompt_check_timeout,
     ignore_prompt,
     ignore_prompt_repeat,
     ignore_prompt_sequence,
     ignore_prompt_timeout,
     server_speaks,
     server_disconnects].

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
    {error,timeout} = ct_telnet:expect(Handle, ["xxx"], [{timeout,1000}]),
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
    {error,timeout} = ct_telnet:expect(Handle, ["yyy"],
				       [{idle_timeout,infinity},
					{total_timeout,3000}]),
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
							 {timeout,1000}]),
    ok = ct_telnet:send(Handle, "echo xxx"), % sends prompt and newline
    {ok,["xxx"]} = ct_telnet:expect(Handle, ["xxx"], [ignore_prompt,
						      {timeout,1000}]),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx\n"), % no prompt, but newline
    {ok,["xxx"]} = ct_telnet:expect(Handle, ["xxx"], [ignore_prompt,
						      {timeout,1000}]),
    ok = ct_telnet:send(Handle, "echo_no_prompt xxx"), % no prompt, no newline
    {error,timeout} = ct_telnet:expect(Handle, ["xxx"], [ignore_prompt,
							 {timeout,1000}]),
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
							 {timeout,1000}]),
    ok = ct_telnet:close(Handle),
    ok.

%% The server says things. Manually check that it gets printed correctly
%% in the general IO log.
server_speaks(_) ->
    {ok, Handle} = ct_telnet:open(telnet_server_conn1),
    ok = ct_telnet:send(Handle, "echo_no_prompt This is the first message\r\n"),
    ok = ct_telnet:send(Handle, "echo_no_prompt This is the second message\r\n"),
    %% let ct_telnet_client get an idle timeout
    timer:sleep(15000),
    ok = ct_telnet:send(Handle, "echo_no_prompt This is the third message\r\n"),
    {ok,_} = ct_telnet:expect(Handle, ["the"], [no_prompt_check]),
    {error,timeout} = ct_telnet:expect(Handle, ["the"], [no_prompt_check,
							 {timeout,1000}]),
    ok = ct_telnet:send(Handle, "echo_no_prompt This is the fourth message\r\n"),
    %% give the server time to respond
    timer:sleep(2000),
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
    timer:sleep(500),
    ok = ct_telnet:send(Handle, "echo_no_prompt This is the message\r\n"),
    %% when the server closes the connection, the last message should be
    %% printed in the log
    timer:sleep(3000),
    _ = ct_telnet:close(Handle),
    ok.
