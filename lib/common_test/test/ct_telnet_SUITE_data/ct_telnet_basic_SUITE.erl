%% Modify your ts.unix.config or ts.win32.config file before running these tests
-module(ct_telnet_basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> [
	    {require,the_telnet_server,{unix,[telnet]}},
	    {require,ct_conn_log},
 	    {ct_hooks, [{cth_conn_log,[]}]}
	   ].

all() -> 
    [start_stop, send_and_get, expect, already_closed, 
     cmd, sendf, close_wrong_type].

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

start_stop(_Config) ->
    {ok, Handle} = ct_telnet:open(the_telnet_server),
    ok = ct_telnet:close(Handle),
    ok.
send_and_get(_) ->
    {ok, Handle} = ct_telnet:open(the_telnet_server),
    ok = ct_telnet:send(Handle, "ayt"),
    {ok, _Data} = ct_telnet:get_data(Handle),
    ok = ct_telnet:close(Handle),
    ok.
    
expect(_) ->
    {ok, Handle} = ct_telnet:open(the_telnet_server),
    ok = ct_telnet:send(Handle, "echo ayt"),
    ok = case ct_telnet:expect(Handle, ["ayt"]) of
	     {ok, _} ->
		 ok;
	     {error, {prompt, _}} ->
		 ok
	 end,
    ok = ct_telnet:close(Handle),
    ok.

already_closed(_) ->
    {ok, Handle} = ct_telnet:open(the_telnet_server),
    ok = ct_telnet:close(Handle),
    {error, already_closed} = ct_telnet:close(Handle),
    ok.

cmd(_) ->
    {ok, Handle} = ct_telnet:open(the_telnet_server),
    {ok, _} = ct_telnet:cmd(Handle, "display"),
    {ok, _} = ct_telnet:cmdf(Handle, "~s ~s", ["set", "bsasdel"]),
    ok = ct_telnet:close(Handle),
    ok.

sendf(_) ->
    {ok, Handle} = ct_telnet:open(the_telnet_server),
    ok = ct_telnet:sendf(Handle, "~s", ["ayt"]),
    ok = ct_telnet:close(Handle),
    ok.

close_wrong_type(_) ->
    {error, _} = ct_telnet:close(whatever),
    ok.
