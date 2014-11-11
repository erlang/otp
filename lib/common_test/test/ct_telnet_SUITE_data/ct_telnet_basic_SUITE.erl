%% Modify your ts.unix.config or ts.win32.config file before running these tests
-module(ct_telnet_basic_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(no_of_sessions, 4).
-define(conn_name(N), (list_to_atom("telnet_server_conn"++integer_to_list(N)))).
-define(get_n(Cfg), (proplists:get_value(n, Cfg))).
	       
%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

suite() -> [
	    {require,ct_conn_log},
 	    {ct_hooks, [{cth_conn_log,[]}]}
	   ].

operations() -> 
    [start_stop, send_and_get, expect, already_closed, 
     cmd, sendf, no_newline, close_wrong_type].

mult_case(_Case, 0) ->
    [];
mult_case(Case, N) ->
    [list_to_atom(atom_to_list(Case)++integer_to_list(N)) | 
     mult_case(Case, N-1)].

groups() -> 
    [{single_connection,[],operations()},
     {multiple_connections,[parallel],
      lists:reverse(mult_case(sessions,?no_of_sessions))}].

all() ->
    [{group,single_connection},
     {group,multiple_connections}].

init_per_suite(Config) ->
    ct:pal("Will use these log hook options: ~p",
	   [ct:get_config(ct_conn_log,[])]),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(Group, Config) ->
    if Group == single_connection ->
	    ct:require(?conn_name(1),{unix,[telnet]});
       true ->
	    ok
    end,
    [{n,1} | Config].

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) when (Case == sessions1) or
				     (Case == sessions2) or
				     (Case == sessions3) or
				     (Case == sessions4) ->
    N = lists:last(atom_to_list(Case))-48,
    ct:log("Using connection ~w for session ~w on ~w",
	   [?conn_name(N),N,self()]),
    ct:require(?conn_name(N),{unix,[telnet]}),
    [{n,N} | proplists:delete(n,Config)];
init_per_testcase(Case, Config) ->
    ct:log("Testcase ~w using connection ~w",
	   [Case,?conn_name(?get_n(Config))]),
    Config.

end_per_testcase(_Case, _) ->
    ok.

%%%-----------------------------------------------------------------
%%% TEST CASES

sessions(Config) ->
    [apply(?MODULE,Op,[Config]) || Op <- operations()],
    ok.

sessions1(Config) -> sessions(Config).
sessions2(Config) -> sessions(Config).
sessions3(Config) -> sessions(Config).
sessions4(Config) -> sessions(Config).

start_stop(Config) ->
    ct:log("Opening ~w...", [?conn_name(?get_n(Config))]),
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    ok = ct_telnet:close(Handle),
    ok.

send_and_get(Config) ->
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    ok = ct_telnet:send(Handle, "ayt"),
    {ok, _Data} = ct_telnet:get_data(Handle),
    ok = ct_telnet:close(Handle),
    ok.
    
expect(Config) ->
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    ok = ct_telnet:send(Handle, "echo ayt"),
    ok = case ct_telnet:expect(Handle, ["ayt"]) of
	     {ok, _} ->
		 ok;
	     {error, {prompt, _}} ->
		 ok
	 end,
    ok = ct_telnet:close(Handle),
    ok.

already_closed(Config) ->
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    ok = ct_telnet:close(Handle),
    {error, already_closed} = ct_telnet:close(Handle),
    ok.

cmd(Config) ->
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    {ok, _} = ct_telnet:cmd(Handle, "display"),
    {ok, _} = ct_telnet:cmdf(Handle, "~s ~s", ["set", "bsasdel"]),
    ok = ct_telnet:close(Handle),
    ok.

sendf(Config) ->
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    ok = ct_telnet:sendf(Handle, "~s", ["ayt"]),
    ok = ct_telnet:close(Handle),
    ok.

no_newline(Config) ->
    {ok, Handle} = ct_telnet:open(?conn_name(?get_n(Config))),
    IAC = 255, % interprete as command
    AYT = 246, % are you there
    ok = ct_telnet:send(Handle, [IAC,AYT], [{newline,false}]),
    {ok,_} = ct_telnet:expect(Handle,"yes",[no_prompt_check]),
    {ok,_} = ct_telnet:cmd(Handle, ""), % send newline only to get back prompt
    ok = ct_telnet:close(Handle),
    ok.

close_wrong_type(_) ->
    {error, _} = ct_telnet:close(whatever),
    ok.

%%%-----------------------------------------------------------------
%%% HELP FUNCS

	    
