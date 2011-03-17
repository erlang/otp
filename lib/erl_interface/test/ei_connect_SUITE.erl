%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
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

%%
-module(ei_connect_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("ei_connect_SUITE_data/ei_connect_test_cases.hrl").

-export([
	 all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, 
	 end_per_testcase/2,
	 
	 ei_send/1, 
	 ei_reg_send/1, 
	 ei_format_pid/1,
	 ei_rpc/1, 
	 rpc_test/1, 
	 ei_send_funs/1,
	 ei_threaded_send/1,
	 ei_set_get_tracelevel/1
	]).

-import(runner, [get_term/1,send_term/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [ei_send, ei_reg_send, ei_rpc, ei_format_pid, ei_send_funs,
     ei_threaded_send, ei_set_get_tracelevel].

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

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:minutes(0.25)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

ei_send(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = ei_connect(P, node()),

    ?line ok = ei_send(P, Fd, self(), AMsg={a,message}),
    ?line receive AMsg -> ok end,

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

ei_format_pid(Config) when is_list(Config) ->
    ?line S = self(),
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = ei_connect(P, node()),

    ?line ok = ei_format_pid(P, Fd, S),
    ?line receive S -> ok end,

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

ei_send_funs(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = ei_connect(P, node()),
    
    ?line Fun1 = fun ei_send/1,
    ?line Fun2 = fun(X) -> P, X, Fd, Fun1 end,

    ?line AMsg={Fun1,Fun2},
    %%AMsg={wait_with_funs, new_dist_format},
    ?line ok = ei_send_funs(P, Fd, self(), AMsg),
    ?line EIMsg = receive M -> M end,
    ?line EIMsg = AMsg,

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

ei_reg_send(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = ei_connect(P, node()),

    ARegName = a_strange_registred_name,
    ?line register(ARegName, self()),
    ?line ok = ei_reg_send(P, Fd, ARegName, AMsg={another,[strange],message}),
    ?line receive AMsg -> ok end,

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

ei_threaded_send(Config) when is_list(Config) ->
    ?line Einode = filename:join(?config(data_dir, Config), "einode"),
    ?line N = 15,
    ?line Host = atom_to_list(node()),
    ?line TestServerPid = self(),
    ?line [ spawn_link(fun() -> rec_einode(I, TestServerPid) end)
	    || I <- lists:seq(0, N-1) ],
    ?line [ receive {I,registered} -> ok end
	    || I <- lists:seq(0, N-1) ],
    ?line spawn_link(fun() -> start_einode(Einode, N, Host) end),
    ?line [ receive I -> ok end
	    || I <- lists:seq(0, N-1) ],
    ok.

rec_einode(N, TestServerPid) ->
    ?line Regname = list_to_atom("mth"++integer_to_list(N)),
    ?line register(Regname, self()),
    ?line TestServerPid ! {N, registered},
    ?line io:format("~p waiting~n", [Regname]),
    ?line receive
	      X ->
		  ?line io:format("Received by ~s ~p~n", [Regname, X]),
		  ?line TestServerPid ! N,
		  ?line X
	  after 10000 ->
		  ?line test_server:fail(Regname)
	  end.

start_einode(Einode, N, Host) ->
    Einodecmd = Einode ++ " " ++ atom_to_list(erlang:get_cookie())
	++ " " ++ integer_to_list(N) ++ " " ++ Host,
    io:format("Einodecmd  ~p ~n", [Einodecmd]),      
    ?line open_port({spawn, Einodecmd}, []),
    ok.

ei_rpc(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = ei_connect(P, node()),

    ?line S= "Hej du glade!", SRev = lists:reverse(S),
    ?line X = ei_rpc(P, Fd, self(), {?MODULE, rpc_test}, [SRev]),
    ?line {term, S}= X,

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

ei_set_get_tracelevel(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 5 = ei_set_get_tracelevel(P, 5),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = ei_connect(P, node()),

    ?line S= "Hej du glade!", SRev = lists:reverse(S),
    ?line X = ei_rpc(P, Fd, self(), {?MODULE, rpc_test}, [SRev]),
    ?line {term, S}= X,

    ?line 0 = ei_set_get_tracelevel(P, 0),

    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.


%%% Interface functions for ei (erl_interface) functions.

ei_connect_init(P, Num, Cookie, Creation) ->
    send_command(P, ei_connect_init, [Num,Cookie,Creation]),
    case get_term(P) of
	{term,Int} when is_integer(Int) -> Int
    end.

ei_connect(P, Node) ->
    send_command(P, ei_connect, [Node]),
    case get_term(P) of
	{term,{Fd,_}} when Fd >= 0 -> {ok,Fd};
	{term,{-1,Errno}} -> {error,Errno}
    end.

ei_set_get_tracelevel(P, Tracelevel) ->
    send_command(P, ei_set_get_tracelevel, [Tracelevel]),
    case get_term(P) of
	{term,{tracelevel, Level}} when is_integer(Level) -> Level
    end.

ei_send(P, Fd, To, Msg) ->
    send_command(P, ei_send, [Fd,To,Msg]),
    get_send_result(P).

ei_format_pid(P, Fd, To) ->
    send_command(P, ei_format_pid, [Fd, To]),
    get_send_result(P).

ei_send_funs(P, Fd, To, Msg) ->
    send_command(P, ei_send_funs, [Fd,To,Msg]),
    get_send_result(P).

ei_reg_send(P, Fd, To, Msg) ->
    send_command(P, ei_reg_send, [Fd,To,Msg]),
    get_send_result(P).

ei_rpc(P, Fd, To, Func, Msg) ->
    send_command(P, ei_rpc, [Fd, To, Func, Msg]),
    get_term(P).


get_send_result(P) ->
    case get_term(P) of
	{term,{0,_}} -> ok;
	{term,{1,_}} -> ok;
	{term,{-1,Errno}} -> {error,Errno};
	{term,{Res,Errno}}->
	    io:format("Return value: ~p\nerl_errno: ~p", [Res,Errno]),
	    ?t:fail(bad_return_value)
    end.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).

%%% Test function for RPC

rpc_test(S) ->    
    lists:reverse(S).
