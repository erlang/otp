%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : erlexec_SUITE.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Test erlexec's command line parsing
%%%
%%% Created : 22 May 2007 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(erlexec_SUITE).


%-define(line_trace, 1).

-define(DEFAULT_TIMEOUT, ?t:minutes(1)).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

-export([args_file/1, evil_args_file/1, env/1, args_file_env/1, otp_7461/1, otp_7461_remote/1, otp_8209/1, zdbbl_dist_buf_busy_limit/1]).

-include_lib("test_server/include/test_server.hrl").
    

init_per_testcase(Case, Config) ->
    Dog = ?t:timetrap(?DEFAULT_TIMEOUT),
    SavedEnv = save_env(),
    [{testcase, Case}, {watchdog, Dog}, {erl_flags_env, SavedEnv} |Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    SavedEnv = ?config(erl_flags_env, Config),
    restore_env(SavedEnv),
    cleanup_nodes(),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [args_file, evil_args_file, env, args_file_env,
     otp_7461, otp_8209, zdbbl_dist_buf_busy_limit].

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

otp_8209(doc) ->
    ["Test that plain first argument does not "
     "destroy -home switch [OTP-8209]"];
otp_8209(suite) ->
    [];
otp_8209(Config) when is_list(Config) ->
    ?line {ok,[[PName]]} = init:get_argument(progname),
    ?line SNameS = "erlexec_test_01",
    ?line SName = list_to_atom(SNameS++"@"++
			 hd(tl(string:tokens(atom_to_list(node()),"@")))),
    ?line Cmd = PName ++ " dummy_param -sname "++SNameS++" -setcookie "++
	atom_to_list(erlang:get_cookie()),
    ?line open_port({spawn,Cmd},[]),
    ?line pong = loop_ping(SName,40),
    ?line {ok,[[_]]} = rpc:call(SName,init,get_argument,[home]),
    ?line ["dummy_param"] = rpc:call(SName,init,get_plain_arguments,[]),
    ?line ok = cleanup_nodes(),
    ok.

cleanup_nodes() ->
    cleanup_node("erlexec_test_01",20).
cleanup_node(SNameS,0) ->
    {error, {would_not_die,list_to_atom(SNameS)}};
cleanup_node(SNameS,N) ->
    SName = list_to_atom(SNameS++"@"++
			 hd(tl(string:tokens(atom_to_list(node()),"@")))),
    case rpc:call(SName,init,stop,[]) of
	{badrpc,_} ->
	    ok;
	ok ->
	    receive after 500 -> ok end,
	    cleanup_node(SNameS,N-1)
    end.

loop_ping(_,0) ->
    pang;
loop_ping(Node,N) ->
    case net_adm:ping(Node) of
	pang ->
	    receive 
	    after 500 ->
		    ok 
	    end,
	    loop_ping(Node, N-1);
	pong ->
	    pong
    end.

args_file(doc) -> [];
args_file(suite) -> [];
args_file(Config) when is_list(Config) ->
    ?line AFN1 = privfile("1", Config),
    ?line AFN2 = privfile("2", Config),
    ?line AFN3 = privfile("3", Config),
    ?line AFN4 = privfile("4", Config),
    ?line AFN5 = privfile("5", Config),
    ?line AFN6 = privfile("6", Config),
    ?line write_file(AFN1,
		     "-MiscArg2~n"
		     "# a comment +\\#1000~n"
		     "+\\#200 # another comment~n"
		     "~n"
		     "# another config file to read~n"
		     " -args_file ~s#acomment~n"
		     "~n"
		     "-MiscArg7~n"
		     "#~n"
		     "+\\#700~n"
		     "-extra +XtraArg6~n",
	       [AFN2]),
    ?line write_file(AFN2,
		     "-MiscArg3~n"
		     "+\\#300~n"
		     "-args_file ~s~n"
		     "-MiscArg5~n"
		     "+\\#500#anothercomment -MiscArg10~n"
		     "-args_file ~s~n"
		     "-args_file ~s~n"
		     "-args_file ~s~n"
		     "-extra +XtraArg5~n",
		     [AFN3, AFN4, AFN5, AFN6]),
    ?line write_file(AFN3,
		     "# comment again~n"
		     " -MiscArg4 +\\#400 -extra +XtraArg1"),
    ?line write_file(AFN4,
		     " -MiscArg6 +\\#600 -extra +XtraArg2~n"
		     "+XtraArg3~n"
		     "+XtraArg4~n"
		     "# comment again~n"),
    ?line write_file(AFN5, ""),
    ?line write_file(AFN6, "-extra # +XtraArg10~n"),
    ?line CmdLine = "+#100 -MiscArg1 "
	++ "-args_file " ++ AFN1
	++ " +#800 -MiscArg8 -extra +XtraArg7 +XtraArg8",
    ?line {Emu, Misc, Extra} = emu_args(CmdLine),
    ?line verify_args(["-#100", "-#200", "-#300", "-#400",
		       "-#500", "-#600", "-#700", "-#800"], Emu),
    ?line verify_args(["-MiscArg1", "-MiscArg2", "-MiscArg3", "-MiscArg4",
		       "-MiscArg5", "-MiscArg6", "-MiscArg7", "-MiscArg8"],
		      Misc),
    ?line verify_args(["+XtraArg1", "+XtraArg2", "+XtraArg3", "+XtraArg4",
		       "+XtraArg5", "+XtraArg6", "+XtraArg7", "+XtraArg8"],
		      Extra),
    ?line verify_not_args(["-MiscArg10", "-#1000", "+XtraArg10",
			   "-MiscArg1", "-MiscArg2", "-MiscArg3", "-MiscArg4",
			   "-MiscArg5", "-MiscArg6", "-MiscArg7", "-MiscArg8",
			   "+XtraArg1", "+XtraArg2", "+XtraArg3", "+XtraArg4",
			   "+XtraArg5", "+XtraArg6", "+XtraArg7", "+XtraArg8"],
			  Emu),
    ?line verify_not_args(["-MiscArg10", "-#1000", "+XtraArg10",
			   "-#100", "-#200", "-#300", "-#400",
			   "-#500", "-#600", "-#700", "-#800",
			   "+XtraArg1", "+XtraArg2", "+XtraArg3", "+XtraArg4",
			   "+XtraArg5", "+XtraArg6", "+XtraArg7", "+XtraArg8"],
			  Misc),
    ?line verify_not_args(["-MiscArg10", "-#1000", "+XtraArg10",
			   "-#100", "-#200", "-#300", "-#400",
			   "-#500", "-#600", "-#700", "-#800",
			   "-MiscArg1", "-MiscArg2", "-MiscArg3", "-MiscArg4",
			   "-MiscArg5", "-MiscArg6", "-MiscArg7", "-MiscArg8"],
			  Extra),
    ?line ok.

evil_args_file(doc) -> [];
evil_args_file(suite) -> [];
evil_args_file(Config) when is_list(Config) ->
    ?line Lim = 300,
    ?line FNums = lists:seq(1, Lim),
    lists:foreach(fun (End) when End == Lim ->
			  ?line AFN = privfile(integer_to_list(End), Config),
			  ?line write_file(AFN,
					   "-MiscArg~p ",
					   [End]);
		      (I) ->
			  ?line AFNX = privfile(integer_to_list(I), Config),
			  ?line AFNY = privfile(integer_to_list(I+1), Config),
			  {Frmt, Args} =
			      case I rem 2 of
				  0 ->
				      {"-MiscArg~p -args_file ~s -MiscArg~p",
				       [I, AFNY, I]};
				  _ ->
				      {"-MiscArg~p -args_file ~s",
				       [I, AFNY]}
			      end,
			  ?line write_file(AFNX, Frmt, Args)
		  end,
		  FNums),
    ?line {_Emu, Misc, _Extra} = emu_args("-args_file "
					  ++ privfile("1", Config)),
    ?line ANums = FNums
	++ lists:reverse(lists:filter(fun (I) when I == Lim -> false;
					  (I) when I rem 2 == 0 -> true;
					  (_) -> false
				      end, FNums)),
    ?line verify_args(lists:map(fun (I) -> "-MiscArg"++integer_to_list(I) end,
				ANums),
		      Misc),
    ?line ok.
		      
			  

env(doc) -> [];
env(suite) -> [];
env(Config) when is_list(Config) ->
    ?line os:putenv("ERL_AFLAGS", "-MiscArg1 +#100 -extra +XtraArg1 +XtraArg2"),
    ?line CmdLine = "+#200 -MiscArg2 -extra +XtraArg3 +XtraArg4",
    ?line os:putenv("ERL_FLAGS", "-MiscArg3 +#300 -extra +XtraArg5"),
    ?line os:putenv("ERL_ZFLAGS", "-MiscArg4 +#400 -extra +XtraArg6"),
    ?line {Emu, Misc, Extra} = emu_args(CmdLine),
    ?line verify_args(["-#100", "-#200", "-#300", "-#400"], Emu),
    ?line verify_args(["-MiscArg1", "-MiscArg2", "-MiscArg3", "-MiscArg4"],
		      Misc),
    ?line verify_args(["+XtraArg1", "+XtraArg2", "+XtraArg3", "+XtraArg4",
		       "+XtraArg5", "+XtraArg6"],
		      Extra),
    ?line ok.

args_file_env(doc) -> [];
args_file_env(suite) -> [];
args_file_env(Config) when is_list(Config) ->
    ?line AFN1 = privfile("1", Config),
    ?line AFN2 = privfile("2", Config),
    ?line write_file(AFN1, "-MiscArg2 +\\#200 -extra +XtraArg1"),
    ?line write_file(AFN2, "-MiscArg3 +\\#400 -extra +XtraArg3"),
    ?line os:putenv("ERL_AFLAGS",
		    "-MiscArg1 +#100 -args_file "++AFN1++ " -extra +XtraArg2"),
    ?line CmdLine = "+#300 -args_file "++AFN2++" -MiscArg4 -extra +XtraArg4",
    ?line os:putenv("ERL_FLAGS", "-MiscArg5 +#500 -extra +XtraArg5"),
    ?line os:putenv("ERL_ZFLAGS", "-MiscArg6 +#600 -extra +XtraArg6"),
    ?line {Emu, Misc, Extra} = emu_args(CmdLine),
    ?line verify_args(["-#100", "-#200", "-#300", "-#400",
		       "-#500", "-#600"], Emu),
    ?line verify_args(["-MiscArg1", "-MiscArg2", "-MiscArg3", "-MiscArg4",
		       "-MiscArg5", "-MiscArg6"],
		      Misc),
    ?line verify_args(["+XtraArg1", "+XtraArg2", "+XtraArg3", "+XtraArg4",
		       "+XtraArg5", "+XtraArg6"],
		      Extra),
    ?line ok.

%% Make sure "erl -detached" survives when parent process group gets killed
otp_7461(doc) -> [];
otp_7461(suite) -> [];
otp_7461(Config) when is_list(Config) ->   
    case os:type() of
    	{unix,_} ->
	    {NetStarted, _} = net_kernel:start([test_server, shortnames]),
	    try
		net_kernel:monitor_nodes(true),
		register(otp_7461, self()),	    

		otp_7461_do(Config)		
	    after 
		catch unregister(otp_7461),
	        catch net_kernel:monitor_nodes(false),
	        case NetStarted of
		    ok -> net_kernel:stop();
		    _ -> ok
		end
	    end;
	_ ->
	    {skip,"Only on Unix."}
    end.
	
otp_7461_do(Config) ->
    io:format("alive=~p node=~p\n",[is_alive(), node()]),
    TestProg = filename:join([?config(data_dir, Config), "erlexec_tests"]),
    {ok, [[ErlProg]]} = init:get_argument(progname),
    ?line Cmd = TestProg ++ " " ++ ErlProg ++
	" -detached -sname " ++ get_nodename(otp_7461) ++
	" -setcookie " ++ atom_to_list(erlang:get_cookie()) ++
	" -pa " ++ filename:dirname(code:which(?MODULE)) ++
	" -s erlexec_SUITE otp_7461_remote init " ++ atom_to_list(node()),
    
    %% otp_7461 --------> erlexec_tests.c --------> cerl -detached
    %%          open_port                 fork+exec
    
    io:format("spawn port prog ~p\n",[Cmd]),
    ?line Port = open_port({spawn, Cmd}, [eof]),
    
    io:format("Wait for node to connect...\n",[]),    
    ?line {nodeup, Slave} = receive Msg -> Msg
			    after 20*1000 -> timeout end,
    io:format("Node alive: ~p\n", [Slave]),
    
    ?line pong = net_adm:ping(Slave),
    io:format("Ping ok towards ~p\n", [Slave]),
    
    ?line Port ! { self(), {command, "K"}}, % Kill child process group
    ?line {Port, {data, "K"}} = receive Msg2 -> Msg2 end,
    ?line port_close(Port),
    
    %% Now the actual test. Detached node should still be alive.
    ?line pong = net_adm:ping(Slave),
    io:format("Ping still ok towards ~p\n", [Slave]),
    
    %% Halt node
    ?line rpc:cast(Slave, ?MODULE, otp_7461_remote, [[halt, self()]]),
    
    ?line {nodedown, Slave} = receive Msg3 -> Msg3 
			      after 20*1000 -> timeout end,
    io:format("Node dead: ~p\n", [Slave]),
    ok.

      	    
%% Executed on slave node
otp_7461_remote([init, Master]) ->
    io:format("otp_7461_remote(init,~p) at ~p\n",[Master, node()]),
    net_kernel:connect_node(Master);
otp_7461_remote([halt, Pid]) ->
    io:format("halt order from ~p to node ~p\n",[Pid,node()]),
    halt().

zdbbl_dist_buf_busy_limit(doc) ->    
    ["Check +zdbbl flag"];
zdbbl_dist_buf_busy_limit(suite) ->
    [];
zdbbl_dist_buf_busy_limit(Config) when is_list(Config) ->
    LimKB = 1122233,
    LimB = LimKB*1024,
    ?line {ok,[[PName]]} = init:get_argument(progname),
    ?line SNameS = "erlexec_test_02",
    ?line SName = list_to_atom(SNameS++"@"++
                         hd(tl(string:tokens(atom_to_list(node()),"@")))),
    ?line Cmd = PName ++ " -sname "++SNameS++" -setcookie "++
        atom_to_list(erlang:get_cookie()) ++
	" +zdbbl " ++ integer_to_list(LimKB),
    ?line open_port({spawn,Cmd},[]),
    ?line pong = loop_ping(SName,40),
    ?line LimB = rpc:call(SName,erlang,system_info,[dist_buf_busy_limit]),
    ?line ok = cleanup_node(SNameS, 10),
    ok.
    

%%
%% Utils
%%

save_env() ->
    {erl_flags,
     os:getenv("ERL_AFLAGS"),
     os:getenv("ERL_FLAGS"),
     os:getenv("ERL_"++erlang:system_info(otp_release)++"_FLAGS"),
     os:getenv("ERL_ZFLAGS")}.

restore_env(EVar, false) when is_list(EVar) ->
    restore_env(EVar, "");
restore_env(EVar, "") when is_list(EVar) ->
    case os:getenv(EVar) of
	false -> ok;
	"" -> ok;
	" " -> ok;
	_ -> os:putenv(EVar, " ")
    end;
restore_env(EVar, Value) when is_list(EVar), is_list(Value) ->
    case os:getenv(EVar) of
	Value -> ok;
	_ -> os:putenv(EVar, Value)
    end.

restore_env({erl_flags, AFlgs, Flgs, RFlgs, ZFlgs}) ->
    restore_env("ERL_AFLAGS", AFlgs),
    restore_env("ERL_FLAGS", Flgs),
    restore_env("ERL_"++erlang:system_info(otp_release)++"_FLAGS", RFlgs),
    restore_env("ERL_ZFLAGS", ZFlgs),
    ok.

privfile(Name, Config) ->
    filename:join([?config(priv_dir, Config),
		   atom_to_list(?config(testcase, Config)) ++ "." ++ Name]).

write_file(FileName, Frmt) ->
    write_file(FileName, Frmt, []).

write_file(FileName, Frmt, Args) ->
    {ok, File} = file:open(FileName, [write]),
    io:format(File, Frmt, Args),
    ok = file:close(File).

verify_args([], _Ys) ->
    ok;
verify_args(Xs, []) ->
    exit({args_not_found_in_order, Xs});
verify_args([X|Xs], [X|Ys]) ->
    verify_args(Xs, Ys);
verify_args(Xs, [_Y|Ys]) ->
    verify_args(Xs, Ys).

verify_not_args(Xs, Ys) ->
    lists:foreach(fun (X) ->
			  case lists:member(X, Ys) of
			      true -> exit({arg_present, X});
			      false -> ok
			  end
		  end,
		  Xs).

emu_args(CmdLineArgs) ->
    io:format("CmdLineArgs = ~ts~n", [CmdLineArgs]),
    {ok,[[Erl]]} = init:get_argument(progname),
    EmuCL = os:cmd(Erl ++ " -emu_args_exit " ++ CmdLineArgs),
    io:format("EmuCL = ~ts", [EmuCL]),
    split_emu_clt(string:tokens(EmuCL, [$ ,$\t,$\n,$\r])).

split_emu_clt(EmuCLT) ->
    split_emu_clt(EmuCLT, [], [], [], emu).

split_emu_clt([], _Emu, _Misc, _Extra, emu) ->
    exit(bad_cmd_line);
split_emu_clt([], Emu, Misc, Extra, _Type) ->
    {lists:reverse(Emu), lists:reverse(Misc), lists:reverse(Extra)};

split_emu_clt(["--"|As], Emu, Misc, Extra, emu) ->
    split_emu_clt(As, Emu, Misc, Extra, misc);
split_emu_clt([A|As], Emu, Misc, Extra, emu = Type) ->
    split_emu_clt(As, [A|Emu], Misc, Extra, Type);

split_emu_clt(["-extra"|As], Emu, Misc, Extra, misc) ->
    split_emu_clt(As, Emu, Misc, Extra, extra);
split_emu_clt([A|As], Emu, Misc, Extra, misc = Type) ->
    split_emu_clt(As, Emu, [A|Misc], Extra, Type);

split_emu_clt([A|As], Emu, Misc, Extra, extra = Type) ->
    split_emu_clt(As, Emu, Misc, [A|Extra], Type).
    

get_nodename(T) ->
    atom_to_list(T)
	++ "-"
	++ atom_to_list(?MODULE)
	++ "-"
	++ integer_to_list(erlang:system_time(seconds))
	++ "-"
	++ integer_to_list(erlang:unique_integer([positive])).
