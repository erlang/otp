%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%% To be used from makefiles on the unix side executing things on the NT-side
-module(ntbuild).

-export([nmake/1, omake/1, waitnode/1, restart/1,
	 setdir/1, run_tests/1, run_command/1]).
-export([serv_nmake/2, serv_omake/2, serv_restart/0, serv_run_tests/2,
	 serv_run_command/1]).

waitnode([NtNode]) ->
    % First, wait for node to disappear.
    case wait_disappear(NtNode, 0) of
	ok ->
	    case wait_appear(NtNode, 0) of
		ok ->
		    halt(0);
		fail ->
		    halt(1)
	    end;
	fail ->
	    halt(1)
    end.

% Wait for nt node to appear within 5 minutes.
wait_appear(_NtNode, 300) ->
    fail;
wait_appear(NtNode, N) ->
    receive after 1000 -> ok end,
    case nt_node_alive(NtNode, quiet) of
	no ->
	    wait_appear(NtNode, N+1);
	yes ->
	    ok
    end.
    


% Waits for nt node to disappear within 3 minutes.
wait_disappear(NtNode, 300) ->
    fail;
wait_disappear(NtNode, N) ->
    receive after 1000 -> ok end,
    case nt_node_alive(NtNode, quiet) of
	yes ->
	    wait_disappear(NtNode, N+1);
	no ->
	    ok
    end.
			    
restart([NtNode]) ->
    case nt_node_alive(NtNode) of
	yes ->
	    case rpc:call(NtNode, ntbuild, serv_restart, []) of
		ok ->
		    io:format("halt(0)~n"),
		    halt();
		Error ->
		    io:format("halt(1)~n"),
		    halt(1)
	    end;
	no ->
	    halt(1)
    end.


setdir([NtNode, Dir0]) ->    
    Dir = atom_to_list(Dir0),
    case nt_node_alive(NtNode) of
	yes ->
	    case rpc:call(NtNode, file, set_cwd, [Dir]) of
		ok ->
		    io:format("halt(0)~n"),
		    halt();
		Error ->
		    io:format("halt(1) (Error: ~p) (~p not found) ~n", [Error, Dir]),
		    halt(1)
	    end;
	no ->
	    halt(1)
    end.

run_tests([NtNode, Vsn0, Logdir]) ->
    Vsn = atom_to_list(Vsn0),
    case nt_node_alive(NtNode) of
	yes ->
	    case rpc:call(NtNode, ntbuild, serv_run_tests, [Vsn, Logdir]) of
		ok ->
		    io:format("halt(0)~n"),
		    halt();
		Error ->
		    io:format("RPC To Windows Node Failed: ~p~n", [Error]),
		    io:format("halt(1)~n"),
		    halt(1)
	    end;
	no ->
	    halt(1)
    end.

run_command([NtNode, Cmd]) ->
    case nt_node_alive(NtNode) of
	yes ->
	    case rpc:call(NtNode, ntbuild, serv_run_command, [Cmd]) of
		ok ->
		    io:format("halt(0)~n"),
		    halt();
		Error ->
		    io:format("RPC To Windows Node Failed: ~p~n", [Error]),
		    io:format("halt(1)~n"),
		    halt(1)
	    end;
	no ->
	    halt(1)
    end.

nmake([NtNode, Path, Options]) ->
%    io:format("nmake2(~w,~w)~n",[Path, Options]),
    Dir=atom_to_list(Path),
    Opt=atom_to_list(Options),
    case nt_node_alive(NtNode) of
	yes ->
	    case rpc:call(NtNode, ntbuild, serv_nmake, [Dir, Opt]) of
		ok ->
		    io:format("halt(0)~n"),
		    halt();
		Error ->
		    io:format("Error: ~n", [Error]),
		    halt(1)
	    end;
	no ->
	    halt(1)
    end.

omake([NtNode, Path, Options]) ->
    Dir=atom_to_list(Path),
    Opt=atom_to_list(Options),
    case nt_node_alive(NtNode) of
	yes ->
	    case rpc:call(NtNode, ntbuild, serv_omake, [Dir, Opt]) of
		ok ->
		    io:format("halt(0)~n"),
		    halt();
		Error ->
		    io:format("RPC To Windows Node Failed: ~p~n", [Error]),
		    io:format("~p        ~p~n", [Dir, Opt]),
		    io:format("halt(1)~n"),
		    halt(1)
	    end;
	no ->
	    halt(1)
    end.





nt_node_alive(NtNode) ->
    case net:ping(NtNode) of
	pong ->
	    yes;
	pang ->
	    io:format("The NT node (~p) is not up. ~n",[NtNode]),
	    no
    end.

nt_node_alive(NtNode, quiet) ->
    case net:ping(NtNode) of
	pong ->
	    yes;
	pang ->
	    no
    end.



%%%
%%% The 'serv_' functions. Theese are the routines run on the WinNT node.
%%%

%%-----------------------
%% serv_run_tests()
%% Runs the tests.
serv_run_tests(Vsn, Logdir) ->
    {ok, Cwd}=file:get_cwd(),
    io:format("serv_run_tests ~p ~p ~n", [Vsn, Logdir]),
    Cmd0= "set central_log_dir=" ++ Logdir,
    Erl = "C:/progra~1/erl"++Vsn++"/bin/erl",
    Cmd1 = Erl++" -sname a -setcookie a -noshell -noinput -s ts install -s ts run -s ts save -s erlang halt",
%%    Dir = "C:/temp/test_suite/test_server",
    Cmd= Cmd0 ++ "/r/n" ++ Cmd1,
    Dir = "C:/temp/test_server/p7a/test_server",
    file:set_cwd(Dir),
    Res=run_make_bat(Dir, Cmd),
    file:set_cwd(Cwd),
    Res.

%%-----------------------
%% serv_run_command()
%% Runs a command.
serv_run_command(Cmd) ->
    {ok, Cwd}=file:get_cwd(),
    Res=run_make_bat("", Cmd),
    file:set_cwd(Cwd),
    Res.

%%-----------------------
%% serv_restart()
%% Reboots the NT machine.
serv_restart() ->
    Exe="\\erts\\install_nt\\reboot.exe",
    open_port({spawn, Exe}, [stream, eof, in]),
    ok.


%%-----------------------
%% serv_nmake(Path, Options) 
%% Runs `nmake' in the given directory.
%% Result: ok | error
serv_nmake(Path, Options) ->
    {ok, Cwd}=file:get_cwd(),
    Command="nmake -e -f Makefile.win32 " ++ Options ++	" 2>&1",
    Res=run_make_bat(Path, Command),
    file:set_cwd(Cwd),
    Res.

%%-----------------------
%% serv_omake(Path, Options) 
%% Runs `omake' in the given directory.
%% Result: ok | error
serv_omake(Path, Options) ->
    {ok, Cwd}=file:get_cwd(),
    Command="omake -W -E -EN -f Makefile.win32 " ++ Options ++ " 2>&1",
    Res=run_make_bat(Path, Command),
    file:set_cwd(Cwd),
    Res.


read_output(Port, SoFar) ->
%    io:format("(read_output)~n"),
    case get_data_from_port(Port) of
	eof ->
	    io:format("*** eof ***~n"),
	    io:format("Never reached a real message"),
	    halt(1);
	{ok, Data} ->
	    case print_line([SoFar|Data]) of
		{ok, Rest} ->
		    read_output(Port, Rest);
		{done, Res} ->
		    Res
	    end
    end.

print_line(Data) ->
    print_line(Data, []).

print_line([], Acc) ->
    {ok, lists:reverse(Acc)};
print_line([$*,$o,$k,$*|Rest], _Acc) ->
    io:format("*ok*~n"),
    {done, ok};
print_line([$*,$e,$r,$r,$o,$r|Rest], _Acc) ->
    io:format("*error*~n"),
    {done, error};
print_line([$\r,$\n|Rest], Acc) ->
    io:format("~s~n", [lists:reverse(Acc)]),
    print_line(Rest, []);
print_line([Chr|Rest], Acc) ->
    print_line(Rest, [Chr|Acc]).
    
get_data_from_port(Port) ->
    receive
	{Port, {data, Bytes}} ->
	    {ok, Bytes};
	{Port, eof} ->
	    unlink(Port),
	    exit(Port, die),
	    eof;
	Other ->
	    io:format("Strange message received: ~p~n", [Other]),
	    get_data_from_port(Port)
    end.


run_make_bat(Dir, Make) ->
    {Name, Exe, Script}=create_make_script(Dir, Make),
    io:format("Exe:~p  Cwd:~p Script:~p ~n",[Exe, Dir, Script]),
    case file:write_file(Name, Script) of
	ok ->
	    case catch open_port({spawn, Exe}, [stderr_to_stdout, stream, hide,
						eof, in]) of
		Port when port(Port) ->
		    read_output(Port, []);
		Other ->
		    io:format("Error, open_port failed: ~p~n", [Other]),
		    {open_port, Other, Exe}
	    end;
	Error ->
	    {write_file, Error, Name}
    end.

create_make_script(Dir, Make) when atom(Make) ->
    create_make_script(Dir, atom_to_list(Make));
create_make_script(Dir, Make) ->
    {"run_make_bs.bat",
     "run_make_bs 2>&1",
     ["@echo off\r\n",
      "@cd ", Dir, "\r\n",
      Make++"\r\n",
      "if errorlevel 1 echo *run_make_bs error*\r\n",
      "if not errorlevel 1 echo *ok*\r\n"]}.





