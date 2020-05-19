%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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
-module(interactive_shell_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 get_columns_and_rows/1, exit_initial/1, job_control_local/1, 
	 job_control_remote/1,stop_during_init/1,
	 job_control_remote_noshell/1,ctrl_keys/1,
         get_columns_and_rows_escript/1,
         remsh/1, remsh_longnames/1, remsh_no_epmd/1]).

-export([init_per_testcase/2, end_per_testcase/2]).
%% For spawn
-export([toerl_server/3]).

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() -> 
    [get_columns_and_rows_escript,get_columns_and_rows,
     exit_initial, job_control_local,
     job_control_remote, job_control_remote_noshell,
     ctrl_keys, stop_during_init,
     remsh, remsh_longnames, remsh_no_epmd].

groups() -> 
    [].

init_per_suite(Config) ->
    Term = os:getenv("TERM", "dumb"),
    os:putenv("TERM","vt100"),
    DefShell = get_default_shell(),
    [{default_shell,DefShell},{term,Term}|Config].

end_per_suite(Config) ->
    Term = proplists:get_value(term,Config),
    os:putenv("TERM",Term),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-define(DEBUG,1).
-ifdef(DEBUG).
-define(dbg(Data),erlang:display(Data)).
-else.
-define(dbg(Data),noop).
-endif.

string_to_term(Str) ->
    {ok,Tokens,_EndLine} = erl_scan:string(Str ++ "."),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

run_unbuffer_escript(Rows, Columns, EScript, NoTermStdIn, NoTermStdOut) ->
    DataDir = filename:join(filename:dirname(code:which(?MODULE)), "interactive_shell_SUITE_data"),
    TmpFile = filename:join(DataDir, "tmp"),
    ok = file:write_file(TmpFile, <<>>),
    CommandModifier =
        case {NoTermStdIn, NoTermStdOut} of
            {false, false} -> "";
            {true, false} -> io_lib:format(" < ~s", [TmpFile]);
            {false, true} -> io_lib:format(" > ~s ; cat ~s", [TmpFile, TmpFile]);
            {true, true} -> io_lib:format(" > ~s < ~s ; cat ~s", [TmpFile, TmpFile, TmpFile])
        end,
    Command = io_lib:format("unbuffer -p bash -c \"stty rows ~p; stty columns ~p; escript ~s ~s\"",
                               [Rows, Columns, EScript, CommandModifier]),
    %% io:format("Command: ~s ~n", [Command]),
    Out = os:cmd(Command),
    %% io:format("Out: ~p ~n", [Out]),
    string_to_term(Out).

get_columns_and_rows_escript(Config) when is_list(Config) ->
    ExpectUnbufferInstalled =
        try
            "79" = string:trim(os:cmd("unbuffer -p bash -c \"stty columns 79 ; tput cols\"")),
            true
        catch
            _:_ -> false
        end,
    case ExpectUnbufferInstalled of
        false ->
            {skip,
             "The unbuffer tool (https://core.tcl-lang.org/expect/index) does not seem to be installed.~n"
             "On Ubuntu/Debian: \"sudo apt-get install expect\""};
        true ->
            DataDir = filename:join(filename:dirname(code:which(?MODULE)), "interactive_shell_SUITE_data"),
            IoColumnsErl = filename:join(DataDir, "io_columns.erl"),
            IoRowsErl = filename:join(DataDir, "io_rows.erl"),
            [
             begin
                 {ok, 42} = run_unbuffer_escript(99, 42, IoColumnsErl, NoTermStdIn, NoTermStdOut),
                 {ok, 99} = run_unbuffer_escript(99, 42, IoRowsErl, NoTermStdIn, NoTermStdOut)
             end
             ||
                {NoTermStdIn, NoTermStdOut} <- [{false, false}, {true, false}, {false, true}]
            ],
            {error,enotsup} = run_unbuffer_escript(99, 42, IoRowsErl, true, true),
            {error,enotsup} = run_unbuffer_escript(99, 42, IoColumnsErl, true, true),
            ok
    end.

%% Test that the shell can access columns and rows.
get_columns_and_rows(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
	old ->
	    %% Old shell tests
	    ?dbg(old_shell),
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,"io:columns()."},
		    {getline_re,".*{error,enotsup}"},
		    {putline,"io:rows()."},
		    {getline_re,".*{error,enotsup}"}

		   ],[]),
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,"io:columns()."},
		    {getline_re,".*{ok,90}"},
		    {putline,"io:rows()."},
		    {getline_re,".*{ok,40}"}],
		   [],
		   "stty rows 40; stty columns 90; ");
	new ->
	    %% New shell tests
	    ?dbg(new_shell),
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,"io:columns()."},
		    %% Behaviour change in R12B-5, returns 80
		    %%		  {getline,"{error,enotsup}"},
		    {getline,"{ok,80}"},
		    {putline,"io:rows()."},
		    %% Behaviour change in R12B-5, returns 24
		    %%		  {getline,"{error,enotsup}"}
		    {getline,"{ok,24}"}
		   ],[]),
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,"io:columns()."},
		    {getline,"{ok,90}"},
		    {putline,"io:rows()."},
		    {getline,"{ok,40}"}],
		   [],
		   "stty rows 40; stty columns 90; ")
    end.



%% Tests that exit of initial shell restarts shell.
exit_initial(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
	old ->
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline_re, ".*2"},
		    {putline,"exit()."},
		    {getline,""},
		    {getline,"Eshell"},
		    {putline,""},
		    {putline,"35."},
		    {getline_re,".*35"}],[]);
	new ->
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,"exit()."},
		    {getline,""},
		    {getline,"Eshell"},
		    {putline,""},
		    {putline,"35."},
		    {getline_re,"35"}],[])
    end.

stop_during_init(Config) when is_list(Config) ->
    case get_progs() of
	{error,_Reason} ->
	    {skip,"No runerl present"};
	{RunErl,_ToErl,Erl} ->
	    case create_tempdir() of
		{error, Reason2} ->
		    {skip, Reason2};
		Tempdir ->
		    XArg = " -kernel shell_history true -s init stop",
		    start_runerl_command(RunErl, Tempdir, "\\\""++Erl++"\\\""++XArg),
		    {ok, Binary} = file:read_file(filename:join(Tempdir, "erlang.log.1")),
		    nomatch = binary:match(Binary, <<"*** ERROR: Shell process terminated! ***">>)
	    end
     end.

%% Tests that local shell can be started by means of job control.
job_control_local(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
	old ->
	    %% Old shell tests
	    {skip,"No new shell found"};
	new ->
	    %% New shell tests
	    rtnode([{putline,""},
		    {putline, "2."},
		    {getline, "2"},
		    {putline,[7]},
		    {sleep,timeout(short)},
		    {putline,""},
		    {getline," -->"},
		    {putline,"s"},
		    {putline,"c"},
		    {putline_raw,""},
		    {getline,"Eshell"},
		    {putline_raw,""},
		    {getline,"1>"},
		    {putline,"35."},
		    {getline,"35"}],[])
    end.

job_control_remote(doc) -> [ "Tests that remote shell can be "
			     "started by means of job control" ];
job_control_remote(Config) when is_list(Config) ->
    case {node(),proplists:get_value(default_shell,Config)} of
	{nonode@nohost,_} ->
	    exit(not_distributed);
	{_,old} ->
	    {skip,"No new shell found"};
	_ ->
	    RNode = create_nodename(),
	    MyNode = atom2list(node()),
	    Pid = spawn_link(fun() ->
				     receive die ->
					     ok
				     end
			     end),
	    PidStr = pid_to_list(Pid),
	    register(kalaskula,Pid),
	    CookieString = lists:flatten(
			     io_lib:format("~w",
					   [erlang:get_cookie()])),
	    Res = rtnode([{putline,""},
			  {putline, "erlang:get_cookie()."},
			  {getline, CookieString},
			  {putline,[7]},
			  {sleep,timeout(short)},
			  {putline,""},
			  {getline," -->"},
			  {putline,"r '"++MyNode++"'"},
			  {putline,"c"},
			  {putline_raw,""},
			  {getline,"Eshell"},
			  {sleep,timeout(short)},
			  {putline_raw,""},
			  {getline,"("++MyNode++")1>"},
			  {putline,"whereis(kalaskula)."},
			  {getline,PidStr},
			  {sleep,timeout(short)}, % Race, known bug.
			  {putline_raw,"exit()."},
			  {getline,"***"},
			  {putline,[7]},
			  {putline,""},
			  {getline," -->"},
			  {putline,"c 1"},
			  {putline,""},
			  {sleep,timeout(short)},
			  {putline_raw,""},
			  {getline,"("++RNode++")"}],RNode),
	    Pid ! die,
	    Res
    end.

%% Tests that remote shell can be
%% started by means of job control to -noshell node.
job_control_remote_noshell(Config) when is_list(Config) ->
    case {node(),proplists:get_value(default_shell,Config)} of
	{nonode@nohost,_} ->
	    exit(not_distributed);
	{_,old} ->
	    {skip,"No new shell found"};
	_ ->
	    RNode = create_nodename(),
	    NSNode = start_noshell_node(interactive_shell_noshell),
	    Pid = spawn_link(NSNode, fun() ->
					     receive die ->
						     ok
					     end
				     end),
	    PidStr = rpc:call(NSNode,erlang,pid_to_list,[Pid]),
	    true = rpc:call(NSNode,erlang,register,[kalaskula,Pid]),
	    NSNodeStr = atom2list(NSNode),
	    CookieString = lists:flatten(
			     io_lib:format("~w",
					   [erlang:get_cookie()])),
	    Res = rtnode([{putline,""},
			  {putline, "erlang:get_cookie()."},
			  {getline, CookieString},
			  {putline,[7]},
			  {sleep,timeout(short)},
			  {putline,""},
			  {getline," -->"},
			  {putline,"r '"++NSNodeStr++"'"},
			  {putline,"c"},
			  {putline_raw,""},
			  {getline,"Eshell"},
			  {sleep,timeout(short)},
			  {putline_raw,""},
			  {getline,"("++NSNodeStr++")1>"},
			  {putline,"whereis(kalaskula)."},
			  {getline,PidStr},
			  {sleep,timeout(short)}, % Race, known bug.
			  {putline_raw,"exit()."},
			  {getline,"***"},
			  {putline,[7]},
			  {putline,""},
			  {getline," -->"},
			  {putline,"c 1"},
			  {putline,""},
			  {sleep,timeout(short)},
			  {putline_raw,""},
			  {getline,"("++RNode++")"}],RNode),
	    Pid ! die,
	    stop_noshell_node(NSNode),
	    Res
    end.

%% Tests various control keys.
ctrl_keys(_Conf) when is_list(_Conf) ->
    Cu=[$\^u],
    Cw=[$\^w],
    Cy=[$\^y],
    Home=[27,$O,$H],
    End=[27,$O,$F],
    rtnode([{putline,""},
	    {putline,"2."},
	    {getline,"2"},
	    {putline,"\"hello "++Cw++"world\"."},	% test <CTRL>+W
	    {getline,"\"world\""},
	    {putline,"\"hello "++Cu++"\"world\"."},	% test <CTRL>+U
	    {getline,"\"world\""},
	    {putline,"world\"."++Home++"\"hello "},	% test <HOME>
	    {getline,"\"hello world\""},
	    {putline,"world"++Home++"\"hello "++End++"\"."},	% test <END>
	    {getline,"\"hello world\""},
	    {putline,"\"hello world\""++Cu++Cy++"."},
	    {getline,"\"hello world\""}]
	   ++wordLeft()++wordRight(),[]).


wordLeft() ->
    L1=[27,27,$[,$D],
    L2=[27]++"[5D",
    L3=[27]++"[1;5D",
    wordLeft(L1)++wordLeft(L2)++wordLeft(L3).

wordLeft(Chars) ->
    End=[27,$O,$F],
    [{putline,"\"world\""++Chars++"hello "++End++"."},
     {getline,"\"hello world\""}].

wordRight() ->
    R1=[27,27,$[,$C],
    R2=[27]++"[5C",
    R3=[27]++"[1;5C",
    wordRight(R1)++wordRight(R2)++wordRight(R3).

wordRight(Chars) ->
    Home=[27,$O,$H],
    [{putline,"world"++Home++"\"hello "++Chars++"\"."},
     {getline,"\"hello world\""}].

%% Test that -remsh works
remsh(Config) when is_list(Config) ->
    case proplists:get_value(default_shell,Config) of
        old -> {skip,"Not supported in old shell"};
        new ->
            NodeStr = lists:flatten(io_lib:format("~p",[node()])),
            [_Name,Host] = string:split(atom_to_list(node()),"@"),
            Cmds = [{kill_emulator_command,sigint},
                    {putline,""},
                    {putline,"node()."},
                    {getline,NodeStr}],

            %% Test that remsh works with explicit -sname
            rtnode(Cmds ++ [{putline,"nodes()."},
                            {getline,"['Remshtest@"++Host++"']"}], [],
                   [], " -sname Remshtest -remsh " ++ NodeStr),

            %% Test that remsh works without -sname
            rtnode(Cmds, [], [], " -remsh " ++ NodeStr)


    end.

%% Test that -remsh works with long names
remsh_longnames(Config) when is_list(Config) ->

    case proplists:get_value(default_shell,Config) of
        old -> {skip,"Not supported in old shell"};
        new ->
            %% If we cannot resolve the domain, we need to add localhost to the longname
            Domain =
                case inet_db:res_option(domain) of
                    [] ->
                        "@127.0.0.1";
                    _ -> ""
                end,
            case rtstart(" -name " ++ atom_to_list(?FUNCTION_NAME)++Domain) of
                {ok, _SRPid, _STPid, SState} ->
                    {ok, _CRPid, CTPid, CState} =
                        rtstart("-name undefined" ++ Domain ++
                                    " -remsh " ++ atom_to_list(?FUNCTION_NAME)),
                    try
                        ok = get_and_put(
                               CTPid,
                               [{kill_emulator_command,sigint},
                                {putline,""},
                                {putline,"node()."},
                                {getline_re,atom_to_list(?FUNCTION_NAME)}], 1)
                    after
                        rtstop(CState), %% Stop client before server
                        rtstop(SState)
                    end;
                Else ->
                    Else
            end
    end.

%% Test that -remsh works without epmd
remsh_no_epmd(Config) when is_list(Config) ->

    case proplists:get_value(default_shell,Config) of
        old -> {skip,"Not supported in old shell"};
        new ->
            EPMD_ARGS = "-start_epmd false -erl_epmd_port 12345 ",
            case rtstart([],"ERL_EPMD_PORT=12345 ",
                         EPMD_ARGS ++ " -sname " ++ atom_to_list(?FUNCTION_NAME)) of
                {ok, _SRPid, _STPid, SState} ->
                    {ok, _CRPid, CTPid, CState} =
                        rtstart([],"ERL_EPMD_PORT=12345 ",
                                EPMD_ARGS ++ " -remsh "++atom_to_list(?FUNCTION_NAME)),
                    try
                        ok = get_and_put(
                               CTPid,
                               [{kill_emulator_command,sigint},
                                {putline,""},
                                {putline,"node()."},
                                {getline_re,atom_to_list(?FUNCTION_NAME)}], 1)
                    after
                        rtstop(CState), %% Stop client before server
                        rtstop(SState)
                    end;
                Else ->
                    Else
            end
    end.

rtnode(C,N) ->
    rtnode(C,N,[]).
rtnode(Commands,Nodename,ErlPrefix) ->
    rtnode(Commands,Nodename,ErlPrefix,[]).
rtnode(Commands,Nodename,ErlPrefix,Args) ->
    case rtstart(Nodename,ErlPrefix,Args) of
        {ok, _SPid, CPid, RTState} ->
            erase(getline_skipped),
            Res = (catch get_and_put(CPid, Commands, 1)),
            rtstop(RTState),
            ok = Res;
        Skip ->
            Skip
    end.

rtstart(Args) ->
    rtstart([],[],Args).
rtstart(Nodename,ErlPrefix,Args) ->
    case get_progs() of
	{error,_Reason} ->
	    {skip,"No runerl present"};
	{RunErl,ToErl,Erl} ->
	    case create_tempdir() of
		{error, Reason2} ->
		    {skip, Reason2};
		Tempdir ->
		    SPid =
			start_runerl_node(RunErl,ErlPrefix++"\\\""++Erl++"\\\"",
					  Tempdir,Nodename,Args),
		    CPid = start_toerl_server(ToErl,Tempdir),
                    {ok, SPid, CPid, {CPid, SPid, ToErl, Tempdir}}
            end
    end.

rtstop({CPid, SPid, ToErl, Tempdir}) ->
    case stop_runerl_node(CPid) of
        {error,_} ->
            CPid2 =
                start_toerl_server(ToErl,Tempdir),
            erase(getline_skipped),
            ok = get_and_put
                   (CPid2,
                    [{putline,[7]},
                     {sleep,
                      timeout(short)},
                     {putline,""},
                     {getline," -->"},
                     {putline,"s"},
                     {putline,"c"},
                     {putline,""}],1),
            stop_runerl_node(CPid2);
        _ ->
            ok
    end,
    wait_for_runerl_server(SPid),
    file:del_dir_r(Tempdir),
    ok.

timeout(long) ->
    2 * timeout(normal);
timeout(short) ->
    timeout(normal) div 10;
timeout(normal) ->
    10000 * test_server:timetrap_scale_factor().


start_noshell_node(Name) ->
    PADir =  filename:dirname(code:which(?MODULE)),
    {ok, Node} = test_server:start_node(Name,slave,[{args," -noshell -pa "++
							 PADir++" "}]),
    Node.
stop_noshell_node(Node) ->
    test_server:stop_node(Node).

get_and_put(_CPid,[],_) ->
    ok;
get_and_put(CPid, [{sleep, X}|T],N) ->
    ?dbg({sleep, X}),
    receive
    after X ->
	    get_and_put(CPid,T,N+1)
    end;
get_and_put(CPid, [{kill_emulator_command, Cmd}|T],N) ->
    ?dbg({kill_emulator_command, Cmd}),
    CPid ! {self(), {kill_emulator_command, Cmd}},
    receive
        {kill_emulator_command,_Res} ->
            get_and_put(CPid,T,N)
    end;
get_and_put(CPid, [{getline, Match}|T],N) ->
    ?dbg({getline, Match}),
    CPid ! {self(), {get_line, timeout(normal)}},
    receive
	{get_line, timeout} ->
	    error_logger:error_msg("~p: getline timeout waiting for \"~s\" "
				   "(command number ~p, skipped: ~p)~n",
				   [?MODULE, Match,N,get(getline_skipped)]),
	    {error, timeout};
	{get_line, Data} ->
	    ?dbg({data,Data}),
	    case lists:prefix(Match, Data) of
		true ->
		    erase(getline_skipped),
		    get_and_put(CPid, T,N+1);
		false ->
		    case get(getline_skipped) of
			undefined ->
			    put(getline_skipped,[Data]);
			List ->
			    put(getline_skipped,List ++ [Data])
		    end,
		    get_and_put(CPid,  [{getline, Match}|T],N)
	    end
    end;

%% Hey ho copy paste from stdlib/io_proto_SUITE
get_and_put(CPid, [{getline_re, Match}|T],N) ->
    ?dbg({getline_re, Match}),
    CPid ! {self(), {get_line, timeout(normal)}},
    receive
	{get_line, timeout} ->
	    error_logger:error_msg("~p: getline_re timeout waiting for \"~s\" "
				   "(command number ~p, skipped: ~p)~n",
				   [?MODULE, Match,N,get(getline_skipped)]),
	    {error, timeout};
	{get_line, Data} ->
	    ?dbg({data,Data}),
	    case re:run(Data, Match,[{capture,none}]) of
		match ->
		    erase(getline_skipped),
		    get_and_put(CPid, T,N+1);
		_ ->
		    case get(getline_skipped) of
			undefined ->
			    put(getline_skipped,[Data]);
			List ->
			    put(getline_skipped,List ++ [Data])
		    end,
		    get_and_put(CPid,  [{getline_re, Match}|T],N)
	    end
    end;

get_and_put(CPid, [{putline_raw, Line}|T],N) ->
    ?dbg({putline_raw, Line}),
    CPid ! {self(), {send_line, Line}},
    Timeout = timeout(normal),
    receive
	{send_line, ok} ->
	    get_and_put(CPid, T,N+1)
    after Timeout ->
	    error_logger:error_msg("~p: putline_raw timeout (~p) sending "
				   "\"~s\" (command number ~p)~n",
				   [?MODULE, Timeout, Line, N]),
	    {error, timeout}
    end;

get_and_put(CPid, [{putline, Line}|T],N) ->
    ?dbg({putline, Line}),
    CPid ! {self(), {send_line, Line}},
    Timeout = timeout(normal),
    receive
	{send_line, ok} ->
	    get_and_put(CPid, [{getline, []}|T],N)
    after Timeout ->
	    error_logger:error_msg("~p: putline timeout (~p) sending "
				   "\"~s\" (command number ~p)~n[~p]~n",
				   [?MODULE, Timeout, Line, N,get()]),
	    {error, timeout}
    end.

wait_for_runerl_server(SPid) ->
    Ref = erlang:monitor(process, SPid), 
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, SPid, _} ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.



stop_runerl_node(CPid) ->
    Ref = erlang:monitor(process, CPid),
    CPid ! {self(), kill_emulator},
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, CPid, noproc} ->
	    ok;
	{'DOWN', Ref, process, CPid, normal} ->
	    ok;
	{'DOWN', Ref, process, CPid, {error, Reason}} ->
	    {error, Reason}
    after Timeout ->
	    {error, timeout}
    end.

get_progs() ->
    case os:type() of
	{unix,freebsd} ->
	    {error,"cant use run_erl on freebsd"};
	{unix,openbsd} ->
	    {error,"cant use run_erl on openbsd"};
	{unix,_} ->
	    case os:find_executable("run_erl") of
		RE when is_list(RE) ->
		    case  os:find_executable("to_erl") of
			TE when is_list(TE) ->
			    case os:find_executable("erl") of
				E when is_list(E) ->
				    {RE,TE,E};
				_ ->
				    {error, "Could not find erl command"}
			    end;
			_ ->
			    {error, "Could not find to_erl command"}
		    end;
		_ ->
		    {error, "Could not find run_erl command"}
	    end;
	_ ->
	    {error, "Not a unix OS"}
    end.

create_tempdir() ->
    create_tempdir(filename:join(["/tmp","rtnode"++os:getpid()]),$A).

create_tempdir(Dir,X) when X > $Z, X < $a ->
    create_tempdir(Dir,$a);
create_tempdir(Dir,X) when X > $z -> 
    Estr = lists:flatten(
	     io_lib:format("Unable to create ~s, reason eexist",
			   [Dir++[$z]])),
    {error, Estr};
create_tempdir(Dir0, Ch) ->
    %% Expect fairly standard unix.
    Dir = Dir0++[Ch],
    case file:make_dir(Dir) of
	{error, eexist} ->
	    create_tempdir(Dir0, Ch+1);
	{error, Reason} ->
	    Estr = lists:flatten(
		     io_lib:format("Unable to create ~s, reason ~p",
				   [Dir,Reason])),
	    {error,Estr};
	ok ->
	    Dir
    end.

create_nodename() ->
    create_nodename($A).

create_nodename(X) when X > $Z, X < $a ->
    create_nodename($a);
create_nodename(X) when X > $z -> 
    {error,out_of_nodenames};
create_nodename(X) ->
    NN = "rtnode"++os:getpid()++[X],
    case file:read_file_info(filename:join(["/tmp",NN])) of
	{error,enoent} ->
	    Host = lists:nth(2,string:tokens(atom_to_list(node()),"@")),
	    NN++"@"++Host;
	_ ->
	    create_nodename(X+1)
    end.


start_runerl_node(RunErl,Erl,Tempdir,Nodename,Args) ->
    XArg = case Nodename of
	       [] ->
		   [];
	       _ ->
		   " -sname "++(if is_atom(Nodename) -> atom_to_list(Nodename);
				   true -> Nodename
				end)++
		       " -setcookie "++atom_to_list(erlang:get_cookie())
	   end ++ " " ++ Args,
    spawn(fun() -> start_runerl_command(RunErl, Tempdir, Erl++XArg) end).

start_runerl_command(RunErl, Tempdir, Cmd) ->
    FullCmd = "\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++" \""++Cmd++"\"",
    ct:pal("~s",[FullCmd]),
    os:cmd(FullCmd).

start_toerl_server(ToErl,Tempdir) ->
    Pid = spawn(?MODULE,toerl_server,[self(),ToErl,Tempdir]),
    receive
	{Pid,started} ->
	    Pid;
	{Pid,error,Reason} ->
	    {error,Reason}
    end.

try_to_erl(_Command, 0) ->
    {error, cannot_to_erl};
try_to_erl(Command, N) ->
    ?dbg({?LINE,N}),
    Port = open_port({spawn, Command},[eof,{line,1000}]),
    Timeout = timeout(normal) div 2,
    receive
	{Port, eof} -> 	
	    receive after Timeout ->
			    ok
		    end,
	    try_to_erl(Command, N-1)
    after Timeout ->
	    ?dbg(Port),
	    Port
    end.

toerl_server(Parent,ToErl,Tempdir) ->
    Port = try_to_erl("\""++ToErl++"\" "++Tempdir++"/ 2>/dev/null",8),
    case Port of
	P when is_port(P) ->
	    Parent ! {self(),started};
	{error,Other} ->
	    Parent ! {self(),error,Other},
	    exit(Other)
    end,
    case toerl_loop(Port,[]) of
	normal ->
	    ok;
	{error, Reason} ->
	    error_logger:error_msg("toerl_server exit with reason ~p~n",
				   [Reason]),
	    exit(Reason)
    end.

toerl_loop(Port,Acc) ->
    ?dbg({toerl_loop, Port, Acc}),
    receive
	{Port,{data,{Tag0,Data}}} when is_port(Port) ->
	    ?dbg({?LINE,Port,{data,{Tag0,Data}}}),
	    case Acc of
		[{noeol,Data0}|T0] ->
		    toerl_loop(Port,[{Tag0, Data0++Data}|T0]);
		_ ->
		    toerl_loop(Port,[{Tag0,Data}|Acc])
	    end;
	{Pid,{get_line,Timeout}} ->
	    case Acc of
		[] ->
		    case get_data_within(Port,Timeout,[]) of
			timeout ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[]);
			{noeol,Data1} ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[{noeol,Data1}]);
			{eol,Data2} ->
			    Pid ! {get_line, Data2}, 
			    toerl_loop(Port,[])
		    end;
		[{noeol,Data3}] ->
		    case get_data_within(Port,Timeout,Data3) of
			timeout ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,Acc);
			{noeol,Data4} ->
			    Pid ! {get_line, timeout},
			    toerl_loop(Port,[{noeol,Data4}]);
			{eol,Data5} ->
			    Pid ! {get_line, Data5},
			    toerl_loop(Port,[])
		    end;
		List ->
		    {NewAcc,[{eol,Data6}]} = lists:split(length(List)-1,List),
		    Pid ! {get_line,Data6},
		    toerl_loop(Port,NewAcc)
	    end;
	{Pid, {send_line, Data7}} ->
	    Port ! {self(),{command, Data7++"\n"}},
	    Pid ! {send_line, ok},
	    toerl_loop(Port,Acc);
        {Pid, {kill_emulator_command, Cmd}} ->
            put(kill_emulator_command, Cmd),
            Pid ! {kill_emulator_command, ok},
	    toerl_loop(Port,Acc);
	{_Pid, kill_emulator} ->
            case get(kill_emulator_command) of
                undefined ->
                    Port ! {self(),{command, "init:stop().\n"}};
                sigint ->
                    Port ! {self(),{command, [3]}},
                    timer:sleep(200),
                    Port ! {self(),{command, "a\n"}}
            end,
	    Timeout1 = timeout(long),
	    receive
		{Port,eof} ->
		    normal
	    after Timeout1 ->
		    {error, kill_timeout}
	    end;
	{Port, eof} ->
	    {error, unexpected_eof};
	Other ->
	    {error, {unexpected, Other}}
    end.

millistamp() ->
    erlang:monotonic_time(millisecond).

get_data_within(Port, X, Acc) when X =< 0 ->
    ?dbg({get_data_within, X, Acc, ?LINE}),
    receive
	{Port,{data,{Tag0,Data}}} ->
	    ?dbg({?LINE,Port,{data,{Tag0,Data}}}),
	    {Tag0, Acc++Data}
    after 0 ->
	    case Acc of
		[] ->
		    timeout;
		Noeol ->
		    {noeol,Noeol}
	    end
    end;


get_data_within(Port, Timeout, Acc) ->	
    ?dbg({get_data_within, Timeout, Acc, ?LINE}),
    T1 = millistamp(),
    receive 
	{Port,{data,{noeol,Data}}} ->
	    ?dbg({?LINE,Port,{data,{noeol,Data}}}),
	    Elapsed = millistamp() - T1 + 1,
	    get_data_within(Port, Timeout - Elapsed, Acc ++ Data); 
	{Port,{data,{eol,Data1}}} ->
	    ?dbg({?LINE,Port,{data,{eol,Data1}}}),
	    {eol, Acc ++ Data1}
    after Timeout ->
	    timeout
    end.

get_default_shell() ->
    try
        rtnode([{putline,""},
                {putline, "whereis(user_drv)."},
                {getline, "undefined"}],[]),
        old
    catch _E:_R ->
            ?dbg({_E,_R}),
            new
    end.

atom2list(A) ->
    lists:flatten(io_lib:format("~s", [A])).
