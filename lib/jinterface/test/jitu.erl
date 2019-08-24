%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% JInterface Test Utils
%%----------------------------------------------------------------------
-module(jitu).


-export([java/3,
	 java/4,
	 java/5,
	 init_all/1,
	 finish_all/1,
	 kill_all_jnodes/0]).

-include("ct.hrl").

%%
%% Lots of stuff here are originating from java_client_erl_server_SUITE.erl
%% (ic) ...
%%



java(Java, Dir, ClassAndArgs) ->
    cmd(Java++" -classpath "++classpath(Dir)++" "++ClassAndArgs).

java(Java, Dir, Class, Args) ->
    java(Java, Dir, Class++" "++to_string(Args)).

java(Java, Dir, Class, Args, Props) ->
    java(Java, Dir, Props++" "++Class, Args).




init_all(Config) when is_list(Config) ->
    case find_executable(["java"]) of
	false -> {skip,"Found no Java VM"};
	Path ->
	    Pid = spawn(fun() ->
				ets:new(jitu_tab,[set,named_table,public]),
				receive stop -> ets:delete(jitu_tab) end
			end),
	    [{java,Path},{tab_proc,Pid}|Config]
    end.

finish_all(Config) ->
    kill_all_jnodes(),
    ?config(tab_proc,Config) ! stop,
    Config.

kill_all_jnodes() ->
    Jnodes = ets:tab2list(jitu_tab),
    [begin
%	 ct:pal("Killing OsPid=~w started with ~p",[OsPid,_Cmd]),
	 kill_os_process(os:type(),integer_to_list(OsPid))
     end || {OsPid,_Cmd} <- Jnodes],
    ets:delete_all_objects(jitu_tab),
    ok.

kill_os_process({win32,_},OsPid) ->
    os:cmd("taskkill /PID " ++ OsPid);
kill_os_process(_,OsPid) ->
    os:cmd("kill " ++ OsPid).


%%
%% Internal stuff...
%%


find_executable([]) ->
    false;
find_executable([E|T]) ->
    case os:find_executable(E) of
	false -> find_executable(T);
	Path -> Path
    end.

to_string([H|T]) when is_integer(H) ->
    integer_to_list(H)++" "++to_string(T);
to_string([H|T]) when is_atom(H) ->
    atom_to_list(H)++" "++to_string(T);
to_string([H|T]) when is_pid(H) ->
    pid_to_list(H)++" "++to_string(T);
to_string([H|T]) when is_list(H) ->
    lists:flatten(H)++" "++to_string(T);
to_string([]) -> [].

% javac(Dir, File) ->
%     cmd("javac -d "++Dir++" -classpath "++classpath(Dir)++" "++
% 	filename:join(Dir, File)).

classpath(Dir) ->
    {PS,Quote,EscSpace} =
	case os:type() of
	    {win32, _} -> {";","\"",""};
	    _          -> {":","","\\"}
	end,
    es(Dir++PS++
	filename:join([code:lib_dir(jinterface),"priv","OtpErlang.jar"])++PS++
	os:getenv("CLASSPATH", ""),
       Quote,
       EscSpace).

es(L,Quote,EscSpace) ->
    Quote++lists:flatmap(fun($ ) ->
				EscSpace++" ";
			   (C) ->
				[C]
			end,lists:flatten(L)) ++ Quote.

cmd(Cmd) ->
    PortOpts = [{line,80},eof,exit_status,stderr_to_stdout],
    io:format("cmd: ~ts~n", [Cmd]),
    case catch open_port({spawn,Cmd}, PortOpts) of
	Port when is_port(Port) ->
	    case erlang:port_info(Port,os_pid) of
		{os_pid,OsPid} ->
		    ets:insert(jitu_tab,{OsPid,Cmd});
		_ ->
		    ok
	    end,
	    Result = cmd_loop(Port, []),
	    io:format("cmd res: ~w~n", [Result]),
	    case Result of
		0 -> ok;
		ExitCode when is_integer(ExitCode) -> {error,ExitCode};
		Error -> Error
	    end;
	{'EXIT',Reason} ->
	    {error,Reason}
    end.

cmd_loop(Port, Line) ->
    receive
	{Port,eof} ->
	    receive
		{Port,{exit_status,ExitStatus}} ->
		    ExitStatus
	    after 1 ->
		    undefined
	    end;
	{Port,{exit_status,ExitStatus}} ->
	    receive
		{Port,eof} ->
		    ok after 1 -> ok end,
	    ExitStatus;
	{Port,{data,{Tag,Data}}} ->
	    case Tag of
		eol ->
		    io:put_chars([Line|cr_to_nl(Data)]),
		    io:nl(),
		    cmd_loop(Port, []);
		noeol ->
		    cmd_loop(Port, [Line|cr_to_nl(Data)])
	    end;
	{'EXIT',Port,Reason} ->
	    {error,Reason};
	Other ->
	    io:format("WARNING: Unexpected at ~s:~p: ~p~n",
		      [?MODULE_STRING,?LINE,Other]),
	    cmd_loop(Port, Line)
    end.

%% Convert lonely CR to NL, and CRLF to NL
%%
cr_to_nl([$\r,$\n|T]) ->
    [$\n|cr_to_nl(T)];
cr_to_nl([$\r|T]) ->
    [$\n|cr_to_nl(T)];
cr_to_nl([C|T]) ->
    [C|cr_to_nl(T)];
cr_to_nl([]) ->
    [].
