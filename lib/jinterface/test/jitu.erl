%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% JInterface Test Utils
%%----------------------------------------------------------------------
-module(jitu).


-export([java/3,
	 java/4,
	 java/5,
	 init_all/1,
	 finish_all/1]).

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




init_all(Config) when list(Config) ->
    case find_executable(["java"]) of
	false -> {skip,"Found no Java VM"};
	Path -> [{java,Path}|Config]
    end.

finish_all(Config) -> Config.

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

to_string([H|T]) when integer(H) ->
    integer_to_list(H)++" "++to_string(T);
to_string([H|T]) when atom(H) ->
    atom_to_list(H)++" "++to_string(T);
to_string([H|T]) when pid(H) ->
    pid_to_list(H)++" "++to_string(T);
to_string([H|T]) when list(H) ->
    lists:flatten(H)++" "++to_string(T);
to_string([]) -> [].

% javac(Dir, File) ->
%     cmd("javac -d "++Dir++" -classpath "++classpath(Dir)++" "++
% 	filename:join(Dir, File)).

classpath(Dir) ->
    PS =
	case os:type() of
	    {win32, _} -> ";";
	    _          -> ":"
	end,
    Dir++PS++
	filename:join([code:lib_dir(jinterface),"priv","OtpErlang.jar"])++PS++
	case os:getenv("CLASSPATH") of
	    false -> "";
	    Classpath -> Classpath
	end.


cmd(Cmd) ->
    PortOpts = [{line,80},eof,exit_status,stderr_to_stdout],
    io:format("cmd: ~s~n", [Cmd]),
    case catch open_port({spawn,Cmd}, PortOpts) of
	Port when port(Port) ->
	    Result = cmd_loop(Port, []),
	    io:format("cmd res: ~w~n", [Result]),
	    case Result of
		0 -> ok;
		ExitCode when integer(ExitCode) -> {error,ExitCode};
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
