%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Test suite for the backends of the IDL compiler
%%%----------------------------------------------------------------------

-module(java_client_erl_server_SUITE).
-include_lib("test_server/include/test_server.hrl").


-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1,end_per_suite/1,
	 init_per_testcase/2,end_per_testcase/2]).
-export([marshal_ll/1,marshal_ull/1,
	 marshal_l/1,marshal_ul/1,
	 marshal_s/1,marshal_us/1,
	 marshal_c/1,marshal_wc/1,
	 marshal_str/1,
	 marshal_any_3/1,marshal_any_2/1]).


%% Top of cases

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [marshal_ll, marshal_ull, marshal_l, marshal_ul,
     marshal_s, marshal_us, marshal_c, marshal_wc,
     marshal_str, marshal_any_3, marshal_any_2].

init_per_suite(Config) when is_list(Config) ->
    case case code:priv_dir(jinterface) of
	     {error,bad_name} -> 
		 false;
	     P ->
		 case filelib:wildcard(filename:join(P, "*.jar")) of
		     [_|_] ->
			 true;
		     [] ->
			 false
		 end
	 end
	of
	true -> 
	    case find_executable(["java"]) of
		false -> 
		    {skip,"Found no Java VM"};
		Path -> 
		    [{java,Path}|Config]
	    end;
	false ->
	    {skip,"No jinterface application"}
    end.

    
find_executable([]) ->
    false;
find_executable([E|T]) ->
    case os:find_executable(E) of
	false -> find_executable(T);
	Path -> Path
    end.

end_per_suite(Config) -> Config.



%% Add/remove code path and watchdog before/after each test case.
%%
init_per_testcase(_Case, Config) ->
    DataDir = ?config(data_dir, Config),
    code:add_patha(DataDir),

    %% Since other test suites use the module m_i et,al, we have
    %% to make sure we are using the right modules.
    code:purge(m_i),
    code:purge(m_i_impl),
    code:purge(oe_java_erl_test),
    code:load_file(m_i),
    code:load_file(m_i_impl),
    code:load_file(oe_java_erl_test),

    WatchDog = test_server:timetrap(test_server:seconds(20)),
    [{watchdog, WatchDog}| Config].

end_per_testcase(_Case, Config) ->
    DataDir = ?config(data_dir, Config),
    code:del_path(DataDir),
    WatchDog = ?config(watchdog, Config),
    test_server:timetrap_cancel(WatchDog).



%%--------------------------------------------------------------------
%%
%% Test cases

marshal_ll(doc) ->
    ["Testing marshalling of IDL long long"];
marshal_ll(suite) -> [];
marshal_ll(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_ll}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_ll]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_ull(doc) ->
    ["Testing marshalling of IDL unsigned long long"];
marshal_ull(suite) -> [];
marshal_ull(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_ull}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_ull]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_l(doc) ->
    ["Testing marshalling of IDL long"];
marshal_l(suite) -> [];
marshal_l(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_l}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_l]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_ul(doc) ->
    ["Testing marshalling of IDL unsigned long"];
marshal_ul(suite) -> [];
marshal_ul(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_ul}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_ul]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_s(doc) ->
    ["Testing marshalling of IDL short"];
marshal_s(suite) -> [];
marshal_s(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_s}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_s]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_us(doc) ->
    ["Testing marshalling of IDL unsigned short"];
marshal_us(suite) -> [];
marshal_us(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_us}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_us]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_c(doc) ->
    ["Testing marshalling of IDL char"];
marshal_c(suite) -> [];
marshal_c(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_c}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_c]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_wc(doc) ->
    ["Testing marshalling of IDL char"];
marshal_wc(suite) -> [];
marshal_wc(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_wc}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_wc]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_str(doc) ->
    ["Testing marshalling of IDL string"];
marshal_str(suite) -> [];
marshal_str(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_str}),
    ?line ok = java(?config(java, Config), DataDir, 
%%% 		    "-DOtpConnection.trace=4 "
		    "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_str]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_any_3(doc) ->
    ["Testing marshalling of IDL any"];
marshal_any_3(suite) -> [];
marshal_any_3(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_any_3}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_any_3]),
    ?line ok = m_i:stop(Server),
    ok.

marshal_any_2(doc) ->
    ["Testing marshalling of IDL any"];
marshal_any_2(suite) -> [];
marshal_any_2(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line {ok,Server} = m_i:oe_create_link([], {local,marshal_any_2}),
    ?line ok = java(?config(java, Config), DataDir, "JavaClient",
		    ["JavaClient",node(),erlang:get_cookie(),marshal_any_2]),
    ?line ok = m_i:stop(Server),
    ok.

%%--------------------------------------------------------------------
%%
%% Utilities


java(Java, Dir, ClassAndArgs) ->
    cmd(Java++" -classpath \""++classpath(Dir)++"\" "++ClassAndArgs).

java(Java, Dir, Class, Args) ->
    java(Java, Dir, Class++" "++to_string(Args)).

to_string([H|T]) when is_integer(H) ->
    integer_to_list(H)++" "++to_string(T);
to_string([H|T]) when is_atom(H) ->
    atom_to_list(H)++" "++to_string(T);
to_string([H|T]) when is_list(H) ->
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
	filename:join([code:lib_dir(ic),"priv","ic.jar"])++PS++
	filename:join([code:lib_dir(jinterface),"priv","OtpErlang.jar"])++PS++
	case os:getenv("CLASSPATH") of
	    false -> "";
	    Classpath -> Classpath
	end.


cmd(Cmd) ->
    PortOpts = [{line,80},eof,exit_status,stderr_to_stdout],
    io:format("<cmd> ~s~n", [Cmd]),
    case catch open_port({spawn,Cmd}, PortOpts) of
	Port when is_port(Port) ->
	    Result = cmd_loop(Port, []),
	    io:format("<cmd=~w>~n", [Result]),
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
