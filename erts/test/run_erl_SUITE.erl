%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
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

-module(run_erl_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 basic/1,heavy/1,heavier/1,defunct/1]).
-export([ping_me_back/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, heavy, heavier, defunct].

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


basic(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} -> basic_1(Config);
	_ -> {skip,"Not Unix"}
    end.

basic_1(Config) ->
    ?line {Node,Pipe} = do_run_erl(Config, "basic"),

    ?line ToErl = open_port({spawn,"to_erl "++Pipe}, []),
    ?line erlang:port_command(ToErl, "halt().\r\n"),

    receive
	{nodedown,Node} ->
	    ?line io:format("Down: ~p\n", [Node])
    after 10000 ->
	    ?line ?t:fail()
    end,

    ok.

heavy(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} -> heavy_1(Config);
	_ -> {skip,"Not Unix"}
    end.

heavy_1(Config) ->
    ?line {Node,Pipe} = do_run_erl(Config, "heavy"),

    ?line ToErl = open_port({spawn,"to_erl "++Pipe}, []),
    IoFormat = "io:format(\"~s\n\", [lists:duplicate(10000, 10)]).\r\n",
    ?line erlang:port_command(ToErl, IoFormat),
    ?line erlang:port_command(ToErl, IoFormat),
    ?line erlang:port_command(ToErl, IoFormat),
    ?line erlang:port_command(ToErl, "init:stop().\r\n"),
    
    receive
	{nodedown,Node} ->
	    ?line io:format("Down: ~p\n", [Node])
    after 10000 ->
	    ?line ?t:fail()
    end,

    ?line case count_new_lines(ToErl, 0) of
	      Nls when Nls > 30000 ->
		  ok;
	      Nls ->
		  ?line io:format("new_lines: ~p\n", [Nls]),
		  ?line ?t:fail()
	  end.
    

ping_me_back([Node]) when is_atom(Node) ->
    net_adm:ping(Node);
ping_me_back([Node]) ->
    net_adm:ping(list_to_atom(Node)).
    
count_new_lines(P, N) ->
    receive
	{P,{data,S}} ->
	    count_new_lines(P, count_new_lines_1(S, N))
    after 0 ->
	    N
    end.

count_new_lines_1([$\n|T], N) ->
    count_new_lines_1(T, N+1);
count_new_lines_1([_|T], N) ->
    count_new_lines_1(T, N);
count_new_lines_1([], N) -> N.
	
heavier(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} -> heavier_1(Config);
	_ -> {skip,"Not Unix"}
    end.

heavier_1(Config) ->
    ?line {Node,Pipe} = do_run_erl(Config, "heavier"),

    ?line ToErl = open_port({spawn,"to_erl "++Pipe}, []),
    io:format("ToErl = ~p\n", [ToErl]),
    Seed = {1,555,42},
    rand:seed(exsplus, Seed),
    SeedCmd = lists:flatten(io_lib:format("rand:seed(exsplus, ~p). \r\n",
					  [Seed])),
    ?line io:format("~p\n", [SeedCmd]),
    ?line erlang:port_command(ToErl, SeedCmd),

    Iter = 1000,
    MaxLen = 2048,

    Random = "f(F), "++
	"F = fun(F,0) -> ok; "++
	       "(F,N) -> " ++
	           "io:format(\"\\\"~s\\\"~n\","++
	                     "[[35|[rand:uniform(25)+65 || " ++
	                     "_ <- lists:seq(1, "++
	                                "rand:uniform("++
                                             integer_to_list(MaxLen)++
                                        "))]]]), "++
	           "F(F,N-1) "++
             "end,"++ 
        "F(F,"++integer_to_list(Iter)++")."++" \r\n",


    ?line io:format("~p\n", [Random]),
    ?line erlang:port_command(ToErl, Random),

    %% Finish.
    
    ?line erlang:port_command(ToErl, "init:stop().\r\n"),
    ?line receive_all(Iter, ToErl, MaxLen),
    receive
	{nodedown,Node} ->
	    ?line io:format("Down: ~p\n", [Node])
    after 10000 ->
	    ?line c:flush(),
	    ?line ?t:fail()
    end,

    ok.

receive_all(Iter, ToErl, MaxLen) ->
    receive_all_1(Iter, [], ToErl, MaxLen).

receive_all_1(0, _, _, _) -> ok;
receive_all_1(Iter, Line, ToErl, MaxLen) ->
    NumChars = rand:uniform(MaxLen),
    Pattern = [rand:uniform(25)+65 || _ <- lists:seq(1, NumChars)],
    receive_all_2(Iter, {NumChars,Pattern}, Line, ToErl, MaxLen).
    

receive_all_2(Iter, {NumChars,Pattern}, Line0, ToErl, MaxLen) ->
    case receive_match(Line0, {NumChars,Pattern}) of
	{match,Line} ->
	    %%io:format("Match: ~p\n", [Line]),
	    receive_all_1(Iter-1, Line, ToErl, MaxLen);
	{nomatch,Line} ->
	    %%io:format("NoMatch: ~p\n", [Line]),
	    receive
		{ToErl,{data,S}} ->
		    %%io:format("Recv: ~p\n", [S]),
		    receive_all_2(Iter, {NumChars,Pattern}, Line++S, ToErl, MaxLen)		    
	    after 10000 ->
		    io:format("Timeout waiting for\n~p\ngot\n~p\n",
			      [Pattern, Line]),
		    ?line ?t:fail()    
	    end
    end.


receive_match("\"#"++T, {NumChars,Pattern}) when length(T) >= NumChars ->
    Match = lists:sublist(T, NumChars),
    io:format("match candidate: ~p\n", [Match]),
    Match = Pattern,
    {match,lists:nthtail(NumChars, T)};
receive_match("\"#"++T, _) ->
    {nomatch,"\"#"++T};
receive_match("\""=Line, _) ->
    {nomatch,Line};
receive_match([_|T], Tpl) ->
    receive_match(T, Tpl);
receive_match(Line, _) ->
    {nomatch,Line}.


defunct(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} -> defunct_1(Config);
	_ -> {skip,"Not Unix"}
    end.

defunct_1(Config) ->
    case os:find_executable(perl) of
	false ->
	    {skip,"No perl found"};
	Perl ->
	    defunct_2(Config, Perl)
    end.

defunct_2(Config, Perl) ->
    ?line Data = ?config(data_dir, Config),
    ?line RunErlTest = filename:join(Data, "run_erl_test.pl"),
    ?line Defuncter = filename:join(Data, "defuncter.pl"),
    ?line Priv = ?config(priv_dir, Config),
    ?line LogDir = filename:join(Priv, "defunct"),
    ?line ok = file:make_dir(LogDir),
    ?line Pipe = LogDir ++ "/",
    ?line RunErl = os:find_executable(run_erl),
    ?line Cmd = Perl ++ " " ++ RunErlTest ++ " \"" ++ RunErl ++ "\" " ++
	Defuncter ++ " " ++ Pipe ++ " " ++ LogDir,
    ?line io:format("~p", [Cmd]),
    ?line Res = os:cmd(Cmd),
    ?line io:format("~p\n", [Res]),
    "OK"++_ = Res,
    ok.

%%% Utilities.

do_run_erl(Config, Case) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line LogDir = filename:join(Priv, Case),
    ?line ok = file:make_dir(LogDir),
    ?line Pipe = LogDir ++ "/",
    ?line NodeName = "run_erl_node_" ++ Case,
    ?line Cmd = "run_erl "++Pipe++" "++LogDir++" \"erl -sname " ++ NodeName ++
	" -pa " ++ filename:dirname(code:which(?MODULE)) ++
	" -s " ++ ?MODULE_STRING ++ " ping_me_back " ++
	atom_to_list(node()) ++ "\"",
    ?line io:format("~p\n", [Cmd]),
    
    ?line net_kernel:monitor_nodes(true),
    ?line open_port({spawn,Cmd}, []),
    ?line [_,Host] = string:tokens(atom_to_list(node()), "@"),
    ?line Node = list_to_atom(NodeName++"@"++Host),

    receive
	{nodeup,Node} ->
	    ?line io:format("Up: ~p\n", [Node]);
	Other ->
	    ?line io:format("Unexpected: ~p\n", [Other]),
	    ?line ?t:fail()
    after 10000 ->
	    ?line ?t:fail()
    end,

    {Node,Pipe}.
