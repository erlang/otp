%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-export([all/0, suite/0]).
-export([basic/1,heavy/1,heavier/1,defunct/1]).
-export([ping_me_back/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [basic, heavy, heavier, defunct].


basic(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} -> basic_1(Config);
	_ -> {skip,"Not Unix"}
    end.

basic_1(Config) ->
    {Node,Pipe} = do_run_erl(Config, "basic"),

    ToErl = open_port({spawn,"to_erl "++Pipe}, []),
    erlang:port_command(ToErl, "halt().\r\n"),

    receive
	{nodedown,Node} ->
	    io:format("Down: ~p\n", [Node])
    after 10000 ->
	    ct:fail(timeout)
    end,

    ok.

heavy(Config) when is_list(Config) ->
    case os:type() of
	{unix,_} -> heavy_1(Config);
	_ -> {skip,"Not Unix"}
    end.

heavy_1(Config) ->
    {Node,Pipe} = do_run_erl(Config, "heavy"),

    ToErl = open_port({spawn,"to_erl "++Pipe}, []),
    IoFormat = "io:format(\"~s\n\", [lists:duplicate(10000, 10)]).\r\n",
    erlang:port_command(ToErl, IoFormat),
    erlang:port_command(ToErl, IoFormat),
    erlang:port_command(ToErl, IoFormat),
    erlang:port_command(ToErl, "init:stop().\r\n"),
    
    receive
	{nodedown,Node} ->
	    io:format("Down: ~p\n", [Node])
    after 10000 ->
	    ct:fail(timeout)
    end,

    case count_new_lines(ToErl, 0) of
        Nls when Nls > 30000 ->
            ok;
        Nls ->
            ct:fail("new_lines: ~p\n", [Nls])
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
    {Node,Pipe} = do_run_erl(Config, "heavier"),

    ToErl = open_port({spawn,"to_erl "++Pipe}, []),
    io:format("ToErl = ~p\n", [ToErl]),
    Seed = {1,555,42},
    rand:seed(exsplus, Seed),
    SeedCmd = lists:flatten(io_lib:format("rand:seed(exsplus, ~p). \r\n",
					  [Seed])),
    io:format("~p\n", [SeedCmd]),
    erlang:port_command(ToErl, SeedCmd),

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


    io:format("~p\n", [Random]),
    erlang:port_command(ToErl, Random),

    %% Finish.
    
    erlang:port_command(ToErl, "init:stop().\r\n"),
    receive_all(Iter, ToErl, MaxLen),
    receive
	{nodedown,Node} ->
	    io:format("Down: ~p\n", [Node])
    after 10000 ->
	    c:flush(),
	    ct:fail(timeout)
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
		    ct:fail("Timeout waiting for\n~p\ngot\n~p\n", [Pattern, Line])
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
    Data = proplists:get_value(data_dir, Config),
    RunErlTest = filename:join(Data, "run_erl_test.pl"),
    Defuncter = filename:join(Data, "defuncter.pl"),
    Priv = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(Priv, "defunct"),
    ok = file:make_dir(LogDir),
    Pipe = LogDir ++ "/",
    RunErl = os:find_executable(run_erl),
    Cmd = Perl ++ " " ++ RunErlTest ++ " \"" ++ RunErl ++ "\" " ++
	Defuncter ++ " " ++ Pipe ++ " " ++ LogDir,
    io:format("~p", [Cmd]),
    Res = os:cmd(Cmd),
    io:format("~p\n", [Res]),
    "OK"++_ = Res,
    ok.

%%% Utilities.

do_run_erl(Config, Case) ->
    Priv = proplists:get_value(priv_dir, Config),
    LogDir = filename:join(Priv, Case),
    ok = file:make_dir(LogDir),
    Pipe = LogDir ++ "/",
    NodeName = "run_erl_node_" ++ Case,
    Cmd = "run_erl "++Pipe++" "++LogDir++" \"erl -sname " ++ NodeName ++
	" -pa " ++ filename:dirname(code:which(?MODULE)) ++
	" -s " ++ ?MODULE_STRING ++ " ping_me_back " ++
	atom_to_list(node()) ++ "\"",
    io:format("~p\n", [Cmd]),
    
    net_kernel:monitor_nodes(true),
    open_port({spawn,Cmd}, []),
    [_,Host] = string:tokens(atom_to_list(node()), "@"),
    Node = list_to_atom(NodeName++"@"++Host),

    receive
	{nodeup,Node} ->
	    io:format("Up: ~p\n", [Node]);
	Other ->
	    ct:fail("Unexpected: ~p\n", [Other])
    after 10000 ->
	    ct:fail(timeout)
    end,
    {Node,Pipe}.
