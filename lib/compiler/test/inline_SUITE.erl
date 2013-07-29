%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2013. All Rights Reserved.
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
%%% Purpose : Tests inlining.

-module(inline_SUITE).

-include_lib("test_server/include/test_server.hrl").

-compile(export_all).
-compile({inline,[badarg/2]}).

%% Needed by test case `lists'.
-compile(inline_list_funcs).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [attribute,bsdecode,bsdes,barnes2,decode1,smith,fname,
       itracer,pseudoknot,comma_splitter,lists,really_inlined,otp_7223,
       coverage]}].

init_per_suite(Config) ->
    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok,Node} = start_node(compiler, Pa),
    [{testing_node,Node}|Config].

end_per_suite(Config) ->
    Node = ?config(testing_node, Config),
    ?t:stop_node(Node),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


attribute(Config) when is_list(Config) ->
    Name = "attribute",
    ?line Src = filename:join(?config(data_dir, Config), Name),
    ?line Out = ?config(priv_dir,Config),

    ?line {ok,attribute=Mod} = compile:file(Src, [{outdir,Out},report,time]),
    ?line Outfile = filename:join(Out, Name++".beam"),
    ?line {ok,{Mod,[{locals,Locals}]}} = beam_lib:chunks(Outfile, [locals]),
    ?line io:format("locals: ~p\n", [Locals]),

    %% The inliner should have removed all local functions.
    ?line [] = Locals,

    ok.

-define(comp(Name),
	Name(Config) when is_list(Config) ->
	       try_inline(Name, Config)).

?comp(bsdecode).
?comp(bsdes).
?comp(barnes2).
?comp(decode1).
?comp(smith).
?comp(itracer).
?comp(pseudoknot).
?comp(comma_splitter).
?comp(fname).

try_inline(Mod, Config) ->
    Node = ?config(testing_node, Config),
    ?line Src = filename:join(?config(data_dir, Config), atom_to_list(Mod)),
    ?line Out = ?config(priv_dir,Config),

    %% Normal compilation.
    ?line io:format("Compiling: ~s\n", [Src]),
    ?line {ok,Mod} = compile:file(Src, [{outdir,Out},report,bin_opt_info,clint]),

    ?line Dog = test_server:timetrap(test_server:minutes(10)),
    ?line NormalResult = rpc:call(Node, ?MODULE, load_and_call, [Out,Mod]),
    ?line test_server:timetrap_cancel(Dog),

    %% Inlining.
    ?line io:format("Compiling with old inliner: ~s\n", [Src]),
    ?line {ok,Mod} = compile:file(Src, [{outdir,Out},report,bin_opt_info,
					{inline,1000},clint]),

    %% Run inlined code.
    ?line Dog3 = test_server:timetrap(test_server:minutes(10)),
    ?line OldInlinedResult = rpc:call(Node, ?MODULE, load_and_call, [Out,Mod]),
    ?line test_server:timetrap_cancel(Dog3),

    %% Compare results.
    ?line compare(NormalResult, OldInlinedResult),
    ?line NormalResult = OldInlinedResult,

    %% Inlining.
    ?line io:format("Compiling with new inliner: ~s\n", [Src]),
    ?line {ok,Mod} = compile:file(Src, [{outdir,Out},report,
					bin_opt_info,inline,clint]),

    %% Run inlined code.
    ?line Dog4 = test_server:timetrap(test_server:minutes(10)),
    ?line InlinedResult = rpc:call(Node, ?MODULE, load_and_call, [Out,Mod]),
    ?line test_server:timetrap_cancel(Dog4),

    %% Compare results.
    ?line compare(NormalResult, InlinedResult),
    ?line NormalResult = InlinedResult,

    %% Delete Beam file.
    ?line ok = file:delete(filename:join(Out, atom_to_list(Mod)++code:objfile_extension())),

    ok.

compare(Same, Same) -> ok;
compare([Same|T1], [Same|T2]) ->
    compare(T1, T2);
compare([{X,Y,RGB1}|T1], [{X,Y,RGB2}|T2]) ->
    io:format("X = ~p, Y = ~p, RGB normal = ~p, RGB inlined ~p\n", [X,Y,RGB1,RGB2]),
    compare(T1, T2);
compare([H1|_], [H2|_]) ->
    io:format("Normal = ~p, Inlined = ~p\n", [H1,H2]),
    ?t:fail();
compare([], []) -> ok.

start_node(Name, Args) ->
    case test_server:start_node(Name, slave, [{args,Args}]) of
	{ok,Node} -> {ok, Node};
	Error  -> ?line test_server:fail(Error)
    end.

load_and_call(Out, Module) ->
    ?line io:format("Loading...\n",[]),
    ?line code:purge(Module),
    ?line LoadRc = code:load_abs(filename:join(Out, Module)),
    ?line {module,Module} = LoadRc,

    ?line io:format("Calling...\n",[]),
    ?line {Time,CallResult} = timer:tc(Module, Module, []),
    ?line io:format("Time: ~p\n", [Time]),
    CallResult.

%% Macros used by lists/1 below.

-define(TestHighOrder_2(Name, Body, List),
	begin
	    put(?MODULE, []),
	    (fun({Res,Res2}) ->
		     {Res,Res2} = my_apply(lists, Name, [Body,List], [])
	     end)(begin
		      (fun(R) ->
			       {R,get(?MODULE)}
		       end)(lists:Name(Body, List))
		  end)
	 end).

-define(TestHighOrder_3(Name, Body, Init, List),
	begin
	    put(?MODULE, []),
	    (fun({Res,Res2}) ->
		     {Res,Res2} = my_apply(lists, Name, [Body,Init,List], [])
	     end)(begin
		      (fun(R) ->
			       {R,get(?MODULE)}
		       end)(lists:Name(Body, Init, List))
		  end)
	 end).

%% For each high order function in the lists module, verify
%% that the inlined version produces the same result and is evaluated
%% in the same order as the function in the lists module.
%%
%% Note: This module must be compiled with the inline_lists_funcs option.

lists(Config) when is_list(Config) ->
    ?line List = lists:seq(1, 20),

    %% lists:map/2
    ?line ?TestHighOrder_2(map, (fun(E) ->
        R = E band 16#ff,
	put(?MODULE, [E|get(?MODULE)]),
        R
	end), List),

    %% lists:flatmap/2
    ?line ?TestHighOrder_2(flatmap, (fun(E) ->
        R = lists:duplicate(E, E),
	put(?MODULE, [E|get(?MODULE)]),
        R
     end), List),

    %% lists:foreach/2
    ?line ?TestHighOrder_2(foreach,
			   (fun(E) ->
				    put(?MODULE, [E bor 7|get(?MODULE)])
			    end), List),

    %% lists:filter/2
    ?line ?TestHighOrder_2(filter, (fun(E) ->
	put(?MODULE, [E|get(?MODULE)]),
        (E bsr 1) band 1 =/= 0
	end), List),

    %% lists:any/2
    ?line ?TestHighOrder_2(any, (fun(E) ->
	put(?MODULE, [E|get(?MODULE)]),
        false					%Force it to go through all.
	end), List),

    %% lists:all/2
    ?line ?TestHighOrder_2(all, (fun(E) ->
	put(?MODULE, [E|get(?MODULE)]),
        true					%Force it to go through all.
	end), List),

    %% lists:foldl/3
    ?line ?TestHighOrder_3(foldl, (fun(E, A) ->
	put(?MODULE, [E|get(?MODULE)]),
        A bxor E
	end), 0, List),

    %% lists:foldr/3
    ?line ?TestHighOrder_3(foldr, (fun(E, A) ->
	put(?MODULE, [E|get(?MODULE)]),
        A bxor (bnot E)
	end), 0, List),

    %% lists:mapfoldl/3
    ?line ?TestHighOrder_3(mapfoldl, (fun(E, A) ->
	put(?MODULE, [E|get(?MODULE)]),
        {bnot E,A bxor (bnot E)}
	end), 0, List),

    %% lists:mapfoldr/3
    ?line ?TestHighOrder_3(mapfoldr, (fun(E, A) ->
	put(?MODULE, [E|get(?MODULE)]),
        {bnot E,A bxor (bnot E)}
	end), 0, List),

    %% Cleanup.
    erase(?MODULE),

    {'EXIT',{function_clause,[{?MODULE,_,[_,not_a_list],_}|_]}} =
        (catch lists:map(fun (X) -> X end, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,not_a_list],_}|_]}} =
        (catch lists:flatmap(fun (X) -> X end, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,not_a_list],_}|_]}} =
        (catch lists:foreach(fun (X) -> X end, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,not_a_list],_}|_]}} =
        (catch lists:filter(fun (_) -> true end, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,not_a_list],_}|_]}} =
        (catch lists:any(fun (_) -> false end, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,not_a_list],_}|_]}} =
        (catch lists:all(fun (_) -> true end, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,acc,not_a_list],_}|_]}} =
        (catch lists:foldl(fun (X, Acc) -> {X,Acc} end, acc, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,acc,not_a_list],_}|_]}} =
        (catch lists:foldr(fun (X, Acc) -> {X,Acc} end, acc, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,acc,not_a_list],_}|_]}} =
        (catch lists:mapfoldl(fun (X, Acc) -> {X,Acc} end, acc, not_a_list)),
    {'EXIT',{function_clause,[{?MODULE,_,[_,acc,not_a_list],_}|_]}} =
        (catch lists:mapfoldr(fun (X, Acc) -> {X,Acc} end, acc, not_a_list)),

    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,[]],_}|_]}} =
        (catch lists:map(not_a_function, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,[]],_}|_]}} =
        (catch lists:flatmap(not_a_function, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,[]],_}|_]}} =
        (catch lists:foreach(not_a_function, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,[]],_}|_]}} =
        (catch lists:filter(not_a_function, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,[]],_}|_]}} =
        (catch lists:any(not_a_function, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,[]],_}|_]}} =
        (catch lists:all(not_a_function, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,acc,[]],_}|_]}} =
        (catch lists:foldl(not_a_function, acc, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,acc,[]],_}|_]}} =
        (catch lists:foldr(not_a_function, acc, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,acc,[]],_}|_]}} =
        (catch lists:mapfoldl(not_a_function, acc, [])),
    {'EXIT',{function_clause,[{?MODULE,_,[not_a_function,acc,[]],_}|_]}} =
        (catch lists:mapfoldr(not_a_function, acc, [])),

    ok.
		       
my_apply(M, F, A, Init) ->
    put(?MODULE, Init),
    Res = apply(M, F, A),
    {Res,get(?MODULE)}.

really_inlined(Config) when is_list(Config) ->
    %% Make sure that badarg/2 really gets inlined.
    {'EXIT',{badarg,[{?MODULE,fail_me_now,[],_}|_]}} =
	(catch fail_me_now()),
    ok.

fail_me_now() ->
    badarg(foo(bar), []).

foo(_X) ->
    badarg.

%% Inlined.
badarg(badarg, A) ->
    erlang:error(badarg, A);
badarg(Reply, _A) ->
    Reply.

otp_7223(Config) when is_list(Config) ->
    ?line {'EXIT', {{case_clause,{1}},_}} = (catch otp_7223_1(1)),
    ok.

-compile({inline,[{otp_7223_1,1}]}).
otp_7223_1(X) ->
    otp_7223_2(X).

-compile({inline,[{otp_7223_2,1}]}).
otp_7223_2({a}) ->
    1.

coverage(Config) when is_list(Config) ->
    Mod = bsdecode,
    Src = filename:join(?config(data_dir, Config), Mod),
    {ok,Mod,_} = compile:file(Src, [binary,report,{inline,0},clint]),
    {ok,Mod,_} = compile:file(Src, [binary,report,{inline,20},
				    verbose,clint]),
    ok.
