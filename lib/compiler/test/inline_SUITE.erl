%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%% Purpose : Tests inlining.

-module(inline_SUITE).

-include_lib("common_test/include/ct.hrl").

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
       itracer,pseudoknot,maps_inline_test,comma_splitter,lists,really_inlined,otp_7223,
       coverage]}].

init_per_suite(Config) ->
    Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
    {ok,Node} = start_node(compiler, Pa),
    [{testing_node,Node}|Config].

end_per_suite(Config) ->
    Node = proplists:get_value(testing_node, Config),
    test_server:stop_node(Node),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


attribute(Config) when is_list(Config) ->
    Name = "attribute",
    Src = filename:join(proplists:get_value(data_dir, Config), Name),
    Out = proplists:get_value(priv_dir,Config),

    {ok,attribute=Mod} = compile:file(Src, [{outdir,Out},report,time]),
    Outfile = filename:join(Out, Name++".beam"),
    {ok,{Mod,[{locals,Locals}]}} = beam_lib:chunks(Outfile, [locals]),
    io:format("locals: ~p\n", [Locals]),

    %% The inliner should have removed all local functions.
    [] = Locals,

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
?comp(maps_inline_test).

try_inline(Mod, Config) ->
    Node = proplists:get_value(testing_node, Config),
    Src = filename:join(proplists:get_value(data_dir, Config),
			atom_to_list(Mod)),
    Out = proplists:get_value(priv_dir,Config),

    %% Normal compilation.
    io:format("Compiling: ~s\n", [Src]),
    {ok,Mod} = compile:file(Src, [{outdir,Out},report,bin_opt_info,clint]),

    ct:timetrap({minutes,10}),
    NormalResult = rpc:call(Node, ?MODULE, load_and_call, [Out,Mod]),

    %% Inlining.
    io:format("Compiling with old inliner: ~s\n", [Src]),
    {ok,Mod} = compile:file(Src, [{outdir,Out},report,bin_opt_info,
					{inline,1000},clint]),

    %% Run inlined code.
    ct:timetrap({minutes,10}),
    OldInlinedResult = rpc:call(Node, ?MODULE, load_and_call, [Out,Mod]),

    %% Compare results.
    compare(NormalResult, OldInlinedResult),
    NormalResult = OldInlinedResult,

    %% Inlining.
    io:format("Compiling with new inliner: ~s\n", [Src]),
    {ok,Mod} = compile:file(Src, [{outdir,Out},report,
					bin_opt_info,inline,clint]),

    %% Run inlined code.
    ct:timetrap({minutes,10}),
    InlinedResult = rpc:call(Node, ?MODULE, load_and_call, [Out,Mod]),

    %% Compare results.
    compare(NormalResult, InlinedResult),
    NormalResult = InlinedResult,

    %% Delete Beam file.
    ok = file:delete(filename:join(Out, atom_to_list(Mod)++code:objfile_extension())),

    ok.

compare(Same, Same) -> ok;
compare([Same|T1], [Same|T2]) ->
    compare(T1, T2);
compare([{X,Y,RGB1}|T1], [{X,Y,RGB2}|T2]) ->
    io:format("X = ~p, Y = ~p, RGB normal = ~p, RGB inlined ~p\n", [X,Y,RGB1,RGB2]),
    compare(T1, T2);
compare([H1|_], [H2|_]) ->
    io:format("Normal = ~p, Inlined = ~p\n", [H1,H2]),
    ct:fail(different);
compare([], []) -> ok.

start_node(Name, Args) ->
    case test_server:start_node(Name, slave, [{args,Args}]) of
	{ok,Node} -> {ok, Node};
	Error  -> ct:fail(Error)
    end.

load_and_call(Out, Module) ->
    io:format("Loading...\n",[]),
    code:purge(Module),
    LoadRc = code:load_abs(filename:join(Out, Module)),
    {module,Module} = LoadRc,

    io:format("Calling...\n",[]),
    {Time,CallResult} = timer:tc(Module, Module, []),
    io:format("Time: ~p\n", [Time]),
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
    List = lists:seq(1, 20),

    %% lists:map/2
    ?TestHighOrder_2(map,
		     (fun(E) ->
			      R = E band 16#ff,
			      put(?MODULE, [E|get(?MODULE)]),
			      R
		      end), List),

    %% lists:flatmap/2
    ?TestHighOrder_2(flatmap,
		     (fun(E) ->
			      R = lists:duplicate(E, E),
			      put(?MODULE, [E|get(?MODULE)]),
			      R
		      end), List),

    %% lists:foreach/2
    ?TestHighOrder_2(foreach,
		     (fun(E) ->
			      put(?MODULE, [E bor 7|get(?MODULE)])
		      end), List),

    %% lists:filter/2
    ?TestHighOrder_2(filter,
		     (fun(E) ->
			      put(?MODULE, [E|get(?MODULE)]),
			      (E bsr 1) band 1 =/= 0
		      end), List),

    %% lists:any/2
    ?TestHighOrder_2(any,
		     (fun(E) ->
			      put(?MODULE, [E|get(?MODULE)]),
			      false	  %Force it to go through all.
		      end), List),

    %% lists:all/2
    ?TestHighOrder_2(all,
		     (fun(E) ->
			      put(?MODULE, [E|get(?MODULE)]),
			      true	  %Force it to go through all.
		      end), List),

    %% lists:foldl/3
    ?TestHighOrder_3(foldl,
		     (fun(E, A) ->
			      put(?MODULE, [E|get(?MODULE)]),
			      A bxor E
		      end), 0, List),

    %% lists:foldr/3
    ?TestHighOrder_3(foldr,
		     (fun(E, A) ->
			      put(?MODULE, [E|get(?MODULE)]),
			      A bxor (bnot E)
		      end), 0, List),

    %% lists:mapfoldl/3
    ?TestHighOrder_3(mapfoldl,
		     (fun(E, A) ->
			      put(?MODULE, [E|get(?MODULE)]),
			      {bnot E,A bxor (bnot E)}
		      end), 0, List),

    %% lists:mapfoldr/3
    ?TestHighOrder_3(mapfoldr,
		     (fun(E, A) ->
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
    {'EXIT', {{case_clause,{1}},_}} = (catch otp_7223_1(1)),
    ok.

-compile({inline,[{otp_7223_1,1}]}).
otp_7223_1(X) ->
    otp_7223_2(X).

-compile({inline,[{otp_7223_2,1}]}).
otp_7223_2({a}) ->
    1.

coverage(Config) when is_list(Config) ->
    Mod = bsdecode,
    Src = filename:join(proplists:get_value(data_dir, Config), Mod),
    {ok,Mod,_} = compile:file(Src, [binary,report,{inline,0},clint]),
    {ok,Mod,_} = compile:file(Src, [binary,report,{inline,20},
				    verbose,clint]),
    ok.
