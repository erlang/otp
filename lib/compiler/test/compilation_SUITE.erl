%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
%%% Purpose : Compiles various modules with tough code

-module(compilation_SUITE).

-include_lib("test_server/include/test_server.hrl").

-compile(export_all).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [self_compile_old_inliner, self_compile, compiler_1,
     compiler_3, compiler_5, beam_compiler_1,
     beam_compiler_2, beam_compiler_3, beam_compiler_4,
     beam_compiler_5, beam_compiler_6, beam_compiler_7,
     beam_compiler_8, beam_compiler_9, beam_compiler_10,
     beam_compiler_11, beam_compiler_12,
     nested_tuples_in_case_expr, otp_2330, guards,
     {group, vsn}, otp_2380, otp_2141, otp_2173, otp_4790,
     const_list_256, bin_syntax_1, bin_syntax_2,
     bin_syntax_3, bin_syntax_4, bin_syntax_5, bin_syntax_6,
     live_var, convopts, bad_functional_value,
     catch_in_catch, redundant_case, long_string, otp_5076,
     complex_guard, otp_5092, otp_5151, otp_5235, otp_5244,
     trycatch_4, opt_crash, otp_5404, otp_5436, otp_5481,
     otp_5553, otp_5632, otp_5714, otp_5872, otp_6121,
     otp_6121a, otp_6121b, otp_7202, otp_7345, on_load,
     string_table,otp_8949_a,otp_8949_a,split_cases].

groups() -> 
    [{vsn, [], [vsn_1, vsn_2, vsn_3]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(comp(N),
	N(Config) when is_list(Config) -> try_it(N, Config)).

-define(comp_fail(N),
	N(Config) when is_list(Config) -> failure(N, Config)).

?comp(compiler_1).
?comp(compiler_3).
?comp(compiler_4).
?comp(compiler_5).

?comp(beam_compiler_1).
?comp(beam_compiler_2).
?comp(beam_compiler_3).
?comp(beam_compiler_4).
?comp(beam_compiler_5).
?comp(beam_compiler_6).
?comp(beam_compiler_8).
?comp(beam_compiler_9).
?comp(beam_compiler_10).
?comp(beam_compiler_11).
?comp(beam_compiler_12).
?comp(beam_compiler_13).

?comp(nested_tuples_in_case_expr).

?comp(otp_2330).
?comp(otp_2380).
?comp(otp_2141).
?comp(otp_2173).
?comp(otp_4790).
?comp(otp_5235).

?comp(otp_5244).

?comp(guards).

?comp(pattern_expr).

?comp(const_list_256).

?comp(bin_syntax_1).
?comp(bin_syntax_2).
?comp(bin_syntax_3).
?comp(bin_syntax_4).

?comp(bin_syntax_6).

?comp(otp_5076).

?comp(complex_guard).

?comp(otp_5092).
?comp(otp_5151).

%%% By Per Gustafsson <pergu@dhcp-12-245.it.uu.se>

bin_syntax_5(Config) when is_list(Config) ->
    {<<45>>,<<>>} = split({int, 1}, <<1:16,45>>).   

split({int, N}, <<N:16,B:N/binary,T/binary>>) ->
    {B,T}.

%% This program works with the old version of the compiler
%% but, the core erlang that it produces have the same variable appearing
%% looks like this:
%%
%% split({int, N}, <<_core1:16, B:N/binary, T/binary>>) when _core1==N
%%
%% with my change it will look like this:
%%
%% split({int, N}, <<_core1:16, B:_core1/binary, T/binary>>) when _core1==N
%%
%% This means that everything worked fine as long as the pattern
%% matching order was left-to-right but on core erlang any order should be possible

?comp(live_var).

?comp(trycatch_4).
?comp(bad_functional_value).

?comp(catch_in_catch).

?comp(opt_crash).

?comp(otp_5404).
?comp(otp_5436).
?comp(otp_5481).
?comp(otp_5553).
?comp(otp_5632).
?comp(otp_5714).
?comp(otp_5872).
?comp(otp_6121).
?comp(otp_6121a).
?comp(otp_6121b).
?comp(convopts).
?comp(otp_7202).
?comp(on_load_inline).

on_load(Config) when is_list(Config) ->
    case test_server:is_native(?MODULE) of
	false ->
	    try_it(on_load, Config);
	true ->
	    {skip,"Native code causes crash"}
    end.

beam_compiler_7(doc) ->
    "Code snippet submitted from Ulf Wiger which fails in R3 Beam.";
beam_compiler_7(suite) -> [];
beam_compiler_7(Config) when is_list(Config) ->
    ?line done = empty(2, false).

empty(N, Toggle) when N > 0 ->
    %% R3 Beam copies the second argument to the first before call.
    empty(N-1, not(Toggle));
empty(_, _) ->
    done.

redundant_case(Config) when is_list(Config) ->
    d = redundant_case_1(1),
    d = redundant_case_1(2),
    d = redundant_case_1(3),
    d = redundant_case_1(4),
    d = redundant_case_1(5),
    d = redundant_case_1({glurf,glarf}),
    ok.

%% This function always returns 'd'. Check that the compiler otptimizes
%% it properly.
redundant_case_1(1) -> d;
redundant_case_1(2) -> d;
redundant_case_1(3) -> d;
redundant_case_1(4) -> d;
redundant_case_1(_) -> d.

failure(Module, Conf) ->
    ?line Src = filename:join(?config(data_dir, Conf), atom_to_list(Module)),
    ?line Out = ?config(priv_dir,Conf),
    ?line io:format("Compiling: ~s\n", [Src]),
    ?line CompRc = compile:file(Src, [{outdir,Out},return,time]),
    ?line io:format("Result: ~p\n",[CompRc]),
    ?line case CompRc of
	      error -> ok;
	      {error,Errors,_} -> check_errors(Errors);
	      _ -> test_server:fail({no_error, CompRc})
	  end,
    ok.

check_errors([{_,Eds}|T]) ->
    check_error(Eds),
    check_errors(T);
check_errors([]) -> ok.

check_error([{_,Mod,Error}|T]) ->
    check_error_1(Mod:format_error(Error)),
    check_error(T);
check_error([{Mod,Error}|T]) ->
    check_error_1(Mod:format_error(Error)),
    check_error(T);
check_error([]) -> ok.

check_error_1(Str0) ->
    Str = lists:flatten(Str0),
    io:format("~s\n", [Str]),
    case Str of
	"internal"++_=Str ->
	    ?t:fail(internal_compiler_error);
	_ ->
	    ok
    end.

-define(TC(Body), tc(fun() -> Body end, ?LINE)).

try_it(Module, Conf) ->
    %% Change 'false' to 'true' to start a new node for every module.
    try_it(false, Module, Conf).
		       
try_it(StartNode, Module, Conf) ->
    ?line OtherOpts = [],			%Can be changed to [time] if needed
    ?line Src = filename:join(?config(data_dir, Conf), atom_to_list(Module)),
    ?line Out = ?config(priv_dir,Conf),
    ?line io:format("Compiling: ~s\n", [Src]),
    ?line CompRc0 = compile:file(Src, [clint,{outdir,Out},report,
				       bin_opt_info|OtherOpts]),
    ?line io:format("Result: ~p\n",[CompRc0]),
    ?line {ok,_Mod} = CompRc0,

    ?line Dog = test_server:timetrap(test_server:minutes(10)),
    Node = case StartNode of
	       false ->
		   node();
	       true ->
		   ?line Pa = "-pa " ++ filename:dirname(code:which(?MODULE)),
		   ?line {ok,Node0} = start_node(compiler, Pa),
		   Node0
	   end,
		   
    ?line ok = rpc:call(Node, ?MODULE, load_and_call, [Out, Module]),
    ?line load_and_call(Out, Module),
    ?line test_server:timetrap_cancel(Dog),

    ?line NewDog = test_server:timetrap(test_server:minutes(10)),
    ?line io:format("Compiling (without optimization): ~s\n", [Src]),
    ?line CompRc1 = compile:file(Src,
				 [no_copt,no_postopt,{outdir,Out},report|OtherOpts]),

    ?line io:format("Result: ~p\n",[CompRc1]),
    ?line {ok,_Mod} = CompRc1,
    ?line ok = rpc:call(Node, ?MODULE, load_and_call, [Out, Module]),
    ?line test_server:timetrap_cancel(NewDog),

    ?line LastDog = test_server:timetrap(test_server:minutes(10)),
    ?line io:format("Compiling (with old inliner): ~s\n", [Src]),
    ?line CompRc2 = compile:file(Src, [{outdir,Out},report,bin_opt_info,
				       {inline,1000}|OtherOpts]),
    ?line io:format("Result: ~p\n",[CompRc2]),
    ?line {ok,_Mod} = CompRc2,
    ?line ok = rpc:call(Node, ?MODULE, load_and_call, [Out, Module]),
    ?line test_server:timetrap_cancel(LastDog),

    case StartNode of
	false -> ok;
	true -> ?line test_server:stop_node(Node)
    end,
    ?line test_server:timetrap_cancel(LastDog),
    ok.

load_and_call(Out, Module) ->
    ?line io:format("Loading...\n",[]),
    ?line {module,Module} = code:load_abs(filename:join(Out, Module)),

    ?line io:format("Calling...\n",[]),
    %% Call M:M, and expect ok back, that's our interface
    ?line CallRc = Module:Module(),
    ?line io:format("Got value: ~p\n",[CallRc]),

    ?line ok = CallRc,

    %% Smoke-test of beam disassembler.
    ?line test_lib:smoke_disasm(Module),

    ?line true = erlang:delete_module(Module),
    ?line true = erlang:purge_module(Module),

    %% Restore state of trap_exit just in case. (Since the compiler
    %% uses a temporary process, we will get {'EXIT',Pid,normal} messages
    %% if trap_exit is true.)

    process_flag(trap_exit, false),
    ok.


tc(F, Line) ->
    {Diff,Value} = timer:tc(erlang, apply, [F,[]]),
    io:format("~p: ~p\n", [Line,Diff]),
    Value.
    
start_node(Name, Args) ->
    case test_server:start_node(Name, slave, [{args, Args}]) of
	{ok, Node} ->
	    {ok, Node};
	Error  ->
	    ?line test_server:fail(Error)
    end.

from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(_, []) -> [].


vsn_1(doc) ->
    "Test generation of 'vsn' attribute";
vsn_1(suite) -> [];
vsn_1(Conf) when is_list(Conf) ->
    ?line M = vsn_1,

    ?line compile_load(M, ?config(data_dir, Conf), Conf),
    ?line Vsn1 = get_vsn(M),
    ?line timer:sleep(1000),

    ?line compile_load(M, ?config(data_dir, Conf), Conf),
    ?line Vsn2 = get_vsn(M),

    ?line compile_load(M, filename:join(?config(data_dir, Conf), "other"),
		       Conf),
    ?line Vsn3 = get_vsn(M),
    ?line if
	      Vsn1 == Vsn2, Vsn2 == Vsn3 ->
		  ok;
	      true ->
		  test_server:fail({vsn, Vsn1, Vsn2, Vsn3})
	  end,
    ok.

vsn_2(doc) ->
    "Test overriding of generation of 'vsn' attribute";
vsn_2(suite) -> [];
vsn_2(Conf) when is_list(Conf) ->
    ?line M = vsn_2,

    ?line compile_load(M, ?config(data_dir, Conf), Conf),
    ?line Vsn = get_vsn(M),
    ?line case Vsn of
	      [34] ->
		  ok;
	      _ ->
		  test_server:fail({vsn, Vsn})
	  end,
    ok.

vsn_3(doc) ->
    "Test that different code yields different generated 'vsn'";
vsn_3(suite) -> [];
vsn_3(Conf) when is_list(Conf) ->
    ?line M = vsn_3,

    ?line compile_load(M, ?config(data_dir, Conf), Conf),
    ?line Vsn1 = get_vsn(M),

    ?line compile_load(M, filename:join(?config(data_dir, Conf), "other"),
		       Conf),
    ?line Vsn2 = get_vsn(M),
    ?line if
	      Vsn1 /= Vsn2 ->
		  ok;
	      true ->
		  test_server:fail({vsn, Vsn1, Vsn2})
	  end,
    ok.

get_vsn(M) ->
    {value, {vsn, V}} = lists:keysearch(vsn, 1, M:module_info(attributes)),
    V.

long_string(Config) when is_list(Config) ->
    %% The test must complete in one minute - it should be plenty of time.
    ?line Dog = test_server:timetrap(test_server:minutes(1)),
    ?line try_it(long_string, Config),
    ?line test_server:timetrap_cancel(Dog),
    ok.

compile_load(Module, Dir, Conf) ->
    ?line Src = filename:join(Dir, atom_to_list(Module)),
    ?line Out = ?config(priv_dir,Conf),
    ?line CompRc = compile:file(Src, [{outdir,Out}]),
    ?line {ok, Module} = CompRc,
    ?line code:purge(Module),
    ?line {module, Module} =
	code:load_abs(filename:join(Out, atom_to_list(Module))),
    ok.

self_compile(Config) when is_list(Config) ->
    self_compile_1(Config, "new", [inline]).

self_compile_old_inliner(Config) when is_list(Config) ->
    %% The old inliner is useful for testing that sys_core_fold does not
    %% introduce name capture problems.
    HowMuch = case test_server:is_native(?MODULE) of
		  true -> 100;
		  false -> 500
	      end,
    self_compile_1(Config, "old", [verbose,{inline,HowMuch}]).

self_compile_1(Config, Prefix, Opts) ->
    ?line Dog = test_server:timetrap(test_server:minutes(40)),

    ?line Priv = ?config(priv_dir,Config),
    ?line Version = compiler_version(),

    %% Compile the compiler. (In this node to get better coverage.)
    ?line CompA = make_compiler_dir(Priv, Prefix++"compiler_a"),
    ?line VsnA = Version ++ ".0",
    ?line compile_compiler(compiler_src(), CompA, VsnA, [clint|Opts]),

    %% Compile the compiler again using the newly compiled compiler.
    %% (In another node because reloading the compiler would disturb cover.)
    CompilerB = Prefix++"compiler_b",
    CompB = make_compiler_dir(Priv, CompilerB),
    ?line VsnB = VsnA ++ ".0",
    self_compile_node(CompA, CompB, VsnB, Opts),

    %% Compare compiler directories.
    ?line compare_compilers(CompA, CompB),

    %% Compile and compare compiler C.
    ?line CompilerC = Prefix++"compiler_c",
    ?line CompC = make_compiler_dir(Priv, CompilerC),
    ?line VsnC = VsnB ++ ".0",
    self_compile_node(CompB, CompC, VsnC, Opts),
    ?line compare_compilers(CompB, CompC),

    ?line test_server:timetrap_cancel(Dog),
    ok.

self_compile_node(CompilerDir, OutDir, Version, Opts) ->
    ?line Dog = test_server:timetrap(test_server:minutes(15)),
    ?line Pa = "-pa " ++ filename:dirname(code:which(?MODULE)) ++
	" -pa " ++ CompilerDir,
    ?line Files = compiler_src(),

    %% We don't want the cover server started on the other node,
    %% because it will load the same cover-compiled code as on this
    %% node. Use a shielded node to prevent the cover server from
    %% being started.
    ?t:run_on_shielded_node(
       fun() ->
	       compile_compiler(Files, OutDir, Version, Opts)
       end, Pa),
    ?line test_server:timetrap_cancel(Dog),
    ok.

compile_compiler(Files, OutDir, Version, InlineOpts) ->
    io:format("~s", [code:which(compile)]),
    io:format("Compiling ~s into ~s", [Version,OutDir]),
    Opts = [report,
	    bin_opt_info,
	    {outdir,OutDir},
	    {d,'COMPILER_VSN',"\""++Version++"\""},
	    nowarn_shadow_vars,
	    {i,filename:join(code:lib_dir(stdlib), "include")}|InlineOpts],
    test_lib:p_run(fun(File) ->
			   case compile:file(File, Opts) of
			       {ok,_} -> ok;
			       _ -> error
			   end
		   end, Files).

compiler_src() ->
    filelib:wildcard(filename:join([code:lib_dir(compiler), "src", "*.erl"])).

compiler_modules(Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "*.beam")),
    [list_to_atom(filename:rootname(filename:basename(F))) || F <- Files].

make_compiler_dir(Priv, Dir0) ->
    ?line Dir = filename:join(Priv, Dir0),
    ?line ok = file:make_dir(Dir),
    Dir.

make_current(Dir) ->    
    true = code:add_patha(Dir),
    lists:foreach(fun(File) ->
			  c:l(File)
		  end, compiler_modules(Dir)),
    io:format("~p\n", [code:which(compile)]).

compiler_version() ->
    {value,{version,Version}} = lists:keysearch(version, 1,
						compile:module_info(compile)),
    Version.

compare_compilers(ADir, BDir) ->
    {[],[],D} = beam_lib:cmp_dirs(ADir, BDir),
    [] = [T || {A,_}=T <- D,
	       filename:basename(A) =/= "beam_asm.beam"]. %Contains compiler version.


%%%
%%% The only test of the following code is that it compiles.
%%%

%% Slightly simplifed from megaco_binary_term_id_gen.
%%  beam_block failed to note that the {gc_bif,'-'...} instruction could
%%  fail, and that therefore {y,0} need to be initialized.
%%    {allocate,8,6}.
%%                     %% {init,{y,0}} needed here.       
%%    {get_list,{x,1},{x,6},{x,7}}.
%%    {'catch',{y,7},{f,3}}.
%%    {move,{x,4},{y,1}}.
%%    {move,{x,3},{y,2}}.
%%    {move,{x,2},{y,3}}.
%%    {move,{x,5},{y,4}}.
%%    {move,{x,7},{y,5}}.
%%    {move,{x,6},{y,6}}.
%%    {gc_bif,'-',{f,0},8,[{x,3},{x,6}],{x,0}}.
%%    {move,{x,0},{y,0}}.

encode_wildcards3([],[],_,_) -> [];
encode_wildcards3([Level|Levels],[BitsInLevel|BitsRest],LevelNo,TotSize) ->
    case (catch ?MODULE:encode_wildcard(Level,BitsInLevel,TotSize-BitsInLevel,
					length(Levels))) of
	{'EXIT',{Reason,Info}} ->
	    exit({Reason,{LevelNo,Info}});

	no_wildcard ->
	    encode_wildcards3(Levels,BitsRest,LevelNo+1,TotSize-BitsInLevel);
	    
	{level,Wl} ->  
	    [Wl|
	     encode_wildcards3(Levels,BitsRest,LevelNo+1,TotSize-BitsInLevel)];

	{recursive,Wr} ->  
	    [Wr]
    end.

%% Slightly simplified code from hipe_rtl_ssapre.
%%  beam_block used to do the following incorrect optimization:
%%
%%    {gc_bif,length,{f,0},1,[{x,0}],{x,3}}.
%%                                   ^^^^^ Was {x,0} - changing to {x,3} is not safe.
%%    {gc_bif,'+',{f,0},0,[{y,2},{integer,1}],{x,0}}.
%%                     ^^^ Only one register live
%%     . . .
%%    {call_last,4,{f,2},4}.   %% beam_validator noted that {x,3} wasn't live.

find_operands(Cfg,XsiGraph,[],_Count) ->
    {Cfg,XsiGraph};
find_operands(Cfg,XsiGraph,ActiveList,Count) ->
    {NewCfg,TempActiveList}=?MODULE:find_operands_for_active_list(Cfg,XsiGraph,
								  ActiveList,[]),
    NewActiveList=lists:reverse(TempActiveList),
    [Count+1, length(NewActiveList), length(digraph:vertices(XsiGraph))],
    find_operands(NewCfg,XsiGraph,NewActiveList,Count+1).


%% The following code
%%
%%    {get_list,{x,2},{x,0},{x,1}}.
%%    {gc_bif,length,{f,0},1,[{x,0}],{x,0}}.
%%    {move,{x,0},{x,1}}.
%%
%% was incorrectly optimized to
%%
%%    {get_list,{x,2},{x,0},{y,0}}.
%%    {gc_bif,length,{f,0},3,[{x,0}],{x,1}}.
%%
%% because beam_block:is_transparent({x,1},
%%                                  {gc_bif,length,{f,0},3,[{x,0}],{x,1}}
%% incorrectly returned true.

-record(contextId,{cid,device_type,contextRef}).
-record(dpRef,{cid,tlli,ms_device_context_id}).
-record(qosProfileBssgp,{peak_bit_rate_msb,
                              peak_bit_rate_lsb,
                              t_a_precedence}).
-record(llUnitdataReq,{sapi,
                            l3_pdu_length,
                            pdu_life}).
-record(ptmsi,{value}).

otp_7345(Config) when is_list(Config) ->
    #llUnitdataReq{l3_pdu_length=3,pdu_life=4} =
	otp_7345(#contextId{}, 0, [[1,2,3],4,5]).


otp_7345(ObjRef, _RdEnv, Args) ->
    Cid = ObjRef#contextId.cid,
    _DpRef =
	#dpRef{cid = Cid,
		     ms_device_context_id = cid_id,
		     tlli = #ptmsi{value = 0}},
    _QosProfile =
	#qosProfileBssgp{peak_bit_rate_msb = 0,
			 peak_bit_rate_lsb = 80,
			 t_a_precedence = 49},
    [Cpdu|_] = Args,
    LlUnitdataReq =
	#llUnitdataReq{sapi = 7,
		       l3_pdu_length = length(Cpdu),
		       pdu_life =
		       id(42)
		       div
		       10},
    id(LlUnitdataReq).

%% Check the generation of the string table.

string_table(Config) when is_list(Config) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line File = filename:join(DataDir, "string_table.erl"),
    ?line {ok,string_table,Beam,[]} = compile:file(File, [return, binary]),
    ?line {ok,{string_table,[StringTableChunk]}} = beam_lib:chunks(Beam, ["StrT"]),
    ?line {"StrT", <<"stringabletringtable">>} = StringTableChunk,
    ok.

otp_8949_a(Config) when is_list(Config) ->
    value = otp_8949_a(),
    ok.

-record(cs, {exs,keys = [],flags = 1}).
-record(exs, {children = []}).

otp_8949_a() ->
    case id([#cs{}]) of
        [#cs{}=Cs] ->
            SomeVar = id(value),
	    if
		Cs#cs.flags band 1 =/= 0 ->
		    id(SomeVar);
		(((Cs#cs.exs)#exs.children /= [])
                 and
		   (Cs#cs.flags band (1 bsl 0 bor (1 bsl 22)) == 0));
		Cs#cs.flags band (1 bsl 22) =/= 0 ->
		    ok
	    end
    end.
    
otp_8949_b(Config) when is_list(Config) ->
    self() ! something,
    ?line value = otp_8949_b([], false),
    ?line {'EXIT',_} = (catch otp_8949_b([], true)),
    ok.

%% Would cause an endless loop in beam_utils.
otp_8949_b(A, B) ->
    Var = id(value),
    if
	A == [], B == false ->
	    ok
    end,
    receive
        something ->
	    id(Var)
    end.
    
split_cases(_) ->
    dummy1 = do_split_cases(x),
    {'EXIT',{{badmatch,b},_}} = (catch do_split_cases(y)),
    ok.

do_split_cases(A) ->
    case A of
        x ->
	    Z = dummy1;
        _ ->
	    Z = dummy2,
	    a=b
    end,
    Z.

id(I) -> I.
