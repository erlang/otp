%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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

-module(ct_property_test).

%%% API
%% Main functions
-export([init_per_suite/1,
         init_tool/1,
         quickcheck/2]).

%% Result presentation
-export([present_result/4, present_result/5,
         title/2, title/3,
         sequential_parallel/1,
         cmnd_names/1,
         num_calls/1,
         print_frequency_ranges/0,
         print_frequency/0
        ]).

%%% Mandatory include
-include_lib("common_test/include/ct.hrl").

%%%================================================================
%%%
%%% API
%%% 

%%%----------------------------------------------------------------
%%%
%%% Search for a property tester in the lib path, and if found, compile
%%% the property tests
%%%
init_per_suite(Config) ->
    case init_tool(Config) of
        {skip, _}=Skip ->
            Skip;
        Config1 ->
            Path = property_tests_path("property_test", Config1),
            case compile_tests(Path, Config1) of
                error ->
                    {fail, "Property test compilation failed in "++Path};
                {skip,Reason} ->
                    {skip,Reason};
                up_to_date ->
                    add_code_pathz(Path),
                    [{property_dir, Path} | Config1]
            end
    end.

init_tool(Config) ->
    ToolsToCheck = proplists:get_value(prop_tools, Config, [eqc,proper,triq]),
    case which_module_exists(ToolsToCheck) of
	{ok,ToolModule} ->
            case code:where_is_file(lists:concat([ToolModule,".beam"])) of
                non_existing ->
                    ct:log("Found ~p, but ~tp~n is not found",
                           [ToolModule, lists:concat([ToolModule,".beam"])]),
                    {skip, "Strange Property testing tool installation"};
                ToolPath ->
                    ct:pal("Found property tester ~p~n"
                           "at ~tp",
                           [ToolModule, ToolPath]),
                    [{property_test_tool, ToolModule} | Config]
            end;
        not_found ->
            ct:pal("No property tester found",[]),
            {skip, "No property testing tool found"}
    end.
	
%%%----------------------------------------------------------------
%%%
%%% Call the found property tester (if any)
%%%
quickcheck(Property, Config) ->
    Tool = proplists:get_value(property_test_tool,Config),
    F = function_name(quickcheck, Tool),
    mk_ct_return( Tool:F(Property), Tool ).


%%%----------------------------------------------------------------
%%%
%%% Present a nice table of the statem result
%%%
present_result(Module, Cmds, Triple, Config) ->
    present_result(Module, Cmds, Triple, Config, []).

present_result(Module, Cmds, {H,Sf,Result}, Config, Options0) ->
    DefSpec = 
        if
            is_tuple(Cmds) ->
                [{"Distribution sequential/parallel", fun sequential_parallel/1}];
            is_list(Cmds) ->
                []
        end
        ++ [{"Function calls", fun cmnd_names/1},
            {"Length of command sequences", fun print_frequency_ranges/0, fun num_calls/1}
           ],
    Options = add_default_options(Options0,
                                  [{print_fun, fun ct:log/2},
                                   {spec, DefSpec}
                                  ]),
    do_present_result(Module, Cmds, H, Sf, Result, Config, Options).


title(Str, Fun) ->
    title(Str, Fun, fun io:format/2).

title(Str, Fun, PrintFun) ->
    fun(L) -> PrintFun("~n~s~n~n~s~n", [Str,Fun(L)]) end.

print_frequency() ->
    fun(L) ->
            [io_lib:format("~5.1f% ~p~n",[Pcnt,V])
             || {V,_Num,Pcnt} <-
                    with_percentage(get_frequencies_no_range(L), length(L))
            ]
    end.

print_frequency_ranges() ->
    print_frequency_ranges([{ngroups,10}]).

print_frequency_ranges(Options0) ->
    fun([]) ->
            io_lib:format('Empty list!~n',[]);
       (L ) ->
            try
                Options = set_default_print_freq_range_opts(Options0, L),
                do_print_frequency_ranges(L, Options)
            catch
                C:E:S ->
                    ct:pal("~p:~p ~p:~p~n~p~n~p",[?MODULE,?LINE,C,E,S,L])
            end
    end.

%%%================================================================
%%%
%%% Local functions
%%% 

%%% Make return values back to the calling Common Test suite
mk_ct_return(true, _Tool) ->
    true;
mk_ct_return(Other, Tool) ->
    try lists:last(hd(Tool:counterexample()))
    of
	{set,{var,_},{call,M,F,Args}} ->
	    {fail, io_lib:format("~p:~tp/~p returned bad result",[M,F,length(Args)])}
    catch
	_:_ ->
	    {fail, Other}
    end.

%%% Check if a property testing tool is found
which_module_exists([Module|Modules]) ->
    case module_exists(Module) of
	true -> {ok,Module};
	false -> which_module_exists(Modules)
    end;
which_module_exists(_) ->
    not_found.

module_exists(Module) ->
    is_list(catch Module:module_info()).

%%% The path to the property tests
property_tests_path(Dir, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join(lists:droplast(filename:split(DataDir))++[Dir]).

%%% Extend the code path with Dir if it not already present
add_code_pathz(Dir) ->
    case lists:member(Dir, code:get_path()) of
	true ->  ok;
	false ->
	    true = code:add_pathz(Dir),
	    ok
    end.

compile_tests(Path, Config) ->
    ToolModule = proplists:get_value(property_test_tool, Config),
    MacroDefs = macro_def(ToolModule),
    {ok,Cwd} = file:get_cwd(),
    case file:set_cwd(Path) of
        ok ->
            case file:list_dir(".") of
                {ok,[]} ->
                    ct:pal("No files found in ~tp", [Path]),
                    ok = file:set_cwd(Cwd),
                    {skip, "No files found"};
                {ok,FileNames} ->
                    BeamFiles = [F || F<-FileNames,
                                      filename:extension(F) == ".beam"],
                    ErlFiles = [F || F<-FileNames,
                                      filename:extension(F) == ".erl"],
                    _ = [file:delete(F) || F<-BeamFiles],
                    ct:pal("Compiling in ~tp~n"
                           "  Deleted:   ~p~n"
                           "  ErlFiles:  ~tp~n"
                           "  MacroDefs: ~p",
                           [Path,BeamFiles,ErlFiles,MacroDefs]),
                    Result = make:all([load|MacroDefs]),
                    ok = file:set_cwd(Cwd),
                    Result
            end;

        {error,Error} ->
            ct:pal("file:set_cwd(~tp) returned ~p.~nCwd = ~tp", [Path, {error,Error}, Cwd]),
            error
    end.
    

macro_def(eqc) -> [{d, 'EQC'}];
macro_def(proper) -> [{d, 'PROPER'}];
macro_def(triq) -> [{d, 'TRIQ'}].
    
function_name(quickcheck, triq) -> check;
function_name(F, _) -> F.
    

%%%================================================================
%%%================================================================
%%%================================================================
%%%
%%% Result presentation part
%%% 
do_present_result(_Module, Cmds, _H, _Sf, ok, Config, Options) ->
    [PrintFun, Spec] = [proplists:get_value(K,Options) || K <- [print_fun,spec]],
    Tool = proplists:get_value(property_test_tool,Config),
    AGGREGATE = function_name(aggregate, Tool),
    lists:foldr(fun({Title, FreqFun, CollecFun}, Result) ->
                        Tool:AGGREGATE(title(Title, FreqFun(), PrintFun), 
                                       CollecFun(Cmds),
                                       Result);
                   ({Title, CollecFun}, Result) ->
                        Tool:AGGREGATE(title(Title, print_frequency(), PrintFun),
                                       CollecFun(Cmds),
                                       Result)
                end, true, Spec);

do_present_result(Module, Cmds, H, Sf, Result, _Config, Options) -> 
    [PrintFun] = [proplists:get_value(K,Options) || K <- [print_fun]],
    PrintFun("Module = ~p,~n"
             "Commands = ~p,~n"
             "History = ~p,~n"
             "FinalDynState = ~p,~n"
             "Result = ~p",
             [Module, Cmds, H, Sf, Result]),
    Result == ok. % Proper dislikes non-boolean results while eqc treats non-true as false.

%%%================================================================
cmnd_names(Cs) -> traverse_commands(fun cmnd_name/1, Cs).
cmnd_name(L) ->  [F || {set,_Var,{call,_Mod,F,_As}} <- L].
    
num_calls(Cs) -> traverse_commands(fun num_call/1, Cs).
num_call(L) -> [length(L)].
    
sequential_parallel(Cs) ->
    traverse_commands(fun(L) -> dup_module(L, sequential) end,
		      fun(L) -> [dup_module(L1, mkmod("parallel",num(L1,L))) || L1<-L] end,
		      Cs).
dup_module(L, ModName) -> lists:duplicate(length(L), ModName).
mkmod(PfxStr,N) -> list_to_atom(PfxStr++"_"++integer_to_list(N)).
    
%% Meta functions for the aggregate functions
traverse_commands(Fun, L) when is_list(L) -> Fun(L);
traverse_commands(Fun, {Seq, ParLs}) -> Fun(lists:append([Seq|ParLs])).
    
traverse_commands(Fseq, _Fpar, L) when is_list(L) -> Fseq(L);
traverse_commands(Fseq, Fpar, {Seq, ParLs}) -> lists:append([Fseq(Seq)|Fpar(ParLs)]).
    
%%%================================================================
-define(middle_dot, 0183).

set_default_print_freq_range_opts(Opts0, L) ->
    add_default_options(Opts0, [{ngroups, 10},
                                {min, 0},
                                {max, max_in_list(L)}
                               ]).

add_default_options(Opts0, DefaultOpts) ->
    [set_def_opt(Key,DefVal,Opts0) || {Key,DefVal} <- DefaultOpts].

set_def_opt(Key, DefaultValue, Opts) ->
    {Key, proplists:get_value(Key, Opts, DefaultValue)}.

max_in_list(L) ->
    case lists:last(L) of
        Max when is_integer(Max) -> Max;
        {Max,_} -> Max
    end.

do_print_frequency_ranges(L0, Options) ->
    [N,Min,Max] = [proplists:get_value(K,Options) || K <- [ngroups, min, max]],
    L = if 
            N>Max ->
                %% There will be less than the demanded number of classes,
                %% insert one last with zero values in it. That will force
                %% the generation of N classes.
                L0++[{N,0}];
            N=<Max ->
                L0
        end,
    try
        Interval = round((Max-Min)/N),
        IntervalLowerLimits = lists:seq(Min,Max,Interval),
        Ranges = [{I,I+Interval-1} || I <- IntervalLowerLimits],
        Acc0 = [{Rng,0} || Rng <- Ranges],
        Fs0 = get_frequencies(L, Acc0),
	SumVal = lists:sum([V||{_,V}<-Fs0]),
	Fs = with_percentage(Fs0, SumVal),
        DistInfo = [{"min", lists:min(L)},
                    {"mean", mean(L)},
                    {"median", median(L)},
                    {"max", lists:max(L)}],

	Npos_value = num_digits(SumVal),
	Npos_range = num_digits(Max),
        [%% Table heading:
         io_lib:format("Range~*s: ~s~n",[2*Npos_range-2,"", "Number in range"]),
         %% Line under heading:
         io_lib:format("~*c:~*c~n",[2*Npos_range+3,$-, max(16,Npos_value+10),$- ]),
         %% Lines with values:
         [io_lib:format("~*w - ~*w:  ~*w  ~5.1f% ~s~n",
                        [Npos_range,Rlow,
                         Npos_range,Rhigh,
                         Npos_value,Val,
                         Percent,
                         cond_prt_vals(DistInfo, Interv)
                        ])
          || {Interv={Rlow,Rhigh},Val,Percent} <- Fs],
         %% Line under the table for the total number of values:
         io_lib:format('~*c    ~*c~n',[2*Npos_range,32, Npos_value+3,$-]),
         %% The total number of values:
         io_lib:format('~*c      ~*w~n',[2*Npos_range,32, Npos_value,SumVal])
        ]            
    catch
	C:E ->
	    ct:pal('*** Failed printing (~p:~p) for~n~p~n',[C,E,L])
    end.

cond_prt_vals(LVs, CurrentInterval) ->
    [prt_val(Label, Value, CurrentInterval) || {Label,Value} <- LVs].

prt_val(Label, Value, CurrentInterval) ->
    case in_interval(Value, CurrentInterval) of
        true ->
            io_lib:format(" <-- ~s=" ++ if 
                                           is_float(Value) -> "~.1f";
                                           true -> "~p"
                                       end,
                          [Label,Value]);
        false ->
            ""
    end.

get_frequencies([{I,Num}|T], [{{Lower,Upper},Cnt}|Acc]) when Lower=<I,I=<Upper ->
    get_frequencies(T,  [{{Lower,Upper},Cnt+Num}|Acc]);
get_frequencies(L=[{I,_Num}|_], [Ah={{_Lower,Upper},_Cnt}|Acc]) when I>Upper ->
    [Ah | get_frequencies(L,Acc)];
get_frequencies([I|T], Acc) when is_integer(I) ->
    get_frequencies([{I,1}|T], Acc);
get_frequencies([], Acc) -> 
    Acc.

get_frequencies_no_range([]) ->
    io_lib:format("No values~n", []);
get_frequencies_no_range(L) ->
    [H|T] = lists:sort(L),
    get_frequencies_no_range(T, H, 1, []).

get_frequencies_no_range([H|T], H, N, Acc) ->
    get_frequencies_no_range(T, H, N+1, Acc);
get_frequencies_no_range([H1|T], H, N, Acc) ->
    get_frequencies_no_range(T, H1, 1, [{H,N}|Acc]);
get_frequencies_no_range([], H, N, Acc) ->
    lists:reverse(
      lists:keysort(2, [{H,N}|Acc])).

%% get_frequencies_percent(L) ->
%%     with_percentage(get_frequencies_no_range(L), length(L)).


with_percentage(Fs, Sum) ->
    [{Rng,Val,100*Val/Sum} || {Rng,Val} <- Fs].
   

num_digits(I) -> 1+trunc(math:log(I)/math:log(10)).

num(Elem, List) -> length(lists:takewhile(fun(E) -> E /= Elem end, List)) + 1.

%%%---- Just for naming an operation for readability
is_odd(I) -> (I rem 2) == 1.

in_interval(Value, {Rlow,Rhigh}) -> 
    try 
	Rlow=<round(Value) andalso round(Value)=<Rhigh
    catch 
	_:_ -> false
    end.

%%%================================================================
%%% Statistical functions

%%%---- Mean value
mean(L = [X|_]) when is_number(X) -> 
    lists:sum(L) / length(L);
mean(L = [{_Value,_Weight}|_]) -> 
    SumOfWeights = lists:sum([W||{_,W}<-L]),
    WeightedSum = lists:sum([W*V||{V,W}<-L]),
    WeightedSum / SumOfWeights;
mean(_) -> 
    undefined.
    
%%%---- Median
median(L = [X|_]) when is_number(X) ->
    Len = length(L),
    case is_odd(Len) of
	true ->
	    hd(lists:nthtail(Len div 2, L));
	false -> 
	    %%  1) L has at least one element (the one in the is_number test).
	    %%  2) Length is even.
	    %%     => Length >= 2
	    [M1,M2|_] = lists:nthtail((Len div 2)-1, L),
	    (M1+M2) / 2
    end;
%% integer Weights...
median(L = [{_Value,_Weight}|_]) ->
    median( lists:append([lists:duplicate(W,V) || {V,W} <- L]) );
median(_) ->
    undefined.

