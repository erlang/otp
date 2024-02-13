%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2024. All Rights Reserved.
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
-moduledoc """
Support in Common Test for running property-based tests.

This module helps running property-based tests in the `Common Test` framework.
One (or more) of the property testing tools

- [QuickCheck](http://www.quviq.com),
- [PropEr](https://proper-testing.github.io) or
- [Triq](https://github.com/krestenkrab/triq)

is assumed to be installed.

The idea with this module is to have a `Common Test` test suite calling a
property testing tool with special property test suites as defined by that tool.
The tests are collected in the `test` directory of the application. The `test`
directory has a subdirectory `property_test`, where everything needed for the
property tests are collected. The usual Erlang application directory structure
is assumed.

A typical `Common Test` test suite using `ct_property_test` is organized as
follows:

```erlang
-module(my_prop_test_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

 all() -> [prop_ftp_case].

 init_per_suite(Config) ->
     ct_property_test:init_per_suite(Config).

 %%%---- test case
 prop_ftp_case(Config) ->
     ct_property_test:quickcheck(
       ftp_simple_client_server:prop_ftp(),
       Config
      ).
```

and the the property test module (in this example
`ftp_simple_client_server.erl`) as almost a usual property testing module (More
examples are in [the User's Guide](ct_property_test_chapter.md)):

```erlang
-module(ftp_simple_client_server).
-export([prop_ftp/0...]).

-include_lib("common_test/include/ct_property_test.hrl").

prop_ftp() ->
    ?FORALL( ....
```
""".
-moduledoc(#{since => "OTP 17.3"}).

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

%%%================================================================
%%%
%%% API
%%% 

%%%----------------------------------------------------------------
%%%
%%% Search for a property tester in the lib path, and if found, compile
%%% the property tests
%%%
-doc """
init_per_suite(Config) -> Config | {skip, Reason}

Initializes and extends `Config` for property based testing.

This function investigates if support is available for either
[QuickCheck](http://www.quviq.com), [PropEr](https://proper-testing.github.io)
or [Triq](https://github.com/krestenkrab/triq) and compiles the properties with
the first tool found. It is supposed to be called in the
[`init_per_suite/1`](`init_per_suite/1`) function in a CommonTest test suite.

Which tools to check for, and in which order could be set with the option
`{prop_tools, list(eqc|proper|triq)}` in the CommonTest configuration `Config`.
The default value is `[eqc, proper, triq]` with `eqc` being the first one
searched for.

If no support is found for any tool, this function returns
`{skip, Explanation}`.

If support is found, the option `{property_test_tool,ToolModule}` with the
selected tool main module name (`eqc`, `proper` or `triq`) is added to the list
`Config` which then is returned.

The property tests are assumed to be in a subdirectory named `property_test`.
All found Erlang files in that directory are compiled with one of the macros
`'EQC'`, `'PROPER'` or `'TRIQ'` set, depending on which tool that is first
found. This could make parts of the Erlang property tests code to be included or
excluded with the macro directives `-ifdef(Macro).` or `-ifndef(Macro).`.

The file(s) in the `property_test` subdirectory could, or should, include the
ct_property_test include file:

```erlang
-include_lib("common_test/include/ct_property_test.hrl").
```

This included file will:

- Include the correct tool's include file
- Set the macro `'MOD_eqc'` to the correct module name for the selected tool.
  That is, the macro `'MOD_eqc'` is set to either `eqc`, `proper` or `triq`.
""".
-doc(#{since => <<"OTP 17.3">>}).
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

-doc false.
init_tool(Config) ->
    ToolsToCheck = proplists:get_value(prop_tools, Config, [eqc,proper,triq]),
    case which_module_exists(ToolsToCheck) of
	{ok,ToolModule} ->
            case code:where_is_file(lists:concat([ToolModule,".beam"])) of
                non_existing ->
                    ct:log("Found ~p, but ~ts was not found",
                           [ToolModule, lists:concat([ToolModule,".beam"])]),
                    {skip, "Strange Property testing tool installation"};
                ToolPath ->
                    ct:log("Found property tester ~p at ~ts",
                           [ToolModule, ToolPath]),
                    init_tool_extensions(ToolModule),
                    [{property_test_tool, ToolModule} | Config]
            end;
        not_found ->
            ct:log("No property tester found",[]),
            {skip, "No property testing tool found"}
    end.

init_tool_extensions(proper) ->
    ProperExtDir = code:lib_dir(common_test, proper_ext),
    true = code:add_patha(ProperExtDir),
    ct:log("Added ~ts to code path~n", [ProperExtDir]),
    ok;
init_tool_extensions(_) ->
    ok.

%%%----------------------------------------------------------------
%%%
%%% Call the found property tester (if any)
%%%
-doc """
quickcheck(Property, Config) -> true | {fail, Reason}

Calls the selected tool's function for running the `Property`. It is usually and
by historical reasons called quickcheck, and that is why that name is used in
this module (`ct_property_test`).

The result is returned in a form suitable for `Common Test` test suites.

This function is intended to be called in test cases in test suites.
""".
-doc(#{since => <<"OTP 17.3">>}).
quickcheck(Property, Config) ->
    Tool = proplists:get_value(property_test_tool,Config),
    F = function_name(quickcheck, Tool),
    mk_ct_return(Tool:F(Property)).


%%%----------------------------------------------------------------
%%%
%%% Present a nice table of the statem result
%%%
-doc """
present_result(Module, Cmds, Triple, Config) -> Result

Same as [`present_result(Module, Cmds, Triple, Config, [])`](`present_result/5`)
""".
-doc(#{since => <<"OTP 22.3">>}).
present_result(Module, Cmds, Triple, Config) ->
    present_result(Module, Cmds, Triple, Config, []).

-doc """
present_result(Module, Cmds, Triple, Config, Options) -> Result

Presents the result of _stateful (statem) property testing_ using the aggregate
function in PropEr, QuickCheck or other similar property testing tool.

It is assumed to be called inside the property called by `quickcheck/2`:

```erlang
...
RunResult = run_parallel_commands(?MODULE, Cmds),
ct_property_test:present_result(?MODULE, Cmds, RunResult, Config)
...
```

See the [User's Guide](ct_property_test_chapter.md#stateful1) for an example of
the usage and of the default printout.

The `StatisticsSpec` is a list of the tuples:

- `{Title::string(), CollectFun::fun/1}`
- `{Title::string(), FrequencyFun::/0, CollectFun::fun/1}`

Each tuple will produce one table in the order of their places in the list.

- `Title` will be the title of one result table
- `CollectFun` is called with one argument: the `Cmds`. It should return a list
  of the values to be counted. The following pre-defined functions exist:
  - `ct_property_test:cmnd_names/1` returns a list of commands (function calls)
    generated in the `Cmnd` sequence, without Module, Arguments and other
    details.
  - `ct_property_test:num_calls/1` returns a list of the length of commands
    lists
  - `ct_property_test:sequential_parallel/1` returns a list with information
    about sequential and parallel parts from `Tool:parallel_commands/1,2`
- `FrequencyFun/0` returns a fun/1 which is supposed to take a list of items as
  input, and return an iolist which will be printed as the table. Per default,
  the number of each item is counted and the percentage is printed for each. The
  list \[a,b,a,a,c] could for example return

  ```text
   ["a 60%\n","b 20%\n","c 20%\n"]
  ```

  which will be printed by the `print_fun`. The default `print_fun` will print
  it as:

  ```text
   a 60%
   b 20%
   c 20%
  ```

The default `StatisticsSpec` is:

- For sequential commands:

  ```text
  [{"Function calls", fun cmnd_names/1},
   {"Length of command sequences", fun print_frequency_ranges/0,
                                                    fun num_calls/1}]
  ```

- For parallel commands:

  ```erlang
  [{"Distribution sequential/parallel", fun sequential_parallel/1},
   {"Function calls", fun cmnd_names/1},
   {"Length of command sequences", fun print_frequency_ranges/0,
                                                    fun num_calls/1}]
  ```
""".
-doc(#{since => <<"OTP 22.3">>}).
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


-doc false.
title(Str, Fun) ->
    title(Str, Fun, fun io:format/2).

-doc false.
title(Str, Fun, PrintFun) ->
    fun(L) -> PrintFun("~n~s~n~n~s~n", [Str,Fun(L)]) end.

-doc false.
print_frequency() ->
    fun(L) ->
            [io_lib:format("~5.1f% ~p~n",[Pcnt,V])
             || {V,_Num,Pcnt} <-
                    with_percentage(get_frequencies_no_range(L), length(L))
            ]
    end.

-doc false.
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
mk_ct_return(true) ->
    true;
mk_ct_return(Other) ->
    {fail, Other}.

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
                    ct:log("No files found in ~tp", [Path]),
                    ok = file:set_cwd(Cwd),
                    {skip, "No files found"};
                {ok,FileNames} ->
                    BeamFiles = [F || F<-FileNames,
                                      filename:extension(F) == ".beam"],
                    ErlFiles = [F || F<-FileNames,
                                      filename:extension(F) == ".erl"],
                    _ = [file:delete(F) || F<-BeamFiles],
                    ct:log("Compiling in ~tp~n"
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
-doc false.
cmnd_names(Cs) -> traverse_commands(fun cmnd_name/1, Cs).
cmnd_name(L) ->  [F || {set,_Var,{call,_Mod,F,_As}} <- L].
    
-doc false.
num_calls(Cs) -> traverse_commands(fun num_call/1, Cs).
num_call(L) -> [length(L)].
    
-doc false.
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

