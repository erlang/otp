%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(bench).

%% User interface
-export([run/0]).

%% Exported to be used in spawn
-export([measure/4]).

%% Internal constants 
-define(MAX, 999999999999999).
-define(RANGE_MAX, 16#7ffffff).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Interface 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------------
%% run() -> _
%%
%% Compiles and runs all benchmarks in the current directory,
%% and creates a report
%%---------------------------------------------------------------------------
run() ->
    run(compiler_options()).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Generic Benchmark functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------------
%% compiler_options() -> OptionsList
%%	OptionsList = list() - See Erlang/OTP module compile
%%---------------------------------------------------------------------------
compiler_options() ->
    [report_errors, report_warnings].

%%---------------------------------------------------------------------------
%% run(OptionsList) ->
%%	OptionsList = list() - See Erlang/OTP module compile
%%
%% Help function to run/0.
%%---------------------------------------------------------------------------
run(OptionsList) ->
    Bms = compile_benchmarks(OptionsList),
    run_benchmarks(Bms),
    report().

%%---------------------------------------------------------------------------
%% compile_benchmarks(OptionsList) -> [BmInfo| _]
%%	OptionsList = list() - See Erlang/OTP module compile
%%	BmInfo = {Module, Iterations, [BmFunctionName| _]}
%%	Module = atom()
%%	Iterations = integer()
%%	BmFunctionName = atom()
%%  
%% Compiles all benchmark modules in the current directory and
%% returns info about the benchmarks. 
%%---------------------------------------------------------------------------
compile_benchmarks(OptionsList) ->
    {ok, FilesInCurrentDir} = file:list_dir("."),
    ErlFiles = [ErlFile || ErlFile <- lists:sort(FilesInCurrentDir), 
			 lists:suffix(".erl", ErlFile)],
    lists:foldr(fun(File, BmInfoAcc) -> 
			case lists:suffix("_bm.erl", File) of
			    true ->
				BmInfo = bm_compile(File, OptionsList),
				[BmInfo | BmInfoAcc];
			    false ->
				just_compile(File, OptionsList),
				BmInfoAcc
			end
		end, [], ErlFiles).
    
%%---------------------------------------------------------------------------
%%  just_compile(FileName, OptionsList) -> ok
%%	FileName = string() 
%%	OptionsList = list() - See Erlang/OTP module compile
%%  
%% Compiles a support module.
%%---------------------------------------------------------------------------
just_compile(FileName, OptionsList) ->
    io:format("Compiling ~s...\n", [FileName]), % Progress info to user
    case c:c(FileName, OptionsList) of
	{ok, _Mod} ->
	    ok;
	%% If compilation fails there is no point in trying to continue
	error -> 
	    Reason = 
		lists:flatten(
		  io_lib:format("Could not compile file ~s", [FileName])),
	    exit(self(), Reason)
    end.
%%---------------------------------------------------------------------------
%%  bm_compile(FileName, OptionsList) -> BmInfo
%%	FileName = string() 
%%	OptionsList = list() - See Erlang/OTP module compile
%%	BmInfo = {Module, Iterations, [BmFunctionName| _]}
%%	Iterations = integer()
%%	Module = atom()
%%	BmFunctionName = atom()
%%  
%% Compiles the benchmark module implemented in <FileName> and returns
%% information about the benchmark tests. 
%%---------------------------------------------------------------------------
bm_compile(FileName, OptionsList) ->
    io:format("Compiling ~s...\n", [FileName]), % Progress info to user
    case c:c(FileName, OptionsList) of
	{ok, Mod} ->
	    bm_cases(Mod);
	%% If compilation fails there is no point in trying to continue
	error -> 
	    Reason = 
		lists:flatten(
		  io_lib:format("Could not compile file ~s", [FileName])),
	    exit(self(), Reason)
    end.
%%---------------------------------------------------------------------------
%% bm_cases(Module) -> {Module, Iter, [BmFunctionName |_]}
%%	Module = atom() 
%%	Iter = integer()
%%	BmFunctionName = atom()
%%
%% Fetches the number of iterations and the names of the benchmark
%% functions for the module <Module>. 
%%---------------------------------------------------------------------------
bm_cases(Module) ->
    case catch Module:benchmarks() of
	{Iter, BmList} when integer(Iter), list(BmList) ->
	    {Module, Iter, BmList};
	%% The benchmark is incorrect implemented there is no point in
	%% trying to continue
	Other -> 
	    Reason = 
		lists:flatten(
		  io_lib:format("Incorrect return value: ~p " 
				"from ~p:benchmarks()",
				[Other, Module])),
	    exit(self(), Reason)
    end.
%%---------------------------------------------------------------------------
%% run_benchmarks(Bms) ->   
%%	Bms = [{Module, Iter, [BmFunctionName |_]} | _] 
%%	Module = atom() 
%%	Iter = integer()
%%	BmFunctionName = atom()
%%  	    
%% Runs all the benchmark tests described in <Bms>.
%%---------------------------------------------------------------------------
run_benchmarks(Bms) ->
    Ver = erlang:system_info(version),
    Machine = erlang:system_info(machine),
    SysInfo = {Ver,Machine},
   
    Res = [bms_run(Mod, Tests, Iter, SysInfo) || {Mod,Iter,Tests} <- Bms],

    %% Create an intermediate file that is later used to generate a bench
    %% mark report.
    Name = Ver ++ [$.|Machine] ++ ".bmres",
    {ok, IntermediatFile} = file:open(Name, [write]),

    %% Create mark that identifies version of the benchmark modules
    io:format(IntermediatFile, "~p.\n", [erlang:phash(Bms, ?RANGE_MAX)]),
    
    io:format(IntermediatFile, "~p.\n", [Res]),
    file:close(IntermediatFile).

%%---------------------------------------------------------------------------
%% bms_run(Module, BmTests, Iter, Info) ->  
%%	Module = atom(),
%%	BmTests = [BmFunctionName|_],
%%      BmFunctionName = atom()
%%	Iter = integer(),
%%	SysInfo = {Ver, Machine}
%%	Ver = string()
%%	Machine = string()
%%  
%% Runs all benchmark tests in module <Module>. 
%%---------------------------------------------------------------------------
bms_run(Module, BmTests, Iter, SysInfo) ->
    io:format("Running ~s:", [Module]),  % Progress info to user
    Res =
	{Module,{SysInfo,[{Bm, bm_run(Module, Bm, Iter)} || Bm <- BmTests]}},
    io:nl(),
    Res.
%%---------------------------------------------------------------------------
%% bm_run(Module, BmTest, Iter) -> Elapsed
%%	Module = atom(),
%%	BmTest = atom(),
%%	Iter = integer()
%%      Elapsed = integer()  - elapsed time in milliseconds.
%%  
%% Runs the benchmark Module:BmTest(Iter)  
%%---------------------------------------------------------------------------
bm_run(Module, BmTest, Iter) ->
    io:format(" ~s", [BmTest]),  % Progress info to user
    spawn_link(?MODULE, measure, [self(), Module, BmTest, Iter]),
    receive
	{Elapsed, ok} ->
	    Elapsed;
	{_Elapsed, Fault} ->
	    io:nl(),
	    Reason = 
		lists:flatten(
		  io_lib:format("~w", [Fault])),
	    exit(self(), Reason)
    end.
%%---------------------------------------------------------------------------
%% measure(Parent, Module, BmTest, Iter) -> _ 
%%	Parent = pid(),
%%	Module = atom(),
%%	BmTest = atom(),
%%	Iter = integer()
%%
%% Measures the time it take to execute Module:Bm(Iter)  
%% and send the result to <Parent>.
%%---------------------------------------------------------------------------
measure(Parent, Module, BmTest, Iter) ->
    statistics(runtime),
    Res = (catch apply(Module, BmTest, [Iter])), 
    {_TotalRunTime, TimeSinceLastCall} = statistics(runtime),
    Parent ! {TimeSinceLastCall, Res}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 	Report functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%---------------------------------------------------------------------------
%% report() -> _
%%  
%% Creates a report of the bench marking test that appeals to a human.
%% Currently this means creating a html-file. (Other formats could be added) 
%%---------------------------------------------------------------------------
report() ->
    {ok, AllFiles} = file:list_dir("."),
    BmResultFiles = [File || File <- AllFiles, lists:suffix(".bmres", File)],

    Results = fetch_bmres_data(BmResultFiles),
    create_report(Results).

%%---------------------------------------------------------------------------
%% fetch_bmres_data(BmResultFiles) -> Results 
%%	BmResultFiles = [FileName | _]
%%	FileName = string()
%%	Results = [[{Bm, Res} | _]]
%%      Bm   =  atom() - Name of benchmark module
%%	Res  = [{VersionInfo, [{Test, Time} | _]}]
%%	VersionInfo = {Ver, Machine}
%%	Ver = string()
%%	Machine = string()
%%	Test = atom()
%%	Time = integer()
%%
%% Reads result data from intermediate files
%%---------------------------------------------------------------------------
fetch_bmres_data(BmResultFiles) ->
    fetch_bmres_data(BmResultFiles, [], undefined).

%%---------------------------------------------------------------------------
%% fetch_bmres_data(BmResultFiles, AccResData, Check) -> Results 
%%	BmResultFiles = [FileName | _]
%%	FileName = string()
%%	AccResData = see Results fetch_bmres_data/1
%%	Check = integer() | undefined (first time)
%%
%% Help function to fetch_bmres_data/1
%%---------------------------------------------------------------------------
fetch_bmres_data([], AccResData, _Check) ->
    AccResData;

fetch_bmres_data([Name | BmResultFiles], AccResData, Check) ->
    {DataList, NewCheck} = read_bmres_file(Name, Check),
    fetch_bmres_data(BmResultFiles, [DataList| AccResData], NewCheck).

%%---------------------------------------------------------------------------
%% read_bmres_file(Name, Check) -> 
%%	Name = string()  
%%	Check = integer() | undefined  
%%
%%  Reads the data from the result files. Checks that all result
%%  files where created with the same set of tests.
%%---------------------------------------------------------------------------
read_bmres_file(Name, Check) ->
    case file:consult(Name) of
	{ok, [Check1, List]} when Check =:= undefined, integer(Check1) ->
	    {List, Check1};
	{ok, [Check, List]} when integer(Check) ->
	    {List, Check};
	{ok, [Check1, _List]} when integer(Check1) ->
	    Reason = 
		lists:flatten(
		  io_lib:format("Different test setup, remove old setup "
				"result by removing *.bmres files and "
				"try again", [])),
	    exit(self(), Reason); 
	{error, Reason} when atom(Reason) ->
	    exit(self(), Reason); 
	{error, Reason} ->
	    exit(self(), file:format(Reason))
    end.

%%---------------------------------------------------------------------------
%% create_report(Results) ->
%%	Results =  see Results fetch_bmres_data/1
%%  
%%  Organizes <Result> so it will be right for create_html_report/1  
%%  i.e. group results for the same benchmark test, run on different versions
%%  of erlang.
%%---------------------------------------------------------------------------
create_report(Results) -> 
    Dictionary = 
	lists:foldl(fun(BmResultList, Dict0) ->
			    lists:foldl(fun({Bm, VerResult}, Dict1) ->
						dict:append(Bm, VerResult, 
							    Dict1) 
					end,Dict0, BmResultList) 
		    end,
		    dict:new(), Results),

    create_html_report(dict:to_list(Dictionary)).
%%---------------------------------------------------------------------------
%%  create_html_report(ResultList) -> _
%%	ResultList = [{Bm, Res} | _]
%%	Bm   =  atom() - Name of benchmark module
%%	Res  = [{VersionInfo, [{Test, Time} | _]} | _] 
%%	VersionInfo = {Ver, Machine}
%%	Ver = string()
%%	Machine = string()
%%	Test = atom()
%%	Time = integer()
%%
%% Writes the result to an html-file 
%%---------------------------------------------------------------------------
create_html_report(ResultList) ->

    {ok, OutputFile} = file:open("index.html", [write]),

    %% Create the beginning of the result html-file.
    Head = Title = "Benchmark Results",
    io:put_chars(OutputFile, "<html>\n"),
    io:put_chars(OutputFile, "<head>\n"),
    io:format(OutputFile, "<title>~s</title>\n", [Title]),
    io:put_chars(OutputFile, "</head>\n"),
    io:put_chars(OutputFile, "<body bgcolor=\"#FFFFFF\" text=\"#000000\"" ++
		 " link=\"#0000FF\" vlink=\"#800080\" alink=\"#FF0000\">\n"),
    io:format(OutputFile, "<h1>~s</h1>\n", [Head]),

    %% Add the result tables
    lists:foreach(fun(Element) -> 
			  create_html_table(OutputFile, Element) end,
		  ResultList),
 
    %% Put in the end-html tags
    io:put_chars(OutputFile, "</body>\n"),
    io:put_chars(OutputFile, "</html>\n"),
    
    file:close(OutputFile).

%%---------------------------------------------------------------------------
%% create_html_table(File, {Bm, Res}) -> _
%%	File = file() - html file to write data to. 
%%	Bm  = atom() - Name of benchmark module
%%	Res = [{VersionInfo, [{Test, Time} | _]}]
%%	VersionInfo = {Ver, Machine}
%%	Ver = string()
%%	Machine = string()
%%	Test = atom()
%%	Time = integer()
%%
%% Creates a html table that displays the result of the benchmark <Bm>.
%%---------------------------------------------------------------------------
create_html_table(File, {Bm, Res}) ->

    {MinTime, Order} = min_time_and_sort(Res),  
        
    io:format(File, "<h2>~s</h2>\n" , [Bm]),
    
    %% Fun that calculates relative measure values and puts them in 
    %% a dictionary
    RelativeMesureFun =  fun({TestName, Time}, Dict1) ->
				 dict:append(TestName, Time/MinTime, Dict1) 
			 end,

    %% For all erlang versions that the benchmark tests has been run, 
    %% calculate the relative measure values and put them in a dictionary.
    ResultDict = 
	lists:foldl(fun({_VerInfo, Bms}, Dict0) ->
			    lists:foldl(RelativeMesureFun, Dict0, Bms) end,
		    dict:new(), Res),

    %% Create the table and its headings
    io:put_chars(File, "<table border=0 cellpadding=1><tr>"
		 "<td bgcolor=\"#000000\">\n"),
    io:put_chars(File, "<table cellpadding=3 border=0 cellspacing=1>\n"),
    io:put_chars(File, "<tr bgcolor=white>"),
    io:put_chars(File, "<td>Test</td>"),
    Heads = table_headers(Res),
    lists:foreach(fun({Ver,Machine}) -> io:format(File, "<td>~s<br>~s</td>",
						  [Ver,Machine]) end, Heads),
    io:put_chars(File, "</tr>\n"),
    
    %% Create table rows
    lists:foreach(fun(Name) -> 
			  create_html_row(File, Name, ResultDict) 
		  end, Order),

    %% Tabel end-tags
    io:put_chars(File, "</table></td></tr></table>\n"),
    
    %% Create link to benchmark source code
    io:format(File, "<p><a href=\"~s.erl\">Source for ~s.erl</a>\n",
	      [Bm,Bm]).

%%---------------------------------------------------------------------------
%% create_html_row(File, Name, Dict) -> _
%%	File = file() - html file to write data to. 
%%	Name = atom() - Name of benchmark test 
%%	Dict = dict() - Dictonary where the relative time measures for 
%%      the test can be found.
%%
%% Creates an actual html table-row.
%%---------------------------------------------------------------------------
create_html_row(File, Name, Dict) ->
    ReletiveTimes = dict:fetch(Name, Dict),
    io:put_chars(File, "<tr bgcolor=white>\n"),
    io:format(File, "<td>~s</td>", [Name]),
    lists:foreach(fun(Time) -> 
			  io:format(File, "<td>~-8.2f</td>", [Time]) end, 
		  ReletiveTimes),
    io:put_chars(File, "</tr>\n").

%%---------------------------------------------------------------------------
%% min_time_and_sort(ResultList) -> {MinTime, Order}
%%	ResultList = [{VersionInfo, [{Test, Time} | _]}] 
%%	MinTime = integer() - The execution time of the fastes test 
%%	Order = [BmFunctionName|_] - the order of the testcases in
%%      increasing execution time.
%%	BmFunctionName = atom()   
%%---------------------------------------------------------------------------
min_time_and_sort(ResultList) ->
    
    %% Use the results from the run on the highest version
    %% of Erlang as norm.
    {_, TestRes} = 
	lists:foldl(fun ({{Ver, _}, ResList}, 
			 {CurrentVer, _}) when Ver > CurrentVer ->
			    {Ver, ResList};
			(_, VerAndRes) ->
			    VerAndRes
		    end, {"0", []}, ResultList),
    
    {lists:foldl(fun ({_, Time0}, Min1) when Time0 < Min1 -> 
			 Time0;
		     (_, Min1) -> 
			 Min1
		 end, ?MAX, TestRes),
     [Name || {Name, _} <- lists:keysort(2, TestRes)]}.

%%---------------------------------------------------------------------------
%% table_headers(VerResultList) -> SysInfo
%%	VerResultList = [{{Ver, Machine},[{BmFunctionName, Time}]} | _]
%%	Ver = string()
%%	Machine = string()
%%	BmFunctionName = atom()
%%	Time = integer() 
%%	SysInfo = {Ver, Machine}
%%---------------------------------------------------------------------------
table_headers(VerResultList) ->
    [SysInfo || {SysInfo, _} <- VerResultList].
