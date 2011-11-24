%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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

%%% @doc Common Test Framework functions handling test specifications.
%%%
%%% <p>This module exports functions that are used within CT to
%%% scan and parse test specifikations.</p>
-module(ct_testspec).

-export([prepare_tests/1, prepare_tests/2, 
	 collect_tests_from_list/2, collect_tests_from_list/3,
	 collect_tests_from_file/2, collect_tests_from_file/3]).

-include("ct_util.hrl").

%%%------------------------------------------------------------------
%%% NOTE:
%%% Multiple testspecs may be used as input with the result that
%%% the data is merged. It's in this case up to the user to ensure
%%% there are no clashes in any "global" variables, such as logdir.
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% prepare_tests/2 compiles the testspec data into a list of tests
%%% to be run and a list of tests to be skipped, either for one
%%% particular node or for all nodes.
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Version 1 - extract and return all tests and skips for Node 
%%%             (incl all_nodes)
%%%-------------------------------------------------------------------
prepare_tests(TestSpec,Node) when is_record(TestSpec,testspec), is_atom(Node) ->
    case lists:keysearch(Node,1,prepare_tests(TestSpec)) of
	{value,{Node,Run,Skip}} ->
	    {Run,Skip};
	false ->
	    {[],[]}
    end.

%%%-------------------------------------------------------------------
%%% Version 2 - create and return a list of {Node,Run,Skip} tuples, 
%%% one for each node specified in the test specification. 
%%% The tuples in the Run list will have the form {Dir,Suites,Cases} 
%%% and the tuples in the Skip list will have the form 
%%% {Dir,Suites,Comment} or {Dir,Suite,Cases,Comment}. 
%%%-------------------------------------------------------------------
prepare_tests(TestSpec) when is_record(TestSpec,testspec) ->
    Tests = TestSpec#testspec.tests,
    %% Sort Tests into "flat" Run and Skip lists (not sorted per node).
    {Run,Skip} = get_run_and_skip(Tests,[],[]),
    %% Create initial list of {Node,{Run,Skip}} tuples
    NodeList = lists:map(fun(N) -> {N,{[],[]}} end, list_nodes(TestSpec)),
    %% Get all Run tests sorted per node basis.
    NodeList1 = run_per_node(Run,NodeList, 
			     TestSpec#testspec.merge_tests),    
    %% Get all Skip entries sorted per node basis.
    NodeList2 = skip_per_node(Skip,NodeList1),
    %% Change representation.
    Result=
	lists:map(fun({Node,{Run1,Skip1}}) ->
			  Run2 = lists:map(fun({D,{Ss,Cs}}) ->
						   {D,Ss,Cs}
					   end, Run1),
			  Skip2 = lists:map(fun({D,{Ss,Cmt}}) ->
						    {D,Ss,Cmt};
					       ({D,{S,Cs,Cmt}}) ->
						    {D,S,Cs,Cmt}
					    end, Skip1),
			  {Node,Run2,Skip2}
		  end, NodeList2),
    Result.

%% run_per_node/2 takes the Run list as input and returns a list
%% of {Node,RunPerNode,[]} tuples where the tests have been sorted
%% on a per node basis.
run_per_node([{{Node,Dir},Test}|Ts],Result, MergeTests) ->
    {value,{Node,{Run,Skip}}} = lists:keysearch(Node,1,Result),
    Run1 = case MergeTests of
	       false ->
		   append({Dir, Test}, Run);
	       true ->
		   merge_tests(Dir,Test,Run)
	   end,
    run_per_node(Ts,insert_in_order({Node,{Run1,Skip}},Result), 
		 MergeTests);
run_per_node([],Result,_) ->
    Result.

merge_tests(Dir,Test={all,_},TestDirs) ->
    %% overwrite all previous entries for Dir
    TestDirs1 = lists:filter(fun({D,_}) when D==Dir -> 
				     false;
				(_) -> 
				     true
			     end,TestDirs),
    insert_in_order({Dir,Test},TestDirs1);
merge_tests(Dir,Test={Suite,all},TestDirs) ->
    TestDirs1 = lists:filter(fun({D,{S,_}}) when D==Dir,S==Suite -> 
				     false;
				(_) -> 
				     true
			     end,TestDirs),
    TestDirs1++[{Dir,Test}];
merge_tests(Dir,Test,TestDirs) ->
    merge_suites(Dir,Test,TestDirs).

merge_suites(Dir,{Suite,Cases},[{Dir,{Suite,Cases0}}|Dirs]) ->
    Cases1 = insert_in_order(Cases,Cases0),
    [{Dir,{Suite,Cases1}}|Dirs];
merge_suites(Dir,Test,[Other|Dirs]) ->
    [Other|merge_suites(Dir,Test,Dirs)];
merge_suites(Dir,Test,[]) ->
    [{Dir,Test}].

%% skip_per_node/2 takes the Skip list as input and returns a list
%% of {Node,RunPerNode,SkipPerNode} tuples where the skips have been 
%% sorted on a per node basis.
skip_per_node([{{Node,Dir},Test}|Ts],Result) ->
    {value,{Node,{Run,Skip}}} = lists:keysearch(Node,1,Result),
    Skip1 = [{Dir,Test}|Skip],
    skip_per_node(Ts,insert_in_order({Node,{Run,Skip1}},Result));
skip_per_node([],Result) -> 
    Result.

%% get_run_and_skip/3 takes a list of test terms as input and sorts
%% them into a list of Run tests and a list of Skip entries. The
%% elements all have the form 
%%
%%   {{Node,Dir},TestData} 
%%
%% TestData has the form:
%%
%%   Run entry:  {Suite,Cases}          
%%
%%   Skip entry: {Suites,Comment} or {Suite,Cases,Comment}
%%
get_run_and_skip([{{Node,Dir},Suites}|Tests],Run,Skip) ->    
    TestDir = ct_util:get_testdir(Dir,catch element(1,hd(Suites))),
    case lists:keysearch(all,1,Suites) of
	{value,_} ->				% all Suites in Dir
	    Skipped = get_skipped_suites(Node,TestDir,Suites),
	    %% note: this adds an 'all' test even if only skip is specified,
	    %% probably a good thing cause it gets logged as skipped then
	    get_run_and_skip(Tests,
			     [[{{Node,TestDir},{all,all}}]|Run],
			     [Skipped|Skip]);
	false ->
	    {R,S} = prepare_suites(Node,TestDir,Suites,[],[]),
	    get_run_and_skip(Tests,[R|Run],[S|Skip])
    end;
get_run_and_skip([],Run,Skip) ->
    {lists:flatten(lists:reverse(Run)),
     lists:flatten(lists:reverse(Skip))}.

prepare_suites(Node,Dir,[{Suite,Cases}|Suites],Run,Skip) ->
    case lists:member(all,Cases) of
	true ->					% all Cases in Suite
	    Skipped = get_skipped_cases(Node,Dir,Suite,Cases),
	    %% note: this adds an 'all' test even if only skip is specified
	    prepare_suites(Node,Dir,Suites,
			   [[{{Node,Dir},{Suite,all}}]|Run],
			   [Skipped|Skip]);
	false ->
	    {RL,SL} = prepare_cases(Node,Dir,Suite,Cases),
	    prepare_suites(Node,Dir,Suites,[RL|Run],[SL|Skip])
    end;
prepare_suites(_Node,_Dir,[],Run,Skip) ->
    {lists:flatten(lists:reverse(Run)),
     lists:flatten(lists:reverse(Skip))}.

prepare_cases(Node,Dir,Suite,Cases) ->
    case get_skipped_cases(Node,Dir,Suite,Cases) of
	SkipAll=[{{Node,Dir},{Suite,_Cmt}}] ->		% all cases to be skipped
	    %% note: this adds an 'all' test even if only skip is specified
	    {[{{Node,Dir},{Suite,all}}],SkipAll};
	Skipped ->
	    %% note: this adds a test even if only skip is specified
	    PrepC = lists:foldr(fun({{G,Cs},{skip,_Cmt}}, Acc) when
					  is_atom(G) ->
					case lists:keymember(G, 1, Cases) of
					    true ->
						Acc;
					    false ->
						[{skipped,G,Cs}|Acc]
					end;
				   ({C,{skip,_Cmt}},Acc) ->
					case lists:member(C,Cases) of
					    true ->
						Acc;
					    false ->
						[C|Acc]
					end;
				   (C,Acc) -> [C|Acc]
				end, [], Cases),
    {{{Node,Dir},{Suite,PrepC}},Skipped}
    end.

get_skipped_suites(Node,Dir,Suites) ->
    lists:flatten(get_skipped_suites1(Node,Dir,Suites)).

get_skipped_suites1(Node,Dir,[{Suite,Cases}|Suites]) ->
    SkippedCases = get_skipped_cases(Node,Dir,Suite,Cases),
    [SkippedCases|get_skipped_suites1(Node,Dir,Suites)];
get_skipped_suites1(_,_,[]) ->
    [].

get_skipped_cases(Node,Dir,Suite,Cases) ->
    case lists:keysearch(all,1,Cases) of
	{value,{all,{skip,Cmt}}} ->
	    [{{Node,Dir},{Suite,Cmt}}];
	_ ->
	    get_skipped_cases1(Node,Dir,Suite,Cases)
    end.

get_skipped_cases1(Node,Dir,Suite,[{Case,{skip,Cmt}}|Cs]) ->
    [{{Node,Dir},{Suite,Case,Cmt}}|get_skipped_cases1(Node,Dir,Suite,Cs)];
get_skipped_cases1(Node,Dir,Suite,[_Case|Cs]) ->
    get_skipped_cases1(Node,Dir,Suite,Cs);
get_skipped_cases1(_,_,_,[]) ->
    [].	

%%% collect_tests_from_file reads a testspec file and returns a record
%%% containing the data found.
collect_tests_from_file(Specs, Relaxed) ->
    collect_tests_from_file(Specs,[node()],Relaxed).

collect_tests_from_file(Specs,Nodes,Relaxed) when is_list(Nodes) ->
    NodeRefs = lists:map(fun(N) -> {undefined,N} end, Nodes),
    catch collect_tests_from_file1(Specs,#testspec{nodes=NodeRefs},Relaxed).

collect_tests_from_file1([Spec|Specs],TestSpec,Relaxed) ->
    SpecDir = filename:dirname(filename:absname(Spec)),
    case file:consult(Spec) of
	{ok,Terms} ->	    
	    TestSpec1 = collect_tests(Terms,
				      TestSpec#testspec{spec_dir=SpecDir},
				      Relaxed),
	    collect_tests_from_file1(Specs,TestSpec1,Relaxed);
	{error,Reason} ->
	    ReasonStr =
		lists:flatten(io_lib:format("~s",
					    [file:format_error(Reason)])),
	    throw({error,{Spec,ReasonStr}})
    end;
collect_tests_from_file1([],TS=#testspec{config=Cfgs,event_handler=EvHs,
					 include=Incl,tests=Tests},_) ->
    TS#testspec{config=lists:reverse(Cfgs),
		event_handler=lists:reverse(EvHs),
		include=lists:reverse(Incl),
		tests=lists:flatten(Tests)}.

collect_tests_from_list(Terms,Relaxed) ->
    collect_tests_from_list(Terms,[node()],Relaxed).

collect_tests_from_list(Terms,Nodes,Relaxed) when is_list(Nodes) ->
    {ok,Cwd} = file:get_cwd(),
    NodeRefs = lists:map(fun(N) -> {undefined,N} end, Nodes),
    case catch collect_tests(Terms,#testspec{nodes=NodeRefs,
					     spec_dir=Cwd},
			     Relaxed) of
	E = {error,_} ->
	    E;
	TS ->
	    #testspec{config=Cfgs,event_handler=EvHs,include=Incl,tests=Tests} = TS,
	    TS#testspec{config=lists:reverse(Cfgs),
			event_handler=lists:reverse(EvHs),
			include=lists:reverse(Incl),
			tests=lists:flatten(Tests)}
    end.
    
collect_tests(Terms,TestSpec,Relaxed) ->
    put(relaxed,Relaxed),
    TestSpec1 = get_global(Terms,TestSpec),
    TestSpec2 = get_all_nodes(Terms,TestSpec1),
    {Terms2, TestSpec3} = filter_init_terms(Terms, [], TestSpec2),
    add_tests(Terms2,TestSpec3).

get_global([{merge_tests, Bool} | Ts], Spec) ->
    get_global(Ts,Spec#testspec{ merge_tests = Bool });
get_global([{alias,Ref,Dir}|Ts],Spec=#testspec{alias=Refs}) ->
    get_global(Ts,Spec#testspec{alias=[{Ref,get_absdir(Dir,Spec)}|Refs]});
get_global([{node,Ref,Node}|Ts],Spec=#testspec{nodes=Refs}) ->
    get_global(Ts,Spec#testspec{nodes=[{Ref,Node}|lists:keydelete(Node,2,Refs)]});
get_global([_|Ts],Spec) -> get_global(Ts,Spec);
get_global([],Spec) -> Spec.

get_absfile(Callback, FullName,#testspec{spec_dir=SpecDir}) ->
    % we need to temporary switch to new cwd here, because
    % otherwise config files cannot be found
    {ok, OldWd} = file:get_cwd(),
    ok = file:set_cwd(SpecDir),
    R =  Callback:check_parameter(FullName),
    ok = file:set_cwd(OldWd),
    case R of
	{ok, {file, FullName}}->
	    File = filename:basename(FullName),
	    Dir = get_absname(filename:dirname(FullName),SpecDir),
	    filename:join(Dir,File);
	{ok, {config, FullName}}->
	    FullName;
	{error, {nofile, FullName}}->
	    FullName;
	{error, {wrong_config, FullName}}->
	    FullName
    end.

get_absfile(FullName,#testspec{spec_dir=SpecDir}) ->
    File = filename:basename(FullName),
    Dir = get_absname(filename:dirname(FullName),SpecDir),
    filename:join(Dir,File).

get_absdir(Dir,#testspec{spec_dir=SpecDir}) ->
    get_absname(Dir,SpecDir).

get_absname(TestDir,SpecDir) ->
    AbsName = filename:absname(TestDir,SpecDir),
    TestDirName = filename:basename(AbsName),
    Path = filename:dirname(AbsName),
    TopDir = filename:basename(Path),
    Path1 =
	case TopDir of
	    "." ->
		[_|Rev] = lists:reverse(filename:split(Path)),
		filename:join(lists:reverse(Rev));
	    ".." ->
		[_,_|Rev] = lists:reverse(filename:split(Path)),
		filename:join(lists:reverse(Rev));
	    _ ->
		Path
	end,
    filename:join(Path1,TestDirName).

%% go through all tests and register all nodes found
get_all_nodes([{suites,Nodes,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{suites,Node,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{cases,Nodes,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{cases,Node,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{skip_suites,Nodes,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{skip_suites,Node,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{skip_cases,Nodes,_,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{skip_cases,Node,_,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([_|Ts],Spec) ->
    get_all_nodes(Ts,Spec);
get_all_nodes([],Spec) ->
    Spec.

filter_init_terms([{init, InitOptions}|Ts], NewTerms, Spec)->
    filter_init_terms([{init, list_nodes(Spec), InitOptions}|Ts], NewTerms, Spec);
filter_init_terms([{init, NodeRef, InitOptions}|Ts], NewTerms, Spec)
    when is_atom(NodeRef)->
    filter_init_terms([{init, [NodeRef], InitOptions}|Ts], NewTerms, Spec);
filter_init_terms([{init, NodeRefs, InitOption}|Ts], NewTerms, Spec) when is_tuple(InitOption) ->
    filter_init_terms([{init, NodeRefs, [InitOption]}|Ts], NewTerms, Spec);
filter_init_terms([{init, [NodeRef|NodeRefs], InitOptions}|Ts], NewTerms, Spec=#testspec{init=InitData})->
    NodeStartOptions = case lists:keyfind(node_start, 1, InitOptions) of
	{node_start, NSOptions}->
	    case lists:keyfind(callback_module, 1, NSOptions) of
		{callback_module, _Callback}->
		    NSOptions;
		false->
		    [{callback_module, ct_slave}|NSOptions]
	    end;
	false->
	    []
    end,
    EvalTerms = case lists:keyfind(eval, 1, InitOptions) of
	{eval, MFA} when is_tuple(MFA)->
	    [MFA];
	{eval, MFAs} when is_list(MFAs)->
	    MFAs;
	false->
	    []
    end,
    Node = ref2node(NodeRef,Spec#testspec.nodes),
    InitData2 = add_option({node_start, NodeStartOptions}, Node, InitData, true),
    InitData3 = add_option({eval, EvalTerms}, Node, InitData2, false),
    filter_init_terms([{init, NodeRefs, InitOptions}|Ts], NewTerms, Spec#testspec{init=InitData3});
filter_init_terms([{init, [], _}|Ts], NewTerms, Spec)->
    filter_init_terms(Ts, NewTerms, Spec);
filter_init_terms([Term|Ts], NewTerms, Spec)->
    filter_init_terms(Ts, [Term|NewTerms], Spec);
filter_init_terms([], NewTerms, Spec)->
    {lists:reverse(NewTerms), Spec}.

add_option({Key, Value}, Node, List, WarnIfExists) when is_list(Value)->
    OldOptions = case lists:keyfind(Node, 1, List) of
	{Node, Options}->
	    Options;
	false->
	    []
    end,
    NewOption = case lists:keyfind(Key, 1, OldOptions) of
	{Key, OldOption} when WarnIfExists, OldOption/=[]->
	    io:format("There is an option ~w=~w already defined for node ~p, skipping new ~w~n",
		[Key, OldOption, Node, Value]),
	    OldOption;
	{Key, OldOption}->
	    OldOption ++ Value;
	false->
	    Value
    end,
    lists:keystore(Node, 1, List,
	{Node, lists:keystore(Key, 1, OldOptions, {Key, NewOption})});
add_option({Key, Value}, Node, List, WarnIfExists)->
    add_option({Key, [Value]}, Node, List, WarnIfExists).

save_nodes(Nodes,Spec=#testspec{nodes=NodeRefs}) ->
    NodeRefs1 =
	lists:foldr(fun(all_nodes,NR) ->
			    NR;
		       (Node,NR) ->
			    case lists:keymember(Node,1,NR) of
				true ->
				    NR;
				false ->
				    case lists:keymember(Node,2,NR) of
					true -> 
					    NR;
					false -> 
					    [{undefined,Node}|NR]
				    end
			    end
		    end,NodeRefs,Nodes),
    Spec#testspec{nodes=NodeRefs1}.    							

list_nodes(#testspec{nodes=NodeRefs}) ->
    lists:map(fun({_Ref,Node}) -> Node end, NodeRefs).		      



%%     ---------------------------------------------------------
%%   /                                                           \
%%  |  When adding tests, remember to update valid_terms/0 also!  |
%%   \                                                           /
%%     ---------------------------------------------------------


%% Associate a "global" logdir with all nodes
%% except those with specific logdir, e.g:
%% ["/tmp/logdir",{ct1@finwe,"/tmp/logdir2"}]
%% means all nodes should write to /tmp/logdir
%% except ct1@finwe that should use /tmp/logdir2.

%% --- logdir ---
add_tests([{logdir,all_nodes,Dir}|Ts],Spec) ->
    Dirs = Spec#testspec.logdir,
    Tests = [{logdir,N,get_absdir(Dir,Spec)} || 
		N <- list_nodes(Spec),
		lists:keymember(ref2node(N,Spec#testspec.nodes),
				1,Dirs) == false],
    add_tests(Tests++Ts,Spec);
add_tests([{logdir,Nodes,Dir}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,logdir,[Dir],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);    
add_tests([{logdir,Node,Dir}|Ts],Spec) ->
    Dirs = Spec#testspec.logdir,
    Dirs1 = [{ref2node(Node,Spec#testspec.nodes),get_absdir(Dir,Spec)} |
	     lists:keydelete(ref2node(Node,Spec#testspec.nodes),1,Dirs)],
    add_tests(Ts,Spec#testspec{logdir=Dirs1});
add_tests([{logdir,Dir}|Ts],Spec) ->
    add_tests([{logdir,all_nodes,Dir}|Ts],Spec);

%% --- logopts ---
add_tests([{logopts,all_nodes,Opts}|Ts],Spec) ->
    LogOpts = Spec#testspec.logopts,
    Tests = [{logopts,N,Opts} ||
		N <- list_nodes(Spec),
		lists:keymember(ref2node(N,Spec#testspec.nodes),1,
				LogOpts) == false],
    add_tests(Tests++Ts,Spec);
add_tests([{logopts,Nodes,Opts}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,logopts,[Opts],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{logopts,Node,Opts}|Ts],Spec) ->
    LogOpts = Spec#testspec.logopts,
    LogOpts1 = [{ref2node(Node,Spec#testspec.nodes),Opts} |
		lists:keydelete(ref2node(Node,Spec#testspec.nodes),
				1,LogOpts)],
    add_tests(Ts,Spec#testspec{logopts=LogOpts1});
add_tests([{logopts,Opts}|Ts],Spec) ->
    add_tests([{logopts,all_nodes,Opts}|Ts],Spec);

%% --- label ---
add_tests([{label,all_nodes,Lbl}|Ts],Spec) ->
    Labels = Spec#testspec.label,
    Tests = [{label,N,Lbl} || N <- list_nodes(Spec),
			      lists:keymember(ref2node(N,Spec#testspec.nodes),
					      1,Labels) == false],
    add_tests(Tests++Ts,Spec);
add_tests([{label,Nodes,Lbl}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,label,[Lbl],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{label,Node,Lbl}|Ts],Spec) ->
    Labels = Spec#testspec.label,
    Labels1 = [{ref2node(Node,Spec#testspec.nodes),Lbl} |
	       lists:keydelete(ref2node(Node,Spec#testspec.nodes),1,Labels)],
    add_tests(Ts,Spec#testspec{label=Labels1});
add_tests([{label,Lbl}|Ts],Spec) ->
    add_tests([{label,all_nodes,Lbl}|Ts],Spec);

%% --- cover ---
add_tests([{cover,all_nodes,File}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {cover,N,File} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{cover,Nodes,File}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,cover,[File],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);    
add_tests([{cover,Node,File}|Ts],Spec) ->
    CoverFs = Spec#testspec.cover,
    CoverFs1 = [{ref2node(Node,Spec#testspec.nodes),get_absfile(File,Spec)} |
		lists:keydelete(ref2node(Node,Spec#testspec.nodes),1,CoverFs)],
    add_tests(Ts,Spec#testspec{cover=CoverFs1});
add_tests([{cover,File}|Ts],Spec) ->
    add_tests([{cover,all_nodes,File}|Ts],Spec);

%% --- multiply_timetraps ---
add_tests([{multiply_timetraps,all_nodes,MT}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {multiply_timetraps,N,MT} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{multiply_timetraps,Nodes,MT}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,multiply_timetraps,[MT],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{multiply_timetraps,Node,MT}|Ts],Spec) ->
    MTs = Spec#testspec.multiply_timetraps,
    MTs1 = [{ref2node(Node,Spec#testspec.nodes),MT} |
	    lists:keydelete(ref2node(Node,Spec#testspec.nodes),1,MTs)],
    add_tests(Ts,Spec#testspec{multiply_timetraps=MTs1});
add_tests([{multiply_timetraps,MT}|Ts],Spec) ->
    add_tests([{multiply_timetraps,all_nodes,MT}|Ts],Spec);

%% --- scale_timetraps ---
add_tests([{scale_timetraps,all_nodes,ST}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {scale_timetraps,N,ST} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{scale_timetraps,Nodes,ST}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,scale_timetraps,[ST],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{scale_timetraps,Node,ST}|Ts],Spec) ->
    STs = Spec#testspec.scale_timetraps,
    STs1 = [{ref2node(Node,Spec#testspec.nodes),ST} |
	    lists:keydelete(ref2node(Node,Spec#testspec.nodes),1,STs)],
    add_tests(Ts,Spec#testspec{scale_timetraps=STs1});
add_tests([{scale_timetraps,ST}|Ts],Spec) ->
    add_tests([{scale_timetraps,all_nodes,ST}|Ts],Spec);

%% --- config ---
add_tests([{config,all_nodes,Files}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {config,N,Files} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{config,Nodes,Files}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,config,[Files],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{config,Node,[F|Fs]}|Ts],Spec) when is_list(F) ->
    Cfgs = Spec#testspec.config,
    Node1 = ref2node(Node,Spec#testspec.nodes),
    add_tests([{config,Node,Fs}|Ts],
	      Spec#testspec{config=[{Node1,get_absfile(F,Spec)}|Cfgs]});
add_tests([{config,_Node,[]}|Ts],Spec) ->
    add_tests(Ts,Spec);
add_tests([{config,Node,F}|Ts],Spec) ->
    add_tests([{config,Node,[F]}|Ts],Spec);
add_tests([{config,Files}|Ts],Spec) ->
    add_tests([{config,all_nodes,Files}|Ts],Spec);


%% --- userconfig ---
add_tests([{userconfig,all_nodes,CBF}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {userconfig,N,CBF} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{userconfig,Nodes,CBF}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,userconfig,[CBF],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{userconfig,Node,[{Callback, Config}|CBF]}|Ts],Spec) ->
    Cfgs = Spec#testspec.userconfig,
    Node1 = ref2node(Node,Spec#testspec.nodes),
    add_tests([{userconfig,Node,CBF}|Ts],
	      Spec#testspec{userconfig=[{Node1,{Callback,
				get_absfile(Callback, Config ,Spec)}}|Cfgs]});
add_tests([{userconfig,_Node,[]}|Ts],Spec) ->
    add_tests(Ts,Spec);
add_tests([{userconfig,Node,CBF}|Ts],Spec) ->
    add_tests([{userconfig,Node,[CBF]}|Ts],Spec);
add_tests([{userconfig,CBF}|Ts],Spec) ->
    add_tests([{userconfig,all_nodes,CBF}|Ts],Spec);

%% --- event_handler ---
add_tests([{event_handler,all_nodes,Hs}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {event_handler,N,Hs,[]} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{event_handler,all_nodes,Hs,Args}|Ts],Spec) when is_list(Args) ->
    Tests = lists:map(fun(N) -> {event_handler,N,Hs,Args} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{event_handler,Hs}|Ts],Spec) ->
    add_tests([{event_handler,all_nodes,Hs,[]}|Ts],Spec);
add_tests([{event_handler,HsOrNodes,HsOrArgs}|Ts],Spec) ->
    case is_noderef(HsOrNodes,Spec#testspec.nodes) of
	true ->					% HsOrNodes == Nodes, HsOrArgs == Hs
	    case {HsOrNodes,HsOrArgs} of
		{Nodes,Hs} when is_list(Nodes) ->
		    Ts1 = separate(Nodes,event_handler,[Hs,[]],Ts,
				   Spec#testspec.nodes),
		    add_tests(Ts1,Spec);
		{_Node,[]} ->		    
		    add_tests(Ts,Spec);
		{Node,HOrHs} ->
		    EvHs = Spec#testspec.event_handler,
		    Node1 = ref2node(Node,Spec#testspec.nodes),
		    case HOrHs of
			[H|Hs] when is_atom(H) ->
			    add_tests([{event_handler,Node,Hs}|Ts],
				      Spec#testspec{event_handler=[{Node1,H,[]}|EvHs]});
			H when is_atom(H) ->
			    add_tests(Ts,Spec#testspec{event_handler=[{Node1,H,[]}|EvHs]})
		    end
	    end;
	false ->				% HsOrNodes == Hs, HsOrArgs == Args
	    add_tests([{event_handler,all_nodes,HsOrNodes,HsOrArgs}|Ts],Spec)
    end;
add_tests([{event_handler,Nodes,Hs,Args}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,event_handler,[Hs,Args],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{event_handler,Node,[H|Hs],Args}|Ts],Spec) when is_atom(H) ->
    EvHs = Spec#testspec.event_handler,
    Node1 = ref2node(Node,Spec#testspec.nodes),
    add_tests([{event_handler,Node,Hs,Args}|Ts],
	      Spec#testspec{event_handler=[{Node1,H,Args}|EvHs]});
add_tests([{event_handler,_Node,[],_Args}|Ts],Spec) ->
    add_tests(Ts,Spec);
add_tests([{event_handler,Node,H,Args}|Ts],Spec) when is_atom(H) ->
    EvHs = Spec#testspec.event_handler,
    Node1 = ref2node(Node,Spec#testspec.nodes),
    add_tests(Ts,Spec#testspec{event_handler=[{Node1,H,Args}|EvHs]});

%% --- ct_hooks --
add_tests([{ct_hooks, all_nodes, Hooks} | Ts], Spec) ->
    Tests = [{ct_hooks,N,Hooks} || N <- list_nodes(Spec)],
    add_tests(Tests ++ Ts, Spec);
add_tests([{ct_hooks, Node, [Hook|Hooks]}|Ts], Spec) ->
    SuiteCbs = Spec#testspec.ct_hooks,
    Node1 = ref2node(Node,Spec#testspec.nodes),
    add_tests([{ct_hooks, Node, Hooks} | Ts],
	      Spec#testspec{ct_hooks = [{Node1,Hook} | SuiteCbs]});
add_tests([{ct_hooks, _Node, []}|Ts], Spec) ->
    add_tests(Ts, Spec);
add_tests([{ct_hooks, Hooks}|Ts], Spec) ->
    add_tests([{ct_hooks, all_nodes, Hooks}|Ts], Spec);

%% -- enable_builtin_hooks --
add_tests([{enable_builtin_hooks,Bool}|Ts],Spec) ->
    add_tests(Ts, Spec#testspec{ enable_builtin_hooks = Bool });

%% --- include ---
add_tests([{include,all_nodes,InclDirs}|Ts],Spec) ->
    Tests = lists:map(fun(N) -> {include,N,InclDirs} end, list_nodes(Spec)),
    add_tests(Tests++Ts,Spec);
add_tests([{include,Nodes,InclDirs}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,include,[InclDirs],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{include,Node,[D|Ds]}|Ts],Spec) when is_list(D) ->
    Dirs = Spec#testspec.include,
    Node1 = ref2node(Node,Spec#testspec.nodes),
    add_tests([{include,Node,Ds}|Ts],
	      Spec#testspec{include=[{Node1,get_absdir(D,Spec)}|Dirs]});
add_tests([{include,_Node,[]}|Ts],Spec) ->
    add_tests(Ts,Spec);
add_tests([{include,Node,D}|Ts],Spec) ->
    add_tests([{include,Node,[D]}|Ts],Spec);
add_tests([{include,InclDirs}|Ts],Spec) ->
    add_tests([{include,all_nodes,InclDirs}|Ts],Spec);

%% --- suites ---
add_tests([{suites,all_nodes,Dir,Ss}|Ts],Spec) ->
    add_tests([{suites,list_nodes(Spec),Dir,Ss}|Ts],Spec);
add_tests([{suites,Dir,Ss}|Ts],Spec) ->
    add_tests([{suites,all_nodes,Dir,Ss}|Ts],Spec);
add_tests([{suites,Nodes,Dir,Ss}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,suites,[Dir,Ss],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{suites,Node,Dir,Ss}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_suites(ref2node(Node,Spec#testspec.nodes),
			   ref2dir(Dir,Spec#testspec.alias),
			   Ss,Tests, Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- groups ---
%% Later make it possible to specify group execution properties
%% that will override thse in the suite. Also make it possible
%% create dynamic groups in specification, i.e. to group test cases
%% by means of groups defined only in the test specification.
add_tests([{groups,all_nodes,Dir,Suite,Gs}|Ts],Spec) ->
    add_tests([{groups,list_nodes(Spec),Dir,Suite,Gs}|Ts],Spec);
add_tests([{groups,all_nodes,Dir,Suite,Gs,{cases,TCs}}|Ts],Spec) ->
    add_tests([{groups,list_nodes(Spec),Dir,Suite,Gs,{cases,TCs}}|Ts],Spec);
add_tests([{groups,Dir,Suite,Gs}|Ts],Spec) ->
    add_tests([{groups,all_nodes,Dir,Suite,Gs}|Ts],Spec);
add_tests([{groups,Dir,Suite,Gs,{cases,TCs}}|Ts],Spec) ->
    add_tests([{groups,all_nodes,Dir,Suite,Gs,{cases,TCs}}|Ts],Spec);
add_tests([{groups,Nodes,Dir,Suite,Gs}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,groups,[Dir,Suite,Gs],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{groups,Nodes,Dir,Suite,Gs,{cases,TCs}}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,groups,[Dir,Suite,Gs,{cases,TCs}],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{groups,Node,Dir,Suite,Gs}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_groups(ref2node(Node,Spec#testspec.nodes),
			   ref2dir(Dir,Spec#testspec.alias),
			   Suite,Gs,all,Tests,
			   Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});
add_tests([{groups,Node,Dir,Suite,Gs,{cases,TCs}}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_groups(ref2node(Node,Spec#testspec.nodes),
			   ref2dir(Dir,Spec#testspec.alias),
			   Suite,Gs,TCs,Tests,
			   Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- cases ---
add_tests([{cases,all_nodes,Dir,Suite,Cs}|Ts],Spec) ->
    add_tests([{cases,list_nodes(Spec),Dir,Suite,Cs}|Ts],Spec);
add_tests([{cases,Dir,Suite,Cs}|Ts],Spec) ->
    add_tests([{cases,all_nodes,Dir,Suite,Cs}|Ts],Spec);
add_tests([{cases,Nodes,Dir,Suite,Cs}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,cases,[Dir,Suite,Cs],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{cases,Node,Dir,Suite,Cs}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_cases(ref2node(Node,Spec#testspec.nodes),
			  ref2dir(Dir,Spec#testspec.alias),
			  Suite,Cs,Tests, Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- skip_suites ---
add_tests([{skip_suites,all_nodes,Dir,Ss,Cmt}|Ts],Spec) ->
    add_tests([{skip_suites,list_nodes(Spec),Dir,Ss,Cmt}|Ts],Spec);
add_tests([{skip_suites,Dir,Ss,Cmt}|Ts],Spec) ->
    add_tests([{skip_suites,all_nodes,Dir,Ss,Cmt}|Ts],Spec);
add_tests([{skip_suites,Nodes,Dir,Ss,Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,skip_suites,[Dir,Ss,Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_suites,Node,Dir,Ss,Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_suites(ref2node(Node,Spec#testspec.nodes),
			 ref2dir(Dir,Spec#testspec.alias),
			 Ss,Cmt,Tests,
			 Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- skip_groups ---
add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,list_nodes(Spec),Dir,Suite,Gs,Cmt}|Ts],Spec);
add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,list_nodes(Spec),Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec);
add_tests([{skip_groups,Dir,Suite,Gs,Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,Cmt}|Ts],Spec);
add_tests([{skip_groups,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec);
add_tests([{skip_groups,Nodes,Dir,Suite,Gs,Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,skip_groups,[Dir,Suite,Gs,Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_groups,Nodes,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,skip_groups,[Dir,Suite,Gs,{cases,TCs},Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_groups,Node,Dir,Suite,Gs,Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_groups(ref2node(Node,Spec#testspec.nodes),
			 ref2dir(Dir,Spec#testspec.alias),
			 Suite,Gs,all,Cmt,Tests,
			 Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});
add_tests([{skip_groups,Node,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_groups(ref2node(Node,Spec#testspec.nodes),
			 ref2dir(Dir,Spec#testspec.alias),
			 Suite,Gs,TCs,Cmt,Tests,
			 Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- skip_cases ---
add_tests([{skip_cases,all_nodes,Dir,Suite,Cs,Cmt}|Ts],Spec) ->
    add_tests([{skip_cases,list_nodes(Spec),Dir,Suite,Cs,Cmt}|Ts],Spec);
add_tests([{skip_cases,Dir,Suite,Cs,Cmt}|Ts],Spec) ->
    add_tests([{skip_cases,all_nodes,Dir,Suite,Cs,Cmt}|Ts],Spec);
add_tests([{skip_cases,Nodes,Dir,Suite,Cs,Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = separate(Nodes,skip_cases,[Dir,Suite,Cs,Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_cases,Node,Dir,Suite,Cs,Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_cases(ref2node(Node,Spec#testspec.nodes),
			ref2dir(Dir,Spec#testspec.alias),
			Suite,Cs,Cmt,Tests,Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- handled/errors ---
add_tests([{alias,_,_}|Ts],Spec) ->		% handled
    add_tests(Ts,Spec);

add_tests([{node,_,_}|Ts],Spec) ->		% handled
    add_tests(Ts,Spec);

add_tests([{merge_tests, _} | Ts], Spec) -> % handled
    add_tests(Ts,Spec);

%% check if it's a CT term that has bad format or if the user seems to
%% have added something of his/her own, which we'll let pass if relaxed
%% mode is enabled.
add_tests([Other|Ts],Spec) when is_tuple(Other) ->
    [Name|_] = tuple_to_list(Other),
    case lists:keymember(Name,1,valid_terms()) of
	true ->					% halt
	    throw({error,{bad_term_in_spec,Other}});
	false ->				% ignore
	    case get(relaxed) of
		true ->
		    %% warn if name resembles a CT term
		    case resembles_ct_term(Name,size(Other)) of
			true ->
			    io:format("~nSuspicious term, please check:~n"
				      "~p~n", [Other]);
			false ->
			    ok
		    end,
		    add_tests(Ts,Spec);
		false ->
		    throw({error,{undefined_term_in_spec,Other}})
	    end
    end;

add_tests([Other|Ts],Spec) ->	
    case get(relaxed) of
	true ->		
	    add_tests(Ts,Spec);
	false ->
	    throw({error,{undefined_term_in_spec,Other}})
    end;

add_tests([],Spec) ->				% done
    Spec.

separate(Nodes,Tag,Data,Tests,Refs) ->
    Separated = separate(Nodes,Tag,Data,Refs),
    Separated ++ Tests.
separate([N|Ns],Tag,Data,Refs) ->
    [list_to_tuple([Tag,ref2node(N,Refs)|Data])|separate(Ns,Tag,Data,Refs)];
separate([],_,_,_) ->
    [].
    

%% Representation:
%% {{Node,Dir},[{Suite1,[GrOrCase11,GrOrCase12,...]},
%%              {Suite2,[GrOrCase21,GrOrCase22,...]},...]}
%% {{Node,Dir},[{Suite1,{skip,Cmt}},
%%              {Suite2,[{GrOrCase21,{skip,Cmt}},GrOrCase22,...]},...]}
%% GrOrCase = {GroupName,[Case1,Case2,...]} | Case

insert_suites(Node,Dir,[S|Ss],Tests, MergeTests) ->
    Tests1 = insert_cases(Node,Dir,S,all,Tests,MergeTests),
    insert_suites(Node,Dir,Ss,Tests1,MergeTests);
insert_suites(_Node,_Dir,[],Tests,_MergeTests) ->
    Tests;
insert_suites(Node,Dir,S,Tests,MergeTests) ->
    insert_suites(Node,Dir,[S],Tests,MergeTests).

insert_groups(Node,Dir,Suite,Group,Cases,Tests,MergeTests) 
  when is_atom(Group) ->
    insert_groups(Node,Dir,Suite,[Group],Cases,Tests,MergeTests);
insert_groups(Node,Dir,Suite,Groups,Cases,Tests,false) when
      ((Cases == all) or is_list(Cases)) and is_list(Groups) ->
    Groups1 = [{Gr,Cases} || Gr <- Groups],
    append({{Node,Dir},[{Suite,Groups1}]},Tests);
insert_groups(Node,Dir,Suite,Groups,Cases,Tests,true) when
      ((Cases == all) or is_list(Cases)) and is_list(Groups) ->
    case lists:keysearch({Node,Dir},1,Tests) of
	{value,{{Node,Dir},[{all,_}]}} ->
	    Tests;
	{value,{{Node,Dir},Suites0}} ->
	    Suites1 = insert_groups1(Suite,
				     [{Gr,Cases} || Gr <- Groups],
				     Suites0),
	    insert_in_order({{Node,Dir},Suites1},Tests);
	false ->
	    Groups1 = [{Gr,Cases} || Gr <- Groups],
	    insert_in_order({{Node,Dir},[{Suite,Groups1}]},Tests)
    end;
insert_groups(Node,Dir,Suite,Groups,Case,Tests, MergeTests) 
  when is_atom(Case) ->
    Cases = if Case == all -> all; true -> [Case] end,
    insert_groups(Node,Dir,Suite,Groups,Cases,Tests, MergeTests).

insert_groups1(_Suite,_Groups,all) ->
    all;
insert_groups1(Suite,Groups,Suites0) ->
    case lists:keysearch(Suite,1,Suites0) of
	{value,{Suite,all}} ->
	    Suites0;
	{value,{Suite,GrAndCases0}} ->
	    GrAndCases = insert_groups2(Groups,GrAndCases0),
	    insert_in_order({Suite,GrAndCases},Suites0);
	false ->
	    insert_in_order({Suite,Groups},Suites0)
    end.

insert_groups2(_Groups,all) ->
    all;
insert_groups2([Group={GrName,Cases}|Groups],GrAndCases) ->
    case lists:keysearch(GrName,1,GrAndCases) of
	{value,{GrName,all}} ->
	    GrAndCases;
	{value,{GrName,Cases0}} ->
	    Cases1 = insert_in_order(Cases,Cases0),
	    insert_groups2(Groups,insert_in_order({GrName,Cases1},GrAndCases));
	false ->
	    insert_groups2(Groups,insert_in_order(Group,GrAndCases))
    end;
insert_groups2([],GrAndCases) ->
    GrAndCases.

insert_cases(Node,Dir,Suite,Cases,Tests,false) when is_list(Cases) ->
    append({{Node,Dir},[{Suite,Cases}]},Tests);
insert_cases(Node,Dir,Suite,Cases,Tests,true) when is_list(Cases) ->
    case lists:keysearch({Node,Dir},1,Tests) of
	{value,{{Node,Dir},[{all,_}]}} ->
	    Tests;
	{value,{{Node,Dir},Suites0}} ->
	    Suites1 = insert_cases1(Suite,Cases,Suites0),
	    insert_in_order({{Node,Dir},Suites1},Tests);
	false ->
	    insert_in_order({{Node,Dir},[{Suite,Cases}]},Tests)
    end;
insert_cases(Node,Dir,Suite,Case,Tests,MergeTests) when is_atom(Case) ->
    insert_cases(Node,Dir,Suite,[Case],Tests,MergeTests).

insert_cases1(_Suite,_Cases,all) ->
    all;
insert_cases1(Suite,Cases,Suites0) ->
    case lists:keysearch(Suite,1,Suites0) of
	{value,{Suite,all}} ->
	    Suites0;
	{value,{Suite,Cases0}} ->
	    Cases1 = insert_in_order(Cases,Cases0),
	    insert_in_order({Suite,Cases1},Suites0);
	false ->
	    insert_in_order({Suite,Cases},Suites0)
    end.

skip_suites(Node,Dir,[S|Ss],Cmt,Tests,MergeTests) ->
    Tests1 = skip_cases(Node,Dir,S,all,Cmt,Tests,MergeTests),
    skip_suites(Node,Dir,Ss,Cmt,Tests1,MergeTests);
skip_suites(_Node,_Dir,[],_Cmt,Tests,_MergeTests) ->
    Tests;
skip_suites(Node,Dir,S,Cmt,Tests,MergeTests) ->
    skip_suites(Node,Dir,[S],Cmt,Tests,MergeTests).

skip_groups(Node,Dir,Suite,Group,all,Cmt,Tests,MergeTests) 
  when is_atom(Group) ->
    skip_groups(Node,Dir,Suite,[Group],all,Cmt,Tests,MergeTests);
skip_groups(Node,Dir,Suite,Group,Cases,Cmt,Tests,MergeTests) 
  when is_atom(Group) ->
    skip_groups(Node,Dir,Suite,[Group],Cases,Cmt,Tests,MergeTests);
skip_groups(Node,Dir,Suite,Groups,Case,Cmt,Tests,MergeTests) 
  when is_atom(Case),Case =/= all ->
    skip_groups(Node,Dir,Suite,Groups,[Case],Cmt,Tests,MergeTests);
skip_groups(Node,Dir,Suite,Groups,Cases,Cmt,Tests,false) when
      ((Cases == all) or is_list(Cases)) and is_list(Groups) ->
    Suites1 = skip_groups1(Suite,[{Gr,Cases} || Gr <- Groups],Cmt,[]),
    append({{Node,Dir},Suites1},Tests);
skip_groups(Node,Dir,Suite,Groups,Cases,Cmt,Tests,true) when
      ((Cases == all) or is_list(Cases)) and is_list(Groups) ->
    Suites =
	case lists:keysearch({Node,Dir},1,Tests) of
	    {value,{{Node,Dir},Suites0}} ->
		Suites0;
	    false ->
		[]
	end,
    Suites1 = skip_groups1(Suite,[{Gr,Cases} || Gr <- Groups],Cmt,Suites),
    insert_in_order({{Node,Dir},Suites1},Tests);
skip_groups(Node,Dir,Suite,Groups,Case,Cmt,Tests,MergeTests) 
  when is_atom(Case) ->
    Cases = if Case == all -> all; true -> [Case] end,
    skip_groups(Node,Dir,Suite,Groups,Cases,Cmt,Tests,MergeTests).

skip_groups1(Suite,Groups,Cmt,Suites0) ->
    SkipGroups = lists:map(fun(Group) ->
				   {Group,{skip,Cmt}}
			   end,Groups),
    case lists:keysearch(Suite,1,Suites0) of
	{value,{Suite,GrAndCases0}} ->
	    GrAndCases1 = GrAndCases0 ++ SkipGroups,
	    insert_in_order({Suite,GrAndCases1},Suites0);
	false ->
	    insert_in_order({Suite,SkipGroups},Suites0)
    end.

skip_cases(Node,Dir,Suite,Cases,Cmt,Tests,false) when is_list(Cases) ->
    Suites1 = skip_cases1(Suite,Cases,Cmt,[]),
    append({{Node,Dir},Suites1},Tests);
skip_cases(Node,Dir,Suite,Cases,Cmt,Tests,true) when is_list(Cases) ->
    Suites =
	case lists:keysearch({Node,Dir},1,Tests) of
	    {value,{{Node,Dir},Suites0}} ->
		Suites0;
	    false ->
		[]
	end,
    Suites1 = skip_cases1(Suite,Cases,Cmt,Suites),
    insert_in_order({{Node,Dir},Suites1},Tests);
skip_cases(Node,Dir,Suite,Case,Cmt,Tests,MergeTests) when is_atom(Case) ->
    skip_cases(Node,Dir,Suite,[Case],Cmt,Tests,MergeTests).

skip_cases1(Suite,Cases,Cmt,Suites0) ->
    SkipCases = lists:map(fun(C) ->
				  {C,{skip,Cmt}}
			  end,Cases),
    case lists:keysearch(Suite,1,Suites0) of
	{value,{Suite,Cases0}} ->
	    Cases1 = Cases0 ++ SkipCases,
	    insert_in_order({Suite,Cases1},Suites0);
	false ->
	    insert_in_order({Suite,SkipCases},Suites0)
    end.

append(Elem, List) ->
    List ++ [Elem].

insert_in_order([E|Es],List) ->
    List1 = insert_elem(E,List,[]),
    insert_in_order(Es,List1);
insert_in_order([],List) ->
    List;
insert_in_order(E,List) ->
    insert_elem(E,List,[]).

%% replace an existing entry (same key) or add last in list
insert_elem({Key,_}=E,[{Key,_}|Rest],SoFar) ->
    lists:reverse([E|SoFar]) ++ Rest;
insert_elem({E,_},[E|Rest],SoFar) ->
    lists:reverse([E|SoFar]) ++ Rest;
insert_elem(E,[E|Rest],SoFar) ->
    lists:reverse([E|SoFar]) ++ Rest;
insert_elem(E,[E1|Rest],SoFar) ->
    insert_elem(E,Rest,[E1|SoFar]);
insert_elem(E,[],SoFar) ->
    lists:reverse([E|SoFar]).

ref2node(all_nodes,_Refs) ->
    all_nodes;
ref2node(master,_Refs) ->
    master;
ref2node(RefOrNode,Refs) ->
    case string:chr(atom_to_list(RefOrNode),$@) of
	0 ->					% a ref	    
	    case lists:keysearch(RefOrNode,1,Refs) of
		{value,{RefOrNode,Node}} ->
		    Node;
		false ->
		    throw({error,{noderef_missing,RefOrNode}})
	    end;
	_ ->					% a node
	    RefOrNode
    end.

ref2dir(Ref,Refs) when is_atom(Ref) ->
    case lists:keysearch(Ref,1,Refs) of
	{value,{Ref,Dir}} ->
	    Dir;
	false ->
	    throw({error,{alias_missing,Ref}})
    end;
ref2dir(Dir,_) when is_list(Dir) ->
    Dir.

is_noderef(What,Nodes) when is_atom(What) ->
    is_noderef([What],Nodes);
is_noderef([master|_],_Nodes) ->
    true;
is_noderef([What|_],Nodes) ->
    case lists:keymember(What,1,Nodes) or
	 lists:keymember(What,2,Nodes) of
	true ->					
	    true;
	false ->
	    false
    end;
is_noderef([],_) ->
    false.

valid_terms() ->
    [
     {node,3},
     {cover,2},
     {cover,3},
     {config,2},
     {config,3},
     {userconfig,2},
     {userconfig,3},
     {alias,3},
     {merge_tests,1},
     {logdir,2},
     {logdir,3},
     {logopts,2},
     {logopts,3},
     {label,2},
     {label,3},
     {event_handler,2},
     {event_handler,3},
     {event_handler,4},
     {ct_hooks,2},
     {ct_hooks,3},
     {enable_builtin_hooks,1},
     {multiply_timetraps,2},
     {multiply_timetraps,3},
     {scale_timetraps,2},
     {scale_timetraps,3},
     {include,2},
     {include,3},
     {suites,3},
     {suites,4},
     {groups,4},
     {groups,5},
     {groups,6},
     {cases,4},
     {cases,5},
     {skip_suites,4},
     {skip_suites,5},
     {skip_groups,5},
     {skip_groups,6},
     {skip_groups,7},
     {skip_cases,5},
     {skip_cases,6}
    ].

%% this function "guesses" if the user has misspelled a term name
resembles_ct_term(Name,Size) when is_atom(Name) ->
    resembles_ct_term2(atom_to_list(Name),Size);
resembles_ct_term(_Name,_) ->
    false.
	
resembles_ct_term2(Name,Size) when length(Name) > 3 ->
    CTTerms = [{atom_to_list(Tag),Sz} || {Tag,Sz} <- valid_terms()],
    compare_names(Name,Size,CTTerms);
resembles_ct_term2(_,_) ->
    false.

compare_names(Name,Size,[{Term,Sz}|Ts]) ->
    if abs(Size-Sz) > 0 ->
	    compare_names(Name,Size,Ts);
       true ->
	    Diff = abs(length(Name)-length(Term)),
	    if Diff > 1 ->
		    compare_names(Name,Size,Ts);
	       true ->
		    Common = common_letters(Name,Term,0),
		    Bad = abs(length(Name)-Common),
		    if Bad > 2 ->
			    compare_names(Name,Size,Ts);
		       true ->
			    true
		    end
	    end
    end;
compare_names(_,_,[]) ->
    false.

common_letters(_,[],Count) -> 
    Count;
common_letters([L|Ls],Term,Count) ->
    case lists:member(L,Term) of
	true ->
	    Term1 = lists:delete(L,Term),
	    common_letters(Ls,Term1,Count+1);
	false ->
	    common_letters(Ls,Term,Count)
    end;
common_letters([],_,Count) -> 
    Count.
