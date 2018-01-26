%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2017. All Rights Reserved.
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

%%% @doc Common Test Framework functions handling test specifications.
%%%
%%% <p>This module exports functions that are used within CT to
%%% scan and parse test specifikations.</p>
-module(ct_testspec).

-export([prepare_tests/1, prepare_tests/2, 
	 collect_tests_from_list/2, collect_tests_from_list/3,
	 collect_tests_from_file/2, collect_tests_from_file/3,
         get_tests/1]).

-export([testspec_rec2list/1, testspec_rec2list/2]).

-include("ct_util.hrl").
-define(testspec_fields, record_info(fields, testspec)).

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
prepare_tests(TestSpec,Node) when is_record(TestSpec,testspec),
				  is_atom(Node) ->
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
run_per_node([{{Node,Dir},Test}|Ts],Result,MergeTests) ->
    {value,{Node,{Run,Skip}}} = lists:keysearch(Node,1,Result),
    Run1 = case MergeTests of
	       false ->
		   append({Dir, Test}, Run);
	       true ->
		   merge_tests(Dir,Test,Run)
	   end,
    run_per_node(Ts,insert_in_order({Node,{Run1,Skip}},Result,replace), 
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
    skip_per_node(Ts,insert_in_order({Node,{Run,Skip1}},Result,replace));
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
	    {Run1,Skip1} = prepare_cases(Node,Dir,Suite,Cases,Run,Skip),
	    prepare_suites(Node,Dir,Suites,Run1,Skip1)
    end;
prepare_suites(_Node,_Dir,[],Run,Skip) ->
    {lists:flatten(lists:reverse(Run)),
     lists:flatten(lists:reverse(Skip))}.

prepare_cases(Node,Dir,Suite,Cases,Run,Skip) ->
    case get_skipped_cases(Node,Dir,Suite,Cases) of
	[SkipAll={{Node,Dir},{Suite,_Cmt}}] ->	      % all cases to be skipped
	    case lists:any(fun({{N,D},{S,all}}) when N == Node,
						     D == Dir,
						     S == Suite ->
				   true;
			      ({{N,D},{S,Cs}}) when N == Node,
						    D == Dir,
						    S == Suite ->
				   lists:member(all,Cs);
			      (_) -> false
			   end, lists:flatten(Run)) of
		true ->
		    {Run,[SkipAll|Skip]};
		false ->
		    %% note: this adds an 'all' test even if
		    %% only skip is specified		    
		    {[{{Node,Dir},{Suite,all}}|Run],[SkipAll|Skip]}
	    end;
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
						[{skipped,C}|Acc]
					end;
				   (C,Acc) -> [C|Acc]
				end, [], Cases),
	    {[{{Node,Dir},{Suite,PrepC}}|Run],[Skipped|Skip]}
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
collect_tests_from_file(Specs,Relaxed) ->
    collect_tests_from_file(Specs,[node()],Relaxed).

collect_tests_from_file(Specs,Nodes,Relaxed) when is_list(Nodes) ->
    NodeRefs = lists:map(fun(N) -> {undefined,N} end, Nodes),
    %% [Spec1,Spec2,...] means create one testpec record per Spec file
    %% [[Spec1,Spec2,...]] means merge all specs into one testspec record
    {Join,Specs1} = if is_list(hd(hd(Specs))) -> {true,hd(Specs)};
		    true -> {false,Specs}
		 end,
    Specs2 = [filename:absname(S) || S <- Specs1],
    TS0 = #testspec{nodes=NodeRefs},

    try create_testspecs(Specs2,TS0,Relaxed,Join) of
	{{[],_},SeparateTestSpecs} ->
	    filter_and_convert(SeparateTestSpecs);
	{{_,#testspec{tests=[]}},SeparateTestSpecs} ->
	    filter_and_convert(SeparateTestSpecs);
	{Joined,SeparateTestSpecs} ->
	    [filter_and_convert(Joined) |
	     filter_and_convert(SeparateTestSpecs)]
    catch
	_:Error={error,_} ->
	    Error;
	_:Error ->
	    {error,Error}
    end.

filter_and_convert(Joined) when is_tuple(Joined) ->
    hd(filter_and_convert([Joined]));
filter_and_convert([{_,#testspec{tests=[]}}|TSs]) ->
    filter_and_convert(TSs);
filter_and_convert([{[{SpecFile,MergeTests}|SMs],TestSpec}|TSs]) ->
    #testspec{config = CfgFiles} = TestSpec,
    TestSpec1 = TestSpec#testspec{config = delete_dups(CfgFiles),
				  merge_tests = MergeTests},
    %% set the merge_tests value for the testspec to the value
    %% of the first test spec in the set
    [{[SpecFile | [SF || {SF,_} <- SMs]], TestSpec1} | filter_and_convert(TSs)];
filter_and_convert([]) ->
    [].

delete_dups(Elems) ->
    delete_dups1(lists:reverse(Elems),[]).

delete_dups1([E|Es],Keep) ->
    case lists:member(E,Es) of
	true ->
	    delete_dups1(Es,Keep);
	false ->
	    delete_dups1(Es,[E|Keep])
    end;
delete_dups1([],Keep) ->
    Keep.

create_testspecs(Specs,TestSpec,Relaxed,Join) ->
    %% SpecsTree = {SpecAbsName, TermsInSpec,
    %%              IncludedJoinTree, IncludedSeparateTree,
    %%              JoinSpecWithRest, RestSpecsTree}
    SpecsTree = create_spec_tree(Specs,TestSpec,Join,[]),
    create_specs(SpecsTree,TestSpec,TestSpec,Relaxed).

create_spec_tree([Spec|Specs],TS,JoinWithNext,Known) ->
    SpecDir = filename:dirname(filename:absname(Spec)),
    TS1 = TS#testspec{spec_dir=SpecDir},
    SpecAbsName = get_absfile(Spec,TS1),
    case lists:member(SpecAbsName,Known) of
	true ->
	    throw({error,{cyclic_reference,SpecAbsName}});
	false ->
	    case file:consult(SpecAbsName) of
		{ok,Terms} ->
		    Terms1 = replace_names(Terms),
		    {InclJoin,InclSep} = get_included_specs(Terms1,TS1),
		    {SpecAbsName,Terms1,
		     create_spec_tree(InclJoin,TS,true,[SpecAbsName|Known]),
		     create_spec_tree(InclSep,TS,false,[SpecAbsName|Known]),
		     JoinWithNext,
		     create_spec_tree(Specs,TS,JoinWithNext,Known)};	
		{error,Reason} ->
		    ReasonStr =
			lists:flatten(io_lib:format("~ts",
						    [file:format_error(Reason)])),
		    throw({error,{SpecAbsName,ReasonStr}})
	    end
    end;
create_spec_tree([],_TS,_JoinWithNext,_Known) ->
    [].

create_specs({Spec,Terms,InclJoin,InclSep,JoinWithNext,NextSpec},
	     TestSpec,TestSpec0,Relaxed) ->
    SpecDir = filename:dirname(filename:absname(Spec)),
    TestSpec1 = create_spec(Terms,TestSpec#testspec{spec_dir=SpecDir},
			    JoinWithNext,Relaxed),

    {{JoinSpecs1,JoinTS1},Separate1} = create_specs(InclJoin,TestSpec1,
						    TestSpec0,Relaxed),

    {{JoinSpecs2,JoinTS2},Separate2} =
	case JoinWithNext of
	    true ->
		create_specs(NextSpec,JoinTS1,
			     TestSpec0,Relaxed);
	    false ->
		{{[],JoinTS1},[]}
	end,
    {SepJoinSpecs,Separate3} = create_specs(InclSep,TestSpec0,
					    TestSpec0,Relaxed),
    {SepJoinSpecs1,Separate4} =
	case JoinWithNext of
	    true ->
		{{[],TestSpec},[]};
	    false ->
		create_specs(NextSpec,TestSpec0,
			     TestSpec0,Relaxed)
	end,            

    SpecInfo = {Spec,TestSpec1#testspec.merge_tests},
    AllSeparate =
	[TSData || TSData = {Ss,_TS} <- Separate3++Separate1++
		                        [SepJoinSpecs]++Separate2++
		                        [SepJoinSpecs1]++Separate4,
		   Ss /= []],
    case {JoinWithNext,JoinSpecs1} of
	{true,_} ->
	    {{[SpecInfo|(JoinSpecs1++JoinSpecs2)],JoinTS2},
	     AllSeparate};
	{false,[]} ->
	    {{[],TestSpec},
	     [{[SpecInfo],TestSpec1}|AllSeparate]};
	{false,_} ->
	    {{[SpecInfo|(JoinSpecs1++JoinSpecs2)],JoinTS2},
	     AllSeparate}
    end;
create_specs([],TestSpec,_,_Relaxed) ->
    {{[],TestSpec},[]}.
		
create_spec(Terms,TestSpec,JoinedByPrev,Relaxed) ->
    %% it's the "includer" that decides the value of merge_tests
    Terms1 = if not JoinedByPrev ->
		     [{set_merge_tests,true}|Terms];
		true ->
		     [{set_merge_tests,false}|Terms]
	     end,
    TS = #testspec{tests=Tests, logdir=LogDirs} =
	collect_tests({false,Terms1},TestSpec,Relaxed),
    LogDirs1 = lists:delete(".",LogDirs) ++ ["."],
    TS#testspec{tests=lists:flatten(Tests),
		logdir=LogDirs1}.

collect_tests_from_list(Terms,Relaxed) ->
    collect_tests_from_list(Terms,[node()],Relaxed).

collect_tests_from_list(Terms,Nodes,Relaxed) when is_list(Nodes) ->
    {ok,Cwd} = file:get_cwd(),
    NodeRefs = lists:map(fun(N) -> {undefined,N} end, Nodes),
    case catch collect_tests({true,Terms},#testspec{nodes=NodeRefs,
						    spec_dir=Cwd},
			     Relaxed) of
	E = {error,_} ->
	    E;
	TS ->
	    #testspec{tests=Tests, logdir=LogDirs} = TS,
	    LogDirs1 = lists:delete(".",LogDirs) ++ ["."],
	    TS#testspec{tests=lists:flatten(Tests), logdir=LogDirs1}
    end.
    
collect_tests({Replace,Terms},TestSpec=#testspec{alias=As,nodes=Ns},Relaxed) ->
    put(relaxed,Relaxed),
    Terms1 = if Replace -> replace_names(Terms);
		true    -> Terms
	     end,
    {MergeTestsDef,Terms2} =
	case proplists:get_value(set_merge_tests,Terms1,true) of
	    false -> 
		%% disable merge_tests
		{TestSpec#testspec.merge_tests,
		 proplists:delete(merge_tests,Terms1)};
	    true ->
		{true,Terms1}
	end,
    %% reverse nodes and aliases initially to get the order of them right
    %% in case this spec is being joined with a previous one
    TestSpec1 = get_global(Terms2,TestSpec#testspec{alias = lists:reverse(As),
						    nodes = lists:reverse(Ns),
						    merge_tests = MergeTestsDef}),
    TestSpec2 = get_all_nodes(Terms2,TestSpec1),
    {Terms3, TestSpec3} = filter_init_terms(Terms2, [], TestSpec2),

    add_tests(Terms3,TestSpec3).

%% replace names (atoms) in the testspec matching those in 'define' terms by
%% searching recursively through tuples and lists
replace_names(Terms) ->
    Defs =
	lists:flatmap(fun(Def={define,Name,_Replacement}) ->
			      %% check that name follows convention
			      if not is_atom(Name) ->
				      throw({illegal_name_in_testspec,Name});
				 true ->
				      [First|_] = atom_to_list(Name),
				      if ((First == $?) or (First == $$)
					  or (First == $_)
					  or ((First >= $A)
					      and (First =< $Z))) ->
					      [Def];
					 true ->
					      throw({illegal_name_in_testspec,
						     Name})
				      end
			      end;
			 (_) -> []
		      end, Terms),
    DefProps = replace_names_in_defs(Defs,[]),
    replace_names(Terms,[],DefProps).

replace_names_in_defs([Def|Left],ModDefs) ->
    [{define,Name,Replacement}] = replace_names([Def],[],ModDefs),
    replace_names_in_defs(Left,[{Name,Replacement}|ModDefs]);
replace_names_in_defs([],ModDefs) ->
    ModDefs.

replace_names([Term|Ts],Modified,Defs) when is_tuple(Term) ->
    [TypeTag|Data] = tuple_to_list(Term),
    Term1 = list_to_tuple([TypeTag|replace_names_in_elems(Data,[],Defs)]),
    replace_names(Ts,[Term1|Modified],Defs);
replace_names([Term|Ts],Modified,Defs) when is_atom(Term) ->
    case proplists:get_value(Term,Defs) of
	undefined ->
	    replace_names(Ts,[Term|Modified],Defs);
	Replacement ->
	    replace_names(Ts,[Replacement|Modified],Defs)
    end;
replace_names([Term=[Ch|_]|Ts],Modified,Defs) when is_integer(Ch) ->
    %% Term *could* be a string, attempt to search through it
    Term1 = replace_names_in_string(Term,Defs),
    replace_names(Ts,[Term1|Modified],Defs);
replace_names([Term|Ts],Modified,Defs) ->
    replace_names(Ts,[Term|Modified],Defs);
replace_names([],Modified,_Defs) ->
    lists:reverse(Modified).

replace_names_in_elems([Elem|Es],Modified,Defs) when is_tuple(Elem) ->
    Elem1 = list_to_tuple(replace_names_in_elems(tuple_to_list(Elem),[],Defs)),
    replace_names_in_elems(Es,[Elem1|Modified],Defs);
replace_names_in_elems([Elem|Es],Modified,Defs) when is_atom(Elem) ->
    case proplists:get_value(Elem,Defs) of
	undefined ->
	    %% if Term is a node name, check it for replacements as well
	    Elem1 = replace_names_in_node(Elem,Defs),
	    replace_names_in_elems(Es,[Elem1|Modified],Defs);
	Replacement ->
	    replace_names_in_elems(Es,[Replacement|Modified],Defs)
    end;
replace_names_in_elems([Elem=[Ch|_]|Es],Modified,Defs) when is_integer(Ch) ->
    %% Term *could* be a string, attempt to search through it
    case replace_names_in_string(Elem,Defs) of
	Elem ->
	    List = replace_names_in_elems(Elem,[],Defs),
	    replace_names_in_elems(Es,[List|Modified],Defs);
	Elem1 ->
	    replace_names_in_elems(Es,[Elem1|Modified],Defs)
    end;
replace_names_in_elems([Elem|Es],Modified,Defs) when is_list(Elem) ->
    List = replace_names_in_elems(Elem,[],Defs),
    replace_names_in_elems(Es,[List|Modified],Defs);
replace_names_in_elems([Elem|Es],Modified,Defs) ->
    replace_names_in_elems(Es,[Elem|Modified],Defs);
replace_names_in_elems([],Modified,_Defs) ->
    lists:reverse(Modified).

replace_names_in_string(Term,Defs=[{Name,Replacement=[Ch|_]}|Ds])
  when is_integer(Ch) ->
    try re:replace(Term,[$'|atom_to_list(Name)]++"'",
		   Replacement,[{return,list},unicode]) of
	Term ->					% no match, proceed
	    replace_names_in_string(Term,Ds);
	Term1 ->
	    replace_names_in_string(Term1,Defs)
    catch
	_:_ -> Term			% Term is not a string
    end;
replace_names_in_string(Term,[_|Ds]) ->
    replace_names_in_string(Term,Ds);
replace_names_in_string(Term,[]) ->
    Term.

replace_names_in_node(Node,Defs) ->
    String = atom_to_list(Node),
    case lists:member($@,String) of
	true ->
	    list_to_atom(replace_names_in_node1(String,Defs));
	false ->
	    Node
    end.

replace_names_in_node1(NodeStr,Defs=[{Name,Replacement}|Ds]) ->
    ReplStr = case Replacement of
		  [Ch|_] when is_integer(Ch) -> Replacement;
		  _ when is_atom(Replacement) -> atom_to_list(Replacement);
		  _ -> false
	      end,
    if ReplStr == false ->
	    replace_names_in_node1(NodeStr,Ds);
       true ->
	    case re:replace(NodeStr,atom_to_list(Name),
			    ReplStr,[{return,list},unicode]) of
		NodeStr ->			% no match, proceed
		    replace_names_in_node1(NodeStr,Ds);
		NodeStr1 ->
		    replace_names_in_node1(NodeStr1,Defs)
	    end
    end;
replace_names_in_node1(NodeStr,[]) ->
    NodeStr.

%% look for other specification files, either to join with the
%% current spec, or execute as separate test runs
get_included_specs(Terms,TestSpec) ->
    get_included_specs(Terms,TestSpec,[],[]).

get_included_specs([{specs,How,SpecOrSpecs}|Ts],TestSpec,Join,Sep) ->
    Specs = case SpecOrSpecs of
		[File|_] when is_list(File) ->
		    [get_absfile(Spec,TestSpec) || Spec <- SpecOrSpecs];
		[Ch|_] when is_integer(Ch) ->
		    [get_absfile(SpecOrSpecs,TestSpec)]
	    end,
    if How == join ->
	    get_included_specs(Ts,TestSpec,Join++Specs,Sep);
       true ->
	    get_included_specs(Ts,TestSpec,Join,Sep++Specs)
    end;
get_included_specs([_|Ts],TestSpec,Join,Sep) ->
    get_included_specs(Ts,TestSpec,Join,Sep);
get_included_specs([],_,Join,Sep) ->
    {Join,Sep}.

%% global terms that will be used for analysing all other terms in the spec
get_global([{merge_tests,Bool}|Ts],Spec) ->
    get_global(Ts,Spec#testspec{merge_tests=Bool});

%% the 'define' term replaces the 'alias' and 'node' terms, but we need to keep
%% the latter two for backwards compatibility...
get_global([{alias,Ref,Dir}|Ts],Spec=#testspec{alias=Refs}) ->
    get_global(Ts,Spec#testspec{alias=[{Ref,get_absdir(Dir,Spec)}|Refs]});
get_global([{node,Ref,Node}|Ts],Spec=#testspec{nodes=Refs}) ->
    get_global(Ts,Spec#testspec{nodes=[{Ref,Node} |
				       lists:keydelete(Node,2,Refs)]});

get_global([_|Ts],Spec) ->
    get_global(Ts,Spec);
get_global([],Spec=#testspec{nodes=Ns, alias=As}) ->
    Spec#testspec{nodes=lists:reverse(Ns), alias=lists:reverse(As)}.

get_absfile(Callback,FullName,#testspec{spec_dir=SpecDir}) ->
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

get_absname(Dir,SpecDir) ->
    AbsName = filename:absname(Dir,SpecDir),
    shorten_path(AbsName,SpecDir).

shorten_path(Path,SpecDir) ->
    case shorten_split_path(filename:split(Path),[]) of
	[] ->
	    [Root|_] = filename:split(SpecDir),
	    Root;
	Short ->	    
	    filename:join(Short)
    end.

shorten_split_path([".."|Path],SoFar) ->
    shorten_split_path(Path,tl(SoFar));
shorten_split_path(["."|Path],SoFar) ->
    shorten_split_path(Path,SoFar);
shorten_split_path([Dir|Path],SoFar) ->
    shorten_split_path(Path,[Dir|SoFar]);
shorten_split_path([],SoFar) ->
    lists:reverse(SoFar).

%% go through all tests and register all nodes found
get_all_nodes([{suites,Nodes,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{suites,Node,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{groups,[Char|_],_,_,_}|Ts],Spec) when is_integer(Char) ->
    get_all_nodes(Ts,Spec);
get_all_nodes([{groups,Nodes,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{groups,Nodes,_,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{groups,_,_,_,{cases,_}}|Ts],Spec) ->
    get_all_nodes(Ts,Spec);
get_all_nodes([{groups,Node,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{groups,Node,_,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{cases,Nodes,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{cases,Node,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{skip_suites,Nodes,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{skip_suites,Node,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{skip_groups,[Char|_],_,_,_,_}|Ts],Spec) when is_integer(Char) ->
    get_all_nodes(Ts,Spec);
get_all_nodes([{skip_groups,Nodes,_,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{skip_groups,Node,_,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{skip_groups,Nodes,_,_,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{skip_groups,Node,_,_,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([{skip_cases,Nodes,_,_,_,_}|Ts],Spec) when is_list(Nodes) ->
    get_all_nodes(Ts,save_nodes(Nodes,Spec));
get_all_nodes([{skip_cases,Node,_,_,_,_}|Ts],Spec) ->
    get_all_nodes(Ts,save_nodes([Node],Spec));
get_all_nodes([_Other|Ts],Spec) ->
    get_all_nodes(Ts,Spec);
get_all_nodes([],Spec) ->
    Spec.

filter_init_terms([{init,InitOptions}|Ts],NewTerms,Spec) ->
    filter_init_terms([{init,list_nodes(Spec),InitOptions}|Ts],
		      NewTerms,Spec);
filter_init_terms([{init,all_nodes,InitOptions}|Ts],NewTerms,Spec) ->
    filter_init_terms([{init,list_nodes(Spec),InitOptions}|Ts],
		      NewTerms,Spec);
filter_init_terms([{init,NodeRef,InitOptions}|Ts],
		  NewTerms,Spec) when is_atom(NodeRef) ->
    filter_init_terms([{init,[NodeRef],InitOptions}|Ts],NewTerms,Spec);
filter_init_terms([{init,NodeRefs,InitOption}|Ts],
		  NewTerms,Spec) when is_tuple(InitOption) ->
    filter_init_terms([{init,NodeRefs,[InitOption]}|Ts],NewTerms,Spec);
filter_init_terms([{init,[NodeRef|NodeRefs],InitOptions}|Ts],
		  NewTerms,Spec=#testspec{init=InitData}) ->
    NodeStartOptions =
	case lists:keyfind(node_start,1,InitOptions) of
	    {node_start,NSOptions}->
		case lists:keyfind(callback_module,1,NSOptions) of
		    {callback_module,_Callback}->
			NSOptions;
		    false->
			[{callback_module,ct_slave}|NSOptions]
		end;
	    false->
		[]
	end,
    EvalTerms = case lists:keyfind(eval,1,InitOptions) of
		    {eval,MFA} when is_tuple(MFA) ->
			[MFA];
		    {eval,MFAs} when is_list(MFAs) ->
			MFAs;
		    false->
			[]
		end,
    Node = ref2node(NodeRef,Spec#testspec.nodes),
    InitData2 = add_option({node_start,NodeStartOptions},Node,InitData,true),
    InitData3 = add_option({eval,EvalTerms},Node,InitData2,false),
    filter_init_terms([{init,NodeRefs,InitOptions}|Ts],
		      NewTerms,Spec#testspec{init=InitData3});
filter_init_terms([{init,[],_}|Ts],NewTerms,Spec) ->
    filter_init_terms(Ts,NewTerms,Spec);
filter_init_terms([Term|Ts],NewTerms,Spec) ->
    filter_init_terms(Ts,[Term|NewTerms],Spec);
filter_init_terms([],NewTerms,Spec) ->
    {lists:reverse(NewTerms),Spec}.

add_option({Key,Value},Node,List,WarnIfExists) when is_list(Value) ->
    OldOptions = case lists:keyfind(Node,1,List) of
	{Node,Options}->
	    Options;
	false->
	    []
    end,
    NewOption = case lists:keyfind(Key,1,OldOptions) of
	{Key,OldOption} when WarnIfExists,OldOption/=[]->
	    io:format("There is an option ~w=~w already "
		      "defined for node ~w, skipping new ~w~n",
		[Key,OldOption,Node,Value]),
	    OldOption;
	{Key,OldOption}->
	    OldOption ++ Value;
	false->
	    Value
    end,
    lists:keystore(Node,1,List,
	{Node,lists:keystore(Key,1,OldOptions,{Key,NewOption})});
add_option({Key,Value},Node,List,WarnIfExists) ->
    add_option({Key,[Value]},Node,List,WarnIfExists).

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


%%%-----------------------------------------------------------------
%%% Parse the given test specs and return the complete set of specs
%%% and tests to run/skip.
%%% [Spec1,Spec2,...] means create separate tests per spec
%%% [[Spec1,Spec2,...]] means merge all specs into one
-spec get_tests(Specs) -> {ok,[{Specs,Tests}]} | {error,Reason} when
      Specs :: [string()] | [[string()]],
      Tests :: {Node,Run,Skip},
      Node :: atom(),
      Run :: {Dir,Suites,Cases},
      Skip :: {Dir,Suites,Comment} | {Dir,Suites,Cases,Comment},
      Dir :: string(),
      Suites :: atom | [atom()] | all,
      Cases :: atom | [atom()] | all,
      Comment :: string(),
      Reason :: term().

get_tests(Specs) ->
    case collect_tests_from_file(Specs,true) of
        Tests when is_list(Tests) ->
            {ok,[{S,prepare_tests(R)} || {S,R} <- Tests]};
        Error ->
            Error
    end.

%%     -----------------------------------------------------
%%   /                                                       \
%%  |  When adding test/config terms, remember to update      |
%%  |  valid_terms/0 also!                                    |
%%   \                                                       /
%%     -----------------------------------------------------

%% --- suites ---
add_tests([{suites,all_nodes,Dir,Ss}|Ts],Spec) ->
    add_tests([{suites,list_nodes(Spec),Dir,Ss}|Ts],Spec);
add_tests([{suites,Dir,Ss}|Ts],Spec) ->
    add_tests([{suites,all_nodes,Dir,Ss}|Ts],Spec);
add_tests([{suites,Nodes,Dir,Ss}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,suites,[Dir,Ss],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{suites,Node,Dir,Ss}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_suites(ref2node(Node,Spec#testspec.nodes),
			   ref2dir(Dir,Spec),
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
    Ts1 = per_node(Nodes,groups,[Dir,Suite,Gs],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{groups,Nodes,Dir,Suite,Gs,{cases,TCs}}|Ts],
	  Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,groups,[Dir,Suite,Gs,{cases,TCs}],Ts,
		   Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{groups,Node,Dir,Suite,Gs}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_groups(ref2node(Node,Spec#testspec.nodes),
			   ref2dir(Dir,Spec),
			   Suite,Gs,all,Tests,
			   Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});
add_tests([{groups,Node,Dir,Suite,Gs,{cases,TCs}}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_groups(ref2node(Node,Spec#testspec.nodes),
			   ref2dir(Dir,Spec),
			   Suite,Gs,TCs,Tests,
			   Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- cases ---
add_tests([{cases,all_nodes,Dir,Suite,Cs}|Ts],Spec) ->
    add_tests([{cases,list_nodes(Spec),Dir,Suite,Cs}|Ts],Spec);
add_tests([{cases,Dir,Suite,Cs}|Ts],Spec) ->
    add_tests([{cases,all_nodes,Dir,Suite,Cs}|Ts],Spec);
add_tests([{cases,Nodes,Dir,Suite,Cs}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,cases,[Dir,Suite,Cs],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{cases,Node,Dir,Suite,Cs}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = insert_cases(ref2node(Node,Spec#testspec.nodes),
			  ref2dir(Dir,Spec),
			  Suite,Cs,Tests,
			  Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- skip_suites ---
add_tests([{skip_suites,all_nodes,Dir,Ss,Cmt}|Ts],Spec) ->
    add_tests([{skip_suites,list_nodes(Spec),Dir,Ss,Cmt}|Ts],Spec);
add_tests([{skip_suites,Dir,Ss,Cmt}|Ts],Spec) ->
    add_tests([{skip_suites,all_nodes,Dir,Ss,Cmt}|Ts],Spec);
add_tests([{skip_suites,Nodes,Dir,Ss,Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,skip_suites,[Dir,Ss,Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_suites,Node,Dir,Ss,Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_suites(ref2node(Node,Spec#testspec.nodes),
			 ref2dir(Dir,Spec),
			 Ss,Cmt,Tests,
			 Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- skip_groups ---
add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,list_nodes(Spec),Dir,Suite,Gs,Cmt}|Ts],Spec);
add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,list_nodes(Spec),Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],
	      Spec);
add_tests([{skip_groups,Dir,Suite,Gs,Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,Cmt}|Ts],Spec);
add_tests([{skip_groups,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) ->
    add_tests([{skip_groups,all_nodes,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec);
add_tests([{skip_groups,Nodes,Dir,Suite,Gs,Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,skip_groups,[Dir,Suite,Gs,Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_groups,Nodes,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],
	  Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,skip_groups,[Dir,Suite,Gs,{cases,TCs},Cmt],Ts,
		   Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_groups,Node,Dir,Suite,Gs,Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_groups(ref2node(Node,Spec#testspec.nodes),
			 ref2dir(Dir,Spec),
			 Suite,Gs,all,Cmt,Tests,
			 Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});
add_tests([{skip_groups,Node,Dir,Suite,Gs,{cases,TCs},Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_groups(ref2node(Node,Spec#testspec.nodes),
			 ref2dir(Dir,Spec),
			 Suite,Gs,TCs,Cmt,Tests,
			 Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- skip_cases ---
add_tests([{skip_cases,all_nodes,Dir,Suite,Cs,Cmt}|Ts],Spec) ->
    add_tests([{skip_cases,list_nodes(Spec),Dir,Suite,Cs,Cmt}|Ts],Spec);
add_tests([{skip_cases,Dir,Suite,Cs,Cmt}|Ts],Spec) ->
    add_tests([{skip_cases,all_nodes,Dir,Suite,Cs,Cmt}|Ts],Spec);
add_tests([{skip_cases,Nodes,Dir,Suite,Cs,Cmt}|Ts],Spec) when is_list(Nodes) ->
    Ts1 = per_node(Nodes,skip_cases,[Dir,Suite,Cs,Cmt],Ts,Spec#testspec.nodes),
    add_tests(Ts1,Spec);
add_tests([{skip_cases,Node,Dir,Suite,Cs,Cmt}|Ts],Spec) ->
    Tests = Spec#testspec.tests,
    Tests1 = skip_cases(ref2node(Node,Spec#testspec.nodes),
			ref2dir(Dir,Spec),
			Suite,Cs,Cmt,Tests,Spec#testspec.merge_tests),
    add_tests(Ts,Spec#testspec{tests=Tests1});

%% --- various configuration terms ---
add_tests([{config,Nodes,CfgDir,Files}|Ts],Spec) when is_list(Nodes);
						      Nodes == all_nodes ->
    add_tests([{config,Nodes,{CfgDir,Files}}|Ts],Spec);
add_tests([{config,Node,CfgDir,FileOrFiles}|Ts],Spec) ->
    add_tests([{config,Node,{CfgDir,FileOrFiles}}|Ts],Spec);
add_tests([{config,CfgDir=[Ch|_],Files}|Ts],Spec) when is_integer(Ch) ->
    add_tests([{config,all_nodes,{CfgDir,Files}}|Ts],Spec);

add_tests([{event_handler,Nodes,Hs,Args}|Ts],Spec) when is_list(Nodes);
							Nodes == all_nodes ->
    add_tests([{event_handler,Nodes,{Hs,Args}}|Ts],Spec);
add_tests([{event_handler,Node,HOrHs,Args}|Ts],Spec) ->
    add_tests([{event_handler,Node,{HOrHs,Args}}|Ts],Spec);

add_tests([{enable_builtin_hooks,Bool}|Ts],Spec) ->
    add_tests(Ts, Spec#testspec{enable_builtin_hooks = Bool});

add_tests([{release_shell,Bool}|Ts],Spec) ->
    add_tests(Ts, Spec#testspec{release_shell = Bool});

%% --- handled/errors ---
add_tests([{set_merge_tests,_}|Ts],Spec) ->	% internal
    add_tests(Ts,Spec);

add_tests([{define,_,_}|Ts],Spec) ->		% handled
    add_tests(Ts,Spec);

add_tests([{alias,_,_}|Ts],Spec) ->		% handled
    add_tests(Ts,Spec);

add_tests([{node,_,_}|Ts],Spec) ->		% handled
    add_tests(Ts,Spec);

add_tests([{merge_tests,_} | Ts], Spec) ->      % handled
    add_tests(Ts,Spec);

add_tests([{specs,_,_} | Ts], Spec) ->          % handled
    add_tests(Ts,Spec);

%%     --------------------------------------------------
%%   /                                                    \
%%  |  General add_tests/2 clauses below will work for     |
%%  |  most test spec configuration terms                  |
%%   \                                                    /
%%     --------------------------------------------------

%% create one test entry per known node and reinsert
add_tests([Term={Tag,all_nodes,Data}|Ts],Spec) ->
    case check_term(Term) of
	valid ->
	    Tests = [{Tag,Node,Data} ||	Node <- list_nodes(Spec),
					should_be_added(Tag,Node,Data,Spec)],
	    add_tests(Tests++Ts,Spec);
	invalid ->				% ignore term
	    Unknown = Spec#testspec.unknown,
	    add_tests(Ts,Spec#testspec{unknown=Unknown++[Term]})
    end;
%% create one test entry per node in Nodes and reinsert
add_tests([{Tag,[],Data}|Ts],Spec) ->
    add_tests([{Tag,all_nodes,Data}|Ts],Spec);
add_tests([{Tag,String=[Ch|_],Data}|Ts],Spec) when is_integer(Ch) ->
    add_tests([{Tag,all_nodes,{String,Data}}|Ts],Spec);
add_tests([{Tag,NodesOrOther,Data}|Ts],Spec) when is_list(NodesOrOther) ->
    case lists:all(fun(Test) -> is_node(Test,Spec#testspec.nodes)
		   end, NodesOrOther) of
	true ->
	    Ts1 = per_node(NodesOrOther,Tag,[Data],Ts,Spec#testspec.nodes),
	    add_tests(Ts1,Spec);
	false ->
	    add_tests([{Tag,all_nodes,{NodesOrOther,Data}}|Ts],Spec)
    end;
%% update data for testspec term of type Tag
add_tests([Term={Tag,NodeOrOther,Data}|Ts],Spec) ->
    case is_node(NodeOrOther,Spec#testspec.nodes) of
	true ->
	    case check_term(Term) of
		valid ->
		    Node = ref2node(NodeOrOther,Spec#testspec.nodes),
		    NodeIxData =
			update_recorded(Tag,Node,Spec) ++
			handle_data(Tag,Node,Data,Spec),
		    add_tests(Ts,mod_field(Spec,Tag,NodeIxData));
		invalid ->			% ignore term
		    Unknown = Spec#testspec.unknown,
		    add_tests(Ts,Spec#testspec{unknown=Unknown++[Term]})
	    end;
	false ->
	    add_tests([{Tag,all_nodes,{NodeOrOther,Data}}|Ts],Spec)
    end;
%% this test should be added for all known nodes
add_tests([Term={Tag,Data}|Ts],Spec) ->
    case check_term(Term) of
	valid ->
	    add_tests([{Tag,all_nodes,Data}|Ts],Spec);
	invalid ->
	    Unknown = Spec#testspec.unknown,
	    add_tests(Ts,Spec#testspec{unknown=Unknown++[Term]})
    end;
%% some other data than a tuple
add_tests([Other|Ts],Spec) ->	
    case get(relaxed) of
	true ->
	    Unknown = Spec#testspec.unknown,
	    add_tests(Ts,Spec#testspec{unknown=Unknown++[Other]});
	false ->
	    throw({error,{undefined_term_in_spec,Other}})
    end;

add_tests([],Spec) ->				% done
    Spec.

%% check if it's a CT term that has bad format or if the user seems to
%% have added something of his/her own, which we'll let pass if relaxed
%% mode is enabled.
check_term(Term) when is_tuple(Term) ->
    Size = size(Term),
    [Name|_] = tuple_to_list(Term),
    Valid = valid_terms(),
    case lists:member({Name,Size},Valid) of
	true ->
	    valid;
	false ->
	    case lists:keymember(Name,1,Valid) of
		true ->					% halt
		    throw({error,{bad_term_in_spec,Term}});
		false ->				% ignore
		    case get(relaxed) of
			true ->
			    %% warn if name resembles a CT term
			    case resembles_ct_term(Name,size(Term)) of
				true ->
				    io:format("~nSuspicious term, "
					      "please check:~n"
					      "~tp~n", [Term]),
				    invalid;
				false ->
				    invalid
			    end;
			false ->
			    throw({error,{undefined_term_in_spec,Term}})
		    end
	    end
    end.

%% specific data handling before saving in testspec record, e.g.
%% converting relative paths to absolute for directories and files
%% (introduce a clause *only* if the data value needs processing)
handle_data(logdir,Node,Dir,Spec) ->
    [{Node,ref2dir(Dir,Spec)}];
handle_data(cover,Node,File,Spec) ->
    [{Node,get_absfile(File,Spec)}];
handle_data(cover_stop,Node,Stop,_Spec) ->
    [{Node,Stop}];
handle_data(include,Node,Dirs=[D|_],Spec) when is_list(D) ->
    [{Node,ref2dir(Dir,Spec)} || Dir <- Dirs];
handle_data(include,Node,Dir=[Ch|_],Spec) when is_integer(Ch) ->
    handle_data(include,Node,[Dir],Spec);
handle_data(config,Node,File=[Ch|_],Spec) when is_integer(Ch) ->
    handle_data(config,Node,[File],Spec);
handle_data(config,Node,{CfgDir,File=[Ch|_]},Spec) when is_integer(Ch) ->
    handle_data(config,Node,{CfgDir,[File]},Spec);
handle_data(config,Node,Files=[F|_],Spec) when is_list(F) ->
    [{Node,get_absfile(File,Spec)} || File <- Files];
handle_data(config,Node,{CfgDir,Files=[F|_]},Spec) when is_list(F) ->
    [{Node,filename:join(ref2dir(CfgDir,Spec),File)} || File <- Files];
handle_data(userconfig,Node,CBs,Spec) when is_list(CBs) ->
    [{Node,{Callback,get_absfile(Callback,Config,Spec)}} ||
	{Callback,Config} <- CBs];
handle_data(userconfig,Node,CB,Spec) when is_tuple(CB) ->
    handle_data(userconfig,Node,[CB],Spec);
handle_data(event_handler,Node,H,Spec) when is_atom(H) ->
    handle_data(event_handler,Node,{[H],[]},Spec);
handle_data(event_handler,Node,{H,Args},Spec) when is_atom(H) ->
    handle_data(event_handler,Node,{[H],Args},Spec);
handle_data(event_handler,Node,Hs,_Spec) when is_list(Hs) ->
    [{Node,EvH,[]} || EvH <- Hs];
handle_data(event_handler,Node,{Hs,Args},_Spec) when is_list(Hs) ->
    [{Node,EvH,Args} || EvH <- Hs];
handle_data(ct_hooks,Node,Hooks,_Spec) when is_list(Hooks) ->
    [{Node,Hook} || Hook <- Hooks ];
handle_data(ct_hooks,Node,Hook,_Spec) ->
    [{Node,Hook}];
handle_data(stylesheet,Node,CSSFile,Spec) ->
    [{Node,get_absfile(CSSFile,Spec)}];
handle_data(verbosity,Node,VLvls,_Spec) when is_integer(VLvls) ->
    [{Node,[{'$unspecified',VLvls}]}];
handle_data(verbosity,Node,VLvls,_Spec) when is_list(VLvls) ->
    VLvls1 = lists:map(fun(VLvl = {_Cat,_Lvl}) -> VLvl;
			  (Lvl) -> {'$unspecified',Lvl} end, VLvls),
    [{Node,VLvls1}];
handle_data(multiply_timetraps,Node,Mult,_Spec) when is_integer(Mult) ->
    [{Node,Mult}];
handle_data(scale_timetraps,Node,Scale,_Spec) when Scale == true;
                                                   Scale == false ->
    [{Node,Scale}];
handle_data(silent_connections,Node,all,_Spec) ->
    [{Node,[all]}];
handle_data(silent_connections,Node,Conn,_Spec) when is_atom(Conn) ->
    [{Node,[Conn]}];
handle_data(silent_connections,Node,Conns,_Spec) ->
    [{Node,Conns}];
handle_data(_Tag,Node,Data,_Spec) ->
    [{Node,Data}].

%% check if duplicates should be saved or not
should_be_added(Tag,Node,_Data,Spec) ->
    if 
	%% list terms *without* possible duplicates here
	Tag == logdir;       Tag == logopts;
	Tag == basic_html;   Tag == esc_chars;
	Tag == label;        Tag == auto_compile;
	Tag == abort_if_missing_suites;
	Tag == stylesheet;   Tag == verbosity;
        Tag == multiply_timetraps;
        Tag == scale_timetraps;
	Tag == silent_connections ->
	    lists:keymember(ref2node(Node,Spec#testspec.nodes),1,
			    read_field(Spec,Tag)) == false;
	%% for terms *with* possible duplicates
	true ->
	    true
    end.

%% check if previous elements for Node should be deleted
update_recorded(Tag,Node,Spec) ->
    if Tag == config; Tag == userconfig; Tag == event_handler;
       Tag == ct_hooks; Tag == include ->
	    read_field(Spec,Tag);
       true ->
	    %% delete previous value for Tag
	    lists:keydelete(Node,1,read_field(Spec,Tag))
    end.

%% create one test term per node
per_node(Nodes,Tag,Data,Tests,Refs) ->
    Separated = per_node(Nodes,Tag,Data,Refs),
    Separated ++ Tests.
per_node([N|Ns],Tag,Data,Refs) ->
    [list_to_tuple([Tag,ref2node(N,Refs)|Data])|per_node(Ns,Tag,Data,Refs)];
per_node([],_,_,_) ->
    [].

%% Change the testspec record "back" to a list of tuples
testspec_rec2list(Rec) ->
    {Terms,_} = lists:mapfoldl(fun(unknown, Pos) ->
				       {element(Pos, Rec),Pos+1};
				  (F, Pos) ->
				       {{F,element(Pos, Rec)},Pos+1}
			       end,2,?testspec_fields),
    lists:flatten(Terms).

%% Extract one or more values from a testspec record and
%% return the result as a list of tuples
testspec_rec2list(Field, Rec) when is_atom(Field) ->
    [Term] = testspec_rec2list([Field], Rec),
    Term;
testspec_rec2list(Fields, Rec) ->
    Terms = testspec_rec2list(Rec),
    [{Field,proplists:get_value(Field, Terms)} || Field <- Fields].

%% read the value for FieldName in record Rec#testspec
read_field(Rec, FieldName) ->
    catch lists:foldl(fun(F, Pos) when F == FieldName ->
			      throw(element(Pos, Rec));
			 (_,Pos)  ->
			      Pos+1
		      end,2,?testspec_fields).

%% modify the value for FieldName in record Rec#testspec
mod_field(Rec, FieldName, NewVal) ->
    [_testspec|RecList] = tuple_to_list(Rec),
    RecList1 =
	(catch lists:foldl(fun(F, {Prev,[_OldVal|Rest]}) when F == FieldName ->
				   throw(lists:reverse(Prev) ++ [NewVal|Rest]);
			      (_,{Prev,[Field|Rest]})  ->
				   {[Field|Prev],Rest}
			   end,{[],RecList},?testspec_fields)),
    list_to_tuple([testspec|RecList1]).

%% Representation:
%% {{Node,Dir},[{Suite1,[GrOrCase11,GrOrCase12,...]},
%%              {Suite2,[GrOrCase21,GrOrCase22,...]},...]}
%% {{Node,Dir},[{Suite1,{skip,Cmt}},
%%              {Suite2,[{GrOrCase21,{skip,Cmt}},GrOrCase22,...]},...]}
%% GrOrCase = {GroupSpec,[Case1,Case2,...]} | Case
%% GroupSpec = {GroupName,OverrideProps} |
%%             {GroupName,OverrideProps,SubGroupSpec}
%% OverrideProps = Props | default
%% SubGroupSpec = GroupSpec | []

insert_suites(Node,Dir,[S|Ss],Tests, MergeTests) ->
    Tests1 = insert_cases(Node,Dir,S,all,Tests,MergeTests),
    insert_suites(Node,Dir,Ss,Tests1,MergeTests);
insert_suites(_Node,_Dir,[],Tests,_MergeTests) ->
    Tests;
insert_suites(Node,Dir,S,Tests,MergeTests) ->
    insert_suites(Node,Dir,[S],Tests,MergeTests).

insert_groups(Node,Dir,Suite,Group,Cases,Tests,MergeTests) 
  when is_atom(Group); is_tuple(Group) ->
    insert_groups(Node,Dir,Suite,[Group],Cases,Tests,MergeTests);
insert_groups(Node,Dir,Suite,Groups,Cases,Tests,false) when
      ((Cases == all) or is_list(Cases)) and is_list(Groups) ->
    Groups1 = [if is_list(Gr) ->		% preserve group path
		       {[Gr],Cases};
		  true ->
		       {Gr,Cases} end || Gr <- Groups],
    append({{Node,Dir},[{Suite,Groups1}]},Tests);
insert_groups(Node,Dir,Suite,Groups,Cases,Tests,true) when
      ((Cases == all) or is_list(Cases)) and is_list(Groups) ->
    Groups1 = [if is_list(Gr) ->		% preserve group path
		       {[Gr],Cases};
		  true ->
		       {Gr,Cases} end || Gr <- Groups],
    {Tests1,Done} =
	lists:foldr(fun(All={{N,D},[{all,_}]},{Replaced,_}) when N == Node,
								 D == Dir ->
			    {[All|Replaced],true};
		       ({{N,D},Suites0},{Replaced,_}) when N == Node,
							   D == Dir ->
			    Suites1 = insert_groups1(Suite,Groups1,Suites0),
			    {[{{N,D},Suites1}|Replaced],true};
		       (T,{Replaced,Match}) ->
			    {[T|Replaced],Match}
		    end, {[],false}, Tests),
    if not Done ->
	    Tests ++ [{{Node,Dir},[{Suite,Groups1}]}];
       true ->
	    Tests1
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
	    insert_in_order({Suite,GrAndCases},Suites0,replace);
	false ->
	    insert_in_order({Suite,Groups},Suites0)
    end.

insert_groups2(_Groups,all) ->
    all;
insert_groups2([Group={Gr,Cases}|Groups],GrAndCases) ->
    case lists:keysearch(Gr,1,GrAndCases) of
	{value,{Gr,all}} ->
	    GrAndCases;
	{value,{Gr,Cases0}} ->
	    Cases1 = insert_in_order(Cases,Cases0),
	    insert_groups2(Groups,insert_in_order({Gr,Cases1},GrAndCases));
	false ->
	    insert_groups2(Groups,insert_in_order(Group,GrAndCases))
    end;
insert_groups2([],GrAndCases) ->
    GrAndCases.

insert_cases(Node,Dir,Suite,Cases,Tests,false) when is_list(Cases) ->
    append({{Node,Dir},[{Suite,Cases}]},Tests);
insert_cases(Node,Dir,Suite,Cases,Tests,true) when is_list(Cases) ->
    {Tests1,Done} =
	lists:foldr(fun(All={{N,D},[{all,_}]},{Merged,_}) when N == Node,
							       D == Dir ->
			    {[All|Merged],true};
		       ({{N,D},Suites0},{Merged,_}) when N == Node,
							   D == Dir ->
			    Suites1 = insert_cases1(Suite,Cases,Suites0),
			    {[{{N,D},Suites1}|Merged],true};
		       (T,{Merged,Match}) ->
			    {[T|Merged],Match}
		    end, {[],false}, Tests),
    if Tests == [] ->
	    %% initial case with length(Cases) > 1, we need to do this
	    %% to merge possible duplicate cases in Cases
	    [{{Node,Dir},insert_cases1(Suite,Cases,[{Suite,[]}])}];
	not Done ->
	    %% no merging done, simply add these cases to Tests 
	    Tests ++ [{{Node,Dir},[{Suite,Cases}]}];
       true ->
	    Tests1
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
	    insert_in_order({Suite,Cases1},Suites0,replace);
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
    {Tests1,Done} =
	lists:foldr(fun({{N,D},Suites0},{Merged,_}) when N == Node,
							   D == Dir ->
			    Suites1 = skip_groups1(Suite,
						   [{Gr,Cases} || Gr <- Groups],
						   Cmt,Suites0),
			    {[{{N,D},Suites1}|Merged],true};
		       (T,{Merged,Match}) ->
			    {[T|Merged],Match}
		    end, {[],false}, Tests),
    if not Done ->
	    Tests ++ [{{Node,Dir},skip_groups1(Suite,
					      [{Gr,Cases} || Gr <- Groups],
					      Cmt,[])}];
       true ->
	    Tests1
    end;
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
	    insert_in_order({Suite,GrAndCases1},Suites0,replace);
	false ->
	    case Suites0 of
		[{all,_}=All|Skips]->
		    [All|Skips++[{Suite,SkipGroups}]];
                _ ->
                    insert_in_order({Suite,SkipGroups},Suites0,replace)
            end
    end.

skip_cases(Node,Dir,Suite,Cases,Cmt,Tests,false) when is_list(Cases) ->
    Suites1 = skip_cases1(Suite,Cases,Cmt,[]),
    append({{Node,Dir},Suites1},Tests);
skip_cases(Node,Dir,Suite,Cases,Cmt,Tests,true) when is_list(Cases) ->
    {Tests1,Done} =
	lists:foldr(fun({{N,D},Suites0},{Merged,_}) when N == Node,
							 D == Dir ->
			    Suites1 = skip_cases1(Suite,Cases,Cmt,Suites0),
			    {[{{N,D},Suites1}|Merged],true};
		       (T,{Merged,Match}) ->
			    {[T|Merged],Match}
		    end, {[],false}, Tests),
    if not Done ->
	    Tests ++ [{{Node,Dir},skip_cases1(Suite,Cases,Cmt,[])}];
       true ->
	    Tests1
    end;
skip_cases(Node,Dir,Suite,Case,Cmt,Tests,MergeTests) when is_atom(Case) ->
    skip_cases(Node,Dir,Suite,[Case],Cmt,Tests,MergeTests).

skip_cases1(Suite,Cases,Cmt,Suites0) ->
    SkipCases = lists:map(fun(C) ->
				  {C,{skip,Cmt}}
			  end,Cases),
    case lists:keysearch(Suite,1,Suites0) of
	{value,{Suite,Cases0}} ->
	    Cases1 = Cases0 ++ SkipCases,
	    insert_in_order({Suite,Cases1},Suites0,replace);
	false ->
	    case Suites0 of
		[{all,_}=All|Skips]->
		    [All|Skips++[{Suite,SkipCases}]];
		_ ->
		    insert_in_order({Suite,SkipCases},Suites0,replace)
	    end
    end.

append(Elem, List) ->
    List ++ [Elem].

insert_in_order(Elems,Dest) ->
        insert_in_order1(Elems,Dest,false).

insert_in_order(Elems,Dest,replace) ->
    insert_in_order1(Elems,Dest,true).

insert_in_order1([_E|Es],all,Replace) ->
    insert_in_order1(Es,all,Replace);

insert_in_order1([E|Es],List,Replace) ->
    List1 = insert_elem(E,List,[],Replace),
    insert_in_order1(Es,List1,Replace);
insert_in_order1([],List,_Replace) ->
    List;
insert_in_order1(E,List,Replace) ->
    insert_elem(E,List,[],Replace).


insert_elem({Key,_}=E,[{Key,_}|Rest],SoFar,true) ->
    lists:reverse([E|SoFar]) ++ Rest;
insert_elem({E,_},[E|Rest],SoFar,true) ->
    lists:reverse([E|SoFar]) ++ Rest;
insert_elem(E,[E|Rest],SoFar,true) ->
    lists:reverse([E|SoFar]) ++ Rest;

insert_elem({all,_}=E,_,SoFar,_Replace) ->
    lists:reverse([E|SoFar]);
insert_elem(_E,[all|_],SoFar,_Replace) ->
    lists:reverse(SoFar);
insert_elem(_E,[{all,_}],SoFar,_Replace) ->
    lists:reverse(SoFar);
insert_elem({Key,_}=E,[{Key,[]}|Rest],SoFar,_Replace) ->
    lists:reverse([E|SoFar]) ++ Rest;
insert_elem(E,[E1|Rest],SoFar,Replace) ->
    insert_elem(E,Rest,[E1|SoFar],Replace);
insert_elem(E,[],SoFar,_Replace) ->
    lists:reverse([E|SoFar]).

ref2node(all_nodes,_Refs) ->
    all_nodes;
ref2node(master,_Refs) ->
    master;
ref2node(RefOrNode,Refs) ->
    case lists:member($@,atom_to_list(RefOrNode)) of
	false ->				% a ref	    
	    case lists:keysearch(RefOrNode,1,Refs) of
		{value,{RefOrNode,Node}} ->
		    Node;
		false ->
		    throw({error,{noderef_missing,RefOrNode}})
	    end;
	true ->					% a node
	    RefOrNode
    end.

ref2dir(Ref,Spec) ->
    ref2dir(Ref,Spec#testspec.alias,Spec).

ref2dir(Ref,Refs,Spec) when is_atom(Ref) ->
    case lists:keysearch(Ref,1,Refs) of
	{value,{Ref,Dir}} ->
	    get_absdir(Dir,Spec);
	false ->
	    throw({error,{alias_missing,Ref}})
    end;
ref2dir(Dir,_,Spec) when is_list(Dir) ->
    get_absdir(Dir,Spec);
ref2dir(What,_,_) ->
    throw({error,{invalid_directory_name,What}}).

is_node(What,Nodes) when is_atom(What) ->
    is_node([What],Nodes);
is_node([master|_],_Nodes) ->
    true;
is_node(What={N,H},Nodes) when is_atom(N), is_atom(H) ->
    is_node([What],Nodes);
is_node([What|_],Nodes) ->
    case lists:keymember(What,1,Nodes) or
	 lists:keymember(What,2,Nodes) of
	true ->					
	    true;
	false ->
	    false
    end;
is_node([],_) ->
    false.

valid_terms() ->
    [
     {set_merge_tests,2},
     {define,3},
     {specs,3},
     {node,3},
     {cover,2},
     {cover,3},
     {cover_stop,2},
     {cover_stop,3},
     {config,2},
     {config,3},
     {config,4},
     {userconfig,2},
     {userconfig,3},
     {alias,3},
     {merge_tests,2},
     {logdir,2},
     {logdir,3},
     {logopts,2},
     {logopts,3},
     {basic_html,2},
     {basic_html,3},
     {esc_chars,2},
     {esc_chars,3},
     {verbosity,2},
     {verbosity,3},
     {silent_connections,2},
     {silent_connections,3},
     {label,2},
     {label,3},
     {event_handler,2},
     {event_handler,3},
     {event_handler,4},
     {ct_hooks,2},
     {ct_hooks,3},
     {enable_builtin_hooks,2},
     {release_shell,2},
     {multiply_timetraps,2},
     {multiply_timetraps,3},
     {scale_timetraps,2},
     {scale_timetraps,3},
     {include,2},
     {include,3},
     {auto_compile,2},
     {auto_compile,3},
     {abort_if_missing_suites,2},
     {abort_if_missing_suites,3},     
     {stylesheet,2},
     {stylesheet,3},
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
     {skip_cases,6},
     {create_priv_dir,2},
     {create_priv_dir,3}
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
