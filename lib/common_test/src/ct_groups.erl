%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2012. All Rights Reserved.
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

%%% @doc Common Test Framework callback module.
%%%
%%% <p>This module contains CT internal help functions for searching
%%%    through groups specification trees and producing resulting
%%%    tests.</p>

-module(ct_groups).

-export([find_groups/4]).
-export([make_all_conf/3, make_conf/5]).
-export([delete_subs/2]).
-export([expand_groups/3, search_and_override/3]).

-define(val(Key, List), proplists:get_value(Key, List)). 
-define(val(Key, List, Def), proplists:get_value(Key, List, Def)).
-define(rev(L), lists:reverse(L)).

find_groups(Mod, GrNames, TCs, GroupDefs) when is_atom(GrNames) ; 
					       (length(GrNames) == 1) ->
    find_groups1(Mod, GrNames, TCs, GroupDefs);

find_groups(Mod, Groups, TCs, GroupDefs) when Groups /= [] ->
    [find_groups1(Mod, [GrNames], TCs, GroupDefs) || GrNames <- Groups];

find_groups(_Mod, [], _TCs, _GroupDefs) ->
    [].

%% GrNames == atom: Single group name, perform full search
%% GrNames == list: List of groups, find all matching paths
%% GrNames == [list]: Search path terminated by last group in GrNames
find_groups1(Mod, GrNames, TCs, GroupDefs) ->
    {GrNames1,FindAll} =
	case GrNames of
	    Name when is_atom(Name), Name /= all ->
		{[Name],true};
	    [Path] when is_list(Path) ->
		{Path,false};
	    Path ->
		{Path,true}
	end,
    TCs1 = if is_atom(TCs), TCs /= all -> [TCs];
	      true -> TCs end,
    Found = find(Mod, GrNames1, TCs1, GroupDefs, [],
		 GroupDefs, FindAll),
    [Conf || Conf <- Found, Conf /= 'NOMATCH'].

%% Locate all groups
find(Mod, all, all, [{Name,Props,Tests} | Gs], Known, Defs, _) 
  when is_atom(Name), is_list(Props), is_list(Tests) ->
    cyclic_test(Mod, Name, Known),
    trim(make_conf(Mod, Name, Props,
		   find(Mod, all, all, Tests, [Name | Known],
			Defs, true))) ++
	find(Mod, all, all, Gs, Known, Defs, true);

%% Locate particular TCs in all groups
find(Mod, all, TCs, [{Name,Props,Tests} | Gs], Known, Defs, _) 
  when is_atom(Name), is_list(Props), is_list(Tests) ->
    cyclic_test(Mod, Name, Known),
    Tests1 = rm_unwanted_tcs(Tests, TCs, []),
    trim(make_conf(Mod, Name, Props,
		   find(Mod, all, TCs, Tests1, [Name | Known],
			Defs, true))) ++
	find(Mod, all, TCs, Gs, Known, Defs, true);

%% Found next group is in search path
find(Mod, [Name|GrNames]=SPath, TCs, [{Name,Props,Tests} | Gs], Known,
     Defs, FindAll) when is_atom(Name), is_list(Props), is_list(Tests) ->
    cyclic_test(Mod, Name, Known),
    Tests1 = rm_unwanted_tcs(Tests, TCs, GrNames),
    trim(make_conf(Mod, Name, Props,
		   find(Mod, GrNames, TCs, Tests1, [Name|Known],
			Defs, FindAll))) ++
	find(Mod, SPath, TCs, Gs, Known, Defs, FindAll);

%% Group path terminated, stop the search
find(Mod, [], TCs, Tests, _Known, _Defs, false) ->
    case [{Mod,TC} || TC <- Tests,
		      (((TCs == all) and is_atom(TC))
		       or ((catch lists:member(TC, TCs)) == true))] of
	[]    -> ['NOMATCH'];
	Cases -> Cases
    end;

%% No more groups
find(_Mod, [_|_], _TCs, [], _Known, _Defs, _) ->
    ['NOMATCH'];

%% Found group not next in search path
find(Mod, GrNames, TCs, [{Name,Props,Tests} | Gs], Known,
     Defs, FindAll) when is_atom(Name), is_list(Props), is_list(Tests) ->
    cyclic_test(Mod, Name, Known),
    Tests1 = rm_unwanted_tcs(Tests, TCs, GrNames),
    trim(make_conf(Mod, Name, Props,
		   find(Mod, GrNames, TCs, Tests1, [Name|Known],
			Defs, FindAll))) ++
	find(Mod, GrNames, TCs, Gs, Known, Defs, FindAll);
  
%% A nested group defined on top level found
find(Mod, GrNames, TCs, [{group,Name1} | Gs], Known, Defs, FindAll) 
  when is_atom(Name1) ->
    find(Mod, GrNames, TCs, [expand(Mod, Name1, Defs) | Gs], Known,
	 Defs, FindAll);

%% Undocumented remote group feature, use with caution
find(Mod, GrNames, TCs, [{group, ExtMod, ExtGrp} | Gs], Known,
     Defs, FindAll) when is_atom(ExtMod), is_atom(ExtGrp) ->
    ExternalDefs = ExtMod:groups(),
    ExternalTCs = find(ExtMod, ExtGrp, TCs, [{group, ExtGrp}],
		       [], ExternalDefs, FindAll),
    ExternalTCs ++ find(Mod, GrNames, TCs, Gs, Known, Defs, FindAll);

%% Group definition without properties, add an empty property list
find(Mod, GrNames, TCs, [{Name1,Tests} | Gs], Known, Defs, FindAll)
  when is_atom(Name1), is_list(Tests) ->
    find(Mod, GrNames, TCs, [{Name1,[],Tests} | Gs], Known, Defs, FindAll);

%% Save, and keep searching
find(Mod, GrNames, TCs, [{ExternalTC, Case} = TC | Gs], Known,
     Defs, FindAll) when is_atom(ExternalTC),
			 is_atom(Case) ->
    [TC | find(Mod, GrNames, TCs, Gs, Known, Defs, FindAll)];

%% Save test case
find(Mod, GrNames, all, [TC | Gs], Known,
     Defs, FindAll) when is_atom(TC) ->
    [{Mod, TC} | find(Mod, GrNames, all, Gs, Known, Defs, FindAll)];

%% Check if test case should be saved
find(Mod, GrNames, TCs, [TC | Gs], Known,
     Defs, FindAll) when is_atom(TC) ->
    case lists:member(TC, TCs) of
	true ->
	    [{Mod, TC} | find(Mod, GrNames, TCs, Gs, Known,
			      Defs, FindAll)];
	false ->
	    find(Mod, GrNames, TCs, Gs, Known, Defs, FindAll)
    end;

%% Unexpeted term in group list
find(Mod, _GrNames, _TCs, [BadTerm | _Gs], Known, _Defs, _FindAll) ->
    Where = if length(Known) == 0 ->
		    atom_to_list(Mod)++":groups/0";
	       true ->
		    "group "++atom_to_list(lists:last(Known))++
			" in "++atom_to_list(Mod)++":groups/0"
	    end,		 
    Term = io_lib:format("~p", [BadTerm]),
    E = "Bad term "++lists:flatten(Term)++" in "++Where,
    throw({error,list_to_atom(E)});

%% No more groups
find(_Mod, _GrNames, _TCs, [], _Known, _Defs, _) ->
    [].

%%%-----------------------------------------------------------------

%% We have to always search bottom up to only remove a branch
%% if there's 'NOMATCH' in the leaf (otherwise, the branch should
%% be kept)

trim({conf,Props,Init,Tests,End}) ->
    try trim(Tests) of
	[] -> [];
	Tests1 -> [{conf,Props,Init,Tests1,End}]
    catch
	throw:_ -> []
    end;

trim(Tests) when is_list(Tests) ->
    %% we need to compare the result of trimming each test on this
    %% level, and only let a 'NOMATCH' fail the level if no
    %% successful sub group can be found
    Tests1 =
	lists:flatmap(fun(Test) ->
			      IsConf = case Test of
					   {conf,_,_,_,_} ->
					       true;
					   _ ->
					       false
				       end,
			      try trim_test(Test) of
				  [] -> [];
				  Test1 when IsConf -> [{conf,Test1}];
				  Test1 -> [Test1]
			      catch
				  throw:_ -> ['NOMATCH']
			      end
		      end, Tests),
    case lists:keymember(conf, 1, Tests1) of
	true ->					% at least one successful group
	    lists:flatmap(fun({conf,Test}) -> [Test];
			     ('NOMATCH') -> [];	% ignore any 'NOMATCH'
			     (Test) -> [Test]
			  end, Tests1);
	false ->
	    case lists:member('NOMATCH', Tests1) of
		true ->
		    throw('NOMATCH');
		false ->
		    Tests1
	    end
    end.

trim_test({conf,Props,Init,Tests,End}) ->
    case trim(Tests) of
	[] ->
	    [];
	Tests1 ->
	    {conf,Props,Init,Tests1,End}
    end;

trim_test('NOMATCH') ->
    throw('NOMATCH');

trim_test(Test) ->
    Test.

%% GrNames is [] if the terminating group has been found. From
%% that point, all specified test should be included (as well as
%% sub groups for deeper search).
rm_unwanted_tcs(Tests, all, []) ->
    Tests;

rm_unwanted_tcs(Tests, TCs, []) ->
    [Test || Test <- Tests,
	     ((is_atom(Test) and (lists:member(Test, TCs) == true))
	      or (not is_atom(Test)))];

rm_unwanted_tcs(Tests, _TCs, _) ->
    [Test || Test <- Tests, not is_atom(Test)].

delete_subs([{conf, _,_,_,_} = Conf | Confs], All) ->
    All1 = delete_conf(Conf, All),
    case is_sub(Conf, All1) of
	true ->
	    delete_subs(Confs, All1);
	false ->
	    delete_subs(Confs, All)
    end;
delete_subs([_Else | Confs], All) ->
    delete_subs(Confs, All);
delete_subs([], All) ->
    All.

delete_conf({conf,Props,_,_,_}, Confs) ->
    Name = ?val(name, Props),
    [Conf || Conf = {conf,Props0,_,_,_} <- Confs,
	     Name =/= ?val(name, Props0)].

is_sub({conf,Props,_,_,_}=Conf, [{conf,_,_,Tests,_} | Confs]) ->
    Name = ?val(name, Props),
    case lists:any(fun({conf,Props0,_,_,_}) ->
			   case ?val(name, Props0) of
			       N when N == Name ->
				   true;
			       _ ->
				   false
			   end;
		      (_) ->
			   false
		   end, Tests) of
	true ->
	    true;
	false ->
	    is_sub(Conf, Tests) or is_sub(Conf, Confs)
    end;

is_sub(Conf, [_TC | Tests]) ->
    is_sub(Conf, Tests);

is_sub(_Conf, []) ->
    false.


cyclic_test(Mod, Name, Names) ->
    case lists:member(Name, Names) of
	true ->
	    E = "Cyclic reference to group "++atom_to_list(Name)++
		" in "++atom_to_list(Mod)++":groups/0",
	    throw({error,list_to_atom(E)});
	false ->
	    ok
    end.

expand(Mod, Name, Defs) ->
    case lists:keysearch(Name, 1, Defs) of
	{value,Def} -> 
	    Def;
	false ->
	    E = "Invalid group "++atom_to_list(Name)++
		" in "++atom_to_list(Mod)++":groups/0",
	    throw({error,list_to_atom(E)})
    end.

make_all_conf(Dir, Mod, _Props) ->
    case code:is_loaded(Mod) of
	false ->
	    code:load_abs(filename:join(Dir,atom_to_list(Mod)));
	_ ->
	    ok
    end,
    make_all_conf(Mod).

make_all_conf(Mod) ->
    case catch apply(Mod, groups, []) of
	{'EXIT',_} ->
	    {error,{invalid_group_definition,Mod}};
	GroupDefs when is_list(GroupDefs) ->
	    case catch find_groups(Mod, all, all, GroupDefs) of
		{error,_} = Error ->
		    %% this makes test_server call error_in_suite as first
		    %% (and only) test case so we can report Error properly
		    [{ct_framework,error_in_suite,[[Error]]}];
		[] ->
		    {error,{invalid_group_spec,Mod}};
		ConfTests ->
		    [{conf,Props,Init,all,End} ||
			{conf,Props,Init,_,End}
			    <- delete_subs(ConfTests, ConfTests)]
	    end
    end.

make_conf(Dir, Mod, Name, Props, TestSpec) ->
    case code:is_loaded(Mod) of
	false ->
	    code:load_abs(filename:join(Dir,atom_to_list(Mod)));
	_ ->
	    ok
    end,
    make_conf(Mod, Name, Props, TestSpec).

make_conf(Mod, Name, Props, TestSpec) ->
    case code:is_loaded(Mod) of
	false ->
	    code:load_file(Mod);
	_ ->
	    ok
    end,
    {InitConf,EndConf,ExtraProps} =
	case erlang:function_exported(Mod,init_per_group,2) of
	    true ->
		{{Mod,init_per_group},{Mod,end_per_group},[]};
	    false ->
		ct_logs:log("TEST INFO", "init_per_group/2 and "
			    "end_per_group/2 missing for group "
			    "~p in ~p, using default.",
			    [Name,Mod]),
		{{ct_framework,init_per_group},
		 {ct_framework,end_per_group},
		 [{suite,Mod}]}
	end,
    {conf,[{name,Name}|Props++ExtraProps],InitConf,TestSpec,EndConf}.

%%%-----------------------------------------------------------------

expand_groups([H | T], ConfTests, Mod) ->
    [expand_groups(H, ConfTests, Mod) | expand_groups(T, ConfTests, Mod)];
expand_groups([], _ConfTests, _Mod) ->
    [];
expand_groups({group,Name}, ConfTests, Mod) ->
    expand_groups({group,Name,default,[]}, ConfTests, Mod);
expand_groups({group,Name,default}, ConfTests, Mod) ->
    expand_groups({group,Name,default,[]}, ConfTests, Mod);
expand_groups({group,Name,ORProps}, ConfTests, Mod) when is_list(ORProps) ->
    expand_groups({group,Name,ORProps,[]}, ConfTests, Mod);
expand_groups({group,Name,ORProps,SubORSpec}, ConfTests, Mod) ->
    FindConf =
	fun(Conf = {conf,Props,Init,Ts,End}) ->
		case ?val(name, Props) of
		    Name when ORProps == default ->
			[Conf];
		    Name ->
			Props1 = case ?val(suite, Props) of
				     undefined ->
					 ORProps;
				     SuiteName ->
					 [{suite,SuiteName}|ORProps]
				 end,
			[{conf,[{name,Name}|Props1],Init,Ts,End}];
		    _    -> 
			[]
		end
	end,					 
    case lists:flatmap(FindConf, ConfTests) of
	[] ->
	    throw({error,invalid_ref_msg(Name, Mod)});
	Matching when SubORSpec == [] -> 
	    Matching;
	Matching -> 
	    override_props(Matching, SubORSpec, Name,Mod)
    end;
expand_groups(SeqOrTC, _ConfTests, _Mod) ->
    SeqOrTC.

%% search deep for the matching conf test and modify it and any 
%% sub tests according to the override specification
search_and_override([Conf = {conf,Props,Init,Tests,End}], ORSpec, Mod) ->
    InsProps = fun(GrName, undefined, Ps) ->
		       [{name,GrName} | Ps];
		  (GrName, Suite, Ps) ->
		       [{name,GrName}, {suite,Suite} | Ps]
	       end,
    Name = ?val(name, Props),
    Suite = ?val(suite, Props),
    case lists:keysearch(Name, 1, ORSpec) of
	{value,{Name,default}} ->
	    [Conf];
	{value,{Name,ORProps}} ->
	    [{conf,InsProps(Name,Suite,ORProps),Init,Tests,End}];
	{value,{Name,default,[]}} ->
	    [Conf];
	{value,{Name,default,SubORSpec}} ->
	    override_props([Conf], SubORSpec, Name,Mod);
	{value,{Name,ORProps,SubORSpec}} ->
	    override_props([{conf,InsProps(Name,Suite,ORProps),
			    Init,Tests,End}], SubORSpec, Name,Mod);
	_ ->
	    [{conf,Props,Init,search_and_override(Tests,ORSpec,Mod),End}]
    end.

%% Modify the Tests element according to the override specification
override_props([{conf,Props,Init,Tests,End} | Confs], SubORSpec, Name,Mod) ->
    {Subs,SubORSpec1} = override_sub_props(Tests, [], SubORSpec, Mod),
    [{conf,Props,Init,Subs,End} | override_props(Confs, SubORSpec1, Name,Mod)];
override_props([], [], _,_) ->
    [];
override_props([], SubORSpec, Name,Mod) ->
    Es = [invalid_ref_msg(Name, element(1,Spec), Mod) || Spec <- SubORSpec],
    throw({error,Es}).

override_sub_props([], New, ORSpec, _) ->    
    {?rev(New),ORSpec};
override_sub_props([T = {conf,Props,Init,Tests,End} | Ts],
		   New, ORSpec, Mod) ->
    Name = ?val(name, Props),
    Suite = ?val(suite, Props),
    case lists:keysearch(Name, 1, ORSpec) of
	{value,Spec} ->				% group found in spec
	    Props1 =
		case element(2, Spec) of
		    default -> Props;
		    ORProps when Suite == undefined -> [{name,Name} | ORProps];
		    ORProps -> [{name,Name}, {suite,Suite} | ORProps]
		end,
	    case catch element(3, Spec) of
		Undef when Undef == [] ; 'EXIT' == element(1, Undef) ->
		    override_sub_props(Ts, [{conf,Props1,Init,Tests,End} | New],
				       lists:keydelete(Name, 1, ORSpec), Mod);
		SubORSpec when is_list(SubORSpec) ->
		    case override_sub_props(Tests, [], SubORSpec, Mod) of
			{Subs,[]} ->
			    override_sub_props(Ts, [{conf,Props1,Init,
						     Subs,End} | New],
					       lists:keydelete(Name, 1, ORSpec),
					       Mod);
			{_,NonEmptySpec} ->
			    Es = [invalid_ref_msg(Name, element(1, GrRef),
						  Mod) || GrRef <- NonEmptySpec],
			    throw({error,Es})
		    end;
		BadGrSpec ->
		    throw({error,{invalid_form,BadGrSpec}})
	    end;
	_ ->					% not a group in spec
	    override_sub_props(Ts, [T | New], ORSpec, Mod)
    end;
override_sub_props([TC | Ts], New, ORSpec, Mod) ->
    override_sub_props(Ts, [TC | New], ORSpec, Mod).

invalid_ref_msg(Name, Mod) ->
    E = "Invalid reference to group "++
	atom_to_list(Name)++" in "++
	atom_to_list(Mod)++":all/0",
    list_to_atom(E).

invalid_ref_msg(Name0, Name1, Mod) ->
    E = "Invalid reference to group "++
	atom_to_list(Name1)++" from "++atom_to_list(Name0)++
	" in "++atom_to_list(Mod)++":all/0",
    list_to_atom(E).
