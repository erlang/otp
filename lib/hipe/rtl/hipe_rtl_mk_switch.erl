%% -*- erlang-indent-level: 2 -*-
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_rtl_mk_switch.erl
%%  Module   :	hipe_rtl_mk_switch
%%  Purpose  :  Implements switching on Erlang values.
%%  Notes    :  Only fixnums are supported well,
%%              atoms work with table search, 
%%              the inline search of atoms might have some bugs.
%%              Should be extended to handle bignums and floats.
%%
%%  History  :	* 2001-02-28 Erik Johansson (happi@it.uu.se): 
%%                Created.
%%              * 2001-04-01 Erik Trulsson (ertr1013@csd.uu.se):
%%                           Stefan Lindström (stli3993@csd.uu.se):
%%                Added clustering and inlined binary search trees.
%%              * 2001-07-30 EJ (happi@it.uu.se):
%%                Fixed some bugs and started cleanup.
%% ====================================================================
%%  Exports  :
%%    gen_switch_val(I, VarMap, ConstTab, Options)
%%    gen_switch_tuple(I, Map, ConstTab, Options)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_mk_switch).

-export([gen_switch_val/4, gen_switch_tuple/4]).

%%-------------------------------------------------------------------------

-include("../main/hipe.hrl").

%%-------------------------------------------------------------------------

-define(MINFORJUMPTABLE,9).
	% Minimum number of integers needed to use something else than an inline search.
-define(MINFORINTSEARCHTREE,65).  % Must be at least 3
	% Minimum number of integer elements needed to use a non-inline binary search.

-define(MININLINEATOMSEARCH,8). 
	% Minimum number of atoms needed to use an inline binary search instead
	% of a fast linear search.

-define(MINFORATOMSEARCHTREE,20).  % Must be at least 3
	% Minimum number of atoms needed to use a non-inline binary search instead
	% of a linear search.
	
-define(MAXINLINEATOMSEARCH,64). % Must be at least 3
	% The cutoff point between inlined and non-inlined binary search for atoms

-define(WORDSIZE, hipe_rtl_arch:word_size()).
-define(MINDENSITY, 0.5).
        % Minimum density required to use a jumptable instead of a binary search.

%% The reason why MINFORINTSEARCHTREE and MINFORATOMSEARCHTREE must be
%% at least 3 is that the function tab/5 will enter an infinite loop
%% and hang when faced with a switch of size 1 or 2.


%% Options used by this module:
%%
%% [no_]use_indexing
%%    Determines if any indexing be should be done at all. Turned on
%%    by default at optimization level o2 and higher.
%%
%% [no_]use_clusters
%%    Controls whether we attempt to divide sparse integer switches
%%    into smaller dense clusters for which jumptables are practical.
%%    Turned off by default since it can increase compilation time
%%    considerably and most programs will gain little benefit from it.
%%
%% [no_]use_inline_atom_search
%%    Controls whether we use an inline binary search for small number
%%    of atoms. Turned off by default since this is currently only
%%    supported on SPARC (and not on x86) and probably needs a bit
%%    more testing before it can be turned on by default.

gen_switch_val(I, VarMap, ConstTab, Options) ->
  case proplists:get_bool(use_indexing, Options) of
    false -> gen_slow_switch_val(I, VarMap, ConstTab, Options);
    true -> gen_fast_switch_val(I, VarMap, ConstTab, Options)
  end.

gen_fast_switch_val(I, VarMap, ConstTab, Options) ->
  {Arg, VarMap0} = 
    hipe_rtl_varmap:icode_var2rtl_var(hipe_icode:switch_val_term(I), VarMap),
  IcodeFail = hipe_icode:switch_val_fail_label(I),
  {Fail, VarMap1} = hipe_rtl_varmap:icode_label2rtl_label(IcodeFail, VarMap0),
  %% Important that the list of cases is sorted when handling integers.
  UnsortedCases = hipe_icode:switch_val_cases(I),
  Cases = lists:sort(UnsortedCases),
  
  check_duplicates(Cases),
  %% This check is currently not really necessary.  The checking
  %% happens at an earlier phase of the compilation.
  {Types, InitCode} = split_types(Cases, Arg),
  handle_types(Types, InitCode, VarMap1, ConstTab, Arg, {I, Fail, Options}).

handle_types([{Type,Lbl,Cases}|Types], Code, VarMap, ConstTab, Arg, Info) ->
  {Code1,VarMap1,ConstTab1} = gen_fast_switch_on(Type, Cases, 
						 VarMap, 
						 ConstTab, Arg, Info),
  handle_types(Types, [Code,Lbl,Code1], VarMap1, ConstTab1, Arg, Info);
handle_types([], Code, VarMap, ConstTab, _, _) ->
  {Code, VarMap, ConstTab}.


gen_fast_switch_on(integer, Cases, VarMap, ConstTab, Arg, {I, Fail, Options})  ->
  {First,_} = hd(Cases),
  Min = hipe_icode:const_value(First),
  if length(Cases) < ?MINFORJUMPTABLE ->
      gen_small_switch_val(Arg,Cases,Fail,VarMap,ConstTab,Options);
     true ->
      case proplists:get_bool(use_clusters, Options) of
	false ->
	  M = list_to_tuple(Cases),
	  D = density(M, 1, tuple_size(M)),
	  if 
	    D >= ?MINDENSITY ->
	      gen_jump_table(Arg,Fail,hipe_icode:switch_val_fail_label(I),VarMap,ConstTab,Cases,Min);
	    true ->
	      gen_search_switch_val(Arg, Cases, Fail, VarMap, ConstTab, Options)
	  end;
	true ->
	  MC = minclusters(Cases),
	  Cl = cluster_split(Cases,MC),
	  CM = cluster_merge(Cl),
	  find_cluster(CM,VarMap,ConstTab,Options,Arg,Fail,hipe_icode:switch_val_fail_label(I))
      end
  end;
gen_fast_switch_on(atom, Cases, VarMap, ConstTab, Arg, {_I, Fail, Options})  ->
  case proplists:get_bool(use_inline_atom_search, Options) of
    true ->
      if
	length(Cases) < ?MININLINEATOMSEARCH ->
	  gen_linear_switch_val(Arg, Cases, Fail, VarMap, ConstTab, Options);
	length(Cases) > ?MAXINLINEATOMSEARCH ->
	  gen_search_switch_val(Arg, Cases, Fail, VarMap, ConstTab, Options);
	true ->
	  gen_atom_switch_val(Arg,Cases,Fail,VarMap,ConstTab,Options)
      end;
    false ->
      if length(Cases) < ?MINFORATOMSEARCHTREE ->
	  gen_linear_switch_val(Arg, Cases, Fail, VarMap, ConstTab, Options);
	 true ->
	  gen_search_switch_val(Arg, Cases, Fail, VarMap, ConstTab, Options)
      end
  end;	
gen_fast_switch_on(_, _, VarMap, ConstTab, _, {I,_Fail,Options})  ->
  %% We can only handle smart indexing of integers and atoms
  %% TODO: Consider bignum
  gen_slow_switch_val(I, VarMap, ConstTab, Options).


%% Split different types into separate switches.
split_types([Case|Cases], Arg) ->
  Type1 = casetype(Case),
  Types = split(Cases,Type1,[Case],[]),
  switch_on_types(Types,[], [], Arg);
split_types([],_) ->
  %% Cant happen.
  ?EXIT({empty_caselist}).

switch_on_types([{Type,Cases}], AccCode, AccCases, _Arg) ->
  Lbl = hipe_rtl:mk_new_label(),
  I = hipe_rtl:mk_goto(hipe_rtl:label_name(Lbl)),
  {[{Type,Lbl,lists:reverse(Cases)} | AccCases], lists:reverse([I|AccCode])};
switch_on_types([{other,Cases} | Rest], AccCode, AccCases, Arg) ->
  %% Make sure the general case is handled last.
  switch_on_types(Rest ++ [{other,Cases}], AccCode, AccCases, Arg);
switch_on_types([{Type,Cases} | Rest], AccCode, AccCases, Arg) ->
  TLab = hipe_rtl:mk_new_label(),
  FLab = hipe_rtl:mk_new_label(),
  TestCode = 
    case Type of
      integer ->
	hipe_tagscheme:test_fixnum(Arg, hipe_rtl:label_name(TLab), 
				   hipe_rtl:label_name(FLab), 0.5);
      atom ->
	hipe_tagscheme:test_atom(Arg, hipe_rtl:label_name(TLab), 
				 hipe_rtl:label_name(FLab), 0.5);
      bignum ->
	hipe_tagscheme:test_bignum(Arg, hipe_rtl:label_name(TLab), 
				   hipe_rtl:label_name(FLab), 0.5);
      _ -> ?EXIT({ooops, type_not_handled, Type})
    end,
  switch_on_types(Rest, [[TestCode,FLab] | AccCode],
		  [{Type,TLab,lists:reverse(Cases)} | AccCases], Arg).

split([Case|Cases], Type, Current, Rest) ->
  case casetype(Case) of
    Type ->
      split(Cases, Type, [Case|Current],Rest);
    Other ->
      split(Cases, Other, [Case], [{Type,Current}|Rest])
  end;
split([], Type, Current, Rest) ->
  [{Type, Current} | Rest].

%% Determine what type an entry in the caselist has

casetype({Const,_}) ->
  casetype(hipe_icode:const_value(Const));
casetype(A) ->
  if
    is_integer(A) -> 
      case hipe_tagscheme:is_fixnum(A) of
	true -> integer;
	false -> bignum
      end;
    is_float(A) -> float;
    is_atom(A) -> atom;
    true -> other
  end.

%% check that no duplicate values occur in the case list and also
%% check that all case values have the same type.
check_duplicates([]) -> true;
check_duplicates([_]) -> true;
check_duplicates([{Const1,_},{Const2,L2}|T]) ->
  C1 = hipe_icode:const_value(Const1),
  C2 = hipe_icode:const_value(Const2),
  %%	T1 = casetype(C1),
  %%	T2 = casetype(C2),
  if C1 =/= C2 -> %% , T1 =:= T2 ->
      check_duplicates([{Const2,L2}|T]);
     true ->
      ?EXIT({bad_values_in_switchval,C1})
  end.

%%
%% Determine the optimal way to divide Cases into clusters such that each 
%% cluster is dense.
%%
%% See:
%%  Producing Good Code for the Case Statement, Robert L. Bernstein
%%  Software - Practice and Experience vol 15, 1985, no 10, pp 1021--1024
%% And
%%  Correction to "Producing Good Code for the Case Statement"
%%  Sampath Kannan and Todd A. Proebsting,
%%  Software - Practice and Experience vol 24, 1994, no 2, p 233
%%
%% (The latter is where the algorithm comes from.)

%% This function will return a tuple with the first element being 0
%% The rest of the elements being integers. A value of M at index N
%% (where the first element is considered to have index 0) means that
%% the first N cases can be divided into M (but no fewer) clusters where
%% each cluster is dense.

minclusters(Cases) when is_list(Cases) ->
  minclusters(list_to_tuple(Cases));
minclusters(Cases) when is_tuple(Cases) ->
  N = tuple_size(Cases),
  MinClusters = list_to_tuple([0|n_list(N,inf)]),
  i_loop(1,N,MinClusters,Cases).

%% Create a list with N elements initialized to Init
n_list(0,_) -> [];
n_list(N,Init) -> [Init | n_list(N-1,Init)].

%% Do the dirty work of minclusters
i_loop(I,N,MinClusters,_Cases) when I > N -> 
  MinClusters;
i_loop(I,N,MinClusters,Cases) when I =< N ->
  M = j_loop(0, I-1, MinClusters, Cases),
  i_loop(I+1, N, M, Cases).

%% More dirty work	
j_loop(J,I1,MinClusters,_Cases) when J > I1 ->
  MinClusters;
j_loop(J,I1,MinClusters,Cases) when J =< I1 ->
  D = density(Cases,J+1,I1+1),
  A0 = element(J+1,MinClusters),
  A = if
	is_number(A0) ->
	  A0+1;
	true ->
	  A0
      end,
  B = element(I1+2,MinClusters),
  M = if
	D >= ?MINDENSITY, A<B ->
	  setelement(I1+2,MinClusters,A);
	true ->
	  MinClusters
      end,
  j_loop(J+1,I1,M,Cases).


%% Determine the density of a (subset of a) case list
%% A is a tuple with the cases in order from smallest to largest
%% I is the index of the first element and J of the last

density(A,I,J) ->
  {AI,_} = element(I,A),
  {AJ,_} = element(J,A),
  (J-I+1)/(hipe_icode:const_value(AJ)-hipe_icode:const_value(AI)+1).


%% Split a case list into dense clusters
%% Returns a list of lists of cases.
%%
%% Cases is the case list and Clust is a list describing the optimal
%% clustering as returned by minclusters
%%
%% If the value in the last place in minclusters is M then we can
%% split the case list into M clusters. We then search for the last
%% (== right-most) occurance of the value M-1 in minclusters. That
%% indicates the largest number of cases that can be split into M-1
%% clusters. This means that the cases in between constitute one
%% cluster. Then we recurse on the remainder of the cases.
%%
%% The various calls to lists:reverse are just to ensure that the
%% cases remain in the correct, sorted order.

cluster_split(Cases, Clust) ->
  A = tl(tuple_to_list(Clust)),
  Max = element(tuple_size(Clust), Clust),
  L1 = lists:reverse(Cases),
  L2 = lists:reverse(A),
  cluster_split(Max, [], [], L1, L2).

cluster_split(0, [], Res, Cases, _Clust) -> 
  L = lists:reverse(Cases),
  {H,_} = hd(L),
  {T,_} = hd(Cases),
  [{dense,hipe_icode:const_value(H),hipe_icode:const_value(T),L}|Res];
cluster_split(N, [], Res, Cases, [N|_] = Clust) -> 
  cluster_split(N-1, [], Res, Cases, Clust);
cluster_split(N,Sofar,Res,Cases,[N|Clust]) -> 
  {H,_} = hd(Sofar),
  {T,_} = lists:last(Sofar),
  cluster_split(N-1,[],[{dense,hipe_icode:const_value(H),hipe_icode:const_value(T),Sofar}|Res],Cases,[N|Clust]);
cluster_split(N,Sofar,Res,[C|Cases],[_|Clust]) ->
  cluster_split(N,[C|Sofar],Res,Cases,Clust).

%%
%% Merge adjacent small clusters into larger sparse clusters
%%
cluster_merge([C]) -> [C];
cluster_merge([{dense,Min,Max,C}|T]) when length(C) >= ?MINFORJUMPTABLE ->
  C2 = cluster_merge(T),
  [{dense,Min,Max,C}|C2];
cluster_merge([{sparse,Min,_,C},{sparse,_,Max,D}|T]) ->
  R = {sparse,Min,Max,C ++ D},
  cluster_merge([R|T]);
cluster_merge([{sparse,Min,_,C},{dense,_,Max,D}|T]) when length(D) < ?MINFORJUMPTABLE ->
  R = {sparse,Min,Max,C ++ D},
  cluster_merge([R|T]);
cluster_merge([{dense,Min,_,C},{dense,_,Max,D}|T]) when length(C) < ?MINFORJUMPTABLE, length(D) < ?MINFORJUMPTABLE ->
  R = {sparse,Min,Max,C ++ D},
  cluster_merge([R|T]);
cluster_merge([{dense,Min,_,D},{sparse,_,Max,C}|T]) when length(D) < ?MINFORJUMPTABLE ->
  R = {sparse,Min,Max,C ++ D},
  cluster_merge([R|T]);
cluster_merge([A,{dense,Min,Max,C}|T]) when length(C) >= ?MINFORJUMPTABLE ->
  R = cluster_merge([{dense,Min,Max,C}|T]),
  [A|R].


%% Generate code to search for the correct cluster

find_cluster([{sparse,_Min,_Max,C}],VarMap,ConstTab,Options,Arg,Fail,_IcodeFail) ->
  case length(C) < ?MINFORINTSEARCHTREE of
    true ->
      gen_small_switch_val(Arg,C,Fail,VarMap,ConstTab,Options);
    _ ->
      gen_search_switch_val(Arg,C,Fail,VarMap,ConstTab,Options)
  end;
find_cluster([{dense,Min,_Max,C}],VarMap,ConstTab,Options,Arg,Fail,IcodeFail) ->	
  case length(C) < ?MINFORJUMPTABLE of
    true ->
      gen_small_switch_val(Arg,C,Fail,VarMap,ConstTab,Options);
    _ ->
      gen_jump_table(Arg,Fail,IcodeFail,VarMap,ConstTab,C,Min)
  end;
find_cluster([{Density,Min,Max,C}|T],VarMap,ConstTab,Options,Arg,Fail,IcodeFail) ->
  ClustLab = hipe_rtl:mk_new_label(),
  NextLab = hipe_rtl:mk_new_label(),
  {ClustCode,V1,C1} = find_cluster([{Density,Min,Max,C}],VarMap,ConstTab,Options,Arg,Fail,IcodeFail),

  {Rest,V2,C2} = find_cluster(T,V1,C1,Options,Arg,Fail,IcodeFail),
  
  {[
    hipe_rtl:mk_branch(Arg, gt, hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(Max)),
		       hipe_rtl:label_name(NextLab), 
		       hipe_rtl:label_name(ClustLab), 0.50),	
    ClustLab
   ] ++	
   ClustCode ++
   [NextLab] ++
   Rest,
   V2,C2}.

%% Generate efficient code for a linear search through the case list.
%% Only works for atoms and integer.
gen_linear_switch_val(Arg,Cases,Fail,VarMap,ConstTab,_Options) ->
  {Values,_Labels} = split_cases(Cases),
  {LabMap,VarMap1} = lbls_from_cases(Cases,VarMap),
  Code = fast_linear_search(Arg,Values,LabMap,Fail),
  {Code,VarMap1,ConstTab}.

fast_linear_search(_Arg,[],[],Fail) ->
  [hipe_rtl:mk_goto(hipe_rtl:label_name(Fail))];
fast_linear_search(Arg,[Case|Cases],[Label|Labels],Fail) ->
  Reg = hipe_rtl:mk_new_reg_gcsafe(),
  NextLab = hipe_rtl:mk_new_label(),
  C2 = fast_linear_search(Arg,Cases,Labels,Fail),
  C1 =
    if
      is_integer(Case) ->
	TVal = hipe_tagscheme:mk_fixnum(Case),
	[
	 hipe_rtl:mk_move(Reg,hipe_rtl:mk_imm(TVal)),
	 hipe_rtl:mk_branch(Arg,eq,Reg,
			    Label,
			    hipe_rtl:label_name(NextLab), 0.5),
	 NextLab
	];
      is_atom(Case) ->
	[
	 hipe_rtl:mk_load_atom(Reg,Case),
	 hipe_rtl:mk_branch(Arg,eq,Reg,
			    Label,
			    hipe_rtl:label_name(NextLab), 0.5),
	 NextLab
	];
      true ->   % This should never happen !
	?EXIT({internal_error_in_switch_val,Case})
    end,
  [C1,C2].


%% Generate code to search through a small cluster of integers using
%% binary search
gen_small_switch_val(Arg,Cases,Fail,VarMap,ConstTab,_Options) -> 
  {Values,_Labels} = split_cases(Cases),
  {LabMap,VarMap1} = lbls_from_cases(Cases,VarMap),
  Keys = [hipe_tagscheme:mk_fixnum(X)    % Add tags to the values
	  || X <- Values],
  Code = inline_search(Keys, LabMap, Arg, Fail),
  {Code, VarMap1, ConstTab}.


%% Generate code to search through a small cluster of atoms
gen_atom_switch_val(Arg,Cases,Fail,VarMap,ConstTab,_Options) -> 
  {Values, _Labels} = split_cases(Cases),
  {LabMap,VarMap1} = lbls_from_cases(Cases,VarMap),
  LMap = [{label,L} || L <- LabMap],
  {NewConstTab,Id} = hipe_consttab:insert_sorted_block(ConstTab, Values),
  {NewConstTab2,LabId} =
    hipe_consttab:insert_sorted_block(NewConstTab, word, LMap, Values),
  Code = inline_atom_search(0, length(Cases)-1, Id, LabId, Arg, Fail, LabMap),
  {Code, VarMap1, NewConstTab2}.


%% calculate the middle position of a list (+ 1 because of 1-indexing of lists)
get_middle(List) ->
  N = length(List),
  N div 2 + 1.

%% get element [N1, N2] from a list
get_cases(_, 0, 0) ->
  [];
get_cases([H|T], 0, N) ->
  [H | get_cases(T, 0, N - 1)];
get_cases([_|T], N1, N2) ->
  get_cases(T, N1 - 1, N2 - 1).


%% inline_search/4 creates RTL code for a inlined binary search.
%% It requires two sorted tables - one with the keys to search
%% through and one with the corresponding labels to jump to.
%%    
%% Input: 
%%  KeyList - A list of keys to search through. 
%%  LableList - A list of labels to jump to.
%%  KeyReg - A register containing the key to search for.
%%  Default - A label to jump to if the key is not found.
%%

inline_search([], _LabelList, _KeyReg, _Default) -> [];
inline_search(KeyList, LabelList, KeyReg, Default) ->
  %% Create some registers and labels that we need.
  Reg = hipe_rtl:mk_new_reg_gcsafe(),
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  Lab3 = hipe_rtl:mk_new_label(),
  
  Length = length(KeyList),
  
  if
    Length >= 3 ->
      %% Get middle element and keys/labels before that and after
      Middle_pos = get_middle(KeyList),
      Middle_key = lists:nth(Middle_pos, KeyList),
      Keys_beginning = get_cases(KeyList, 0, Middle_pos - 1),
      Labels_beginning = get_cases(LabelList, 0, Middle_pos - 1),
      Keys_ending = get_cases(KeyList, Middle_pos, Length),
      Labels_ending = get_cases(LabelList, Middle_pos, Length),
      
      %% Create the code.
      
      %% Get the label and build it up properly
      Middle_label = lists:nth(Middle_pos, LabelList),
      
      A = [hipe_rtl:mk_move(Reg, hipe_rtl:mk_imm(Middle_key)),
	   hipe_rtl:mk_branch(KeyReg, lt, Reg, 
			      hipe_rtl:label_name(Lab2), 
			      hipe_rtl:label_name(Lab1), 0.5),
	   Lab1,
	   hipe_rtl:mk_branch(KeyReg, gt, Reg,
			      hipe_rtl:label_name(Lab3), 
			      Middle_label , 0.5),
	   Lab2],
      %% build search tree for keys less than the middle element
      B = inline_search(Keys_beginning, Labels_beginning, KeyReg, Default),
      %% ...and for keys bigger than the middle element
      D = inline_search(Keys_ending, Labels_ending, KeyReg, Default),
      
      %% append the code and return it
      A ++ B ++ [Lab3] ++ D;
    
    Length =:= 2 ->
      %% get the first and second elements and theirs labels
      Key_first = hd(KeyList),
      First_label = hd(LabelList),
      
      %% Key_second = hipe_tagscheme:mk_fixnum(lists:nth(2, KeyList)),
      Key_second = lists:nth(2, KeyList),
      Second_label = lists:nth(2, LabelList),
      
      NewLab = hipe_rtl:mk_new_label(),
      
      %% compare them
      A = [hipe_rtl:mk_move(Reg,hipe_rtl:mk_imm(Key_first)),
	   hipe_rtl:mk_branch(KeyReg, eq, Reg,
			      First_label, 
			      hipe_rtl:label_name(NewLab) , 0.5),
	   NewLab],
      
      B = [hipe_rtl:mk_move(Reg,hipe_rtl:mk_imm(Key_second)),
	   hipe_rtl:mk_branch(KeyReg, eq, Reg,
			      Second_label, 
			      hipe_rtl:label_name(Default) , 0.5)],
      A ++ B;
    
    Length =:= 1 ->
      Key = hd(KeyList),
      Label = hd(LabelList),
      
      [hipe_rtl:mk_move(Reg,hipe_rtl:mk_imm(Key)),
       hipe_rtl:mk_branch(KeyReg, eq, Reg,
			  Label, 
			  hipe_rtl:label_name(Default) , 0.5)]
  end.


inline_atom_search(Start, End, Block, LBlock, KeyReg, Default, Labels) ->
  Reg = hipe_rtl:mk_new_reg_gcsafe(),
  
  Length = (End - Start) + 1,
  
  if
    Length >= 3 ->
      Lab1 = hipe_rtl:mk_new_label(),
      Lab2 = hipe_rtl:mk_new_label(),
      Lab3 = hipe_rtl:mk_new_label(),
      Lab4 = hipe_rtl:mk_new_label(),
      
      Mid = ((End-Start) div 2)+Start,
      End1 = Mid-1,
      Start1 = Mid+1,
      A = [
	   hipe_rtl:mk_load_word_index(Reg,Block,Mid),
	   hipe_rtl:mk_branch(KeyReg, lt, Reg,
			      hipe_rtl:label_name(Lab2),
			      hipe_rtl:label_name(Lab1), 0.5),
	   Lab1,
	   hipe_rtl:mk_branch(KeyReg, gt, Reg,
			      hipe_rtl:label_name(Lab3),
			      hipe_rtl:label_name(Lab4), 0.5),
	   Lab4,
	   hipe_rtl:mk_goto_index(LBlock, Mid, Labels),
	   Lab2
	  ],
      B = [inline_atom_search(Start,End1,Block,LBlock,KeyReg,Default,Labels)],
      C = [inline_atom_search(Start1,End,Block,LBlock,KeyReg,Default,Labels)],
      A ++ B ++ [Lab3] ++ C;
    
    Length =:= 2 ->
      L1 = hipe_rtl:mk_new_label(),
      L2 = hipe_rtl:mk_new_label(),
      L3 = hipe_rtl:mk_new_label(),
      [
       hipe_rtl:mk_load_word_index(Reg,Block,Start),
       hipe_rtl:mk_branch(KeyReg,eq,Reg,
			  hipe_rtl:label_name(L1),
			  hipe_rtl:label_name(L2), 0.5),
       L1,
       hipe_rtl:mk_goto_index(LBlock,Start,Labels),
       
       L2,
       hipe_rtl:mk_load_word_index(Reg,Block,End),
       hipe_rtl:mk_branch(KeyReg,eq,Reg,
			  hipe_rtl:label_name(L3),
			  hipe_rtl:label_name(Default), 0.5),
       L3,
       hipe_rtl:mk_goto_index(LBlock, End, Labels)
      ];
    
    Length =:= 1 ->
      NewLab = hipe_rtl:mk_new_label(),
      [
       hipe_rtl:mk_load_word_index(Reg,Block,Start),
       hipe_rtl:mk_branch(KeyReg, eq, Reg,
			  hipe_rtl:label_name(NewLab),
			  hipe_rtl:label_name(Default), 0.9),
       NewLab,
       hipe_rtl:mk_goto_index(LBlock, Start, Labels)
      ]
  end.


%% Create a jumptable
gen_jump_table(Arg,Fail,IcodeFail,VarMap,ConstTab,Cases,Min) ->
  %% Map is a rtl mapping of Dense
  {Max,DenseTbl} = dense_interval(Cases,Min,IcodeFail),
  {Map,VarMap2} = lbls_from_cases(DenseTbl,VarMap),
  
  %% Make some labels and registers that we need.
  BelowLab = hipe_rtl:mk_new_label(),
  UntaggedR = hipe_rtl:mk_new_reg_gcsafe(),
  StartR = hipe_rtl:mk_new_reg_gcsafe(),
  
  %% Generate the code to do the switch...
  {[
    %% Untag the index.
    hipe_tagscheme:untag_fixnum(UntaggedR, Arg)|
    %% Check that the index is within Min and Max.
    case Min of
      0 -> %% First element is 0 this is simple.
	[hipe_rtl:mk_branch(UntaggedR, gtu, hipe_rtl:mk_imm(Max),
			    hipe_rtl:label_name(Fail), 
			    hipe_rtl:label_name(BelowLab), 0.01),
	 BelowLab,
	 %% StartR contains the index into the jumptable
	 hipe_rtl:mk_switch(UntaggedR, Map)];
      _ -> %% First element is not 0 
	[hipe_rtl:mk_alu(StartR, UntaggedR, sub, 
			 hipe_rtl:mk_imm(Min)), 
	 hipe_rtl:mk_branch(StartR, gtu, hipe_rtl:mk_imm(Max-Min),
			    hipe_rtl:label_name(Fail), 
			    hipe_rtl:label_name(BelowLab), 0.01),
	 BelowLab,
	 %% StartR contains the index into the jumptable
	 hipe_rtl:mk_switch(StartR, Map)]
    end],
   VarMap2,
   ConstTab}.


%% Generate the jumptable for Cases while filling in unused positions
%% with the fail label

dense_interval(Cases, Min, IcodeFail) ->
  dense_interval(Cases, Min, IcodeFail, 0, 0).
dense_interval([Pair = {Const,_}|Rest], Pos, Fail, Range, NoEntries) ->
  Val = hipe_icode:const_value(Const),
  if 
    Pos < Val ->
      {Max, Res} = 
	dense_interval([Pair|Rest], Pos+1, Fail, Range+1, NoEntries),
      {Max,[{hipe_icode:mk_const(Pos), Fail}|Res]};
    true ->
      {Max, Res} = dense_interval(Rest, Pos+1, Fail, Range+1, NoEntries+1),
      {Max, [Pair | Res]}
  end;
dense_interval([], Max, _, _, _) -> 
  {Max-1, []}.


%%-------------------------------------------------------------------------
%% switch_val without jumptable
%%

gen_slow_switch_val(I, VarMap, ConstTab, Options) ->
  Is = rewrite_switch_val(I),
  ?IF_DEBUG_LEVEL(3,?msg("Switch: ~w\n", [Is]), no_debug),
  hipe_icode2rtl:translate_instrs(Is, VarMap, ConstTab, Options).

rewrite_switch_val(I) ->
  Var = hipe_icode:switch_val_term(I),
  Fail = hipe_icode:switch_val_fail_label(I),
  Cases = hipe_icode:switch_val_cases(I),
  rewrite_switch_val_cases(Cases, Fail, Var).

rewrite_switch_val_cases([{C,L}|Cases], Fail, Arg) ->
  Tmp = hipe_icode:mk_new_var(),
  NextLab = hipe_icode:mk_new_label(),
  [hipe_icode:mk_move(Tmp, C),
   hipe_icode:mk_if(op_exact_eqeq_2, [Arg, Tmp], L,
		    hipe_icode:label_name(NextLab)),
   NextLab |
   rewrite_switch_val_cases(Cases, Fail, Arg)];
rewrite_switch_val_cases([], Fail, _Arg) ->
  [hipe_icode:mk_goto(Fail)].


%%-------------------------------------------------------------------------
%% switch_val with binary search jumptable
%%

gen_search_switch_val(Arg, Cases, Default, VarMap, ConstTab, _Options) ->
  ValTableR = hipe_rtl:mk_new_reg_gcsafe(),

  {Values,_Labels} = split_cases(Cases),
  {NewConstTab,Id} = hipe_consttab:insert_sorted_block(ConstTab, Values),
  {LabMap,VarMap1} = lbls_from_cases(Cases,VarMap),
  
  Code = 
    [hipe_rtl:mk_load_address(ValTableR, Id, constant)|
     tab(Values,LabMap,Arg,ValTableR,Default)],
  {Code, VarMap1, NewConstTab}.


%%-------------------------------------------------------------------------
%%
%% tab/5 creates RTL code for a binary search.
%% It requires two sorted tables one with the keys to search
%% through and one with the corresponding labels to jump to.
%%
%% The implementation is derived from John Bentlys
%% Programming Pearls.
%%
%% Input: 
%%  KeyList - A list of keys to search through. 
%%           (Just used to calculate the number of elements.)
%%  LableList - A list of labels to jump to.
%%  KeyReg - A register containing the key to search for.
%%  TablePntrReg - A register containing a pointer to the
%%                tables with keys
%%  Default - A lable to jump to if the key is not found.
%%
%% Example:
%%  KeyTbl: < a, b, d, f, h, i, z >
%%  Lbls:   < 5, 3, 2, 4, 1, 7, 6 >
%%  Default: 8
%%  KeyReg: v37
%%  TablePntrReg: r41 
%%
%%  should give code like:
%%    r41 <- KeyTbl
%%    r42 <- 0
%%    r43 <- [r41+16]
%%    if (r43 gt v37) then L17 (0.50) else L16
%% L16:
%%    r42 <- 16
%%    goto L17
%% L17: 
%%    r46 <- r42 add 16
%%    r45 <- [r41+r46]
%%    if (r45 gt v37) then L21 (0.50) else L20
%% L20: 
%%    r42 <- r46
%%    goto L21
%% L21: 
%%    r48 <- r42 add 8
%%    r47 <- [r41+r48]
%%    if (r47 gt v37) then L23 (0.50) else L22
%% L22: 
%%    r42 <- r48
%%    goto L23
%% L23: 
%%    r50 <- r42 add 4
%%    r49 <- [r41+r50]
%%    if (r49 gt v37) then L25 (0.50) else L24
%% L24: 
%%    r42 <- r42 add 4
%%    goto L25
%% L25: 
%%    if (r42 gt 28) then L6 (0.50) else L18
%% L18: 
%%    r44 <- [r41+r42]
%%    if (r44 eq v37) then L19 (0.90) else L8
%% L19: 
%%    r42 <- r42 sra 2
%%    switch (r42) <L5, L3, L2, L4, L1, 
%%              L7, L6>

%%   
%% The search is done like a rolled out binary search,
%% but instead of starting in the middle we start at 
%% the power of two closest above the middle.
%%
%% We let IndexReg point to the lower bound of our
%% search, and then we speculatively look at a 
%% position at IndexReg + I where I is a power of 2.
%%
%% Example: Looking for 'h' in 
%%  KeyTbl: < a, b, d, f, h, i, z >
%%
%%  We start with IndexReg=0 and I=4
%%          < a, b, d, f, h, i, z >
%%            ^        ^
%%     IndexReg    +   I
%%
%%  'f' < 'h' so we add I to IndexReg and divide I with 2
%%  IndexReg=4 and I=2
%%          < a, b, d, f, h, i, z >
%%                     ^     ^
%%              IndexReg +  I
%%
%%  'i' > 'h' so we keep IndexReg and divide I with 2
%%  IndexReg=4 and I=1
%%          < a, b, d, f, h, i, z >
%%                     ^  ^
%%              IndexReg+ I
%%  Now we have found 'h' so we add I to IndexReg -> 5
%%  And we can load switch to the label at position 5 in
%%  the label table.
%%
%%  Now since the wordsize is 4 all numbers above are 
%%  Multiples of 4.

tab(KeyList, LabelList, KeyReg, TablePntrReg, Default) ->
  %% Calculate the size of the table: 
  %%  the number of keys * wordsize
  LastOffset = (length(KeyList)-1)*?WORDSIZE,

  %% Calculate the power of two closest to the size of the table.
  Pow2 = 1 bsl trunc(math:log(LastOffset) / math:log(2)),

  %% Create some registers and lables that we need
  IndexReg = hipe_rtl:mk_new_reg_gcsafe(),
  Temp = hipe_rtl:mk_new_reg_gcsafe(),
  Temp2 = hipe_rtl:mk_new_reg_gcsafe(),
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  Lab3 = hipe_rtl:mk_new_label(),
  Lab4 = hipe_rtl:mk_new_label(),
  
  %% Calculate the position to start looking at
  Init = (LastOffset)-Pow2,

  %% Create the code
  [
   hipe_rtl:mk_move(IndexReg,hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_load(Temp,TablePntrReg,hipe_rtl:mk_imm(Init)),
   hipe_rtl:mk_branch(Temp, geu, KeyReg,
		      hipe_rtl:label_name(Lab2), 
		      hipe_rtl:label_name(Lab1), 0.5),
   Lab1,
   hipe_rtl:mk_alu(IndexReg, IndexReg, add, hipe_rtl:mk_imm(Init+?WORDSIZE)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(Lab2)),
   Lab2] ++

    step(Pow2 div 2, TablePntrReg, IndexReg, KeyReg) ++

    [hipe_rtl:mk_branch(IndexReg, gt, hipe_rtl:mk_imm(LastOffset),
		       hipe_rtl:label_name(Default), 
		       hipe_rtl:label_name(Lab3), 0.5),
     Lab3,
     hipe_rtl:mk_load(Temp2,TablePntrReg,IndexReg),
     hipe_rtl:mk_branch(Temp2, eq, KeyReg,
			hipe_rtl:label_name(Lab4), 
			hipe_rtl:label_name(Default), 0.9),
     Lab4,
     hipe_rtl:mk_alu(IndexReg, IndexReg, sra,
                     hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
     hipe_rtl:mk_sorted_switch(IndexReg, LabelList, KeyList)
    ].

step(I,TablePntrReg,IndexReg,KeyReg) ->
  Temp = hipe_rtl:mk_new_reg_gcsafe(),
  TempIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(TempIndex, IndexReg, add, hipe_rtl:mk_imm(I)),
   hipe_rtl:mk_load(Temp,TablePntrReg,TempIndex),
   hipe_rtl:mk_branch(Temp, gtu, KeyReg,
                      hipe_rtl:label_name(Lab2), 
                      hipe_rtl:label_name(Lab1) , 0.5),
   Lab1] ++
    case ?WORDSIZE of
      I -> %% Recursive base case
        [hipe_rtl:mk_alu(IndexReg, IndexReg, add, hipe_rtl:mk_imm(I)),
         hipe_rtl:mk_goto(hipe_rtl:label_name(Lab2)),
         Lab2
        ];
      _ -> %% Recursion case
        [hipe_rtl:mk_move(IndexReg, TempIndex),
         hipe_rtl:mk_goto(hipe_rtl:label_name(Lab2)),
         Lab2
         | step(I div 2, TablePntrReg, IndexReg, KeyReg)
        ]
    end.

%%-------------------------------------------------------------------------

lbls_from_cases([{_,L}|Rest], VarMap) ->
  {Map,VarMap1} = lbls_from_cases(Rest, VarMap),
  {RtlL, VarMap2} = hipe_rtl_varmap:icode_label2rtl_label(L,VarMap1),
  %%  {[{label,hipe_rtl:label_name(RtlL)}|Map],VarMap2};
  {[hipe_rtl:label_name(RtlL)|Map],VarMap2};
lbls_from_cases([], VarMap) ->
  {[], VarMap}.

%%-------------------------------------------------------------------------

split_cases(L) -> 
  split_cases(L, [], []).

split_cases([], Vs, Ls) -> {lists:reverse(Vs),lists:reverse(Ls)};
split_cases([{V,L}|Rest], Vs, Ls) ->
  split_cases(Rest, [hipe_icode:const_value(V)|Vs], [L|Ls]).

%%-------------------------------------------------------------------------
%%
%% {switch_tuple_arity,X,Fail,N,[{A1,L1},...,{AN,LN}]}
%%
%% if not boxed(X) goto Fail
%% Hdr := *boxed_val(X)
%% switch_int(Hdr,Fail,[{H(A1),L1},...,{H(AN),LN}])
%% where H(Ai) = make_arityval(Ai)
%% 
%%-------------------------------------------------------------------------

gen_switch_tuple(I, Map, ConstTab, _Options) ->
  Var = hipe_icode:switch_tuple_arity_term(I),
  {X, Map1} = hipe_rtl_varmap:icode_var2rtl_var(Var, Map),
  Fail0 = hipe_icode:switch_tuple_arity_fail_label(I),
  {Fail1, Map2} = hipe_rtl_varmap:icode_label2rtl_label(Fail0, Map1),
  FailLab = hipe_rtl:label_name(Fail1),
  {Cases, Map3} =
    lists:foldr(fun({A,L}, {Rest,M}) ->
		    {L1,M1} = hipe_rtl_varmap:icode_label2rtl_label(L, M),
		    L2 = hipe_rtl:label_name(L1),
		    A1 = hipe_icode:const_value(A),
		    H1 = hipe_tagscheme:mk_arityval(A1),
		    {[{H1,L2}|Rest], M1} end,
		{[], Map2},
		hipe_icode:switch_tuple_arity_cases(I)),
  Hdr = hipe_rtl:mk_new_reg_gcsafe(),
  IsBoxedLab = hipe_rtl:mk_new_label(),
  {[hipe_tagscheme:test_is_boxed(X, hipe_rtl:label_name(IsBoxedLab),
				 FailLab, 0.9),
    IsBoxedLab,
    hipe_tagscheme:get_header(Hdr, X) |
    gen_switch_int(Hdr, FailLab, Cases)],
   Map3, ConstTab}.

%%
%% RTL-level switch-on-int
%%

gen_switch_int(X, FailLab, [{C,L}|Rest]) ->
  NextLab = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_branch(X, eq, hipe_rtl:mk_imm(C), L,
		      hipe_rtl:label_name(NextLab), 0.5),
   NextLab |
   gen_switch_int(X, FailLab, Rest)];
gen_switch_int(_, FailLab, []) ->
  [hipe_rtl:mk_goto(FailLab)].

