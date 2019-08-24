%%% -*- erlang-indent-level: 2 -*-
%%%======================================================================
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% An implementation of the algorithm described in:
%%% "Assembling Code for Machines with Span-Dependent Instructions",
%%% Thomas G. Szymanski, CACM 21(4), April 1978, pp. 300--308.
%%%
%%% Copyright (C) 2000, 2004, 2007  Mikael Pettersson

-module(hipe_sdi).
-export([pass1_init/0,
	 pass1_add_label/3,
	 pass1_add_sdi/4,
	 pass2/1]).

-include("hipe_sdi.hrl").

%%------------------------------------------------------------------------

-type hipe_array() :: integer(). % declare this in hipe.hrl or builtin?
-type hipe_vector(E) :: {} | {E} | {E, E} | {E, E, E} | tuple().

-type label()      :: non_neg_integer().
-type address()    :: non_neg_integer().

-type parents()    :: {hipe_vector(_ :: integer()), hipe_segment_trees:tree()}.

%%------------------------------------------------------------------------

-record(label_data, {address :: address(),
		     prevSdi :: integer()}).

-record(pre_sdi_data, {address :: address(),
		       label   :: label(),
		       si      :: #sdi_info{}}).

-record(pass1, {prevSdi			    :: integer(),
		preS	 = []		    :: [#pre_sdi_data{}],
		labelMap = gb_trees:empty() :: gb_trees:tree()}).

-record(sdi_data, {address       :: address(),
		   label_address :: address(),
		   prevSdi       :: integer(), %% -1 is the first previous
		   si            :: #sdi_info{}}).

%%------------------------------------------------------------------------

%%% "During the first pass we assign addresses to instructions
%%% and build a symbol table of labels and their addresses
%%% according to the minimum address assignment. We do this by
%%% treating each sdi as having its shorter length. We also
%%% number the sdi's [sic] from 1 to n in order of occurrence
%%% and record in the symbol table entry for each label the
%%% number of sdi's [sic] preceding it in the program.
%%% Simultaneously with pass 1 we build a set
%%% S = {(i,a,l,c) | 1 <= i <= n, a is the minimum address of
%%%	 the ith sdi, l and c, are the label and constant
%%%	 components of the operand of the ith sdi respectively}."
%%%
%%% Implementation notes:
%%% - We number the SDIs from 0 to n-1, not from 1 to n.
%%% - SDIs target only labels, so the constant offsets are omitted.
%%% - The set S is represented by a vector S[0..n-1] such that if
%%%   (i,a,l) is in the set, then S[i] = (a,l).
%%% - The symbol table maps a label to its minimum address and the
%%%   number of the last SDI preceding it (-1 if none).
%%% - To allow this module to make architecture-specific decisions
%%%   without using callbacks or making it architecture-specific,
%%%   the elements in the set S include a fourth component, SdiInfo,
%%%   supplied by the caller of this module.
%%% - At the end of the first pass we finalise the preliminary SDIs
%%%   by replacing their symbolic target labels with the corresponding
%%%   data from the symbol table. This avoids repeated O(logn) time
%%%   lookup costs for the labels.

-spec pass1_init() -> #pass1{}.
pass1_init() ->
  #pass1{prevSdi = -1}.

-spec pass1_add_label(#pass1{}, non_neg_integer(), label()) -> #pass1{}.
pass1_add_label(Pass1, Address, Label) ->
  #pass1{prevSdi=PrevSdi, labelMap=LabelMap} = Pass1,
  LabelData = #label_data{address=Address, prevSdi=PrevSdi},
  LabelMap2 = gb_trees:insert(Label, LabelData, LabelMap),
  Pass1#pass1{labelMap=LabelMap2}.

-spec pass1_add_sdi(#pass1{}, non_neg_integer(), label(), #sdi_info{}) ->
	#pass1{}.
pass1_add_sdi(Pass1, Address, Label, SdiInfo) ->
  #pass1{prevSdi=PrevSdi, preS=PreS} = Pass1,
  PreSdiData = #pre_sdi_data{address=Address, label=Label, si=SdiInfo},
  Pass1#pass1{prevSdi=PrevSdi+1, preS=[PreSdiData|PreS]}.

-spec pass1_finalise(#pass1{}) -> {non_neg_integer(),tuple(),gb_trees:tree()}.
pass1_finalise(#pass1{prevSdi=PrevSdi, preS=PreS, labelMap=LabelMap}) ->
  {PrevSdi+1, pass1_finalise_preS(PreS, LabelMap, []), LabelMap}.

-spec pass1_finalise_preS([#pre_sdi_data{}], gb_trees:tree(), [#sdi_data{}]) ->
	tuple().
pass1_finalise_preS([], _LabelMap, S) -> vector_from_list(S);
pass1_finalise_preS([PreSdiData|PreS], LabelMap, S) ->
  #pre_sdi_data{address=Address, label=Label, si=SdiInfo} = PreSdiData,
  LabelData = gb_trees:get(Label, LabelMap),
  #label_data{address=LabelAddress, prevSdi=PrevSdi} = LabelData,
  SdiData = #sdi_data{address=Address, label_address=LabelAddress,
		      prevSdi=PrevSdi, si=SdiInfo},
  pass1_finalise_preS(PreS, LabelMap, [SdiData|S]).

%%% Pass2.

-spec pass2(#pass1{}) -> {gb_trees:tree(), non_neg_integer()}.
pass2(Pass1) ->
  {N,SDIS,LabelMap} = pass1_finalise(Pass1),
  LONG = mk_long(N),
  SPAN = mk_span(N, SDIS),
  PARENTS = mk_parents(N, SDIS),
  update_long(N, SDIS, SPAN, PARENTS, LONG),
  {INCREMENT,CodeSizeIncr} = mk_increment(N, LONG),
  {adjust_label_map(LabelMap, INCREMENT), CodeSizeIncr}.

%%% "Between passes 1 and 2 we will construct an integer table
%%% LONG[1:n] such that LONG[i] is nonzero if and only if the
%%% ith sdi must be given a long form translation. Initially
%%% LONG[i] is zero for all i."
%%%
%%% Implementation notes:
%%% - LONG is an integer array indexed from 0 to N-1.

-spec mk_long(non_neg_integer()) -> hipe_array().
mk_long(N) ->
  mk_array_of_zeros(N).

%%% "At the heart of our algorithm is a graphical representation
%%% of the interdependencies of the sdi's [sic] of the program.
%%% For each sdi we construct a node containing the empty span
%%% of that instruction. Nodes of this graph will be referred to
%%% by the number of the sdi to which they correspond. Directed
%%% arcs are now added to the graph so that i->j is an arc if
%%% and only if the span of the ith sdi depends on the size of
%%% the jth sdi, that is, the jth sdi lies between the ith sdi
%%% and the label occurring in its operand. It is easy to see
%%% that the graph we have just described can be constructed from
%%% the information present in the set S and the symbol table.
%%%
%%% The significance if this graph is that sizes can be assigned
%%% to the sdi's [sic] of the program so that the span of the ith
%%% sdi is equal to the number appearing in node i if and only if
%%% all the children of i can be given short translations."
%%%
%%% Implementation notes:
%%% - The nodes are represented by an integer array SPAN[0..n-1]
%%%   such that SPAN[i] contains the current span of sdi i.
%%% - Since the graph is traversed from child to parent nodes in
%%%   Step 3, the edges are represented by a vector PARENTS[0..n-1]
%%%   such that PARENTS[j] = { i | i is a parent of j }.
%%% - An explicit PARENTS graph would have size O(n^2). Instead, we
%%%   observe that (i is a parent of j) iff (j \in range(i)), where
%%%   range(i) is a constant function. We can thus precompute all the
%%%   ranges i and insert them into a data structure built for such
%%%   queries. In this case, we use a segment tree.

-spec mk_span(non_neg_integer(), tuple()) -> hipe_array().
mk_span(N, SDIS) ->
  initSPAN(0, N, SDIS, mk_array_of_zeros(N)).

-spec initSPAN(non_neg_integer(), non_neg_integer(),
	       tuple(), hipe_array()) -> hipe_array().
initSPAN(SdiNr, N, SDIS, SPAN) ->
  if SdiNr >= N -> SPAN;
     true ->
      SdiData = vector_sub(SDIS, SdiNr),
      #sdi_data{address=SdiAddress, label_address=LabelAddress} = SdiData,
      SdiSpan = LabelAddress - SdiAddress,
      array_update(SPAN, SdiNr, SdiSpan),
      initSPAN(SdiNr+1, N, SDIS, SPAN)
  end.

-spec mk_parents(non_neg_integer(), tuple()) -> parents().
mk_parents(N, SDIS) ->
  PrevSDIS = vector_from_list(select_prev_sdis(N-1, SDIS, [])),
  Ranges = parents_generate_ranges(N-1, PrevSDIS, []),
  {PrevSDIS, hipe_segment_trees:build(Ranges)}.

select_prev_sdis(-1, _SDIS, Acc) -> Acc;
select_prev_sdis(SdiNr, SDIS, Acc) ->
  #sdi_data{prevSdi=PrevSdi} = vector_sub(SDIS, SdiNr),
  select_prev_sdis(SdiNr-1, SDIS, [PrevSdi|Acc]).

parents_generate_ranges(-1, _PrevSDIS, Acc) -> Acc;
parents_generate_ranges(SdiNr, PrevSDIS, Acc) ->
  %% inclusive
  {LO,HI} = parents_generate_range(SdiNr, PrevSDIS),
  parents_generate_ranges(SdiNr-1, PrevSDIS, [{LO,HI}|Acc]).

-compile({inline, parents_generate_range/2}).
parents_generate_range(SdiNr, PrevSDIS) ->
  PrevSdi = vector_sub(PrevSDIS, SdiNr),
  if SdiNr =< PrevSdi -> {SdiNr+1, PrevSdi};	% forwards
     true -> {PrevSdi+1, SdiNr-1}		% backwards
  end.

%%% "After the structure is built we process it as follows.
%%% For any node i whose listed span exceeds the architectural
%%% limit for a short form instruction, the LONG[i] equal to
%%% the difference between the long and short forms of the ith
%%% sdi. Increment the span of each parent of i by LONG[i] if
%%% the parent precedes the child in the program. Otherwise,
%%% decrement the span of the parent by LONG[i]. Finally, remove
%%% node i from the graph. Clearly this process must terminate.
%%% Any nodes left in the final graph correspond to sdi's [sic]
%%% which can be translated in the short form."
%%%
%%% Implementation notes:
%%% - We use a simple worklist algorithm, operating on a set
%%%   of SDIs known to require long form.
%%% - A node is removed from the graph by setting its span to zero.
%%% - The result is the updated LONG array. Afterwards, S, SPAN,
%%%   and PARENTS are no longer useful.

-spec update_long(non_neg_integer(), tuple(), hipe_array(),
		  parents(),hipe_array()) -> 'ok'.
update_long(N, SDIS, SPAN, PARENTS, LONG) ->
  WKL = initWKL(N-1, SDIS, SPAN, []),
  processWKL(WKL, SDIS, SPAN, PARENTS, LONG).

-spec initWKL(integer(), tuple(),
	      hipe_array(), [non_neg_integer()]) -> [non_neg_integer()].
initWKL(SdiNr, SDIS, SPAN, WKL) ->
  if SdiNr < 0 -> WKL;
     true ->
      SdiSpan = array_sub(SPAN, SdiNr),
      WKL2 = updateWKL(SdiNr, SDIS, SdiSpan, WKL),
      initWKL(SdiNr-1, SDIS, SPAN, WKL2)
  end.

-spec processWKL([non_neg_integer()], tuple(), hipe_array(),
		 parents(), hipe_array()) -> 'ok'.
processWKL([], _SDIS, _SPAN, _PARENTS, _LONG) -> ok;
processWKL([Child|WKL], SDIS, SPAN, PARENTS0, LONG) ->
  {WKL2, PARENTS} =
    case array_sub(SPAN, Child) of
      0 -> {WKL, PARENTS0};				% removed
      _ ->
	SdiData = vector_sub(SDIS, Child),
	Incr = sdiLongIncr(SdiData),
	array_update(LONG, Child, Incr),
	array_update(SPAN, Child, 0),			% remove child
	PARENTS1 = deleteParent(PARENTS0, Child),
	PS = parentsOfChild(PARENTS1, Child),
	{updateParents(PS, Child, Incr, SDIS, SPAN, WKL), PARENTS1}
    end,
  processWKL(WKL2, SDIS, SPAN, PARENTS, LONG).

-spec parentsOfChild(parents(), non_neg_integer()) -> [non_neg_integer()].
parentsOfChild({_PrevSDIS, SegTree}, Child) ->
  hipe_segment_trees:intersect(Child, SegTree).

-spec deleteParent(parents(), non_neg_integer()) -> parents().
deleteParent({PrevSDIS, SegTree0}, Parent) ->
  {LO,HI} = parents_generate_range(Parent, PrevSDIS),
  SegTree = hipe_segment_trees:delete(Parent, LO, HI, SegTree0),
  {PrevSDIS, SegTree}.

-spec updateParents([non_neg_integer()], non_neg_integer(),
		    byte(), tuple(), hipe_array(),
		    [non_neg_integer()]) -> [non_neg_integer()].
updateParents([], _Child, _Incr, _SDIS, _SPAN, WKL) -> WKL;
updateParents([P|PS], Child, Incr, SDIS, SPAN, WKL) ->
  WKL2 = updateParent(P, Child, Incr, SDIS, SPAN, WKL),
  updateParents(PS, Child, Incr, SDIS, SPAN, WKL2).

-spec updateParent(non_neg_integer(), non_neg_integer(),
		   byte(), tuple(), hipe_array(),
		   [non_neg_integer()]) -> [non_neg_integer()].
updateParent(Parent, Child, Incr, SDIS, SPAN, WKL) ->
  case array_sub(SPAN, Parent) of
    0 -> WKL;						% removed
    OldSpan ->
      NewSpan =
	if Parent < Child -> OldSpan + Incr;
	   true -> OldSpan - Incr
	end,
      array_update(SPAN, Parent, NewSpan),
      updateWKL(Parent, SDIS, NewSpan, WKL)
  end.

-spec updateWKL(non_neg_integer(), tuple(),
		integer(), [non_neg_integer()]) -> [non_neg_integer()].
updateWKL(SdiNr, SDIS, SdiSpan, WKL) ->
  case sdiSpanIsShort(vector_sub(SDIS, SdiNr), SdiSpan) of
    true -> WKL;
    false -> [SdiNr|WKL]
  end.

-compile({inline, sdiSpanIsShort/2}). %% Only called once
-spec sdiSpanIsShort(#sdi_data{}, integer()) -> boolean().
sdiSpanIsShort(#sdi_data{si = #sdi_info{lb = LB, ub = UB}}, SdiSpan) ->
  SdiSpan >= LB andalso SdiSpan =< UB.

-compile({inline, sdiLongIncr/1}). %% Only called once
-spec sdiLongIncr(#sdi_data{}) -> byte().
sdiLongIncr(#sdi_data{si = #sdi_info{incr = Incr}}) -> Incr.

%%% "Now construct a table INCREMENT[0:n] by defining
%%% INCREMENT[0] = 0 and INCREMENT[i] = INCREMENT[i-1]+LONG[i]
%%% for 1 <= i <= n. INCREMENT[i] represents the total increase
%%% in size of the first i sdi's [sic] in the program."
%%%
%%% Implementation notes:
%%% - INCREMENT is an integer vector indexed from 0 to n-1.
%%%   INCREMENT[i] = SUM(0 <= j <= i)(LONG[j]), for 0 <= i < n.
%%% - Due to the lack of an SML-like Array.extract operation,
%%%   INCREMENT is an array, not an immutable vector.

-spec mk_increment(non_neg_integer(), hipe_array()) ->
			{hipe_array(), non_neg_integer()}.
mk_increment(N, LONG) ->
  initINCR(0, 0, N, LONG, mk_array_of_zeros(N)).

-spec initINCR(non_neg_integer(), non_neg_integer(), non_neg_integer(),
	       hipe_array(), hipe_array()) -> {hipe_array(), non_neg_integer()}.
initINCR(SdiNr, PrevIncr, N, LONG, INCREMENT) ->
  if SdiNr >= N -> {INCREMENT, PrevIncr};
     true ->
      SdiIncr = PrevIncr + array_sub(LONG, SdiNr),
      array_update(INCREMENT, SdiNr, SdiIncr),
      initINCR(SdiNr+1, SdiIncr, N, LONG, INCREMENT)
  end.

%%% "At this point we can adjust the addresses of each label L
%%% in the symbol table. If L is preceded by i sdi's [sic] in
%%% the program, then add INCREMENT[i] to the value of L in the
%%% symbol table."
%%%
%%% Implementation notes:
%%% - Due to the 0..n-1 SDI numbering, a label L with address
%%%   a and previous sdi i is remapped to a+incr(i), where
%%%   incr(i) = if i < 0 then 0 else INCREMENT[i].

-spec adjust_label_map(gb_trees:tree(), hipe_array()) -> gb_trees:tree().
adjust_label_map(LabelMap, INCREMENT) ->
  applyIncr(gb_trees:to_list(LabelMap), INCREMENT, gb_trees:empty()).

-type label_pair() :: {label(), #label_data{}}.

-spec applyIncr([label_pair()], hipe_array(), gb_trees:tree()) ->
                   gb_trees:tree().
applyIncr([], _INCREMENT, LabelMap) -> LabelMap;
applyIncr([{Label,LabelData}|List], INCREMENT, LabelMap) ->
  #label_data{address=Address, prevSdi=PrevSdi} = LabelData,
  Incr =
    if PrevSdi < 0 -> 0;
       true -> array_sub(INCREMENT, PrevSdi)
    end,
  applyIncr(List, INCREMENT, gb_trees:insert(Label, Address+Incr, LabelMap)).

%%% ADT for immutable vectors, indexed from 0 to N-1.
%%% Currently implemented as tuples.
%%% Used for the 'SDIS' and 'PARENTS' vectors.

-spec vector_from_list([E]) -> hipe_vector(E).
vector_from_list(Values) -> list_to_tuple(Values).

-compile({inline, vector_sub/2}).
-spec vector_sub(hipe_vector(E), non_neg_integer()) -> V when V :: E.
vector_sub(Vec, I) -> element(I+1, Vec).

%%% ADT for mutable integer arrays, indexed from 0 to N-1.
%%% Currently implemented as HiPE arrays.
%%% Used for the 'LONG', 'SPAN', and 'INCREMENT' arrays.

-spec mk_array_of_zeros(non_neg_integer()) -> hipe_array().
mk_array_of_zeros(N) -> hipe_bifs:array(N, 0).

-compile({inline, array_update/3}).
-spec array_update(hipe_array(), non_neg_integer(), integer()) -> hipe_array().
array_update(A, I, V) -> hipe_bifs:array_update(A, I, V).

-compile({inline, array_sub/2}).
-spec array_sub(hipe_array(), non_neg_integer()) -> integer().
array_sub(A, I) -> hipe_bifs:array_sub(A, I).
