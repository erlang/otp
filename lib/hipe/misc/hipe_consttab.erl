%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc
%% CONSTTAB - maps labels to constants.
%% <p>
%% <strong> Note:</strong> 'constant' is a misnomer throughout this code.
%% </p>
%% <p>
%% There are two different types of constants that can be stored:
%%  <ul>
%%     <li>Erlang terms</li>
%%     <li>Blocks of binary data</li>
%%  </ul>
%% </p>
%% <p>
%% Erlang terms are just what you would expect, you can store any
%% Erlang term in the constant table. 
%% The term is assumed to be loaded to the place in memory denoted by the
%% label returned by the insertion function. 
%% </p>
%% <p>
%% Blocks of binary data comes in some different shapes, you can 
%% either insert a block of integers (of byte, word (4 bytes), or
%% word (8 bytes) size) or a list of references to code.
%% These references will then be threated as word sized addresses
%% and can be used for jumptables.
%% The list of references can have an optional ordering, so that 
%% you can create a jumptable that will be sorted on the load-time
%% representation of e.g. atoms.
%% </p>
%% @type ctdata() = #ctdata{}. See {@link mk_ctdata/4}.
%% @type ct_type() = term | block | sorted_block | ref
%% @type data() = term() | [term()] | [byte()] | internal().
%%   This type is dependent on ct_type
%%   <ul>
%%     <li> If ct_type() = term -- data() = term() </li>
%%     <li> If ct_type() = block -- data() = [byte()] </li>
%%     <li> If ct_type() = sorted_block -- data() = [term()] </li>
%%     <li> If ct_type() = ref -- data() = internal() </li>
%%   </ul>
%% @type ct_alignment().
%%    Alignment is always a power of two equal to the number of bytes
%%    in the machine word.
%% @end
%% @type byte(). <code>B</code> is an integer between 0 and 255.
%% @type hipe_consttab().
%%   An abstract datatype for storing data.
%% @end
%%  Internal note:
%%    A hipe_consttab is a tuple {Data, ReferedLabels, NextConstLabel}
%% @type hipe_constlbl().
%%   An abstract datatype for referring to data.
%% @type element_type() = byte | word | ctab_array()
%% @type ctab_array() = {ctab_array, Type::element_type(),
%%                                   NoElements::pos_integer()}
%% @type block() = [integer() | label_ref()]
%% @type label_ref() = {label, Label::code_label()}
%% @type code_label() = hipe_sparc:label_name() | hipe_x86:label_name()
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(hipe_consttab).

-export([new/0,             % new() -> ConstTab
	 insert_term/2,     % insert_term(ConstTab, Term) -> {NewTab, Lbl}
	 %% insert_fun/2,   % insert_term(ConstTab, Fun) -> {NewTab, Lbl}
	 %% insert_word/2,  % insert_word(ConstTab, Value) -> {NewTab, Lbl}
	 insert_sorted_block/2,    % insert_word(ConstTab, ValueList) -> 
	                           % {NewTab, Lbl}
	 insert_sorted_block/4,
	 insert_block/3,
	 insert_binary_const/3,
	 %% insert_global_word/2,
	 %% insert_global_block/4,
	 %% update_word/3,  % update_word(ConstTab, Value) -> {NewTab, Lbl}
	 %% update_block/5,
	 %% update_global_word/3,
	 %% update_global_block/5,
	 lookup/2,          % lookup(Key, ConstTab) -> [Term|Block]
	 labels/1,          % labels(ConstTab) -> LabelList
	 referred_labels/1, % referred_labels(ConstTab) -> LabelList
	 update_referred_labels/2, 
	 decompose/1,
	 size_of/1,
	 const_type/1,
	 const_align/1, 
	 const_exported/1,
	 const_data/1,
	 const_size/1
	 %% block_size/1    % size of a block in bytes 
	]).

%%-----------------------------------------------------------------------------

-include("hipe_consttab.hrl").

-type code_label()   :: term(). % XXX: FIXME
-type label_ref()    :: {'label', code_label()}.
-type block()	     :: [hipe_constlbl() | label_ref()].

-type ctab_array()   :: {'ctab_array', 'byte' | 'word', pos_integer()}.
-type element_type() :: 'byte' | 'word' | ctab_array().

-type sort_order()   :: term(). % XXX: FIXME

%%-----------------------------------------------------------------------------

%% @doc Create a new constant table.
-spec new() -> hipe_consttab().
new() -> {tree_empty(), [], 0}.


%% @spec insert_term(ConstTab::hipe_consttab(), Term::term()) -> {NewTab, Lbl}
%% NewTab = hipe_consttab()
%% Lbl = hipe_constlbl()
%% @doc Inserts an erlang term into the const table if the term was not 
%% present before, otherwise do nothing.
-spec insert_term(hipe_consttab(), term()) -> {hipe_consttab(),hipe_constlbl()}.
insert_term(ConstTab, Term) ->
  case lookup_const(ConstTab, term, word_size(), false, Term) of
    {value, Label} ->
      {ConstTab, Label};
    none ->
      insert_const(ConstTab, term, word_size(), false, Term)
  end.


%% %% @spec insert_fun(ConstTab::hipe_consttab(), Term::term()) -> {NewTab, Lbl}
%% %% NewTab = hipe_consttab()
%% %% Lbl = hipe_constlbl()
%% %% @doc Inserts a Fun into the const table.
%% %% Don't ask me what this is for...
%% -spec insert_fun(hipe_consttab(), term()) -> {hipe_consttab(), hipe_constlbl()}.
%% insert_fun(ConstTab, Fun) ->
%%   insert_const(ConstTab, term, word_size(), false, Fun).


%% @spec (ConstTab::hipe_consttab(), TermList::[term()]) -> {NewTab, Lbl}
%% NewTab = hipe_consttab()
%% Lbl = hipe_constlbl()
%% @doc Inserts a list of terms into the const table.
-spec insert_sorted_block(hipe_consttab(), [term()]) -> {hipe_consttab(), hipe_constlbl()}.
insert_sorted_block(CTab, TermList) ->
  insert_const(CTab, sorted_block, word_size(), false, TermList).

%% %% @spec (ConstTab::hipe_consttab(), InitVal::integer()) -> {NewTab, Lbl}
%% %% NewTab = hipe_consttab()
%% %% Lbl = hipe_constlbl()
%% %% @doc Inserts a word into the const table.
%% %% Shorthand for inserting a word.
%% insert_word(ConstTab, InitVal) ->
%%    insert_block(ConstTab, word, [InitVal]).

%% %% @spec (ConstTab::hipe_consttab(), InitVal::integer()) -> {NewTab, Lbl}
%% %% NewTab = hipe_consttab()
%% %% Lbl = hipe_constlbl()
%% %% @doc Inserts a word into the const table.
%% %% This constant should be exported from the function...
%% %% <strong>Note</strong> Global constants are 
%% %% not supported in current version of HiPE.
%% insert_global_word(ConstTab, InitVal) ->
%%    insert_global_block(ConstTab, word_size(), word, [InitVal]).


%% @spec (ConstTab::hipe_consttab(),
%%        ElementType::element_type(),
%%        InitList::block()) -> {hipe_consttab(), hipe_constlbl()}
%% @doc Inserts a block into the const table.
%% The block can consist of references to labels in the code.
%% This is used for jump tables. These references should be tracked 
%% and the corresponding BBs should not be considered dead.
-spec insert_block(hipe_consttab(), element_type(), block()) ->
	{hipe_consttab(), hipe_constlbl()}.
insert_block({ConstTab, RefToLabels, NextLabel}, ElementType, InitList) ->
  ReferredLabels = get_labels(InitList, []),
  NewRefTo = ReferredLabels ++ RefToLabels,
  {NewTa, Id} = insert_const({ConstTab, NewRefTo, NextLabel}, 
			     block, word_size(), false,
			     {ElementType,InitList}),
  {insert_backrefs(NewTa, Id, ReferredLabels), Id}.

%% @doc Inserts a binary constant literal into the const table.
-spec insert_binary_const(hipe_consttab(), ct_alignment(), binary()) ->
	{hipe_consttab(), hipe_constlbl()}.
insert_binary_const(ConstTab, Alignment, Binary)
  when (Alignment =:= 4 orelse Alignment =:= 8 orelse Alignment =:= 16
	orelse Alignment =:= 32), is_binary(Binary),
       size(Binary) rem Alignment =:= 0 ->
  insert_const(ConstTab, block, Alignment, false,
	       {byte, binary_to_list(Binary)}).


%% @spec (ConstTab::hipe_consttab(), ElementType::element_type(),
%%        InitList::block(), SortOrder) -> {hipe_consttab(), hipe_constlbl()}
%% @doc Inserts a block into the const table.
%% The block can consist of references to labels in the code.
%% This is used for jump tables. These references should be tracked 
%% and the corresponding BBs should not be considered dead.
%% At load-time the block will be sorted according to SortOrder.
%% This is used to make jump tables on atom indices.
-spec insert_sorted_block(hipe_consttab(), element_type(), block(), sort_order()) ->
	{hipe_consttab(), hipe_constlbl()}.
insert_sorted_block({ConstTab, RefToLabels, NextLabel}, 
                    ElementType, InitList, SortOrder) ->
  ReferredLabels = get_labels(InitList, []),
  NewRefTo = ReferredLabels ++ RefToLabels,
  {NewTa, Id} = insert_const({ConstTab, NewRefTo, NextLabel}, 
			     block, word_size(), false,
			     {ElementType, InitList, SortOrder}),
  {insert_backrefs(NewTa, Id, ReferredLabels), Id}.

insert_backrefs(Tbl, From, ToLabels) ->
  lists:foldl(fun(To, Tab) ->
		  insert_ref(Tab, From, To)
	      end, Tbl, ToLabels).

insert_ref({Table, RefToLabels, NextLblNr}, From, To) ->
  Ref = {To, ref},
  case tree_lookup(Ref, Table) of
    none ->
      {tree_insert(Ref, [From], Table), RefToLabels, NextLblNr};
    {value, RefList} ->
      {tree_update(Ref, [From|RefList], Table), RefToLabels, NextLblNr}
  end.

find_refs(To, {Table,_,_}) ->
  %% returns 'none' or {value, V}
  tree_lookup({To, ref}, Table).

delete_ref(To, {ConstTab, RefToLabels, NextLabel}) ->
  {tree_delete({To, ref}, ConstTab), RefToLabels, NextLabel}.

%% TODO: handle refs to labels.
%% insert_global_block(ConstTab, Align, ElementType, InitList) ->
%%    ByteList = decompose(size_of(ElementType), InitList),
%%    insert_const(ConstTab, block, Align, true, {byte,ByteList}).

get_labels([{label, L}|Rest], Acc) ->
  get_labels(Rest, [L|Acc]);
get_labels([I|Rest], Acc) when is_integer(I) -> 
  get_labels(Rest, Acc);
get_labels([], Acc) ->
  Acc.
  
%% @spec size_of(element_type()) -> pos_integer()
%% @doc Returns the size in bytes of an element_type.
%%  The is_atom/1 guard in the clause handling arrays
%%  constraints the argument to 'byte' | 'word'
-spec size_of(element_type()) -> pos_integer().
size_of(byte) -> 1;
size_of(word) -> word_size();
size_of({ctab_array,S,N}) when is_atom(S), is_integer(N), N > 0 ->
    N * size_of(S).

%% @spec decompose({element_type(), block()}) -> [byte()]
%% @doc Turns a block into a list of bytes.
%% <strong>Note:</strong> Be careful with the byte order here.
-spec decompose({element_type(), block()}) -> [byte()].
decompose({ElementType, Data}) ->
  decompose(size_of(ElementType), Data).

decompose(_Bytes, []) ->
   [];
decompose(Bytes, [X|Xs]) ->
   number_to_bytes(Bytes, X, decompose(Bytes, Xs)).

number_to_bytes(0, X, Bytes) when is_integer(X) ->
   Bytes;
number_to_bytes(N, X, Bytes) ->
   Byte = X band 255,
   number_to_bytes(N-1, X bsr 8, [Byte|Bytes]).

%% @spec block_size({element_type(), block()}) -> non_neg_integer()
%% @doc Returns the size in bytes of a block.
block_size({ElementType, Block}) ->
  length(Block) * size_of(ElementType);
block_size({ElementType, Block, _SortOrder}) ->
  length(Block) * size_of(ElementType).


%%--------------------
%% ctdata and friends
%%--------------------

-type ct_type() :: 'block' | 'ref' | 'sorted_block' | 'term'.

-record(ctdata, {type      :: ct_type(),
		 alignment :: ct_alignment(),
		 exported  :: boolean(),
		 data      :: term()}).
-type ctdata() :: #ctdata{}.

-spec mk_ctdata(Type::ct_type(), Alignment::ct_alignment(),
		Exported::boolean(), Data::term()) -> ctdata().
mk_ctdata(Type, Alignment, Exported, Data) ->
  #ctdata{type = Type, alignment = Alignment, exported = Exported, data = Data}.

-spec const_type(ctdata()) -> ct_type().
const_type(#ctdata{type = Type}) -> Type.

-spec const_align(ctdata()) -> ct_alignment().
const_align(#ctdata{alignment = Alignment}) -> Alignment.

-spec const_exported(ctdata()) -> boolean().
const_exported(#ctdata{exported = Exported}) -> Exported.

-spec const_data(ctdata()) -> term().
const_data(#ctdata{data = Data}) -> Data.

-spec update_const_data(ctdata(), {_,[_]} | {_,[_],_}) -> ctdata().
update_const_data(CTData, Data) -> 
  CTData#ctdata{data = Data}.

%% @doc Returns the size in bytes.
-spec const_size(ctdata()) -> non_neg_integer().
const_size(Constant) ->
  case const_type(Constant) of
    %% term: you can't and shouldn't ask for its size
    block -> block_size(const_data(Constant));
    sorted_block -> length(const_data(Constant)) * word_size()
  end.

-spec word_size() -> ct_alignment().
word_size() ->
  hipe_rtl_arch:word_size().


%%--------------------
%% Update a label
%%--------------------


%% TODO: Remove RefsTOfrom overwitten labels...
%% update_word(ConstTab, Label, InitVal) ->
%%    update_block(ConstTab, Label, word_size(), word, [InitVal]).
%%
%% update_global_word(ConstTab, Label, InitVal) ->
%%    update_global_block(ConstTab, Label, word_size(), word, [InitVal]).

%%
%% Update info for an existing label
%%
%% Returns NewTable
%%
%%
%% update_block(ConstTab, Label, Align, ElementType, InitList) ->
%%   ByteList = decompose(size_of(ElementType), InitList),
%%   update_const(ConstTab, Label, block, Align, false, {ElementType,ByteList}).

update_block_labels(ConstTab, DataLbl, OldLbl, NewLbl) ->
  Const = lookup(DataLbl, ConstTab),
  Old = {label, OldLbl},
  case const_data(Const) of
    {Type, Data} ->
      NewData = update_data(Data, Old, NewLbl),
      update(ConstTab, DataLbl, update_const_data(Const, {Type,NewData}));
    {Type, Data, Order} -> 
      NewData = update_data(Data, Old, NewLbl),
      update(ConstTab, DataLbl, update_const_data(Const, {Type,NewData,Order}))
    end.

update_data(Data, Old, New) ->
    [if Lbl =:= Old -> {label, New}; true -> Lbl end || Lbl <- Data].

%% update_global_block(ConstTab, Label, Align, ElementType, InitList) ->
%%   ByteList = decompose(size_of(ElementType), InitList),
%%   update_const(ConstTab, Label, block, Align, true, ByteList).

%%
%% Insert a constant in the table, returns {NewTable, Label}.
%%

insert_const({Table, RefToLabels, NextLblNr}, Type, Alignment, Exported, Data) ->
   Const = mk_ctdata(Type, Alignment, Exported, Data),
   {{tree_insert(NextLblNr, Const, Table), RefToLabels, NextLblNr+1}, 
    NextLblNr}.

%% %% Update information for a label, returns NewTable.
%% %% (Removes old info.)
%% 
%% update_const({Table, RefToLabels, NextLblNr}, Label, Type, Alignment, Exported, Data) ->
%%    Const = mk_ctdata(Type, Alignment, Exported, Data),
%%    {tree_update(Label, Const, Table), RefToLabels, NextLblNr}.

update({Table, RefToLabels, NextLblNr}, Label, NewConst) ->
  {tree_update(Label, NewConst, Table), RefToLabels, NextLblNr}.

%% @spec lookup(hipe_constlbl(), hipe_consttab()) -> ctdata()
%% @doc Lookup a label.
-spec lookup(hipe_constlbl(), hipe_consttab()) -> ctdata().
lookup(Lbl, {Table, _RefToLabels, _NextLblNr}) ->
  tree_get(Lbl, Table).

%% Find out if a constant term is present in the constant table.
lookup_const({Table, _RefToLabels, _NextLblNr}, 
	     Type, Alignment, Exported, Data) ->
  Const = mk_ctdata(Type, Alignment, Exported, Data),
  tree_lookup_key_for_value(Const, Table).

%% @doc Return the labels bound in a table.
-spec labels(hipe_consttab()) -> [hipe_constlbl() | {hipe_constlbl(), 'ref'}].
labels({Table, _RefToLabels, _NextLblNr}) ->
  tree_keys(Table).

%% @spec referred_labels(hipe_consttab()) -> [hipe_constlbl()]
%% @doc Return the referred labels bound in a table.
-spec referred_labels(hipe_consttab()) -> [hipe_constlbl()].
referred_labels({_Table, RefToLabels, _NextLblNr}) ->
  RefToLabels.


%%
%% Change label names in constant blocks (jump_tables).
%%
-spec update_referred_labels(hipe_consttab(),
			     [{hipe_constlbl(), hipe_constlbl()}]) ->
	 hipe_consttab().
update_referred_labels(Table, LabelMap) ->
  %% io:format("LabelMap: ~w\nTb:~w\n", [LabelMap, Table]),
  {Tb, Refs, Next} =
    lists:foldl(
      fun({OldLbl, NewLbl}, Tbl) ->
	  case find_refs(OldLbl, Tbl) of
	    none ->
	      Tbl;
	    {value, DataLbls} ->
	      %% A label may be referred several times.
	      UniqueLbls = ordsets:from_list(DataLbls),
	      lists:foldl(fun(DataLbl, AccTbl) ->
			      insert_ref(
				delete_ref(OldLbl,
					   update_block_labels(AccTbl, DataLbl, OldLbl, NewLbl)),
				DataLbl, NewLbl)
			  end,
			  Tbl,
			  UniqueLbls)
	  end
      end,
      Table,
      LabelMap),
  NewRefs = [case lists:keyfind(Lbl, 1, LabelMap) of
	       {_, New} -> New;
	       false -> Lbl
	     end || Lbl <- Refs],
  %% io:format("NewTb:~w\n", [{Tb, NewRefs, Next}]),
  {Tb, NewRefs, Next}.


%%-----------------------------------------------------------------------------
%% primitives for constants
%%-----------------------------------------------------------------------------

%% Since using `gb_trees' is not safe because of term ordering, we use
%% the `dict' module instead since it matches with =:= on the keys.

tree_keys(T) ->
  dict:fetch_keys(T).

-spec tree_to_list(dict:dict()) -> [{_, _}].
tree_to_list(T) ->
  dict:to_list(T).

tree_get(Key, T) ->
  dict:fetch(Key, T).

tree_update(Key, Val, T) ->
  dict:store(Key, Val, T).

tree_insert(Key, Val, T) ->
  dict:store(Key, Val, T).

tree_delete(Key, T) ->
  dict:erase(Key, T).

tree_lookup(Key, T) ->
  case dict:find(Key, T) of
    {ok, Val} ->
      {value, Val};
    error ->
      none
  end.

-spec tree_empty() -> dict:dict().
tree_empty() ->
  dict:new().

-spec tree_lookup_key_for_value(ctdata(), dict:dict()) -> 'none' | {'value', _}.
tree_lookup_key_for_value(Val, T) ->
  tree_lookup_key_for_value_1(tree_to_list(T), Val).

-spec tree_lookup_key_for_value_1([{_,_}], ctdata()) -> 'none' | {'value', _}.
tree_lookup_key_for_value_1([{Key, Val}|_], Val) ->
  {value, Key};
tree_lookup_key_for_value_1([_|Left], Val) ->
  tree_lookup_key_for_value_1(Left, Val);
tree_lookup_key_for_value_1([], _Val) ->
  none.
