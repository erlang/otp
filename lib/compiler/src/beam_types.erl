%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2019-2025. All Rights Reserved.
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

-module(beam_types).
-moduledoc false.

-define(BEAM_TYPES_INTERNAL, true).
-include("beam_types.hrl").

-import(lists, [foldl/3, mapfoldl/3, reverse/1, usort/1]).

-export([meet/1, meet/2, join/1, join/2, subtract/2]).

-export([is_boolean_type/1,
         is_numerical_type/1,
         get_bs_matchable_unit/1,
         is_bs_matchable_type/1,
         get_singleton_value/1,
         is_singleton_type/1,
         normalize/1]).

-export([get_tuple_element/2,
         set_tuple_element/3,
         update_tuple/2]).

-export([make_type_from_value/1]).

-export([make_atom/1,
         make_boolean/0,
         make_cons/2,
         make_float/1,
         make_integer/1,
         make_integer/2]).

-export([limit_depth/1]).

-export([decode_ext/1, encode_ext/1, convert_ext/2]).

%% This is exported to help catch errors in property test generators and is not
%% meant to be used outside of test suites.
-export([verified_type/1]).

-define(IS_LIST_TYPE(N),
        is_record(N, t_list) orelse
        is_record(N, t_cons) orelse
        N =:= nil).

-define(IS_NUMBER_TYPE(N),
        is_record(N, t_number) orelse
        is_record(N, t_float) orelse
        is_record(N, t_integer)).

%% Folds meet/2 over a list.

-spec meet([type()]) -> type().

meet([T1, T2 | Ts]) ->
    meet([meet(T1, T2) | Ts]);
meet([T]) -> T.

%% Return the "meet" of Type1 and Type2, which is more specific than Type1 and
%% Type2. This is identical to glb/2 but can operate on and produce unions.
%%
%%    A = #t_union{list=nil, number=[number], other=[#t_map{}]}
%%    B = #t_union{number=[#t_integer{}], other=[#t_map{}]}
%%
%%    meet(A, B) ->
%%         #t_union{number=[#t_integer{}], other=[#t_map{}]}
%%
%% The meet of two different types result in 'none', which is the bottom
%% element for our type lattice:
%%
%%    meet(#t_integer{}, #t_map{}) -> none

-spec meet(type(), type()) -> type().

meet(T, T) ->
    verified_type(T);
meet(any, T) ->
    verified_type(T);
meet(T, any) ->
    verified_type(T);
meet(#t_union{}=A, B) ->
    meet_unions(A, B);
meet(A, #t_union{}=B) ->
    meet_unions(B, A);
meet(A, B) ->
    glb(A, B).

meet_unions(#t_union{atom=AtomA,list=ListA,number=NumberA,
                     tuple_set=TSetA,other=OtherA},
            #t_union{atom=AtomB,list=ListB,number=NumberB,
                     tuple_set=TSetB,other=OtherB}) ->
    Union = #t_union{atom=glb(AtomA, AtomB),
                     list=glb(ListA, ListB),
                     number=glb(NumberA, NumberB),
                     tuple_set=meet_tuple_sets(TSetA, TSetB),
                     other=glb(OtherA, OtherB)},
    shrink_union(Union);
meet_unions(#t_union{atom=AtomA}, #t_atom{}=B) ->
    case glb(AtomA, B) of
        none -> none;
        Atom -> Atom
    end;
meet_unions(#t_union{number=NumberA}, B) when ?IS_NUMBER_TYPE(B) ->
    case glb(NumberA, B) of
        none -> none;
        Number -> Number
    end;
meet_unions(#t_union{list=ListA}, B) when ?IS_LIST_TYPE(B) ->
    case glb(ListA, B) of
        none -> none;
        List -> List
    end;
meet_unions(#t_union{tuple_set=Tuples}, #t_tuple{}=B) ->
    Set = meet_tuple_sets(Tuples, new_tuple_set(B)),
    shrink_union(#t_union{tuple_set=Set});
meet_unions(#t_union{other=OtherA}, OtherB) ->
    case glb(OtherA, OtherB) of
        none -> none;
        Other -> Other
    end.

meet_tuple_sets(none, _) ->
    none;
meet_tuple_sets(_, none) ->
    none;
meet_tuple_sets(#t_tuple{}=A, #t_tuple{}=B) ->
    new_tuple_set(glb(A, B));
meet_tuple_sets(#t_tuple{}=Tuple, Records) ->
    mts_tuple(Records, Tuple, []);
meet_tuple_sets(Records, #t_tuple{}=Tuple) ->
    meet_tuple_sets(Tuple, Records);
meet_tuple_sets(RecordsA, RecordsB) ->
    mts_records(RecordsA, RecordsB).

mts_tuple([{Key, Type} | Records], Tuple, Acc) ->
    case glb(Type, Tuple) of
        none -> mts_tuple(Records, Tuple, Acc);
        T -> mts_tuple(Records, Tuple, [{Key, T} | Acc])
    end;
mts_tuple([], _Tuple, [_|_]=Acc) ->
    reverse(Acc);
mts_tuple([], _Tuple, []) ->
    none.

mts_records(RecordsA, RecordsB) ->
    mts_records(RecordsA, RecordsB, []).

mts_records([{Key, A} | RsA], [{Key, B} | RsB], Acc) ->
    case glb(A, B) of
        none -> mts_records(RsA, RsB, Acc);
        T -> mts_records(RsA, RsB, [{Key, T} | Acc])
    end;
mts_records([{KeyA, _} | _]=RsA, [{KeyB, _} | _]=RsB, Acc) ->
    %% We must use total ordering rather than plain '<' as -0.0 differs from
    %% +0.0
    case total_compare(KeyA, KeyB, fun erlang:'<'/2) of
        true ->
            mts_records(tl(RsA), RsB, Acc);
        false ->
            true = KeyA =/= KeyB,               %Assertion.
            mts_records(RsA, tl(RsB), Acc)
    end;
mts_records(_RsA, [], [_|_]=Acc) ->
    reverse(Acc);
mts_records([], _RsB, [_|_]=Acc) ->
    reverse(Acc);
mts_records(_RsA, _RsB, []) ->
    none.

%% Folds join/2 over a list.

-spec join([type()]) -> type().

join([T | Ts]) ->
    join_list(Ts, T).

join_list([T | Ts], T) ->
    join_list(Ts, T);
join_list([T1 | Ts], T) ->
    join_list(Ts, join(T1, T));
join_list([], T) -> T.

%% Return the "join" of Type1 and Type2, which is more general than Type1 and
%% Type2. This is identical to lub/2 but can operate on and produce unions.
%%
%%    join(#t_integer{}, #t_map{}) -> #t_union{number=[#t_integer{}],
%%                                             other=[#t_map{}]}

-spec join(type(), type()) -> type().

join(T, T) -> T;
join(_T, any) -> any;
join(any, _T) -> any;
join(T, none) -> T;
join(none, T) -> T;

join(#t_union{}=A, B) ->
    join_unions(A, B);
join(A, #t_union{}=B) ->
    join_unions(B, A);

%% Union creation...
join(#t_atom{}=A, #t_atom{}=B) ->
    lub(A, B);
join(#t_atom{}=A, B) when ?IS_LIST_TYPE(B) ->
    #t_union{atom=A,list=B};
join(#t_atom{}=A, B) when ?IS_NUMBER_TYPE(B) ->
    #t_union{atom=A,number=B};
join(#t_atom{}=A, #t_tuple{}=B) ->
    #t_union{atom=A,tuple_set=new_tuple_set(B)};
join(#t_atom{}=A, B) ->
    #t_union{atom=A,other=B};
join(A, #t_atom{}=B) ->
    join(B, A);

join(A, B) when ?IS_LIST_TYPE(A), ?IS_LIST_TYPE(B) ->
    lub(A, B);
join(A, B) when ?IS_LIST_TYPE(A), ?IS_NUMBER_TYPE(B) ->
    #t_union{list=A,number=B};
join(A, #t_tuple{}=B) when ?IS_LIST_TYPE(A) ->
    #t_union{list=A,tuple_set=new_tuple_set(B)};
join(A, B) when ?IS_LIST_TYPE(A) ->
    #t_union{list=A,other=B};
join(A, B) when ?IS_LIST_TYPE(B) ->
    join(B, A);

join(A, B) when ?IS_NUMBER_TYPE(A), ?IS_NUMBER_TYPE(B) ->
    lub(A, B);
join(A, #t_tuple{}=B) when ?IS_NUMBER_TYPE(A) ->
    #t_union{number=A,tuple_set=new_tuple_set(B)};
join(A, B) when ?IS_NUMBER_TYPE(A) ->
    #t_union{number=A,other=B};
join(A, B) when ?IS_NUMBER_TYPE(B) ->
    join(B, A);

join(#t_tuple{}=A, #t_tuple{}=B) ->
    case {record_key(A), record_key(B)} of
        {Same, Same} ->
            lub(A, B);
        {none, _Key} ->
            lub(A, B);
        {_Key, none} ->
            lub(A, B);
        {KeyA, KeyB} ->
            %% We must use total ordering rather than plain '<' as -0.0 differs
            %% from +0.0
            case total_compare(KeyA, KeyB, fun erlang:'<'/2) of
                true ->
                    #t_union{tuple_set=[{KeyA, A}, {KeyB, B}]};
                false ->
                    #t_union{tuple_set=[{KeyB, B}, {KeyA, A}]}
            end
    end;
join(#t_tuple{}=A, B) ->
    %% All other combinations have been tried already, so B must be 'other'
    #t_union{tuple_set=new_tuple_set(A),other=B};
join(A, #t_tuple{}=B) ->
    join(B, A);

join(A, B) ->
    lub(A, B).

join_unions(#t_union{atom=AtomA,list=ListA,number=NumberA,
                     tuple_set=TSetA,other=OtherA},
            #t_union{atom=AtomB,list=ListB,number=NumberB,
                     tuple_set=TSetB,other=OtherB}) ->
    Union = #t_union{atom=lub(AtomA, AtomB),
                     list=lub(ListA, ListB),
                     number=lub(NumberA, NumberB),
                     tuple_set=join_tuple_sets(TSetA, TSetB),
                     other=lub(OtherA, OtherB)},
    shrink_union(Union);
join_unions(#t_union{atom=AtomA}=A, #t_atom{}=B) ->
    shrink_union(A#t_union{atom=lub(AtomA, B)});
join_unions(#t_union{list=ListA}=A, B) when ?IS_LIST_TYPE(B) ->
    shrink_union(A#t_union{list=lub(ListA, B)});
join_unions(#t_union{number=NumberA}=A, B) when ?IS_NUMBER_TYPE(B) ->
    shrink_union(A#t_union{number=lub(NumberA, B)});
join_unions(#t_union{tuple_set=TSetA}=A, #t_tuple{}=B) ->
    Set = join_tuple_sets(TSetA, new_tuple_set(B)),
    shrink_union(A#t_union{tuple_set=Set});
join_unions(#t_union{other=OtherA}=A, B) ->
    T = lub(OtherA, B),
    shrink_union(A#t_union{other=T}).

join_tuple_sets(A, none) ->
    A;
join_tuple_sets(none, B) ->
    B;
join_tuple_sets(#t_tuple{}=A, #t_tuple{}=B) ->
    lub(A, B);
join_tuple_sets(#t_tuple{}=Tuple, Records) ->
    jts_tuple(Records, Tuple);
join_tuple_sets(Records, #t_tuple{}=Tuple) ->
    join_tuple_sets(Tuple, Records);
join_tuple_sets(RecordsA, RecordsB) ->
    jts_records(RecordsA, RecordsB).

jts_tuple([{_Key, Tuple} | Records], Acc) ->
    jts_tuple(Records, lub(Tuple, Acc));
jts_tuple([], Acc) ->
    Acc.

jts_records(RsA, RsB) ->
    jts_records(RsA, RsB, 0, []).

jts_records([], [], _N, Acc) ->
    reverse(Acc);
jts_records(RsA, RsB, N, Acc) when N > ?TUPLE_SET_LIMIT ->
    A = normalize_tuple_set(RsA, none),
    B = normalize_tuple_set(RsB, A),
    #t_tuple{} = normalize_tuple_set(Acc, B);
jts_records([{Key, A} | RsA], [{Key, B} | RsB], N, Acc) ->
    jts_records(RsA, RsB, N + 1, [{Key, lub(A, B)} | Acc]);
jts_records([{KeyA, A} | _]=RsA, [{KeyB, B} | _]=RsB, N, Acc) ->
    %% We must use total ordering rather than plain '<' as -0.0 differs from
    %% +0.0
    case total_compare(KeyA, KeyB, fun erlang:'<'/2) of
        true ->
            jts_records(tl(RsA), RsB, N + 1, [{KeyA, A} | Acc]);
        false ->
            true = KeyA =/= KeyB,               %Assertion.
            jts_records(RsA, tl(RsB), N + 1, [{KeyB, B} | Acc])
    end;
jts_records([{KeyA, A} | RsA], [], N, Acc) ->
    jts_records(RsA, [], N + 1, [{KeyA, A} | Acc]);
jts_records([], [{KeyB, B} | RsB], N, Acc) ->
    jts_records([], RsB, N + 1, [{KeyB, B} | Acc]).

%% Subtract Type2 from Type1. Example:
%%    subtract(list, cons) -> nil

-spec subtract(type(), type()) -> type().

subtract(any, #t_number{elements={'-inf',Max}}) ->
    %% We handle this case specially in order to represent the type
    %% Var in Var =< Integer.
    #t_union{atom=#t_atom{},
             list=#t_list{},
             number=#t_number{elements={Max,'+inf'}},
             tuple_set=#t_tuple{},
             other=other};
subtract(any, nil) ->
    %%
    %% We handle this subtraction mainly for correctness. Consider:
    %%
    %%   foobar(L) when L =/= [], is_list(L), L =/= [], hd(L) -> ok.
    %%
    %% The type-based optimizations would rewrite the hd/1 BIF call to
    %% a get_hd instruction:
    %%
    %%   foobar(L) when L =/= [], is_list(L), L =/= [], get_hd(L) -> ok.
    %%
    %% The beam_ssa_dead pass would later remove the redundant second
    %% test of `L =/= []`:
    %%
    %%   foobar(L) when L =/= [], is_list(L), get_hd(L) -> ok.
    %%
    %% With that test removed, the type for L in the get_hd instruction
    %% would be #t_list{} instead of the required #t_cons{} and that
    %% would trigger an assertion. (If the assertion were to be removed,
    %% beam_validator would complain instead.)
    %%
    %% By letting the type subtraction of `any` by `nil` return a
    %% union, the optimized code above becomes legal. As an added bonus,
    %% the is_list/1 guard test can be replaced with the cheaper
    %% is_nonempty_list instruction:
    %%
    %%   foobar(L) when L =/= [], is_nonempty_list(L), get_hd(L) -> ok.
    %%
    #t_union{atom=#t_atom{},
             list=#t_cons{},
             number=#t_number{},
             tuple_set=#t_tuple{},
             other=other};
subtract(#t_atom{elements=[_|_]=Set0}, #t_atom{elements=[_|_]=Set1}) ->
    case ordsets:subtract(Set0, Set1) of
        [] -> none;
        [_|_]=Set -> #t_atom{elements=Set}
    end;
subtract(#t_bitstring{size_unit=UnitA}=T, #t_bs_matchable{tail_unit=UnitB}) ->
    subtract_matchable(T, UnitA, UnitB);
subtract(#t_bitstring{appendable=App,size_unit=UnitA}=T,
         #t_bitstring{appendable=App,size_unit=UnitB}) ->
    subtract_matchable(T, UnitA, UnitB);
subtract(#t_bitstring{}=T, #t_bitstring{}) ->
    T;
subtract(#t_bs_context{tail_unit=UnitA}=T, #t_bs_matchable{tail_unit=UnitB}) ->
    subtract_matchable(T, UnitA, UnitB);
subtract(#t_bs_context{tail_unit=UnitA}=T, #t_bs_context{tail_unit=UnitB}) ->
    subtract_matchable(T, UnitA, UnitB);
subtract(#t_integer{elements={Min, Max}}, #t_integer{elements={N,N}}) ->
    if
        Min =:= N, Max =:= N ->
            none;
        Min =/= N, Max =/= N ->
            #t_integer{elements={Min, Max}};
        Min =:= N ->
            #t_integer{elements={Min + 1, Max}};
        Max =:= N ->
            #t_integer{elements={Min, Max - 1}}
    end;
subtract(#t_number{elements=R}, #t_float{elements=any}) ->
    integer_from_range(R);
subtract(#t_number{elements=R}, #t_integer{elements=any}) ->
    float_from_range(R);

%% A list is essentially `#t_cons{} | nil`, so we're left with nil if we
%% subtract a cons cell that is more general than the one in the list.
subtract(#t_list{type=TypeA,terminator=TermA}=T,
         #t_cons{type=TypeB,terminator=TermB}) ->
    case {meet(TypeA, TypeB), meet(TermA, TermB)} of
        {TypeA, TermA} -> nil;
        _ -> T
    end;
subtract(#t_list{type=Type,terminator=Term}, nil) ->
    #t_cons{type=Type,terminator=Term};

subtract(identifier, other) ->
    identifier;

subtract(#t_union{atom=Atom}=A, #t_atom{}=B)->
    shrink_union(A#t_union{atom=subtract(Atom, B)});
subtract(#t_union{number=Number}=A, B) when ?IS_NUMBER_TYPE(B) ->
    shrink_union(A#t_union{number=subtract(Number, B)});
subtract(#t_union{list=List}=A, B) when ?IS_LIST_TYPE(B) ->
    shrink_union(A#t_union{list=subtract(List, B)});
subtract(#t_union{tuple_set=[_|_]=Records0}=A, #t_tuple{}=B) ->
    %% Filter out all records that are more specific than B.
    NewSet = case [{Key, T} || {Key, T} <:- Records0, meet(T, B) =/= T] of
                 [_|_]=Records -> Records;
                 [] -> none
             end,
    shrink_union(A#t_union{tuple_set=NewSet});
subtract(#t_union{tuple_set=#t_tuple{}=Tuple}=A, #t_tuple{}=B) ->
    %% Exclude Tuple if it's more specific than B.
    case meet(Tuple, B) of
        Tuple -> shrink_union(A#t_union{tuple_set=none});
        _ -> A
    end;
subtract(#t_union{other=Other}=A, B) ->
    shrink_union(A#t_union{other=subtract(Other, B)});

subtract(A, B) ->
    %% There's nothing left if A is more specific than B.
    case meet(A, B) of
        A -> none;
        _Other -> A
    end.

subtract_matchable(T, UnitA, UnitB) ->
    if
        UnitA rem UnitB =:= 0 -> none;
        UnitA rem UnitB =/= 0 -> T
    end.

%%%
%%% Type operators
%%%

-spec get_bs_matchable_unit(type()) -> pos_integer() | error.
get_bs_matchable_unit(#t_bitstring{size_unit=Unit}) ->
    Unit;
get_bs_matchable_unit(#t_bs_context{tail_unit=Unit}) ->
    Unit;
get_bs_matchable_unit(#t_bs_matchable{tail_unit=Unit}) ->
    Unit;
get_bs_matchable_unit(_) ->
    error.

-spec is_bs_matchable_type(type()) -> boolean().
is_bs_matchable_type(Type) ->
    get_bs_matchable_unit(Type) =/= error.

-spec get_singleton_value(Type) -> Result when
      Type :: type(),
      Result :: {ok, term()} | error.
get_singleton_value(#t_atom{elements=[Atom]}) ->
    {ok, Atom};
get_singleton_value(#t_float{elements={Float,Float}}) ->
    {ok, Float};
get_singleton_value(#t_integer{elements={Int,Int}}) ->
    {ok, Int};
get_singleton_value(#t_map{super_key=none,super_value=none}) ->
    {ok, #{}};
get_singleton_value(#t_tuple{exact=true,size=Size,elements=Es}) ->
    case gsv_elements(Size, Es, []) of
        Values when is_list(Values) ->
            {ok, list_to_tuple(Values)};
        error ->
            error
    end;
get_singleton_value(nil) ->
    {ok, []};
get_singleton_value(_) ->
    error.

gsv_elements(0, _Es, Acc) ->
    %% The elements were added right-to-left, so it's already in order.
    Acc;
gsv_elements(N, Es, Acc) ->
    ElementType = get_tuple_element(N, Es),
    case get_singleton_value(ElementType) of
        {ok, Value} -> gsv_elements(N - 1, Es, [Value | Acc]);
        error -> error
    end.

-spec is_singleton_type(type()) -> boolean().
is_singleton_type(Type) ->
    get_singleton_value(Type) =/= error.

-spec is_boolean_type(type()) -> boolean().
is_boolean_type(#t_atom{elements=[F,T]}) ->
    F =:= false andalso T =:= true;
is_boolean_type(#t_atom{elements=[B]}) ->
    is_boolean(B);
is_boolean_type(#t_union{}=T) ->
    is_boolean_type(normalize(T));
is_boolean_type(_) ->
    false.

-spec is_numerical_type(type()) -> boolean().
is_numerical_type(#t_integer{}) -> true;
is_numerical_type(#t_number{}) -> true;
is_numerical_type(_) -> false.

-spec set_tuple_element(Index, Type, Elements) -> Elements when
      Index :: pos_integer(),
      Type :: type(),
      Elements :: tuple_elements().
set_tuple_element(Index, _Type, Es) when Index > ?TUPLE_ELEMENT_LIMIT ->
    Es;
set_tuple_element(_Index, none, Es) ->
    Es;
set_tuple_element(Index, any, Es) ->
    maps:remove(Index, Es);
set_tuple_element(Index, Type, Es) ->
    Es#{ Index => Type }.

-spec get_tuple_element(Index, Elements) -> type() when
      Index :: pos_integer(),
      Elements :: tuple_elements().
get_tuple_element(Index, Es) ->
    case Es of
        #{ Index := T } -> T;
        #{} -> any
    end.

%% Helper routine for `update_tuple` / `update_record` instructions, which copy
%% an existing type and updates a few fields.
-spec update_tuple(Type, Updates) -> Tuple when
      Type :: type(),
      Updates :: [{pos_integer(), type()}, ...],
      Tuple :: type().
update_tuple(#t_union{tuple_set=[_|_]=Set0}, [_|_]=Updates) ->
    case Updates of
        [{1, _} | _] ->
            %% The update overwrites the tag, so we can no longer keep any
            %% records apart. Normalize the set before trying again.
            update_tuple(normalize_tuple_set(Set0, none), Updates);
        [_|_] ->
            case update_tuple_set(Set0, Updates) of
                [] ->
                    none;
                [_|_]=Set ->
                    verified_type(shrink_union(#t_union{tuple_set=Set}))
            end
    end;
update_tuple(#t_union{tuple_set=#t_tuple{}=Tuple}, [_|_]=Updates) ->
    update_tuple(Tuple, Updates);
update_tuple(#t_tuple{exact=Exact,
                      size=Size,
                      elements=Es0}=Tuple,
             [_|_]=Updates) ->
    case update_tuple_1(Updates, Size, Es0) of
        {MinSize, _Es} when Exact, MinSize > Size ->
            none;
        {MinSize, Es} ->
            verified_normal_type(Tuple#t_tuple{size=MinSize,elements=Es})
    end;
update_tuple(Type, [_|_]=Updates) ->
    case meet(Type, #t_tuple{size=1}) of
        none -> none;
        Tuple -> update_tuple(Tuple, Updates)
    end.

update_tuple_set([{Tag, Record0} | Set], Updates) ->
    case update_tuple(Record0, Updates) of
        none -> update_tuple_set(Set, Updates);
        #t_tuple{}=Record -> [{Tag, Record} | update_tuple_set(Set, Updates)]
    end;
update_tuple_set([], _Es) ->
    [].

update_tuple_1([{Index, Type} | Updates], MinSize, Es0) ->
    Es = set_tuple_element(Index, Type, Es0),
    update_tuple_1(Updates, max(Index, MinSize), Es);
update_tuple_1([], MinSize, Es) ->
    {MinSize, Es}.

-spec normalize(type()) -> normal_type().
normalize(#t_union{atom=Atom,list=List,number=Number,
                   tuple_set=Tuples,other=Other}) ->
    A = lub(Atom, List),
    B = lub(A, Number),
    C = lub(B, Other),
    normalize_tuple_set(Tuples, C);
normalize(T) ->
    verified_normal_type(T).

normalize_tuple_set([{_, A} | Records], B) ->
    normalize_tuple_set(Records, lub(A, B));
normalize_tuple_set([], B) ->
    B;
normalize_tuple_set(A, B) ->
    lub(A, B).

%%%
%%% Type constructors
%%%

-spec make_type_from_value(term()) -> type().
make_type_from_value(Value) ->
    mtfv_1(Value).

mtfv_1(A) when is_atom(A) ->
    #t_atom{elements=[A]};
mtfv_1(B) when is_bitstring(B) ->
    case bit_size(B) of
        0 ->
            %% See the #t_bitstring{} definition in beam_types.hrl for
            %% why empty binaries are considered appendable.
            #t_bitstring{size_unit=256,appendable=true};
        Size ->
            #t_bitstring{size_unit=gcd(Size, 256)}
    end;
mtfv_1(F) when is_float(F) ->
    make_float(F);
mtfv_1(F) when is_function(F) ->
    {arity, Arity} = erlang:fun_info(F, arity),
    #t_fun{arity=Arity};
mtfv_1(I) when is_integer(I) ->
    make_integer(I);
mtfv_1(L) when is_list(L) ->
    case L of
        [_|_] -> mtfv_cons(L, none);
        [] -> nil
    end;
mtfv_1(M) when is_map(M) ->
    {SKey, SValue} =
        maps:fold(fun(Key, Value, {SKey0, SValue0}) ->
                          SKey = join(mtfv_1(Key), SKey0),
                          SValue = join(mtfv_1(Value), SValue0),
                          {SKey, SValue}
                  end, {none, none}, M),
    #t_map{super_key=SKey,super_value=SValue};
mtfv_1(T) when is_tuple(T) ->
    {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                           Type = mtfv_1(Val),
                           Es = set_tuple_element(Index, Type, Es0),
                           {Es, Index + 1}
                   end, {#{}, 1}, tuple_to_list(T)),
    #t_tuple{exact=true,size=tuple_size(T),elements=Es};
mtfv_1(_Term) ->
    any.

mtfv_cons([Head | Tail], Type) ->
    mtfv_cons(Tail, join(mtfv_1(Head), Type));
mtfv_cons(Terminator, Type) ->
    #t_cons{type=Type,terminator=mtfv_1(Terminator)}.

-spec make_atom(atom()) -> type().
make_atom(Atom) when is_atom(Atom) ->
    #t_atom{elements=[Atom]}.

-spec make_boolean() -> type().
make_boolean() ->
    #t_atom{elements=[false,true]}.

-spec make_cons(type(), type()) -> type().
make_cons(Head0, Tail) ->
    case meet(Tail, #t_cons{}) of
        #t_cons{type=Type0,terminator=Term0} ->
            %% Propagate element and terminator types. Note that if the tail is
            %% the union of a list and something else, the new list could be
            %% terminated by the other types in the union.
            Type = join(Head0, Type0),
            Term = join(subtract(Tail, #t_cons{}), Term0),
            #t_cons{type=Type,terminator=Term};
        _ ->
            %% Tail can't be a cons cell, so we know it terminates the list.
            #t_cons{type=Head0,terminator=Tail}
    end.

-spec make_float(float()) -> type().
make_float(Float) when is_float(Float) ->
    #t_float{elements={Float,Float}}.

-spec make_integer(integer()) -> type().
make_integer(Int) when is_integer(Int) ->
    make_integer(Int, Int).

-spec make_integer(Min, Max) -> type() when
      Min :: integer(),
      Max :: integer().
make_integer(Min, Max) when is_integer(Min), is_integer(Max), Min =< Max ->
    #t_integer{elements={Min,Max}}.

-spec limit_depth(type()) -> type().

limit_depth(Type) ->
    limit_depth(Type, ?MAX_TYPE_DEPTH).

-spec limit_depth(type(), non_neg_integer()) -> type().

limit_depth(#t_cons{}=T, Depth) ->
    limit_depth_list(T, Depth);
limit_depth(#t_list{}=T, Depth) ->
    limit_depth_list(T, Depth);
limit_depth(#t_tuple{}=T, Depth) ->
    limit_depth_tuple(T, Depth);
limit_depth(#t_fun{}=T, Depth) ->
    limit_depth_fun(T, Depth);
limit_depth(#t_map{}=T, Depth) ->
    limit_depth_map(T, Depth);
limit_depth(#t_union{list=List0,tuple_set=TupleSet0,other=Other0}=U, Depth) ->
    TupleSet = limit_depth_tuple(TupleSet0, Depth),
    List = limit_depth_list(List0, Depth),
    Other = limit_depth(Other0, Depth),
    shrink_union(U#t_union{list=List,tuple_set=TupleSet,other=Other});
limit_depth(Type, _Depth) ->
    Type.

limit_depth_fun(#t_fun{type=Type0}=T, Depth) ->
    Type = if
               Depth > 0 -> limit_depth(Type0, Depth - 1);
               Depth =< 0 -> any
           end,
    T#t_fun{type=Type}.

limit_depth_list(#t_cons{type=Type0,terminator=Term0}=T, Depth) ->
    {Type, Term} = limit_depth_list_1(Type0, Term0, Depth),
    T#t_cons{type=Type,terminator=Term};
limit_depth_list(#t_list{type=Type0,terminator=Term0}=T, Depth) ->
    {Type, Term} = limit_depth_list_1(Type0, Term0, Depth),
    T#t_list{type=Type,terminator=Term};
limit_depth_list(nil, _Depth) ->
    nil;
limit_depth_list(none, _Depth) ->
    none.

limit_depth_list_1(Type0, Terminator0, Depth) when Depth > 0 ->
    Type = limit_depth(Type0, Depth - 1),
    Terminator = limit_depth(Terminator0, Depth - 1),
    {Type, Terminator};
limit_depth_list_1(_Type, _Terminator, Depth) when Depth =< 0 ->
    {any, any}.

limit_depth_map(#t_map{ super_key=SKey0,
                        super_value=SValue0 }, Depth) when Depth > 0 ->
    SKey = limit_depth(SKey0, Depth - 1),
    SValue = limit_depth(SValue0, Depth - 1),
    #t_map{super_key=SKey,super_value=SValue};
limit_depth_map(#t_map{}, Depth) when Depth =< 0 ->
    #t_map{}.

limit_depth_tuple(#t_tuple{elements=Es0}=T, Depth) ->
    if
        Depth > 0 ->
            Es = foldl(fun({Index, E0}, Es1) ->
                               E = limit_depth(E0, Depth - 1),
                               set_tuple_element(Index, E, Es1)
                       end, Es0, maps:to_list(Es0)),
            T#t_tuple{elements=Es};
        Depth =< 0 ->
            #t_tuple{elements=#{}}
    end;
limit_depth_tuple([{{MinSize,_},_}|_], Depth) when Depth =< 0 ->
    %% Preserve the minimum size of the tuple set.
    #t_tuple{exact=false,size=MinSize};
limit_depth_tuple([{SzTag,Tuple}|Ts], Depth) ->
    [{SzTag, limit_depth_tuple(Tuple, Depth)} | limit_depth_tuple(Ts, Depth)];
limit_depth_tuple([], _Depth) ->
    [];
limit_depth_tuple(none, _Depth) ->
    none.

%%%
%%% Helpers
%%%

%% Return the greatest lower bound of the types Type1 and Type2. The GLB is a
%% more specific type than Type1 and Type2, and is always a normal type.
%%
%%    glb(#t_integer{elements=any}, #t_integer{elements={0,3}}) ->
%%         #t_integer{elements={0,3}}
%%
%% The GLB of two different types result in 'none', which is the bottom
%% element for our type lattice:
%%
%%    glb(#t_integer{}, #t_map{}) -> none

-spec glb(normal_type(), normal_type()) -> normal_type().

glb(T, T) ->
    verified_normal_type(T);
glb(any, T) ->
    verified_normal_type(T);
glb(T, any) ->
    verified_normal_type(T);
glb(#t_atom{elements=[_|_]=Set1}, #t_atom{elements=[_|_]=Set2}) ->
    case ordsets:intersection(Set1, Set2) of
        [] ->
            none;
        [_|_]=Set ->
            #t_atom{elements=Set}
    end;
glb(#t_atom{elements=[_|_]}=T, #t_atom{elements=any}) ->
    T;
glb(#t_atom{elements=any}, #t_atom{elements=[_|_]}=T) ->
    T;
glb(#t_bitstring{size_unit=U1,appendable=A1},
    #t_bitstring{size_unit=U2,appendable=A2}) ->
    #t_bitstring{size_unit=U1 * U2 div gcd(U1, U2),appendable=A1 or A2};
glb(#t_bitstring{size_unit=UnitA,appendable=Appendable}=T,
    #t_bs_matchable{tail_unit=UnitB}) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    T#t_bitstring{size_unit=Unit,appendable=Appendable};
glb(#t_bs_context{tail_unit=UnitA}, #t_bs_context{tail_unit=UnitB}) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    #t_bs_context{tail_unit=Unit};
glb(#t_bs_context{tail_unit=UnitA}=T, #t_bs_matchable{tail_unit=UnitB}) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    T#t_bs_context{tail_unit=Unit};
glb(#t_bs_matchable{tail_unit=UnitA}, #t_bs_matchable{tail_unit=UnitB}) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    #t_bs_matchable{tail_unit=Unit};
glb(#t_bs_matchable{tail_unit=UnitA},
    #t_bitstring{size_unit=UnitB,appendable=Appendable}=T) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    T#t_bitstring{size_unit=Unit,appendable=Appendable};
glb(#t_bs_matchable{tail_unit=UnitA}, #t_bs_context{tail_unit=UnitB}=T) ->
    Unit = UnitA * UnitB div gcd(UnitA, UnitB),
    T#t_bs_context{tail_unit=Unit};
glb(#t_cons{type=TypeA,terminator=TermA},
    #t_cons{type=TypeB,terminator=TermB}) ->
    %% Note the use of meet/2; elements don't need to be normal types.
    case {meet(TypeA, TypeB), meet(TermA, TermB)} of
        {none, _} -> none;
        {_, none} -> none;
        {Type, Term} -> #t_cons{type=Type,terminator=Term}
    end;
glb(#t_cons{type=TypeA,terminator=TermA},
    #t_list{type=TypeB,terminator=TermB}) ->
    case {meet(TypeA, TypeB), meet(TermA, TermB)} of
        {none, _} -> none;
        {_, none} -> none;
        {Type, Term} -> #t_cons{type=Type,terminator=Term}
    end;
glb(#t_float{elements=R1}, #t_float{elements=R2}) ->
    float_from_range(glb_ranges(R1, R2));
glb(#t_fun{arity=SameArity,target=SameTarget,type=TypeA},
    #t_fun{arity=SameArity,target=SameTarget,type=TypeB}=T) ->
    T#t_fun{type=meet(TypeA, TypeB)};
glb(#t_fun{target=TargetA}=A, #t_fun{target=any}=B) when TargetA =/= any ->
    glb(A, B#t_fun{target=TargetA});
glb(#t_fun{target=any}=A, #t_fun{target=TargetB}=B) when TargetB =/= any ->
    glb(A#t_fun{target=TargetB}, B);
glb(#t_fun{arity=any}=A, #t_fun{arity=ArityB}=B) when ArityB =/= any->
    glb(A#t_fun{arity=ArityB}, B);
glb(#t_fun{arity=ArityA}=A, #t_fun{arity=any}=B) when ArityA =/= any ->
    glb(A, B#t_fun{arity=ArityA});
glb(#t_integer{elements=R1}, #t_integer{elements=R2}) ->
    integer_from_range(glb_ranges(R1, R2));
glb(#t_integer{elements=R1}, #t_number{elements=R2}) ->
    integer_from_range(glb_ranges(R1, R2));
glb(#t_float{elements=R1}, #t_number{elements=R2}) ->
    float_from_range(glb_ranges(R1, number_to_float_range(R2)));
glb(#t_list{type=TypeA,terminator=TermA},
    #t_list{type=TypeB,terminator=TermB}) ->
    %% A list is a union of `[type() | _]` and `[]`, so we're left with
    %% nil when the element types are incompatible.
    case {meet(TypeA, TypeB), meet(TermA, TermB)} of
        {none, _} -> nil;
        {_, none} -> nil;
        {Type, Term} -> #t_list{type=Type,terminator=Term}
    end;
glb(#t_list{}=A, #t_cons{}=B) ->
    glb(B, A);
glb(#t_list{}, nil) ->
    nil;
glb(nil, #t_list{}) ->
    nil;
glb(#t_number{elements=R1}, #t_number{elements=R2}) ->
    number_from_range(glb_ranges(R1, R2));
glb(#t_number{elements=R1}, #t_integer{elements=R2}) ->
    integer_from_range(glb_ranges(R1, R2));
glb(#t_number{elements=R1}, #t_float{elements=R2}) ->
    float_from_range(glb_ranges(number_to_float_range(R1), R2));
glb(#t_map{super_key=SKeyA,super_value=SValueA},
    #t_map{super_key=SKeyB,super_value=SValueB}) ->
    %% Note the use of meet/2; elements don't need to be normal types.
    SKey = meet(SKeyA, SKeyB),
    SValue = meet(SValueA, SValueB),
    #t_map{super_key=SKey,super_value=SValue};
glb(#t_tuple{}=T1, #t_tuple{}=T2) ->
    glb_tuples(T1, T2);
glb(identifier, T) ->
    case is_identifier(T) of
        true ->
            T;
        false ->
            case T of
                other -> identifier;
                _ -> none
            end
    end;
glb(T, identifier) ->
    glb(identifier, T);
glb(other, T) ->
    case is_other(T) of
        true -> T;
        false -> none
    end;
glb(T, other) ->
    glb(other, T);
glb(_, _) ->
    %% Inconsistent types. There will be an exception at runtime.
    none.

glb_ranges({MinA,MaxA}, {MinB,MaxB}) ->
    true = inf_le(MinA, MaxA) andalso inf_le(MinB, MaxB), %Assertion.
    case (inf_ge(MinA, MinB) andalso inf_le(MinA, MaxB)) orelse
        (inf_ge(MinB, MinA) andalso inf_le(MinB, MaxA)) of
        true ->
            true = inf_le(MinA, MaxA) andalso inf_le(MinB, MaxB),   %Assertion.
            {inf_max(MinA, MinB),inf_min(MaxA, MaxB)};
        false ->
            none
    end;
glb_ranges({MinA,MaxA}, any) ->
    {MinA,MaxA};
glb_ranges(any, {MinB,MaxB}) ->
    {MinB,MaxB};
glb_ranges(_, _) ->
    any.

glb_tuples(#t_tuple{size=Sz1,exact=Ex1}, #t_tuple{size=Sz2,exact=Ex2})
  when Ex1, Sz1 < Sz2;
       Ex2, Sz2 < Sz1 ->
    none;
glb_tuples(#t_tuple{size=Sz1,exact=Ex1,elements=Es1},
           #t_tuple{size=Sz2,exact=Ex2,elements=Es2}) ->
    Size = max(Sz1, Sz2),
    Exact = Ex1 or Ex2,
    case glb_elements(Es1, Es2) of
        none ->
            none;
        Es ->
            #t_tuple{size=Size,exact=Exact,elements=Es}
    end.

glb_elements(Es1, Es2) ->
    Keys = usort(maps:keys(Es1) ++ maps:keys(Es2)),
    glb_elements_1(Keys, Es1, Es2, #{}).

glb_elements_1([Key | Keys], Es1, Es2, Acc) ->
    case {Es1, Es2} of
        {#{ Key := Type1 }, #{ Key := Type2 }} ->
            %% Note the use of meet/2; elements don't need to be normal types.
            case meet(Type1, Type2) of
                none -> none;
                Type -> glb_elements_1(Keys, Es1, Es2, Acc#{ Key => Type })
            end;
        {#{ Key := Type1 }, _} ->
            glb_elements_1(Keys, Es1, Es2, Acc#{ Key => Type1 });
        {_, #{ Key := Type2 }} ->
            glb_elements_1(Keys, Es1, Es2, Acc#{ Key => Type2 })
    end;
glb_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

%% Return the least upper bound of the types Type1 and Type2. The LUB is a more
%% general type than Type1 and Type2, and is always a normal type.
%%
%% For example:
%%
%%    lub(#t_integer{elements=any}, #t_integer=elements={0,3}}) ->
%%         #t_integer{}
%%
%% The LUB for two different types result in 'any' (not a union type!), which
%% is the top element for our type lattice:
%%
%%    lub(#t_integer{}, #t_map{}) -> any

-spec lub(normal_type(), normal_type()) -> normal_type().

lub(T, T) ->
    verified_normal_type(T);
lub(none, T) ->
    verified_normal_type(T);
lub(T, none) ->
    verified_normal_type(T);
lub(any, _) ->
    any;
lub(_, any) ->
    any;
lub(#t_atom{elements=[_|_]=Set1}, #t_atom{elements=[_|_]=Set2}) ->
    Set = ordsets:union(Set1, Set2),
    case ordsets:size(Set) of
        Size when Size =< ?ATOM_SET_SIZE ->
            #t_atom{elements=Set};
        _Size ->
            #t_atom{elements=any}
    end;
lub(#t_atom{elements=any}=T, #t_atom{elements=[_|_]}) -> T;
lub(#t_atom{elements=[_|_]}, #t_atom{elements=any}=T) -> T;
lub(#t_bitstring{size_unit=U1,appendable=A1},
    #t_bitstring{size_unit=U2,appendable=A2}) ->
    #t_bitstring{size_unit=gcd(U1, U2),appendable=A1 and A2};
lub(#t_bitstring{size_unit=U1}, #t_bs_context{tail_unit=U2}) ->
    #t_bs_matchable{tail_unit=gcd(U1, U2)};
lub(#t_bitstring{size_unit=UnitA}, #t_bs_matchable{tail_unit=UnitB}) ->
    lub_bs_matchable(UnitA, UnitB);
lub(#t_bs_context{tail_unit=UnitA}, #t_bs_context{tail_unit=UnitB}) ->
    #t_bs_context{tail_unit=gcd(UnitA, UnitB)};
lub(#t_bs_context{tail_unit=U1}, #t_bitstring{size_unit=U2}) ->
    #t_bs_matchable{tail_unit=gcd(U1, U2)};
lub(#t_bs_context{tail_unit=UnitA}, #t_bs_matchable{tail_unit=UnitB}) ->
    lub_bs_matchable(UnitA, UnitB);
lub(#t_bs_matchable{tail_unit=UnitA}, #t_bs_matchable{tail_unit=UnitB}) ->
    lub_bs_matchable(UnitA, UnitB);
lub(#t_bs_matchable{tail_unit=UnitA}, #t_bitstring{size_unit=UnitB}) ->
    lub_bs_matchable(UnitA, UnitB);
lub(#t_bs_matchable{tail_unit=UnitA}, #t_bs_context{tail_unit=UnitB}) ->
    lub_bs_matchable(UnitA, UnitB);
lub(#t_cons{type=TypeA,terminator=TermA},
    #t_cons{type=TypeB,terminator=TermB}) ->
    %% Note the use of join/2; elements don't need to be normal types.
    #t_cons{type=join(TypeA,TypeB),terminator=join(TermA, TermB)};
lub(#t_cons{type=TypeA,terminator=TermA},
    #t_list{type=TypeB,terminator=TermB}) ->
    #t_list{type=join(TypeA,TypeB),terminator=join(TermA, TermB)};
lub(#t_cons{type=Type,terminator=Term}, nil) ->
    #t_list{type=Type,terminator=Term};
lub(#t_float{elements=R1}, #t_float{elements=R2}) ->
    float_from_range(lub_ranges(R1, R2));
lub(#t_float{elements=R1}, #t_integer{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_float{elements=R1}, #t_number{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_fun{arity=SameArity,target=SameTarget,type=TypeA},
    #t_fun{arity=SameArity,target=SameTarget,type=TypeB}) ->
    #t_fun{arity=SameArity,target=SameTarget,type=join(TypeA, TypeB)};
lub(#t_fun{arity=SameArity,type=TypeA}, #t_fun{arity=SameArity,type=TypeB}) ->
    #t_fun{arity=SameArity,type=join(TypeA, TypeB)};
lub(#t_fun{type=TypeA}, #t_fun{type=TypeB}) ->
    #t_fun{type=join(TypeA, TypeB)};
lub(#t_integer{elements=R1}, #t_integer{elements=R2}) ->
    integer_from_range(lub_ranges(R1, R2));
lub(#t_integer{elements=R1}, #t_float{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_integer{elements=R1}, #t_number{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_list{type=TypeA,terminator=TermA},
    #t_list{type=TypeB,terminator=TermB}) ->
    #t_list{type=join(TypeA, TypeB),terminator=join(TermA, TermB)};
lub(#t_list{}=A, #t_cons{}=B) ->
    lub(B, A);
lub(nil=A, #t_cons{}=B) ->
    lub(B, A);
lub(nil, #t_list{}=T) ->
    T;
lub(#t_list{}=T, nil) ->
    T;
lub(#t_number{elements=R1}, #t_number{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_number{elements=R1}, #t_integer{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_number{elements=R1}, #t_float{elements=R2}) ->
    number_from_range(lub_ranges(R1, R2));
lub(#t_map{super_key=SKeyA,super_value=SValueA},
    #t_map{super_key=SKeyB,super_value=SValueB}) ->
    %% Note the use of join/2; elements don't need to be normal types.
    SKey = join(SKeyA, SKeyB),
    SValue = join(SValueA, SValueB),
    #t_map{super_key=SKey,super_value=SValue};
lub(#t_tuple{size=Sz,exact=ExactA,elements=EsA},
    #t_tuple{size=Sz,exact=ExactB,elements=EsB}) ->
    Exact = ExactA and ExactB,
    Es = lub_tuple_elements(Sz, EsA, EsB),
    #t_tuple{size=Sz,exact=Exact,elements=Es};
lub(#t_tuple{size=SzA,elements=EsA}, #t_tuple{size=SzB,elements=EsB}) ->
    Sz = min(SzA, SzB),
    Es = lub_tuple_elements(Sz, EsA, EsB),
    #t_tuple{size=Sz,elements=Es};
lub(T1, T2) ->
    %%io:format("~p ~p\n", [T1,T2]),
    case is_identifier(T1) andalso is_identifier(T2) of
        true ->
            identifier;
        false ->
            case is_other(T1) andalso is_other(T2) of
                true -> other;
                false -> any
            end
    end.

is_other(Type) ->
    AnyMinusOther = #t_union{atom=#t_atom{},
                             list=#t_list{},
                             number=#t_number{},
                             tuple_set=#t_tuple{},
                             other=none},
    meet(AnyMinusOther, Type) =:= none.

is_identifier(identifier) -> true;
is_identifier(pid) -> true;
is_identifier(port) -> true;
is_identifier(reference) -> true;
is_identifier(_) -> false.

lub_ranges({MinA,MaxA}, {MinB,MaxB}) ->
    {inf_min(MinA, MinB), inf_max(MaxA, MaxB)};
lub_ranges(_, _) ->
    any.

%% Expands integer 0 to `-0.0 .. +0.0`
number_to_float_range({Min, 0}) ->
    number_to_float_range({Min, +0.0});
number_to_float_range({0, Max}) ->
    number_to_float_range({-0.0, Max});
number_to_float_range(Other) ->
    Other.

lub_bs_matchable(UnitA, UnitB) ->
    #t_bs_matchable{tail_unit=gcd(UnitA, UnitB)}.

lub_tuple_elements(MinSize, EsA, EsB) ->
    Es0 = lub_elements(EsA, EsB),
    #{Index => Type || Index := Type <- Es0, Index =< MinSize}.

lub_elements(Es1, Es2) ->
    Keys = if
               map_size(Es1) =< map_size(Es2) -> maps:keys(Es1);
               map_size(Es1) > map_size(Es2) -> maps:keys(Es2)
           end,
    lub_elements_1(Keys, Es1, Es2, #{}).

lub_elements_1([Key | Keys], Es1, Es2, Acc0) ->
    case {Es1, Es2} of
        {#{ Key := Type1 }, #{ Key := Type2 }} ->
            %% Note the use of join/2; elements don't need to be normal types.
            Acc = set_tuple_element(Key, join(Type1, Type2), Acc0),
            lub_elements_1(Keys, Es1, Es2, Acc);
        {#{}, #{}} ->
            lub_elements_1(Keys, Es1, Es2, Acc0)
    end;
lub_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

%%

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

%%%
%%% Handling of ranges.
%%%
%%% A range can begin with '-inf' OR end with '+inf'.
%%%
%%% Atoms are greater than all integers. Therefore, we don't
%%% need any special handling of '+inf'.
%%%

float_from_range(none) ->
    none;
float_from_range(any) ->
    #t_float{};
float_from_range({Min0, Max0}) ->
    true = inf_le(Min0, Max0),                  %Assertion.
    case {safe_float(Min0), safe_float(Max0)} of
        {'-inf','+inf'} ->
            #t_float{};
        {Min, Max} ->
            #t_float{elements={Min, Max}}
    end.

safe_float(N) when is_number(N) ->
    try
        float(N)
    catch
        error:_ when N < 0 -> '-inf';
        error:_ when N > 0 -> '+inf'
    end;
safe_float('-inf'=NegInf) -> NegInf;
safe_float('+inf'=PosInf) -> PosInf.

integer_from_range(none) ->
    none;
integer_from_range(any) ->
    #t_integer{};
integer_from_range({'-inf','+inf'}) ->
    #t_integer{};
integer_from_range({'-inf',Max}) ->
    #t_integer{elements={'-inf',ceil(Max)}};
integer_from_range({Min,'+inf'}) ->
    #t_integer{elements={floor(Min),'+inf'}};
integer_from_range({Min,Max}) ->
    #t_integer{elements={floor(Min),ceil(Max)}}.

number_from_range(N) ->
    case integer_from_range(N) of
        #t_integer{elements=R} ->
            #t_number{elements=R};
        none ->
            none
    end.

inf_le('-inf', _) ->
    true;
inf_le(A, B) when is_float(A), is_float(B) ->
    %% When float ranges are compared to float ranges, the total ordering
    %% function must be used to preserve `-0.0 =/= +0.0`.
    total_compare(A, B, fun erlang:'=<'/2);
inf_le(A, B) ->
    A =< B.

inf_ge(_, '-inf') ->
    true;
inf_ge('-inf', _) ->
    false;
inf_ge(A, B) when is_float(A), is_float(B) ->
    total_compare(A, B, fun erlang:'>='/2);
inf_ge(A, B) ->
    A >= B.

inf_min(A, B) when A =:= '-inf'; B =:= '-inf' ->
    '-inf';
inf_min(A, B) when is_float(A), is_float(B) ->
    case total_compare(A, B, fun erlang:'=<'/2) of
        true -> A;
        false -> B
    end;
inf_min(A, B) ->
    min(A, B).

inf_max('-inf', B) ->
    B;
inf_max(A, '-inf') ->
    A;
inf_max(A, B) when is_float(A), is_float(B) ->
    case total_compare(A, B, fun erlang:'>='/2) of
        true -> A;
        false -> B
    end;
inf_max(A, B) ->
    max(A, B).

total_compare(A, B, Order) ->
    Order(erts_internal:cmp_term(A, B), 0).

%%

record_key(#t_tuple{exact=true,size=Size,elements=#{ 1 := Tag }}) ->
    case is_singleton_type(Tag) of
        true -> {Size, Tag};
        false -> none
    end;
record_key(_) ->
    none.

new_tuple_set(T) ->
    case record_key(T) of
        none -> T;
        Key -> [{Key, T}]
    end.

%%

shrink_union(#t_union{atom=Atom,list=none,number=none,
                      tuple_set=none,other=none}) ->
    Atom;
shrink_union(#t_union{atom=none,list=List,number=none,
                      tuple_set=none,other=none}) ->
    List;
shrink_union(#t_union{atom=none,list=none,number=Number,
                      tuple_set=none,other=none}) ->
    Number;
shrink_union(#t_union{atom=none,list=none,number=none,
                      tuple_set=#t_tuple{}=Tuple,other=none}) ->
    Tuple;
shrink_union(#t_union{atom=none,list=none,number=none,
                      tuple_set=[{_Key, Record}],other=none}) ->
    #t_tuple{} = Record;                        %Assertion.
shrink_union(#t_union{atom=none,list=none,number=none,
                      tuple_set=none,other=Other}) ->
    Other;
shrink_union(#t_union{atom=#t_atom{elements=any},
                      list=#t_list{type=any,terminator=any},
                      number=#t_number{elements=any},
                      tuple_set=#t_tuple{size=0,exact=false},
                      other=other}) ->
    any;
shrink_union(#t_union{}=T) ->
    T.

%% Verifies that the given type is well-formed.

-spec verified_type(T) -> T when
      T :: type().

verified_type(#t_union{atom=Atom,
                       list=List,
                       number=Number,
                       tuple_set=TSet,
                       other=Other}=T) ->
    _ = verified_normal_type(Atom),
    _ = verified_normal_type(List),
    _ = verified_normal_type(Number),
    _ = verify_tuple_set(TSet),
    _ = verified_normal_type(Other),
    T;
verified_type(T) ->
    verified_normal_type(T).

verify_tuple_set([_|_]=T) ->
    _ = verify_tuple_set_1(T, 0),
    T;
verify_tuple_set(#t_tuple{}=T) ->
    none = record_key(T),                       %Assertion.
    T;
verify_tuple_set(none=T) ->
    T.

verify_tuple_set_1([{_Tag, Record} | Records], Size) ->
    true = Size =< ?TUPLE_SET_LIMIT,            %Assertion.
    _ = verified_normal_type(Record),
    verify_tuple_set_1(Records, Size + 1);
verify_tuple_set_1([], _Size) ->
    ok.

-spec verified_normal_type(T) -> T when
      T :: normal_type().

verified_normal_type(any=T) -> T;
verified_normal_type(none=T) -> T;
verified_normal_type(#t_atom{elements=any}=T) -> T;
verified_normal_type(#t_atom{elements=[_|_]}=T) -> T;
verified_normal_type(#t_bitstring{size_unit=U}=T)
  when is_integer(U), U >= 1 ->
    T;
verified_normal_type(#t_bs_context{tail_unit=U}=T)
  when is_integer(U), U >= 1 ->
    T;
verified_normal_type(#t_bs_matchable{tail_unit=U}=T)
  when is_integer(U), U >= 1 ->
    T;
verified_normal_type(#t_cons{type=Type,terminator=Term}=T) ->
    _ = verified_type(Type),
    _ = verified_type(Term),
    T;
verified_normal_type(#t_fun{arity=Arity,
                            target={Name,TotalArity},
                            type=ReturnType}=T)
  when is_integer(Arity),
       is_atom(Name),
       is_integer(TotalArity),
       TotalArity >= Arity ->
    _ = verified_type(ReturnType),
    T;
verified_normal_type(#t_fun{arity=Arity,
                            target=any,
                            type=ReturnType}=T)
  when Arity =:= any; is_integer(Arity) ->
    _ = verified_type(ReturnType),
    T;
verified_normal_type(#t_float{}=T) -> T;
verified_normal_type(#t_integer{elements=any}=T) -> T;
verified_normal_type(#t_integer{elements={'-inf',Max}}=T)
  when is_integer(Max) ->
    T;
verified_normal_type(#t_integer{elements={Min,'+inf'}}=T)
  when is_integer(Min) ->
    T;
verified_normal_type(#t_integer{elements={Min,Max}}=T)
  when is_integer(Min), is_integer(Max), Min =< Max ->
    T;
verified_normal_type(#t_list{type=Type,terminator=Term}=T) ->
    _ = verified_type(Type),
    _ = verified_type(Term),
    T;
verified_normal_type(#t_map{}=T) -> T;
verified_normal_type(nil=T) -> T;
verified_normal_type(#t_number{}=T) -> T;
verified_normal_type(other=T) -> T;
verified_normal_type(pid=T) -> T;
verified_normal_type(port=T) -> T;
verified_normal_type(reference=T) -> T;
verified_normal_type(identifier=T) -> T;
verified_normal_type(#t_tuple{size=Size,elements=Es}=T) ->
    %% All known elements must have a valid index and type (which may be a
    %% union). 'any' is prohibited since it's implicit and should never be
    %% present in the map, and a 'none' element ought to have reduced the
    %% entire tuple to 'none'.
    _ = [verified_type(Element) ||
            Index := Element <- Es,
            is_integer(Index), 1 =< Index, Index =< Size,
            Index =< ?TUPLE_ELEMENT_LIMIT,
            Element =/= any, Element =/= none],
    T.

%%%
%%% External type format
%%%
%%% This is a stripped-down version of our internal format, focusing solely on
%%% primary types and unions thereof. The idea is to help the JIT skip minor
%%% details inside instructions when we know they're pointless, such as
%%% checking whether the source of `is_tagged_tuple` is boxed, or reducing an
%%% equality check to a single machine compare instruction when we know that
%%% both arguments are immediates.
%%%

-define(BEAM_TYPE_ATOM,          (1 bsl 0)).
-define(BEAM_TYPE_BITSTRING,     (1 bsl 1)).
-define(BEAM_TYPE_CONS,          (1 bsl 2)).
-define(BEAM_TYPE_FLOAT,         (1 bsl 3)).
-define(BEAM_TYPE_FUN,           (1 bsl 4)).
-define(BEAM_TYPE_INTEGER,       (1 bsl 5)).
-define(BEAM_TYPE_MAP,           (1 bsl 6)).
-define(BEAM_TYPE_NIL,           (1 bsl 7)).
-define(BEAM_TYPE_PID,           (1 bsl 8)).
-define(BEAM_TYPE_PORT,          (1 bsl 9)).
-define(BEAM_TYPE_REFERENCE,     (1 bsl 10)).
-define(BEAM_TYPE_TUPLE,         (1 bsl 11)).

-define(BEAM_TYPE_HAS_LOWER_BOUND, (1 bsl 12)).
-define(BEAM_TYPE_HAS_UPPER_BOUND, (1 bsl 13)).
-define(BEAM_TYPE_HAS_UNIT,        (1 bsl 14)).

-define(BEAM_TYPES_VERSION_27, ?BEAM_TYPES_VERSION).
-define(BEAM_TYPES_VERSION_26, 2).
-define(BEAM_TYPES_VERSION_25, 1).

-spec convert_ext(pos_integer(), binary()) -> binary() | 'none'.
convert_ext(?BEAM_TYPES_VERSION_27, Types) ->
    Types;
convert_ext(?BEAM_TYPES_VERSION_26, Types) ->
    convert_ext(?BEAM_TYPES_VERSION_27, convert_ext_26(Types, <<>>));
convert_ext(?BEAM_TYPES_VERSION_25, Types0) ->
    NumberMask = (?BEAM_TYPE_FLOAT bor ?BEAM_TYPE_INTEGER),
    Types = << case Min =< Max of
                   true ->
                       true = 0 =/= (TypeBits0 band NumberMask), %Assertion.
                       TypeBits = TypeBits0 bor
                           ((?BEAM_TYPE_HAS_LOWER_BOUND bor
                                 ?BEAM_TYPE_HAS_UPPER_BOUND) bsl 1),
                       <<TypeBits:16,Min:64/signed,Max:64/signed>>;
                   false ->
                       <<TypeBits0:16>>
               end || <<TypeBits0:16,Min:64/signed,Max:64/signed>> <:= Types0 >>,
    convert_ext(?BEAM_TYPES_VERSION_26, Types);
convert_ext(_Version, _Types) ->
    none.

convert_ext_26(<<TypeBits0:16/big,More/binary>>, Types) ->
    true = TypeBits0 =/= 0,                      %Assertion.
    %% OTP 27 removed #t_bs_context{} from the type information, which used to
    %% occupy bit 2. As these are now considered to be a regular bitstring that
    %% happens to be mutable, we'll combine it with the #t_bitstring{} type
    %% bit.
    TypeBits = (TypeBits0 band 3) bor ((TypeBits0 band (bnot 3)) bsr 1),

    Res = foldl(fun({Id, Type}, Acc) ->
                        decode_ext_bits(TypeBits, Id, Type, Acc)
                end, none, ext_type_mapping()),
    {[Min, Max, Unit], Rest} = decode_extra(TypeBits, More),
    R = case {Min,Max} of
            {'-inf','+inf'} -> any;
            R0 -> R0
        end,

    Encoded = encode_ext(decode_fix(Res, R, Unit)),
    convert_ext_26(Rest, <<Types/bits, Encoded/bits>>);
convert_ext_26(<<>>, Types) ->
    Types.

ext_type_mapping() ->
    [{?BEAM_TYPE_ATOM,          #t_atom{}},
     {?BEAM_TYPE_BITSTRING,     #t_bs_matchable{}},
     {?BEAM_TYPE_CONS,          #t_cons{}},
     {?BEAM_TYPE_FLOAT,         #t_float{}},
     {?BEAM_TYPE_FUN,           #t_fun{}},
     {?BEAM_TYPE_INTEGER,       #t_integer{}},
     {?BEAM_TYPE_MAP,           #t_map{}},
     {?BEAM_TYPE_NIL,           nil},
     {?BEAM_TYPE_PID,           pid},
     {?BEAM_TYPE_PORT,          port},
     {?BEAM_TYPE_REFERENCE,     reference},
     {?BEAM_TYPE_TUPLE,         #t_tuple{}}].

-spec decode_ext(binary()) -> {type(),binary()} | 'done'.
decode_ext(<<TypeBits:16/big,More/binary>>) ->
    true = TypeBits =/= 0,                      %Assertion.
    Res = foldl(fun({Id, Type}, Acc) ->
                        decode_ext_bits(TypeBits, Id, Type, Acc)
                end, none, ext_type_mapping()),
    {[Min,Max,Unit],Extra} = decode_extra(TypeBits, More),
    R = case {Min,Max} of
            {'-inf','+inf'} -> any;
            R0 -> R0
        end,
    {decode_fix(Res, R, Unit),Extra};
decode_ext(<<>>) ->
    done.

decode_ext_bits(Input, TypeBit, Type, Acc) ->
    case Input band TypeBit of
        0 -> Acc;
        _ -> join(Type, Acc)
    end.

decode_extra(TypeBits, Extra) ->
    L = [{?BEAM_TYPE_HAS_LOWER_BOUND, 64, signed, '-inf'},
         {?BEAM_TYPE_HAS_UPPER_BOUND, 64, signed, '+inf'},
         {?BEAM_TYPE_HAS_UNIT, 8, unsigned, 1}],
    mapfoldl(fun({Bit,Size,Spec,Default}, Acc0) ->
                     case {TypeBits band Bit, Spec, Acc0} of
                         {Bit, unsigned, <<Value:Size/unsigned,Acc/binary>>} ->
                             {Value, Acc};
                         {Bit, signed, <<Value:Size/signed,Acc/binary>>} ->
                             {Value, Acc};
                         {0, _Spec, <<_/binary>>} ->
                             {Default,Acc0}
                     end
             end, Extra, L).

decode_fix(#t_integer{}, Range, _Unit) ->
    #t_integer{elements=Range};
decode_fix(#t_number{}, Range, _Unit) ->
    #t_number{elements=Range};
decode_fix(#t_bitstring{}, _Range, Unit) ->
    #t_bitstring{size_unit=Unit+1};
decode_fix(#t_union{}=Type0, Range, Unit) ->
    Type1 = case meet(Type0, #t_integer{}) of
                #t_integer{} ->
                    Type0#t_union{number=#t_integer{elements=Range}};
                _ ->
                    Type0
            end,
    case meet(Type1, #t_bitstring{}) of
        #t_bitstring{} ->
            Type1#t_union{other=#t_bitstring{size_unit=Unit}};
        _ ->
            Type1
    end;
decode_fix(Type, _, _) ->
    Type.

-spec encode_ext(type()) -> binary().
encode_ext(Input) ->
    TypeBits0 = foldl(fun({Id, Type}, Acc) ->
                              encode_ext_bits(Input, Id, Type, Acc)
                      end, 0, ext_type_mapping()),
    {TypeBits1,Extra} = encode_extra(Input),
    TypeBits = TypeBits0 bor TypeBits1,
    true = TypeBits =/= 0,                      %Assertion.
    <<TypeBits:16,Extra/binary>>.

encode_ext_bits(Input, TypeBit, Type, Acc) ->
    case meet(Input, Type) of
        none -> Acc;
        _ -> Acc bor TypeBit
    end.

encode_extra(Input) ->
    {TypeBits0,Extra0} = encode_range(Input),
    {TypeBits1,Extra1} = encode_unit(Input),
    {TypeBits0 bor TypeBits1,<<Extra0/binary,Extra1/binary>>}.

encode_range(#t_integer{elements={Min,Max}}) ->
    encode_range(Min, Max);
encode_range(#t_number{elements={Min,Max}}) ->
    encode_range(Min, Max);
encode_range(#t_union{number=N}) ->
    encode_range(N);
encode_range(_) ->
    {0,<<>>}.

encode_range(Min, Max) ->
    case is_small(Min) of
        true ->
            encode_range(Max, ?BEAM_TYPE_HAS_LOWER_BOUND, <<Min:64>>);
        false ->
            encode_range(Max, 0, <<>>)
    end.

encode_range(Max, TypeBits, Extra) ->
    case is_small(Max) of
        true ->
            {TypeBits bor ?BEAM_TYPE_HAS_UPPER_BOUND,
             <<Extra/binary,Max:64>>};
        false ->
            {TypeBits,Extra}
    end.

encode_unit(#t_bitstring{size_unit=Unit}) ->
    true = is_integer(Unit) andalso 0 < Unit andalso Unit =< 256, %Assertion.
    {?BEAM_TYPE_HAS_UNIT,<<(Unit-1):8>>};
encode_unit(#t_union{other=Other}) ->
    encode_unit(Other);
encode_unit(_) ->
    {0,<<>>}.

%% Test whether the number is a small on a 64-bit machine.
%% (Normally the compiler doesn't know/doesn't care whether something is
%% bignum, but because the type representation is versioned this is safe.)
is_small(N) when is_integer(N), -(1 bsl 59) =< N andalso N =< (1 bsl 59) - 1 ->
    true;
is_small(_) ->
    false.
