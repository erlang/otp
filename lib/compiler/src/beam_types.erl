%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019. All Rights Reserved.
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

-include("beam_types.hrl").

-import(lists, [foldl/3]).

-export([meet/1, meet/2, join/1, join/2, subtract/2]).

-export([get_singleton_value/1,
         get_tuple_size/1,
         is_singleton_type/1,
         is_boolean_type/1]).

-export([get_element_type/2, set_element_type/3]).

-export([make_type_from_value/1]).

-export([make_atom/1,
         make_boolean/0,
         make_integer/1,
         make_integer/2,
         make_tuple/2,
         make_tuple/3]).

%% Return the "join" of Type1 and Type2. The join is a more general
%% type than Type1 and Type2. For example:
%%
%%    join(#t_integer{elements=any}, #t_integer=elements={0,3}}) ->
%%         #t_integer{}
%%
%% The join for two different types result in 'any', which is
%% the top element for our type lattice:
%%
%%    join(#t_integer{}, #t_map{}) -> any

-spec join(type(), type()) -> type().

join(T, T) ->
    verified_type(T);
join(none, T) ->
    verified_type(T);
join(T, none) ->
    verified_type(T);
join(any, _) -> any;
join(_, any) -> any;
join(#t_atom{elements=[_|_]=Set1}, #t_atom{elements=[_|_]=Set2}) ->
    Set = ordsets:union(Set1, Set2),
    case ordsets:size(Set) of
        Size when Size =< ?ATOM_SET_SIZE ->
            #t_atom{elements=Set};
        _Size ->
            #t_atom{elements=any}
    end;
join(#t_atom{elements=any}=T, #t_atom{elements=[_|_]}) -> T;
join(#t_atom{elements=[_|_]}, #t_atom{elements=any}=T) -> T;
join(#t_bitstring{unit=U1}, #t_bitstring{unit=U2}) ->
    #t_bitstring{unit=gcd(U1, U2)};
join(#t_fun{}, #t_fun{}) ->
    #t_fun{};
join(#t_integer{elements={MinA,MaxA}},
     #t_integer{elements={MinB,MaxB}}) ->
    #t_integer{elements={min(MinA,MinB),max(MaxA,MaxB)}};
join(#t_bs_context{slots=SlotsA,valid=ValidA},
     #t_bs_context{slots=SlotsB,valid=ValidB}) ->
    #t_bs_context{slots=min(SlotsA, SlotsB),
                  valid=ValidA band ValidB};
join(#t_integer{}, #t_integer{}) -> #t_integer{};
join(list, cons) -> list;
join(cons, list) -> list;
join(nil, cons) -> list;
join(cons, nil) -> list;
join(nil, list) -> list;
join(list, nil) -> list;
join(#t_integer{}, float) -> number;
join(float, #t_integer{}) -> number;
join(#t_integer{}, number) -> number;
join(number, #t_integer{}) -> number;
join(float, number) -> number;
join(number, float) -> number;
join(#t_tuple{size=Sz,exact=ExactA,elements=EsA},
     #t_tuple{size=Sz,exact=ExactB,elements=EsB}) ->
    Exact = ExactA and ExactB,
    Es = join_tuple_elements(Sz, EsA, EsB),
    #t_tuple{size=Sz,exact=Exact,elements=Es};
join(#t_tuple{size=SzA,elements=EsA}, #t_tuple{size=SzB,elements=EsB}) ->
    Sz = min(SzA, SzB),
    Es = join_tuple_elements(Sz, EsA, EsB),
    #t_tuple{size=Sz,elements=Es};
join(_T1, _T2) ->
    %%io:format("~p ~p\n", [_T1,_T2]),
    any.

join_tuple_elements(MinSize, EsA, EsB) ->
    Es0 = join_elements(EsA, EsB),
    maps:filter(fun(Index, _Type) -> Index =< MinSize end, Es0).

join_elements(Es1, Es2) ->
    Keys = if
               map_size(Es1) =< map_size(Es2) -> maps:keys(Es1);
               map_size(Es1) > map_size(Es2) -> maps:keys(Es2)
           end,
    join_elements_1(Keys, Es1, Es2, #{}).

join_elements_1([Key | Keys], Es1, Es2, Acc0) ->
    case {Es1, Es2} of
        {#{ Key := Type1 }, #{ Key := Type2 }} ->
            Acc = set_element_type(Key, join(Type1, Type2), Acc0),
            join_elements_1(Keys, Es1, Es2, Acc);
        {#{}, #{}} ->
            join_elements_1(Keys, Es1, Es2, Acc0)
    end;
join_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

%% Joins all the given types into a single type.
-spec join([type()]) -> type().

join([T1,T2|Ts]) ->
    join([join(T1, T2)|Ts]);
join([T]) -> T.

%% Return the "meet" of Type1 and Type2. The meet is a narrower
%% type than Type1 and Type2. For example:
%%
%%    meet(#t_integer{elements=any}, #t_integer{elements={0,3}}) ->
%%         #t_integer{elements={0,3}}
%%
%% The meet for two different types result in 'none', which is
%% the bottom element for our type lattice:
%%
%%    meet(#t_integer{}, #t_map{}) -> none

-spec meet(type(), type()) -> type().

meet(T, T) ->
    verified_type(T);
meet(#t_atom{elements=[_|_]=Set1}, #t_atom{elements=[_|_]=Set2}) ->
    case ordsets:intersection(Set1, Set2) of
        [] ->
            none;
        [_|_]=Set ->
            #t_atom{elements=Set}
    end;
meet(#t_atom{elements=[_|_]}=T, #t_atom{elements=any}) ->
    T;
meet(#t_atom{elements=any}, #t_atom{elements=[_|_]}=T) ->
    T;
meet(#t_fun{arity=any}, #t_fun{}=T) ->
    T;
meet(#t_fun{}=T, #t_fun{arity=any}) ->
    T;
meet(#t_integer{elements={_,_}}=T, #t_integer{elements=any}) ->
    T;
meet(#t_integer{elements=any}, #t_integer{elements={_,_}}=T) ->
    T;
meet(#t_integer{elements={MinA,MaxA}}, #t_integer{elements={MinB,MaxB}})
  when MinA >= MinB, MaxA =< MaxB;
       MinB >= MinA, MaxB =< MaxA ->
    #t_integer{elements={max(MinA, MinB),min(MaxA, MaxB)}};
meet(#t_integer{}=T, number) -> T;
meet(float=T, number) -> T;
meet(number, #t_integer{}=T) -> T;
meet(number, float=T) -> T;
meet(list, cons) -> cons;
meet(list, nil) -> nil;
meet(cons, list) -> cons;
meet(nil, list) -> nil;
meet(#t_tuple{}=T1, #t_tuple{}=T2) ->
    meet_tuples(T1, T2);
meet(#t_bitstring{unit=U1}, #t_bitstring{unit=U2}) ->
    #t_bitstring{unit=U1 * U2 div gcd(U1, U2)};
meet(any, T) ->
    verified_type(T);
meet(T, any) ->
    verified_type(T);
meet(_, _) ->
    %% Inconsistent types. There will be an exception at runtime.
    none.

meet_tuples(#t_tuple{size=Sz1,exact=true},
            #t_tuple{size=Sz2,exact=true}) when Sz1 =/= Sz2 ->
    none;
meet_tuples(#t_tuple{size=Sz1,exact=Ex1,elements=Es1},
            #t_tuple{size=Sz2,exact=Ex2,elements=Es2}) ->
    Size = max(Sz1, Sz2),
    Exact = Ex1 or Ex2,
    case meet_elements(Es1, Es2) of
        none ->
            none;
        Es ->
            #t_tuple{size=Size,exact=Exact,elements=Es}
    end.

meet_elements(Es1, Es2) ->
    Keys = maps:keys(Es1) ++ maps:keys(Es2),
    meet_elements_1(Keys, Es1, Es2, #{}).

meet_elements_1([Key | Keys], Es1, Es2, Acc) ->
    case {Es1, Es2} of
        {#{ Key := Type1 }, #{ Key := Type2 }} ->
            case meet(Type1, Type2) of
                none -> none;
                Type -> meet_elements_1(Keys, Es1, Es2, Acc#{ Key => Type })
            end;
        {#{ Key := Type1 }, _} ->
            meet_elements_1(Keys, Es1, Es2, Acc#{ Key => Type1 });
        {_, #{ Key := Type2 }} ->
            meet_elements_1(Keys, Es1, Es2, Acc#{ Key => Type2 })
    end;
meet_elements_1([], _Es1, _Es2, Acc) ->
    Acc.

%% Meets all the given types into a single type.
-spec meet([type()]) -> type().

meet([T1,T2|Ts]) ->
    meet([meet(T1, T2)|Ts]);
meet([T]) -> T.

%% Subtract Type2 from Type1. Example:
%%    subtract(list, cons) -> nil

-spec subtract(type(), type()) -> type().

subtract(#t_atom{elements=[_|_]=Set0}, #t_atom{elements=[_|_]=Set1}) ->
    case ordsets:subtract(Set0, Set1) of
        [] -> none;
        [_|_]=Set -> #t_atom{elements=Set}
    end;
subtract(number, float) -> #t_integer{};
subtract(number, #t_integer{elements=any}) -> float;
subtract(list, cons) -> nil;
subtract(list, nil) -> cons;
subtract(T, _) -> T.

%% Verifies that the given type is well-formed.

-spec verified_type(T) -> T when
      T :: type().

verified_type(any=T) -> T;
verified_type(none=T) -> T;
verified_type(#t_atom{elements=any}=T) -> T;
verified_type(#t_atom{elements=[_|_]}=T) -> T;
verified_type(#t_bitstring{unit=U}=T) when is_integer(U), U >= 1 -> T;
verified_type(#t_bs_context{}=T) -> T;
verified_type(#t_fun{arity=Arity}=T) when Arity =:= any; is_integer(Arity) -> T;
verified_type(#t_integer{elements=any}=T) -> T;
verified_type(#t_integer{elements={Min,Max}}=T)
  when is_integer(Min), is_integer(Max), Min =< Max -> T;
verified_type(list=T) -> T;
verified_type(#t_map{}=T) -> T;
verified_type(nil=T) -> T;
verified_type(cons=T) -> T;
verified_type(number=T) -> T;
verified_type(#t_tuple{size=Size,elements=Es}=T) ->
    %% All known elements must have a valid index and type. 'any' is prohibited
    %% since it's implicit and should never be present in the map.
    maps:fold(fun(Index, Element, _) when is_integer(Index),
                                          1 =< Index, Index =< Size,
                                          Element =/= any, Element =/= none ->
                      verified_type(Element)
              end, [], Es),
    T;
verified_type(float=T) -> T.

%%%
%%% Type operators
%%%

-spec get_singleton_value(Type) -> Result when
      Type :: type(),
      Result :: {ok, term()} | error.
get_singleton_value(#t_atom{elements=[Atom]}) ->
    {ok, Atom};
get_singleton_value(#t_integer{elements={Int,Int}}) ->
    {ok, Int};
get_singleton_value(nil) ->
    {ok, []};
get_singleton_value(_) ->
    error.

-spec get_tuple_size(Type) -> Result when
      Type :: type(),
      Result :: none | {at_least, Size} | {exact, Size},
      Size :: non_neg_integer().
get_tuple_size(#t_tuple{size=Size,exact=false}) ->
    {at_least,Size};
get_tuple_size(#t_tuple{size=Size,exact=true}) ->
    {exact,Size};
get_tuple_size(_) ->
    none.

-spec is_boolean_type(type()) -> boolean().
is_boolean_type(#t_atom{elements=[F,T]}) ->
    F =:= false andalso T =:= true;
is_boolean_type(#t_atom{elements=[B]}) ->
    is_boolean(B);
is_boolean_type(_) -> false.

-spec is_singleton_type(type()) -> boolean().
is_singleton_type(Type) ->
    get_singleton_value(Type) =/= error.

-spec set_element_type(Key, Type, Elements) -> Elements when
      Key :: term(),
      Type :: type(),
      Elements :: elements().
set_element_type(_Key, none, Es) ->
    Es;
set_element_type(Key, any, Es) ->
    maps:remove(Key, Es);
set_element_type(Key, Type, Es) ->
    Es#{ Key => Type }.

-spec get_element_type(Key, Elements) -> type() when
      Key :: term(),
      Elements :: elements().
get_element_type(Index, Es) ->
    case Es of
        #{ Index := T } -> T;
        #{} -> any
    end.

%%%
%%% Type constructors
%%%

-spec make_type_from_value(term()) -> type().
make_type_from_value(Value) ->
    mtfv_1(Value).

mtfv_1([]) -> nil;
mtfv_1([_|_]) -> cons;
mtfv_1(A) when is_atom(A) -> #t_atom{elements=[A]};
mtfv_1(B) when is_binary(B) -> #t_bitstring{unit=8};
mtfv_1(B) when is_bitstring(B) -> #t_bitstring{};
mtfv_1(F) when is_float(F) -> float;
mtfv_1(F) when is_function(F) ->
    {arity, Arity} = erlang:fun_info(F, arity),
    #t_fun{arity=Arity};
mtfv_1(I) when is_integer(I) -> make_integer(I);
mtfv_1(M) when is_map(M) -> #t_map{};
mtfv_1(T) when is_tuple(T) ->
    {Es,_} = foldl(fun(Val, {Es0, Index}) ->
                           Type = mtfv_1(Val),
                           Es = set_element_type(Index, Type, Es0),
                           {Es, Index + 1}
                   end, {#{}, 1}, tuple_to_list(T)),
    #t_tuple{exact=true,size=tuple_size(T),elements=Es};
mtfv_1(_Term) ->
    any.

-spec make_atom(atom()) -> type().
make_atom(Atom) when is_atom(Atom) ->
    #t_atom{elements=[Atom]}.

-spec make_boolean() -> type().
make_boolean() ->
    #t_atom{elements=[false,true]}.

-spec make_integer(integer()) -> type().
make_integer(Int) when is_integer(Int) ->
    make_integer(Int, Int).

-spec make_integer(Min, Max) -> type() when
      Min :: integer(),
      Max :: integer().
make_integer(Min, Max) when is_integer(Min), is_integer(Max), Min =< Max ->
    #t_integer{elements={Min,Max}}.

-spec make_tuple(Size, Exact) -> type() when
      Size :: non_neg_integer(),
      Exact :: boolean().
make_tuple(Size, Exact) ->
    make_tuple(Size, Exact, #{}).

-spec make_tuple(Size, Exact, Elements) -> type() when
      Size :: non_neg_integer(),
      Exact :: boolean(),
      Elements :: #{ non_neg_integer() => type() }.
make_tuple(Size, Exact, Elements) ->
    #t_tuple{size=Size,
             exact=Exact,
             elements=Elements}.
