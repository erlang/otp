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

-module(beam_call_types).

-include("beam_types.hrl").

-import(lists, [duplicate/2,foldl/3]).

-export([types/3]).

%%
%% Returns the inferred return and argument types for known functions, and
%% whether it's safe to subtract argument types on failure.
%%
%% Note that the return type will be 'none' if we can statically determine that
%% the function will fail at runtime.
%%

-spec types(Mod, Func, ArgTypes) -> {RetType, ArgTypes, CanSubtract} when
      Mod :: atom(),
      Func :: atom(),
      ArgTypes :: [type()],
      RetType :: type(),
      CanSubtract :: boolean().

%% Functions that only fail due to bad argument *types*, meaning it's safe to
%% subtract argument types on failure.
%%
%% Note that these are all from the erlang module; suitable functions in other
%% modules could fail due to the module not being loaded.
types(erlang, 'map_size', [_]) ->
    sub_safe(#t_integer{}, [#t_map{}]);
types(erlang, 'tuple_size', [_]) ->
    sub_safe(#t_integer{}, [#t_tuple{}]);
types(erlang, 'bit_size', [_]) ->
    sub_safe(#t_integer{}, [#t_bitstring{}]);
types(erlang, 'byte_size', [_]) ->
    sub_safe(#t_integer{}, [#t_bitstring{}]);
types(erlang, 'hd', [_]) ->
    sub_safe(any, [cons]);
types(erlang, 'tl', [_]) ->
    sub_safe(any, [cons]);
types(erlang, 'length', [_]) ->
    sub_safe(#t_integer{}, [list]);
types(erlang, 'not', [_]) ->
    Bool = beam_types:make_boolean(),
    sub_safe(Bool, [Bool]);

%% Boolean ops
types(erlang, 'and', [_,_]) ->
    Bool = beam_types:make_boolean(),
    sub_unsafe(Bool, [Bool, Bool]);
types(erlang, 'or', [_,_]) ->
    Bool = beam_types:make_boolean(),
    sub_unsafe(Bool, [Bool, Bool]);
types(erlang, 'xor', [_,_]) ->
    Bool = beam_types:make_boolean(),
    sub_unsafe(Bool, [Bool, Bool]);

%% Bitwise ops
types(erlang, 'band', [_,_]=Args) ->
    sub_unsafe(band_return_type(Args), [#t_integer{}, #t_integer{}]);
types(erlang, 'bor', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'bxor', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'bsl', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'bsr', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'bnot', [_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}]);

%% Fixed-type arithmetic
types(erlang, 'float', [_]) ->
    sub_unsafe(float, [number]);
types(erlang, 'round', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, 'floor', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, 'ceil', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, 'trunc', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, '/', [_,_]) ->
    sub_unsafe(float, [number, number]);
types(erlang, 'div', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'rem', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);

%% Mixed-type arithmetic; '+'/2 and friends are handled in the catch-all
%% clause for the 'erlang' module.
types(erlang, 'abs', [_]=Args) ->
    mixed_arith_types(Args);

%% List operations
types(erlang, '++', [LHS,RHS]) ->
    %% `[] ++ RHS` yields RHS, even if RHS is not a list.
    RetType = case {LHS, RHS} of
                  {cons, _} -> cons;
                  {_, cons} -> cons;
                  _ -> beam_types:join(list, RHS)
              end,
    sub_unsafe(RetType, [list, any]);
types(erlang, '--', [_,_]) ->
    sub_unsafe(list, [list, list]);

%% Misc ops.
types(erlang, 'binary_part', [_, _]) ->
    PosLen = make_two_tuple(#t_integer{}, #t_integer{}),
    Binary = #t_bitstring{unit=8},
    sub_unsafe(Binary, [Binary, PosLen]);
types(erlang, 'binary_part', [_, _, _]) ->
    Binary = #t_bitstring{unit=8},
    sub_unsafe(Binary, [Binary, #t_integer{}, #t_integer{}]);
types(erlang, 'is_map_key', [_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [any,#t_map{}]);
types(erlang, 'map_get', [_,_]) ->
    sub_unsafe(any, [any,#t_map{}]);
types(erlang, 'node', [_]) ->
    sub_unsafe(#t_atom{}, [any]);
types(erlang, 'node', []) ->
    sub_unsafe(#t_atom{}, []);
types(erlang, 'size', [_]) ->
    sub_unsafe(#t_integer{}, [any]);
types(erlang, 'size', [_]) ->
    sub_unsafe(#t_integer{}, [any]);

%% Tuple element ops
types(erlang, element, [PosType, TupleType]) ->
    Index = case PosType of
                #t_integer{elements={Same,Same}} when is_integer(Same) ->
                    Same;
                _ ->
                    0
            end,

    RetType = case TupleType of
                  #t_tuple{size=Sz,elements=Es} when Index =< Sz,
                                                     Index >= 1 ->
                      beam_types:get_element_type(Index, Es);
                  _ ->
                      any
              end,

    sub_unsafe(RetType, [#t_integer{}, #t_tuple{size=Index}]);
types(erlang, setelement, [PosType, TupleType, ArgType]) ->
    RetType = case {PosType,TupleType} of
                  {#t_integer{elements={Index,Index}},
                   #t_tuple{elements=Es0,size=Size}=T} ->
                      %% This is an exact index, update the type of said
                      %% element or return 'none' if it's known to be out of
                      %% bounds.
                      Es = beam_types:set_element_type(Index, ArgType, Es0),
                      case T#t_tuple.exact of
                          false ->
                              T#t_tuple{size=max(Index, Size),elements=Es};
                          true when Index =< Size ->
                              T#t_tuple{elements=Es};
                          true ->
                              none
                      end;
                  {#t_integer{elements={Min,Max}},
                   #t_tuple{elements=Es0,size=Size}=T} ->
                      %% We know this will land between Min and Max, so kill
                      %% the types for those indexes.
                      Es = discard_tuple_element_info(Min, Max, Es0),
                      case T#t_tuple.exact of
                          false ->
                              T#t_tuple{elements=Es,size=max(Min, Size)};
                          true when Min =< Size ->
                              T#t_tuple{elements=Es,size=Size};
                          true ->
                              none
                      end;
                  {_,#t_tuple{}=T} ->
                      %% Position unknown, so we have to discard all element
                      %% information.
                      T#t_tuple{elements=#{}};
                  {#t_integer{elements={Min,_Max}},_} ->
                      #t_tuple{size=Min};
                  {_,_} ->
                      #t_tuple{}
              end,
    sub_unsafe(RetType, [#t_integer{}, #t_tuple{}, any]);

types(erlang, make_fun, [_,_,Arity0]) ->
    Type = case Arity0 of
               #t_integer{elements={Arity,Arity}} when Arity >= 0 ->
                   #t_fun{arity=Arity};
               _ ->
                   #t_fun{}
           end,
    sub_unsafe(Type, [#t_atom{}, #t_atom{}, #t_integer{}]);

types(erlang, Name, Args) ->
    Arity = length(Args),

    case erl_bifs:is_exit_bif(erlang, Name, Arity) of
        true ->
            {none, Args, false};
        false ->
            case erl_internal:arith_op(Name, Arity) of
                true ->
                    mixed_arith_types(Args);
                false ->
                    IsTest =
                        erl_internal:new_type_test(Name, Arity) orelse
                        erl_internal:comp_op(Name, Arity),

                    RetType = case IsTest of
                                  true -> beam_types:make_boolean();
                                  false -> any
                              end,

                    sub_unsafe(RetType, duplicate(Arity, any))
            end
    end;

%%
%% Math BIFs
%%

types(math, cos, [_]) ->
    sub_unsafe(float, [number]);
types(math, cosh, [_]) ->
    sub_unsafe(float, [number]);
types(math, sin, [_]) ->
    sub_unsafe(float, [number]);
types(math, sinh, [_]) ->
    sub_unsafe(float, [number]);
types(math, tan, [_]) ->
    sub_unsafe(float, [number]);
types(math, tanh, [_]) ->
    sub_unsafe(float, [number]);
types(math, acos, [_]) ->
    sub_unsafe(float, [number]);
types(math, acosh, [_]) ->
    sub_unsafe(float, [number]);
types(math, asin, [_]) ->
    sub_unsafe(float, [number]);
types(math, asinh, [_]) ->
    sub_unsafe(float, [number]);
types(math, atan, [_]) ->
    sub_unsafe(float, [number]);
types(math, atanh, [_]) ->
    sub_unsafe(float, [number]);
types(math, erf, [_]) ->
    sub_unsafe(float, [number]);
types(math, erfc, [_]) ->
    sub_unsafe(float, [number]);
types(math, exp, [_]) ->
    sub_unsafe(float, [number]);
types(math, log, [_]) ->
    sub_unsafe(float, [number]);
types(math, log2, [_]) ->
    sub_unsafe(float, [number]);
types(math, log10, [_]) ->
    sub_unsafe(float, [number]);
types(math, sqrt, [_]) ->
    sub_unsafe(float, [number]);
types(math, atan2, [_,_]) ->
    sub_unsafe(float, [number, number]);
types(math, pow, [_,_]) ->
    sub_unsafe(float, [number, number]);
types(math, ceil, [_]) ->
    sub_unsafe(float, [number]);
types(math, floor, [_]) ->
    sub_unsafe(float, [number]);
types(math, fmod, [_,_]) ->
    sub_unsafe(float, [number, number]);
types(math, pi, []) ->
    sub_unsafe(float, []);

%%
%% List functions
%%

%% Operator aliases.
types(lists, append, [_,_]=Args) ->
    types(erlang, '++', Args);
types(lists, append, [_]) ->
    %% This is implemented through folding the list over erlang:'++'/2, so it
    %% can hypothetically return anything, but we can infer that its argument
    %% is a list on success.
    sub_unsafe(any, [list]);
types(lists, subtract, [_,_]) ->
    sub_unsafe(list, [list, list]);

%% Functions returning booleans.
types(lists, all, [_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [#t_fun{arity=1}, list]);
types(lists, any, [_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [#t_fun{arity=1}, list]);
types(lists, keymember, [_,_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [any, #t_integer{}, list]);
types(lists, member, [_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [any, list]);
types(lists, prefix, [_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [list, list]);
types(lists, suffix, [_,_]) ->
    sub_unsafe(beam_types:make_boolean(), [list, list]);

%% Functions returning plain lists.
types(lists, dropwhile, [_,_]) ->
    sub_unsafe(list, [#t_fun{arity=1}, list]);
types(lists, duplicate, [_,_]) ->
    sub_unsafe(list, [#t_integer{}, any]);
types(lists, filter, [_,_]) ->
    sub_unsafe(list, [#t_fun{arity=1}, list]);
types(lists, flatten, [_]) ->
    sub_unsafe(list, [list]);
types(lists, map, [_Fun, List]) ->
    sub_unsafe(same_length_type(List), [#t_fun{arity=1}, list]);
types(lists, reverse, [List]) ->
    sub_unsafe(same_length_type(List), [list]);
types(lists, sort, [List]) ->
    sub_unsafe(same_length_type(List), [list]);
types(lists, takewhile, [_,_]) ->
    sub_unsafe(list, [#t_fun{arity=1}, list]);
types(lists, usort, [List]) ->
    sub_unsafe(same_length_type(List), [list]);
types(lists, zip, [A,B]) ->
    ZipType = lists_zip_type([A,B]),
    sub_unsafe(ZipType, [ZipType, ZipType]);
types(lists, zip3, [A,B,C]) ->
    ZipType = lists_zip_type([A,B,C]),
    sub_unsafe(ZipType, [ZipType, ZipType, ZipType]);
types(lists, zipwith, [_,A,B]) ->
    ZipType = lists_zip_type([A,B]),
    sub_unsafe(ZipType, [#t_fun{arity=2}, ZipType, ZipType]);
types(lists, zipwith3, [_,A,B,C]) ->
    ZipType = lists_zip_type([A,B,C]),
    sub_unsafe(ZipType, [#t_fun{arity=3}, ZipType, ZipType, ZipType]);

%% Functions with complex return values.
types(lists, partition, [_,_]) ->
    sub_unsafe(make_two_tuple(list, list), [#t_fun{arity=1}, list]);
types(lists, MapFold, [_Fun, _Init, List])
  when MapFold =:= mapfoldl; MapFold =:= mapfoldr ->
    ListType = same_length_type(List),
    RetType = #t_tuple{size=2,
                       exact=true,
                       elements=#{ 1 => ListType }},
    sub_unsafe(RetType, [#t_fun{arity=2}, any, list]);
types(lists, splitwith, [_,_]) ->
    sub_unsafe(make_two_tuple(list, list), [#t_fun{arity=1}, list]);
types(lists, unzip, [List]) ->
    ListType = same_length_type(List),
    RetType = make_two_tuple(ListType, ListType),
    sub_unsafe(RetType, [list]);

%% Catch-all clause for unknown functions.

types(_, _, Args) ->
    sub_unsafe(any, [any || _ <- Args]).

%%
%% Helpers
%%

sub_unsafe(none, ArgTypes) ->
    %% This is known to fail at runtime, but the type optimization pass
    %% doesn't yet support cutting a block short at any point, so we
    %% pretend it's raining instead.
    %%
    %% Actual exit BIFs get special treatment in the catch-all clause
    %% for the 'erlang' module.
    sub_unsafe(any, ArgTypes);
sub_unsafe(RetType, ArgTypes) ->
    {RetType, ArgTypes, false}.

sub_safe(RetType, ArgTypes) ->
    {RetType, ArgTypes, true}.

mixed_arith_types([FirstType | _]=Args0) ->
    RetType = foldl(fun(#t_integer{}, #t_integer{}) -> #t_integer{};
                       (#t_integer{}, number) -> number;
                       (#t_integer{}, float) -> float;
                       (float, #t_integer{}) -> float;
                       (float, number) -> float;
                       (float, float) -> float;
                       (number, #t_integer{}) -> number;
                       (number, float) -> float;
                       (number, number) -> number;
                       (any, _) -> number;
                       (_, _) -> none
                    end, FirstType, Args0),
    sub_unsafe(RetType, [number || _ <- Args0]).

band_return_type([#t_integer{elements={Int,Int}}, RHS]) when is_integer(Int) ->
    band_return_type_1(RHS, Int);
band_return_type([LHS, #t_integer{elements={Int,Int}}]) when is_integer(Int) ->
    band_return_type_1(LHS, Int);
band_return_type(_) ->
    #t_integer{}.

band_return_type_1(LHS, Int) ->
    case LHS of
        #t_integer{elements={Min0,Max0}} when Max0 - Min0 < 1 bsl 256 ->
            {Intersection, Union} = range_masks(Min0, Max0),

            Min = Intersection band Int,
            Max = min(Max0, Union band Int),

            #t_integer{elements={Min,Max}};
        _ when Int >= 0 ->
            %% The range is either unknown or too wide, conservatively assume
            %% that the new range is 0 .. Int.
            #t_integer{elements={0,Int}};
        _ when Int < 0 ->
            %% We can't infer boundaries when the range is unknown and the
            %% other operand is a negative number, as the latter sign-extends
            %% to infinity and we can't express an inverted range at the
            %% moment (cf. X band -8; either less than -7 or greater than 7).
            #t_integer{}
    end.

%% Returns two bitmasks describing all possible values between From and To.
%%
%% The first contains the bits that are common to all values, and the second
%% contains the bits that are set by any value in the range.
range_masks(From, To) when From =< To ->
    range_masks_1(From, To, 0, -1, 0).

range_masks_1(From, To, BitPos, Intersection, Union) when From < To ->
    range_masks_1(From + (1 bsl BitPos), To, BitPos + 1,
                  Intersection band From, Union bor From);
range_masks_1(_From, To, _BitPos, Intersection0, Union0) ->
    Intersection = To band Intersection0,
    Union = To bor Union0,
    {Intersection, Union}.

discard_tuple_element_info(Min, Max, Es) ->
    foldl(fun(El, Acc) when Min =< El, El =< Max ->
                  maps:remove(El, Acc);
             (_El, Acc) -> Acc
          end, Es, maps:keys(Es)).

%% For a lists function that return a list of the same length as the input
%% list, return the type of the list.
same_length_type(cons) -> cons;
same_length_type(nil) -> nil;
same_length_type(_) -> list.

%% lists:zip/2 and friends only succeed when all arguments have the same
%% length, so if one of them is cons, we can infer that all of them are cons
%% on success.
lists_zip_type(Types) ->
    foldl(fun(cons, _) -> cons;
             (_, cons) -> cons;
             (nil, _) -> nil;
             (_, T) -> T
          end, list, Types).

make_two_tuple(Type1, Type2) ->
    #t_tuple{size=2,exact=true,
             elements=#{1=>Type1,2=>Type2}}.
