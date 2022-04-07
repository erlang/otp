%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2019-2022. All Rights Reserved.
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

-import(lists, [any/2,duplicate/2,foldl/3]).

-export([will_succeed/3, types/3, arith_type/2]).

%%
%% Returns whether a call will succeed or not.
%%
%% Note that it only answers 'yes' for functions in the 'erlang' module as
%% calls to other modules may fail due to not being loaded, even if we consider
%% the module to be known.
%%

-spec will_succeed(Mod, Func, ArgTypes) -> Result when
      Mod :: atom(),
      Func :: atom(),
      ArgTypes :: [normal_type()],
      Result :: 'yes' | 'no' | 'maybe'.

will_succeed(erlang, Op, [LHS, RHS])
  when Op =:= '+'; Op =:= '-'; Op =:= '*' ->
    succeeds_if_smallish(LHS, RHS);
will_succeed(erlang, Op, [#t_integer{elements={_,_}},
                          #t_integer{elements={Div,_}}])
  when (Op =:= 'div' orelse Op =:= 'rem'), Div > 0 ->
    yes;
will_succeed(erlang, 'bsr', [#t_integer{elements={_,_}},
                             #t_integer{elements={S,_}}])
  when S >= 0 ->
    yes;
will_succeed(erlang, 'bsl', [#t_integer{}=LHS,#t_integer{elements={S,_}}])
  when S < 64 ->
    succeeds_if_smallish(LHS);
will_succeed(erlang, '++', [LHS, _RHS]) ->
    succeeds_if_type(LHS, proper_list());
will_succeed(erlang, '--', [_, _] = Args) ->
    succeeds_if_types(Args, proper_list());
will_succeed(erlang, BoolOp, [_, _] = Args) when BoolOp =:= 'and';
                                                 BoolOp =:= 'or' ->
    succeeds_if_types(Args, beam_types:make_boolean());
will_succeed(erlang, Op, [_, _] = Args) when Op =:= 'band'; Op =:= 'bor'; Op =:= 'bxor' ->
    succeeds_if_types(Args, #t_integer{});
will_succeed(erlang, bit_size, [Arg]) ->
    succeeds_if_type(Arg, #t_bitstring{});
will_succeed(erlang, byte_size, [Arg]) ->
    succeeds_if_type(Arg, #t_bitstring{});
will_succeed(erlang, element, [Pos, #t_tuple{size=Sz}] = Args) when Sz > 0 ->
    SizeType = #t_integer{elements={1,Sz}},
    case beam_types:meet(Pos, SizeType) of
        Pos ->
            yes;
        _ ->
            fails_on_conflict(Args, [#t_integer{}, #t_tuple{}])
    end;
will_succeed(erlang, hd, [Arg]) ->
    succeeds_if_type(Arg, #t_cons{});
will_succeed(erlang, is_function, [_,#t_integer{elements={Min,_}}])
  when Min >= 0 ->
    yes;
will_succeed(erlang, is_map_key, [_Key, Map]) ->
    succeeds_if_type(Map, #t_map{});
will_succeed(erlang, length, [Arg]) ->
    succeeds_if_type(Arg, proper_list());
will_succeed(erlang, map_size, [Arg]) ->
    succeeds_if_type(Arg, #t_map{});
will_succeed(erlang, 'not', [Arg]) ->
    succeeds_if_type(Arg, beam_types:make_boolean());
will_succeed(erlang, setelement, [#t_integer{elements={Min,Max}},
                                  #t_tuple{exact=Exact,size=Size}, _]) ->
    if
        1 =< Min, Max =< Size ->
            %% The index is always in range.
            yes;
        Max < 1; Exact, Size < Min ->
            %% The index is always out of range.
            no;
        true ->
            'maybe'
    end;
will_succeed(erlang, size, [Arg]) ->
    ArgType = beam_types:join(#t_tuple{}, #t_bitstring{}),
    succeeds_if_type(Arg, ArgType);
will_succeed(erlang, tuple_size, [Arg]) ->
    succeeds_if_type(Arg, #t_tuple{});
will_succeed(erlang, tl, [Arg]) ->
    succeeds_if_type(Arg, #t_cons{});
will_succeed(erlang, raise, [Class, _Reason, nil]) ->
    case beam_types:meet(Class, #t_atom{elements=[error,exit,throw]}) of
        Class -> no;
        none -> yes;
        _ -> 'maybe'
    end;
will_succeed(Mod, Func, Args) ->
    Arity = length(Args),
    case erl_bifs:is_safe(Mod, Func, Arity) of
        true ->
            yes;
        false ->
            case erl_bifs:is_exit_bif(Mod, Func, Arity) of
                true ->
                    no;
                false ->
                    %% While we can't infer success for functions outside the
                    %% 'erlang' module (see above comment), it's safe to infer
                    %% failure when we know they return none or if the
                    %% arguments must have certain types.
                    case types(Mod, Func, Args) of
                        {none, _, _} -> no;
                        {_, ArgTypes, _} -> fails_on_conflict(Args, ArgTypes)
                    end
            end
    end.

fails_on_conflict([ArgType | Args], [Required | Types]) ->
    case beam_types:meet(ArgType, Required) of
        none -> no;
        _ -> fails_on_conflict(Args, Types)
    end;
fails_on_conflict([], []) ->
    'maybe'.

succeeds_if_types([LHS, RHS], Required) ->
    case {succeeds_if_type(LHS, Required),
          succeeds_if_type(RHS, Required)} of
        {yes, yes} -> yes;
        {no, _} -> no;
        {_, no} -> no;
        {_, _} -> 'maybe'
    end.

succeeds_if_type(ArgType, Required) ->
    case beam_types:meet(ArgType, Required) of
        ArgType -> yes;
        none -> no;
        _ -> 'maybe'
    end.

succeeds_if_smallish(#t_integer{elements={Min,Max}})
  when abs(Min) bsr 128 =:= 0, abs(Max) bsr 128 =:= 0 ->
    yes;
succeeds_if_smallish(_) ->
    'maybe'.

succeeds_if_smallish(LHS, RHS) ->
    case {succeeds_if_smallish(LHS),
          succeeds_if_smallish(RHS)} of
        {yes, yes} -> yes;
        {_, _} -> 'maybe'
    end.

%%
%% Define an upper limit for functions that return sizes of data
%% structures. The chosen value is about half the maxium size of a
%% smallnum. That means that adding a small constant to it will result
%% in a smallnum, but still the value is still sufficiently high to
%% make it impossible to reach in the foreseeable future.
%%
-define(SIZE_UPPER_LIMIT, ((1 bsl 58) - 1)).

%%
%% The document maximum size of a tuple.
%%
-define(MAX_TUPLE_SIZE, (1 bsl 24) - 1).

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
      ArgTypes :: [normal_type()],
      RetType :: type(),
      CanSubtract :: boolean().

%% Functions that only fail due to bad argument *types*, meaning it's safe to
%% subtract argument types on failure.
%%
%% Note that these are all from the erlang module; suitable functions in other
%% modules could fail due to the module not being loaded.
types(erlang, 'map_size', [_]) ->
    sub_safe(#t_integer{elements={0,?SIZE_UPPER_LIMIT}}, [#t_map{}]);
types(erlang, 'tuple_size', [Src]) ->
    Min = case Src of
              #t_tuple{size=Sz} -> Sz;
              _ -> 0
          end,
    Max = ?MAX_TUPLE_SIZE,
    sub_safe(#t_integer{elements={Min,Max}}, [#t_tuple{}]);
types(erlang, 'bit_size', [_]) ->
    sub_safe(#t_integer{elements={0,?SIZE_UPPER_LIMIT}}, [#t_bitstring{}]);
types(erlang, 'byte_size', [_]) ->
    sub_safe(#t_integer{elements={0,?SIZE_UPPER_LIMIT}}, [#t_bitstring{}]);
types(erlang, hd, [Src]) ->
    RetType = erlang_hd_type(Src),
    sub_safe(RetType, [#t_cons{}]);
types(erlang, tl, [Src]) ->
    RetType = erlang_tl_type(Src),
    sub_safe(RetType, [#t_cons{}]);
types(erlang, 'not', [_]) ->
    Bool = beam_types:make_boolean(),
    sub_safe(Bool, [Bool]);
types(erlang, 'length', [Src]) ->
    Min = case Src of
              #t_cons{} -> 1;
              _ -> 0
          end,
    sub_safe(#t_integer{elements={Min,?SIZE_UPPER_LIMIT}}, [proper_list()]);

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

%% Relational operators.
types(erlang, Op, [#t_integer{elements=Range1},
                   #t_integer{elements=Range2}])
  when Op =:= '<'; Op =:= '=<'; Op =:= '>='; Op =:= '>' ->
    case beam_bounds:relop(Op, Range1, Range2) of
        'maybe' ->
            sub_unsafe(beam_types:make_boolean(), [any, any]);
        Bool when is_boolean(Bool) ->
            sub_unsafe(#t_atom{elements=[Bool]}, [any, any])
    end;

%% Type tests.
types(erlang, is_atom, [Type]) ->
    sub_unsafe_type_test(Type, #t_atom{});
types(erlang, is_binary, [Type]) ->
    sub_unsafe_type_test(Type, #t_bs_matchable{tail_unit=8});
types(erlang, is_bitstring, [Type]) ->
    sub_unsafe_type_test(Type, #t_bs_matchable{});
types(erlang, is_boolean, [Type]) ->
    case beam_types:is_boolean_type(Type) of
        true ->
            sub_unsafe(#t_atom{elements=[true]}, [any]);
        false ->
            case beam_types:meet(Type, #t_atom{}) of
                #t_atom{elements=[_|_]=Es} ->
                    case any(fun is_boolean/1, Es) of
                        true ->
                            sub_unsafe(beam_types:make_boolean(), [any]);
                        false ->
                            sub_unsafe(#t_atom{elements=[false]}, [any])
                    end;
                #t_atom{} ->
                    sub_unsafe(beam_types:make_boolean(), [any]);
                none ->
                    sub_unsafe(#t_atom{elements=[false]}, [any])
            end
    end;
types(erlang, is_float, [Type]) ->
    sub_unsafe_type_test(Type, #t_float{});
types(erlang, is_function, [Type, #t_integer{elements={Arity,Arity}}])
  when Arity >= 0, Arity =< ?MAX_FUNC_ARGS ->
    RetType =
        case beam_types:meet(Type, #t_fun{arity=Arity}) of
            Type -> #t_atom{elements=[true]};
            none -> #t_atom{elements=[false]};
            _ -> beam_types:make_boolean()
        end,
    sub_unsafe(RetType, [any, any]);
types(erlang, is_function, [Type]) ->
    sub_unsafe_type_test(Type, #t_fun{});
types(erlang, is_integer, [Type]) ->
    sub_unsafe_type_test(Type, #t_integer{});
types(erlang, is_list, [Type]) ->
    sub_unsafe_type_test(Type, #t_list{});
types(erlang, is_map, [Type]) ->
    sub_unsafe_type_test(Type, #t_map{});
types(erlang, is_number, [Type]) ->
    sub_unsafe_type_test(Type, number);
types(erlang, is_pid, [Type]) ->
    sub_unsafe_type_test(Type, pid);
types(erlang, is_port, [Type]) ->
    sub_unsafe_type_test(Type, port);
types(erlang, is_reference, [Type]) ->
    sub_unsafe_type_test(Type, reference);
types(erlang, is_tuple, [Type]) ->
    sub_unsafe_type_test(Type, #t_tuple{});

%% Bitwise ops
types(erlang, 'band', [_,_]=Args) ->
    sub_unsafe(erlang_band_type(Args), [#t_integer{}, #t_integer{}]);
types(erlang, 'bor', [_,_]=Args) ->
    sub_unsafe(erlang_bor_type(Args), [#t_integer{}, #t_integer{}]);
types(erlang, 'bxor', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'bsl', [_,_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}, #t_integer{}]);
types(erlang, 'bsr', [_,_]=Args) ->
    sub_unsafe(erlang_bsr_type(Args), [#t_integer{}, #t_integer{}]);
types(erlang, 'bnot', [_]) ->
    sub_unsafe(#t_integer{}, [#t_integer{}]);

%% Fixed-type arithmetic
types(erlang, 'float', [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(erlang, 'round', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, 'floor', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, 'ceil', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, 'trunc', [_]) ->
    sub_unsafe(#t_integer{}, [number]);
types(erlang, '/', [_,_]) ->
    sub_unsafe(#t_float{}, [number, number]);
types(erlang, 'div', [_,_]=Args) ->
    ArgTypes = [#t_integer{}, #t_integer{}],
    sub_unsafe(erlang_div_type(Args), ArgTypes);
types(erlang, 'rem', Args) ->
    ArgTypes = [#t_integer{}, #t_integer{}],
    sub_unsafe(erlang_rem_type(Args), ArgTypes);

%% Mixed-type arithmetic; '+'/2 and friends are handled in the catch-all
%% clause for the 'erlang' module.
types(erlang, 'abs', [_]=Args) ->
    mixed_arith_types(Args);

%% List operations
types(erlang, '++', [LHS, RHS]) ->
    %% `[] ++ RHS` yields RHS, even if RHS is not a list.
    ListType = copy_list(LHS, same_length, proper),
    RetType = beam_types:join(ListType, RHS),
    sub_unsafe(RetType, [proper_list(), any]);
types(erlang, '--', [LHS, _]) ->
    RetType = copy_list(LHS, new_length, proper),
    sub_unsafe(RetType, [proper_list(), proper_list()]);

types(erlang, 'iolist_to_binary', [_]) ->
    %% Arg is an iodata(), despite its name.
    ArgType = beam_types:join(#t_list{}, #t_bitstring{size_unit=8}),
    sub_unsafe(#t_bitstring{size_unit=8}, [ArgType]);
types(erlang, 'iolist_size', [_]) ->
    %% Arg is an iodata(), despite its name. The size is NOT limited
    %% by the size of memory.
    ArgType = beam_types:join(#t_list{}, #t_bitstring{size_unit=8}),
    sub_unsafe(#t_integer{}, [ArgType]);
types(erlang, 'list_to_binary', [_]) ->
    %% Arg is an iolist(), despite its name.
    sub_unsafe(#t_bitstring{size_unit=8}, [#t_list{}]);
types(erlang, 'list_to_bitstring', [_]) ->
    %% As list_to_binary but with bitstrings rather than binaries.
    sub_unsafe(#t_bitstring{}, [proper_list()]);

%% Process operations
types(erlang, alias, []) ->
    sub_unsafe(reference, []);
types(erlang, alias, [_]) ->
    sub_unsafe(reference, [proper_list()]);
types(erlang, monitor, [_, _]) ->
    sub_unsafe(reference, [any, any]);
types(erlang, monitor, [_, _, _]) ->
    sub_unsafe(reference, [any, any, proper_list()]);
types(erlang, 'spawn', [_]) ->
    sub_unsafe(pid, [#t_fun{arity=0}]);
types(erlang, 'spawn', [_, _]) ->
    sub_unsafe(pid, [#t_atom{}, #t_fun{arity=0}]);
types(erlang, 'spawn', [_, _, _]) ->
    sub_unsafe(pid, [#t_atom{}, #t_atom{}, proper_list()]);
types(erlang, 'spawn_link', Args) ->
    types(erlang, 'spawn', Args);
types(erlang, 'spawn_monitor', [_]) ->
    RetType = make_two_tuple(pid, reference),
    sub_unsafe(RetType, [#t_fun{arity=0}]);
types(erlang, 'spawn_monitor', [_, _]) ->
    RetType = make_two_tuple(pid, reference),
    sub_unsafe(RetType, [#t_atom{}, #t_fun{arity=0}]);
types(erlang, 'spawn_monitor', [_, _, _]) ->
    RetType = make_two_tuple(pid, reference),
    sub_unsafe(RetType, [#t_atom{}, #t_atom{}, proper_list()]);
types(erlang, 'spawn_request', [_ | _]=Args) when length(Args) =< 5 ->
    sub_unsafe(reference, [any || _ <- Args]);

%% Misc ops.
types(erlang, 'binary_part', [_, _]) ->
    PosLen = make_two_tuple(#t_integer{}, #t_integer{}),
    Binary = #t_bitstring{size_unit=8},
    sub_unsafe(Binary, [Binary, PosLen]);
types(erlang, 'binary_part', [_, _, _]) ->
    Binary = #t_bitstring{size_unit=8},
    sub_unsafe(Binary, [Binary, #t_integer{}, #t_integer{}]);
types(erlang, 'is_map_key', [Key, Map]) ->
    RetType = case erlang_map_get_type(Key, Map) of
                  none -> beam_types:make_atom(false);
                  _ -> beam_types:make_boolean()
              end,
    sub_unsafe(RetType, [any, #t_map{}]);
types(erlang, make_ref, []) ->
    sub_unsafe(reference, []);
types(erlang, 'map_get', [Key, Map]) ->
    RetType = erlang_map_get_type(Key, Map),
    sub_unsafe(RetType, [any, #t_map{}]);
types(erlang, 'node', [_]) ->
    sub_unsafe(#t_atom{}, [any]);
types(erlang, 'node', []) ->
    sub_unsafe(#t_atom{}, []);
types(erlang, 'size', [_]) ->
    ArgType = beam_types:join(#t_tuple{}, #t_bitstring{}),
    sub_unsafe(#t_integer{}, [ArgType]);

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
                      beam_types:get_tuple_element(Index, Es);
                  _ ->
                      any
              end,

    ArgTypes = [#t_integer{elements={1,?MAX_TUPLE_SIZE}},
                #t_tuple{size=Index}],

    sub_unsafe(RetType, ArgTypes);
types(erlang, setelement, [PosType, TupleType, ArgType]) ->
    RetType = case {PosType,TupleType} of
                  {#t_integer{elements={Index,Index}},
                   #t_tuple{elements=Es0,size=Size}=T} when Index >= 1 ->
                      %% This is an exact index, update the type of said
                      %% element or return 'none' if it's known to be out of
                      %% bounds.
                      Es = beam_types:set_tuple_element(Index, ArgType, Es0),
                      case T#t_tuple.exact of
                          false ->
                              T#t_tuple{size=max(Index, Size),elements=Es};
                          true when Index =< Size ->
                              T#t_tuple{elements=Es};
                          true ->
                              none
                      end;
                  {#t_integer{elements={Min,Max}},
                   #t_tuple{elements=Es0,size=Size}=T} when Min >= 1 ->
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
               #t_integer{elements={Arity,Arity}}
                 when Arity >= 0, Arity =< ?MAX_FUNC_ARGS ->
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
    sub_unsafe(#t_float{}, [number]);
types(math, cosh, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, sin, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, sinh, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, tan, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, tanh, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, acos, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, acosh, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, asin, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, asinh, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, atan, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, atanh, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, erf, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, erfc, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, exp, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, log, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, log2, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, log10, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, sqrt, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, atan2, [_,_]) ->
    sub_unsafe(#t_float{}, [number, number]);
types(math, pow, [_,_]) ->
    sub_unsafe(#t_float{}, [number, number]);
types(math, ceil, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, floor, [_]) ->
    sub_unsafe(#t_float{}, [number]);
types(math, fmod, [_,_]) ->
    sub_unsafe(#t_float{}, [number, number]);
types(math, pi, []) ->
    sub_unsafe(#t_float{}, []);

%%
%% List functions
%%
%% These tend to have tricky edge cases around nil and proper lists, be very
%% careful and try not to narrow the types needlessly. Keep in mind that they
%% need to be safe regardless of how the function is implemented, so it's best
%% not to say that a list is proper unless every element must be visited to
%% succeed.
%%

%% Operator aliases.
types(lists, append, [_,_]=Args) ->
    types(erlang, '++', Args);
types(lists, append, [_]) ->
    %% This is implemented through folding the list over erlang:'++'/2, so it
    %% can hypothetically return anything, but we can infer that its argument
    %% is a proper list on success.
    sub_unsafe(any, [proper_list()]);
types(lists, subtract, [_,_]=Args) ->
    types(erlang, '--', Args);

%% Functions returning booleans.
types(lists, all, [_,_]) ->
    %% This can succeed on improper lists if the fun returns 'false' for an
    %% element before reaching the end.
    sub_unsafe(beam_types:make_boolean(), [#t_fun{arity=1}, #t_list{}]);
types(lists, any, [_,_]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    sub_unsafe(beam_types:make_boolean(), [#t_fun{arity=1}, #t_list{}]);
types(lists, keymember, [_,_,_]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    sub_unsafe(beam_types:make_boolean(), [any, #t_integer{}, #t_list{}]);
types(lists, member, [_,_]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    sub_unsafe(beam_types:make_boolean(), [any, #t_list{}]);
types(lists, prefix, [_,_]) ->
    %% This function doesn't need to reach the end of either list to return
    %% false, so we can succeed even when both are improper lists.
    sub_unsafe(beam_types:make_boolean(), [#t_list{}, #t_list{}]);
types(lists, suffix, [_,_]) ->
    %% A different implementation could return true when the first list is nil,
    %% so we can't tell if either is proper.
    sub_unsafe(beam_types:make_boolean(), [#t_list{}, #t_list{}]);

%% Simple folds
types(lists, foldl, [Fun, Init, List]) ->
    RetType = lists_fold_type(Fun, Init, List),
    sub_unsafe(RetType, [#t_fun{arity=2}, any, proper_list()]);
types(lists, foldr, [Fun, Init, List]) ->
    RetType = lists_fold_type(Fun, Init, List),
    sub_unsafe(RetType, [#t_fun{arity=2}, any, proper_list()]);

%% Functions returning plain lists.
types(lists, droplast, [List]) ->
    RetType = copy_list(List, new_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, dropwhile, [_Fun, List]) ->
    %% If the element is found before the end of the list, we could return an
    %% improper list.
    RetType = copy_list(List, new_length, maybe_improper),
    sub_unsafe(RetType, [#t_fun{arity=1}, #t_list{}]);
types(lists, duplicate, [_Count, Element]) ->
    sub_unsafe(proper_list(Element), [#t_integer{}, any]);
types(lists, filter, [_Fun, List]) ->
    RetType = copy_list(List, new_length, proper),
    sub_unsafe(RetType, [#t_fun{arity=1}, proper_list()]);
types(lists, flatten, [_]) ->
    sub_unsafe(proper_list(), [proper_list()]);
types(lists, map, [Fun, List]) ->
    RetType = lists_map_type(Fun, List),
    sub_unsafe(RetType, [#t_fun{arity=1}, proper_list()]);
types(lists, reverse, [List]) ->
    RetType = copy_list(List, same_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, sort, [List]) ->
    RetType = copy_list(List, same_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, takewhile, [_Fun, List]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    RetType = copy_list(List, new_length, proper),
    sub_unsafe(RetType, [#t_fun{arity=1}, #t_list{}]);
types(lists, usort, [List]) ->
    %% The result is not quite the same length, but a non-empty list will stay
    %% non-empty.
    RetType = copy_list(List, same_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, zip, [_,_]=Lists) ->
    {RetType, ArgType} = lists_zip_types(Lists),
    sub_unsafe(RetType, [ArgType, ArgType]);
types(lists, zipwith, [Fun | [_,_]=Lists]) ->
    {RetType, ArgType} = lists_zipwith_types(Fun, Lists),
    sub_unsafe(RetType, [#t_fun{arity=2}, ArgType, ArgType]);

%% Functions with complex return values.
types(lists, keyfind, [KeyType,PosType,_]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    TupleType = case PosType of
                    #t_integer{elements={Index,Index}} when is_integer(Index),
                                                            Index >= 1 ->
                        Es = beam_types:set_tuple_element(Index, KeyType, #{}),
                        #t_tuple{size=Index,elements=Es};
                    _ ->
                        #t_tuple{}
                end,
    RetType = beam_types:join(TupleType, beam_types:make_atom(false)),
    sub_unsafe(RetType, [any, #t_integer{}, #t_list{}]);
types(lists, MapFold, [Fun, Init, List])
  when MapFold =:= mapfoldl; MapFold =:= mapfoldr ->
    RetType = lists_mapfold_type(Fun, Init, List),
    sub_unsafe(RetType, [#t_fun{arity=2}, any, proper_list()]);
types(lists, partition, [_Fun, List]) ->
    ListType = copy_list(List, new_length, proper),
    RetType = make_two_tuple(ListType, ListType),
    sub_unsafe(RetType, [#t_fun{arity=1}, proper_list()]);
types(lists, search, [_,_]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    TupleType = make_two_tuple(beam_types:make_atom(value), any),
    RetType = beam_types:join(TupleType, beam_types:make_atom(false)),
    sub_unsafe(RetType, [#t_fun{arity=1}, #t_list{}]);
types(lists, splitwith, [_Fun, List]) ->
    %% Only the elements in the left list are guaranteed to be visited, so both
    %% the argument and the right list may be improper.
    Left = copy_list(List, new_length, proper),
    Right = copy_list(List, new_length, maybe_improper),
    sub_unsafe(make_two_tuple(Left, Right), [#t_fun{arity=1}, #t_list{}]);
types(lists, unzip, [List]) ->
    RetType = lists_unzip_type(2, List),
    sub_unsafe(RetType, [proper_list()]);

%%
%% Map functions
%%

types(maps, filter, [_Fun, Map]) ->
    %% Conservatively assume that key/value types are unchanged.
    RetType = case Map of
                  #t_map{}=T -> T;
                  _ -> #t_map{}
              end,
    sub_unsafe(RetType, [#t_fun{arity=2}, #t_map{}]);
types(maps, find, [Key, Map]) ->
    TupleType = case erlang_map_get_type(Key, Map) of
                    none ->
                        none;
                    ValueType ->
                        make_two_tuple(beam_types:make_atom(ok), ValueType)
                end,
    %% error | {ok, Value}
    RetType = beam_types:join(beam_types:make_atom(error), TupleType),
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, fold, [Fun, Init, _Map]) ->
    RetType = case Fun of
                  #t_fun{type=Type} ->
                      %% The map is potentially empty, so we have to assume it
                      %% can return the initial value.
                      beam_types:join(Type, Init);
                  _ ->
                      any
              end,
    sub_unsafe(RetType, [#t_fun{arity=3}, any, #t_map{}]);
types(maps, from_keys, [Keys, Value]) ->
    KeyType = erlang_hd_type(Keys),
    RetType = case KeyType of
                  none -> #t_map{super_key=none,super_value=none};
                  _ -> #t_map{super_key=KeyType,super_value=Value}
              end,
    sub_unsafe(RetType, [proper_list(), any]);
types(maps, from_list, [Pairs]) ->
    PairType = erlang_hd_type(Pairs),
    RetType = case beam_types:normalize(PairType) of
                  #t_tuple{elements=Es} ->
                      SKey = beam_types:get_tuple_element(1, Es),
                      SValue = beam_types:get_tuple_element(2, Es),
                      #t_map{super_key=SKey,super_value=SValue};
                  none ->
                      #t_map{super_key=none,super_value=none};
                  _ ->
                      #t_map{}
              end,
    sub_unsafe(RetType, [proper_list()]);
types(maps, get, [_Key, _Map]=Args) ->
    types(erlang, map_get, Args);
types(maps, get, [Key, Map, Default]) ->
    RetType = case erlang_map_get_type(Key, Map) of
                  none -> Default;
                  ValueType -> beam_types:join(ValueType, Default)
              end,
    sub_unsafe(RetType, [any, #t_map{}, any]);
types(maps, keys, [Map]) ->
    RetType = case Map of
                  #t_map{super_key=none} -> nil;
                  #t_map{super_key=SKey} -> proper_list(SKey);
                  _ -> proper_list()
              end,
    sub_unsafe(RetType, [#t_map{}]);
types(maps, map, [Fun, Map]) ->
    RetType = case {Fun, Map} of
                  {#t_fun{type=FunRet}, #t_map{super_value=SValue0}} ->
                      SValue = beam_types:join(FunRet, SValue0),
                      Map#t_map{super_value=SValue};
                  _ ->
                      #t_map{}
              end,
    sub_unsafe(RetType, [#t_fun{arity=2}, #t_map{}]);
types(maps, merge, [A, B]) ->
    RetType = case {A, B} of
                  {#t_map{super_key=SKeyA,super_value=SValueA},
                   #t_map{super_key=SKeyB,super_value=SValueB}} ->
                      SKey = beam_types:join(SKeyA, SKeyB),
                      SValue = beam_types:join(SValueA, SValueB),
                      #t_map{super_key=SKey,super_value=SValue};
                  _ ->
                      #t_map{}
              end,
    sub_unsafe(RetType, [#t_map{}, #t_map{}]);
types(maps, new, []) ->
    RetType = #t_map{super_key=none,super_value=none},
    sub_unsafe(RetType, []);
types(maps, put, [Key, Value, Map]) ->
    RetType = case Map of
                  #t_map{super_key=SKey0,super_value=SValue0} ->
                      SKey = beam_types:join(Key, SKey0),
                      SValue = beam_types:join(Value, SValue0),
                      #t_map{super_key=SKey,super_value=SValue};
                  _ ->
                      #t_map{}
              end,
    sub_unsafe(RetType, [any, any, #t_map{}]);
types(maps, remove, [Key, Map]) ->
    RetType = maps_remove_type(Key, Map),
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, take, [Key, Map]) ->
    TupleType = case erlang_map_get_type(Key, Map) of
                    none ->
                        none;
                    ValueType ->
                        MapType = beam_types:meet(Map, #t_map{}),
                        make_two_tuple(ValueType, MapType)
                end,
    %% error | {Value, Map}
    RetType = beam_types:join(beam_types:make_atom(error), TupleType),
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, to_list, [Map]) ->
    RetType = case Map of
                  #t_map{super_key=SKey,super_value=SValue} ->
                      proper_list(make_two_tuple(SKey, SValue));
                  _ ->
                      proper_list()
              end,
    sub_unsafe(RetType, [#t_map{}]);
types(maps, update_with, [_Key, Fun, Map]) ->
    RetType = case {Fun, Map} of
                  {#t_fun{type=FunRet}, #t_map{super_value=SValue0}} ->
                      SValue = beam_types:join(FunRet, SValue0),
                      Map#t_map{super_value=SValue};
                  _ ->
                      #t_map{}
              end,
    sub_unsafe(RetType, [any, #t_fun{arity=1}, #t_map{}]);
types(maps, values, [Map]) ->
    RetType = case Map of
                  #t_map{super_value=none} -> nil;
                  #t_map{super_value=SValue} -> proper_list(SValue);
                  _ -> proper_list()
              end,
    sub_unsafe(RetType, [#t_map{}]);
types(maps, with, [Keys, Map]) ->
    RetType = case {erlang_hd_type(Keys), Map} of
                  {none, _} ->
                      #t_map{super_key=none,super_value=none};
                  {KeysType, #t_map{super_key=SKey0}} ->
                      %% Since we know that the Map will only contain the pairs
                      %% pointed out by Keys, we can restrict the types to
                      %% those in the list.
                      SKey = beam_types:meet(KeysType, SKey0),
                      Map#t_map{super_key=SKey};
                  {_, _} ->
                      #t_map{}
              end,
    sub_unsafe(RetType, [proper_list(), #t_map{}]);
types(maps, without, [Keys, Map]) ->
    RetType = maps_remove_type(erlang_hd_type(Keys), Map),
    sub_unsafe(RetType, [proper_list(), #t_map{}]);

%% Catch-all clause for unknown functions.

types(_, _, Args) ->
    sub_unsafe(any, [any || _ <- Args]).

-spec arith_type(Op, ArgTypes) -> RetType when
      Op :: beam_ssa:op(),
      ArgTypes :: [normal_type()],
      RetType :: type().

arith_type({bif,'+'}, [#t_integer{elements=Range1},
                       #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'+'(Range1, Range2)};
arith_type({bif,'-'}, [#t_integer{elements=Range1},
                       #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'-'(Range1, Range2)};
arith_type({bif,'*'}, [#t_integer{elements=Range1},
                       #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'*'(Range1, Range2)};
arith_type({bif,'div'}, ArgTypes) ->
    erlang_div_type(ArgTypes);
arith_type({bif,'rem'}, ArgTypes) ->
    erlang_rem_type(ArgTypes);
arith_type({bif,'band'}, ArgTypes) ->
    erlang_band_type(ArgTypes);
arith_type({bif,'bor'}, Args) ->
    erlang_bor_type(Args);
arith_type({bif,'bxor'}, Args) ->
    erlang_bxor_type(Args);
arith_type({bif,'bsr'}, Args) ->
    erlang_bsr_type(Args);
arith_type({bif,'bsl'}, Args) ->
    erlang_bsl_type(Args);
arith_type(_Op, _Args) ->
    any.

%%
%% Function-specific helpers.
%%

mixed_arith_types([FirstType | _]=Args0) ->
    RetType = foldl(fun(#t_integer{}, #t_integer{}) -> #t_integer{};
                       (#t_integer{}, number) -> number;
                       (#t_integer{}, #t_float{}) -> #t_float{};
                       (#t_float{}, #t_integer{}) -> #t_float{};
                       (#t_float{}, number) -> #t_float{};
                       (#t_float{}, #t_float{}) -> #t_float{};
                       (number, #t_integer{}) -> number;
                       (number, #t_float{}) -> #t_float{};
                       (number, number) -> number;
                       (any, _) -> number;
                       (_, _) -> none
                    end, FirstType, Args0),
    sub_unsafe(RetType, [number || _ <- Args0]).

erlang_hd_type(Src) ->
    case beam_types:meet(Src, #t_cons{}) of
        #t_cons{type=Type} -> Type;
        none -> none
    end.

erlang_tl_type(Src) ->
    case beam_types:meet(Src, #t_cons{}) of
        #t_cons{terminator=Term}=Cons -> beam_types:join(Cons, Term);
        none -> none
    end.

erlang_band_type([#t_integer{elements=Range1}, #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'band'(Range1, Range2)};
erlang_band_type([#t_integer{elements=Range}, _RHS]) ->
    #t_integer{elements=beam_bounds:'band'(Range, any)};
erlang_band_type([_LHS, #t_integer{elements=Range}]) ->
    #t_integer{elements=beam_bounds:'band'(any, Range)};
erlang_band_type([_, _]) ->
    #t_integer{}.

erlang_bor_type([#t_integer{elements=Range1}, #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'bor'(Range1, Range2)};
erlang_bor_type([_, _]) ->
    #t_integer{}.

erlang_bxor_type([#t_integer{elements=Range1}, #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'bxor'(Range1, Range2)};
erlang_bxor_type([_, _]) ->
    #t_integer{}.

erlang_bsr_type([#t_integer{elements=Range1}, #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'bsr'(Range1, Range2)};
erlang_bsr_type([_, _]) ->
    #t_integer{}.

erlang_bsl_type([#t_integer{elements=Range1}, #t_integer{elements=Range2}]) ->
    #t_integer{elements=beam_bounds:'bsl'(Range1, Range2)};
erlang_bsl_type([_, _]) ->
    #t_integer{}.

erlang_div_type(ArgTypes) ->
    case ArgTypes of
        [#t_integer{elements=Range1},#t_integer{elements=Range2}]->
            #t_integer{elements=beam_bounds:'div'(Range1, Range2)};
        _ ->
            #t_integer{}
    end.

erlang_rem_type([LHS0, #t_integer{elements=Range2}]) ->
    Range1 = case LHS0 of
                 #t_integer{elements=R1} -> R1;
                 _ -> any
             end,
    #t_integer{elements=beam_bounds:'rem'(Range1, Range2)};
erlang_rem_type(_) ->
    #t_integer{}.

erlang_map_get_type(Key, Map) ->
    case Map of
        #t_map{super_key=SKey,super_value=SValue} ->
            case beam_types:meet(SKey, Key) of
                none -> none;
                _ -> SValue
            end;
        _ ->
            any
    end.

lists_fold_type(_Fun, Init, nil) ->
    Init;
lists_fold_type(#t_fun{type=Type}, _Init, #t_cons{}) ->
    %% The list is non-empty so it's safe to ignore Init.
    Type;
lists_fold_type(#t_fun{type=Type}, Init, #t_list{}) ->
    %% The list is possibly empty so we have to assume it can return the
    %% initial value, whose type can differ significantly from the fun's
    %% return value.
    beam_types:join(Type, Init);
lists_fold_type(_Fun, _Init, _List) ->
    any.

lists_map_type(#t_fun{type=Type}, Types) ->
    lists_map_type_1(Types, Type);
lists_map_type(_Fun, Types) ->
    lists_map_type_1(Types, any).

lists_map_type_1(nil, _ElementType) ->
    nil;
lists_map_type_1(#t_cons{}, none) ->
    %% The list is non-empty and the fun never returns.
    none;
lists_map_type_1(#t_cons{}, ElementType) ->
    proper_cons(ElementType);
lists_map_type_1(_, none) ->
    %% The fun never returns, so the only way we could return normally is
    %% if the list is empty.
    nil;
lists_map_type_1(_, ElementType) ->
    proper_list(ElementType).

lists_mapfold_type(#t_fun{type=#t_tuple{size=2,elements=Es}}, Init, List) ->
    ElementType = beam_types:get_tuple_element(1, Es),
    AccType = beam_types:get_tuple_element(2, Es),
    lists_mapfold_type_1(List, ElementType, Init, AccType);
lists_mapfold_type(#t_fun{type=none}, _Init, #t_cons{}) ->
    %% The list is non-empty and the fun never returns.
    none;
lists_mapfold_type(#t_fun{type=none}, Init, _List) ->
    %% The fun never returns, so the only way we could return normally is
    %% if the list is empty, in which case we'll return [] and the initial
    %% value.
    make_two_tuple(nil, Init);
lists_mapfold_type(_Fun, Init, List) ->
    lists_mapfold_type_1(List, any, Init, any).

lists_mapfold_type_1(nil, _ElementType, Init, _AccType) ->
    make_two_tuple(nil, Init);
lists_mapfold_type_1(#t_cons{}, ElementType, _Init, AccType) ->
    %% The list has at least one element, so it's safe to ignore Init.
    make_two_tuple(proper_cons(ElementType), AccType);
lists_mapfold_type_1(_, ElementType, Init, AccType0) ->
    %% We can only rely on AccType when we know the list is non-empty, so we
    %% have to join it with the initial value in case the list is empty.
    AccType = beam_types:join(AccType0, Init),
    make_two_tuple(proper_list(ElementType), AccType).

lists_unzip_type(Size, List) ->
    Es = lut_make_elements(lut_list_types(Size, List), 1, #{}),
    #t_tuple{size=Size,exact=true,elements=Es}.

lut_make_elements([Type | Types], Index, Es0) ->
    Es = beam_types:set_tuple_element(Index, Type, Es0),
    lut_make_elements(Types, Index + 1, Es);
lut_make_elements([], _Index, Es) ->
    Es.

lut_list_types(Size, #t_cons{type=#t_tuple{size=Size,elements=Es}}) ->
    Types = lut_element_types(1, Size, Es),
    [proper_cons(T) || T <- Types];
lut_list_types(Size, #t_list{type=#t_tuple{size=Size,elements=Es}}) ->
    Types = lut_element_types(1, Size, Es),
    [proper_list(T) || T <- Types];
lut_list_types(Size, nil) ->
    lists:duplicate(Size, nil);
lut_list_types(Size, _) ->
    lists:duplicate(Size, proper_list()).

lut_element_types(Index, Max, #{}) when Index > Max ->
    [];
lut_element_types(Index, Max, Es) ->
    ElementType = beam_types:get_tuple_element(Index, Es),
    [ElementType | lut_element_types(Index + 1, Max, Es)].

%% lists:zip/2 and friends only succeed when all arguments have the same
%% length, so if one of them is #t_cons{}, we can infer that all of them are
%% #t_cons{} on success.

lists_zip_types(Types) ->
    lists_zip_types_1(Types, false, #{}, 1).

lists_zip_types_1([nil | _], _AnyCons, _Es, _N) ->
    %% Early exit; we know the result is [] on success.
    {nil, nil};
lists_zip_types_1([#t_cons{type=Type,terminator=nil} | Lists],
                  _AnyCons, Es0, N) ->
    Es = beam_types:set_tuple_element(N, Type, Es0),
    lists_zip_types_1(Lists, true, Es, N + 1);
lists_zip_types_1([#t_list{type=Type,terminator=nil} | Lists],
                  AnyCons, Es0, N) ->
    Es = beam_types:set_tuple_element(N, Type, Es0),
    lists_zip_types_1(Lists, AnyCons, Es, N + 1);
lists_zip_types_1([_ | Lists], AnyCons, Es, N) ->
    lists_zip_types_1(Lists, AnyCons, Es, N + 1);
lists_zip_types_1([], true, Es, N) ->
    %% At least one element was cons, so we know it's non-empty on success.
    ElementType = #t_tuple{exact=true,size=(N - 1),elements=Es},
    RetType = proper_cons(ElementType),
    ArgType = proper_cons(),
    {RetType, ArgType};
lists_zip_types_1([], false, Es, N) ->
    ElementType = #t_tuple{exact=true,size=(N - 1),elements=Es},
    RetType = proper_list(ElementType),
    ArgType = proper_list(),
    {RetType, ArgType}.

lists_zipwith_types(#t_fun{type=Type}, Types) ->
    lists_zipwith_type_1(Types, Type);
lists_zipwith_types(_Fun, Types) ->
    lists_zipwith_type_1(Types, any).

lists_zipwith_type_1([nil | _], _ElementType) ->
    %% Early exit; we know the result is [] on success.
    {nil, nil};
lists_zipwith_type_1([#t_cons{} | _Lists], none) ->
    %% Early exit; the list is non-empty and we know the fun never
    %% returns.
    {none, any};
lists_zipwith_type_1([#t_cons{} | _Lists], ElementType) ->
    %% Early exit; we know the result is cons on success.
    RetType = proper_cons(ElementType),
    ArgType = proper_cons(),
    {RetType, ArgType};
lists_zipwith_type_1([_ | Lists], ElementType) ->
    lists_zipwith_type_1(Lists, ElementType);
lists_zipwith_type_1([], none) ->
    %% Since we know the fun won't return, the only way we could return
    %% normally is if all lists are empty.
    {nil, nil};
lists_zipwith_type_1([], ElementType) ->
    RetType = proper_list(ElementType),
    ArgType = proper_list(),
    {RetType, ArgType}.

maps_remove_type(Key, #t_map{super_key=SKey0}=Map) ->
    case beam_types:is_singleton_type(Key) of
        true ->
            SKey = beam_types:subtract(SKey0, Key),
            Map#t_map{super_key=SKey};
        false ->
            Map
    end;
maps_remove_type(_Key, _Map) ->
    #t_map{}.

%%%
%%% Generic helpers
%%%

sub_unsafe_type_test(ArgType, Required) ->
    RetType =
        case beam_types:meet(ArgType, Required) of
            ArgType -> #t_atom{elements=[true]};
            none -> #t_atom{elements=[false]};
            _ -> beam_types:make_boolean()
        end,
    sub_unsafe(RetType, [any]).

sub_unsafe(RetType, ArgTypes) ->
    {RetType, ArgTypes, false}.

sub_safe(RetType, ArgTypes) ->
    {RetType, ArgTypes, true}.

discard_tuple_element_info(Min, Max, Es) ->
    foldl(fun(El, Acc) when Min =< El, El =< Max ->
                  maps:remove(El, Acc);
             (_El, Acc) -> Acc
          end, Es, maps:keys(Es)).

proper_cons() ->
    #t_cons{terminator=nil}.

proper_cons(ElementType) ->
    #t_cons{type=ElementType,terminator=nil}.

proper_list() ->
    #t_list{terminator=nil}.

proper_list(ElementType) ->
    #t_list{type=ElementType,terminator=nil}.

%% Constructs a new list type based on another, optionally keeping the same
%% length and/or making it proper.
-spec copy_list(List, Length, Proper) -> type() when
      List :: type(),
      Length :: same_length | new_length,
      Proper :: proper | maybe_improper.
copy_list(#t_cons{terminator=Term}=T, Length, maybe_improper) ->
    copy_list_1(T, Length, Term);
copy_list(#t_list{terminator=Term}=T, Length, maybe_improper) ->
    copy_list_1(T, Length, Term);
copy_list(T, Length, proper) ->
    copy_list_1(T, Length, nil);
copy_list(T, Length, _Proper) ->
    copy_list_1(T, Length, any).

copy_list_1(#t_cons{}=T, same_length, Terminator) ->
    T#t_cons{terminator=Terminator};
copy_list_1(#t_cons{type=Type}, new_length, Terminator) ->
    #t_list{type=Type,terminator=Terminator};
copy_list_1(#t_list{}=T, _Length, Terminator) ->
    T#t_list{terminator=Terminator};
copy_list_1(nil, _Length, _Terminator) ->
    nil;
copy_list_1(_, _Length, Terminator) ->
    #t_list{terminator=Terminator}.

make_two_tuple(Type1, Type2) ->
    Es0 = beam_types:set_tuple_element(1, Type1, #{}),
    Es = beam_types:set_tuple_element(2, Type2, Es0),
    #t_tuple{size=2,exact=true,elements=Es}.
