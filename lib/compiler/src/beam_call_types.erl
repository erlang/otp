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

-module(beam_call_types).
-moduledoc false.

-include("beam_types.hrl").

-import(lists, [any/2,duplicate/2,foldl/3]).

-export([will_succeed/3, types/3, arith_type/2]).

%%
%% Define an upper limit for functions that return sizes of data
%% structures. The chosen value is about half the maxium size of a
%% smallnum. That means that adding a small constant to it will result
%% in a smallnum, but still the value is still sufficiently high to
%% make it impossible to reach in the foreseeable future.
%%
-define(SIZE_UPPER_LIMIT, ((1 bsl 58) - 1)).

-define(UNICODE_TYPE, #t_integer{elements={0,16#10FFFF}}).

%%
%% Returns whether a call will succeed or not.
%%
%% Notes:
%%
%% This function only answers 'yes' for functions in the 'erlang' module as
%% calls to other modules may fail due to not being loaded, even if we consider
%% the module to be known.
%%
%% This function MUST return 'no' if types/3 with the same arguments will return
%% the return type 'none'.
%%

-spec will_succeed(Mod, Func, ArgTypes) -> Result when
      Mod :: atom(),
      Func :: atom(),
      ArgTypes :: [type()],
      Result :: 'yes' | 'no' | 'maybe'.

will_succeed(erlang, Op, [LHS, RHS]) when Op =:= '+';
                                          Op =:= '-';
                                          Op =:= '*' ->
    succeeds_if_smallish(LHS, RHS);
will_succeed(erlang, Op, [LHS, RHS]=Args) when Op =:= 'div';
                                               Op =:= 'rem' ->
    case {meet(LHS, #t_integer{}), meet(RHS, #t_integer{})} of
        {#t_integer{elements={_,_}}=LHS,
         #t_integer{elements={Min,Max}}=RHS}
          when is_integer(Min), Min > 0;
               is_integer(Max), Max < -1 ->
            'yes';
        {#t_integer{}, #t_integer{}} ->
            fails_on_conflict(erlang, Op, Args);
        {_, _} ->
            no
    end;
will_succeed(erlang, 'bsr'=Op, [LHS, RHS]=Args) ->
    case {meet(LHS, #t_integer{}), meet(RHS, #t_integer{})} of
        {#t_integer{elements={_,_}}=LHS,
         #t_integer{elements={MinShift,_}}=RHS}
          when is_integer(MinShift), MinShift >= 0 ->
            'yes';
        {#t_integer{}, #t_integer{}} ->
            fails_on_conflict(erlang, Op, Args);
        {_, _} ->
            no
    end;
will_succeed(erlang, 'bsl'=Op, [LHS, RHS]=Args) ->
    case {meet(LHS, #t_integer{}), meet(RHS, #t_integer{})} of
        {LHS, #t_integer{elements={_,MaxShift}}=RHS}
          when is_integer(MaxShift), MaxShift < 64 ->
            succeeds_if_smallish(LHS);
        {#t_integer{}, #t_integer{}} ->
            fails_on_conflict(erlang, Op, Args);
        {_, _} ->
            no
    end;
will_succeed(erlang, '++', [LHS, _RHS]) ->
    succeeds_if_type(LHS, proper_list());
will_succeed(erlang, '--', [_, _] = Args) ->
    succeeds_if_types(Args, proper_list());
will_succeed(erlang, BoolOp, [_, _] = Args) when BoolOp =:= 'and';
                                                 BoolOp =:= 'or' ->
    succeeds_if_types(Args, beam_types:make_boolean());
will_succeed(erlang, Op, [_, _] = Args) when Op =:= 'band';
                                             Op =:= 'bor';
                                             Op =:= 'bxor' ->
    succeeds_if_types(Args, #t_integer{});
will_succeed(erlang, bit_size, [Arg]) ->
    succeeds_if_type(Arg, #t_bs_matchable{});
will_succeed(erlang, byte_size, [Arg]) ->
    succeeds_if_type(Arg, #t_bs_matchable{});
will_succeed(erlang, element, [Pos, Tuple]=Args) ->
    case normalize(Tuple) of
        #t_tuple{exact=Exact,size=Sz} when Sz >= 1 ->
            case meet(Pos, #t_integer{elements={1,Sz}}) of
                Pos -> yes;
                none when Exact -> no;
                _ -> fails_on_conflict(erlang, element, Args)

            end;
        _ ->
            fails_on_conflict(erlang, element, Args)
    end;
will_succeed(erlang, hd, [Arg]) ->
    succeeds_if_type(Arg, #t_cons{});
will_succeed(erlang, is_function, [_, Arity]=Args) ->
    case meet(Arity, #t_integer{}) of
        #t_integer{elements={Min,_}}=Arity when is_integer(Min), Min >= 0 ->
            yes;
        #t_integer{} ->
            fails_on_conflict(erlang, is_function, Args);
        _ ->
            no
    end;
will_succeed(erlang, is_map_key, [_Key, Map]) ->
    succeeds_if_type(Map, #t_map{});
will_succeed(erlang, length, [Arg]) ->
    succeeds_if_type(Arg, proper_list());
will_succeed(erlang, map_size, [Arg]) ->
    succeeds_if_type(Arg, #t_map{});
will_succeed(erlang, node, [Arg]) ->
    succeeds_if_type(Arg, identifier);
will_succeed(erlang, 'and', [_, _]=Args) ->
    succeeds_if_types(Args, beam_types:make_boolean());
will_succeed(erlang, 'not', [Arg]) ->
    succeeds_if_type(Arg, beam_types:make_boolean());
will_succeed(erlang, 'or', [_, _]=Args) ->
    succeeds_if_types(Args, beam_types:make_boolean());
will_succeed(erlang, 'xor', [_, _]=Args) ->
    succeeds_if_types(Args, beam_types:make_boolean());
will_succeed(erlang, setelement, [Pos, Tuple0, _Value]=Args) ->
    PosRange = #t_integer{elements={1,?MAX_TUPLE_SIZE}},
    case {meet(Pos, PosRange), meet(Tuple0, #t_tuple{size=1})} of
        {none, _} ->
            no;
        {_, none} ->
            no;
        {#t_integer{elements={Min,Max}}=Pos, Tuple} ->
            MaxTupleSize = max_tuple_size(Tuple),
            if
                MaxTupleSize < Min ->
                    %% Index is always out of range.
                    no;
                Tuple0 =:= Tuple, Max =< MaxTupleSize ->
                    %% We always have a tuple, and the index is always in
                    %% range.
                    yes;
                true ->
                    %% We may or may not have a tuple, and the index may or may
                    %% not be in range if we do.
                    fails_on_conflict(erlang, setelement, Args)
            end;
        {_, _} ->
            fails_on_conflict(erlang, setelement, Args)
    end;
will_succeed(erlang, size, [Arg]) ->
    ArgType = join(#t_tuple{}, #t_bitstring{}),
    succeeds_if_type(Arg, ArgType);
will_succeed(erlang, tuple_size, [Arg]) ->
    succeeds_if_type(Arg, #t_tuple{});
will_succeed(erlang, tl, [Arg]) ->
    succeeds_if_type(Arg, #t_cons{});
will_succeed(erlang, raise, [Class, _Reason, nil]) ->
    case meet(Class, #t_atom{elements=[error,exit,throw]}) of
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
                    fails_on_conflict(Mod, Func, Args)
            end
    end.

max_tuple_size(#t_union{tuple_set=[_|_]=Set}=Union) ->
    Union = meet(Union, #t_tuple{}),            %Assertion.
    Arities = [Arity || {{Arity, _Tag}, _Record} <:- Set],
    lists:max(Arities);
max_tuple_size(#t_tuple{exact=true,size=Size}) ->
    Size;
max_tuple_size(#t_tuple{exact=false}) ->
    ?MAX_TUPLE_SIZE.

%% While we can't infer success for functions outside the 'erlang'
%% module, it's safe to infer failure when we know they return `none` or
%% if the arguments must have certain types.
%%
%% Returns: 'maybe' or 'no'
fails_on_conflict(Mod, Func, Args) ->
    case types(Mod, Func, Args) of
        {none, _, _} -> no;
        {_, ArgTypes, _} -> fails_on_conflict_1(Args, ArgTypes)
    end.

fails_on_conflict_1([ArgType | Args], [Required | Types]) ->
    case meet(ArgType, Required) of
        none -> no;
        _ -> fails_on_conflict_1(Args, Types)
    end;
fails_on_conflict_1([], []) ->
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
    case meet(ArgType, Required) of
        ArgType -> yes;
        none -> no;
        _ -> 'maybe'
    end.

succeeds_if_smallish(#t_integer{elements={Min,Max}})
  when abs(Min) bsr 128 =:= 0, abs(Max) bsr 128 =:= 0 ->
    yes;
succeeds_if_smallish(ArgType) ->
    case succeeds_if_type(ArgType, #t_number{}) of
        yes ->
            %% Could potentially fail with a `system_limit` exception.
            'maybe';
        Other ->
            Other
    end.

succeeds_if_smallish(LHS, RHS) ->
    case {succeeds_if_smallish(LHS),
          succeeds_if_smallish(RHS)} of
        {yes, yes} -> yes;
        {no, _} -> no;
        {_, no} -> no;
        {_, _} -> 'maybe'
    end.

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
    Min = case normalize(meet(Src, #t_tuple{})) of
              #t_tuple{size=Sz} -> Sz;
              _ -> 0
          end,
    Max = ?MAX_TUPLE_SIZE,
    sub_safe(#t_integer{elements={Min,Max}}, [#t_tuple{}]);
types(erlang, 'bit_size', [_]) ->
    sub_safe(#t_integer{elements={0,?SIZE_UPPER_LIMIT}}, [#t_bs_matchable{}]);
types(erlang, 'byte_size', [_]) ->
    sub_safe(#t_integer{elements={0,?SIZE_UPPER_LIMIT}}, [#t_bs_matchable{}]);
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
types(erlang, Op, [Arg1, Arg2])
  when Op =:= '<'; Op =:= '=<'; Op =:= '>='; Op =:= '>' ->
    {R1,R2} = {get_range(Arg1), get_range(Arg2)},
    case beam_bounds:relop(Op, R1, R2) of
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
            case meet(Type, #t_atom{}) of
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
types(erlang, is_function, [Type, ArityType]) ->
    RetType = case meet(ArityType, #t_integer{}) of
                  none ->
                      none;
                  #t_integer{elements={Arity,Arity}}
                    when is_integer(Arity) ->
                      if
                          Arity < 0 ->
                              none;
                          0 =< Arity, Arity =< ?MAX_FUNC_ARGS ->
                              case meet(Type, #t_fun{arity=Arity}) of
                                  Type -> #t_atom{elements=[true]};
                                  none -> #t_atom{elements=[false]};
                                  _ -> beam_types:make_boolean()
                              end;
                          Arity > ?MAX_FUNC_ARGS ->
                              #t_atom{elements=[false]}
                      end;
                  #t_integer{} ->
                      case meet(Type, #t_fun{}) of
                          none -> #t_atom{elements=[false]};
                          _ -> beam_types:make_boolean()
                      end
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
    sub_unsafe_type_test(Type, #t_number{});
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
    sub_unsafe(beam_bounds_type('band', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);
types(erlang, 'bor', [_,_]=Args) ->
    sub_unsafe(beam_bounds_type('bor', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);
types(erlang, 'bxor', [_,_]=Args) ->
    sub_unsafe(beam_bounds_type('bxor', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);
types(erlang, 'bsl', [_,_]=Args) ->
    sub_unsafe(beam_bounds_type('bsl', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);
types(erlang, 'bsr', [_,_]=Args) ->
    sub_unsafe(beam_bounds_type('bsr', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);
types(erlang, 'bnot', [_]) ->
    %% Calculating the tighest possible range for the result would
    %% cause the type analysis pass to loop for a very long time for
    %% code such as:
    %%
    %%     f(0) -> -1;
    %%     f(N) -> abs(bnot f(N)).
    %%
    %% By calculating looser bounds and widening the range to `any` at
    %% some suitable limit, convergence can be ensured (see
    %% 8e5b1fbb16d186). However, that can cause a contradiction
    %% between the ranges calculated by the type pass and by
    %% beam_validator.
    %%
    %% Therefore, don't attempt to calculate a range now. Save the
    %% range calculation for the opt_ranges pass (arith_type/2), which
    %% is only run once.
    sub_unsafe(#t_integer{}, [#t_integer{}]);

%% Fixed-type arithmetic
types(erlang, 'float', [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(erlang, 'round', [_]) ->
    sub_unsafe(#t_integer{}, [#t_number{}]);
types(erlang, 'floor', [_]) ->
    sub_unsafe(#t_integer{}, [#t_number{}]);
types(erlang, 'ceil', [_]) ->
    sub_unsafe(#t_integer{}, [#t_number{}]);
types(erlang, 'trunc', [_]) ->
    sub_unsafe(#t_integer{}, [#t_number{}]);
types(erlang, '/', [_,_]) ->
    sub_unsafe(#t_float{}, [#t_number{}, #t_number{}]);
types(erlang, 'div', [_,_]=Args) ->
    sub_unsafe(beam_bounds_type('div', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);
types(erlang, 'rem', Args) ->
    sub_unsafe(beam_bounds_type('rem', #t_integer{}, Args),
               [#t_integer{}, #t_integer{}]);

%% Some mixed-type arithmetic.
types(erlang, Op, [LHS, RHS]) when Op =:= '+'; Op =:= '-' ->
    case get_range(LHS, RHS, #t_number{}) of
        {Type, {A,B}, {C,_D}} when is_integer(C), C >= 0 ->
            R = beam_bounds:bounds(Op, {A,B}, {C,'+inf'}),
            RetType = case Type of
                          integer -> #t_integer{elements=R};
                          number -> #t_number{elements=R}
                      end,
            sub_unsafe(RetType, [#t_number{}, #t_number{}]);
        {Type, {A,_B}, {C,D}} when Op =:= '+', is_integer(A), A >= 0 ->
            R = beam_bounds:bounds(Op, {A,'+inf'}, {C,D}),
            RetType = case Type of
                          integer -> #t_integer{elements=R};
                          number -> #t_number{elements=R}
                      end,
            sub_unsafe(RetType, [#t_number{}, #t_number{}]);
        _ ->
            mixed_arith_types([LHS, RHS])
    end;

types(erlang, '*', [LHS, RHS]) ->
    case get_range(LHS, RHS, #t_number{}) of
        {Type, {A,B}, {C,D}} ->
            case beam_bounds:bounds('*', {A,B}, {C,D}) of
                {Min,_Max} when is_integer(Min), Min >= 0 ->
                    R = {Min,'+inf'},
                    RetType = case Type of
                                  integer -> #t_integer{elements=R};
                                  number -> #t_number{elements=R}
                              end,
                    sub_unsafe(RetType, [#t_number{}, #t_number{}]);
                _ ->
                    mixed_arith_types([LHS, RHS])
            end;
        _ ->
            mixed_arith_types([LHS, RHS])
    end;

types(erlang, abs, [Type]) ->
    case meet(Type, #t_number{}) of
        #t_float{} ->
            sub_unsafe(#t_float{}, [#t_float{}]);
        #t_integer{elements=R} ->
            RetType = #t_integer{elements=beam_bounds:bounds(abs, R)},
            sub_unsafe(RetType, [#t_integer{}]);
        #t_number{elements=R} ->
            RetType = #t_number{elements=beam_bounds:bounds(abs, R)},
            sub_unsafe(RetType, [#t_number{}]);
        _ ->
            sub_unsafe(#t_number{}, [#t_number{}])
    end;

%% The rest of the mixed-type arithmetic is handled in the catch-all
%% clause for the 'erlang' module.

%% List operations
types(erlang, '++', [LHS, RHS]) ->
    %% `[] ++ RHS` yields RHS, even if RHS is not a list.
    ListType = copy_list(LHS, same_length, proper),
    RetType = join(ListType, RHS),
    sub_unsafe(RetType, [proper_list(), any]);
types(erlang, '--', [LHS, _]) ->
    RetType = copy_list(LHS, new_length, proper),
    sub_unsafe(RetType, [proper_list(), proper_list()]);

types(erlang, atom_to_list, [_]) ->
    sub_unsafe(proper_list(?UNICODE_TYPE), [#t_atom{}]);
types(erlang, 'iolist_to_binary', [_]) ->
    %% Arg is an iodata(), despite its name.
    ArgType = join(#t_list{}, #t_bitstring{size_unit=8}),
    sub_unsafe(#t_bitstring{size_unit=8}, [ArgType]);
types(erlang, 'iolist_size', [_]) ->
    %% Arg is an iodata(), despite its name. The size is NOT limited
    %% by the size of memory.
    ArgType = join(#t_list{}, #t_bitstring{size_unit=8}),
    sub_unsafe(#t_integer{elements={0,'+inf'}}, [ArgType]);
types(erlang, 'list_to_binary', [_]) ->
    %% Arg is an iolist(), despite its name.
    sub_unsafe(#t_bitstring{size_unit=8}, [#t_list{}]);
types(erlang, 'list_to_bitstring', [_]) ->
    %% As list_to_binary but with bitstrings rather than binaries.
    sub_unsafe(#t_bitstring{}, [#t_list{}]);
types(erlang, list_to_integer, [_]) ->
    sub_unsafe(#t_integer{}, [proper_cons()]);
types(erlang, list_to_integer, [_, _]) ->
    sub_unsafe(#t_integer{}, [proper_cons(), #t_integer{}]);

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

%% Conversion functions.
types(erlang, atom_to_binary, [_]) ->
    sub_safe(binary(), [#t_atom{}]);
types(erlang, binary_to_integer, [_]) ->
    sub_unsafe(#t_integer{}, [binary()]);
types(erlang, binary_to_list, [_]) ->
    sub_safe(proper_list(), [binary()]);
types(erlang, integer_to_list, [_]) ->
    sub_safe(proper_cons(), [#t_integer{}]);
types(erlang, list_to_atom, [_]) ->
    sub_unsafe(#t_atom{}, [#t_list{}]);
types(erlang, list_to_tuple, [Arg]) ->
    Sz = case meet(Arg, #t_list{}) of
             #t_cons{} -> 1;
             _ -> 0
         end,
    sub_unsafe(#t_tuple{size=Sz}, [#t_list{}]);
types(erlang, term_to_binary, [_]) ->
    sub_unsafe(binary(), [any]);
types(erlang, tuple_to_list, [Arg]) ->
    T = case meet(Arg, #t_tuple{}) of
            #t_tuple{size=Sz} when Sz >= 1 ->
                proper_cons();
            _ ->
                proper_list()
        end,
    sub_safe(T, [#t_tuple{}]);

%% Misc functions returning integers.
types(erlang, convert_time_unit, [_, _, _]) ->
    sub_unsafe(#t_integer{}, [any, any, any]);
types(erlang, monotonic_time, []) ->
    sub_unsafe(#t_integer{}, []);
types(erlang, phash2, [_]) ->
    R = {0, (1 bsl 27) - 1},
    sub_unsafe(#t_integer{elements=R}, [any]);
types(erlang, phash2, [_, _]) ->
    R = {0, (1 bsl 32) - 1},
    sub_unsafe(#t_integer{elements=R}, [any, any]);
types(erlang, unique_integer, []) ->
    sub_unsafe(#t_integer{}, []);
types(erlang, unique_integer, [_]) ->
    sub_unsafe(#t_integer{}, [any]);

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
    sub_unsafe(#t_atom{}, [identifier]);
types(erlang, 'node', []) ->
    sub_unsafe(#t_atom{}, []);
types(erlang, self, []) ->
    sub_unsafe(pid, []);
types(erlang, 'size', [_]) ->
    ArgType = join(#t_tuple{}, #t_bitstring{}),
    sub_unsafe(#t_integer{}, [ArgType]);
types(erlang, split_binary, [_, _]) ->
    %% Note that, contrary to the documentation at the time of writing,
    %% split_binary/2 accepts a bitstring and that it can return a
    %% bitstring in the second element of the result tuple.
    Binary = binary(),
    T = make_two_tuple(Binary, #t_bitstring{}),
    sub_unsafe(T, [#t_bitstring{}, #t_integer{}]);

%% Tuple element ops
types(erlang, element, [Pos, Tuple0]) ->
    PosRange = #t_integer{elements={1,?MAX_TUPLE_SIZE}},
    case meet(Pos, PosRange) of
        #t_integer{elements={Index,Index}} when Index >= 1 ->
            case normalize(meet(Tuple0, #t_tuple{size=Index})) of
                #t_tuple{elements=Es}=Tuple ->
                    RetType = beam_types:get_tuple_element(Index, Es),
                    sub_unsafe(RetType, [PosRange, Tuple]);
                none ->
                    sub_unsafe(none, [PosRange, #t_tuple{}])
            end;
        _ ->
            sub_unsafe(any, [PosRange, #t_tuple{}])
    end;
types(erlang, setelement, [PosType, TupleType, ArgType]) ->
    PosRange = #t_integer{elements={1,?MAX_TUPLE_SIZE}},
    RetType = case meet(PosType, PosRange) of
                  #t_integer{elements={Same,Same}} ->
                      beam_types:update_tuple(TupleType, [{Same, ArgType}]);
                  #t_integer{} ->
                      case normalize(meet(TupleType, #t_tuple{size=1})) of
                          #t_tuple{}=T -> T#t_tuple{elements=#{}};
                          none -> none
                      end;
                  none ->
                      none
              end,
    sub_unsafe(RetType, [PosRange, #t_tuple{size=1}, any]);

types(erlang, make_fun, [_,_,Arity0]) ->
    Type = case meet(Arity0, #t_integer{}) of
               #t_integer{elements={Arity,Arity}}
                 when Arity >= 0, Arity =< ?MAX_FUNC_ARGS ->
                   #t_fun{arity=Arity};
               #t_integer{} ->
                   #t_fun{};
               _ ->
                   none
           end,
    sub_unsafe(Type, [#t_atom{}, #t_atom{}, #t_integer{}]);

types(erlang, min, [LHS,RHS]) ->
    R1 = case get_range(meet(LHS, #t_number{})) of
             none -> any;
             R10 -> R10
         end,
    R2 = case get_range(meet(RHS, #t_number{})) of
             none -> any;
             R20 -> R20
         end,
    R = beam_bounds:bounds(min, R1, R2),
    RetType = case {LHS, RHS} of
                  {#t_integer{}, #t_integer{}} ->
                      #t_integer{elements=R};
                  {#t_integer{}, _} ->
                      #t_number{elements=R};
                  {#t_number{}, _} ->
                      #t_number{elements=R};
                  {#t_float{}, _} ->
                      #t_number{elements=R};
                  {_, #t_integer{}} ->
                      #t_number{elements=R};
                  {_, #t_number{}} ->
                      #t_number{elements=R};
                  {_, #t_float{}} ->
                      #t_number{elements=R};
                  {_, _} ->
                      join(LHS, RHS)
              end,
    sub_unsafe(RetType, [any, any]);

types(erlang, max, [LHS,RHS]) ->
    RetType =
        case get_range(LHS, RHS, #t_number{}) of
            {_, none, _} ->
                join(LHS, RHS);
            {_, _, none} ->
                join(LHS, RHS);
            {_, R1, R2} ->
                R = beam_bounds:bounds(max, R1, R2),
                case {LHS, RHS} of
                    {#t_integer{}, #t_integer{}} ->
                        #t_integer{elements=R};
                    {any, #t_integer{elements={Min,_}}} when is_integer(Min) ->
                        beam_types:subtract(any, #t_number{elements={'-inf',Min}});
                    {#t_integer{elements={Min,_}}, any} when is_integer(Min) ->
                        beam_types:subtract(any, #t_number{elements={'-inf',Min}});
                    {_, _} ->
                        case join(LHS, RHS) of
                            #t_number{} ->
                                #t_number{elements=R};
                            Join ->
                                Join
                        end
                end
        end,
    sub_unsafe(RetType, [any, any]);

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
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, cosh, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, sin, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, sinh, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, tan, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, tanh, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, acos, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, acosh, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, asin, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, asinh, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, atan, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, atanh, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, erf, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, erfc, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, exp, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, log, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, log2, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, log10, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, sqrt, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, atan2, [_,_]) ->
    sub_unsafe(#t_float{}, [#t_number{}, #t_number{}]);
types(math, pow, [_,_]) ->
    sub_unsafe(#t_float{}, [#t_number{}, #t_number{}]);
types(math, ceil, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, floor, [_]) ->
    sub_unsafe(#t_float{}, [#t_number{}]);
types(math, fmod, [_,_]) ->
    sub_unsafe(#t_float{}, [#t_number{}, #t_number{}]);
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
    %% element before reaching the end. It can also return 'true' for the
    %% empty list, which means we cannot assume the predicate is a fun.
    sub_unsafe(beam_types:make_boolean(), [any, #t_list{}]);
types(lists, any, [_,_]) ->
    %% Doesn't imply that the argument is a proper list, nor that the fun is
    %% valid; see lists:all/2
    sub_unsafe(beam_types:make_boolean(), [any, #t_list{}]);
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
    sub_unsafe(RetType, [any, any, proper_list()]);
types(lists, foldr, [Fun, Init, List]) ->
    RetType = lists_fold_type(Fun, Init, List),
    sub_unsafe(RetType, [any, any, proper_list()]);

%% Functions returning plain lists.
types(lists, droplast, [List]) ->
    RetType = copy_list(List, new_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, dropwhile, [_Fun, List]) ->
    %% If the element is found before the end of the list, we could return an
    %% improper list.
    RetType = copy_list(List, new_length, maybe_improper),
    sub_unsafe(RetType, [any, #t_list{}]);
types(lists, duplicate, [_Count, Element]) ->
    sub_unsafe(proper_list(Element), [#t_integer{}, any]);
types(lists, filter, [_Fun, List]) ->
    RetType = copy_list(List, new_length, proper),
    sub_unsafe(RetType, [any, proper_list()]);
types(lists, flatten, [_]) ->
    sub_unsafe(proper_list(), [proper_list()]);
types(lists, map, [Fun, List]) ->
    RetType = lists_map_type(Fun, List),
    sub_unsafe(RetType, [any, proper_list()]);
types(lists, reverse, [List]) ->
    RetType = copy_list(List, same_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, sort, [List]) ->
    RetType = copy_list(List, same_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, takewhile, [_Fun, List]) ->
    %% Doesn't imply that the argument is a proper list, nor that the fun is
    %% valid; see lists:all/2
    RetType = copy_list(List, new_length, proper),
    sub_unsafe(RetType, [any, #t_list{}]);
types(lists, usort, [List]) ->
    %% The result is not quite the same length, but a non-empty list will stay
    %% non-empty.
    RetType = copy_list(List, same_length, proper),
    sub_unsafe(RetType, [proper_list()]);
types(lists, zip, [_,_]=Lists) ->
    {RetType, ArgType} = lists_zip_types(Lists),
    sub_unsafe(RetType, [ArgType, ArgType]);
types(lists, zipwith, [Fun | [_,_]=Lists]) ->
    %% Doesn't imply that the argument is a fun, as a possible implementation
    %% could succeed when both lists are empty.
    {RetType, ArgType} = lists_zipwith_types(Fun, Lists),
    sub_unsafe(RetType, [any, ArgType, ArgType]);

%% Functions with complex return values.
types(lists, keyfind, [KeyType,PosType,_]) ->
    %% Doesn't imply that the argument is a proper list; see lists:all/2
    TupleType = case meet(PosType, #t_integer{}) of
                    #t_integer{elements={Index,Index}} when is_integer(Index),
                                                            Index >= 1 ->
                        Es = beam_types:set_tuple_element(Index, KeyType, #{}),
                        #t_tuple{size=Index,elements=Es};
                    #t_integer{} ->
                        #t_tuple{};
                    none ->
                        none
                end,
    RetType = join(TupleType, beam_types:make_atom(false)),
    sub_unsafe(RetType, [any, any, #t_list{}]);
types(lists, MapFold, [Fun, Init, List])
  when MapFold =:= mapfoldl; MapFold =:= mapfoldr ->
    RetType = lists_mapfold_type(Fun, Init, List),
    %% Doesn't imply that the argument is a proper list, nor that the fun is
    %% valid; see lists:all/2
    sub_unsafe(RetType, [any, any, proper_list()]);
types(lists, partition, [_Fun, List]) ->
    %% Doesn't imply that the argument is a proper list, nor that the fun is
    %% valid; see lists:all/2
    ListType = copy_list(List, new_length, proper),
    RetType = make_two_tuple(ListType, ListType),
    sub_unsafe(RetType, [any, proper_list()]);
types(lists, search, [_,_]) ->
    %% Doesn't imply that the argument is a proper list, nor that the fun is
    %% valid; see lists:all/2
    TupleType = make_two_tuple(beam_types:make_atom(value), any),
    RetType = join(TupleType, beam_types:make_atom(false)),
    sub_unsafe(RetType, [any, #t_list{}]);
types(lists, splitwith, [_Fun, List]) ->
    %% Only the elements in the left list are guaranteed to be visited, so both
    %% the argument and the right list may be improper. The fun isn't
    %% guaranteed to be valid either if the list is empty.
    Left = copy_list(List, new_length, proper),
    Right = copy_list(List, new_length, maybe_improper),
    sub_unsafe(make_two_tuple(Left, Right), [any, #t_list{}]);
types(lists, unzip, [List]) ->
    RetType = lists_unzip_type(2, List),
    sub_unsafe(RetType, [proper_list()]);

%%
%% Map functions
%%

types(maps, filter, [_Fun, Map]) ->
    %% Conservatively assume that key/value types are unchanged. Note that we
    %% cannot assume that Fun is a function on success, as a potential
    %% implementation could short-circuit on the empty map.
    RetType = case meet(Map, #t_map{}) of
                  #t_map{}=T -> T;
                  _ -> none
              end,
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, find, [Key, Map]) ->
    TupleType = case erlang_map_get_type(Key, Map) of
                    none ->
                        none;
                    ValueType ->
                        make_two_tuple(beam_types:make_atom(ok), ValueType)
                end,
    %% error | {ok, Value}
    RetType = join(beam_types:make_atom(error), TupleType),
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, fold, [Fun, Init, _Map]) ->
    RetType = case meet(Fun, #t_fun{arity=3}) of
                  #t_fun{type=Type} ->
                      %% The map is potentially empty, so we have to assume it
                      %% can return the initial value.
                      join(Type, Init);
                  _ ->
                      %% A potential implementation could still succeed with a
                      %% non-fun for empty maps.
                      Init
              end,
    sub_unsafe(RetType, [any, any, #t_map{}]);
types(maps, from_keys, [Keys, Value]) ->
    KeyType = erlang_hd_type(Keys),
    ValueType = case KeyType of
                    none -> none;
                    _ -> Value
                end,
    RetType = #t_map{super_key=KeyType,super_value=ValueType},
    sub_unsafe(RetType, [proper_list(), any]);
types(maps, from_list, [Pairs]) ->
    PairType = erlang_hd_type(Pairs),
    RetType = case normalize(meet(PairType, #t_tuple{exact=true,size=2})) of
                  #t_tuple{elements=Es} ->
                      SKey = beam_types:get_tuple_element(1, Es),
                      SValue = beam_types:get_tuple_element(2, Es),
                      #t_map{super_key=SKey,super_value=SValue};
                  none when PairType =:= none ->
                      #t_map{super_key=none,super_value=none};
                  none when PairType =/= none ->
                      none
              end,
    sub_unsafe(RetType, [proper_list()]);
types(maps, get, [_Key, _Map]=Args) ->
    types(erlang, map_get, Args);
types(maps, get, [Key, Map, Default]) ->
    RetType = case erlang_map_get_type(Key, Map) of
                  none -> Default;
                  ValueType -> join(ValueType, Default)
              end,
    sub_unsafe(RetType, [any, #t_map{}, any]);
types(maps, keys, [Map]) ->
    RetType = case meet(Map, #t_map{}) of
                  #t_map{super_key=none} -> nil;
                  #t_map{super_key=SKey} -> proper_list(SKey);
                  _ -> none
              end,
    sub_unsafe(RetType, [#t_map{}]);
types(maps, map, [Fun, Map0]) ->
    RetType = case {meet(Fun, #t_fun{arity=2}), meet(Map0, #t_map{})} of
                  {#t_fun{type=FunRet}, #t_map{super_value=SValue0}=Map} ->
                      SValue = join(FunRet, SValue0),
                      Map#t_map{super_value=SValue};
                  {none, #t_map{}} ->
                      %% A potential implementation could still work on empty
                      %% maps even when the fun is broken.
                      #t_map{super_key=none,super_value=none};
                  {_, none} ->
                      none
              end,
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, merge, [A, B]) ->
    RetType = case {meet(A, #t_map{}), meet(B, #t_map{})} of
                  {#t_map{super_key=SKeyA,super_value=SValueA},
                   #t_map{super_key=SKeyB,super_value=SValueB}} ->
                      SKey = join(SKeyA, SKeyB),
                      SValue = join(SValueA, SValueB),
                      #t_map{super_key=SKey,super_value=SValue};
                  _ ->
                      none
              end,
    sub_unsafe(RetType, [#t_map{}, #t_map{}]);
types(maps, new, []) ->
    RetType = #t_map{super_key=none,super_value=none},
    sub_unsafe(RetType, []);
types(maps, put, [Key, Value, Map]) ->
    RetType = case meet(Map, #t_map{}) of
                  #t_map{super_key=SKey0,super_value=SValue0} ->
                      SKey = join(Key, SKey0),
                      SValue = join(Value, SValue0),
                      #t_map{super_key=SKey,super_value=SValue};
                  _ ->
                      none
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
                        MapType = meet(Map, #t_map{}),
                        make_two_tuple(ValueType, MapType)
                end,
    %% error | {Value, Map}
    RetType = join(beam_types:make_atom(error), TupleType),
    sub_unsafe(RetType, [any, #t_map{}]);
types(maps, to_list, [Map]) ->
    RetType = case meet(Map, #t_map{}) of
                  #t_map{super_key=SKey,super_value=SValue} ->
                      proper_list(make_two_tuple(SKey, SValue));
                  _ ->
                      none
              end,
    sub_unsafe(RetType, [#t_map{}]);
types(maps, update_with, [_Key, Fun, Map0]) ->
    RetType = case {meet(Fun, #t_fun{arity=1}), meet(Map0, #t_map{})} of
                  {#t_fun{type=FunRet}, #t_map{super_value=SValue0}=Map}
                    when FunRet =/= none ->
                      SValue = join(FunRet, SValue0),
                      Map#t_map{super_value=SValue};
                  _ ->
                      none
              end,
    sub_unsafe(RetType, [any, #t_fun{arity=1}, #t_map{}]);
types(maps, values, [Map]) ->
    RetType = case meet(Map, #t_map{}) of
                  #t_map{super_value=none} -> nil;
                  #t_map{super_value=SValue} -> proper_list(SValue);
                  _ -> none
              end,
    sub_unsafe(RetType, [#t_map{}]);
types(maps, with, [Keys, Map0]) ->
    RetType = case {erlang_hd_type(Keys), meet(Map0, #t_map{})} of
                  {none, _} ->
                      #t_map{super_key=none,super_value=none};
                  {KeysType, #t_map{super_key=SKey0}=Map} ->
                      %% Since we know that the Map will only contain the pairs
                      %% pointed out by Keys, we can restrict the types to
                      %% those in the list.
                      SKey = meet(KeysType, SKey0),
                      Map#t_map{super_key=SKey};
                  {_, _} ->
                      none
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
      ArgTypes :: [type()],
      RetType :: type().

arith_type({bif,'-'}, [Arg]) ->
    ArgTypes = [#t_integer{elements={0,0}},Arg],
    beam_bounds_type('-', #t_number{}, ArgTypes);
arith_type({bif,'bnot'}, [Arg0]) ->
    case meet(Arg0, #t_integer{}) of
        none ->
            none;
        #t_integer{elements=R} ->
            #t_integer{elements=beam_bounds:bounds('bnot', R)}
    end;
arith_type({bif,Op}, [_,_]=ArgTypes) when Op =:= '+';
                                          Op =:= '-';
                                          Op =:= '*' ->
    beam_bounds_type(Op, #t_number{}, ArgTypes);
arith_type({bif,Op}, [_,_]=ArgTypes) when Op =:= 'band';
                                          Op =:= 'bor';
                                          Op =:= 'bsl';
                                          Op =:= 'bsr';
                                          Op =:= 'bxor';
                                          Op =:= 'div';
                                          Op =:= 'rem' ->
    beam_bounds_type(Op, #t_integer{}, ArgTypes);
arith_type(_Op, _Args) ->
    any.

%%
%% Function-specific helpers.
%%

mixed_arith_types(Args0) ->
    [FirstType|_] = Args = [meet(A, #t_number{}) || A <- Args0],
    RetType = foldl(fun(#t_integer{}, #t_integer{}) -> #t_integer{};
                       (#t_integer{}, #t_number{}) -> #t_number{};
                       (#t_integer{}, #t_float{}) -> #t_float{};
                       (#t_float{}, #t_integer{}) -> #t_float{};
                       (#t_float{}, #t_number{}) -> #t_float{};
                       (#t_float{}, #t_float{}) -> #t_float{};
                       (#t_number{}, #t_integer{}) -> #t_number{};
                       (#t_number{}, #t_float{}) -> #t_float{};
                       (#t_number{}, #t_number{}) -> #t_number{};
                       (_, _) -> none
                    end, FirstType, Args),
    sub_unsafe(RetType, [#t_number{} || _ <- Args]).

erlang_hd_type(Src) ->
    case meet(Src, #t_cons{}) of
        #t_cons{type=Type} -> Type;
        none -> none
    end.

erlang_tl_type(Src) ->
    case meet(Src, #t_cons{}) of
        #t_cons{terminator=Term}=Cons -> join(Cons, Term);
        none -> none
    end.

beam_bounds_type(Op, Type, [LHS, RHS]) ->
    case get_range(LHS, RHS, Type) of
        {_, none, _} ->
            none;
        {_, _, none} ->
            none;
        {float, _R1, _R2} ->
            #t_float{};
        {integer, R1, R2} ->
            #t_integer{elements=beam_bounds:bounds(Op, R1, R2)};
        {number, R1, R2} ->
            #t_number{elements=beam_bounds:bounds(Op, R1, R2)}
    end.

get_range(LHS, RHS, Type) ->
    get_range(meet(LHS, Type), meet(RHS, Type)).

get_range(#t_float{}=LHS, #t_float{}=RHS) ->
    {float, get_range(LHS), get_range(RHS)};
get_range(#t_integer{}=LHS, #t_integer{}=RHS) ->
    {integer, get_range(LHS), get_range(RHS)};
get_range(LHS, RHS) ->
    {number, get_range(LHS), get_range(RHS)}.

get_range(#t_float{}) -> any;
get_range(#t_integer{elements=R}) -> R;
get_range(#t_number{elements=R}) -> R;
get_range(_) -> none.

erlang_map_get_type(Key, Map) ->
    case meet(Map, #t_map{}) of
        #t_map{super_key=SKey,super_value=SValue} ->
            case meet(SKey, Key) of
                none -> none;
                _ -> SValue
            end;
        none ->
            none
    end.

lists_fold_type(Fun, Init, List) ->
    lists_fold_type_1(meet(Fun, #t_fun{arity=2}),
                      Init,
                      meet(List, #t_list{})).

lists_fold_type_1(_Fun, Init, nil) ->
    Init;
lists_fold_type_1(#t_fun{type=Type}, _Init, #t_cons{}) ->
    %% The list is non-empty so it's safe to ignore Init.
    Type;
lists_fold_type_1(#t_fun{type=Type}, Init, #t_list{}) ->
    %% The list is possibly empty so we have to assume it can return the
    %% initial value, whose type can differ significantly from the fun's
    %% return value.
    join(Type, Init);
lists_fold_type_1(_Fun, _Init, _List) ->
    any.

lists_map_type(Fun, Types) ->
    case meet(Fun, #t_fun{arity=1}) of
        #t_fun{type=Type} -> lists_map_type_1(Types, Type);
        none -> none
    end.

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

lists_mapfold_type(Fun, Init, List) ->
    case {meet(Fun, #t_fun{type=#t_tuple{size=2}}), meet(List, #t_list{})} of
        {_, nil} ->
            make_two_tuple(nil, Init);
        {#t_fun{type=#t_tuple{elements=Es}}, ListType} ->
            ElementType = beam_types:get_tuple_element(1, Es),
            AccType = beam_types:get_tuple_element(2, Es),
            lists_mapfold_type_1(ListType, ElementType, Init, AccType);
        {#t_fun{type=none}, #t_list{}} ->
            %% The fun never returns, so the only way we could return normally
            %% is if the list is empty, in which case we'll return [] and the
            %% initial value.
            make_two_tuple(nil, Init);
        _ ->
            none
    end.

lists_mapfold_type_1(#t_cons{}, ElementType, _Init, AccType) ->
    %% The list has at least one element, so it's safe to ignore Init.
    make_two_tuple(proper_cons(ElementType), AccType);
lists_mapfold_type_1(_, ElementType, Init, AccType0) ->
    %% We can only rely on AccType when we know the list is non-empty, so we
    %% have to join it with the initial value in case the list is empty.
    AccType = join(AccType0, Init),
    make_two_tuple(proper_list(ElementType), AccType).

lists_unzip_type(Size, List) ->
    case meet(List, #t_list{type=#t_tuple{exact=true,size=Size}}) of
        none ->
            none;
        ListType ->
            Es = lut_make_elements(lut_list_types(Size, ListType), 1, #{}),
            #t_tuple{size=Size,exact=true,elements=Es}
    end.

lut_make_elements([Type | Types], Index, Es0) ->
    Es = beam_types:set_tuple_element(Index, Type, Es0),
    lut_make_elements(Types, Index + 1, Es);
lut_make_elements([], _Index, Es) ->
    Es.

lut_list_types(Size, #t_cons{type=Tuple}) ->
    #t_tuple{size=Size,elements=Es} = normalize(Tuple),
    Types = lut_element_types(1, Size, Es),
    [proper_cons(T) || T <- Types];
lut_list_types(Size, #t_list{type=Tuple}) ->
    #t_tuple{size=Size,elements=Es} = normalize(Tuple),
    Types = lut_element_types(1, Size, Es),
    [proper_list(T) || T <- Types];
lut_list_types(Size, nil) ->
    lists:duplicate(Size, nil).

lut_element_types(Index, Max, #{}) when Index > Max ->
    [];
lut_element_types(Index, Max, Es) ->
    ElementType = beam_types:get_tuple_element(Index, Es),
    [ElementType | lut_element_types(Index + 1, Max, Es)].

%% lists:zip/2 and friends only succeed when all arguments have the same
%% length, so if one of them is #t_cons{}, we can infer that all of them are
%% #t_cons{} on success.

lists_zip_types(Types0) ->
    Types = [meet(T, #t_list{terminator=nil}) || T <- Types0],
    lists_zip_types_1(Types, fun proper_list/1, #{}, 1).

lists_zip_types_1([none | _], _ListFun, _Es, _N) ->
    %% At least one of the lists is not a proper list
    {none, nil};
lists_zip_types_1([nil | _], _ListFun, _Es, _N) ->
    %% Early exit; we know the result is [] on success.
    {nil, nil};
lists_zip_types_1([#t_cons{type=Type} | Lists], _ListFun, Es0, N) ->
    Es = beam_types:set_tuple_element(N, Type, Es0),
    %% The result will be cons.
    lists_zip_types_1(Lists, fun proper_cons/1, Es, N + 1);
lists_zip_types_1([#t_list{type=Type} | Lists], ListFun, Es0, N) ->
    Es = beam_types:set_tuple_element(N, Type, Es0),
    lists_zip_types_1(Lists, ListFun, Es, N + 1);
lists_zip_types_1([], ListFun, Es, N) ->
    ElementType = #t_tuple{exact=true,size=N-1,elements=Es},
    RetType = ListFun(ElementType),
    ArgType = ListFun(any),
    {RetType, ArgType}.

lists_zipwith_types(Fun, Types0) ->
    ElementType = case meet(Fun, #t_fun{}) of
                      #t_fun{type=T} -> T;
                      none -> none
                  end,
    Types = [meet(T, #t_list{terminator=nil}) || T <- Types0],
    lists_zipwith_type_1(Types, ElementType).

lists_zipwith_type_1([nil | _], _ElementType) ->
    %% Early exit; we know the result is [] on success.
    {nil, nil};
lists_zipwith_type_1([none | _], _ElementType) ->
    %% Early exit; at least one argument cannot be a proper list.
    {none, any};
lists_zipwith_type_1([#t_cons{} | _Lists], none) ->
    %% Early exit; the list is non-empty and we know the fun never
    %% returns.
    {none, any};
lists_zipwith_type_1([#t_cons{} | _Lists], ElementType) ->
    %% Early exit; we know the result is cons on success.
    RetType = proper_cons(ElementType),
    ArgType = proper_cons(),
    {RetType, ArgType};
lists_zipwith_type_1([#t_list{} | Lists], ElementType) ->
    lists_zipwith_type_1(Lists, ElementType);
lists_zipwith_type_1([], none) ->
    %% Since we know the fun won't return, the only way we could return
    %% normally is if all lists are empty.
    {nil, nil};
lists_zipwith_type_1([], ElementType) ->
    RetType = proper_list(ElementType),
    ArgType = proper_list(),
    {RetType, ArgType}.

maps_remove_type(Key, Map0) ->
    case meet(Map0, #t_map{}) of
        #t_map{super_key=SKey0}=Map ->
            case beam_types:is_singleton_type(Key) of
                true ->
                    SKey = beam_types:subtract(SKey0, Key),
                    Map#t_map{super_key=SKey};
                false ->
                    Map
            end;
        none ->
            none
    end.

%%%
%%% Generic helpers
%%%

sub_unsafe_type_test(ArgType, Required) ->
    RetType =
        case meet(ArgType, Required) of
            ArgType -> #t_atom{elements=[true]};
            none -> #t_atom{elements=[false]};
            _ -> beam_types:make_boolean()
        end,
    sub_unsafe(RetType, [any]).

sub_unsafe(RetType, ArgTypes) ->
    {RetType, ArgTypes, false}.

sub_safe(RetType, ArgTypes) ->
    {RetType, ArgTypes, true}.

proper_cons() ->
    #t_cons{terminator=nil}.

proper_cons(ElementType) ->
    #t_cons{type=ElementType,terminator=nil}.

proper_list() ->
    #t_list{terminator=nil}.

proper_list(ElementType) ->
    #t_list{type=ElementType,terminator=nil}.

binary() ->
    #t_bitstring{size_unit=8}.

%% Constructs a new list type based on another, optionally keeping the same
%% length and/or making it proper.
-spec copy_list(List, Length, Proper) -> type() when
      List :: type(),
      Length :: same_length | new_length,
      Proper :: proper | maybe_improper.
copy_list(List0, Length, Proper) ->
    case {meet(List0, #t_list{}), Length, Proper} of
        {#t_cons{type=Type,terminator=Term}, new_length, maybe_improper} ->
            #t_list{type=Type,terminator=Term};
        {#t_cons{type=Type}, new_length, proper} ->
            #t_list{type=Type,terminator=nil};
        {#t_cons{}=T, _, proper} ->
            T#t_cons{terminator=nil};
        {#t_list{}=T, _, proper} ->
            T#t_list{terminator=nil};
        {none, _, _} ->
            none;
        {List, _, _} ->
            List
    end.

make_two_tuple(Type1, Type2) ->
    Es0 = beam_types:set_tuple_element(1, Type1, #{}),
    Es = beam_types:set_tuple_element(2, Type2, Es0),
    #t_tuple{size=2,exact=true,elements=Es}.

normalize(T) -> beam_types:normalize(T).
join(A, B) -> beam_types:join(A, B).
meet(A, B) -> beam_types:meet(A, B).
