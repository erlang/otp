%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(array_eqc).

%% This is a stateful eqc_statem model that only QuickCheck (EQC)
%% supports. When compiled for another property testing tool (PropEr,
%% Triq) via ct_property_test, the 'EQC' macro is undefined and the
%% module compiles to an empty stub so the test build does not fail.
%% The array_eqc_test case in array_SUITE skips unless the tool is eqc.
-ifdef('EQC').

-compile([export_all, nowarn_export_all]).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-define(MAXSIZE, 200).
-define(PUT(Ref, Val), begin put(Ref, Val), get(Ref) end).
-define(GET(Ref), get(Ref)).

%% State machine model
%% The state consists of a map of array reference => #array_model{}
%% We model an array as a map index => value
%% We don't try to generate arrays that respect 1 specific type
%% The arrays we construct are kept in the process dictionary and refered to symbolically.
 -record(array_model,
               {model = #{},
                size = 0,
                fixed = false,
                default = undefined
              }).

%% Initial state - we start with no array
initial_state() ->
    #{}.

command_precondition_common(S, new) ->
    maps:size(S) < 5;
command_precondition_common(S, _Cmd) ->
    maps:size(S) /= 0.

precondition_common(S, {_, _, concat, [Arrays, _]}) ->
    %% Only refer to existing arrays (for shrinking) and keep the sizes
    %% small, because undefault is expensive
    lists:all(fun({array, N}) -> maps:is_key({array, N}, S);
                  (_) -> true
              end, Arrays);
precondition_common(S, {_, _, Cmd, Args}) ->
    Cmd == new orelse
    lists:all(fun({array, N}) -> maps:is_key({array, N}, S);
                 (_) -> true
              end, Args).

postcondition_common(S, Cmd, Res) ->
    NS = next_state(S, Res, Cmd),
    ModToOrdDict = fun(Model) ->
                           #array_model{model=Map} = remove_default(prune(Model)),
                           lists:keysort(1, maps:to_list(Map))
                   end,
    conj([eq(array:sparse_to_orddict(?GET(Ref)), ModToOrdDict(Model))
          || Ref := Model <- NS]).

%% Commands

%% -- array:new/0, array:new/1 and array:new/2

new_args(S) ->
    ?LET({PossibleSize, Prefer0}, {array_size_opt(), bool()},
        [{array, maps:size(S)+1}, Prefer0, PossibleSize, array_opts()]).

new_pre(S, [ArrayRef, _, _, _]) ->
    not maps:is_key(ArrayRef, S).

new(ArrayRef, true, [], []) ->
    ?PUT(ArrayRef, array:new());
new(ArrayRef, _, [], Opts) ->
    ?PUT(ArrayRef, array:new(Opts));
new(ArrayRef, _, [Size], Opts) ->
    ?PUT(ArrayRef, array:new(Size, Opts)).

new_next(S, _, [ArrayRef, _, PossibleSize, Opts]) ->
    {Size, Fixed, Default} = sfd_from_opts(PossibleSize ++ Opts),
    S#{ArrayRef =>
        #array_model{model = #{},
                    size = Size,
                    fixed = Fixed,
                    default = Default}}.

%% Weak postcondition, only check it's an array.
%% Other commands will check the size, fixed, defaults etc, so if those are wrong, this will fail as well.
new_post(_S, [_, _, _, _], Result) ->
    eqc_statem:tag(is_array, array:is_array(Result)).

new_features(_S, [_, _, PossibleSize, Opts], _Res) ->
  {_Size, Fixed, _} = sfd_from_opts(PossibleSize ++ Opts),
  OptSizes = [ T || T <- Opts, is_integer(T) orelse (is_tuple(T) andalso element(1, T) == size)],
  [{new, {fixed, Fixed}}] ++
  [{new, no_size} || OptSizes == [], PossibleSize == []] ++
  [{new, {opt_sizes, if length(OptSizes) == 1 -> one; true -> several end}}
      || OptSizes /= [], PossibleSize == []] ++
  [{new, opt_sizes_and_arg_size} || OptSizes /= [], PossibleSize /= []] ++
  [{new, arg_size} || OptSizes == [], PossibleSize /= []].

%% -- array:size

size_args(S) ->
    [elements(maps:keys(S))].

size(Array) ->
    array:size(?GET(Array)).

size_post(S, [Array], Size) ->
    Model = maps:get(Array, S),
    eq(Size, Model#array_model.size).

%% -- array:sparse_size

sparse_size_args(S) ->
    [elements(maps:keys(S))].

sparse_size(Array) ->
    array:sparse_size(?GET(Array)).

sparse_size_post(S, [Array], Size) ->
    Model = maps:get(Array, S),
    case non_default_indices(Model) of
        [] -> eq(Size, 0);
        L -> eq(Size, lists:max(L) + 1)
    end.

%% -- array:resize

resize_args(S) ->
    [array_size_opt(), elements(maps:keys(S))].

resize([], Array) ->
    ?PUT(Array, array:resize(?GET(Array)));
resize([Size], Array) ->
    ?PUT(Array, array:resize(Size, ?GET(Array))).

resize_next(S, _, [SizeOpt, Array]) ->
    Model = maps:get(Array, S),
    Prune = erlang:system_info(otp_release) >= "29",
    OldSize = Model#array_model.size,
    NewSize =
        case {SizeOpt, non_default_indices(Model)} of
            {[], []} -> 0;
            {[], L} -> lists:max(L) + 1;
            {[Size], _} -> Size
        end,
    NewModel =
        Model#array_model{model = #{ K => V ||
                                      K := V <- Model#array_model.model,
                                      not Prune
                                          orelse
                                            (0 =< K andalso K < min(OldSize, NewSize))},
                          size = NewSize},
    S#{Array := NewModel}.

resize_features(_S, [SizeOpt, _Array], _Res) ->
    [{resize, {arity, length(SizeOpt) + 1}}].

%% -- array:is_fix

is_fix_args(S) ->
    [elements(maps:keys(S))].

is_fix(Array) ->
    array:is_fix(?GET(Array)).

is_fix_post(S, [Array], Fix) ->
    Model = maps:get(Array, S),
    eq(Fix, Model#array_model.fixed).

is_fix_features(_S, _Args, Res) ->
    [{is_fix, Res}].

%% -- array:default

default_args(S) ->
    [elements(maps:keys(S))].

default(Array) ->
    array:default(?GET(Array)).

default_post(S, [Array], Default) ->
    Model = maps:get(Array, S),
    eq(Default, Model#array_model.default).

%% -- array:get

get_args(S) ->
    ?LET(Array, elements(maps:keys(S)),
         [array_index(Array, S), Array]).

get(I, Array) ->
    try array:get(I, ?GET(Array))
    catch
        error:badarg -> badarg
    end.

get_post(S, [I, Array], Res) ->
    Model = maps:get(Array, S),
    #array_model{size = Size, default = Default} = Model,
    case Res of
        badarg -> not index_valid(Model, I);
        Default when I >= Size ->
            eqc_statem:tag(invalid_index, index_valid(Model, I));
        _ ->
            conj([eqc_statem:tag(invalid_index, index_valid(Model, I)),
                  eq(Res, maps:get(I, Model#array_model.model, Model#array_model.default))])
    end.

get_features(S, [_, Array], Res) ->
        Model = maps:get(Array, S),
    [{get, badarg} || Res == badarg] ++
        [{get, {default, Res == Model#array_model.default}} || Res /= badarg].

%% -- array:set

set_args(S) ->
  ?LET({Array, V}, {elements(maps:keys(S)), any()},
       [array_index(Array, S), V, Array]).

set(I, V, Array) ->
    try ?PUT(Array, array:set(I, V, ?GET(Array)))
    catch
        error:badarg -> badarg
    end.

set_next(S, _, [I, V, Array]) ->
    Model = maps:get(Array, S),
    case index_valid(Model, I) of
        false -> S;
        true ->
          %% For fixed arrays I is in size bound, hence max does not change size
          NewModel = Model#array_model{model = maps:put(I, V, Model#array_model.model),
                                       size = max(Model#array_model.size, I+1)},
          S#{Array := NewModel}
    end.

set_post(S, [I, _, Array], Res) ->
   Model = maps:get(Array, S),
   case Res of
    badarg -> not index_valid(Model, I);
    _ ->
      eqc_statem:tag(invalid_index, index_valid(Model, I))
  end.

set_features(S, [_, _, Array], Res) ->
    Model = maps:get(Array, S),
    [{set, badarg} || Res == badarg] ++
      [{set, {default, Res == Model#array_model.default}} || Res /= badarg].

%% -- array:reset

reset_args(S) ->
  ?LET(Array, elements(maps:keys(S)),
       [array_index(Array, S), Array]).

reset(I, Array) ->
    try ?PUT(Array, array:reset(I, ?GET(Array)))
    catch
        error:badarg -> badarg
    end.

reset_next(S, _, [I, Array]) ->
    Model = maps:get(Array, S),
    Default = Model#array_model.default,
    case I >= 0 andalso I < Model#array_model.size
        andalso maps:get(I, Model#array_model.model, Default) =/= Default of
        false -> S;
        true ->
          %% Reset never changes the array size
          NewModel = Model#array_model{model = maps:put(I, Default, Model#array_model.model)},
          S#{Array := NewModel}
    end.

reset_post(S, [I, Array], Res) ->
   Model = maps:get(Array, S),
   case Res of
    badarg -> not index_valid(Model, I);
    _ ->
      eqc_statem:tag(invalid_index, index_valid(Model, I))
  end.

reset_features(S, [I, Array], Res) ->
    Model = maps:get(Array, S),
    [{reset, badarg} || Res == badarg] ++
      [{reset, {default, maps:get(I, Model#array_model.model, Model#array_model.default) == Model#array_model.default}} || Res /= badarg].

%% -- array:fix

fix_args(S) ->
    [elements(maps:keys(S))].

fix(Array) ->
    ?PUT(Array, array:fix(?GET(Array))).

fix_next(S, _, [Array]) ->
    Model = maps:get(Array, S),
    S#{Array := Model#array_model{fixed = true}}.

fix_features(S, [Array], _Res) ->
    Model = maps:get(Array, S),
    %% Did we fix an already fixed array?
    [{fix, {fixed, Model#array_model.fixed}}].

%% -- array:relax
relax_args(S) ->
    [elements(maps:keys(S))].

relax(Array) ->
    ?PUT(Array, array:relax(?GET(Array))).

relax_next(S, _, [Array]) ->
    Model = maps:get(Array, S),
    S#{Array := Model#array_model{fixed = false}}.

relax_features(S, [Array], _Res) ->
    Model = maps:get(Array, S),
    %% Did we relax an already relaxed array?
    [{relax, {fixed, Model#array_model.fixed}}].

%% -- array:append

append_pre(_) ->
     erlang:system_info(otp_release) >= "29".

append_args(S) ->
  ?LET(V, any(),
       [V, elements(maps:keys(S))]).

append(V, Array) ->
    ?PUT(Array, array:append(V, ?GET(Array))).

append_next(S, _, [V, Array]) ->
    Model = maps:get(Array, S),
    NewModel = Model#array_model{model = maps:put(Model#array_model.size, V, Model#array_model.model),
                                 size = Model#array_model.size+1},
    S#{Array := NewModel}.

append_features(S, [_, Array], _Res) ->
    Model = maps:get(Array, S),
    [{append, {fixed, Model#array_model.fixed}}].

%% -- array:prepend

prepend_pre(_) ->
     erlang:system_info(otp_release) >= "29".

prepend_args(S) ->
  ?LET(V, any(),
       [V, elements(maps:keys(S))]).

prepend(V, Array) ->
    ?PUT(Array, array:prepend(V, ?GET(Array))).

prepend_next(S, _, [V, Array]) ->
    Model = maps:get(Array, S),
    NewModel = Model#array_model{model = maps:put(0, V, shift_right(1, Model#array_model.model)),
                                 size = Model#array_model.size+1},
    S#{Array := NewModel}.

prepend_features(S, [_, Array], _Res) ->
    Model = maps:get(Array, S),
    [{prepend, {fixed, Model#array_model.fixed}}].

%% -- array:shift
%% Positive shifts to the left, negative to the right

shift_pre(_) ->
     erlang:system_info(otp_release) >= "29".

shift_args(S) ->
  ?LET(V, int(),
       [V, elements(maps:keys(S))]).

shift(V, Array) ->
    try ?PUT(Array, array:shift(V, ?GET(Array)))
    catch _:badarg -> badarg end.

shift_next(S, _, [V, Array]) ->
    Model = maps:get(Array, S),
    NewSize = Model#array_model.size - V,
    case NewSize < 0 orelse V =:= 0 of
        true -> S;
        false ->
            case V > 0 of
                true ->
                    NewModel = Model#array_model{model = shift_left(V, Model#array_model.model),
                                                 size = NewSize},
                    S#{Array := NewModel};
                false ->
                    %% -V is positive!
                    NewModel = Model#array_model{model = shift_right(-V, Model#array_model.model),
                                                 size = NewSize},
                    S#{Array := NewModel}
            end
    end.

shift_post(S, [V, Array], Res) ->
    Model = maps:get(Array, S),
    Res /= badarg orelse V > Model#array_model.size.

shift_features(S, [V, Array], _Res) ->
    Model = maps:get(Array, S),
    [{shift, {if V < 0 -> left; true -> right end, fixed, Model#array_model.fixed}}].


%% array:concat
%% the first array determines the default and also fixed/non-fixed
%% We must assume that the sparse length is computed from the orginal defaults in the array.
%% Hence the undefault function that fills in the default values up to the (sparse) size of the array.
concat_pre(S) ->
     length(maps:keys(S)) > 1 andalso erlang:system_info(otp_release) >= "29".

concat_args(S) ->
    ?LET(NumArrays, choose(2, 4),  %% don't concat too many arrays, as we don't want to create huge maps in the model
    ?LET(Arrays, vector(NumArrays, elements(maps:keys(S))),
        [Arrays, if NumArrays == 2 -> bool(); true -> false end])).

concat_pre(S, [Arrays, _]) ->
   %% Avoid large arrays (for sake of undefault) and
   %% do not concat if there are elements to be pruned
   %% The user is supposed to resize before concatenation
    lists:all(fun({array, N}) ->
                  Model = maps:get({array, N}, S),
                  Model#array_model.size < 5000 andalso
                     lists:min([0|maps:keys(Model#array_model.model)]) >= 0
              end, Arrays).

%% Arbitary choice to perform destructive update of first array
concat([A1, A2], true) ->
    ?PUT(A1, array:concat(?GET(A1), ?GET(A2)));
concat([A1 | _] = Arrays, _) ->
    ?PUT(A1, array:concat([ ?GET(A) || A <- Arrays])).

concat_next(S, _, [Arrays, _]) ->
    [Model | Models] = [maps:get(A, S) || A <- Arrays],
    %% Shift the next array to the end of first
    %% The default of first array is default of total array
    NewModel =
        lists:foldl(fun(M, Acc) ->
                       MUD = undefault(M),
                       Acc#array_model{model = maps:merge(Acc#array_model.model,
                                                          shift_right(Acc#array_model.size,MUD#array_model.model)),
                                       size = Acc#array_model.size + M#array_model.size
                                    }
                    end, Model, Models),
    S#{hd(Arrays) := NewModel}.

slice_pre(_) ->
    erlang:system_info(otp_release) >= "29".

slice_args(S) ->
    ?LET({Start, Len, A}, {int(), int(), elements(maps:keys(S))},
         [Start, Len, A]).

slice(Start, Len, A) ->
    try
        ?PUT(A, array:slice(Start, Len, ?GET(A)))
    catch
        error:badarg -> badarg
    end.

slice_next(S, _, [Start, Len, Array]) ->
    Model = maps:get(Array, S),
    case Start < 0 orelse Len < 0 orelse (Start+Len) > Model#array_model.size of
        true ->
            S;
        false ->
            Shifted = shift_left(Start, Model#array_model.model),
            NewModel = Model#array_model{model = Shifted, size = Len},
            S#{Array := NewModel}
    end.

slice_post(S, [Start, Len, Array], Res) ->
    Model = maps:get(Array, S),
    InValidArgs = Start < 0 orelse Len < 0 orelse (Start+Len) > Model#array_model.size,
    case Res of
        badarg ->
            InValidArgs;
        _ ->
            eqc_statem:tag(invalid_args, not InValidArgs)
    end.

%% array:to_list
to_list_args(S) ->
    [elements(maps:keys(S))].

to_list(Array) ->
    array:to_list(?GET(Array)).

to_list_post(S, [Array], Res) ->
    #array_model{default = Def, size = Sz, model = Model} = maps:get(Array, S),
    eq(Res, [maps:get(Indx, Model, Def) || Indx <- lists:seq(0, Sz-1)]).

to_list_features(_S, [_Array], Res)  ->
    [{to_list, {empty, Res == []}}].

%% Command distribution

weight(_S, get) ->
    60;
weight(_S, set) ->
    120;
weight(S, resize) ->
    Models = maps:values(S),
    case [ 1 || #array_model{model = M} <- Models, lists:min([0|maps:keys(M)]) < 0 ] of
      [] -> 5;
      _ -> 40  %% some negative keys means pruning before some operations are ok again
    end;
weight(_S, Cmd) when Cmd == fix; Cmd == relax ->
    2;
weight(_S, _) ->
    5.

%% Generators

any() ->
    oneof([int(), bool(), real(), elements([a,b,c,d]), list(int())]).

array_opts() ->
  list(oneof(
    [fixed, {fixed, bool()}, {default, any()},
     choose(0, ?MAXSIZE), {size, choose(0, ?MAXSIZE)}])).

%% Generates a list with zero or one integer in it.
%% Used to optionally include a size argument in the new/2 command.
array_size_opt() ->
  ?LET(WithSizeArg, bool(), [choose(0, ?MAXSIZE) || WithSizeArg]).

array_index(Array, S) ->
    Model = maps:get(Array, S),
    frequency([{1, choose(-2, ?MAXSIZE+10)},
               {3, choose(0, Model#array_model.size)} ] ++
              [{16, elements(maps:keys(Model#array_model.model))}
                || maps:size(Model#array_model.model) > 0 ]
              ).

%% Property

%% Run N tests (used by array_SUITE to run more than the default 100).
prop_array(N) ->
    eqc:numtests(N, prop_array()).

prop_array() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
          {H, S, Res} = run_commands(Cmds),
          measure(array_size, lists:max([0] ++ [ Model#array_model.size || _ := Model <- S ]),
          aggregate_feats(call_features(H),
            features(call_features(H),
            check_command_names(Cmds,
            pretty_commands(?MODULE, Cmds, {H, S, Res}, Res == ok)
            ))))
        end).


%% Utility functions
aggregate_feats(Features, Prop) ->
  aggregate_feats(
    lists:sort(all_command_names() ++ []),
    Features,
    Prop
  ).

aggregate_feats([], Features, Prop) ->
  features(Features, Prop);
aggregate_feats([OpId | OpIds], Features, Prop) ->
  aggregate(
    with_title(OpId),
    [F || {Id, F} <- Features, Id == OpId],
    aggregate_feats(OpIds, Features, Prop)
  ).


arrays_gen() ->
    ?LET(Cmds, commands(?MODULE),
        begin
             {_, S, _} = run_commands(?MODULE, Cmds),
             [ ?GET(A) || A <- maps:keys(S)]
        end).

%% From the documentation:
%% Options are processed in the order they occur in the list, that is, later options have higher precedence.
%% We assume here that as soon as a size is given, the array is fixed size
sfd_from_opts(Opts) ->
    lists:foldl(fun({size, Val}, {_, _, D}) -> {Val, true, D};
                   (Val, {_, _, D}) when is_integer(Val) -> {Val, true, D};
                   (fixed, {V, _, D}) -> {V, true, D};
                   ({fixed, B}, {V, _, D}) -> {V, B, D};
                   ({default, D}, {V, F, _}) -> {V, F, D};
                   (_, Acc) -> Acc
                end, {0, false, undefined}, Opts).

index_valid(Model, I) ->
  case Model#array_model.fixed of
    true -> I >= 0 andalso I < Model#array_model.size;
    false -> I >= 0
  end.

non_default_indices(Model) ->
    OldSize = Model#array_model.size,
    [ I || I := V <- Model#array_model.model,
           I >= 0, %% For left shifted arrays
           I < OldSize,
           V =/= Model#array_model.default ].

shift_right(Shift, Map) ->
   #{ K+Shift => V || K := V <- Map }.

shift_left(Shift, Map) ->
   #{ K-Shift => V || K := V <- Map}.

undefault(Model) ->
    Size =  Model#array_model.size,
    Model#array_model{model = #{ K => maps:get(K, Model#array_model.model, Model#array_model.default)
                                 || K <- lists:seq(0, Size-1)}}.

remove_default(#array_model{default = Default} = Model) ->
    Model#array_model{model = #{ K => V || K := V <- Model#array_model.model, V =/= Default}}.


prune(Model) ->
    Size = Model#array_model.size,
    Model#array_model{model = #{ K => V || K := V <- Model#array_model.model,
                                           0 =< K andalso K < Size }}.

-endif. %% 'EQC'

