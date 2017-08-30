%%% Copyright 2010-2015 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2015 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Manolis Papadakis

%%% @doc Generator subsystem and generators for basic types.
%%%
%%% You can use <a href="#index">these</a> functions to try out the random
%%% instance generation and shrinking subsystems.
%%%
%%% CAUTION: These functions should never be used inside properties. They are
%%% meant for demonstration purposes only.

-module(proper_gen).
-export([pick/1, pick/2, pick/3,
	 sample/1, sample/3, sampleshrink/1, sampleshrink/2]).

-export([safe_generate/1]).
-export([generate/1, normal_gen/1, alt_gens/1, clean_instance/1,
	 get_ret_type/1]).
-export([integer_gen/3, float_gen/3, atom_gen/1, atom_rev/1, binary_gen/1,
	 binary_rev/1, binary_len_gen/1, bitstring_gen/1, bitstring_rev/1,
	 bitstring_len_gen/1, list_gen/2, distlist_gen/3, vector_gen/2,
	 union_gen/1, weighted_union_gen/1, tuple_gen/1, loose_tuple_gen/2,
	 loose_tuple_rev/2, exactly_gen/1, fixed_list_gen/1, function_gen/2,
	 any_gen/1, native_type_gen/2, safe_weighted_union_gen/1,
	 safe_union_gen/1]).

-export_type([instance/0, imm_instance/0, sized_generator/0, nosize_generator/0,
	      generator/0, reverse_gen/0, combine_fun/0, alt_gens/0]).

-include("proper_internal.hrl").

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------

%% TODO: update imm_instance() when adding more types: be careful when reading
%%	 anything that returns it
%% @private_type
-type imm_instance() :: proper_types:raw_type()
		      | instance()
		      | {'$used', imm_instance(), imm_instance()}
		      | {'$to_part', imm_instance()}.
-type instance() :: term().
%% A value produced by the random instance generator.
-type error_reason() :: 'arity_limit' | 'cant_generate' | {'typeserver',term()}.

%% @private_type
-type sized_generator() :: fun((size()) -> imm_instance()).
%% @private_type
-type typed_sized_generator() :: {'typed',
                                  fun((proper_types:type(),size()) ->
                                      imm_instance())}.
%% @private_type
-type nosize_generator() :: fun(() -> imm_instance()).
%% @private_type
-type typed_nosize_generator() :: {'typed',
                                   fun((proper_types:type()) ->
                                       imm_instance())}.
%% @private_type
-type generator() :: sized_generator()
                   | typed_sized_generator()
                   | nosize_generator()
                   | typed_nosize_generator().
%% @private_type
-type plain_reverse_gen() :: fun((instance()) -> imm_instance()).
%% @private_type
-type typed_reverse_gen() :: {'typed',
                              fun((proper_types:type(),instance()) ->
                                  imm_instance())}.
%% @private_type
-type reverse_gen() :: plain_reverse_gen() | typed_reverse_gen().
%% @private_type
-type combine_fun() :: fun((instance()) -> imm_instance()).
%% @private_type
-type alt_gens() :: fun(() -> [imm_instance()]).
%% @private_type
-type fun_seed() :: {non_neg_integer(),non_neg_integer()}.


%%-----------------------------------------------------------------------------
%% Instance generation functions
%%-----------------------------------------------------------------------------

%% @private
-spec safe_generate(proper_types:raw_type()) ->
	  {'ok',imm_instance()} | {'error',error_reason()}.
safe_generate(RawType) ->
    try generate(RawType) of
	ImmInstance -> {ok, ImmInstance}
    catch
	throw:'$arity_limit'            -> {error, arity_limit};
	throw:'$cant_generate'          -> {error, cant_generate};
	throw:{'$typeserver',SubReason} -> {error, {typeserver,SubReason}}
    end.

%% @private
-spec generate(proper_types:raw_type()) -> imm_instance().
generate(RawType) ->
    Type = proper_types:cook_outer(RawType),
    ok = add_parameters(Type),
    Instance = generate(Type, get('$constraint_tries'), none),
    ok = remove_parameters(Type),
    Instance.

-spec add_parameters(proper_types:type()) -> 'ok'.
add_parameters(Type) ->
    case proper_types:find_prop(parameters, Type) of
	{ok, Params} ->
	    OldParams = erlang:get('$parameters'),
	    case OldParams of
		undefined ->
		    erlang:put('$parameters', Params);
		_ ->
		    erlang:put('$parameters', Params ++ OldParams)
	    end,
	    ok;
	_ ->
	    ok
    end.

-spec remove_parameters(proper_types:type()) -> 'ok'.
remove_parameters(Type) ->
    case proper_types:find_prop(parameters, Type) of
	{ok, Params} ->
	    AllParams = erlang:get('$parameters'),
	    case AllParams of
		Params->
		    erlang:erase('$parameters');
	        _ ->
		    erlang:put('$parameters', AllParams -- Params)
	    end,
	    ok;
	_ ->
	    ok
    end.

-spec generate(proper_types:type(), non_neg_integer(),
	       'none' | {'ok',imm_instance()}) -> imm_instance().
generate(_Type, 0, none) ->
    throw('$cant_generate');
generate(_Type, 0, {ok,Fallback}) ->
    Fallback;
generate(Type, TriesLeft, Fallback) ->
    ImmInstance =
	case proper_types:get_prop(kind, Type) of
	    constructed ->
		PartsType = proper_types:get_prop(parts_type, Type),
		Combine = proper_types:get_prop(combine, Type),
		ImmParts = generate(PartsType),
		Parts = clean_instance(ImmParts),
		ImmInstance1 = Combine(Parts),
		%% TODO: We can just generate the internal type: if it's not
		%%       a type, it will turn into an exactly.
		ImmInstance2 =
		    case proper_types:is_raw_type(ImmInstance1) of
			true  -> generate(ImmInstance1);
			false -> ImmInstance1
		    end,
		{'$used',ImmParts,ImmInstance2};
	    _ ->
		ImmInstance1 = normal_gen(Type),
		case proper_types:is_raw_type(ImmInstance1) of
		    true  -> generate(ImmInstance1);
		    false -> ImmInstance1
		end
	end,
    case proper_types:satisfies_all(clean_instance(ImmInstance), Type) of
	{_,true}      -> ImmInstance;
	{true,false}  -> generate(Type, TriesLeft - 1, {ok,ImmInstance});
	{false,false} -> generate(Type, TriesLeft - 1, Fallback)
    end.

%% @equiv pick(Type, 10)
-spec pick(Type::proper_types:raw_type()) -> {'ok',instance()} | 'error'.
pick(RawType) ->
    pick(RawType, 10).

%% @equiv pick(Type, Size, os:timestamp())
-spec pick(Type::proper_types:raw_type(), size()) -> {'ok',instance()} | 'error'.
pick(RawType, Size) ->
    pick(RawType, Size, os:timestamp()).

%% @doc Generates a random instance of `Type', of size `Size' with seed `Seed'.
-spec pick(Type::proper_types:raw_type(), size(), seed()) ->
	  {'ok',instance()} | 'error'.
pick(RawType, Size, Seed) ->
    proper:global_state_init_size_seed(Size, Seed),
    case clean_instance(safe_generate(RawType)) of
	{ok,Instance} = Result ->
	    Msg = "WARNING: Some garbage has been left in the process registry "
		  "and the code server~n"
		  "to allow for the returned function(s) to run normally.~n"
		  "Please run proper:global_state_erase() when done.~n",
	    case contains_fun(Instance) of
		true  -> io:format(Msg, []);
		false -> proper:global_state_erase()
	    end,
	    Result;
	{error,Reason} ->
	    proper:report_error(Reason, fun io:format/2),
	    proper:global_state_erase(),
	    error
    end.

%% @equiv sample(Type, 10, 20)
-spec sample(Type::proper_types:raw_type()) -> 'ok'.
sample(RawType) ->
    sample(RawType, 10, 20).

%% @doc Generates and prints one random instance of `Type' for each size from
%% `StartSize' up to `EndSize'.
-spec sample(Type::proper_types:raw_type(), size(), size()) -> 'ok'.
sample(RawType, StartSize, EndSize) when StartSize =< EndSize ->
    Tests = EndSize - StartSize + 1,
    Prop = ?FORALL(X, RawType, begin io:format("~p~n",[X]), true end),
    Opts = [quiet,{start_size,StartSize},{max_size,EndSize},{numtests,Tests}],
    _ = proper:quickcheck(Prop, Opts),
    ok.

%% @equiv sampleshrink(Type, 10)
-spec sampleshrink(Type::proper_types:raw_type()) -> 'ok'.
sampleshrink(RawType) ->
    sampleshrink(RawType, 10).

%% @doc Generates a random instance of `Type', of size `Size', then shrinks it
%% as far as it goes. The value produced on each step of the shrinking process
%% is printed on the screen.
-spec sampleshrink(Type::proper_types:raw_type(), size()) -> 'ok'.
sampleshrink(RawType, Size) ->
    proper:global_state_init_size(Size),
    Type = proper_types:cook_outer(RawType),
    case safe_generate(Type) of
	{ok,ImmInstance} ->
	    Shrunk = keep_shrinking(ImmInstance, [], Type),
	    PrintInst = fun(I) -> io:format("~p~n",[clean_instance(I)]) end,
	    lists:foreach(PrintInst, Shrunk);
	{error,Reason} ->
	    proper:report_error(Reason, fun io:format/2)
    end,
    proper:global_state_erase(),
    ok.

-spec keep_shrinking(imm_instance(), [imm_instance()], proper_types:type()) ->
	  [imm_instance(),...].
keep_shrinking(ImmInstance, Acc, Type) ->
    keep_shrinking(ImmInstance, Acc, Type, init).

keep_shrinking(ImmInstance, Acc, Type, State) ->
    case proper_shrink:shrink(ImmInstance, Type, State) of
	{[], done} -> %% no more shrinkers
	    lists:reverse([ImmInstance|Acc]);
	{[], NewState} ->
	    %% try next shrinker
	    keep_shrinking(ImmInstance, Acc, Type, NewState);
	{[Shrunk|_Rest], _NewState} ->
        Acc2 = [ImmInstance|Acc],
        case lists:member(Shrunk, Acc2) of
            true ->
                %% Avoid infinite loops
                lists:reverse(Acc2);
            false ->
                keep_shrinking(Shrunk, Acc2, Type)
        end
    end.

-spec contains_fun(term()) -> boolean().
contains_fun(List) when is_list(List) ->
    proper_arith:safe_any(fun contains_fun/1, List);
contains_fun(Tuple) when is_tuple(Tuple) ->
    contains_fun(tuple_to_list(Tuple));
contains_fun(Fun) when is_function(Fun) ->
    true;
contains_fun(_Term) ->
    false.


%%-----------------------------------------------------------------------------
%% Utility functions
%%-----------------------------------------------------------------------------

%% @private
-spec normal_gen(proper_types:type()) -> imm_instance().
normal_gen(Type) ->
    case proper_types:get_prop(generator, Type) of
        {typed, Gen} ->
            if
                is_function(Gen, 1) -> Gen(Type);
                is_function(Gen, 2) -> Gen(Type, proper:get_size(Type))
            end;
        Gen ->
            if
                is_function(Gen, 0) -> Gen();
                is_function(Gen, 1) -> Gen(proper:get_size(Type))
            end
    end.

%% @private
-spec alt_gens(proper_types:type()) -> [imm_instance()].
alt_gens(Type) ->
    case proper_types:find_prop(alt_gens, Type) of
	{ok, AltGens} -> ?FORCE(AltGens);
	error         -> []
    end.

%% @private
-spec clean_instance(imm_instance()) -> instance().
clean_instance({'$used',_ImmParts,ImmInstance}) ->
    clean_instance(ImmInstance);
clean_instance({'$to_part',ImmInstance}) ->
    clean_instance(ImmInstance);
clean_instance(ImmInstance) ->
    if
	is_list(ImmInstance) ->
	    %% CAUTION: this must handle improper lists
	    proper_arith:safe_map(fun clean_instance/1, ImmInstance);
	is_tuple(ImmInstance) ->
	    proper_arith:tuple_map(fun clean_instance/1, ImmInstance);
	true ->
	    ImmInstance
    end.


%%-----------------------------------------------------------------------------
%% Basic type generators
%%-----------------------------------------------------------------------------

%% @private
-spec integer_gen(size(), proper_types:extint(), proper_types:extint()) ->
	  integer().
integer_gen(Size, inf, inf) ->
    proper_arith:rand_int(Size);
integer_gen(Size, inf, High) ->
    High - proper_arith:rand_non_neg_int(Size);
integer_gen(Size, Low, inf) ->
    Low + proper_arith:rand_non_neg_int(Size);
integer_gen(Size, Low, High) ->
    proper_arith:smart_rand_int(Size, Low, High).

%% @private
-spec float_gen(size(), proper_types:extnum(), proper_types:extnum()) ->
	  float().
float_gen(Size, inf, inf) ->
    proper_arith:rand_float(Size);
float_gen(Size, inf, High) ->
    High - proper_arith:rand_non_neg_float(Size);
float_gen(Size, Low, inf) ->
    Low + proper_arith:rand_non_neg_float(Size);
float_gen(_Size, Low, High) ->
    proper_arith:rand_float(Low, High).

%% @private
-spec atom_gen(size()) -> proper_types:type().
%% We make sure we never clash with internal atoms by checking that the first
%% character is not '$'.
atom_gen(Size) ->
    ?LET(Str,
	 ?SUCHTHAT(X,
		   proper_types:resize(Size,
				       proper_types:list(proper_types:byte())),
		   X =:= [] orelse hd(X) =/= $$),
	 list_to_atom(Str)).

%% @private
-spec atom_rev(atom()) -> imm_instance().
atom_rev(Atom) ->
    {'$used', atom_to_list(Atom), Atom}.

%% @private
-spec binary_gen(size()) -> proper_types:type().
binary_gen(Size) ->
    ?LET(Bytes,
	 proper_types:resize(Size,
			     proper_types:list(proper_types:byte())),
	 list_to_binary(Bytes)).

%% @private
-spec binary_rev(binary()) -> imm_instance().
binary_rev(Binary) ->
    {'$used', binary_to_list(Binary), Binary}.

%% @private
-spec binary_len_gen(length()) -> proper_types:type().
binary_len_gen(Len) ->
    ?LET(Bytes,
	 proper_types:vector(Len, proper_types:byte()),
	 list_to_binary(Bytes)).

%% @private
-spec bitstring_gen(size()) -> proper_types:type().
bitstring_gen(Size) ->
    ?LET({BytesHead, NumBits, TailByte},
	 {proper_types:resize(Size,proper_types:binary()),
	  proper_types:range(0,7), proper_types:range(0,127)},
	 <<BytesHead/binary, TailByte:NumBits>>).

%% @private
-spec bitstring_rev(bitstring()) -> imm_instance().
bitstring_rev(BitString) ->
    List = bitstring_to_list(BitString),
    {BytesList, BitsTail} = lists:splitwith(fun erlang:is_integer/1, List),
    {NumBits, TailByte} = case BitsTail of
			      []     -> {0, 0};
			      [Bits] -> N = bit_size(Bits),
					<<Byte:N>> = Bits,
					{N, Byte}
			  end,
    {'$used',
     {{'$used',BytesList,list_to_binary(BytesList)}, NumBits, TailByte},
     BitString}.

%% @private
-spec bitstring_len_gen(length()) -> proper_types:type().
bitstring_len_gen(Len) ->
    BytesLen = Len div 8,
    BitsLen = Len rem 8,
    ?LET({BytesHead, NumBits, TailByte},
	 {proper_types:binary(BytesLen), BitsLen,
	  proper_types:range(0, 1 bsl BitsLen - 1)},
	  <<BytesHead/binary, TailByte:NumBits>>).

%% @private
-spec list_gen(size(), proper_types:type()) -> [imm_instance()].
list_gen(Size, ElemType) ->
    Len = proper_arith:rand_int(0, Size),
    vector_gen(Len, ElemType).

%% @private
-spec distlist_gen(size(), sized_generator(), boolean()) -> [imm_instance()].
distlist_gen(RawSize, Gen, NonEmpty) ->
    Len = case NonEmpty of
	      true  -> proper_arith:rand_int(1, erlang:max(1,RawSize));
	      false -> proper_arith:rand_int(0, RawSize)
	  end,
    Size = case Len of
	       1 -> RawSize - 1;
	       _ -> RawSize
	   end,
    %% TODO: this produces a lot of types: maybe a simple 'div' is sufficient?
    Sizes = proper_arith:distribute(Size, Len),
    InnerTypes = [Gen(S) || S <- Sizes],
    fixed_list_gen(InnerTypes).

%% @private
-spec vector_gen(length(), proper_types:type()) -> [imm_instance()].
vector_gen(Len, ElemType) ->
    vector_gen_tr(Len, ElemType, []).

-spec vector_gen_tr(length(), proper_types:type(), [imm_instance()]) ->
	  [imm_instance()].
vector_gen_tr(0, _ElemType, AccList) ->
    AccList;
vector_gen_tr(Left, ElemType, AccList) ->
    vector_gen_tr(Left - 1, ElemType, [generate(ElemType) | AccList]).

%% @private
-spec union_gen([proper_types:type(),...]) -> imm_instance().
union_gen(Choices) ->
    {_Choice,Type} = proper_arith:rand_choose(Choices),
    generate(Type).

%% @private
-spec weighted_union_gen([{frequency(),proper_types:type()},...]) ->
	  imm_instance().
weighted_union_gen(FreqChoices) ->
    {_Choice,Type} = proper_arith:freq_choose(FreqChoices),
    generate(Type).

%% @private
-spec safe_union_gen([proper_types:type(),...]) -> imm_instance().
safe_union_gen(Choices) ->
    {Choice,Type} = proper_arith:rand_choose(Choices),
    try generate(Type)
    catch
	error:_ ->
	    safe_union_gen(proper_arith:list_remove(Choice, Choices))
    end.

%% @private
-spec safe_weighted_union_gen([{frequency(),proper_types:type()},...]) ->
         imm_instance().
safe_weighted_union_gen(FreqChoices) ->
    {Choice,Type} = proper_arith:freq_choose(FreqChoices),
    try generate(Type)
    catch
	error:_ ->
	    safe_weighted_union_gen(proper_arith:list_remove(Choice,
							     FreqChoices))
    end.

%% @private
-spec tuple_gen([proper_types:type()]) -> tuple().
tuple_gen(Fields) ->
    list_to_tuple(fixed_list_gen(Fields)).

%% @private
-spec loose_tuple_gen(size(), proper_types:type()) -> proper_types:type().
loose_tuple_gen(Size, ElemType) ->
    ?LET(L,
	 proper_types:resize(Size, proper_types:list(ElemType)),
	 list_to_tuple(L)).

%% @private
-spec loose_tuple_rev(tuple(), proper_types:type()) -> imm_instance().
loose_tuple_rev(Tuple, ElemType) ->
    CleanList = tuple_to_list(Tuple),
    List = case proper_types:find_prop(reverse_gen, ElemType) of
                {ok,{typed, ReverseGen}} ->
                    [ReverseGen(ElemType,X) || X <- CleanList];
                {ok,ReverseGen} -> [ReverseGen(X) || X <- CleanList];
                error           -> CleanList
            end,
    {'$used', List, Tuple}.

%% @private
-spec exactly_gen(T) -> T.
exactly_gen(X) ->
    X.

%% @private
-spec fixed_list_gen([proper_types:type()]) -> imm_instance()
		  ; ({[proper_types:type()],proper_types:type()}) ->
	  maybe_improper_list(imm_instance(), imm_instance() | []).
fixed_list_gen({ProperHead,ImproperTail}) ->
    [generate(F) || F <- ProperHead] ++ generate(ImproperTail);
fixed_list_gen(ProperFields) ->
    [generate(F) || F <- ProperFields].

%% @private
-spec function_gen(arity(), proper_types:type()) -> function().
function_gen(Arity, RetType) ->
    FunSeed = {proper_arith:rand_int(0, ?SEED_RANGE - 1),
	       proper_arith:rand_int(0, ?SEED_RANGE - 1)},
    create_fun(Arity, RetType, FunSeed).

%% @private
-spec any_gen(size()) -> imm_instance().
any_gen(Size) ->
    case get('$any_type') of
	undefined      -> real_any_gen(Size);
	{type,AnyType} -> generate(proper_types:resize(Size, AnyType))
    end.

-spec real_any_gen(size()) -> imm_instance().
real_any_gen(0) ->
    SimpleTypes = [proper_types:integer(), proper_types:float(),
		   proper_types:atom()],
    union_gen(SimpleTypes);
real_any_gen(Size) ->
    FreqChoices = [{?ANY_SIMPLE_PROB,simple}, {?ANY_BINARY_PROB,binary},
		   {?ANY_EXPAND_PROB,expand}],
    case proper_arith:freq_choose(FreqChoices) of
	{_,simple} ->
	    real_any_gen(0);
	{_,binary} ->
	    generate(proper_types:resize(Size, proper_types:bitstring()));
	{_,expand} ->
	    %% TODO: statistics of produced terms?
	    NumElems = proper_arith:rand_int(0, Size - 1),
	    ElemSizes = proper_arith:distribute(Size - 1, NumElems),
	    ElemTypes = [?LAZY(real_any_gen(S)) || S <- ElemSizes],
	    case proper_arith:rand_int(1,2) of
		1 -> fixed_list_gen(ElemTypes);
		2 -> tuple_gen(ElemTypes)
	    end
    end.

%% @private
-spec native_type_gen(mod_name(), string()) -> proper_types:type().
native_type_gen(Mod, TypeStr) ->
    case proper_typeserver:translate_type({Mod,TypeStr}) of
	{ok,Type}      -> Type;
	{error,Reason} -> throw({'$typeserver',Reason})
    end.


%%------------------------------------------------------------------------------
%% Function-generation functions
%%------------------------------------------------------------------------------

-spec create_fun(arity(), proper_types:type(), fun_seed()) -> function().
create_fun(_Arity, _RetType, _FunSeed) ->
    fun() -> throw('$arity_limit') end.

%% @private
-spec get_ret_type(function()) -> proper_types:type().
get_ret_type(Fun) ->
    {arity,Arity} = erlang:fun_info(Fun, arity),
    put('$get_ret_type', true),
    RetType = apply(Fun, lists:duplicate(Arity,dummy)),
    erase('$get_ret_type'),
    RetType.
