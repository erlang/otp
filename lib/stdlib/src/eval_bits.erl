%% -*- erlang-indent-level: 4 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1999-2025. All Rights Reserved.
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
-module(eval_bits).
-moduledoc false.

-compile(nowarn_deprecated_catch).

%% Avoid warning for local function error/1 clashing with autoimported BIF.
-compile({no_auto_import,[error/1]}).
-export([expr_grp/3,expr_grp/4,match_bits/6,
	 match_bits/7,bin_gen/6,bin_gen/7]).

-define(STACKTRACE,
        element(2, erlang:process_info(self(), current_stacktrace))).

%% Types used in this module:
%% @type bindings(). An abstract structure for bindings between
%% variables and values (the environment)
%%
%% @type evalfun(). A closure which evaluates an expression given an
%% environment
%%
%% @type matchfun(). A closure which depending on its first argument
%% can perform a match (given a value, a pattern and an environment),
%% lookup a variable in the bindings, or add a new binding
%%
%% @type field(). Represents a field in a "bin".
%%
%% @type errorfun(). A closure invoked in case of errors.

%%% Part 1: expression evaluation (binary construction)

%% @spec expr_grp(Fields::[field()], Bindings::bindings(), 
%%                EvalFun::evalfun(), ErrorFun::errorfun()) ->
%%                  {value, binary(), bindings()}
%%
%% @doc Returns a tuple with {value,Bin,Bs} where Bin is the binary
%% constructed from form the Fields under the current Bindings. Bs
%% contains the present bindings. This function can also throw an
%% exception if the construction fails.

expr_grp(Fields, Bindings, EvalFun, ErrorFun) ->
    expr_grp(Fields, Bindings, EvalFun, ErrorFun, <<>>).

expr_grp(Fields, Bindings, EvalFun) ->
    expr_grp(Fields, Bindings, EvalFun, fun default_error/3, <<>>).

expr_grp(FS, Bs0, EvalFun, ErrorFun, Acc) ->
    %% Separate the evaluation of values, sizes, and TLS:s from the
    %% creation of the binary in order to mimic compiled code when it
    %% comes to loops and failures.
    {ListOfEvalField,Bs1} = expr_grp1(FS, Bs0, EvalFun, ErrorFun, [], 1),
    {value,create_binary(ListOfEvalField, Acc),Bs1}.

expr_grp1([Field | FS], Bs0, EvalFun, ErrorFun, ListOfEvalField, Pos) ->
    {EvalField,Bs} = eval_field(Field, Bs0, EvalFun, ErrorFun, Pos),
    expr_grp1(FS, Bs, EvalFun, ErrorFun, [EvalField|ListOfEvalField], Pos + 1);
expr_grp1([], Bs, _EvalFun, _ErrorFun, ListOfFieldData, _Pos) ->
    {lists:reverse(ListOfFieldData),Bs}.

create_binary([EvalField|ListOfEvalField], Acc) ->
    Bin = EvalField(),
    create_binary(ListOfEvalField, <<Acc/binary-unit:1,Bin/binary-unit:1>>);
create_binary([], Acc) ->
    Acc.

eval_field({bin_element, _, {string, _, S}, {integer,_,8}, [integer,{unit,1},unsigned,big]}, Bs0, _Fun, _ErrorFun, _Pos) ->
    Latin1 = [C band 16#FF || C <- S],
    {fun() -> list_to_binary(Latin1) end,Bs0};
eval_field({bin_element, _, {string, _, S}, default, default}, Bs0, _Fun, _ErrorFun, _Pos) ->
    Latin1 = [C band 16#FF || C <- S],
    {fun() ->list_to_binary(Latin1) end,Bs0};
eval_field({bin_element, Anno, {string, _, S}, Size0, Options0}, Bs0, Fun, ErrorFun, Pos) ->
    {Size1,[Type,{unit,Unit},Sign,Endian]} =
        make_bit_type(Anno, Size0, Options0, ErrorFun),
    {value,Size,Bs1} = Fun(Size1, Bs0),
    {fun() ->
             Res = << <<(eval_exp_field1(Anno, C, Size, Unit,
                                         Type, Endian, Sign, ErrorFun, Pos))/bitstring>> ||
                       C <- S >>,
             case S of
                 "" -> % find errors also when the string is empty
                     _ = eval_exp_field1(Anno, 0, Size, Unit, Type, Endian, Sign, ErrorFun, Pos),
                     ok;
                 _ ->
                     ok
             end,
             Res
     end,Bs1};
eval_field({bin_element,Anno,E,Size0,Options0}, Bs0, Fun, ErrorFun, Pos) ->
    {value,V,Bs1} = Fun(E, Bs0),
    {Size1,[Type,{unit,Unit},Sign,Endian]} = 
        make_bit_type(Anno, Size0, Options0, ErrorFun),
    {value,Size,Bs} = Fun(Size1, Bs1),
    {fun() -> eval_exp_field1(Anno, V, Size, Unit, Type, Endian, Sign, ErrorFun, Pos) end,Bs}.

eval_exp_field1(Anno, V, Size, Unit, Type, Endian, Sign, ErrorFun, Pos) ->
    try
        eval_exp_field(V, Size, Unit, Type, Endian, Sign)
    catch
        error:system_limit:Stacktrace ->
            ErrorFun(Anno, system_limit, add_eval_pos_to_error_info(Stacktrace, Pos));
        error:_:Stacktrace ->
            ErrorFun(Anno, badarg, add_eval_pos_to_error_info(Stacktrace, Pos))
    end.

add_eval_pos_to_error_info([{Mod, Fun, Arity, Meta0} = Head, _Skip | Rest], Pos) ->
    case lists:keytake(error_info, 1, Meta0) of
        {value, {error_info, #{} = Info}, Meta1} ->
            Meta2 = Meta1 ++ [{error_info, Info#{override_segment_position => Pos}}],
            [{Mod, Fun, Arity, Meta2} | Rest];

        _ ->
            [Head | Rest]
    end.

eval_exp_field(Val, Size, Unit, integer, little, signed) ->
    <<Val:(Size*Unit)/little-signed>>;
eval_exp_field(Val, Size, Unit, integer, little, unsigned) ->
    <<Val:(Size*Unit)/little>>;
eval_exp_field(Val, Size, Unit, integer, native, signed) ->
    <<Val:(Size*Unit)/native-signed>>;
eval_exp_field(Val, Size, Unit, integer, native, unsigned) ->
    <<Val:(Size*Unit)/native>>;
eval_exp_field(Val, Size, Unit, integer, big, signed) ->
    <<Val:(Size*Unit)/signed>>;
eval_exp_field(Val, Size, Unit, integer, big, unsigned) ->
    <<Val:(Size*Unit)>>;
eval_exp_field(Val, _Size, _Unit, utf8, _, _) ->
    <<Val/utf8>>;
eval_exp_field(Val, _Size, _Unit, utf16, big, _) ->
    <<Val/big-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf16, little, _) ->
    <<Val/little-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf16, native, _) ->
    <<Val/native-utf16>>;
eval_exp_field(Val, _Size, _Unit, utf32, big, _) ->
    <<Val/big-utf32>>;
eval_exp_field(Val, _Size, _Unit, utf32, little, _) ->
    <<Val/little-utf32>>;
eval_exp_field(Val, _Size, _Unit, utf32, native, _) ->
    <<Val/native-utf32>>;
eval_exp_field(Val, Size, Unit, float, little, _) ->
    <<Val:(Size*Unit)/float-little>>;
eval_exp_field(Val, Size, Unit, float, native, _) ->
    <<Val:(Size*Unit)/float-native>>;
eval_exp_field(Val, Size, Unit, float, big, _) ->
    <<Val:(Size*Unit)/float>>;
eval_exp_field(Val, all, Unit, binary, _, _) ->
    case bit_size(Val) of
	Size when Size rem Unit =:= 0 ->
	    <<Val:Size/binary-unit:1>>;
	_ ->
	    erlang:raise(error, badarg, ?STACKTRACE)
    end;
eval_exp_field(Val, Size, Unit, binary, _, _) ->
    <<Val:(Size*Unit)/binary-unit:1>>.


%%% Part 2: matching in binary comprehensions
%% @spec bin_gen(BinPattern::{bin,integer(),[field()]}, Bin::binary(),
%%               GlobalEnv::bindings(), LocalEnv::bindings(),  
%%               MatchFun::matchfun(), EvalFun::evalfun(),
%%               ErrorFun::errorfun()) ->
%%                 {match, binary(), bindings()} | {nomatch, binary()} | done
%%
%% @doc Used to perform matching in a comprehension. If the match
%% succeeds a new environment and what remains of the binary is
%% returned. If the match fails what remains of the binary is returned.
%% If nothing remains of the binary the atom 'done' is returned.

bin_gen({bin,_,Fs}, Bin, Bs0, BBs0, Mfun, Efun) ->
    bin_gen(Fs, Bin, Bs0, BBs0, Mfun, Efun, fun default_error/3, true).

bin_gen({bin,_,Fs}, Bin, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    bin_gen(Fs, Bin, Bs0, BBs0, Mfun, Efun, ErrorFun, true).

bin_gen([F|Fs], Bin, Bs0, BBs0, Mfun, Efun, ErrorFun, Flag)
  when is_function(Mfun, 2), is_function(Efun, 2) ->
    case bin_gen_field(F, Bin, Bs0, BBs0, Mfun, Efun, ErrorFun) of
        {match,Bs,BBs,Rest} ->
            bin_gen(Fs, Rest, Bs, BBs, Mfun, Efun, ErrorFun, Flag);
        {nomatch,Rest} ->
            bin_gen(Fs, Rest, Bs0, BBs0, Mfun, Efun, ErrorFun, false);
        done ->
            done
    end;
bin_gen([], Bin, Bs0, _BBs0, _Mfun, _Efun, _ErrorFun, true) ->
    {match, Bin, Bs0};
bin_gen([], Bin, _Bs0, _BBs0, _Mfun, _Efun, _ErrorFun, false) ->
    {nomatch, Bin}.
  
bin_gen_field({bin_element,_,{string,_,S},default,default},
              Bin, Bs, BBs, _Mfun, _Efun, _ErrorFun) ->
    Bits = try list_to_binary(S)
           catch _:_ -> <<>>
           end,
    Size = length(S),
    case Bin of
        <<Bits:Size/binary,Rest/bitstring>> ->
            {match,Bs,BBs,Rest};
        <<_:Size/binary,Rest/bitstring>> ->
            {nomatch,Rest};
        _ ->
            done
    end;
bin_gen_field({bin_element,Anno,{string,SAnno,S},Size0,Options0},
              Bin0, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    {Size1, [Type,{unit,Unit},Sign,Endian]} =
        make_bit_type(Anno, Size0, Options0, ErrorFun),
    case catch Efun(Size1, BBs0) of
        {value, Size, _BBs} -> % 
            F = fun(C, Bin, Bs, BBs) ->
                        bin_gen_field1(Bin, Type, Size, Unit, Sign, Endian,
                                       {integer,SAnno,C}, Bs, BBs, Mfun)
                end,
            bin_gen_field_string(S, Bin0, Bs0, BBs0, F)
    end;
bin_gen_field({bin_element,Anno,VE,Size0,Options0},
              Bin, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    {Size1, [Type,{unit,Unit},Sign,Endian]} = 
        make_bit_type(Anno, Size0, Options0, ErrorFun),
    V = erl_eval:partial_eval(VE),
    NewV = coerce_to_float(V, Type),
    case catch Efun(Size1, BBs0) of
        {value, Size, _BBs} ->
            bin_gen_field1(Bin, Type, Size, Unit, Sign, Endian,
                           NewV, Bs0, BBs0, Mfun);
        _ ->
            done
    end.

bin_gen_field_string([], Rest, Bs, BBs, _F) ->
    {match,Bs,BBs,Rest};
bin_gen_field_string([C|Cs], Bin0, Bs0, BBs0, Fun) ->
    case Fun(C, Bin0, Bs0, BBs0) of
        {match,Bs,BBs,Rest} ->
            bin_gen_field_string(Cs, Rest, Bs, BBs, Fun);
        {nomatch,Rest} ->
            {nomatch,Rest};
        done ->
            done
    end.

bin_gen_field1(Bin, float, Size, Unit, Sign, Endian, NewV, Bs0, BBs0, Mfun) ->
    case catch get_value(Bin, float, Size, Unit, Sign, Endian) of
        {Val,<<_/bitstring>>=Rest} ->
            case catch Mfun(match, {NewV,Val,Bs0}) of
                {match,Bs} ->
                    BBs = add_bin_binding(Mfun, NewV, Bs, BBs0),
                    {match,Bs,BBs,Rest};
                _ ->
                    {nomatch,Rest}
            end;
        _ ->
            case catch get_value(Bin, integer, Size, Unit, Sign, Endian) of
                {_,<<_/bitstring>>=Rest} ->
                    {nomatch,Rest};
                _ -> done
            end
    end;
bin_gen_field1(Bin, Type, Size, Unit, Sign, Endian, NewV, Bs0, BBs0, Mfun) ->
    case catch get_value(Bin, Type, Size, Unit, Sign, Endian) of
        {Val,<<_/bitstring>>=Rest} ->
            case catch Mfun(match, {NewV,Val,Bs0}) of
                {match,Bs} ->
                    BBs = add_bin_binding(Mfun, NewV, Bs, BBs0),
                    {match,Bs,BBs,Rest};
                _ ->
                    {nomatch,Rest}
            end;
        _ ->
            done
    end.

%%% Part 3: binary pattern matching 
%% @spec match_bits(Fields::[field()], Bin::binary(),
%%                  GlobalEnv::bindings(), LocalEnv::bindings(),  
%%                  MatchFun::matchfun(),EvalFun::evalfun(),
%%                  ErrorFun::errorfun()) ->
%%                  {match, bindings()}
%% @doc Used to perform matching. If the match succeeds a new
%% environment is returned. If the match have some syntactic or
%% semantic problem which would have been caught at compile time this
%% function throws 'invalid', if the matching fails for other reasons
%% the function throws 'nomatch'

match_bits(Fs, Bin, Bs0, BBs, Mfun, Efun) ->
    match_bits(Fs, Bin, Bs0, BBs, Mfun, Efun, fun default_error/3).

match_bits(Fs, Bin, Bs0, BBs, Mfun, Efun, ErrorFun)
  when is_function(Mfun, 2), is_function(Efun, 2), is_function(ErrorFun, 3) ->
    case catch match_bits_1(Fs, Bin, Bs0, BBs, Mfun, Efun, ErrorFun) of
        {match,Bs} -> {match,Bs};
        invalid -> throw(invalid);
        _Error -> throw(nomatch)
    end.

match_bits_1([], <<>>,  Bs, _BBs, _Mfun, _Efun, _ErrorFun) ->
    {match,Bs};
match_bits_1([F|Fs], Bits0, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    {Bs,BBs,Bits} = match_field_1(F, Bits0, Bs0, BBs0, Mfun, Efun, ErrorFun),
    match_bits_1(Fs, Bits, Bs, BBs, Mfun, Efun, ErrorFun).

match_field_1({bin_element,_,{string,_,S},default,default},
              Bin, Bs, BBs, _Mfun, _Efun, _ErrorFun) ->
    Bits = list_to_binary(S), % fails if there are characters > 255
    Size = byte_size(Bits),
    <<Bits:Size/binary,Rest/binary-unit:1>> = Bin,
    {Bs,BBs,Rest};
match_field_1({bin_element,Anno,{string,SAnno,S},Size0,Options0},
              Bin0, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    {Size1, [Type,{unit,Unit},Sign,Endian]} =
        make_bit_type(Anno, Size0, Options0, ErrorFun),
    Size2 = erl_eval:partial_eval(Size1),
    {value, Size, _BBs} = Efun(Size2, BBs0),
    F = fun(C, Bin, Bs, BBs) ->
                match_field(Bin, Type, Size, Unit, Sign, Endian,
                            {integer,SAnno,C}, Bs, BBs, Mfun)
        end,
    match_field_string(S, Bin0, Bs0, BBs0, F);
match_field_1({bin_element,Anno,VE,Size0,Options0},
              Bin, Bs0, BBs0, Mfun, Efun, ErrorFun) ->
    {Size1, [Type,{unit,Unit},Sign,Endian]} = 
        make_bit_type(Anno, Size0, Options0, ErrorFun),
    V = erl_eval:partial_eval(VE),
    NewV = coerce_to_float(V, Type),
    Size2 = erl_eval:partial_eval(Size1),
    {value, Size, _BBs} = Efun(Size2, BBs0),
    match_field(Bin, Type, Size, Unit, Sign, Endian, NewV, Bs0, BBs0, Mfun).

match_field_string([], Rest, Bs, BBs, _Fun) ->
    {Bs,BBs,Rest};
match_field_string([C|Cs], Bin0, Bs0, BBs0, Fun) ->
    {Bs,BBs,Bin} = Fun(C, Bin0, Bs0, BBs0),
    match_field_string(Cs, Bin, Bs, BBs, Fun).

match_field(Bin, Type, Size, Unit, Sign, Endian, NewV, Bs0, BBs0, Mfun) ->
    {Val,Rest} = get_value(Bin, Type, Size, Unit, Sign, Endian),
    {match,Bs} = Mfun(match, {NewV,Val,Bs0}),
    BBs = add_bin_binding(Mfun, NewV, Bs, BBs0),
    {Bs,BBs,Rest}.

%% Almost identical to the one in sys_pre_expand.
coerce_to_float({integer,Anno,I}=E, float) ->
    try
	{float,Anno,float(I)}
    catch
	error:badarg -> E;
	error:badarith -> E
    end;
coerce_to_float(E, _Type) -> 
    E.

add_bin_binding(_, {var,_,'_'}, _Bs, BBs) ->
    BBs;
add_bin_binding(Mfun, {var,_,Name}, Bs, BBs) ->
    {value,Value} = Mfun(binding, {Name,Bs}),
    Mfun(add_binding, {Name,Value,BBs});
add_bin_binding(_, _, _Bs, BBs) ->
    BBs.

get_value(Bin, integer, Size, Unit, Sign, Endian) ->
    get_integer(Bin, Size*Unit, Sign, Endian);
get_value(Bin, float, Size, Unit, _Sign, Endian) ->
    get_float(Bin, Size*Unit, Endian);
get_value(Bin, utf8, undefined, _Unit, _Sign, _Endian) ->
    <<I/utf8,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, big) ->
    <<I/big-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, little) ->
    <<I/little-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf16, undefined, _Unit, _Sign, native) ->
    <<I/native-utf16,Rest/bits>> = Bin,
    {I,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, big) ->
    <<Val/big-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, little) ->
    <<Val/little-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, utf32, undefined, _Unit, _Sign, native) ->
    <<Val/native-utf32,Rest/bits>> = Bin,
    {Val,Rest};
get_value(Bin, binary, all, Unit, _Sign, _Endian) ->
    0 = (bit_size(Bin) rem Unit),
    {Bin,<<>>};
get_value(Bin, binary, Size, Unit, _Sign, _Endian) ->
    TotSize = Size*Unit,
    <<Val:TotSize/bitstring,Rest/bits>> = Bin,
    {Val,Rest}.

get_integer(Bin, Size, signed, little) ->
    <<Val:Size/little-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, little) ->
    <<Val:Size/little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, signed, native) ->
    <<Val:Size/native-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, native) ->
    <<Val:Size/native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, signed, big) ->
    <<Val:Size/signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_integer(Bin, Size, unsigned, big) ->
    <<Val:Size,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

get_float(Bin, Size, little) -> 
    <<Val:Size/float-little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float(Bin, Size, native) -> 
    <<Val:Size/float-native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float(Bin, Size, big) -> 
    <<Val:Size/float,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

%% Identical to the one in sys_pre_expand.
make_bit_type(Anno, default, Type0, ErrorFun) ->
    case erl_bits:set_bit_type(default, Type0) of
        {ok,all,Bt} -> {{atom,Anno,all},erl_bits:as_list(Bt)};
	{ok,undefined,Bt} -> {{atom,Anno,undefined},erl_bits:as_list(Bt)};
        {ok,Size,Bt} -> {{integer,Anno,Size},erl_bits:as_list(Bt)};
        {error,Reason} -> ErrorFun(Anno, Reason, ?STACKTRACE)
    end;
make_bit_type(Anno, Size, Type0, ErrorFun) -> %Size evaluates to an integer or 'all'
    case erl_bits:set_bit_type(Size, Type0) of
        {ok,Size,Bt} -> {Size,erl_bits:as_list(Bt)};
        {error,Reason} -> ErrorFun(Anno, Reason, ?STACKTRACE)
    end.

default_error(_Anno, Reason, Stacktrace) ->
    erlang:raise(error, Reason, Stacktrace).
