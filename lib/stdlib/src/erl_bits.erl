%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(erl_bits).

-export([system_bittypes/0, 
	 system_bitdefault/0,
	 set_bit_type/2,
	 as_list/1]).

-include("../include/erl_bits.hrl").

%% Dummies.

-spec system_bitdefault() -> 'no_system_bitdefault'.

system_bitdefault() -> no_system_bitdefault.

-spec system_bittypes() -> 'no_system_types'.

system_bittypes() -> no_system_types.

-spec as_list(#bittype{}) ->
    [bt_endian() | bt_sign() | bt_type() | {'unit', 'undefined' | bt_unit()}].

as_list(Bt) ->
    [Bt#bittype.type,{unit,Bt#bittype.unit},Bt#bittype.sign,Bt#bittype.endian].

%% XXX: tuple() below stands for what's produced by the parser
%%   {integer,L,M} | {var,L,VAR} | {atom,L,ATOM} | {op,L,OP,OP1,OP2} | ...
-type size() :: 'all' | 'unknown' | non_neg_integer() | tuple(). % XXX: REFINE
-type type() :: 'bytes' | 'bitstring' | 'bits'
              | bt_type() | bt_endian() | bt_sign()
              | {'unit', 'undefined' | bt_unit()}.

-spec set_bit_type('default' | size(), 'default' | [type()]) ->
        {'ok', 'undefined' | size(), #bittype{}} |
        {'error', {'undefined_bittype', term()}} |
        {'error', {'bittype_mismatch', term(), term(), string()}}.

set_bit_type(Size, default) ->
    set_bit_type(Size, []);
set_bit_type(Size, TypeList) ->
    try
	#bittype{type=Type,unit=Unit,sign=Sign,endian=Endian} =
	set_bit(TypeList),
	apply_defaults(Type, Size, Unit, Sign, Endian)
    catch
	throw:Error -> Error
    end.

set_bit([]) -> #bittype{};
set_bit([H|T]) -> set_bit_1(T, type_to_record(H)).

set_bit_1([T0|Ts], Bt0) ->
    Type = type_to_record(T0),
    Bt = merge_bittype(Type, Bt0),
    set_bit_1(Ts, Bt);
set_bit_1([], Bt) -> Bt.

type_to_record(integer) ->   #bittype{type   = integer};
type_to_record(utf8) ->      #bittype{type   = utf8};
type_to_record(utf16) ->     #bittype{type   = utf16};
type_to_record(utf32) ->     #bittype{type   = utf32};
type_to_record(float) ->     #bittype{type   = float};
type_to_record(binary) ->    #bittype{type   = binary};
type_to_record(bytes) ->     #bittype{type   = binary, unit = 8};
type_to_record(bitstring) -> #bittype{type   = binary, unit = 1};
type_to_record(bits) ->      #bittype{type   = binary, unit = 1};

type_to_record({unit,undefined}) ->
    #bittype{unit=undefined};
type_to_record({unit,Sz}) when is_integer(Sz), Sz > 0, Sz =< 256 ->
    #bittype{unit=Sz};

type_to_record(big) ->       #bittype{endian = big};
type_to_record(little) ->    #bittype{endian = little};
type_to_record(native) ->    #bittype{endian = native};

type_to_record(signed) ->    #bittype{sign   = signed};
type_to_record(unsigned) ->  #bittype{sign   = unsigned};

type_to_record(Name) ->      throw({error,{undefined_bittype,Name}}).

%%
%% Merge two bit type specifications.
%%
merge_bittype(B1, B2) ->
    Endian = merge_field(B1#bittype.endian, B2#bittype.endian, endianness),
    Sign   = merge_field(B1#bittype.sign, B2#bittype.sign, sign),
    Type   = merge_field(B1#bittype.type, B2#bittype.type, type),
    Unit   = merge_field(B1#bittype.unit, B2#bittype.unit, unit),
    #bittype{type=Type,unit=Unit,endian=Endian,sign=Sign}.

merge_field(undefined, B, _) -> B;
merge_field(A, undefined, _) -> A;
merge_field(A, A, _) -> A;
merge_field(X, Y, What) ->
    throw({error,{bittype_mismatch,X,Y,atom_to_list(What)}}).

%%
%% Defaults are as follows.
%% 
%% The default is integer.
%% The default size is 'all' for binaries, 8 for integers, 64 for floats.
%% No unit must be given if the size is not given.
%% The default unit size is 8 for binaries, and 1 for integers and floats.
%% The default sign is always unsigned.
%% The default endian is always big.
%%

apply_defaults(undefined, Size, Unit, Sign, Endian) -> %default type
    apply_defaults(integer, Size, Unit, Sign, Endian);

apply_defaults(binary, default, Unit, Sign, Endian) -> %default size
    %% check_unit(Unit), removed to allow bitlevel binaries
    apply_defaults(binary, all, Unit, Sign, Endian);
apply_defaults(integer, default, Unit, Sign, Endian) ->
    check_unit(Unit),
    apply_defaults(integer, 8, 1, Sign, Endian);
apply_defaults(utf8=Type, default, Unit, Sign, Endian) ->
    apply_defaults(Type, undefined, Unit, Sign, Endian);
apply_defaults(utf16=Type, default, Unit, Sign, Endian) ->
    apply_defaults(Type, undefined, Unit, Sign, Endian);
apply_defaults(utf32=Type, default, Unit, Sign, Endian) ->
    apply_defaults(Type, undefined, Unit, Sign, Endian);
apply_defaults(float, default, Unit, Sign, Endian) ->
    check_unit(Unit),
    apply_defaults(float, 64, 1, Sign, Endian);

apply_defaults(binary, Size, undefined, Sign, Endian) -> %default unit
    apply_defaults(binary, Size, 8, Sign, Endian);
apply_defaults(integer, Size, undefined, Sign, Endian) ->
    apply_defaults(integer, Size, 1, Sign, Endian);
apply_defaults(float, Size, undefined, Sign, Endian) ->
    apply_defaults(float, Size, 1, Sign, Endian);

apply_defaults(Type, Size, Unit, undefined, Endian) -> %default sign
    apply_defaults(Type, Size, Unit, unsigned, Endian);

apply_defaults(Type, Size, Unit, Sign, undefined) -> %default endian
    apply_defaults(Type, Size, Unit, Sign, big);

apply_defaults(Type, Size, Unit, Sign, Endian) -> %done
    check_size_unit(Type, Size, Unit),
    {ok,Size,#bittype{type=Type,unit=Unit,sign=Sign,endian=Endian}}.

check_size_unit(utf8, Size, Unit) ->
    check_size_unit_1(Size, Unit);
check_size_unit(utf16, Size, Unit) ->
    check_size_unit_1(Size, Unit);
check_size_unit(utf32, Size, Unit) ->
    check_size_unit_1(Size, Unit);
check_size_unit(_, _, _) -> ok.

check_size_unit_1(Size, Unit) ->
    case Size of
	default -> ok;
	undefined -> ok;
	{atom,_,undefined} -> ok;
	{value,_,undefined} -> ok;
	_ -> throw({error,utf_bittype_size_or_unit})
    end,
    case Unit of
	undefined -> ok;
	_ -> throw({error,utf_bittype_size_or_unit})
    end.

check_unit(undefined) -> ok;
check_unit(_) -> throw({error,bittype_unit}).
