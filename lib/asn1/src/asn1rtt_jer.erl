%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2025. All Rights Reserved.
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

-module(asn1rtt_jer).
%% encoding / decoding of BER
-ifdef(DEBUG).
-compile(export_all).
-endif.
%% For typeinfo JER
-export([encode_jer/3, decode_jer/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common code for all JER encoding/decoding
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
encode_jer(Module, Type, Val) ->
    Info = Module:typeinfo(Type),
    Enc = encode_jer(Info, Val),
    EncFun = fun({'KV_LIST', Value}, Encode) ->
                     json:encode_key_value_list(Value, Encode);
                (Other, Encode) ->
                     json:encode_value(Other, Encode)
             end,
    iolist_to_binary(json:encode(Enc, EncFun)).

%% {sequence,
%%    Name::atom() % The record name used for the sequence 
%%    Arity::integer() % number of components
%%    CompInfos::[CompInfo()] % list of components with name, type etc
%%    Value::record matching name and arity

encode_jer({sequence_tab,Simple,Sname,Arity,CompInfos},Value) 
  when tuple_size(Value) == Arity+1 ->
    [Sname|Clist] = tuple_to_list(Value),
    encode_jer_component_tab(CompInfos,Clist,Simple,#{});
%% {sequence,
%%    Name::atom() % The record name used for the sequence 
%%    Arity::integer() % number of components
%%    CompInfos::[CompInfo()] % list of components with name, type etc
%%    Value::record matching name and arity
encode_jer({sequence_map,_Sname,_Arity,CompInfos},Value) when is_map(Value) ->
    encode_jer_component_map(CompInfos,Value,[]);
encode_jer({sequence,Sname,Arity,CompInfos},Value) 
  when tuple_size(Value) == Arity+1 ->
    [Sname|Clist] = tuple_to_list(Value),
    encode_jer_component(CompInfos,Clist,[]);
encode_jer(string,Str) when is_list(Str) ->
    list_to_binary(Str);
encode_jer({string,_Prop},Str) when is_list(Str) ->
    list_to_binary(Str);
encode_jer(string,Str) when is_binary(Str) ->
    Str;
encode_jer({string,_Prop},Str) when is_binary(Str) ->
    Str;
encode_jer('INTEGER',Int) when is_integer(Int) ->
    Int;
encode_jer({'INTEGER',{Min,Max}},Int) when is_integer(Int),Max >=Int, Int >= Min ->
    Int;
encode_jer({'INTEGER_NNL',_NNL},Int) when is_integer(Int) ->
    Int;
encode_jer(Type = {'INTEGER_NNL',NNList},Int) when is_atom(Int) ->
    case lists:keyfind(Int, 1, NNList) of
        {_, NewVal} ->
            NewVal;
        _ ->
            exit({error, {asn1, {Type,Int}}})
    end;
encode_jer({Type = {'INTEGER_NNL',_NNList},_Constraint},Int) when is_atom(Int) ->
    encode_jer(Type,Int);
encode_jer({{'INTEGER_NNL',_NNList},Constraint},Int) when is_integer(Int) ->
    encode_jer({'INTEGER',Constraint},Int);
encode_jer('BOOLEAN',Bool) when is_boolean(Bool) ->
    Bool;
encode_jer({'BOOLEAN',_Prop},Bool) when is_boolean(Bool) ->
    Bool;
encode_jer('NULL',_) ->
    null;
encode_jer(legacy_octet_string, Value) when is_list(Value) ->
    bitstring2json(list_to_binary(Value));
encode_jer({legacy_octet_string,_Prop}, Value) when is_list(Value) ->
    bitstring2json(list_to_binary(Value));
encode_jer(octet_string,Value) when is_binary(Value) ->
    encode_jer({octet_string,[]}, Value);
encode_jer({octet_string,_Prop}, Value) when is_binary(Value) ->
    bitstring2json(Value);

encode_jer({'ENUMERATED',EnumMap},Val) when is_map_key(Val,EnumMap) ->
    Val;
encode_jer({Type = {'ENUMERATED',_EnumList},_Constr}, Val) ->
    encode_jer(Type,Val);

encode_jer({'ENUMERATED_EXT',_EnumMap},Val) when is_atom(Val) ->
    Val;
encode_jer({Type = {'ENUMERATED_EXT',_EnumList},_Constr}, Val) ->
    encode_jer(Type,Val);


encode_jer({typeinfo,{Module,Type}},Val) ->
    TypeInfo = Module:typeinfo(Type),
    encode_jer(TypeInfo, Val);

encode_jer({sof,Type},Vals) when is_list(Vals) ->
    [encode_jer(Type,Val)||Val <- Vals];
encode_jer({choice,Choices},{Alt,Value}) ->
    case is_map_key(AltBin = atom_to_binary(Alt,utf8),Choices) of
        true ->
            EncodedVal = encode_jer(maps:get(AltBin,Choices),Value),
            #{AltBin => EncodedVal};
        false ->
            exit({error,{asn1,{invalid_choice,Alt,Choices}}})
    end;
    
encode_jer(bit_string,Value) ->
    Str = bitstring2json(Value),
    #{value => Str, length => bit_size(Value)};
encode_jer({bit_string,{_,_}},Value) ->
    Str = bitstring2json(Value),
    #{value => Str, length => bit_size(Value)};
encode_jer({bit_string,FixedLength},Value) when is_bitstring(Value), is_integer(FixedLength) ->
    Value2 = jer_padbitstr(Value,FixedLength),
    bitstring2json(Value2);
encode_jer(compact_bit_string,Compact) ->
    BitStr = jer_compact2bitstr(Compact),
    encode_jer(bit_string,BitStr);
encode_jer({compact_bit_string,{_,_}},Compact) ->
    BitStr = jer_compact2bitstr(Compact),
    encode_jer(bit_string,BitStr);
encode_jer({compact_bit_string,FixedLength}, {_,Binary}=Compact) when is_binary(Binary) ->
    BitStr = jer_compact2bitstr(Compact),
    encode_jer({bit_string,FixedLength},BitStr);
encode_jer({compact_bit_string,FixedLength}, Compact) when is_integer(Compact) ->
    BitStr = jer_compact2bitstr(Compact),
    encode_jer({bit_string,FixedLength},BitStr);
encode_jer({bit_string_nnl,NNL},Value) -> 
    Value1 = jer_bit_str2bitstr(Value,NNL),
    encode_jer(bit_string,Value1);
encode_jer({{bit_string_nnl,NNL},FixedLength},Value) ->
    Value1 = jer_bit_str2bitstr(Value,NNL),
    encode_jer({bit_string,FixedLength},Value1);
encode_jer({compact_bit_string_nnl,NNL},Value) ->
    Value1 = jer_bit_str2bitstr(Value,NNL),
    encode_jer(bit_string,Value1);
encode_jer({{compact_bit_string_nnl,NNL},FixedLength},Value) ->
    Value1 = jer_bit_str2bitstr(Value,NNL),
    encode_jer({bit_string,FixedLength},Value1);
%%encode_jer({legacy_bit_string_nnl,NNL},Value) ->
%%encode_jer({{legacy_bit_string_nnl,NNL},FixedLength},Value) ->
encode_jer('OBJECT IDENTIFIER',Oid) when is_tuple(Oid) ->
    oid2json(Oid);
encode_jer('RELATIVE-OID',Oid) when is_tuple(Oid) ->
    oid2json(Oid);
encode_jer({'ObjClassFieldType',_,_},Val) when is_binary(Val)->
    Val;
encode_jer('ASN1_OPEN_TYPE',Val) when is_binary(Val) ->
    Val;
encode_jer({container,Type,_Containing}, Val) ->
    encode_jer(Type, Val);
encode_jer(Type,Val) ->
    exit({error,{asn1,{{encode,Type},Val}}}).


encode_jer_component_tab([{_Name, _Type, 'OPTIONAL'} | CompInfos], [asn1_NOVALUE | Rest], Simple, MapAcc) ->
    encode_jer_component_tab(CompInfos, Rest, Simple, MapAcc);
encode_jer_component_tab([{_Name, _Type, {'DEFAULT',_}} | CompInfos], [asn1_DEFAULT | Rest], Simple, MapAcc) ->
    encode_jer_component_tab(CompInfos, Rest, Simple, MapAcc);
encode_jer_component_tab([{Name, Type, _OptOrDefault} | CompInfos], [Value | Rest], Simple, MapAcc) ->
    Enc = encode_jer(Type, Value),
    encode_jer_component_tab(CompInfos, Rest, Simple, MapAcc#{Name => Enc});
encode_jer_component_tab([], _, _Simple, MapAcc) ->
    MapAcc.

encode_jer_component_map([{Name, AName, Type, _OptOrDefault} | CompInfos], MapVal, Acc)
  when is_map_key(AName, MapVal)->
    Value = maps:get(AName, MapVal),
    Enc = encode_jer(Type, Value),
    encode_jer_component_map(CompInfos, MapVal, [{Name,Enc}|Acc]);
encode_jer_component_map([{_Name, _AName, _Type, 'OPTIONAL'} | CompInfos], MapVal, Acc) ->
    encode_jer_component_map(CompInfos, MapVal, Acc);
encode_jer_component_map([{_Name, _AName, _Type, {'DEFAULT',_}} | CompInfos], MapVal, Acc) ->
    encode_jer_component_map(CompInfos, MapVal, Acc);
encode_jer_component_map([], MapVal, Acc) when map_size(MapVal) =:= length(Acc) ->
    {'KV_LIST', lists:reverse(Acc)};
encode_jer_component_map(_, MapVal, Acc) ->
    ErroneousKeys = maps:keys(MapVal) -- [K || {K,_V} <- Acc],
    exit({error,{asn1,{{encode,'SEQUENCE'},{erroneous_keys,ErroneousKeys}}}}).

encode_jer_component([{_Name, _Type, 'OPTIONAL'} | CompInfos], [asn1_NOVALUE | Rest], Acc) ->
    encode_jer_component(CompInfos, Rest, Acc);
encode_jer_component([{_Name, _Type, {'DEFAULT',_}} | CompInfos], [asn1_DEFAULT | Rest], Acc) ->
    encode_jer_component(CompInfos, Rest, Acc);
encode_jer_component([{Name, Type, _OptOrDefault} | CompInfos], [Value | Rest], Acc) ->
    Enc = encode_jer(Type, Value),
    encode_jer_component(CompInfos, Rest, [{Name,Enc}|Acc]);
encode_jer_component([], _, Acc) ->
    {'KV_LIST', lists:reverse(Acc)}.


decode_jer(Module, Type, Val) ->
    TypeInfo = Module:typeinfo(Type),
    decode_jer(TypeInfo, Val).
%% FIXME probably generate EnumList as a map with binaries as keys
%% and check if the Value is in the map. Also take the extensionmarker into
%% account and in that case allow any value but return as binary since it
%% is a potential atom leak to convert unknown values to atoms
%% maybe convert to existing atom
%% FIXME this is a discrepancy compare with other backends which return {asn1_enum,Val}
%% for unknown enum values when the type is extensible
decode_jer({'ENUMERATED',_EnumList}, Val) when is_binary(Val) ->
    binary_to_existing_atom(Val,utf8);
decode_jer({'ENUMERATED',_EnumList}, Val) when is_boolean(Val) ->
    Val;
decode_jer({'ENUMERATED',_EnumList}, null) ->
    null;
decode_jer({Type = {'ENUMERATED',_EnumList},_Constr}, Val) ->
    decode_jer(Type,Val);
decode_jer({'ENUMERATED_EXT',EnumList}, Val) ->
    decode_jer({'ENUMERATED',EnumList}, Val);
decode_jer({Type = {'ENUMERATED_EXT',_EnumList},_Constr}, Val) ->
    decode_jer(Type,Val);

decode_jer({typeinfo,{Module,Type}}, Val) ->
    TypeInfo = Module:typeinfo(Type),
    decode_jer(TypeInfo, Val);
decode_jer({sequence,Sname,_Arity,CompInfos},Value) 
  when is_map(Value) ->    
    DecodedComps = decode_jer_component(CompInfos,Value,[]),
    list_to_tuple([Sname|DecodedComps]);
decode_jer({sequence_map,_Sname,_Arity,CompInfos},Value) 
  when is_map(Value) ->    
    decode_jer_component_map(CompInfos,Value,[]);

%% Unfortunately we have to represent strings as lists to be compatible 
%% with the other backends. Should add an option to the compiler in the future
%% which makes it possible to represent all strings as erlang binaries
decode_jer(string,Str) when is_binary(Str) ->
    binary_to_list(Str);
decode_jer({string,_Prop},Str) when is_binary(Str) ->
    binary_to_list(Str);
decode_jer('INTEGER',Int) when is_integer(Int) ->
    Int;
decode_jer({'INTEGER',{Min,Max}},Int) when is_integer(Int),Max >=Int, Int >= Min ->
    Int;
decode_jer({Type = {'INTEGER_NNL',_NNList},_},Int) ->
    decode_jer(Type,Int);
decode_jer({'INTEGER_NNL',NNList},Int) ->
    case lists:keyfind(Int, 2, NNList) of
        {NewName, _} ->
            NewName;
        _ ->
            Int
    end;
decode_jer('BOOLEAN',Bool) when is_boolean(Bool) ->
    Bool;
decode_jer({'BOOLEAN',_Prop},Bool) when is_boolean(Bool) ->
    Bool;
decode_jer('NULL',null) ->
    'NULL';
decode_jer(legacy_octet_string,Str) when is_binary(Str) ->
    json2octetstring2string(binary_to_list(Str));
decode_jer({legacy_octet_string,_Size},Str) when is_binary(Str) ->
    json2octetstring2string(binary_to_list(Str));
decode_jer(octet_string,Str) when is_binary(Str) ->
    json2octetstring2binary(binary_to_list(Str));
decode_jer({octet_string,_Size},Str) when is_binary(Str) ->
    json2octetstring2binary(binary_to_list(Str));
decode_jer({sof,Type},Vals) when is_list(Vals) ->
    [decode_jer(Type,Val)||Val <- Vals];
decode_jer({choice,ChoiceTypes},ChoiceVal) ->
    [{Alt,Val}] = maps:to_list(ChoiceVal),
    case ChoiceTypes of
        #{Alt := Type} ->
            Type = maps:get(Alt,ChoiceTypes),
            {binary_to_atom(Alt,utf8),decode_jer(Type,Val)};
        _ ->
            exit({error,{asn1,{invalid_choice,Alt,maps:keys(ChoiceTypes)}}})
    end;
decode_jer(bit_string,#{<<"value">> := Str, <<"length">> := Length}) ->
    json2bitstring(binary_to_list(Str),Length);
decode_jer({bit_string,FixedLength},Str) when is_binary(Str) ->
    json2bitstring(binary_to_list(Str),FixedLength);
decode_jer({{bit_string_nnl,NNL},{_,_}},#{<<"value">> := Str, <<"length">> := Length}) ->
    BitStr = json2bitstring(binary_to_list(Str),Length),
    jer_bitstr2names(BitStr,NNL);
decode_jer({bit_string_nnl,NNL},#{<<"value">> := Str, <<"length">> := Length}) -> 
    BitStr = json2bitstring(binary_to_list(Str),Length),
    jer_bitstr2names(BitStr,NNL);
decode_jer({{bit_string_nnl,NNL},FixedLength},Str) when is_binary(Str)->
    BitStr = json2bitstring(binary_to_list(Str),FixedLength),
    jer_bitstr2names(BitStr,NNL);
decode_jer({compact_bit_string_nnl,NNL},Value) ->
    decode_jer({bit_string_nnl,NNL},Value);
decode_jer({{compact_bit_string_nnl,NNL},FixedLength},Value) ->
    decode_jer({{bit_string_nnl,NNL},FixedLength},Value);
decode_jer(compact_bit_string,#{<<"value">> := Str, <<"length">> := Length}) ->
    BitStr = json2bitstring(binary_to_list(Str),Length),
    jer_bitstr2compact(BitStr);
decode_jer({compact_bit_string,{_,_}},#{<<"value">> := Str, <<"length">> := Length}) ->
    BitStr = json2bitstring(binary_to_list(Str),Length),
    jer_bitstr2compact(BitStr);
decode_jer({compact_bit_string,FixedLength},Str) ->
    BitStr = json2bitstring(binary_to_list(Str),FixedLength),
    Unused = (8 - (FixedLength rem 8)) band 7,
    {Unused,<<BitStr/bitstring,0:Unused>>};
decode_jer('OBJECT IDENTIFIER',OidBin) when is_binary(OidBin) ->
    json2oid(OidBin);
decode_jer('RELATIVE-OID',OidBin) when is_binary(OidBin) ->
    json2oid(OidBin);
decode_jer({'ObjClassFieldType',_,_},Bin) when is_binary(Bin) ->
    Bin;
decode_jer('ASN1_OPEN_TYPE',Bin) when is_binary(Bin) ->
    Bin;
decode_jer({container,Type,_Containing}, Val) ->
    decode_jer(Type, Val);
decode_jer(Type,Val) ->
    exit({error,{asn1,{{decode,Type},Val}}}).

decode_jer_component([{Name, Type, _OptOrDefault} | CompInfos], VMap, Acc)
    when is_map_key(Name, VMap) ->
    Value = maps:get(Name, VMap),
    Dec = decode_jer(Type, Value),
    decode_jer_component(CompInfos, VMap, [Dec | Acc]);
decode_jer_component([{_Name, _Type, 'OPTIONAL'} | CompInfos], VMap, Acc) ->
    decode_jer_component(CompInfos, VMap, [asn1_NOVALUE | Acc]);
decode_jer_component([{_Name, _Type, {'DEFAULT',Dvalue}} | CompInfos], VMap, Acc) ->
    decode_jer_component(CompInfos, VMap, [Dvalue | Acc]);
decode_jer_component([{Name, _Type, _OptOrDefault} | _CompInfos], VMap, _Acc) ->
    exit({error,{asn1,{{decode,{mandatory_component_missing,Name}},VMap}}});
decode_jer_component([], _, Acc) ->
    lists:reverse(Acc).

decode_jer_component_map([{Name, AtomName, Type, _OptOrDefault} | CompInfos], VMap, Acc)
    when is_map_key(Name, VMap) ->
    Value = maps:get(Name, VMap),
    Dec = decode_jer(Type, Value),
    decode_jer_component_map(CompInfos, VMap, [{AtomName,Dec} | Acc]);
decode_jer_component_map([{_Name, _AtomName, _Type, 'OPTIONAL'} | CompInfos], VMap, Acc) ->
    decode_jer_component_map(CompInfos, VMap, Acc);
decode_jer_component_map([{_Name, AtomName, _Type, {'DEFAULT',Dvalue}} | CompInfos], VMap, Acc) ->
    decode_jer_component_map(CompInfos, VMap, [{AtomName, Dvalue} | Acc]);
decode_jer_component_map([{Name, _AtomName, _Type, _OptOrDefault} | _CompInfos], VMap, _Acc) ->
    exit({error,{asn1,{{decode,{mandatory_component_missing,Name}},VMap}}});
decode_jer_component_map([], _, Acc) ->
    %% not reusing the map from JSON decoder since it can contain non expected extra K,V pairs
    maps:from_list(Acc).

%% This is the default representation of octet string i.e binary
json2octetstring2binary(Value) ->
    list_to_binary(json2octetstring(Value,[])).

%% This is the legacy_types representation of octet string i.e as a list
json2octetstring2string(Value) ->
    json2octetstring(Value,[]).

json2octetstring([A1,A2|Rest],Acc) ->
    Int = list_to_integer([A1,A2],16),
    json2octetstring(Rest,[Int|Acc]);
json2octetstring([], Acc) ->
    lists:reverse(Acc).

json2bitstring(Value,Length) ->
    json2bitstring(Value,Length,[]).

json2bitstring([A1,A2],Length,Acc) ->
    Int = list_to_integer([A1,A2],16) bsr (8-Length),
    Bin = list_to_binary(lists:reverse(Acc)),
    << Bin/binary,Int:Length>>;
json2bitstring([A1,A2|Rest],Length,Acc) ->
    Int = list_to_integer([A1,A2],16),
    json2bitstring(Rest,Length-8,[Int|Acc]);
json2bitstring([],0,Acc) ->
    Bin = list_to_binary(lists:reverse(Acc)),
    Bin.

bitstring2json(BitStr) when is_binary(BitStr) ->
    octetstring2json(binary_to_list(BitStr));
bitstring2json(BitStr) ->
    Pad = 8 - bit_size(BitStr) rem 8,
    NewStr = <<BitStr/bitstring,0:Pad>>,
    octetstring2json(binary_to_list(NewStr)).

octetstring2json(List) when is_list(List) ->
    list_to_binary([begin Num = integer_to_list(X,16), 
           if length(Num) == 1 -> "0"++Num;
              true -> Num
           end 
     end|| X<-List]).

oid2json(Oid) when is_tuple(Oid) ->
    OidList = tuple_to_list(Oid),
    OidNumberStr = [integer_to_list(V)|| V <- OidList],
    oid2json(OidNumberStr,[]).

oid2json([Num|T],[]) ->
    oid2json(T,[Num]);
oid2json([Num|T],Acc) ->
    oid2json(T,[Num,$.|Acc]);
oid2json([],Acc) ->
    list_to_binary(lists:reverse(Acc)).

json2oid(OidStr) when is_binary(OidStr) ->
    OidList = binary:split(OidStr,[<<".">>],[global]),
    OidNumList = [binary_to_integer(Num)||Num <- OidList],
    list_to_tuple(OidNumList).

jer_bit_str2bitstr(Compact = {_Unused,_Binary}, _NamedBitList) ->
    jer_compact2bitstr(Compact);
jer_bit_str2bitstr(Int, _NamedBitList) when is_integer(Int) ->
    jer_compact2bitstr(Int);
jer_bit_str2bitstr(BitList = [Bit|_], _NamedBitList) when Bit == 1; Bit == 0 ->
    Int = list_to_integer([case B of 0 -> $0; 1 -> $1 end || B <- BitList],2),
    Len = length(BitList),
    <<Int:Len>>;
jer_bit_str2bitstr([H | _] = Bits, NamedBitList)
    when is_atom(H) ->
    jer_do_encode_named_bit_string(Bits, NamedBitList);
jer_bit_str2bitstr([{bit, _} | _] = Bits, NamedBitList) ->
    jer_do_encode_named_bit_string(Bits, NamedBitList);
jer_bit_str2bitstr([], _NamedBitList) ->
    <<>>;
jer_bit_str2bitstr(BitStr,_NamedBitList) when is_bitstring(BitStr) ->
    BitStr.

jer_compact2bitstr({Unused,Binary}) ->
    Size = bit_size(Binary) - Unused,
    <<BitStr:Size/bitstring,_/bitstring >> = Binary,
    BitStr;
jer_compact2bitstr(Int) when is_integer(Int) ->
    jer_int2bitstr(Int);
jer_compact2bitstr(BitList = [Bit|_]) when Bit == 1; Bit == 0 ->
    IntStr = jer_skip_trailing_zeroes(BitList,[]),
    Int = list_to_integer(IntStr,2),
    Len = length(IntStr),
    <<Int:Len>>.

jer_skip_trailing_zeroes([1|Rest],Acc) ->
    jer_skip_trailing_zeroes(Rest,[$1|Acc]);
jer_skip_trailing_zeroes([0|Rest],Acc) ->
    jer_skip_trailing_zeroes(Rest,[$0|Acc]);
jer_skip_trailing_zeroes([],[$0|Acc]) ->
    jer_skip_trailing_zeroes([],Acc);
jer_skip_trailing_zeroes([],Acc) ->
    lists:reverse(Acc).


    

jer_padbitstr(BitStr,FixedLength) when bit_size(BitStr) == FixedLength ->
    BitStr;
jer_padbitstr(BitStr,FixedLength) when bit_size(BitStr) < FixedLength ->
    Len = bit_size(BitStr),
    PadLen = FixedLength - Len,
    <<BitStr/bitstring,0:PadLen>>.

jer_int2bitstr(Int) when is_integer(Int), Int >= 0 ->
    jer_int2bitstr(Int,<<>>).

jer_int2bitstr(0,Acc) ->
    Acc;
jer_int2bitstr(Int,Acc) ->
    Bit = Int band 1,
    jer_int2bitstr(Int bsr 1,<<Acc/bitstring,Bit:1>>).
    
jer_bitstr2compact(BitStr) ->
    Size = bit_size(BitStr),
    Unused = (8 - Size rem 8) band 7,
    {Unused,<<BitStr/bitstring,0:Unused>>}.

jer_do_encode_named_bit_string([FirstVal | RestVal], NamedBitList) ->
    ToSetPos = jer_get_all_bitposes([FirstVal | RestVal], NamedBitList, []),
    Size = lists:max(ToSetPos) + 1,
    BitList = jer_make_and_set_list(Size, ToSetPos, 0),
    jer_encode_bitstring(BitList).

jer_get_all_bitposes([{bit, ValPos} | Rest], NamedBitList, Ack) ->
    jer_get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
jer_get_all_bitposes([Val | Rest], NamedBitList, Ack) when is_atom(Val) ->
    case lists:keyfind(Val, 1, NamedBitList) of
        {_ValName, ValPos} ->
            jer_get_all_bitposes(Rest, NamedBitList, [ValPos | Ack]);
        _ ->
            exit({error, {asn1, {bitstring_namedbit, Val}}})
    end;
jer_get_all_bitposes([], _NamedBitList, Ack) ->
    lists:sort(Ack).

jer_make_and_set_list(0, [], _) ->
    [];
jer_make_and_set_list(0, _, _) ->
    exit({error, {asn1, bitstring_sizeconstraint}});
jer_make_and_set_list(Len, [XPos | SetPos], XPos) ->
    [1 | jer_make_and_set_list(Len - 1, SetPos, XPos + 1)];
jer_make_and_set_list(Len, [Pos | SetPos], XPos) ->
    [0 | jer_make_and_set_list(Len - 1, [Pos | SetPos], XPos + 1)];
jer_make_and_set_list(Len, [], XPos) ->
    [0 | jer_make_and_set_list(Len - 1, [], XPos + 1)].

%%=================================================================
%% Do the actual encoding
%%     ([bitlist]) -> {ListLen, UnusedBits, OctetList}
%%=================================================================

jer_encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1 | Rest]) ->
    Val = (B8 bsl 7) bor (B7 bsl 6) bor (B6 bsl 5) bor (B5 bsl 4) bor
	(B4 bsl 3) bor (B3 bsl 2) bor (B2 bsl 1) bor B1,
    jer_encode_bitstring(Rest, <<Val>>);
jer_encode_bitstring(Val) ->
    jer_unused_bitlist(Val, <<>>).

jer_encode_bitstring([B8, B7, B6, B5, B4, B3, B2, B1 | Rest], Acc) ->
    Val = (B8 bsl 7) bor (B7 bsl 6) bor (B6 bsl 5) bor (B5 bsl 4) bor
	(B4 bsl 3) bor (B3 bsl 2) bor (B2 bsl 1) bor B1,
    jer_encode_bitstring(Rest, [Acc | [Val]]);
%%even multiple of 8 bits..
jer_encode_bitstring([], Acc) ->
    Acc;
%% unused bits in last octet
jer_encode_bitstring(Rest, Acc) ->
    jer_unused_bitlist(Rest, Acc).

%%%%%%%%%%%%%%%%%%
%% unused_bitlist([list of ones and zeros <= 7], 7, []) ->
%%  {Unused bits, Last octet with bits moved to right}
jer_unused_bitlist([], Acc) ->
    Acc;
jer_unused_bitlist([Bit | Rest], Acc) ->
    jer_unused_bitlist(Rest, <<Acc/bitstring,Bit:1>>).

jer_bitstr2names(BitStr,[]) ->
    BitStr;
jer_bitstr2names(BitStr,NNL) ->
    %% Fixme, the sorting should be done in compile time, maybe it already is
    SortedList  = lists:keysort(2,NNL), %% Should be from bit 0 to bit N
    jer_bitstr2names(BitStr,SortedList,0,[]).

jer_bitstr2names(<<1:1,BitStr/bitstring>>,[{Name,Pos}|Rest],Pos,Acc) ->
    jer_bitstr2names(BitStr,Rest,Pos+1,[Name|Acc]);
jer_bitstr2names(<<1:1,BitStr/bitstring>>,NNL,Num,Acc) ->
    jer_bitstr2names(BitStr,NNL,Num+1,[{bit,Num}|Acc]);
jer_bitstr2names(<<0:1,BitStr/bitstring>>,[{_,Num}|Rest],Num,Acc) ->
    jer_bitstr2names(BitStr,Rest,Num+1,Acc);
jer_bitstr2names(<<0:1,BitStr/bitstring>>,NNL,Num,Acc) ->
    jer_bitstr2names(BitStr,NNL,Num+1,Acc);
jer_bitstr2names(<<>>,_,_,Acc) ->
    lists:reverse(Acc).


    

    
    
