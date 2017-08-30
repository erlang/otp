%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
-module(asn1ct_value).
-compile([{nowarn_deprecated_function,{asn1rt,utf8_list_to_binary,1}}]).

%%  Generate Erlang values for ASN.1 types.
%%  The value is randomized within it's constraints

-include("asn1_records.hrl").
%-compile(export_all).

-export([from_type/2]).

%% Generate examples of values ******************************
%%****************************************x


from_type(M,Typename) ->
    case asn1_db:dbload(M) of
	error ->
	    {error,{not_found,{M,Typename}}};
	ok ->
	    #typedef{typespec=Type} = asn1_db:dbget(M, Typename),
	    from_type(M,[Typename],Type);
    Vdef when is_record(Vdef,valuedef) ->
        from_value(Vdef);
	Err ->
	    {error,{other,Err}}
    end.

from_type(M,Typename,Type) when is_record(Type,type) ->
    InnerType = get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    from_type(Emod,Etype);
	{_,user} ->
		from_type(M,InnerType);
	{primitive,bif} ->
	    from_type_prim(M, Type);
	'ASN1_OPEN_TYPE' ->
	    case  Type#type.constraint of
		[#'Externaltypereference'{type=TrefConstraint}] ->
		    from_type(M,TrefConstraint);
		_ ->
		    ERule = get_encoding_rule(M),
		    open_type_value(ERule)
	    end;
	{constructed,bif} when Typename == ['EXTERNAL'] ->
	    Val=from_type_constructed(M,Typename,InnerType,Type),
	    asn1ct_eval_ext:transform_to_EXTERNAL1994(Val);
	{constructed,bif} ->
	    from_type_constructed(M,Typename,InnerType,Type)
    end;
from_type(M,Typename,#'ComponentType'{name = Name,typespec = Type})  ->
    from_type(M,[Name|Typename],Type);
from_type(_,_,_) -> % 'EXTENSIONMARK'
    undefined.

from_value(#valuedef{type = #type{def = 'INTEGER'}, value = Val}) ->
    Val.

get_inner(A) when is_atom(A) -> A;    
get_inner(Ext) when is_record(Ext,'Externaltypereference') -> Ext;    
get_inner({typereference,_Pos,Name}) -> Name;
get_inner(T) when is_tuple(T) -> 
    case asn1ct_gen:get_inner(T) of
	{fixedtypevaluefield,_,Type} ->
	    Type#type.def;
	{typefield,_FieldName} -> 
	    'ASN1_OPEN_TYPE';
	Other ->
	    Other
    end.
%%get_inner(T) when is_tuple(T) -> element(1,T).



from_type_constructed(M,Typename,InnerType,D) when is_record(D,type) ->
    case InnerType of
	'SET' ->
	    get_sequence(M,Typename,D);
	'SEQUENCE' ->
	    get_sequence(M,Typename,D);
	'CHOICE' ->
	    get_choice(M,Typename,D);
	'SEQUENCE OF' ->
	    {_,Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType,Type#type.def),
	    get_sequence_of(M,Typename,D,NameSuffix);
	'SET OF' ->
	    {_,Type} = D#type.def,
	    NameSuffix = asn1ct_gen:constructed_suffix(InnerType,Type#type.def),
	    get_sequence_of(M,Typename,D,NameSuffix);
	_ ->
	    exit({nyi,InnerType})
    end.

get_sequence(M,Typename,Type) ->
    {_SEQorSET,CompList} = 
	case Type#type.def of
	    #'SEQUENCE'{components=Cl} -> {'SEQUENCE',Cl};
	    #'SET'{components=Cl} -> {'SET',to_textual_order(Cl)}
	end,
    case get_components(M,Typename,CompList) of
        [] ->
            {list_to_atom(asn1ct_gen:list2rname(Typename))};
        C ->
            list_to_tuple([list_to_atom(asn1ct_gen:list2rname(Typename))|C])
    end.

get_components(M,Typename,{Root,Ext}) ->
    get_components(M,Typename,Root++Ext);

%% Should enhance this *** HERE *** with proper handling of extensions

get_components(M,Typename,[H|T]) ->
    [from_type(M,Typename,H)|
    get_components(M,Typename,T)];
get_components(_,_,[]) ->
    [].

get_choice(M,Typename,Type) ->
    {'CHOICE',TCompList} = Type#type.def,
    case TCompList of
	[] -> 
	    {asn1_EMPTY,asn1_EMPTY};
	{CompList,ExtList} -> % Should be enhanced to handle extensions too
	    CList = CompList ++ ExtList,
	    C = lists:nth(random(length(CList)),CList),
	    {C#'ComponentType'.name,from_type(M,Typename,C)};
	CompList when is_list(CompList) ->
	    C = lists:nth(random(length(CompList)),CompList),
	    {C#'ComponentType'.name,from_type(M,Typename,C)}
    end.
    
get_sequence_of(M,Typename,Type,TypeSuffix) ->
    %% should generate length according to constraints later
    {_,Oftype} = Type#type.def,
    C = Type#type.constraint,
    S = size_random(C),
    NewTypeName = [TypeSuffix|Typename],
    gen_list(M,NewTypeName,Oftype,S).

gen_list(_,_,_,0) ->
    [];
gen_list(M,Typename,Oftype,N) ->
    [from_type(M,Typename,Oftype)|gen_list(M,Typename,Oftype,N-1)].
    
from_type_prim(M, D) ->
    C = D#type.constraint,
    case D#type.def of
	'INTEGER' ->
	    i_random(C);
	{'INTEGER',[_|_]=NNL} ->
	    case C of
		[] ->
		    {N,_} = lists:nth(random(length(NNL)), NNL),
		    N;
		_ ->
		    V = i_random(C),
		    case lists:keyfind(V, 2, NNL) of
			false -> V;
			{N,V} -> N
		    end
	    end;
	Enum when is_tuple(Enum),element(1,Enum)=='ENUMERATED' ->
	    NamedNumberList =
		case Enum of
		    {_,_,NNL} -> NNL;
		    {_,NNL} -> NNL
		end,
	    NNew=
		case NamedNumberList of
		    {N1,N2} ->
			N1 ++ N2;
		    _->
			NamedNumberList
		end,
	    NN = [X||{X,_} <- NNew],
	    case NN of
		[] ->
            io:format(user, "Enum = ~p~n", [Enum]),
		    asn1_EMPTY;
		_ ->
		    case C of
			[] ->
			    lists:nth(random(length(NN)),NN);
			_ ->
			    lists:nth((fun(0)->1;(X)->X end(i_random(C))),NN)
		    end
	    end;
	{'BIT STRING',NamedNumberList} ->
	    NN = [X||{X,_} <- NamedNumberList],
	    case NN of
		[] ->
		    random_unnamed_bit_string(M, C);
		_ ->
		    [lists:nth(random(length(NN)),NN)]
	    end;
	'NULL' ->
	    'NULL';
	'OBJECT IDENTIFIER' ->
	    Len = random(3),
	    Olist = [(random(1000)-1)||_X <-lists:seq(1,Len)],
	    list_to_tuple([random(3)-1,random(40)-1|Olist]);
	'RELATIVE-OID' ->
	    Len = random(5),
	    Olist = [(random(16#ffff)-1)||_X <-lists:seq(1,Len)],
	    list_to_tuple(Olist);
	'ObjectDescriptor' ->
	    "Dummy ObjectDescriptor";
	'REAL' ->
	    %% Base is 2 or 10, format is string (base 10) or tuple
	    %% (base 2 or 10)
	    %% Tuple: {Mantissa, Base, Exponent}
	    case random(3) of
		1 ->
		    %% base 2
		    case random(3) of
			3 ->
			    {129,2,10};
			2 ->
			    {1,2,1};
			_ ->
			    {2#11111111,2,2}
		    end;
%% 		    Sign1 = random_sign(integer),
%% 		    Sign2 = random_sign(integer),
%% 		    {Sign1*random(10000),2,Sign2*random(1028)};
%% 		2 ->
%% 		    %% base 10 tuple format
%% 		    Sign1 = random_sign(integer),
%% 		    Sign2 = random_sign(integer),
%% 		    {Sign1*random(10000),10,Sign2*random(1028)};
		_ ->
		    %% base 10 string format, NR3 format
		    case random(2) of
			2 ->
			    "123.E10";
			_ ->
			    "-123.E-10"
		    end
	    end;
	'BOOLEAN' ->
	    true;
	'OCTET STRING' ->
	    S0 = adjust_list(size_random(C), c_string(C, "OCTET STRING")),
	    case M:legacy_erlang_types() of
		false -> list_to_binary(S0);
		true -> S0
	    end;
	'NumericString' ->
	    adjust_list(size_random(C),c_string(C,"0123456789"));
	'TeletexString' ->
	    adjust_list(size_random(C),c_string(C,"TeletexString"));
	'T61String' ->
	    adjust_list(size_random(C),c_string(C,"T61String"));
	'VideotexString' ->
	    adjust_list(size_random(C),c_string(C,"VideotexString"));
	'UTCTime' ->
	    "97100211-0500";
	'GeneralizedTime' ->
	    "19971002103130.5";
	'GraphicString' ->
	    adjust_list(size_random(C),c_string(C,"GraphicString"));
	'VisibleString' ->
	    adjust_list(size_random(C),c_string(C,"VisibleString"));
	'GeneralString' ->
	    adjust_list(size_random(C),c_string(C,"GeneralString"));
	'PrintableString' ->
	    adjust_list(size_random(C),c_string(C,"PrintableString"));
	'IA5String' ->
	    adjust_list(size_random(C),c_string(C,"IA5String"));
	'BMPString' ->
	    adjust_list(size_random(C),c_string(C,"BMPString"));
	'UTF8String' ->
	    {ok,Res}=asn1rt:utf8_list_to_binary(adjust_list(random(50),[$U,$T,$F,$8,$S,$t,$r,$i,$n,$g,16#ffff,16#fffffff,16#ffffff,16#fffff,16#fff])),
	    Res;
	'UniversalString' ->
	    adjust_list(size_random(C),c_string(C,"UniversalString"));
	XX ->
	    exit({asn1_error,nyi,XX})
    end.

c_string(C,Default) ->
    case get_constraint(C,'PermittedAlphabet') of
	{'SingleValue',Sv} when is_list(Sv) ->
	    Sv;
	{'SingleValue',V} when is_integer(V) ->
	    [V];
	no ->
	    Default
    end.

random_unnamed_bit_string(M, C) ->
    Bl1 = lists:reverse(adjust_list(size_random(C), [1,0,1,1])),
    Bl2 = lists:reverse(lists:dropwhile(fun(0)-> true;
					   (1) -> false
					end,Bl1)),
    Val = case {length(Bl2),get_constraint(C, 'SizeConstraint')} of
	      {Len,Len} ->
		  Bl2;
	      {_Len,Int} when is_integer(Int) ->
		  Bl1;
	      {Len,{Min,_}} when Min > Len ->
		  Bl1;
	      _ ->
		  Bl2
	  end,
    case M:bit_string_format() of
	legacy ->
	    Val;
	bitstring ->
	    << <<B:1>> || B <- Val >>;
	compact ->
	    BitString = << <<B:1>> || B <- Val >>,
	    PadLen = (8 - (bit_size(BitString) band 7)) band 7,
	    {PadLen,<<BitString/bitstring,0:PadLen>>}
    end.

%% FIXME:
%% random_sign(integer) ->
%%     case random(2) of
%% 	2 ->
%% 	    -1;
%% 	_ ->
%% 	    1
%%     end;
%% random_sign(string) ->
%%     case random(2) of
%% 	2 ->
%% 	    "-";
%% 	_ ->
%% 	    ""
%%     end.

random(Upper) ->
    rand:uniform(Upper).

size_random(C) ->
    case get_constraint(C,'SizeConstraint') of
	no ->
	    c_random({0,5},no);
	{{Lb,Ub},_} when is_integer(Lb),is_integer(Ub) ->
	    if
		Ub-Lb =< 4 ->
		    c_random({Lb,Ub},no);
		true ->
		    c_random({Lb,Lb+4},no)
	    end;
	{Lb,Ub} when Ub-Lb =< 4 ->
	    c_random({Lb,Ub},no);
	{Lb,_}  ->
	    c_random({Lb,Lb+4},no);
	Sv ->
	    c_random(no,Sv)
    end.
	
i_random(C) ->
    c_random(get_constraint(C,'ValueRange'),get_constraint(C,'SingleValue')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% c_random(Range,SingleValue)
%% only called from other X_random functions

c_random(VRange,Single) ->
    case {VRange,Single} of
	{no,no} ->
	    random(16#fffffff) - (16#fffffff bsr 1);
	{R,no} ->
	    case R of 
		{Lb,Ub} when is_integer(Lb),is_integer(Ub) ->
		    Range = Ub - Lb +1,
		    Lb + (random(Range)-1);
		{Lb,'MAX'} ->
		    Lb + random(16#fffffff)-1;
		{'MIN',Ub} ->
		    Ub - random(16#fffffff)-1;
		{A,{'ASN1_OK',B}} ->
		    Range = B - A +1,
		    A + (random(Range)-1)
	    end;
	{_,S} when is_integer(S) ->
	    S;
	{_,S} when is_list(S) ->
	    lists:nth(random(length(S)),S)
%%	{S1,S2} ->
%%	    io:format("asn1ct_value: hejsan hoppsan~n");
%%	_ ->
%%	    io:format("asn1ct_value: hejsan hoppsan 2~n")
%%	    io:format("asn1ct_value: c_random/2: S1 = ~w~n"
%%		      "S2 = ~w,~n",[S1,S2])
%%	    exit(self(),goodbye)
    end.

adjust_list(Len,Orig) ->
    adjust_list1(Len,Orig,Orig,[]).

adjust_list1(0,_Orig,[_Oh|_Ot],Acc) ->
    lists:reverse(Acc);
adjust_list1(Len,Orig,[],Acc) ->
    adjust_list1(Len,Orig,Orig,Acc);
adjust_list1(Len,Orig,[Oh|Ot],Acc) ->
    adjust_list1(Len-1,Orig,Ot,[Oh|Acc]).


get_constraint(C, Key) ->
    case lists:keyfind(Key, 1, C) of
        false                    -> no;
        {'ValueRange', {Lb, Ub}} -> {check_external(Lb), check_external(Ub)};
        {'SizeConstraint', N}    -> N;
        {Key, Value}             -> Value
    end.

check_external(ExtRef) when is_record(ExtRef, 'Externalvaluereference') ->
    #'Externalvaluereference'{module = Emod, value = Evalue} = ExtRef,
    from_type(Emod, Evalue);
check_external(Value) ->
    Value.

get_encoding_rule(M) ->
    Mod =
	if is_list(M) ->
		list_to_atom(M);
	   true ->M
	end,
    case (catch Mod:encoding_rule()) of
	A when is_atom(A) ->
	    A;
	_ -> unknown
    end.

open_type_value(ber) ->
    <<4,9,111,112,101,110,95,116,121,112,101>>;
open_type_value(_) ->
    <<"\n\topen_type">>.	       %octet string value "open_type"

to_textual_order({Root,Ext}) ->
    {to_textual_order(Root),Ext};
to_textual_order(Cs) when is_list(Cs) ->
    case Cs of
	[#'ComponentType'{textual_order=undefined}|_] ->
	    Cs;
	_ ->
	    lists:keysort(#'ComponentType'.textual_order,Cs)
    end.
