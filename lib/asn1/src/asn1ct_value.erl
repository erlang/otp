%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%%
-module(asn1ct_value).

%%  Generate Erlang values for ASN.1 types.
%%  The value is randomized within it's constraints

-include("asn1_records.hrl").
%-compile(export_all).

-export([get_type/3]).
-export([i_random/1]).


%% Generate examples of values ******************************
%%****************************************x


get_type(M,Typename,Tellname) ->
    case asn1_db:dbget(M,Typename) of
	undefined -> 
	    {asn1_error,{not_found,{M,Typename}}};
	Tdef when is_record(Tdef,typedef) ->
	    Type = Tdef#typedef.typespec,
	    get_type(M,[Typename],Type,Tellname);
	Err ->
	    {asn1_error,{other,Err}}
    end.

get_type(M,Typename,Type,Tellname) when is_record(Type,type) ->
    InnerType = get_inner(Type#type.def),
    case asn1ct_gen:type(InnerType) of
	#'Externaltypereference'{module=Emod,type=Etype} ->
	    get_type(Emod,Etype,Tellname);
	{_,user} ->
	    case Tellname of
		yes -> {Typename,get_type(M,InnerType,no)};
		no -> get_type(M,InnerType,no)
	    end;
	{notype,_} ->
	    true;
	{primitive,bif} ->
	    get_type_prim(Type,get_encoding_rule(M));
	'ASN1_OPEN_TYPE' ->
	    case  Type#type.constraint of
		[#'Externaltypereference'{type=TrefConstraint}] ->
		    get_type(M,TrefConstraint,no);
		_ ->
		    ERule = get_encoding_rule(M),
		    open_type_value(ERule)
	    end;
	{constructed,bif} when Typename == ['EXTERNAL'] ->
	    Val=get_type_constructed(M,Typename,InnerType,Type),
	    asn1rt_check:transform_to_EXTERNAL1994(Val);
	{constructed,bif} ->
	    get_type_constructed(M,Typename,InnerType,Type)
    end;
get_type(M,Typename,#'ComponentType'{name = Name,typespec = Type},_)  ->
    get_type(M,[Name|Typename],Type,no);
get_type(_,_,_,_) -> % 'EXTENSIONMARK'
    undefined.

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



get_type_constructed(M,Typename,InnerType,D) when is_record(D,type) ->
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
    [get_type(M,Typename,H,no)|
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
	    {C#'ComponentType'.name,get_type(M,Typename,C,no)};
	CompList when is_list(CompList) ->
	    C = lists:nth(random(length(CompList)),CompList),
	    {C#'ComponentType'.name,get_type(M,Typename,C,no)}
    end.
    
get_sequence_of(M,Typename,Type,TypeSuffix) ->
    %% should generate length according to constraints later
    {_,Oftype} = Type#type.def,
    C = Type#type.constraint,
    S = size_random(C),
    NewTypeName = [TypeSuffix|Typename],
    gen_list(M,NewTypeName,Oftype,no,S).

gen_list(_,_,_,_,0) ->
    [];
gen_list(M,Typename,Oftype,Tellname,N) ->
    [get_type(M,Typename,Oftype,no)|gen_list(M,Typename,Oftype,Tellname,N-1)].
    
get_type_prim(D,Erule) ->
    C = D#type.constraint,
    case D#type.def of
	'INTEGER' ->
	    i_random(C);
	{'INTEGER',NamedNumberList} ->
	    NN = [X||{X,_} <- NamedNumberList],
	    case NN of 
		[] ->
		    i_random(C);
		_ ->
		    case C of
			[] ->
			    lists:nth(random(length(NN)),NN);
			_ ->
			    lists:nth((fun(0)->1;(X)->X end(i_random(C))),NN)
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
		    Bl1 =lists:reverse(adjust_list(size_random(C),[1,0,1,1])),
		    Bl2 = lists:reverse(lists:dropwhile(fun(0)->true;(1)->false end,Bl1)),
		    case {length(Bl2),get_constraint(C,'SizeConstraint')} of
			{Len,Len} ->
			    Bl2;
			{_Len,Int} when is_integer(Int) ->
			    Bl1;
			{Len,{Min,_}} when Min > Len ->
			    Bl1;
			_ ->
			    Bl2
		    end;
		_ ->
		    [lists:nth(random(length(NN)),NN)]
	    end;
	'ANY' ->
	    exit({asn1_error,nyi,'ANY'});
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
	    adjust_list(size_random(C),c_string(C,"OCTET STRING"));
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
	    case Erule of
		per ->
		    binary_to_list(Res);
		_ ->
		    Res
	    end;
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
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    random:uniform(Upper).

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


get_constraint(C,Key) ->
    case lists:keysearch(Key,1,C) of
	false ->
	     no;
	{value,{_,V}} -> 
	    V
    end.

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
    [4,9,111,112,101,110,95,116,121,112,101];
open_type_value(ber_bin) ->
    [4,9,111,112,101,110,95,116,121,112,101];
%    <<4,9,111,112,101,110,95,116,121,112,101>>;
open_type_value(ber_bin_v2) ->
    [4,9,111,112,101,110,95,116,121,112,101];
%    <<4,9,111,112,101,110,95,116,121,112,101>>;
open_type_value(per) ->
    "\n\topen_type"; %octet string value "open_type"
open_type_value(per_bin) ->
    "\n\topen_type";
%    <<10,9,111,112,101,110,95,116,121,112,101>>;
open_type_value(_) ->
    [4,9,111,112,101,110,95,116,121,112,101].

to_textual_order({Root,Ext}) ->
    {to_textual_order(Root),Ext};
to_textual_order(Cs) when is_list(Cs) ->
    case Cs of
	[#'ComponentType'{textual_order=undefined}|_] ->
	    Cs;
	_ ->
	    lists:keysort(#'ComponentType'.textual_order,Cs)
    end.
