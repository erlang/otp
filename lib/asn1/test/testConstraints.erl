%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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
-module(testConstraints).

-export([int_constraints/1,refed_NNL_name/1]).


-include_lib("test_server/include/test_server.hrl").

int_constraints(Rules) ->

    %%==========================================================
    %% SingleValue ::=  INTEGER (1) 
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('Constraints','SingleValue',1),
    ?line {ok,1} = asn1_wrapper:decode('Constraints','SingleValue',
					  lists:flatten(Bytes1)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber ->
		  ?line {ok,Bytes2} = 
		      asn1_wrapper:encode('Constraints','SingleValue',0),
		  ?line {error,{asn1,{integer_range,_,0}}} = 
		      asn1_wrapper:decode('Constraints','SingleValue',
					  lists:flatten(Bytes2)),
		  ?line {ok,Bytes3} = 
		      asn1_wrapper:encode('Constraints','SingleValue',1000),
		  ?line {error,{asn1,{integer_range,_,1000}}} = 
		      asn1_wrapper:decode('Constraints','SingleValue',
					  lists:flatten(Bytes3));
	      per ->
		  ?line {error,_Reason1} =
		      asn1_wrapper:encode('Constraints','SingleValue',0),
		  ?line {error,_Reason2} =
		      asn1_wrapper:encode('Constraints','SingleValue',1000)
	  end,

    %%==========================================================
    %% SingleValue2 ::=  INTEGER (1..20) 
    %%==========================================================

    ?line {ok,Bytes4} = asn1_wrapper:encode('Constraints','SingleValue2',1),
    ?line {ok,1} = asn1_wrapper:decode('Constraints','SingleValue2',
					  lists:flatten(Bytes4)),

    ?line {ok,Bytes5} = asn1_wrapper:encode('Constraints','SingleValue2',20),
    ?line {ok,20} = asn1_wrapper:decode('Constraints','SingleValue2',
					  lists:flatten(Bytes5)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber ->
		  ?line {ok,Bytes6} = 
		      asn1_wrapper:encode('Constraints','SingleValue2',0),
		  ?line {error,{asn1,{integer_range,{1,20},0}}} = 
		      asn1_wrapper:decode('Constraints','SingleValue2',
					  lists:flatten(Bytes6)),
		  ?line {ok,Bytes7} = 
		      asn1_wrapper:encode('Constraints','SingleValue2',21),
		  ?line {error,{asn1,{integer_range,{1,20},21}}} = 
		      asn1_wrapper:decode('Constraints','SingleValue2',
					  lists:flatten(Bytes7));
	      per ->
		  ?line {error,_Reason3} =
		      asn1_wrapper:encode('Constraints','SingleValue',0),
		  ?line {error,_Reason4} =
		      asn1_wrapper:encode('Constraints','SingleValue',1000)
	  end,

    %%==========================================================
    %% SingleValue3 ::=  INTEGER (Predefined | 5 | 10)
    %% Testcase for OTP-10139. A single value subtyping of an integer type
    %% where one value is predefined.
    %%==========================================================

    ?line {ok,BytesSV3} = asn1_wrapper:encode('Constraints','SingleValue3',1),
    ?line {ok,1} = asn1_wrapper:decode('Constraints','SingleValue3',
                                       lists:flatten(BytesSV3)),
    ?line {ok,BytesSV3_2} = asn1_wrapper:encode('Constraints','SingleValue3',5),
    ?line {ok,5} = asn1_wrapper:decode('Constraints','SingleValue3',
                                       lists:flatten(BytesSV3_2)),
    ?line {ok,BytesSV3_3} = asn1_wrapper:encode('Constraints','SingleValue3',10),
    ?line {ok,10} = asn1_wrapper:decode('Constraints','SingleValue3',
                                        lists:flatten(BytesSV3_3)),

    %%==========================================================
    %% Range2to19 ::=  INTEGER (1<..<20) 
    %%==========================================================

    ?line {ok,Bytes8} = asn1_wrapper:encode('Constraints','Range2to19',2),
    ?line {ok,2} = asn1_wrapper:decode('Constraints','Range2to19',lists:flatten(Bytes8)),

    ?line {ok,Bytes9} = asn1_wrapper:encode('Constraints','Range2to19',19),
    ?line {ok,19} = asn1_wrapper:decode('Constraints','Range2to19',lists:flatten(Bytes9)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,Bytes10} = 
		      asn1_wrapper:encode('Constraints','Range2to19',1),
		  ?line {error,{asn1,{integer_range,{2,19},1}}} = 
		      asn1_wrapper:decode('Constraints','Range2to19',
					  lists:flatten(Bytes10)),
		  ?line {ok,Bytes11} = 
		      asn1_wrapper:encode('Constraints','Range2to19',20),
		  ?line {error,{asn1,{integer_range,{2,19},20}}} = 
		      asn1_wrapper:decode('Constraints','Range2to19',
					  lists:flatten(Bytes11));
	      per ->
		  ?line {error,_Reason5} =
		      asn1_wrapper:encode('Constraints','Range2to19',1),
		  ?line {error,_Reason6} =
		      asn1_wrapper:encode('Constraints','Range2to19',20)
	  end,

    %%==========================================================
    %% Tests for Range above 16^4 up to maximum supported by asn1 assuming the
    %% octet length field is encoded on max 8 bits
    %%==========================================================
    LastNumWithoutLengthEncoding = 65536,
    ?line {ok,BytesFoo} = asn1_wrapper:encode('Constraints','Range256to65536',
                                              LastNumWithoutLengthEncoding),
    ?line {ok,LastNumWithoutLengthEncoding} = 
        asn1_wrapper:decode('Constraints','Range256to65536',lists:flatten(BytesFoo)),

    FirstNumWithLengthEncoding = 65537,
    ?line {ok,BytesBar} = asn1_wrapper:encode('LargeConstraints','RangeMax',
                                              FirstNumWithLengthEncoding),
    ?line {ok,FirstNumWithLengthEncoding} =
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesBar)),

    FirstNumOver16_6 = 16777217,
    ?line {ok, BytesBaz} =
        asn1_wrapper:encode('LargeConstraints','RangeMax', FirstNumOver16_6),
    ?line {ok, FirstNumOver16_6} = 
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesBaz)),
    
    FirstNumOver16_8 = 4294967297,
    ?line {ok, BytesQux} =
        asn1_wrapper:encode('LargeConstraints','RangeMax', FirstNumOver16_8),
    ?line {ok, FirstNumOver16_8} = 
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesQux)),

    FirstNumOver16_10 = 1099511627776,
    ?line {ok, BytesBur} =
        asn1_wrapper:encode('LargeConstraints','RangeMax', FirstNumOver16_10),
    ?line {ok, FirstNumOver16_10} = 
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesBur)),

    FirstNumOver16_10 = 1099511627776,
    ?line {ok, BytesBur} =
        asn1_wrapper:encode('LargeConstraints','RangeMax', FirstNumOver16_10),
    ?line {ok, FirstNumOver16_10} =
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesBur)),

    HalfMax = 1 bsl (128*8),
    ?line {ok, BytesHalfMax} =
        asn1_wrapper:encode('LargeConstraints','RangeMax', HalfMax),
    ?line {ok, HalfMax} =
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesHalfMax)),

    Max = 1 bsl (255*8),
    ?line {ok, BytesMax} =
        asn1_wrapper:encode('LargeConstraints','RangeMax', Max),
    ?line {ok, Max} =
        asn1_wrapper:decode('LargeConstraints','RangeMax',lists:flatten(BytesMax)),
    
    %% Random number within longlong range
    LongLong = 12672809400538808320,
    ?line {ok, BytesLongLong} =
        asn1_wrapper:encode('Constraints','LongLong', LongLong),
    ?line {ok, LongLong} =
        asn1_wrapper:decode('Constraints','LongLong',lists:flatten(BytesLongLong)),

    %%==========================================================
    %%  Constraint Combinations (Duboisson p. 285)
    %%  I ::= INTEGER (0|15..269)
    %%==========================================================

    ?line {ok,Bytes12} = asn1_wrapper:encode('Constraints','I',0),
    ?line {ok,0} = asn1_wrapper:decode('Constraints','I',Bytes12),
    ?line {ok,Bytes13} = asn1_wrapper:encode('Constraints','I',20),
    ?line {ok,20} = asn1_wrapper:decode('Constraints','I',Bytes13),

    %%==========================================================
    %%  Constraint Combinations (Duboisson p. 285)
    %%  X1 ::= INTEGER (1..4|8|10|20)
    %%==========================================================

    ?line {ok,Bytes14} = asn1_wrapper:encode('Constraints','X1',1),
    ?line {ok,1} = asn1_wrapper:decode('Constraints','X1',Bytes14),
    ?line {ok,Bytes15} = asn1_wrapper:encode('Constraints','X1',20),
    ?line {ok,20} = asn1_wrapper:decode('Constraints','X1',Bytes15),   
    %%==========================================================
    %%  SIZE Constraint (Duboisson p. 268)
    %%  T ::=  IA5String (SIZE (1|2, ..., SIZE (1|2|3)))
    %%  T2 ::= IA5String (SIZE (1|2, ..., 3))
    %%==========================================================

    ?line {ok,Bytes16} = asn1_wrapper:encode('Constraints','T',"IA"),
    ?line {ok,"IA"} = asn1_wrapper:decode('Constraints','T',Bytes16),
    ?line {ok,Bytes17} = asn1_wrapper:encode('Constraints','T2',"IA"),
    ?line {ok,"IA"} = asn1_wrapper:decode('Constraints','T2',Bytes17).


refed_NNL_name(_Erule) ->
    ?line {ok,_} = asn1_wrapper:encode('Constraints','AnotherThing',fred),
    ?line {error,_Reason} = 
	asn1_wrapper:encode('Constraints','AnotherThing',fred3).
