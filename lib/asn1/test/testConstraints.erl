%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

-export([compile/3]).
-export([int_constraints/1,refed_NNL_name/1]).


-include_lib("test_server/include/test_server.hrl").



compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "Constraints",[Rules,{outdir,OutDir}]++Options).




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
    %%  Constraint Combinations (Duboisson p. 285)
    %%  I ::= INTEGER (0|15..269)
    %%==========================================================

    ?line {ok,Bytes12} = asn1_wrapper:encode('Constraints','I',0),
    ?line {ok,0} = asn1_wrapper:decode('Constraints','I',Bytes12),
    ?line {ok,Bytes13} = asn1_wrapper:encode('Constraints','I',20),
    ?line {ok,20} = asn1_wrapper:decode('Constraints','I',Bytes13),
    
    %%==========================================================
    %%  SIZE Constraint (Duboisson p. 268)
    %%  T ::=  IA5String (SIZE (1|2, ..., SIZE (1|2|3)))
    %%  T2 ::= IA5String (SIZE (1|2, ..., 3))
    %%==========================================================

    ?line {ok,Bytes14} = asn1_wrapper:encode('Constraints','T',"IA"),
    ?line {ok,"IA"} = asn1_wrapper:decode('Constraints','T',Bytes14),
    ?line {ok,Bytes15} = asn1_wrapper:encode('Constraints','T2',"IA"),
    ?line {ok,"IA"} = asn1_wrapper:decode('Constraints','T2',Bytes15).


refed_NNL_name(_Erule) ->
    ?line {ok,_} = asn1_wrapper:encode('Constraints','AnotherThing',fred),
    ?line {error,_Reason} = 
	asn1_wrapper:encode('Constraints','AnotherThing',fred3).
