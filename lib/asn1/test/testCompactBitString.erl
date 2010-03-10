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
-module(testCompactBitString).

-export([compile/3]).
-export([compact_bit_string/1, bit_string_unnamed/1,otp_4869/1,
	 ticket_7734/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Option) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "PrimStrings",
			      [Rules,{outdir,OutDir}]++Option),
    case Rules of
	per_bin ->
	    ?line ok = asn1ct:compile(DataDir ++ "Constraints",
				      [Rules,{outdir,OutDir}]++Option);
	_ ->  ok
    end.



compact_bit_string(Rules) ->

    %%==========================================================
    %% Bs1 ::= BIT STRING
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('PrimStrings','Bs1',0),
    ?line {ok,{0,<<>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes1)),
    
    ?line {ok,Bytes2} = asn1_wrapper:encode('PrimStrings','Bs1',4),
    ?line {ok,{5,<<32>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes2)),
    
    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','Bs1',15),
    ?line {ok,{4,<<240>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',
			    lists:flatten(Bytes3)),
    
    ?line {ok,Bytes4} = asn1_wrapper:encode('PrimStrings','Bs1',255),
    ?line {ok,{0,<<255>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes4)),
	    
    ?line {ok,Bytes5} = asn1_wrapper:encode('PrimStrings','Bs1',256),
    ?line {ok,{7,<<0,128>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes5)),
    
    ?line {ok,Bytes6} = asn1_wrapper:encode('PrimStrings','Bs1',257),
    ?line {ok,{7,<<128,128>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',
			    lists:flatten(Bytes6)),
    
    ?line {ok,Bytes7} = asn1_wrapper:encode('PrimStrings','Bs1',444),
    ?line {ok,{7,<<61,128>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',
			    lists:flatten(Bytes7)),
    
    ?line {ok,Bytes8} = asn1_wrapper:encode('PrimStrings','Bs1',
					    12345678901234567890),
    ?line {ok,_} = asn1_wrapper:decode('PrimStrings','Bs1',
				       lists:flatten(Bytes8)),
    
%% Removed due to beam cannot handle this big integers    
%%    Bs1_1 = 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
%%    ?line {ok,Bytes9} = asn1_wrapper:encode('PrimStrings','Bs1',Bs1_1),
%%    ?line {ok,_} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes9)),
    
%%    Bs1_2 = 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
%%    ?line {ok,Bytes10} = asn1_wrapper:encode('PrimStrings','Bs1',Bs1_2),
%%    ?line {ok,_} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes10)),
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('PrimStrings','Bs1',
					     [1,1,1,1,1,1,1,1]),
    ?line {ok,{0,<<255>>}} = asn1_wrapper:decode('PrimStrings','Bs1',
						 lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('PrimStrings',
					     'Bs1',
					     [0,1,0,0,1,0]),
    ?line {ok,{2,<<72>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',
			    lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('PrimStrings', 'Bs1',
			    [1,0,0,0,0,0,0,0,0]),
    ?line {ok,{7,<<128,0>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',
			    lists:flatten(Bytes13)),
    
    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('PrimStrings','Bs1',
			    [0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
    ?line {ok,{5,<<75,226,96>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes14)),
    
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line Bytes15 = [35,8,3,2,0,73,3,2,4,32],
		  ?line {ok,{4,<<73,32>>}} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',
					  lists:flatten(Bytes15)),
		  
		  ?line Bytes16 = [35,9,3,2,0,234,3,3,7,156,0],
		  ?line {ok,{7,<<234,156,0>>}} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',
					  lists:flatten(Bytes16)),
		  
		  ?line Bytes17 = [35,128,3,2,0,73,3,2,4,32,0,0],
		  ?line {ok,{4,<<73,32>>}} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',
					  lists:flatten(Bytes17)),
		  
		  ?line Bytes18 = [35,128,3,2,0,234,3,3,7,156,0,0,0],
		  ?line {ok,{7,<<234,156,0>>}} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',
					  lists:flatten(Bytes18)),
		  ok;
	      
	      per ->
		  ok
	  end,
    
    %% The following case to test OTP-4200
     ?line {ok,Bytes19} = 
	asn1_wrapper:encode('PrimStrings','Bs1',{0,<<0,0,1,1>>}),
    ?line {ok,{0,<<0,0,1,1>>}} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes19)),

    %%==========================================================
    %% Bs2 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (7))
    %%==========================================================
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('PrimStrings','Bs2',[mo,tu,fr]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('PrimStrings','Bs2',[0,1,1,0,0,1,0]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs2',lists:flatten(Bytes22)),
    
    % ?line case asn1_wrapper:erule(Rules) of
% 	      ber -> 
% 		  ?line {ok,[mo,tu,fr,su,mo,th]} = 
% 		      asn1_wrapper:decode('PrimStrings','Bs2',[35,8,3,2,1,100,3,2,2,200]),
		  
% 		  ?line {ok,[mo,tu,fr,su,mo,th]} = 
% 		      asn1_wrapper:decode('PrimStrings','Bs2',[35,128,3,2,1,100,3,2,2,200,0,0]),
% 		  ok;
	      
% 	      per ->
% 		  ok
% 	  end,
    
    
    
    %%==========================================================
    %% Bs3 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (1..7))
    %%==========================================================
    
    ?line {ok,Bytes31} = asn1_wrapper:encode('PrimStrings','Bs3',[mo,tu,fr]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs3',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('PrimStrings','Bs3',[0,1,1,0,0,1,0]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs3',lists:flatten(Bytes32)),
    
    
    
    %%==========================================================
    %% BsPri ::= [PRIVATE 61] BIT STRING
    %%==========================================================
    
    ?line {ok,Bytes41} = asn1_wrapper:encode('PrimStrings','BsPri',45),
    ?line {ok,{2,<<180>>}} = 
	asn1_wrapper:decode('PrimStrings','BsPri',lists:flatten(Bytes41)),
    
    ?line {ok,Bytes42} = asn1_wrapper:encode('PrimStrings','BsPri',211),
    ?line {ok,{0,<<203>>}} = 
	asn1_wrapper:decode('PrimStrings','BsPri',lists:flatten(Bytes42)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,{5,<<75,226,96>>}} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',
					  [223,61,4,5,75,226,96]),
		  
		  ?line {ok,{5,<<75,226,96>>}} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',
					  [255,61,128,3,4,5,75,226,96,0,0]),
		  
		  ?line {ok,{5,<<75,226,96>>}} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',
					  [255,61,9,3,2,0,75,3,3,5,226,96]),
		  
		  ?line {ok,{5,<<75,226,96>>}} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',
					  [255,61,128,3,2,0,75,3,3,5,226,96,0,0]),
		  ok;
	      
	      per ->
		  ok
	  end,
    
    
    
    %%==========================================================
    %% BsExpPri ::= [PRIVATE 61] EXPLICIT BIT STRING
    %%==========================================================
    
    ?line {ok,Bytes51} = asn1_wrapper:encode('PrimStrings','BsExpPri',45),
    ?line {ok,{2,<<180>>}} = 
	asn1_wrapper:decode('PrimStrings','BsExpPri',lists:flatten(Bytes51)),
    
    ?line {ok,Bytes52} = asn1_wrapper:encode('PrimStrings','BsExpPri',211),
    ?line {ok,{0,<<203>>}} = 
	asn1_wrapper:decode('PrimStrings','BsExpPri',lists:flatten(Bytes52)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,{5,<<75,226,96>>}} = 
		      asn1_wrapper:decode('PrimStrings','BsExpPri',[255,61,6,3,4,5,75,226,96]),
		  ok;
	      
	      per ->
		  ok
	  end,
    
    ok.

ticket_7734(per_bin) ->
    ?line BS = {0,list_to_binary(lists:duplicate(128,0))},
    ?line {ok,BSEnc} = asn1_wrapper:encode('PrimStrings','BS1024',BS),
    ?line {ok,BS} = asn1_wrapper:decode('PrimStrings','BS1024',BSEnc).

bit_string_unnamed(Rules) ->
    case asn1_wrapper:erule(Rules) of
	ber ->
	    ok;
	per ->
	    ?line {ok,Bytes1} = 
		asn1_wrapper:encode('PrimStrings','TransportLayerAddress',
				    [0,1,1,0]),
	    ?line {ok,{4,<<96>>}} = 
		asn1_wrapper:decode('PrimStrings','TransportLayerAddress',
				    lists:flatten(Bytes1))
    end.

otp_4869(per_bin) ->
    ?line Val1={'IP',[0],{0,<<62,235,90,50,0,0,0,0,0,0,0,0,0,0,0,0>>},asn1_NOVALUE},
    ?line Val2 = {'IP',[0],[0,0,1,1,1,1,1,0,1,1,1,0,1,0,1,1,0,1,0,1,1,0,1,0,0,0,1,1,0,0,1,0] ++ lists:duplicate(128 - 32,0),asn1_NOVALUE},

    ?line {ok,Bytes1} = asn1_wrapper:encode('Constraints','IP',Val1),
    ?line {ok,Bytes1} = asn1_wrapper:encode('Constraints','IP',Val2);
otp_4869(_) ->
    ok.
