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
-module(testPrimStrings).

-export([compile/3]).
-export([bit_string/1]).
-export([bit_string_unnamed/1]).
-export([octet_string/1]).
-export([numeric_string/1]).
-export([other_strings/1]).
-export([more_strings/1]).
-export([universal_string/1]).
-export([bmp_string/1]).
-export([times/1]).
-export([utf8_string/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Option) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "PrimStrings",
			      [Rules,{outdir,OutDir}]++Option),
    ?line {ok,IO} = file:open(test_config,write),
    io:format(IO,"~p.~n",[Config]),
    file:close(IO),
    ?line ok = asn1ct:compile(DataDir ++ "BitStr",
			      [Rules, {outdir,OutDir}]++Option).



bit_string(Rules) ->
    
    %%==========================================================
    %% Bs1 ::= BIT STRING
    %%==========================================================
    
    ?line {ok,Bytes1} = asn1_wrapper:encode('PrimStrings','Bs1',0),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes1)),
    
    ?line {ok,Bytes2} = asn1_wrapper:encode('PrimStrings','Bs1',4),
    ?line {ok,[0,0,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes2)),
    
    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','Bs1',15),
    ?line {ok,[1,1,1,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes3)),
    
    ?line {ok,Bytes4} = asn1_wrapper:encode('PrimStrings','Bs1',255),
    ?line {ok,[1,1,1,1,1,1,1,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes4)),
    
    ?line {ok,Bytes5} = asn1_wrapper:encode('PrimStrings','Bs1',256),
    ?line {ok,[0,0,0,0,0,0,0,0,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes5)),
    
    ?line {ok,Bytes6} = asn1_wrapper:encode('PrimStrings','Bs1',257),
    ?line {ok,[1,0,0,0,0,0,0,0,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes6)),
    
    ?line {ok,Bytes7} = asn1_wrapper:encode('PrimStrings','Bs1',444),
    ?line {ok,[0,0,1,1,1,1,0,1,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes7)),
    
    ?line {ok,Bytes8} = asn1_wrapper:encode('PrimStrings','Bs1',12345678901234567890),
    ?line {ok,_} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes8)),
    
%% Removed due to beam cannot handle this big integers    
%%    Bs1_1 = 123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
%%    ?line {ok,Bytes9} = asn1_wrapper:encode('PrimStrings','Bs1',Bs1_1),
%%    ?line {ok,_} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes9)),
    
%%    Bs1_2 = 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
%%    ?line {ok,Bytes10} = asn1_wrapper:encode('PrimStrings','Bs1',Bs1_2),
%%    ?line {ok,_} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes10)),
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('PrimStrings','Bs1',[1,1,1,1,1,1,1,1]),
    ?line {ok,[1,1,1,1,1,1,1,1]} = asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes11)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,Bytes12} = asn1_wrapper:encode('PrimStrings','Bs1',[0,1,0,0,1,0]),
		  ?line {ok,[0,1,0,0,1,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes12)),
		  
		  ?line {ok,Bytes13} = asn1_wrapper:encode('PrimStrings','Bs1',[1,0,0,0,0,0,0,0,0]),
		  ?line {ok,[1,0,0,0,0,0,0,0,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes13)),
		  ok;
	      per ->
		  ?line {ok,Bytes12} = asn1_wrapper:encode('PrimStrings','Bs1',[0,1,0,0,1,0]),
		  ?line {ok,[0,1,0,0,1,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes12)),
		  
		  ?line {ok,Bytes13} = asn1_wrapper:encode('PrimStrings','Bs1',[1,0,0,0,0,0,0,0,0]),
		  ?line {ok,[1,0,0,0,0,0,0,0,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes13)),
		  ok
	  end,
    
    ?line {ok,Bytes14} = 
	asn1_wrapper:encode('PrimStrings','Bs1',[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]),
    ?line {ok,[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]} = 
	asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes14)),
    
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line Bytes15 = [35,8,3,2,0,73,3,2,4,32],
		  ?line {ok,[0,1,0,0,1,0,0,1,0,0,1,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes15)),
		  
		  ?line Bytes16 = [35,9,3,2,0,234,3,3,7,156,0],
		  ?line {ok,[1,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes16)),
		  
		  ?line Bytes17 = [35,128,3,2,0,73,3,2,4,32,0,0],
		  ?line {ok,[0,1,0,0,1,0,0,1,0,0,1,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes17)),
		  
		  ?line Bytes18 = [35,128,3,2,0,234,3,3,7,156,0,0,0],
		  ?line {ok,[1,1,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0]} = 
		      asn1_wrapper:decode('PrimStrings','Bs1',lists:flatten(Bytes18)),
		  ok;
	      
	      per ->
		  ok
	  end,
    

    
    %%==========================================================
    %% Bs2 ::= BIT STRING {su(0), mo(1), tu(2), we(3), th(4), fr(5), sa(6) } (SIZE (7))
    %%==========================================================
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('PrimStrings','Bs2',[mo,tu,fr]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs2',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('PrimStrings','Bs2',[0,1,1,0,0,1,0]),
    ?line {ok,[mo,tu,fr]} = asn1_wrapper:decode('PrimStrings','Bs2',lists:flatten(Bytes22)),
    ok,
%% skip this because it is wrong
%     ?line case asn1_wrapper:erule(Rules) of
% 	      ber -> 
% 		  ?line {ok,[mo,tu,fr,su,mo,th]} = 
% 		      asn1_wrapper:decode('PrimStrings','Bs2',[35,8,3,2,0,101,3,2,2,200]),
		  
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
    %% Bs7 ::= BIT STRING (SIZE (24))
    %%==========================================================

    ?line {ok,Bytes33} = asn1_wrapper:encode('PrimStrings','Bs7',53245),
    ?line {ok,[1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,0,0]} =
	asn1_wrapper:decode('PrimStrings','Bs7',Bytes33),

    ?line {ok,Bytes34} = asn1_wrapper:encode('PrimStrings','Bs7',[1,0,1,0]),
    ?line {ok,[1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]} =
	asn1_wrapper:decode('PrimStrings','Bs7',Bytes34),
    
    %%==========================================================
    %% BsPri ::= [PRIVATE 61] BIT STRING
    %%==========================================================
    
    ?line {ok,Bytes41} = asn1_wrapper:encode('PrimStrings','BsPri',45),
    ?line {ok,[1,0,1,1,0,1]} = asn1_wrapper:decode('PrimStrings','BsPri',lists:flatten(Bytes41)),
    
    ?line {ok,Bytes42} = asn1_wrapper:encode('PrimStrings','BsPri',211),
    ?line {ok,[1,1,0,0,1,0,1,1]} = asn1_wrapper:decode('PrimStrings','BsPri',lists:flatten(Bytes42)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',[223,61,4,5,75,226,96]),
		  
		  ?line {ok,[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',[255,61,128,3,4,5,75,226,96,0,0]),
		  
		  ?line {ok,[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',[255,61,9,3,2,0,75,3,3,5,226,96]),
		  
		  ?line {ok,[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]} = 
		      asn1_wrapper:decode('PrimStrings','BsPri',[255,61,128,3,2,0,75,3,3,5,226,96,0,0]),
		  ok;
	      
	      per ->
		  ok
	  end,
    
    
    
    %%==========================================================
    %% BsExpPri ::= [PRIVATE 61] EXPLICIT BIT STRING
    %%==========================================================
    
    ?line {ok,Bytes51} = asn1_wrapper:encode('PrimStrings','BsExpPri',45),
    ?line {ok,[1,0,1,1,0,1]} = 
	asn1_wrapper:decode('PrimStrings','BsExpPri',lists:flatten(Bytes51)),
    
    ?line {ok,Bytes52} = asn1_wrapper:encode('PrimStrings','BsExpPri',211),
    ?line {ok,[1,1,0,0,1,0,1,1]} = 
	asn1_wrapper:decode('PrimStrings','BsExpPri',lists:flatten(Bytes52)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,[0,1,0,0,1,0,1,1,1,1,1,0,0,0,1,0,0,1,1]} = 
		      asn1_wrapper:decode('PrimStrings','BsExpPri',[255,61,6,3,4,5,75,226,96]),
		  ok;
	      
	      per ->
		  ok
	  end,
    
    %%==========================================================
    %% TestS ::= BIT STRING {a(0),b(1)} (SIZE (3..8)), test case for OTP-4353
    %%==========================================================

    ?line {ok,Bytes53} = asn1_wrapper:encode('PrimStrings','TestS',[a]),
    ?line {ok,[a]} = 
	asn1_wrapper:decode('PrimStrings','TestS',lists:flatten(Bytes53)),

    %%==========================================================
    %% PersonalStatus ::= BIT STRING {married(0),employed(1),
    %%    veteran(2), collegeGraduate(3)}, test case for OTP-5710
    %%==========================================================
    
    ?line {ok,Bytes54} = asn1_wrapper:encode('BitStr','PersonalStatus',[]),
    ?line {ok,[]} = asn1_wrapper:decode('BitStr','PersonalStatus',Bytes54),
    
    %%==========================================================
    %% BS5932 ::= BIT STRING (SIZE (5..MAX))
    %% test case for OTP-5932
    %%==========================================================
    case asn1_wrapper:erule(Rules) of
	ber ->
	    ?line {error,_} = asn1_wrapper:encode('PrimStrings','BSMAX',
						  [1,0,1]),
	    ?line {ok,Bytes55} = 
		asn1_wrapper:encode('PrimStrings','BSMAX',[1,0,1,0,1]),
	    ?line {ok,[1,0,1,0,1]} = 
		asn1_wrapper:decode('PrimStrings','BSMAX',Bytes55);
	_ ->
	    ok
    end,
    
    %%==========================================================
    %% BS255 ::= BIT STRING (SIZE (255))
    %% BS256 ::= BIT STRING (SIZE (256))
    %% BS1024 ::= BIT STRING (SIZE (1024))
    %% test case for OTP-7602
    %%==========================================================
    BSmaker =
	fun(_F,S,S,_,Acc) -> 
		Acc;
	   (F,Ix,S,{A,B},Acc) -> 
		F(F,Ix+1,S,{B,A},[A|Acc]) 
	end,
    
    BSList255 = BSmaker(BSmaker,0,255,{1,0},[]),
    BSList256 = BSmaker(BSmaker,0,256,{1,0},[]),
    BSList1024 = BSmaker(BSmaker,0,1024,{1,0},[]),
    ?line {ok,Bytes56} = 
	asn1_wrapper:encode('PrimStrings','BS255',BSList255),
    ?line {ok,BSList255} = 
	asn1_wrapper:decode('PrimStrings','BS255',Bytes56),
    ?line {ok,Bytes57} = 
	asn1_wrapper:encode('PrimStrings','BS256',BSList256),
    ?line {ok,BSList256} = 
	asn1_wrapper:decode('PrimStrings','BS256',Bytes57),
    ?line {ok,Bytes58} = 
	asn1_wrapper:encode('PrimStrings','BS1024',BSList1024),
    ?line {ok,BSList1024} = 
	asn1_wrapper:decode('PrimStrings','BS1024',Bytes58).
    
	    

bit_string_unnamed(Rules) ->
    case asn1_wrapper:erule(Rules) of
	ber ->
	    ok;
	per ->
	    ?line {ok,Bytes1} = 
		case catch asn1_wrapper:encode('PrimStrings','TransportLayerAddress',[0,1,1,0]) of
		    Ret = {ok,_} -> Ret;
		    Err ->
			Config = file:consult(test_config),
			?line OutDir = ?config(priv_dir,Config),
			MyOut = "/home/bertil/daily_build",
			file:copy(filename:join([OutDir,"PrimStrings.erl"]),
				  filename:join([MyOut,"PrimStrings.erl"])),
			file:copy(filename:join([OutDir,"PrimStrings.beam"]),
				  filename:join([MyOut,"PrimStrings.beam"])),
			file:copy(code:which(asn1rt_per_v1),
				  filename:join([MyOut,"asn1rt_per_v1.beam"])),
			file:copy(filename:join([code:lib_dir(asn1),src,"asn1rt_per_v1.erl"]),filename:join([MyOut,"asn1rt_per_v1.erl"])),
			io:format("Err: ~p~n",[Err]),
			Err
		end,
	    ?line {ok,[0,1,1,0]} = asn1_wrapper:decode('PrimStrings','TransportLayerAddress',lists:flatten(Bytes1))
    end.

octet_string(Rules) ->

    %%==========================================================
    %% Os ::= OCTET STRING
    %%==========================================================

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,"Jones"} = 
		      asn1_wrapper:decode('PrimStrings','Os',[4,5,16#4A,16#6F,16#6E,16#65,16#73]),

		  ?line {ok,"Jones"} = 
		      asn1_wrapper:decode('PrimStrings','Os',[36,9,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73]),
		  
		  ?line {ok,"Jones"} = 
		      asn1_wrapper:decode('PrimStrings','Os',[36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0]),
		  ok;
	      
	      per ->
		  ok
	  end,


    
    ?line {ok,Bytes4} = 
	asn1_wrapper:encode('PrimStrings','Os',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','Os',lists:flatten(Bytes4)),

    ?line {ok,Bytes5} = 
	asn1_wrapper:encode('PrimStrings','OsCon',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','OsCon',lists:flatten(Bytes5)),

    ?line {ok,Bytes6} = 
	asn1_wrapper:encode('PrimStrings','OsPri',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','OsPri',lists:flatten(Bytes6)),

    ?line {ok,Bytes7} = 
	asn1_wrapper:encode('PrimStrings','OsApp',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','OsApp',lists:flatten(Bytes7)),

    ?line {ok,Bytes8} = 
	asn1_wrapper:encode('PrimStrings','OsExpCon',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','OsExpCon',lists:flatten(Bytes8)),

    ?line {ok,Bytes9} = 
	asn1_wrapper:encode('PrimStrings','OsExpPri',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','OsExpPri',lists:flatten(Bytes9)),

    ?line {ok,Bytes10} = 
	asn1_wrapper:encode('PrimStrings','OsExpApp',[47,23,99,255,1]),
    ?line {ok,[47,23,99,255,1]} = asn1_wrapper:decode('PrimStrings','OsExpApp',lists:flatten(Bytes10)),

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('PrimStrings','Os',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Os',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('PrimStrings','OsApp',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','OsApp',lists:flatten(Bytes12)),

    ?line {ok,Bytes13} = 
	asn1_wrapper:encode('PrimStrings','OsExpApp',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','OsExpApp',lists:flatten(Bytes13)),






    
    OsR = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",

    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('PrimStrings','Os',OsR),
    ?line {ok,Os1} = asn1_wrapper:decode('PrimStrings','Os',lists:flatten(Bytes21)),
    ?line Os1 = OsR,
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('PrimStrings','OsCon',OsR),
    ?line {ok,Os2} = asn1_wrapper:decode('PrimStrings','OsCon',lists:flatten(Bytes22)),
    ?line Os2 = OsR,
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('PrimStrings','OsExpApp',OsR),
    ?line {ok,Os3} = asn1_wrapper:decode('PrimStrings','OsExpApp',lists:flatten(Bytes23)),
    ?line Os3 = OsR,
    


    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,7,4,5,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,11,36,9,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,13,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,128,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,0,0]),
		  ?line {ok,"JonesJones"} = asn1_wrapper:decode('PrimStrings','OsExpApp',[127,62,128,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,36,128,4,3,16#4A,16#6F,16#6E,4,2,16#65,16#73,0,0,0,0]),
		  ok;
	      
	      per ->
		  ok
	  end,


    ok.
    



		       
numeric_string(Rules) ->

    %%==========================================================
    %% Ns ::= NumericString
    %%==========================================================

    ?line {ok,BytesNs2} = asn1_wrapper:encode('PrimStrings','Ns',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Ns',lists:flatten(BytesNs2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,BytesNs1} = asn1_wrapper:encode('PrimStrings','Ns',[48,49,32,51,52]),
		  ?line {ok,[48,49,32,51,52]} = asn1_wrapper:decode('PrimStrings','Ns',lists:flatten(BytesNs1)),

		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','Ns',[16#12,5,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','Ns',[16#32,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','Ns',[16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0]),
		  ok;
	      
	      per ->
		  ?line {ok,BytesNs1} = asn1_wrapper:encode('PrimStrings','Ns',[48,49,32,51,52]),
		  ?line {ok,"01 34"} = asn1_wrapper:decode('PrimStrings','Ns',lists:flatten(BytesNs1)),
		  ok
	  end,

    

		       
    %%==========================================================
    %% NsCon ::= [70] NumericString
    %%==========================================================

    ?line {ok,BytesNs12} = asn1_wrapper:encode('PrimStrings','NsCon',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','NsCon',lists:flatten(BytesNs12)),

    

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,BytesNs11} = asn1_wrapper:encode('PrimStrings','NsCon',[48,49,32,51,52]),
		  ?line {ok,[48,49,32,51,52]} = asn1_wrapper:decode('PrimStrings','NsCon',lists:flatten(BytesNs11)),

		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsCon',[16#9F,16#46,5,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsCon',[16#BF,16#46,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsCon',[16#BF,16#46,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0]),
		  ok;
	      
	      per ->
		  ?line {ok,BytesNs11} = asn1_wrapper:encode('PrimStrings','NsCon',[48,49,32,51,52]),
		  ?line {ok,"01 34"} = asn1_wrapper:decode('PrimStrings','NsCon',lists:flatten(BytesNs11)),
		  ok
	  end,


		       
    %%==========================================================
    %% NsExpCon ::= [71] EXPLICIT NumericString
    %%==========================================================

    ?line {ok,BytesNs22} = asn1_wrapper:encode('PrimStrings','NsExpCon',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','NsExpCon',lists:flatten(BytesNs22)),

    

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,BytesNs21} = asn1_wrapper:encode('PrimStrings','NsExpCon',[48,49,32,51,52]),
		  ?line {ok,[48,49,32,51,52]} = asn1_wrapper:decode('PrimStrings','NsExpCon',lists:flatten(BytesNs21)),

		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,16#07,16#12,16#05,16#4A,16#6F,16#6E,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,11,16#32,9,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73]),
		  ?line {ok,"Jones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,128,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,0,0]),
		  ?line {ok,"JonesJones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,26,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0]),
		  ?line {ok,"JonesJones"} = asn1_wrapper:decode('PrimStrings','NsExpCon',[16#BF,16#47,128,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,16#32,128,18,3,16#4A,16#6F,16#6E,18,2,16#65,16#73,0,0,0,0]),
		  ok;
	      
	      per ->
		  ?line {ok,BytesNs21} = asn1_wrapper:encode('PrimStrings','NsExpCon',[48,49,32,51,52]),
		  ?line {ok,"01 34"} = asn1_wrapper:decode('PrimStrings','NsExpCon',lists:flatten(BytesNs21)),
		  ok
	  end,

    ok.




		       
other_strings(_Rules) ->

    %%==========================================================
    %% Ps ::= PrintableString
    %%==========================================================

    ?line {ok,BytesPs1} = asn1_wrapper:encode('PrimStrings','Ps',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','Ps',lists:flatten(BytesPs1)),

    ?line {ok,BytesPs2} = asn1_wrapper:encode('PrimStrings','Ps',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Ps',lists:flatten(BytesPs2)),

    
		       
    %%==========================================================
    %% Vis ::= VisibleString
    %%==========================================================

    ?line {ok,BytesVis1} = asn1_wrapper:encode('PrimStrings','Vis',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','Vis',lists:flatten(BytesVis1)),

    ?line {ok,BytesVis2} = asn1_wrapper:encode('PrimStrings','Vis',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Vis',lists:flatten(BytesVis2)),
    

		       
    %%==========================================================
    %% IA5 ::= IA5String
    %%==========================================================

    ?line {ok,BytesIA51} = asn1_wrapper:encode('PrimStrings','IA5',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','IA5',lists:flatten(BytesIA51)),

    ?line {ok,BytesIA52} = asn1_wrapper:encode('PrimStrings','IA5',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','IA5',lists:flatten(BytesIA52)),

    
    IA5_1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",

    ?line {ok,BytesIA53} = asn1_wrapper:encode('PrimStrings','IA5',IA5_1),
    ?line {ok,IA5_1r} = asn1_wrapper:decode('PrimStrings','IA5',lists:flatten(BytesIA53)),
    ?line IA5_1 = IA5_1r,

		       
    

    ok.


more_strings(_Rules) ->
    %%==========================================================
    %% Ts ::= TeletexString
    %%==========================================================

    ?line {ok,BytesTs1} = asn1_wrapper:encode('PrimStrings','Ts',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','Ts',lists:flatten(BytesTs1)),
    
    ?line {ok,BytesTs2} = asn1_wrapper:encode('PrimStrings','Ts',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Ts',lists:flatten(BytesTs2)),


		       
    %%==========================================================
    %% Vxs ::= VideotexString
    %%==========================================================

    ?line {ok,BytesVxs1} = asn1_wrapper:encode('PrimStrings','Vxs',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','Vxs',lists:flatten(BytesVxs1)),
    
    ?line {ok,BytesVxs2} = asn1_wrapper:encode('PrimStrings','Vxs',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Vxs',lists:flatten(BytesVxs2)),


    
    %%==========================================================
    %% Grs ::= GraphicString
    %%==========================================================

    ?line {ok,BytesGrs1} = asn1_wrapper:encode('PrimStrings','Grs',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','Grs',lists:flatten(BytesGrs1)),
		  
    ?line {ok,BytesGrs2} = asn1_wrapper:encode('PrimStrings','Grs',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Grs',lists:flatten(BytesGrs2)),
    

    %%==========================================================
    %% ODesc ::= ObjectDescriptor, test case for OTP-4161
    %%==========================================================

    ?line {ok,BytesODesc1} = asn1_wrapper:encode('PrimStrings','ODesc',[79,98,106,101,99,116,68,101,115,99,114,105,112,116,111,114]),
    ?line {ok,[79,98,106,101,99,116,68,101,115,99,114,105,112,116,111,114]} = 
	asn1_wrapper:decode('PrimStrings','ODesc',lists:flatten(BytesODesc1)),
		  
    ?line {ok,BytesODesc2} = asn1_wrapper:encode('PrimStrings','ODesc',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','ODesc',lists:flatten(BytesODesc2)),
		       
    %%==========================================================
    %% Ges ::= GeneralString
    %%==========================================================

    ?line {ok,BytesGes1} = asn1_wrapper:encode('PrimStrings','Ges',[47,23,99,75,47]),
    ?line {ok,[47,23,99,75,47]} = 
	asn1_wrapper:decode('PrimStrings','Ges',lists:flatten(BytesGes1)),
    
    ?line {ok,BytesGes2} = asn1_wrapper:encode('PrimStrings','Ges',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Ges',lists:flatten(BytesGes2)),
    
    ok.


		       
universal_string(Rules) ->
    
    
    %%==========================================================
    %% Us ::= UniversalString
    %%==========================================================
    
    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('PrimStrings','Us',[{47,23,99,47},{0,0,55,66}]),
    ?line {ok,[{47,23,99,47},{0,0,55,66}]} = 
	asn1_wrapper:decode('PrimStrings','Us',lists:flatten(Bytes1)),
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('PrimStrings','Us',[{47,23,99,255},{0,0,0,201}]),
    ?line {ok,[{47,23,99,255},201]} = 
	asn1_wrapper:decode('PrimStrings','Us',lists:flatten(Bytes2)),
    
    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','Us',"Universal String"),
    ?line {ok,"Universal String"} = 
	asn1_wrapper:decode('PrimStrings','Us',lists:flatten(Bytes3)),
    
    ?line {ok,Bytes4} = asn1_wrapper:encode('PrimStrings','Us',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','Us',lists:flatten(Bytes4)),
    
    ?line {ok,Bytes5} = asn1_wrapper:encode('PrimStrings','Us',[{47,23,99,47}]),
    ?line {ok,[{47,23,99,47}]} = 
	asn1_wrapper:decode('PrimStrings','Us',lists:flatten(Bytes5)),

    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
    
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','Us',lists:flatten([16#3C,12,28,4,47,23,99,255,28,4,0,0,2,201])),
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','Us',lists:flatten([16#3C,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0]));
	      _ ->
		  ok
	  end,


    Us1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    ?line {ok,Bytes15} = asn1_wrapper:encode('PrimStrings','IA5',Us1),
    ?line {ok,Us1r} = asn1_wrapper:decode('PrimStrings','IA5',lists:flatten(Bytes15)),
    ?line Us1 = Us1r,



%%==========================================================
%% UsCon ::= [70] UniversalString
%%==========================================================

    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('PrimStrings','UsCon',[{47,23,99,255},{0,0,2,201}]),
    ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
	asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = 
	asn1_wrapper:encode('PrimStrings','UsCon',[{47,23,99,255},{0,0,0,201}]),
    ?line {ok,[{47,23,99,255},201]} = 
	asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten(Bytes12)),
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('PrimStrings','UsCon',"Universal String"),
    ?line {ok,"Universal String"} = 
	asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = asn1_wrapper:encode('PrimStrings','UsCon',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten(Bytes14)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten([16#BF,16#46,12,28,4,47,23,99,255,28,4,0,0,2,201])),
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsCon',lists:flatten([16#BF,16#46,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0]));
	      _ -> ok
	  end,



%%==========================================================
%% UsExpCon ::= [71] EXPLICIT UniversalString
%%==========================================================

    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('PrimStrings','UsExpCon',[{47,23,99,255},{0,0,2,201}]),
    ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
	asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten(Bytes21)),
    
    ?line {ok,Bytes22} = 
	asn1_wrapper:encode('PrimStrings','UsExpCon',[{47,23,99,255},{0,0,0,201}]),
    ?line {ok,[{47,23,99,255},201]} = 
	asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten(Bytes22)),
    
    ?line {ok,Bytes23} = 
	asn1_wrapper:encode('PrimStrings','UsExpCon',"Universal String"),
    ?line {ok,"Universal String"} = 
	asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten(Bytes23)),
    
    ?line {ok,Bytes24} = 
	asn1_wrapper:encode('PrimStrings','UsExpCon',[]),
    ?line {ok,[]} = 
	asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten(Bytes24)),
    
    ?line case asn1_wrapper:erule(Rules) of
	      ber ->     
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten([16#BF,16#47,14,60,12,28,4,47,23,99,255,28,4,0,0,2,201])),
		  ?line {ok,[{47,23,99,255},{0,0,2,201}]} = 
		      asn1_wrapper:decode('PrimStrings','UsExpCon',lists:flatten([16#BF,16#47,16,60,16#80,28,4,47,23,99,255,28,4,0,0,2,201,0,0]));
	      _ -> ok
	  end,
    

ok.




		       
bmp_string(_Rules) ->
		       
    %%==========================================================
    %% BMP ::= BMPString
    %%==========================================================

    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('PrimStrings','BMP',[{0,0,99,48},{0,0,2,201}]),
    ?line {ok,[{0,0,99,48},{0,0,2,201}]} = 
	asn1_wrapper:decode('PrimStrings','BMP',lists:flatten(Bytes1)),
    
    ?line {ok,Bytes2} = 
	asn1_wrapper:encode('PrimStrings','BMP',[{0,0,0,48},{0,0,2,201}]),
    ?line {ok,[48,{0,0,2,201}]} = 
	asn1_wrapper:decode('PrimStrings','BMP',lists:flatten(Bytes2)),
    
    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','BMP',"BMP String"),
    ?line {ok,"BMP String"} = 
	asn1_wrapper:decode('PrimStrings','BMP',lists:flatten(Bytes3)),
    
    ?line {ok,Bytes4} = asn1_wrapper:encode('PrimStrings','BMP',[]),
    ?line {ok,[]} = asn1_wrapper:decode('PrimStrings','BMP',lists:flatten(Bytes4)),


    BMP1 = "12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    ?line {ok,Bytes5} = asn1_wrapper:encode('PrimStrings','BMP',BMP1),
    ?line {ok,BMP1r} = asn1_wrapper:decode('PrimStrings','BMP',lists:flatten(Bytes5)),
    ?line BMP1 = BMP1r,
		       
		     
    ok.




		       
times(_Rules) ->

    %%==========================================================
    %% Gt ::= GeneralizedTime
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('PrimStrings','Gt',"19970923110723.2"),
    ?line {ok,"19970923110723.2"} = 
	asn1_wrapper:decode('PrimStrings','Gt',lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = asn1_wrapper:encode('PrimStrings','Gt',"19970923110723.2Z"),
    ?line {ok,"19970923110723.2Z"} = 
	asn1_wrapper:decode('PrimStrings','Gt',lists:flatten(Bytes2)),

    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','Gt',"19970923110723.2-0500"),
    ?line {ok,"19970923110723.2-0500"} = 
	asn1_wrapper:decode('PrimStrings','Gt',lists:flatten(Bytes3)),


    
    

		       

    %%==========================================================
    %% UTC ::= UTCTime
    %%==========================================================

    ?line {ok,Bytes11} = asn1_wrapper:encode('PrimStrings','UTC',"9709211107Z"),
    ?line {ok,"9709211107Z"} = 
	asn1_wrapper:decode('PrimStrings','UTC',lists:flatten(Bytes11)),

    ?line {ok,Bytes12} = asn1_wrapper:encode('PrimStrings','UTC',"9709211107-0500"),
    ?line {ok,"9709211107-0500"} = 
	asn1_wrapper:decode('PrimStrings','UTC',lists:flatten(Bytes12)),    

    ok.


utf8_string(_Rules) ->
		       
    %%==========================================================
    %% UTF ::= UTF8String
    %%==========================================================

    %% test values in all ranges

    ValLbR1 = [16#00],
    ValUbR1 = [16#7f],
    ValLbR2 = [16#80],
    ValUbR2 = [16#7ff],
    ValLbR3 = [16#800],
    ValUbR3 = [16#ffff],
    ValLbR4 = [16#10000],
    ValUbR4 = [16#1fffff],
    ValLbR5 = [16#200000],
    ValUbR5 = [16#3ffffff],
    ValLbR6 = [16#4000000],
    ValUbR6 = [16#7fffffff],
    
    ?line {ok,UTF8L1} = asn1rt:utf8_list_to_binary(ValLbR1),
    ?line {ok,Bytes1} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L1),
    ?line {ok,Bin1} = asn1_wrapper:decode('PrimStrings','UTF',Bytes1),
    ?line {ok,ValLbR1} = wrapper_utf8_binary_to_list(Bin1),
    
    ?line {ok,UTF8L2} = asn1rt:utf8_list_to_binary(ValUbR1),
    ?line {ok,Bytes2} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L2),
    ?line {ok,Bin2} = asn1_wrapper:decode('PrimStrings','UTF',Bytes2),
    ?line {ok,ValUbR1} = wrapper_utf8_binary_to_list(Bin2),

    ?line {ok,UTF8L3} = asn1rt:utf8_list_to_binary(ValLbR2),
    ?line {ok,Bytes3} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L3),
    ?line {ok,Bin3} = asn1_wrapper:decode('PrimStrings','UTF',Bytes3),
    ?line {ok,ValLbR2} = wrapper_utf8_binary_to_list(Bin3),

    ?line {ok,UTF8L4} = asn1rt:utf8_list_to_binary(ValUbR2),
    ?line {ok,Bytes4} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L4),
    ?line {ok,Bin4} = asn1_wrapper:decode('PrimStrings','UTF',Bytes4),
    ?line {ok,ValUbR2} = wrapper_utf8_binary_to_list(Bin4),

    ?line {ok,UTF8L5} = asn1rt:utf8_list_to_binary(ValLbR3),
    ?line {ok,Bytes5} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L5),
    ?line {ok,Bin5} = asn1_wrapper:decode('PrimStrings','UTF',Bytes5),
    ?line {ok,ValLbR3} = wrapper_utf8_binary_to_list(Bin5),

    ?line {ok,UTF8L6} = asn1rt:utf8_list_to_binary(ValUbR3),
    ?line {ok,Bytes6} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L6),
    ?line {ok,Bin6} = asn1_wrapper:decode('PrimStrings','UTF',Bytes6),
    ?line {ok,ValUbR3} = wrapper_utf8_binary_to_list(Bin6),

    ?line {ok,UTF8L7} = asn1rt:utf8_list_to_binary(ValLbR4),
    ?line {ok,Bytes7} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L7),
    ?line {ok,Bin7} = asn1_wrapper:decode('PrimStrings','UTF',Bytes7),
    ?line {ok,ValLbR4} = wrapper_utf8_binary_to_list(Bin7),

    ?line {ok,UTF8L8} = asn1rt:utf8_list_to_binary(ValUbR4),
    ?line {ok,Bytes8} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L8),
    ?line {ok,Bin8} = asn1_wrapper:decode('PrimStrings','UTF',Bytes8),
    ?line {ok,ValUbR4} = wrapper_utf8_binary_to_list(Bin8),

    ?line {ok,UTF8L9} = asn1rt:utf8_list_to_binary(ValLbR5),
    ?line {ok,Bytes9} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L9),
    ?line {ok,Bin9} = asn1_wrapper:decode('PrimStrings','UTF',Bytes9),
    ?line {ok,ValLbR5} = wrapper_utf8_binary_to_list(Bin9),

    ?line {ok,UTF8L10} = asn1rt:utf8_list_to_binary(ValUbR5),
    ?line {ok,Bytes10} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L10),
    ?line {ok,Bin10} = asn1_wrapper:decode('PrimStrings','UTF',Bytes10),
    ?line {ok,ValUbR5} = wrapper_utf8_binary_to_list(Bin10),

    ?line {ok,UTF8L11} = asn1rt:utf8_list_to_binary(ValLbR6),
    ?line {ok,Bytes11} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L11),
    ?line {ok,Bin11} = asn1_wrapper:decode('PrimStrings','UTF',Bytes11),
    ?line {ok,ValLbR6} = wrapper_utf8_binary_to_list(Bin11),

    ?line {ok,UTF8L12} = asn1rt:utf8_list_to_binary(ValUbR6),
    ?line {ok,Bytes12} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L12),
    ?line {ok,Bin12} = asn1_wrapper:decode('PrimStrings','UTF',Bytes12),
    ?line {ok,ValUbR6} = wrapper_utf8_binary_to_list(Bin12),
    
    LVal = ValLbR1++ValUbR1++ValLbR2++ValUbR2++ValLbR3++ValUbR3++
	ValLbR4++ValUbR4++ValLbR5++ValUbR5++ValLbR6++ValUbR6,
    LongVal = LVal++LVal++LVal++LVal++LVal++LVal++LVal++"hello",
    
    ?line {ok,UTF8L13} = asn1rt:utf8_list_to_binary(LongVal),
    ?line {ok,Bytes13} = asn1_wrapper:encode('PrimStrings','UTF',UTF8L13),
    ?line {ok,Bin13} = asn1_wrapper:decode('PrimStrings','UTF',Bytes13),
    ?line {ok,LongVal} = wrapper_utf8_binary_to_list(Bin13).

wrapper_utf8_binary_to_list(L) when is_list(L) ->
    asn1rt:utf8_binary_to_list(list_to_binary(L));
wrapper_utf8_binary_to_list(B) ->
    asn1rt:utf8_binary_to_list(B).
