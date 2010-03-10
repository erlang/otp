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
-module(testPrim).

-export([compile/3]).
-export([bool/1]).
-export([int/1]).
-export([enum/1]).
-export([obj_id/1]).
-export([rel_oid/1]).
-export([null/1]).
-export([real/1]).

-include_lib("test_server/include/test_server.hrl").



compile(Config,Rules,Opt) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    case Opt of
	[optimize] ->
	    ?line ok = asn1ct:compile(DataDir ++ "Prim",
				      [Rules,optimize,{outdir,OutDir}]),
	    ?line ok = asn1ct:compile(DataDir ++ "Real",
				      [Rules,optimize,{outdir,OutDir}]);
	__ ->
	    ?line ok = asn1ct:compile(DataDir ++ "Prim",
				      [Rules,{outdir,OutDir}]),
	    ?line ok = asn1ct:compile(DataDir ++ "Real",
				      [Rules,{outdir,OutDir}])
    end.




bool(Rules) ->

    %%==========================================================
    %% Bool ::=  BOOLEAN 
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('Prim','Bool',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','Bool',lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = asn1_wrapper:encode('Prim','Bool',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','Bool',lists:flatten(Bytes2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','Bool',517)),
		  ok;
	      per ->
		  ok
	  end,

    



    %%==========================================================
    %% BoolCon ::=  [20] BOOLEAN 
    %%==========================================================


    ?line {ok,BytesCon1} = asn1_wrapper:encode('Prim','BoolCon',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','BoolCon',lists:flatten(BytesCon1)),

    ?line {ok,BytesCon2} = asn1_wrapper:encode('Prim','BoolCon',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','BoolCon',lists:flatten(BytesCon2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','BoolCon',517)),
		  ok;
	      per ->
		  ok
	  end,
    




    %%==========================================================
    %% BoolPri ::=  [PRIVATE 21] BOOLEAN 
    %%==========================================================

    ?line {ok,BytesPri1} = asn1_wrapper:encode('Prim','BoolPri',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','BoolPri',lists:flatten(BytesPri1)),

    ?line {ok,BytesPri2} = asn1_wrapper:encode('Prim','BoolPri',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','BoolPri',lists:flatten(BytesPri2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','BoolPri',517)),
		  ok;
	      per ->
		  ok
	  end,


    %%==========================================================
    %% BoolApp ::=  [APPLICATION 22] BOOLEAN 
    %%==========================================================

    ?line {ok,BytesApp1} = asn1_wrapper:encode('Prim','BoolApp',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','BoolApp',lists:flatten(BytesApp1)),

    ?line {ok,BytesApp2} = asn1_wrapper:encode('Prim','BoolApp',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','BoolApp',lists:flatten(BytesApp2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','BoolApp',517)),
		  ok;
	      per ->
		  ok
	  end,


    %%==========================================================
    %% BoolExpCon ::=  [30] EXPLICIT BOOLEAN 
    %%==========================================================

    ?line {ok,BytesExpCon1} = asn1_wrapper:encode('Prim','BoolExpCon',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','BoolExpCon',lists:flatten(BytesExpCon1)),

    ?line {ok,BytesExpCon2} = asn1_wrapper:encode('Prim','BoolExpCon',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','BoolExpCon',lists:flatten(BytesExpCon2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','BoolExpCon',517)),
		  ok;
	      per ->
		  ok
	  end,



    %%==========================================================
    %% BoolExpPri ::=  [PRIVATE 31] EXPLICIT BOOLEAN 
    %%==========================================================

    ?line {ok,BytesExpPri1} = asn1_wrapper:encode('Prim','BoolExpPri',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','BoolExpPri',lists:flatten(BytesExpPri1)),

    ?line {ok,BytesExpPri2} = asn1_wrapper:encode('Prim','BoolExpPri',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','BoolExpPri',lists:flatten(BytesExpPri2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','BoolExpPri',517)),
		  ok;
	      per ->
		  ok
	  end,


    %%==========================================================
    %% BoolExpApp ::=  [APPLICATION 32] EXPLICIT BOOLEAN 
    %%==========================================================

    ?line {ok,BytesExpApp1} = asn1_wrapper:encode('Prim','BoolExpApp',true),
    ?line {ok,true} = asn1_wrapper:decode('Prim','BoolExpApp',lists:flatten(BytesExpApp1)),

    ?line {ok,BytesExpApp2} = asn1_wrapper:encode('Prim','BoolExpApp',false),
    ?line {ok,false} = asn1_wrapper:decode('Prim','BoolExpApp',lists:flatten(BytesExpApp2)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{encode_boolean,517}}} = 
		      (catch asn1_wrapper:encode('Prim','BoolExpApp',517)),
		  ok;
	      per ->
		  ok
	  end,

    ok.


int(Rules) ->


    %%==========================================================
    %% Int ::=  INTEGER 
    %%==========================================================

    %% test of OTP-2666 encoding should use minimum number of octets x.690 8.3.2
    ?line {ok,Bytes0} = asn1_wrapper:encode('Prim','Int',-128),
    ?line L0 = lists:flatten(Bytes0),
    ?line {ok,-128} = asn1_wrapper:decode('Prim','Int',lists:flatten(L0)),
    case asn1_wrapper:erule(Rules) of
	ber ->
	    ?line [_,1,128] = L0;
	per -> ok
    end,

    ?line {ok,Bytes1} = asn1_wrapper:encode('Prim','Int',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = asn1_wrapper:encode('Prim','Int',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes2)),

    ?line {ok,Bytes3} = asn1_wrapper:encode('Prim','Int',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes3)),

    ?line {ok,Bytes4} = asn1_wrapper:encode('Prim','Int',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes4)),

    ?line {ok,Bytes5} = asn1_wrapper:encode('Prim','Int',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes5)),

    ?line {ok,Bytes6} = asn1_wrapper:encode('Prim','Int',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes6)),

    ?line {ok,Bytes7} = asn1_wrapper:encode('Prim','Int',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes7)),

    ?line {ok,Bytes8} = asn1_wrapper:encode('Prim','Int',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes8)),

    ?line {ok,Bytes9} = asn1_wrapper:encode('Prim','Int',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes9)),

    ?line {ok,Bytes10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(Bytes10)),





    %%==========================================================
    %% IntCon ::=  [40] INTEGER 
    %%==========================================================

    ?line {ok,BytesCon1} = asn1_wrapper:encode('Prim','IntCon',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon1)),

    ?line {ok,BytesCon2} = asn1_wrapper:encode('Prim','IntCon',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon2)),

    ?line {ok,BytesCon3} = asn1_wrapper:encode('Prim','IntCon',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon3)),

    ?line {ok,BytesCon4} = asn1_wrapper:encode('Prim','IntCon',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon4)),

    ?line {ok,BytesCon5} = asn1_wrapper:encode('Prim','IntCon',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon5)),

    ?line {ok,BytesCon6} = asn1_wrapper:encode('Prim','IntCon',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon6)),

    ?line {ok,BytesCon7} = asn1_wrapper:encode('Prim','IntCon',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon7)),

    ?line {ok,BytesCon8} = asn1_wrapper:encode('Prim','IntCon',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon8)),

    ?line {ok,BytesCon9} = asn1_wrapper:encode('Prim','IntCon',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntCon',lists:flatten(BytesCon9)),

    ?line {ok,BytesCon10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesCon10)),



    %%==========================================================
    %% IntPri ::=  [PRIVATE 41] INTEGER 
    %%==========================================================

    ?line {ok,BytesPri1} = asn1_wrapper:encode('Prim','IntPri',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri1)),

    ?line {ok,BytesPri2} = asn1_wrapper:encode('Prim','IntPri',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri2)),

    ?line {ok,BytesPri3} = asn1_wrapper:encode('Prim','IntPri',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri3)),

    ?line {ok,BytesPri4} = asn1_wrapper:encode('Prim','IntPri',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri4)),

    ?line {ok,BytesPri5} = asn1_wrapper:encode('Prim','IntPri',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri5)),

    ?line {ok,BytesPri6} = asn1_wrapper:encode('Prim','IntPri',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri6)),

    ?line {ok,BytesPri7} = asn1_wrapper:encode('Prim','IntPri',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri7)),

    ?line {ok,BytesPri8} = asn1_wrapper:encode('Prim','IntPri',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri8)),

    ?line {ok,BytesPri9} = asn1_wrapper:encode('Prim','IntPri',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntPri',lists:flatten(BytesPri9)),

    ?line {ok,BytesPri10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesPri10)),



    %%==========================================================
    %% IntApp ::=  [APPLICATION 42] INTEGER 
    %%==========================================================

    ?line {ok,BytesApp1} = asn1_wrapper:encode('Prim','IntApp',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp1)),

    ?line {ok,BytesApp2} = asn1_wrapper:encode('Prim','IntApp',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp2)),

    ?line {ok,BytesApp3} = asn1_wrapper:encode('Prim','IntApp',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp3)),

    ?line {ok,BytesApp4} = asn1_wrapper:encode('Prim','IntApp',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp4)),

    ?line {ok,BytesApp5} = asn1_wrapper:encode('Prim','IntApp',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp5)),

    ?line {ok,BytesApp6} = asn1_wrapper:encode('Prim','IntApp',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp6)),

    ?line {ok,BytesApp7} = asn1_wrapper:encode('Prim','IntApp',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp7)),

    ?line {ok,BytesApp8} = asn1_wrapper:encode('Prim','IntApp',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp8)),

    ?line {ok,BytesApp9} = asn1_wrapper:encode('Prim','IntApp',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntApp',lists:flatten(BytesApp9)),

    ?line {ok,BytesApp10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesApp10)),


    %%==========================================================
    %% IntExpCon ::=  [50] EXPLICIT INTEGER 
    %%==========================================================

    ?line {ok,BytesExpCon1} = asn1_wrapper:encode('Prim','IntExpCon',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon1)),

    ?line {ok,BytesExpCon2} = asn1_wrapper:encode('Prim','IntExpCon',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon2)),

    ?line {ok,BytesExpCon3} = asn1_wrapper:encode('Prim','IntExpCon',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon3)),

    ?line {ok,BytesExpCon4} = asn1_wrapper:encode('Prim','IntExpCon',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon4)),

    ?line {ok,BytesExpCon5} = asn1_wrapper:encode('Prim','IntExpCon',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon5)),

    ?line {ok,BytesExpCon6} = asn1_wrapper:encode('Prim','IntExpCon',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon6)),

    ?line {ok,BytesExpCon7} = asn1_wrapper:encode('Prim','IntExpCon',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon7)),

    ?line {ok,BytesExpCon8} = asn1_wrapper:encode('Prim','IntExpCon',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon8)),

    ?line {ok,BytesExpCon9} = asn1_wrapper:encode('Prim','IntExpCon',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntExpCon',lists:flatten(BytesExpCon9)),

    ?line {ok,BytesExpCon10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesExpCon10)),

    %%==========================================================
    %% IntExpPri ::=  [PRIVATE 51] EXPLICIT INTEGER 
    %%==========================================================

    ?line {ok,BytesExpPri1} = asn1_wrapper:encode('Prim','IntExpPri',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri1)),

    ?line {ok,BytesExpPri2} = asn1_wrapper:encode('Prim','IntExpPri',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri2)),

    ?line {ok,BytesExpPri3} = asn1_wrapper:encode('Prim','IntExpPri',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri3)),

    ?line {ok,BytesExpPri4} = asn1_wrapper:encode('Prim','IntExpPri',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri4)),

    ?line {ok,BytesExpPri5} = asn1_wrapper:encode('Prim','IntExpPri',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri5)),

    ?line {ok,BytesExpPri6} = asn1_wrapper:encode('Prim','IntExpPri',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri6)),

    ?line {ok,BytesExpPri7} = asn1_wrapper:encode('Prim','IntExpPri',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri7)),

    ?line {ok,BytesExpPri8} = asn1_wrapper:encode('Prim','IntExpPri',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri8)),

    ?line {ok,BytesExpPri9} = asn1_wrapper:encode('Prim','IntExpPri',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntExpPri',lists:flatten(BytesExpPri9)),

    ?line {ok,BytesExpPri10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesExpPri10)),

    %%==========================================================
    %% IntExpApp ::=  [APPLICATION 52] EXPLICIT INTEGER 
    %%==========================================================

    ?line {ok,BytesExpApp1} = asn1_wrapper:encode('Prim','IntExpApp',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp1)),

    ?line {ok,BytesExpApp2} = asn1_wrapper:encode('Prim','IntExpApp',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp2)),

    ?line {ok,BytesExpApp3} = asn1_wrapper:encode('Prim','IntExpApp',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp3)),

    ?line {ok,BytesExpApp4} = asn1_wrapper:encode('Prim','IntExpApp',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp4)),

    ?line {ok,BytesExpApp5} = asn1_wrapper:encode('Prim','IntExpApp',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp5)),

    ?line {ok,BytesExpApp6} = asn1_wrapper:encode('Prim','IntExpApp',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp6)),

    ?line {ok,BytesExpApp7} = asn1_wrapper:encode('Prim','IntExpApp',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp7)),

    ?line {ok,BytesExpApp8} = asn1_wrapper:encode('Prim','IntExpApp',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp8)),

    ?line {ok,BytesExpApp9} = asn1_wrapper:encode('Prim','IntExpApp',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntExpApp',lists:flatten(BytesExpApp9)),

    ?line {ok,BytesExpApp10} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesExpApp10)),


    %%==========================================================
    %% IntEnum ::=  INTEGER {first(1),last(31)} 
    %%==========================================================

    ?line {ok,BytesEnum1} = asn1_wrapper:encode('Prim','IntEnum',4),
    ?line {ok,4} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum1)),

    ?line {ok,BytesEnum2} = asn1_wrapper:encode('Prim','IntEnum',444),
    ?line {ok,444} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum2)),

    ?line {ok,BytesEnum3} = asn1_wrapper:encode('Prim','IntEnum',123456789),
    ?line {ok,123456789} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum3)),

    ?line {ok,BytesEnum4} = asn1_wrapper:encode('Prim','IntEnum',12345678901234567890),
    ?line {ok,12345678901234567890} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum4)),

    ?line {ok,BytesEnum5} = asn1_wrapper:encode('Prim','IntEnum',-100),
    ?line {ok,-100} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum5)),

    ?line {ok,BytesEnum6} = asn1_wrapper:encode('Prim','IntEnum',-255),
    ?line {ok,-255} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum6)),

    ?line {ok,BytesEnum7} = asn1_wrapper:encode('Prim','IntEnum',-256),
    ?line {ok,-256} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum7)),

    ?line {ok,BytesEnum8} = asn1_wrapper:encode('Prim','IntEnum',-257),
    ?line {ok,-257} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum8)),

    ?line {ok,BytesEnum9} = asn1_wrapper:encode('Prim','IntEnum',-1234567890),
    ?line {ok,-1234567890} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum9)),

    ?line {ok,BytesEnum910} = asn1_wrapper:encode('Prim','Int',-2147483648),
    ?line {ok,-2147483648} = asn1_wrapper:decode('Prim','Int',lists:flatten(BytesEnum910)),


    ?line {ok,BytesEnum10} = asn1_wrapper:encode('Prim','IntEnum',first),
    ?line {ok,first} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum10)),

    ?line {ok,BytesEnum11} = asn1_wrapper:encode('Prim','IntEnum',last),
    ?line {ok,last} = asn1_wrapper:decode('Prim','IntEnum',lists:flatten(BytesEnum11)),
    


    ok.



enum(Rules) ->

    %%==========================================================
    %% Enum ::=  ENUMERATED {monday(1),tuesday(2),wednesday(3),thursday(4),
    %%                       friday(5),saturday(6),sunday(7)}
    %%==========================================================

    ?line {ok,BytesEnum1} = asn1_wrapper:encode('Prim','Enum',monday),
    ?line {ok,monday} = asn1_wrapper:decode('Prim','Enum',lists:flatten(BytesEnum1)),

    ?line {ok,BytesEnum2} = asn1_wrapper:encode('Prim','Enum',thursday),
    ?line {ok,thursday} = asn1_wrapper:decode('Prim','Enum',lists:flatten(BytesEnum2)),


    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {error,{asn1,{_,4}}} = 
		      case catch asn1_wrapper:encode('Prim','Enum',4) of Enum -> Enum end,
		  ok;
	      per ->
		  ?line {error,{asn1,{_,4}}} = 
		      case catch asn1_wrapper:encode('Prim','Enum',4) of Enum -> Enum end,
		  ok
	  end,
    ok.



obj_id(Rules) ->

    %%==========================================================
    %% ObjId ::= OBJECT IDENTIFIER
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('Prim','ObjId',{0,22,3}),
    ?line {ok,{0,22,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = asn1_wrapper:encode('Prim','ObjId',{1,39,3}),
    ?line {ok,{1,39,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes2)),

    ?line {ok,Bytes3} = asn1_wrapper:encode('Prim','ObjId',{2,100,3}),
    ?line {ok,{2,100,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes3)),

    ?line {ok,Bytes4} = asn1_wrapper:encode('Prim','ObjId',{2,16303,3}),
    ?line {ok,{2,16303,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes4)),

    ?line case asn1_wrapper:erule(Rules) of
	      ber -> 
		  ?line {ok,Bytes5} = asn1_wrapper:encode('Prim','ObjId',{2,16304,3}),
		  ?line {ok,{2,16304,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes5)),
		  ok;
	      per ->
		  ?line {ok,Bytes5} = asn1_wrapper:encode('Prim','ObjId',{2,16304,3}),
		  ?line {ok,{2,16304,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes5)),
%%		  ?line test_server:format("~p~n",[Kurt]),
%		  ?line {ok,{2,16304,3}} = asn1_wrapper:decode('Prim','ObjId',lists:flatten(Bytes5)),
		  ok
	  end,



    ok.

rel_oid(_Rules) ->

    %%==========================================================
    %% RelOid ::= RELATIVE-OID
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('Prim','RelOid',{0,22,3}),
    ?line {ok,{0,22,3}} = asn1_wrapper:decode('Prim','RelOid',lists:flatten(Bytes1)),

    ?line {ok,Bytes2} = asn1_wrapper:encode('Prim','RelOid',{1,39,3}),
    ?line {ok,{1,39,3}} = asn1_wrapper:decode('Prim','RelOid',lists:flatten(Bytes2)),

    ?line {ok,Bytes3} = asn1_wrapper:encode('Prim','RelOid',{2,100,3}),
    ?line {ok,{2,100,3}} = asn1_wrapper:decode('Prim','RelOid',lists:flatten(Bytes3)),

    ?line {ok,Bytes4} = asn1_wrapper:encode('Prim','RelOid',{2,16303,3}),
    ?line {ok,{2,16303,3}} = asn1_wrapper:decode('Prim','RelOid',lists:flatten(Bytes4)),
    
    ?line {ok,Bytes5} = asn1_wrapper:encode('Prim','RelOid',{2,16304,3}),
    ?line {ok,{2,16304,3}} = asn1_wrapper:decode('Prim','RelOid',lists:flatten(Bytes5)),
    
    ?line {ok,Bytes6} = asn1_wrapper:encode('Prim','RelOid',{8,16304,16#ffff}),
    ?line {ok,{8,16304,16#ffff}} = asn1_wrapper:decode('Prim','RelOid',lists:flatten(Bytes6)),
    


    ok.





null(_Rules) ->

    %%==========================================================
    %% Null ::= NULL
    %%==========================================================

    ?line {ok,Bytes1} = asn1_wrapper:encode('Prim','Null',monday),
    ?line {ok,'NULL'} = asn1_wrapper:decode('Prim','Null',lists:flatten(Bytes1)),



ok.



real(_Rules) ->
    %%==========================================================
    %% AngleInRadians ::= REAL
    %%==========================================================
    
    %% Base 2
    ?line {ok,Bytes1} = asn1_wrapper:encode('Real','AngleInRadians',{1,2,1}),
    ?line {ok,{1,2,1}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes1),

    ?line {ok,Bytes2} = asn1_wrapper:encode('Real','AngleInRadians',{129,2,1}),
    ?line {ok,{129,2,1}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes2),

    ?line {ok,Bytes3} = asn1_wrapper:encode('Real','AngleInRadians',{128,2,1}),
    ?line {ok,{1,2,8}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes3),

    ?line {ok,Bytes4} = asn1_wrapper:encode('Real','AngleInRadians',{128,2,-7}),
    ?line {ok,{1,2,0}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes4),

    ?line {ok,Bytes5} = asn1_wrapper:encode('Real','AngleInRadians',{16#f1f1f1,2,128}),
    ?line {ok,{16#f1f1f1,2,128}} = asn1_wrapper:decode('Real','AngleInRadians',Bytes5),

    %% Base 10, tuple format
    ?line {ok,Bytes6} = asn1_wrapper:encode('Real','AngleInRadians',{1,10,1}),
    ?line {ok,"1.E1"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes6),
    
    ?line {ok,Bytes7} = asn1_wrapper:encode('Real','AngleInRadians',{100,10,1}),
    ?line {ok,"1.E3"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes7),

    ?line {ok,Bytes8} = asn1_wrapper:encode('Real','AngleInRadians',{-100,10,1}),
    ?line {ok,"-1.E3"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes8),

    ?line {ok,Bytes9} = asn1_wrapper:encode('Real','AngleInRadians',{00002,10,1}),
    ?line {ok,"2.E1"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes9),

    ?line {ok,Bytes10} = asn1_wrapper:encode('Real','AngleInRadians',{123000,10,0}),
    ?line {ok,"123.E3"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes10),

    ?line {ok,Bytes11} = asn1_wrapper:encode('Real','AngleInRadians',{123456789,10,123456789}),
    ?line {ok,"123456789.E123456789"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes11),

    ?line {ok,Bytes12} = asn1_wrapper:encode('Real','AngleInRadians',{-12345,10,-12345}),
    ?line {ok,"-12345.E-12345"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes12),

    %% Base 10, string format NR3
    
    ?line {ok,Bytes13} = asn1_wrapper:encode('Real','AngleInRadians',"123.123E123"),
    ?line {ok,"123123.E120"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes13),

    ?line {ok,Bytes14} = asn1_wrapper:encode('Real','AngleInRadians',"0.0E0"),
    ?line {ok,"0.E+0"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes14),

    ?line {ok,Bytes15} = asn1_wrapper:encode('Real','AngleInRadians',"0.0123"),
    ?line {ok,"123.E-4"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes15),

    ?line {ok,Bytes16} = asn1_wrapper:encode('Real','AngleInRadians',"0"),
    ?line {ok,"0.E+0"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes16),
    
    ?line {ok,Bytes17} = asn1_wrapper:encode('Real','AngleInRadians',"-123.45"),
    ?line {ok,"-12345.E-2"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes17),

    ?line {ok,Bytes18} = 
	asn1_wrapper:encode('Real','AngleInRadians',"123456789E123456789"),
    ?line {ok,"123456789.E123456789"} = 
	asn1_wrapper:decode('Real','AngleInRadians',Bytes18),

    ?line {ok,Bytes19} = asn1_wrapper:encode('Real','AngleInRadians',"01.000E1"),
    ?line {ok,"1.E1"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes19),

    ?line {ok,Bytes20} = asn1_wrapper:encode('Real','AngleInRadians',"120.0001"),
    ?line {ok,"1200001.E-4"} = asn1_wrapper:decode('Real','AngleInRadians',Bytes20).
