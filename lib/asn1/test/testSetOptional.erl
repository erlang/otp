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
-module(testSetOptional).

-include("External.hrl").
-export([compile/3]).
-export([main/1]).
-export([ticket_7533/1,decoder/4]).
-include_lib("test_server/include/test_server.hrl").

-record('SetOpt1',{bool1 = asn1_NOVALUE, int1, set1 = asn1_NOVALUE}).
-record('SetOpt1Imp',{bool1 = asn1_NOVALUE, int1, set1 = asn1_NOVALUE}).
-record('SetOpt1Exp',{bool1 = asn1_NOVALUE, int1, set1 = asn1_NOVALUE}).
-record('SetOpt2',{set2 = asn1_NOVALUE, bool2, int2}).
-record('SetOpt2Imp',{set2 = asn1_NOVALUE, bool2, int2}).
-record('SetOpt2Exp',{set2 = asn1_NOVALUE, bool2, int2}).
-record('SetOpt3',{bool3 = asn1_NOVALUE, set3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SetOpt3Imp',{bool3 = asn1_NOVALUE, set3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SetOpt3Exp',{bool3 = asn1_NOVALUE, set3 = asn1_NOVALUE, int3 = asn1_NOVALUE}).
-record('SetIn',{boolIn, intIn}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SetOptional",[Rules,{outdir,OutDir}]++Options).



main(_Rules) ->
    
    ?line {ok,Bytes11} = 
	asn1_wrapper:encode('SetOptional','SetOpt1',#'SetOpt1'{bool1 = true,
							int1 = 15,
							set1 = #'SetIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SetOpt1',true,15,{'SetIn',true,66}}} = 
	asn1_wrapper:decode('SetOptional','SetOpt1',lists:flatten(Bytes11)),
    
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('SetOptional','SetOpt1',#'SetOpt1'{int1 = 15}), 
    ?line {ok,{'SetOpt1',asn1_NOVALUE,15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SetOptional','SetOpt1',lists:flatten(Bytes12)),
    
    
    ?line {ok,Bytes21} = 
	asn1_wrapper:encode('SetOptional','SetOpt2',#'SetOpt2'{bool2 = true,
							int2 = 15,
							set2 = #'SetIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SetOpt2',{'SetIn',true,66},true,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt2',lists:flatten(Bytes21)),
    
    
    ?line {ok,Bytes22} = asn1_wrapper:encode('SetOptional','SetOpt2',#'SetOpt2'{int2 = 15,
									  bool2 = true}), 
    ?line {ok,{'SetOpt2',asn1_NOVALUE,true,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt2',lists:flatten(Bytes22)),
    
    
    
    ?line {ok,Bytes31} = 
	asn1_wrapper:encode('SetOptional','SetOpt3',#'SetOpt3'{bool3 = true,
							int3 = 15,
							set3 = #'SetIn'{boolIn = true,
									intIn = 66}}), 
    ?line {ok,{'SetOpt3',true,{'SetIn',true,66},15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt3',lists:flatten(Bytes31)),
    
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('SetOptional','SetOpt3',#'SetOpt3'{int3 = 15}), 
    ?line {ok,{'SetOpt3',asn1_NOVALUE,asn1_NOVALUE,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt3',lists:flatten(Bytes32)),
    
    
    
    
    
    ?line {ok,Bytes41} = 
	asn1_wrapper:encode('SetOptional','SetOpt1Imp',#'SetOpt1Imp'{bool1 = true,
							      int1 = 15,
							      set1 = #'SetIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetOpt1Imp',true,15,{'SetIn',true,66}}} = 
	asn1_wrapper:decode('SetOptional','SetOpt1Imp',lists:flatten(Bytes41)),
    
    
    ?line {ok,Bytes42} = asn1_wrapper:encode('SetOptional','SetOpt1Imp',#'SetOpt1Imp'{int1 = 15}), 
    ?line {ok,{'SetOpt1Imp',asn1_NOVALUE,15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SetOptional','SetOpt1Imp',lists:flatten(Bytes42)),
    
    
    ?line {ok,Bytes51} = 
	asn1_wrapper:encode('SetOptional','SetOpt2Imp',#'SetOpt2Imp'{bool2 = true,
							      int2 = 15,
							      set2 = #'SetIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetOpt2Imp',{'SetIn',true,66},true,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt2Imp',lists:flatten(Bytes51)),
    
    
    ?line {ok,Bytes52} = asn1_wrapper:encode('SetOptional','SetOpt2Imp',#'SetOpt2Imp'{int2 = 15,
									        bool2 = true}), 
    ?line {ok,{'SetOpt2Imp',asn1_NOVALUE,true,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt2Imp',lists:flatten(Bytes52)),
    
    
    
    ?line {ok,Bytes61} = 
	asn1_wrapper:encode('SetOptional','SetOpt3Imp',#'SetOpt3Imp'{bool3 = true,
							      int3 = 15,
							      set3 = #'SetIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetOpt3Imp',true,{'SetIn',true,66},15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt3Imp',lists:flatten(Bytes61)),
    
    
    ?line {ok,Bytes62} = asn1_wrapper:encode('SetOptional','SetOpt3Imp',#'SetOpt3Imp'{int3 = 15}), 
    ?line {ok,{'SetOpt3Imp',asn1_NOVALUE,asn1_NOVALUE,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt3Imp',lists:flatten(Bytes62)),
    
    
    
    
    
    
    ?line {ok,Bytes71} = 
	asn1_wrapper:encode('SetOptional','SetOpt1Exp',#'SetOpt1Exp'{bool1 = true,
							      int1 = 15,
							      set1 = #'SetIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetOpt1Exp',true,15,{'SetIn',true,66}}} = 
	asn1_wrapper:decode('SetOptional','SetOpt1Exp',lists:flatten(Bytes71)),
    
    
    ?line {ok,Bytes72} = asn1_wrapper:encode('SetOptional','SetOpt1Exp',#'SetOpt1Exp'{int1 = 15}), 
    ?line {ok,{'SetOpt1Exp',asn1_NOVALUE,15,asn1_NOVALUE}} = 
	asn1_wrapper:decode('SetOptional','SetOpt1Exp',lists:flatten(Bytes72)),
    
    
    ?line {ok,Bytes81} = 
	asn1_wrapper:encode('SetOptional','SetOpt2Exp',#'SetOpt2Exp'{bool2 = true,
							      int2 = 15,
							      set2 = #'SetIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetOpt2Exp',{'SetIn',true,66},true,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt2Exp',lists:flatten(Bytes81)),
    
    
    ?line {ok,Bytes82} = asn1_wrapper:encode('SetOptional','SetOpt2Exp',#'SetOpt2Exp'{int2 = 15,
									       bool2 = true}), 
    ?line {ok,{'SetOpt2Exp',asn1_NOVALUE,true,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt2Exp',lists:flatten(Bytes82)),
    
    
    
    ?line {ok,Bytes91} = 
	asn1_wrapper:encode('SetOptional','SetOpt3Exp',#'SetOpt3Exp'{bool3 = true,
							      int3 = 15,
							      set3 = #'SetIn'{boolIn = true,
									      intIn = 66}}), 
    ?line {ok,{'SetOpt3Exp',true,{'SetIn',true,66},15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt3Exp',lists:flatten(Bytes91)),
    
    
    ?line {ok,Bytes92} = asn1_wrapper:encode('SetOptional','SetOpt3Exp',#'SetOpt3Exp'{int3 = 15}), 
    ?line {ok,{'SetOpt3Exp',asn1_NOVALUE,asn1_NOVALUE,15}} = 
	asn1_wrapper:decode('SetOptional','SetOpt3Exp',lists:flatten(Bytes92)),
    
    
    
    
    ok.


ticket_7533(Ber) when Ber == ber; Ber == ber_bin ->
    Val = #'SetOpt1'{bool1 = true,int1=12,set1=#'SetIn'{boolIn=false,intIn=13}},
    ?line {ok,B} = asn1_wrapper:encode('SetOptional','SetOpt1',Val),
    ?line {ok,Val} = asn1_wrapper:decode('SetOptional','SetOpt1',B),
    
    CorruptVal = [49,14,1,1,255,2,1,12] ++ lists:duplicate(8,0),
    Pid = spawn(?MODULE,decoder,[self(),'SetOptional','SetOpt1',CorruptVal]),
    receive
	{ok,Pid,Result} ->
	    io:format("Decode result: ~p~n",[Result]),
	    ok
    after 10000 ->
	    exit(Pid,normal),
	    io:format("Decode timeout~n",[]),
	    error
    end;
ticket_7533(_) ->
    ok.

decoder(Parent,Module,Type,Val) ->
    io:format("Decoding~n",[]),
    ?line {ok,Res} = asn1_wrapper:decode(Module,Type,Val),
    io:format("Decode res: ~p~n",[Res]),
    Parent ! {ok,self(),Res}.
