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
-module(testPrimExternal).

-export([compile/3]).
-export([external/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "PrimExternal",[Rules,{outdir,OutDir}]++Options).



external(_Rules) ->
    

    ?line {ok,Bytes10} = asn1_wrapper:encode('PrimExternal','NT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','NT',lists:flatten(Bytes10)),
    
    ?line {ok,Bytes11} = asn1_wrapper:encode('PrimExternal','Imp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','Imp',lists:flatten(Bytes11)),
    
    ?line {ok,Bytes12} = asn1_wrapper:encode('PrimExternal','Exp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','Exp',lists:flatten(Bytes12)),
    

    ?line {ok,Bytes13} = asn1_wrapper:encode('PrimExternal','NTNT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','NTNT',lists:flatten(Bytes13)),
    
    ?line {ok,Bytes14} = asn1_wrapper:encode('PrimExternal','NTImp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','NTImp',lists:flatten(Bytes14)),
    
    ?line {ok,Bytes15} = asn1_wrapper:encode('PrimExternal','NTExp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','NTExp',lists:flatten(Bytes15)),
    

    ?line {ok,Bytes16} = asn1_wrapper:encode('PrimExternal','ImpNT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','ImpNT',lists:flatten(Bytes16)),
    
    ?line {ok,Bytes17} = asn1_wrapper:encode('PrimExternal','ImpImp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','ImpImp',lists:flatten(Bytes17)),
    
    ?line {ok,Bytes18} = asn1_wrapper:encode('PrimExternal','ImpExp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','ImpExp',lists:flatten(Bytes18)),
    

    ?line {ok,Bytes19} = asn1_wrapper:encode('PrimExternal','ExpNT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','ExpNT',lists:flatten(Bytes19)),
    
    ?line {ok,Bytes20} = asn1_wrapper:encode('PrimExternal','ExpImp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','ExpImp',lists:flatten(Bytes20)),
    
    ?line {ok,Bytes21} = asn1_wrapper:encode('PrimExternal','ExpExp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','ExpExp',lists:flatten(Bytes21)),
    



    
    ?line {ok,Bytes31} = asn1_wrapper:encode('PrimExternal','XNTNT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XNTNT',lists:flatten(Bytes31)),
    
    ?line {ok,Bytes32} = asn1_wrapper:encode('PrimExternal','XNTImp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XNTImp',lists:flatten(Bytes32)),
    
    ?line {ok,Bytes33} = asn1_wrapper:encode('PrimExternal','XNTExp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XNTExp',lists:flatten(Bytes33)),
    

    ?line {ok,Bytes34} = asn1_wrapper:encode('PrimExternal','XImpNT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XImpNT',lists:flatten(Bytes34)),
    
    ?line {ok,Bytes35} = asn1_wrapper:encode('PrimExternal','XImpImp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XImpImp',lists:flatten(Bytes35)),
    
    ?line {ok,Bytes36} = asn1_wrapper:encode('PrimExternal','XImpExp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XImpExp',lists:flatten(Bytes36)),
    

    ?line {ok,Bytes37} = asn1_wrapper:encode('PrimExternal','XExpNT',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XExpNT',lists:flatten(Bytes37)),
    
    ?line {ok,Bytes38} = asn1_wrapper:encode('PrimExternal','XExpImp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XExpImp',lists:flatten(Bytes38)),
    
    ?line {ok,Bytes39} = asn1_wrapper:encode('PrimExternal','XExpExp',"kalle"),
    ?line {ok,"kalle"} = asn1_wrapper:decode('PrimExternal','XExpExp',lists:flatten(Bytes39)),
    



    


ok.
