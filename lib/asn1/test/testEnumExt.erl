%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
-module(testEnumExt).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "EnumExt",[Rules,{outdir,OutDir}]++Options).


main(Rules) when Rules == per; Rules == per_bin; Rules == uper_bin ->
    io:format("main(~p)~n",[Rules]),
    B32=[32],B64=[64],
    %% ENUMERATED with extensionmark (value is in root set)
    ?line {ok,B32} = asn1_wrapper:encode('EnumExt','Ext',red),
    ?line {ok,red} = asn1_wrapper:decode('EnumExt','Ext',B32),

    %% ENUMERATED with extensionmark (value is an extensionvalue)
    ?line {ok,Or} = asn1_wrapper:encode('EnumExt','Ext1',orange),
    ?line {ok,orange} = asn1_wrapper:decode('EnumExt','Ext1',Or),
    %% unknown extensionvalue
    ?line {ok,{asn1_enum,0}} = asn1_wrapper:decode('EnumExt','Ext',Or),


    %% ENUMERATED no extensionmark 
    ?line {ok,B64} = asn1_wrapper:encode('EnumExt','Noext',red),
    ?line {ok,red} = asn1_wrapper:decode('EnumExt','Noext',B64),
    ok;

main(ber_bin_v2) ->
    main(ber);
main(ber_bin) ->
    main(ber);
main(ber) ->
    io:format("main(ber)~n",[]),
    %% ENUMERATED with extensionmark (value is in root set)
    ?line {ok,Bytes1} = asn1_wrapper:encode('EnumExt','Ext',red),
    ?line {ok,red} = asn1_wrapper:decode('EnumExt','Ext',lists:flatten(Bytes1)),

    %% value is an extensionvalue
    ?line {ok,Bytes1_1} = asn1_wrapper:encode('EnumExt','Ext1',orange),
    ?line {ok,{asn1_enum,7}} = asn1_wrapper:decode('EnumExt','Ext',lists:flatten(Bytes1_1)),
%%    ?line {ok,Bytes1_1} = asn1_wrapper:encode('EnumExt','Ext',{asn1_enum,7}),

    %% ENUMERATED no extensionmark 
    ?line {ok,Bytes2} = asn1_wrapper:encode('EnumExt','Noext',red),
    ?line {ok,red} = asn1_wrapper:decode('EnumExt','Noext',lists:flatten(Bytes2)),
    ?line {error,{asn1,_}} = (catch asn1_wrapper:encode('EnumExt','Noext',orange)),
%%    ?line {error,{asn1,_}} = (catch asn1_wrapper:encode('EnumExt','Noext',{asn1_enum,7})),
    ok,
    
    %% ENUMERATED with atom 'com'
    ?line {ok,Bytes3} = asn1_wrapper:encode('EnumExt','Globalstate',{'Globalstate',preop}),
    ?line {ok,preop} = asn1_wrapper:decode('EnumExt','Globalstate',
					   lists:flatten(Bytes3)),
    ?line {ok,Bytes4} = asn1_wrapper:encode('EnumExt','Globalstate',{'Globalstate',com}),
    ?line {ok,com} = asn1_wrapper:decode('EnumExt','Globalstate',
					   lists:flatten(Bytes4)).













