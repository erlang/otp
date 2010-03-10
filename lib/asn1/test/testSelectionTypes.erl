%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
-module(testSelectionTypes).

-export([compile/3]).
-export([test/0]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rule,Options) ->
    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SelectionType",
			      [Rule,{outdir,OutDir}]++Options).

test() ->
    Val = ["PrintableString","PrintableString","PrintableString"],
    ?line {ok,Bin}=asn1_wrapper:encode('SelectionType','MendeleyevTable',Val),          
    ?line {ok,Val} = asn1_wrapper:decode('SelectionType','MendeleyevTable',Bin),

    ?line Val2 = ['SelectionType':einsteinium()],
    ?line ["Es"] = Val2,
    
    ?line {ok,Bin2}=asn1_wrapper:encode('SelectionType','MendeleyevTable',Val2),          
    ?line {ok,Val2} = asn1_wrapper:decode('SelectionType','MendeleyevTable',Bin2).

