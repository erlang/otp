%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(testTypeValueNotation).

-export([compile/3]).
-export([main/2]).

-include_lib("test_server/include/test_server.hrl").

-record('Seq',{octstr, int, bool, enum, bitstr, null, oid, vstr}).


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "SeqTypeRefPrim",
			      [Rules,{outdir,OutDir}]++Options),
    %% OTP-6695
    ?line ok = asn1ct:compile(DataDir ++ "ValueTest",
			      [Rules,{outdir,OutDir}]++Options).


main(Rules,Option) ->

    io:format("testTypeValueNotation:main/2 with arguments:~nRules: ~w, Option: ~w~n",[Rules,Option]),
    Value1 = #'Seq'{octstr = [1,2,3,4],
		    int = 12,
		    bool = true,
		    enum = a,
		    bitstr = [1,0,1,0],
		    null = 'NULL',
		    oid = {1,2,55},
		    vstr = "Hello World"},
    Value2 = #'Seq'{octstr = {'OctStr',[1,2,3,4]},
		    int = {'Int',12},
		    bool = {'Bool',true},
		    enum = {'Enum',a},
		    bitstr = {'BitStr',[1,0,1,0]},
		    null = {'Null','NULL'},
		    oid = {'OId',{1,2,55}},
		    vstr = {'VStr',"Hello World"}},
    case Option of
	optimize when Rules == per_bin; Rules == ber_bin ; Rules == uper_bin; Rules == ber_bin_v2 ->
	    ?line {ok,Bytes} = 
		asn1_wrapper:encode('SeqTypeRefPrim','Seq',Value1),
	    ?line {error,_Reason} = 
		asn1_wrapper:encode('SeqTypeRefPrim','Seq',Value2),
	    ?line {ok,Value1} = 
		asn1_wrapper:decode('SeqTypeRefPrim','Seq',Bytes);
	_ ->
	    ?line {ok,Bytes} = 
		asn1_wrapper:encode('SeqTypeRefPrim','Seq',Value1),
	    ?line {ok,Bytes} = 
		asn1_wrapper:encode('SeqTypeRefPrim','Seq',Value2),
	    ?line {ok,Value1} = 
		asn1_wrapper:decode('SeqTypeRefPrim','Seq',Bytes)
    end,

    ok.
