%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
-module(ber_decode_error).

-export([run/1, compile/3]).

-include_lib("test_server/include/test_server.hrl").

compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "Constructed",
			      [Rules,{outdir,OutDir}]++Options).


run([]) ->
    ?line {ok,B}  = asn1_wrapper:encode('Constructed','S3',{'S3',17}),
    ?line [T,L|V] = lists:flatten(B),
    ?line Bytes = [T,L+3|V] ++ [2,1,3],
    ?line case asn1_wrapper:decode('Constructed','S3',Bytes) of
	      {error,{asn1,{unexpected,_}}} -> ok
	  end,
    %% Unexpected bytes must be accepted if there is an extensionmark
    ?line {ok,{'S3ext',17}} = asn1_wrapper:decode('Constructed','S3ext',Bytes),
    ok; 
run([driver]) ->
    %% test of OTP-4797, bad indata to driver does not cause an EXIT
    ?line {error,_Reason} = asn1rt:decode('Constructed','S3',[3,5]),
    ok.








