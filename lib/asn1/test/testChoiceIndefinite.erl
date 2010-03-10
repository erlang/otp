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
-module(testChoiceIndefinite).

-export([compile/3]).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").


compile(Config,Rules,Options) ->

    ?line DataDir = ?config(data_dir,Config),
    ?line OutDir = ?config(priv_dir,Config),
    ?line true = code:add_patha(?config(priv_dir,Config)),
    ?line ok = asn1ct:compile(DataDir ++ "ChoiceIndef",
			      [Rules,{outdir,OutDir}]++Options).

main(per_bin) -> ok;
main(per) -> ok;
main(ber_bin_v2) ->
    main(ber);
main(ber_bin) ->
    main(ber);
main(ber) ->
    %% Test case related to OTP-4358
    %% normal encoding
    B = [48,8,160,3,128,1,11,129,1,12],
    %% indefinite length encoding
    Bi = [48,128,160,128,128,1,11,0,0,129,1,12,0,0],
    %% the value which is encoded
    V = {'Seq',{ca,11},12},
    ?line {ok,V} = asn1_wrapper:decode('ChoiceIndef','Seq',B),
    ?line {ok,V} = asn1_wrapper:decode('ChoiceIndef','Seq',Bi),
    ok.



