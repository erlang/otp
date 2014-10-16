%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014. All Rights Reserved.
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

-module(testRfcs).

-export([compile/3,test/0]).

-include_lib("test_server/include/test_server.hrl").

compile(Config, Erules, Options0) ->
    Options = [no_ok_wrapper|Options0],
    DataDir = ?config(data_dir, Config),
    Specs0 = filelib:wildcard("*.asn1", filename:join(DataDir, rfcs)),
    Specs = [filename:join(rfcs, Spec) || Spec <- Specs0],
    CaseDir = ?config(case_dir, Config),
    asn1_test_lib:compile_all(Specs, Config, [Erules,{i,CaseDir}|Options]).

test() ->
    {1,3,6,1,5,5,7,48,1,2} =
	IdPkixOcspNonce =
	'OCSP-2009':'id-pkix-ocsp-nonce'(),
    roundtrip('OCSP-2009', 'OCSPRequest',
	      {'OCSPRequest',
	       {'TBSRequest',
		0,
		{rfc822Name,"name string"},
		[{'Request',
		  {'CertID',{'_',{2,9,3,4,5},asn1_NOVALUE},
		   <<"POTATOHASH">>,<<"HASHBROWN">>,42},
		  [{'_',IdPkixOcspNonce,true,<<34,159,16,57,199>>}]}],
		asn1_NOVALUE},
	       asn1_NOVALUE}),
    ok.

roundtrip(Module, Type, Value0) ->
    Enc = Module:encode(Type, Value0),
    Value1 = Module:decode(Type, Enc),
    asn1_test_lib:match_value(Value0, Value1),
    ok.
