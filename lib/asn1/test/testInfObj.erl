%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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

-module(testInfObj).

-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('InitiatingMessage2',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{first,second}).

main(_Erule) ->
    Val1 = #'InitiatingMessage'{procedureCode=1,
				criticality=ignore,
				value=#'Iu-ReleaseCommand'{
				  first=13,
				  second=true}},
    ?line {ok,Bytes1} = 
	asn1_wrapper:encode('RANAPextract1','InitiatingMessage',Val1),
    
    ?line {ok,{'InitiatingMessage',1,ignore,{'Iu-ReleaseCommand',13,true}}}=
	asn1_wrapper:decode('RANAPextract1','InitiatingMessage',Bytes1),
    
    ?line {ok,Bytes2} =
	asn1_wrapper:encode('InfObj','InitiatingMessage',Val1),
    
    ?line {ok,Val1} =
	asn1_wrapper:decode('InfObj','InitiatingMessage',Bytes2),

    Val2 = Val1#'InitiatingMessage'{procedureCode=2},
    
    ?line {error,_R1} =
	asn1_wrapper:encode('InfObj','InitiatingMessage',Val2),
    

    %% Test case for OTP-4275
    Val3 = #'InitiatingMessage2'{procedureCode=3,
				 criticality=reject,
				 value=#'Iu-ReleaseCommand'{
				   first=13,
				   second=true}},

    ?line {ok,Bytes3} = 
	asn1_wrapper:encode('RANAPextract1','InitiatingMessage2',Val3),

    
    ?line {ok,{'InitiatingMessage2',3,reject,{'Iu-ReleaseCommand',13,true}}}=
	asn1_wrapper:decode('RANAPextract1','InitiatingMessage2',Bytes3).
    
