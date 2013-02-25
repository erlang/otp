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

-record('InitiatingMessage',{procedureCode,criticality,value}).
-record('InitiatingMessage2',{procedureCode,criticality,value}).
-record('Iu-ReleaseCommand',{first,second}).

main(_Erule) ->
    Val1 = #'InitiatingMessage'{procedureCode=1,
				criticality=ignore,
				value=#'Iu-ReleaseCommand'{
				  first=13,
				  second=true}},
    roundtrip('RANAPextract1', 'InitiatingMessage', Val1),
    roundtrip('InfObj', 'InitiatingMessage', Val1),

    Val2 = Val1#'InitiatingMessage'{procedureCode=2},
    {error,_R1} = 'InfObj':encode('InitiatingMessage', Val2),
    

    %% Test case for OTP-4275
    Val3 = #'InitiatingMessage2'{procedureCode=3,
				 criticality=reject,
				 value=#'Iu-ReleaseCommand'{
				   first=13,
				   second=true}},

    roundtrip('RANAPextract1', 'InitiatingMessage2', Val3),

    roundtrip('InfObj', 'MyPdu', {'MyPdu',42,12,false,"string"}),
    roundtrip('InfObj', 'MyPdu', {'MyPdu',{'Seq',1023,"hello"},
				  42,true,"longer string"}).


roundtrip(M, T, V) ->
    {ok,Enc} = M:encode(T, V),
    {ok,V} = M:decode(T, Enc),
    ok.
