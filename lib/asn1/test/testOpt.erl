%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
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
-module(testOpt).
-export([main/1]).

-include_lib("test_server/include/test_server.hrl").

-record('Opt1',{bool0, 
		bool1 = asn1_NOVALUE, 
		bool2 = asn1_NOVALUE, 
		bool3 = asn1_NOVALUE}).

-record('Opt2',{bool10, 
		bool11 = asn1_NOVALUE, 
		bool12 = asn1_NOVALUE, 
		bool13}).

-record('Opt3',{bool30 = asn1_NOVALUE, 
		bool31 = asn1_NOVALUE, 
		bool32 = asn1_NOVALUE, 
		bool33 = asn1_NOVALUE}).

main(_Rules) ->
    roundtrip('Opt1', #'Opt1'{bool0=true,bool1=true,bool2=true,bool3=true}),
    roundtrip('Opt1', #'Opt1'{bool0=true,bool1=asn1_NOVALUE,bool2=asn1_NOVALUE,
        bool3=asn1_NOVALUE}),
    roundtrip('Opt1', #'Opt1'{bool0=true,bool1=asn1_NOVALUE,bool2=false,bool3=asn1_NOVALUE}),
    roundtrip('Opt1', #'Opt1'{bool0=false,bool1=asn1_NOVALUE,bool2=asn1_NOVALUE,bool3=false}),

    roundtrip('Opt2', #'Opt2'{bool10=false,bool11=false,bool12=false,bool13=false}),
    roundtrip('Opt2', #'Opt2'{bool10=true,bool11=asn1_NOVALUE,bool12=asn1_NOVALUE,
        bool13=false}),
    roundtrip('Opt2', #'Opt2'{bool10=true,bool11=false,bool12=asn1_NOVALUE,bool13=false}),
    roundtrip('Opt2', #'Opt2'{bool10=false,bool11=asn1_NOVALUE,bool12=false,bool13=false}),

    roundtrip('Opt3', #'Opt3'{bool30=false,bool31=false,bool32=false,bool33=false}),
    roundtrip('Opt3', #'Opt3'{bool30=asn1_NOVALUE,bool31=asn1_NOVALUE,bool32=asn1_NOVALUE,
        bool33=asn1_NOVALUE}),
    roundtrip('Opt3', #'Opt3'{bool30=true,bool31=asn1_NOVALUE,bool32=asn1_NOVALUE,
        bool33=asn1_NOVALUE}),
    roundtrip('Opt3', #'Opt3'{bool30=asn1_NOVALUE,bool31=asn1_NOVALUE,bool32=false,
        bool33=asn1_NOVALUE}),
    roundtrip('Opt3', #'Opt3'{bool30=asn1_NOVALUE,bool31=asn1_NOVALUE,bool32=asn1_NOVALUE,
        bool33=false}),
    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('Opt', Type, Value).
