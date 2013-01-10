%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
-module(testSeqExtension).

-include("External.hrl").
-export([main/2]).

-include_lib("test_server/include/test_server.hrl").

-record('SeqExt1',{}).
-record('SeqExt2',{bool, int}).
-record('SeqExt3',{bool, int}).
-record('SeqExt4',{bool, int}).
-record('SeqExt5',{name, shoesize}).
-record('SeqExt6',{i1,i2,i3,i4,i5,i6,i7}).
-record('SuperSeq',{s1,s2,s3,s4,s5,s6,i}).

main(DataDir, Opts) ->
    roundtrip('SeqExt1', #'SeqExt1'{}),

    roundtrip('SeqExt2', #'SeqExt2'{bool=true,int=99}),
    roundtrip('SeqExt2', #'SeqExt2'{bool=false,int=42}),

    roundtrip('SeqExt3', #'SeqExt3'{bool=true,int=-77777}),
    roundtrip('SeqExt3', #'SeqExt3'{bool=false,int=-42000}),

    roundtrip('SeqExt4', #'SeqExt4'{bool=true,int=12345}),
    roundtrip('SeqExt4', #'SeqExt4'{bool=false,int=123456}),

    roundtrip('SeqExt5', #'SeqExt5'{name="Arne",shoesize=47}),

    %% Encode a value with this version of the specification.
    BigInt = 128638468966,
    SuperSeq = #'SuperSeq'{s1=#'SeqExt1'{},
			   s2=#'SeqExt2'{bool=true,int=2345},
			   s3=#'SeqExt3'{bool=false,int=17},
			   s4=#'SeqExt4'{bool=true,int=38739739},
			   s5=#'SeqExt5'{name="Arne",shoesize=47},
			   s6=#'SeqExt6'{i1=531,i2=601,i3=999,
					 i4=777,i5=11953,
					 i6=13553,i7=77777},
			   i=BigInt
			  },
    {ok,SuperSeqEnc} = 'SeqExtension':encode('SuperSeq', SuperSeq),
    {ok,SuperSeq} = 'SeqExtension':decode('SuperSeq', SuperSeqEnc),

    %% Remove all extensions from the ASN.1 specification and compile it.
    CaseDir = filename:dirname(code:which('SeqExtension')),
    Asn1SrcBase = "SeqExtension.asn1",
    Asn1SrcFile0 = filename:join(DataDir, Asn1SrcBase),
    {ok,Src0} = file:read_file(Asn1SrcFile0),
    %% Remove all declarations following "...," up to the end
    %% of the SEQUENCE.
    Src1 = re:replace(Src0, "[.][.][.],[^}]*", "...\n",
		      [global,{return,binary}]),
    %% Remove the last double bracket group in the SEQUENCE.
    Src = re:replace(Src1, ",\\s*\\[\\[.*?\\]\\]\\s*\\}", "\n}",
		     [global,{return,binary}]),
    io:format("~s\n\n", [Src]),
    Asn1SrcFile = filename:join(CaseDir, Asn1SrcBase),
    ok = file:write_file(Asn1SrcFile, Src),
    ok = asn1ct:compile(Asn1SrcFile,
			[{i,DataDir},{outdir,CaseDir}|Opts]),

    %% Decode the encoded sequence with the version of the spec
    %% with no extensions following the extension marks
    %% (except in SeqExt6). The integer 'i' at the end
    %% of the sequence must still be the correct integer (otherwise
    %% some extension has not been skipped correctly).
    {ok,DecodedSuperSeq} = 'SeqExtension':decode('SuperSeq', SuperSeqEnc),
    #'SuperSeq'{s1={'SeqExt1'},
		s2=#'SeqExt2'{bool=true,int=2345},
		s3={'SeqExt3'},
		s4={'SeqExt4',true},
		s5={'SeqExt5'},
		s6={'SeqExt6',531,601,999,777,11953},
		i=BigInt} = DecodedSuperSeq,
    ok.

roundtrip(Type, Value) ->
    {ok,Encoded} = 'SeqExtension':encode(Type, Value),
    {ok,Value} = 'SeqExtension':decode(Type, Encoded),
    ok.
