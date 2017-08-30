%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%
-module(testSeqExtension).

-include("External.hrl").
-export([main/3]).

-include_lib("common_test/include/ct.hrl").

-record('SeqExt1',{}).
-record('SeqExt2',{bool, int}).
-record('SeqExt3',{bool, int}).
-record('SeqExt4',{bool, int}).
-record('SeqExt5',{name, shoesize}).
-record('SeqExt6',{i1,i2,i3,i4,i5,i6,i7}).
-record('SeqExt7',{a=asn1_NOVALUE,b=asn1_NOVALUE,c}).
-record('SuperSeq',{s1,s2,s3,s4,s5,s6,i}).

main(Erule, DataDir, Opts) ->
    roundtrip('SeqExt1', #'SeqExt1'{}),

    roundtrip('SeqExt2', #'SeqExt2'{bool=true,int=99}),
    roundtrip('SeqExt2', #'SeqExt2'{bool=false,int=42}),

    roundtrip('SeqExt3', #'SeqExt3'{bool=true,int=-77777}),
    roundtrip('SeqExt3', #'SeqExt3'{bool=false,int=-42000}),

    roundtrip('SeqExt4', #'SeqExt4'{bool=true,int=12345}),
    roundtrip('SeqExt4', #'SeqExt4'{bool=false,int=123456}),

    case Erule of
        ber ->
            %% BER currently does not handle Extension Addition Groups
            %% correctly.
            ok;
        _ ->
            v_roundtrip3('SeqExt5', #'SeqExt5'{name=asn1_NOVALUE,
                                               shoesize=asn1_NOVALUE},
                         Erule, #{per=>"00",
                                  uper=>"00"}),
            v_roundtrip3('SeqExt7', #'SeqExt7'{c=asn1_NOVALUE},
                         Erule, #{per=>"00",
                                  uper=>"00"})
    end,
    roundtrip('SeqExt5', #'SeqExt5'{name = <<"Arne">>,shoesize=47}),

    v_roundtrip3('SeqExt7', #'SeqExt7'{c=false},
                 Erule, #{per=>"80800100",
                          uper=>"80808000"}),
    v_roundtrip3('SeqExt7', #'SeqExt7'{c=true},
                 Erule, #{per=>"80800120",
                          uper=>"80809000"}),
    v_roundtrip3('SeqExt7', #'SeqExt7'{a=777,b = <<16#AA>>,c=false},
                 Erule, #{per=>"808006C0 030901AA 00",
                          uper=>"8082E061 20354000"}),
    v_roundtrip3('SeqExt7', #'SeqExt7'{a=8888,c=false},
                 Erule, #{per=>"80800480 22B800",
                          uper=>"8081C457 0000"}),

    %% Encode a value with this version of the specification.
    BigInt = 128638468966,
    SuperSeq = #'SuperSeq'{s1=#'SeqExt1'{},
			   s2=#'SeqExt2'{bool=true,int=2345},
			   s3=#'SeqExt3'{bool=false,int=17},
			   s4=#'SeqExt4'{bool=true,int=38739739},
			   s5=#'SeqExt5'{name = <<"Arne">>,shoesize=47},
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


    %% Test more than 64 extensions.
    roundtrip2('SeqExt66',
	       list_to_tuple(['SeqExt66'|lists:seq(0, 65)])),
    v_roundtrip2(Erule, 'SeqExt66',
		 list_to_tuple(['SeqExt66'|
				lists:duplicate(65, asn1_NOVALUE)++[125]])),
    roundtrip2('SeqExt130',
	       list_to_tuple(['SeqExt130'|lists:seq(0, 129)])),
    v_roundtrip2(Erule, 'SeqExt130',
		 list_to_tuple(['SeqExt130'|
				lists:duplicate(129, asn1_NOVALUE)++[199]])),

    ok.

roundtrip(Type, Value) ->
    asn1_test_lib:roundtrip('SeqExtension', Type, Value).

v_roundtrip2(Erule, Type, Value) ->
    Encoded = asn1_test_lib:hex_to_bin(v(Erule, Type)),
    Encoded = roundtrip2(Type, Value).

roundtrip2(Type, Value) ->
    asn1_test_lib:roundtrip_enc('SeqExtension2', Type, Value).

v_roundtrip3(Type, Value, Erule, Map) ->
    case maps:find(Erule, Map) of
        {ok,Hex} ->
            Encoded = asn1_test_lib:hex_to_bin(Hex),
            Encoded = asn1_test_lib:roundtrip_enc('SeqExtension', Type, Value);
        error ->
            asn1_test_lib:roundtrip('SeqExtension', Type, Value)
    end.

v(ber, 'SeqExt66') ->  "30049F41 017D";
v(per, 'SeqExt66') ->  "C0420000 00000000 00004001 FA";
v(uper, 'SeqExt66') -> "D0800000 00000000 00101FA0";
v(ber, 'SeqExt130') ->  "30069F81 010200C7";
v(per, 'SeqExt130') ->  "C0808200 00000000 00000000 00000000 00000040 01C7";
v(uper, 'SeqExt130') -> "E0208000 00000000 00000000 00000000 0000101C 70".
