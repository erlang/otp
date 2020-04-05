%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2017. All Rights Reserved.
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

-module(base64_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0, groups/0, group/1]).

%% Test cases must be exported.
-export([base64_encode/1, base64_decode/1, base64_otp_5635/1,
	 base64_otp_6279/1, big/1, illegal/1, mime_decode/1,
	 mime_decode_to_string/1,
	 roundtrip_1/1, roundtrip_2/1, roundtrip_3/1, roundtrip_4/1]).

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() ->
    [base64_encode, base64_decode, base64_otp_5635,
     base64_otp_6279, big, illegal, mime_decode, mime_decode_to_string,
     {group, roundtrip}].

groups() ->
    [{roundtrip, [parallel],
      [roundtrip_1, roundtrip_2, roundtrip_3, roundtrip_4]}].

group(roundtrip) ->
    %% valgrind needs a lot of time
    [{timetrap,{minutes,10}}].

%%-------------------------------------------------------------------------
%% Test base64:encode/1.
base64_encode(Config) when is_list(Config) ->
    %% Two pads
    <<"QWxhZGRpbjpvcGVuIHNlc2FtZQ==">> =
	base64:encode("Aladdin:open sesame"),
    %% One pad
    <<"SGVsbG8gV29ybGQ=">> = base64:encode(<<"Hello World">>),
    %% No pad
    "QWxhZGRpbjpvcGVuIHNlc2Ft" =
	base64:encode_to_string("Aladdin:open sesam"),

    "MDEyMzQ1Njc4OSFAIzBeJiooKTs6PD4sLiBbXXt9" =
	base64:encode_to_string(<<"0123456789!@#0^&*();:<>,. []{}">>),
    ok.
%%-------------------------------------------------------------------------
%% Test base64:decode/1.
base64_decode(Config) when is_list(Config) ->
    %% Two pads
    <<"Aladdin:open sesame">> =
	base64:decode("QWxhZGRpbjpvcGVuIHNlc2FtZQ=="),
    %% One pad
    <<"Hello World">> = base64:decode(<<"SGVsbG8gV29ybGQ=">>),
    %% No pad
    <<"Aladdin:open sesam">> =
	base64:decode("QWxhZGRpbjpvcGVuIHNlc2Ft"),

    Alphabet = list_to_binary(lists:seq(0, 255)),
    Alphabet = base64:decode(base64:encode(Alphabet)),

    %% Encoded base 64 strings may be divided by non base 64 chars.
    %% In this cases whitespaces.
    "0123456789!@#0^&*();:<>,. []{}" =
	base64:decode_to_string(
	  "MDEy MzQ1Njc4 \tOSFAIzBeJ \niooKTs6 PD4sLi \r\nBbXXt9"),
    "0123456789!@#0^&*();:<>,. []{}" =
	base64:decode_to_string(
	  <<"MDEy MzQ1Njc4 \tOSFAIzBeJ \niooKTs6 PD4sLi \r\nBbXXt9">>),
    ok.
%%-------------------------------------------------------------------------
%% OTP-5635: Some data doesn't pass through base64:decode/1 correctly.
base64_otp_5635(Config) when is_list(Config) ->
    <<"===">> = base64:decode(base64:encode("===")),
    ok.
%%-------------------------------------------------------------------------
%% OTP-6279: Make sure illegal characters are rejected when decoding.
base64_otp_6279(Config) when is_list(Config) ->
    {'EXIT',_} = (catch base64:decode("dGVzda==a")),
    ok.
%%-------------------------------------------------------------------------
%% Encode and decode big binaries.
big(Config) when is_list(Config) ->
    Big = make_big_binary(300000),
    B = base64:encode(Big),
    true = is_binary(B),
    400000 = byte_size(B),
    Big = base64:decode(B),
    Big = base64:mime_decode(B),
    ok.
%%-------------------------------------------------------------------------
%% Make sure illegal characters are rejected when decoding.
illegal(Config) when is_list(Config) ->
    %% A few samples with different error reasons. Nothing can be
    %% assumed about the reason for the crash.
    {'EXIT',_} = (catch base64:decode("()")),
    {'EXIT',_} = (catch base64:decode(<<19:8,20:8,21:8,22:8>>)),
    {'EXIT',_} = (catch base64:decode([19,20,21,22])),
    {'EXIT',_} = (catch base64:decode_to_string(<<19:8,20:8,21:8,22:8>>)),
    {'EXIT',_} = (catch base64:decode_to_string([19,20,21,22])),
    ok.
%%-------------------------------------------------------------------------
%% mime_decode and mime_decode_to_string have different implementations
%% so test both with the same input separately.
%%
%% Test base64:mime_decode/1.
mime_decode(Config) when is_list(Config) ->
    MimeDecode = fun(In) ->
                         Out = base64:mime_decode(In),
                         Out = base64:mime_decode(binary_to_list(In))
                 end,
    %% Test correct padding
    <<"one">> = MimeDecode(<<"b25l">>),
    <<"on">>  = MimeDecode(<<"b24=">>),
    <<"o">>   = MimeDecode(<<"bw==">>),
    %% Test 1 extra padding
    <<"one">> = MimeDecode(<<"b25l= =">>),
    <<"on">>  = MimeDecode(<<"b24== =">>),
    <<"o">>   = MimeDecode(<<"bw=== =">>),
    %% Test 2 extra padding
    <<"one">> = MimeDecode(<<"b25l===">>),
    <<"on">>  = MimeDecode(<<"b24====">>),
    <<"o">>   = MimeDecode(<<"bw=====">>),
    %% Test misc embedded padding
    <<"one">> = MimeDecode(<<"b2=5l===">>),
    <<"on">>  = MimeDecode(<<"b=24====">>),
    <<"o">>   = MimeDecode(<<"b=w=====">>),
    %% Test misc white space and illegals with embedded padding
    <<"one">> = MimeDecode(<<" b~2=\r\n5()l===">>),
    <<"on">>  = MimeDecode(<<"\tb =2\"¤4=¤=   ==">>),
    <<"o">>   = MimeDecode(<<"\nb=w=====">>),
    %% Two pads
    <<"Aladdin:open sesame">> =
	MimeDecode(<<"QWxhZGRpbjpvc()GVuIHNlc2FtZQ==">>),
    %% One pad to ignore, followed by more text
    <<"Hello World!!">> = MimeDecode(<<"SGVsb)(G8gV29ybGQ=h IQ= =">>),
    %% No pad
    <<"Aladdin:open sesam">> =
	MimeDecode(<<"QWxhZGRpbjpvcG¤\")(VuIHNlc2Ft">>),
    %% Encoded base 64 strings may be divided by non base 64 chars.
    %% In this cases whitespaces.
    <<"0123456789!@#0^&*();:<>,. []{}">> =
	MimeDecode(
	  <<"MDEy MzQ1Njc4 \tOSFAIzBeJ \nio)(oKTs6 PD4sLi \r\nBbXXt9">>),
    %% Zeroes
    <<"012">> = MimeDecode(<<"\000M\000D\000E\000y=\000">>),
    <<"o">>   = MimeDecode(<<"bw==\000">>),
    <<"o">>   = MimeDecode(<<"bw=\000=">>),
    ok.

%%-------------------------------------------------------------------------

%% Repeat of mime_decode() tests

%% Test base64:mime_decode_to_string/1.
mime_decode_to_string(Config) when is_list(Config) ->
    MimeDecodeToString =
        fun(In) ->
                Out = base64:mime_decode_to_string(In),
                Out = base64:mime_decode_to_string(binary_to_list(In))
        end,
    %% Test correct padding
    "one" = MimeDecodeToString(<<"b25l">>),
    "on"  = MimeDecodeToString(<<"b24=">>),
    "o"   = MimeDecodeToString(<<"bw==">>),
    %% Test 1 extra padding
    "one" = MimeDecodeToString(<<"b25l= =">>),
    "on"  = MimeDecodeToString(<<"b24== =">>),
    "o"   = MimeDecodeToString(<<"bw=== =">>),
    %% Test 2 extra padding
    "one" = MimeDecodeToString(<<"b25l===">>),
    "on"  = MimeDecodeToString(<<"b24====">>),
    "o"   = MimeDecodeToString(<<"bw=====">>),
    %% Test misc embedded padding
    "one" = MimeDecodeToString(<<"b2=5l===">>),
    "on"  = MimeDecodeToString(<<"b=24====">>),
    "o"   = MimeDecodeToString(<<"b=w=====">>),
    %% Test misc white space and illegals with embedded padding
    "one" = MimeDecodeToString(<<" b~2=\r\n5()l===">>),
    "on"  = MimeDecodeToString(<<"\tb =2\"¤4=¤=   ==">>),
    "o"   = MimeDecodeToString(<<"\nb=w=====">>),
    %% Two pads
    "Aladdin:open sesame" =
	MimeDecodeToString(<<"QWxhZGRpbjpvc()GVuIHNlc2FtZQ==">>),
    %% One pad to ignore, followed by more text
    "Hello World!!" = MimeDecodeToString(<<"SGVsb)(G8gV29ybGQ=h IQ= =">>),
    %% No pad
    "Aladdin:open sesam" =
	MimeDecodeToString(<<"QWxhZGRpbjpvcG¤\")(VuIHNlc2Ft">>),
    %% Encoded base 64 strings may be divided by non base 64 chars.
    %% In this cases whitespaces.
    "0123456789!@#0^&*();:<>,. []{}" =
	MimeDecodeToString(
	  <<"MDEy MzQ1Njc4 \tOSFAIzBeJ \nio)(oKTs6 PD4sLi \r\nBbXXt9">>),
    %% Zeroes
    "012" = MimeDecodeToString(<<"\000M\000D\000E\000y=\000">>),
    "o"   = MimeDecodeToString(<<"bw==\000">>),
    "o"   = MimeDecodeToString(<<"bw=\000=">>),
    ok.

%%-------------------------------------------------------------------------

roundtrip_1(Config) when is_list(Config) ->
    do_roundtrip(1).

roundtrip_2(Config) when is_list(Config) ->
    do_roundtrip(2).

roundtrip_3(Config) when is_list(Config) ->
    do_roundtrip(3).

roundtrip_4(Config) when is_list(Config) ->
    do_roundtrip(4).

do_roundtrip(Offset) ->
    Sizes = lists:seq(Offset, 255, 4) ++ lists:seq(2400-6+Offset, 2440, 4),
    do_roundtrip_1(Sizes, []).

do_roundtrip_1([NextSize|Sizes], Current) ->
    Len = length(Current),
    io:format("~p", [Len]),
    do_roundtrip_2(Current),
    Next = random_byte_list(NextSize - Len, Current),
    do_roundtrip_1(Sizes, Next);
do_roundtrip_1([], Last) ->
    io:format("~p", [length(Last)]),
    do_roundtrip_2(Last).

do_roundtrip_2(List) ->
    Bin = list_to_binary(List),
    Base64Bin = base64:encode(List),
    Base64Bin = base64:encode(Bin),
    Base64List = base64:encode_to_string(List),
    Base64Bin = list_to_binary(Base64List),
    Bin = base64:decode(Base64Bin),
    List = base64:decode_to_string(Base64Bin),
    Bin = base64:mime_decode(Base64Bin),
    List = base64:mime_decode_to_string(Base64Bin),
    append_roundtrip(8, Bin, List, Base64Bin),
    prepend_roundtrip(8, Bin, List, Base64List),
    interleaved_ws_roundtrip(Bin, List, Base64List).

append_roundtrip(0, _, _, _) -> ok;
append_roundtrip(N, Bin, List, Base64Bin0) ->
    Base64Bin = <<Base64Bin0/binary,"\n">>,
    Bin = base64:decode(Base64Bin),
    List = base64:decode_to_string(Base64Bin),
    Bin = base64:mime_decode(Base64Bin),
    List = base64:mime_decode_to_string(Base64Bin),

    Base64List = binary_to_list(Base64Bin),
    Bin = base64:decode(Base64List),
    List = base64:decode_to_string(Base64List),
    Bin = base64:mime_decode(Base64List),
    List = base64:mime_decode_to_string(Base64List),
    append_roundtrip(N-1, Bin, List, Base64Bin).

prepend_roundtrip(0, _, _, _) -> ok;
prepend_roundtrip(N, Bin, List, Base64List0) ->
    Base64List = [$\s|Base64List0],
    Bin = base64:decode(Base64List),
    List = base64:decode_to_string(Base64List),
    Bin = base64:mime_decode(Base64List),
    List = base64:mime_decode_to_string(Base64List),

    Base64Bin = list_to_binary(Base64List),
    Bin = base64:decode(Base64Bin),
    List = base64:decode_to_string(Base64Bin),
    Bin = base64:mime_decode(Base64Bin),
    List = base64:mime_decode_to_string(Base64Bin),
    prepend_roundtrip(N-1, Bin, List, Base64List).

%% Do an exhaustive test of interleaving whitespace (for short strings).
interleaved_ws_roundtrip(Bin, List, Base64List) when byte_size(Bin) =< 6 ->
    interleaved_ws_roundtrip_1(lists:reverse(Base64List), [], Bin, List);
interleaved_ws_roundtrip(_, _, _) -> ok.

interleaved_ws_roundtrip_1([H|T], Tail, Bin, List) ->
    interleaved_ws_roundtrip_1(T, [H|Tail], Bin, List),
    interleaved_ws_roundtrip_1(T, [H,$\s|Tail], Bin, List),
    interleaved_ws_roundtrip_1(T, [H,$\s,$\t|Tail], Bin, List),
    interleaved_ws_roundtrip_1(T, [H,$\n,$\t|Tail], Bin, List);
interleaved_ws_roundtrip_1([], Base64List, Bin, List) ->
    Bin = base64:decode(Base64List),
    List = base64:decode_to_string(Base64List),
    Bin = base64:mime_decode(Base64List),
    List = base64:mime_decode_to_string(Base64List),

    Base64Bin = list_to_binary(Base64List),
    Bin = base64:decode(Base64Bin),
    List = base64:decode_to_string(Base64Bin),
    Bin = base64:mime_decode(Base64Bin),
    List = base64:mime_decode_to_string(Base64Bin),
    ok.

random_byte_list(0, Acc) ->
    Acc;
random_byte_list(N, Acc) ->
    random_byte_list(N-1, [rand:uniform(255)|Acc]).

make_big_binary(N) ->
    list_to_binary(mbb(N, [])).

mbb(N, Acc) when N > 256 ->
    B = list_to_binary(lists:seq(0, 255)),
    mbb(N - 256, [B | Acc]);
mbb(N, Acc) ->
    B = list_to_binary(lists:seq(0, N-1)),
    lists:reverse(Acc, B).
