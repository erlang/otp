%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2013. All Rights Reserved.
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

-module(base64_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2]).

%% Test cases must be exported.
-export([base64_encode/1, base64_decode/1, base64_otp_5635/1,
	 base64_otp_6279/1, big/1, illegal/1, mime_decode/1,
	 mime_decode_to_string/1, roundtrip/1]).

init_per_testcase(_, Config) ->
    Dog = test_server:timetrap(?t:minutes(4)),
    NewConfig = lists:keydelete(watchdog, 1, Config),
    [{watchdog, Dog} | NewConfig].

end_per_testcase(_, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [base64_encode, base64_decode, base64_otp_5635,
     base64_otp_6279, big, illegal, mime_decode, mime_decode_to_string,
     roundtrip].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%-------------------------------------------------------------------------
base64_encode(doc) ->
    ["Test base64:encode/1."];
base64_encode(suite) ->
    [];
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
base64_decode(doc) ->
    ["Test base64:decode/1."];
base64_decode(suite) ->
    [];
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

    %% Encoded base 64 strings may be devided by non base 64 chars.
    %% In this cases whitespaces.
    "0123456789!@#0^&*();:<>,. []{}" =
	base64:decode_to_string(
	  "MDEy MzQ1Njc4 \tOSFAIzBeJ \niooKTs6 PD4sLi \r\nBbXXt9"),
    "0123456789!@#0^&*();:<>,. []{}" =
	base64:decode_to_string(
	  <<"MDEy MzQ1Njc4 \tOSFAIzBeJ \niooKTs6 PD4sLi \r\nBbXXt9">>),
    ok.
%%-------------------------------------------------------------------------
base64_otp_5635(doc) ->
    ["OTP-5635: Some data doesn't pass through base64:decode/1 "
     "correctly"];
base64_otp_5635(suite) ->
    [];
base64_otp_5635(Config) when is_list(Config) ->
    <<"===">> = base64:decode(base64:encode("===")),
    ok.
%%-------------------------------------------------------------------------
base64_otp_6279(doc) ->
    ["OTP-6279: Guard needed so that function fails in a correct"
     "way for faulty input i.e. function_clause"];
base64_otp_6279(suite) ->
    [];
base64_otp_6279(Config) when is_list(Config) ->
    {'EXIT',{function_clause, _}} = (catch base64:decode("dGVzda==a")),
    ok.
%%-------------------------------------------------------------------------
big(doc) ->
    ["Encode and decode big binaries."];
big(suite) ->
    [];
big(Config) when is_list(Config) ->
    Big = make_big_binary(300000),
    B = base64:encode(Big),
    true = is_binary(B),
    400000 = byte_size(B),
    Big = base64:decode(B),
    Big = base64:mime_decode(B),
    ok.
%%-------------------------------------------------------------------------
illegal(doc) ->
    ["Make sure illegal characters are rejected when decoding."];
illegal(suite) ->
    [];
illegal(Config) when is_list(Config) ->
    {'EXIT',{function_clause, _}} = (catch base64:decode("()")),
    ok.
%%-------------------------------------------------------------------------
%% mime_decode and mime_decode_to_string have different implementations
%% so test both with the same input separately. Both functions have
%% the same implementation for binary/string arguments.
mime_decode(doc) ->
    ["Test base64:mime_decode/1."];
mime_decode(suite) ->
    [];
mime_decode(Config) when is_list(Config) ->
    %% Test correct padding
    <<"one">> = base64:mime_decode(<<"b25l">>),
    <<"on">>  = base64:mime_decode(<<"b24=">>),
    <<"o">>   = base64:mime_decode(<<"bw==">>),
    %% Test 1 extra padding
    <<"one">> = base64:mime_decode(<<"b25l= =">>),
    <<"on">>  = base64:mime_decode(<<"b24== =">>),
    <<"o">>   = base64:mime_decode(<<"bw=== =">>),
    %% Test 2 extra padding
    <<"one">> = base64:mime_decode(<<"b25l===">>),
    <<"on">>  = base64:mime_decode(<<"b24====">>),
    <<"o">>   = base64:mime_decode(<<"bw=====">>),
    %% Test misc embedded padding
    <<"one">> = base64:mime_decode(<<"b2=5l===">>),
    <<"on">>  = base64:mime_decode(<<"b=24====">>),
    <<"o">>   = base64:mime_decode(<<"b=w=====">>),
    %% Test misc white space and illegals with embedded padding
    <<"one">> = base64:mime_decode(<<" b~2=\r\n5()l===">>),
    <<"on">>  = base64:mime_decode(<<"\tb =2\"¤4=¤=   ==">>),
    <<"o">>   = base64:mime_decode(<<"\nb=w=====">>),
    %% Two pads
    <<"Aladdin:open sesame">> =
	base64:mime_decode("QWxhZGRpbjpvc()GVuIHNlc2FtZQ=="),
    %% One pad to ignore, followed by more text
    <<"Hello World!!">> = base64:mime_decode(<<"SGVsb)(G8gV29ybGQ=h IQ= =">>),
    %% No pad
    <<"Aladdin:open sesam">> =
	base64:mime_decode("QWxhZGRpbjpvcG¤\")(VuIHNlc2Ft"),
    %% Encoded base 64 strings may be divided by non base 64 chars.
    %% In this cases whitespaces.
    <<"0123456789!@#0^&*();:<>,. []{}">> =
	base64:mime_decode(
	  <<"MDEy MzQ1Njc4 \tOSFAIzBeJ \nio)(oKTs6 PD4sLi \r\nBbXXt9">>),
    ok.

%%-------------------------------------------------------------------------

%% Repeat of mime_decode() tests
mime_decode_to_string(doc) ->
    ["Test base64:mime_decode_to_string/1."];
mime_decode_to_string(suite) ->
    [];
mime_decode_to_string(Config) when is_list(Config) ->
    %% Test correct padding
    "one" = base64:mime_decode_to_string(<<"b25l">>),
    "on"  = base64:mime_decode_to_string(<<"b24=">>),
    "o"   = base64:mime_decode_to_string(<<"bw==">>),
    %% Test 1 extra padding
    "one" = base64:mime_decode_to_string(<<"b25l= =">>),
    "on"  = base64:mime_decode_to_string(<<"b24== =">>),
    "o"   = base64:mime_decode_to_string(<<"bw=== =">>),
    %% Test 2 extra padding
    "one" = base64:mime_decode_to_string(<<"b25l===">>),
    "on"  = base64:mime_decode_to_string(<<"b24====">>),
    "o"   = base64:mime_decode_to_string(<<"bw=====">>),
    %% Test misc embedded padding
    "one" = base64:mime_decode_to_string(<<"b2=5l===">>),
    "on"  = base64:mime_decode_to_string(<<"b=24====">>),
    "o"   = base64:mime_decode_to_string(<<"b=w=====">>),
    %% Test misc white space and illegals with embedded padding
    "one" = base64:mime_decode_to_string(<<" b~2=\r\n5()l===">>),
    "on"  = base64:mime_decode_to_string(<<"\tb =2\"¤4=¤=   ==">>),
    "o"   = base64:mime_decode_to_string(<<"\nb=w=====">>),
    %% Two pads
    "Aladdin:open sesame" =
	base64:mime_decode_to_string("QWxhZGRpbjpvc()GVuIHNlc2FtZQ=="),
    %% One pad to ignore, followed by more text
    "Hello World!!" = base64:mime_decode_to_string(<<"SGVsb)(G8gV29ybGQ=h IQ= =">>),
    %% No pad
    "Aladdin:open sesam" = 
	base64:mime_decode_to_string("QWxhZGRpbjpvcG¤\")(VuIHNlc2Ft"),
    %% Encoded base 64 strings may be divided by non base 64 chars.
    %% In this cases whitespaces.
    "0123456789!@#0^&*();:<>,. []{}" =
	base64:mime_decode_to_string(
	  <<"MDEy MzQ1Njc4 \tOSFAIzBeJ \nio)(oKTs6 PD4sLi \r\nBbXXt9">>),
    ok.

%%-------------------------------------------------------------------------

roundtrip(Config) when is_list(Config) ->
    Sizes = lists:seq(1, 255) ++ lists:seq(2400-5, 2440),
    roundtrip_1(Sizes, []).

roundtrip_1([NextSize|Sizes], Current) ->
    Len = length(Current),
    io:format("~p", [Len]),
    do_roundtrip(Current),
    Next = random_byte_list(NextSize - Len, Current),
    roundtrip_1(Sizes, Next);
roundtrip_1([], Last) ->
    io:format("~p", [length(Last)]),
    do_roundtrip(Last).

do_roundtrip(List) ->
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
    random_byte_list(N-1, [random:uniform(255)|Acc]).

make_big_binary(N) ->
    list_to_binary(mbb(N, [])).

mbb(N, Acc) when N > 256 ->
    B = list_to_binary(lists:seq(0, 255)),
    mbb(N - 256, [B | Acc]);
mbb(N, Acc) ->
    B = list_to_binary(lists:seq(0, N-1)),
    lists:reverse(Acc, B).
