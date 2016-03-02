%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

-module(bs_utf_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1,end_per_suite/1,
	 init_per_testcase/2,end_per_testcase/2,
	 utf8_roundtrip/1,unused_utf_char/1,utf16_roundtrip/1,
	 utf32_roundtrip/1,guard/1,extreme_tripping/1]).

-include_lib("common_test/include/ct.hrl").
-compile([no_jopt,time]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [utf8_roundtrip, unused_utf_char, utf16_roundtrip,
     utf32_roundtrip, guard, extreme_tripping].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

utf8_roundtrip(Config) when is_list(Config) ->
    [utf8_roundtrip_1(P) || P <- utf_data()],
    ok.

utf8_roundtrip_1({Str,Bin,Bin}) ->
    Str = utf8_to_list(Bin),
    Bin = list_to_utf8(Str),
    [ok = utf8_guard(C, <<42,C/utf8>>) || C <- Str],
    [error = utf8_guard(C, <<C/utf8>>) || C <- Str],
    ok.

utf8_guard(C, Bin) when <<42,C/utf8>> =:= Bin -> ok;
utf8_guard(_, _) -> error.

utf8_to_list(<<C/utf8,T/binary>>) ->
    [C|utf8_to_list(T)];
utf8_to_list(<<>>) -> [].

list_to_utf8(L) ->
    list_to_utf8(L, <<>>).

list_to_utf8([H|T], Bin) ->
    list_to_utf8(T, <<Bin/binary,H/utf8>>);
list_to_utf8([], Bin) -> Bin.

unused_utf_char(Config) when is_list(Config) ->
    [true = utf8_len(Utf8) =:= length(Str) ||
	{Str,Utf8} <- utf_data()],
    ok.

utf8_len(B) ->
    utf8_len(B, 0).

utf8_len(<<_/utf8,T/binary>>, N) ->
    utf8_len(T, N+1);
utf8_len(<<>>, N) -> N.

utf16_roundtrip(Config) when is_list(Config) ->
    {Str,Big,Big,Little,Little} = utf16_data(),
    4 = utf16_big_len(Big),
    4 = utf16_little_len(Little),
    Str = big_utf16_to_list(Big),
    Str = little_utf16_to_list(Little),

    Big = list_to_big_utf16(Str),
    Little = list_to_little_utf16(Str),

    ok.

utf16_big_len(B) ->
    utf16_big_len(B, 0).

utf16_big_len(<<_/utf16,T/binary>>, N) ->
    utf16_big_len(T, N+1);
utf16_big_len(<<>>, N) -> N.

utf16_little_len(B) ->
    utf16_little_len(B, 0).

utf16_little_len(<<_/little-utf16,T/binary>>, N) ->
    utf16_little_len(T, N+1);
utf16_little_len(<<>>, N) -> N.

list_to_big_utf16(List) ->
    list_to_big_utf16(List, <<>>).

list_to_big_utf16([H|T], Bin) ->
    list_to_big_utf16(T, <<Bin/binary,H/utf16>>);
list_to_big_utf16([], Bin) -> Bin.

list_to_little_utf16(List) ->
    list_to_little_utf16(List, <<>>).

list_to_little_utf16([H|T], Bin) ->
    list_to_little_utf16(T, <<Bin/binary,H/little-utf16>>);
list_to_little_utf16([], Bin) -> Bin.

big_utf16_to_list(<<H/utf16,T/binary>>) ->
    [H|big_utf16_to_list(T)];
big_utf16_to_list(<<>>) -> [].

little_utf16_to_list(<<H/little-utf16,T/binary>>) ->
    [H|little_utf16_to_list(T)];
little_utf16_to_list(<<>>) -> [].

utf32_roundtrip(Config) when is_list(Config) ->
    {Str,Big,Big,Little,Little} = utf32_data(),
    4 = utf32_big_len(Big),
    4 = utf32_little_len(Little),
    Str = big_utf32_to_list(Big),
    Str = little_utf32_to_list(Little),

    Big = list_to_big_utf32(Str),
    Little = list_to_little_utf32(Str),

    ok.

utf32_big_len(B) ->
    utf32_big_len(B, 0).

utf32_big_len(<<_/utf32,T/binary>>, N) ->
    utf32_big_len(T, N+1);
utf32_big_len(<<>>, N) -> N.

utf32_little_len(B) ->
    utf32_little_len(B, 0).

utf32_little_len(<<_/little-utf32,T/binary>>, N) ->
    utf32_little_len(T, N+1);
utf32_little_len(<<>>, N) -> N.

list_to_big_utf32(List) ->
    list_to_big_utf32(List, <<>>).

list_to_big_utf32([H|T], Bin) ->
    list_to_big_utf32(T, <<Bin/binary,H/utf32>>);
list_to_big_utf32([], Bin) -> Bin.

list_to_little_utf32(List) ->
    list_to_little_utf32(List, <<>>).

list_to_little_utf32([H|T], Bin) ->
    list_to_little_utf32(T, <<Bin/binary,H/little-utf32>>);
list_to_little_utf32([], Bin) -> Bin.

big_utf32_to_list(<<H/utf32,T/binary>>) ->
    [H|big_utf32_to_list(T)];
big_utf32_to_list(<<>>) -> [].

little_utf32_to_list(<<H/little-utf32,T/binary>>) ->
    [H|little_utf32_to_list(T)];
little_utf32_to_list(<<>>) -> [].


guard(Config) when is_list(Config) ->
    error = do_guard(16#D800),
    ok.

do_guard(C) when byte_size(<<C/utf8>>) =/= 42 -> ok;
do_guard(C) when byte_size(<<C/utf16>>) =/= 42 -> ok;
do_guard(C) when byte_size(<<C/utf32>>) =/= 42 -> ok;
do_guard(_) -> error.

%% The purpose of this test is to make sure that
%% the delayed creation of sub-binaries works.

extreme_tripping(Config) when is_list(Config) ->
    Unicode = lists:seq(0, 1024),
    Utf8 = unicode_to_utf8(Unicode, <<>>),
    Utf16 = utf8_to_utf16(Utf8, <<>>),
    Utf32 = utf8_to_utf32(Utf8, <<>>),
    Utf32 = utf16_to_utf32(Utf16, <<>>),
    Utf8 = utf32_to_utf8(Utf32, <<>>),
    Unicode = utf32_to_unicode(Utf32),
    ok.

unicode_to_utf8([C|T], Bin) ->
    unicode_to_utf8(T, <<Bin/bytes,C/utf8>>);
unicode_to_utf8([], Bin) -> Bin.

utf8_to_utf16(<<C/utf8,T/binary>>, Bin) ->
    utf8_to_utf16(T, <<Bin/bytes,C/utf16>>);
utf8_to_utf16(<<>>, Bin) -> Bin.

utf16_to_utf32(<<C/utf16,T/binary>>, Bin) ->
    utf16_to_utf32(T, <<Bin/bytes,C/utf32>>);
utf16_to_utf32(<<>>, Bin) -> Bin.

utf8_to_utf32(<<C/utf8,T/binary>>, Bin) ->
    utf8_to_utf32(T, <<Bin/bytes,C/utf32>>);
utf8_to_utf32(<<>>, Bin) -> Bin.

utf32_to_utf8(<<C/utf32,T/binary>>, Bin) ->
    utf32_to_utf8(T, <<Bin/bytes,C/utf8>>);
utf32_to_utf8(<<>>, Bin) -> Bin.

utf32_to_unicode(<<C/utf32,T/binary>>) ->
    [C|utf32_to_unicode(T)];
utf32_to_unicode(<<>>) -> [].

utf_data() ->
    %% From RFC-3629.

    %% Give the compiler a change to do some constant propagation.
    NotIdentical = 16#2262,

    [
     %% "A<NOT IDENTICAL TO><ALPHA>."
     {[16#0041,NotIdentical,16#0391,16#002E],
      <<16#0041/utf8,NotIdentical/utf8,16#0391/utf8,16#002E/utf8>>,
      <<16#41,16#E2,16#89,16#A2,16#CE,16#91,16#2E>>},

     %% Korean "hangugeo" (meaning "the Korean language")
     {[16#D55C,16#AD6D,16#C5B4],
      <<16#D55C/utf8,16#AD6D/utf8,16#C5B4/utf8>>,
      <<16#ED,16#95,16#9C,16#EA,16#B5,16#AD,16#EC,16#96,16#B4>>},

     %% Japanese "nihongo" (meaning "the Japanese language").
     {[16#65E5,16#672C,16#8A9E],
      <<16#65E5/utf8,16#672C/utf8,16#8A9E/utf8>>,
      <<16#E6,16#97,16#A5,16#E6,16#9C,16#AC,16#E8,16#AA,16#9E>>}
    ].

utf16_data() ->
    %% Example from RFC-2781. "*=Ra", where "*" represents a
    %% hypothetical Ra hieroglyph (code point 16#12345).

    %% Give the compiler a change to do some constant propagation.
    RaHieroglyph = 16#12345,

    %% First as a list of Unicode characters.
    {[RaHieroglyph,16#3D,16#52,16#61],

     %% Big endian (the two binaries should be equal).
     <<RaHieroglyph/big-utf16,16#3D/big-utf16,16#52/big-utf16,16#61/big-utf16>>,
     <<16#D8,16#08,16#DF,16#45,16#00,16#3D,16#00,16#52,16#00,16#61>>,

     %% Little endian (the two binaries should be equal).
     <<RaHieroglyph/little-utf16,16#3D/little-utf16,
       16#52/little-utf16,16#61/little-utf16>>,
     <<16#08,16#D8,16#45,16#DF,16#3D,16#00,16#52,16#00,16#61,16#00>>}.

utf32_data() ->
    %% "A<NOT IDENTICAL TO><ALPHA>."
    NotIdentical = 16#2262,
    {[16#0041,NotIdentical,16#0391,16#002E],

     %% Big endian.
     <<16#0041/utf32,NotIdentical/utf32,16#0391/utf32,16#002E/utf32>>,
     <<16#41:32,NotIdentical:32,16#0391:32,16#2E:32>>,

     %% Little endian.
     <<16#0041/little-utf32,NotIdentical/little-utf32,
       16#0391/little-utf32,16#002E/little-utf32>>,
     <<16#41:32/little,NotIdentical:32/little,
       16#0391:32/little,16#2E:32/little>>}.
