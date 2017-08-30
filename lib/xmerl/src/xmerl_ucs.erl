%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-module(xmerl_ucs).

-compile([verbose,report_warnings,warn_unused_vars]).



%%% Conversion to/from IANA recognised character sets
-export([to_unicode/2]).

%%% Micellaneous predicates
-export([is_iso10646/1, is_unicode/1, is_bmpchar/1, is_latin1/1, is_ascii/1,
	 is_visible_latin1/1, is_visible_ascii/1, is_iso646_basic/1,
	 is_incharset/2]).

%%% Conversion to/from RFC-1345 style mnemonic strings consisting
%%% of subsets of ISO-10646 with "escape" sequences.
%-export([from_mnemonic/1, from_mnemonic/2]).

%%% UCS-2, UCS-4, UTF-16, and UTF-8 encoding and decoding
-export([to_ucs2be/1,from_ucs2be/1, from_ucs2be/2]).
-export([to_ucs2le/1,from_ucs2le/1, from_ucs2le/2]).
-export([to_ucs4be/1,from_ucs4be/1, from_ucs4be/2]).
-export([to_ucs4le/1,from_ucs4le/1, from_ucs4le/2]).
-export([to_utf16be/1, from_utf16be/1, from_utf16be/2]).
-export([to_utf16le/1, from_utf16le/1, from_utf16le/2]).
-export([to_utf8/1, from_utf8/1]).
-export([from_latin9/1]).

%%% NB: Non-canonical UTF-8 encodings and incorrectly used
%%% surrogate-pair codes are disallowed by this code.  There are
%%% important security implications concerning them.  DO NOT REMOVE
%%% THE VARIOUS GUARDS AND TESTS THAT ENFORCE THIS POLICY.

%%% Test if Ch is a legitimate ISO-10646 character code
is_iso10646(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch  < 16#D800 -> true;
       Ch  < 16#E000 -> false;	% Surrogates
       Ch  < 16#FFFE -> true;
       Ch =< 16#FFFF -> false;	% FFFE and FFFF (not characters)
       Ch =< 16#7FFFFFFF -> true;
       true -> false
    end;
is_iso10646(_) -> false.

%%% Test if Ch is a legitimate ISO-10646 character code capable of
%%% being encoded in a UTF-16 string.
is_unicode(Ch) when Ch < 16#110000 -> is_iso10646(Ch);
is_unicode(_) -> false.

%%% Test if Ch is a legitimate ISO-10646 character code belonging to
%%% the basic multi-lingual plane (BMP).
is_bmpchar(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch < 16#D800 -> true;
       Ch < 16#E000 -> false;	% Surrogates
       Ch < 16#FFFE -> true;
       true -> false
    end;
is_bmpchar(_) -> false.

%%% Test for legitimate Latin-1 code
is_latin1(Ch) when is_integer(Ch), Ch >= 0, Ch =< 255 -> true;
is_latin1(_) -> false.

%%% Test for legitimate ASCII code
is_ascii(Ch) when is_integer(Ch), Ch >= 0, Ch =< 127 -> true;
is_ascii(_) -> false.

%%% Test for char an element of ISO-646.basic set
is_iso646_basic(Ch) when is_integer(Ch), Ch >= $\s ->
    if Ch =< $Z ->
	    %% Everything in this range except $# $$ and $@
	    if Ch > $$ -> Ch =/= $@;
	       true -> Ch < $#
	    end;
       %% Only $_ and $a .. $z in range above $Z
       Ch > $z -> false;
       Ch >= $a -> true;
       true -> Ch =:= $_
    end;
is_iso646_basic(_) ->
    false.

%%% Test for char a visible Latin-1 char, i.e. a non-control Latin-1 char,
%%% excepting non-break space (but including space).
is_visible_latin1(Ch) when is_integer(Ch), Ch >= $\s ->
    if Ch =< $~ -> true;
       Ch >= 161 -> Ch =< 255
    end;
is_visible_latin1(_) ->
    false.

%%% Test for char a visible ASCII char, i.e. a non-control ASCII char
%%% (including space).
is_visible_ascii(Ch) when is_integer(Ch), Ch >= $\s -> Ch =< $~;
is_visible_ascii(_) -> false.


%%% UCS-4, big and little endian versions, encoding and decoding
to_ucs4be(List) when is_list(List) -> lists:flatmap(fun to_ucs4be/1, List);
to_ucs4be(Ch) -> char_to_ucs4be(Ch).

from_ucs4be(Bin) when is_binary(Bin) -> from_ucs4be(Bin,[],[]);
from_ucs4be(List) -> from_ucs4be(list_to_binary(List),[],[]).

from_ucs4be(Bin,Tail) when is_binary(Bin) -> from_ucs4be(Bin,[],Tail);
from_ucs4be(List,Tail) -> from_ucs4be(list_to_binary(List),[],Tail).

to_ucs4le(List) when is_list(List) -> lists:flatmap(fun to_ucs4le/1, List);
to_ucs4le(Ch) -> char_to_ucs4le(Ch).

from_ucs4le(Bin) when is_binary(Bin) -> from_ucs4le(Bin,[],[]);
from_ucs4le(List) -> from_ucs4le(list_to_binary(List),[],[]).

from_ucs4le(Bin,Tail) when is_binary(Bin) -> from_ucs4le(Bin,[],Tail);
from_ucs4le(List,Tail) -> from_ucs4le(list_to_binary(List),[],Tail).

%%% UCS-2, big and little endian versions, encoding and decoding
to_ucs2be(List) when is_list(List) -> lists:flatmap(fun to_ucs2be/1, List);
to_ucs2be(Ch) -> char_to_ucs2be(Ch).

from_ucs2be(Bin) when is_binary(Bin) -> from_ucs2be(Bin,[],[]);
from_ucs2be(List) -> from_ucs2be(list_to_binary(List),[],[]).

from_ucs2be(Bin,Tail) when is_binary(Bin) -> from_ucs2be(Bin,[],Tail);
from_ucs2be(List,Tail) -> from_ucs2be(list_to_binary(List),[],Tail).

to_ucs2le(List) when is_list(List) -> lists:flatmap(fun to_ucs2le/1, List);
to_ucs2le(Ch) -> char_to_ucs2le(Ch).

from_ucs2le(Bin) when is_binary(Bin) -> from_ucs2le(Bin,[],[]);
from_ucs2le(List) -> from_ucs2le(list_to_binary(List),[],[]).

from_ucs2le(Bin,Tail) when is_binary(Bin) -> from_ucs2le(Bin,[],Tail);
from_ucs2le(List,Tail) -> from_ucs2le(list_to_binary(List),[],Tail).


%%% UTF-16, big and little endian versions, encoding and decoding
to_utf16be(List) when is_list(List) -> lists:flatmap(fun to_utf16be/1, List);
to_utf16be(Ch) -> char_to_utf16be(Ch).

from_utf16be(Bin) when is_binary(Bin) -> from_utf16be(Bin,[],[]);
from_utf16be(List) -> from_utf16be(list_to_binary(List),[],[]).

from_utf16be(Bin,Tail) when is_binary(Bin) -> from_utf16be(Bin,[],Tail);
from_utf16be(List,Tail) -> from_utf16be(list_to_binary(List),[],Tail).

to_utf16le(List) when is_list(List) -> lists:flatmap(fun to_utf16le/1, List);
to_utf16le(Ch) -> char_to_utf16le(Ch).

from_utf16le(Bin) when is_binary(Bin) -> from_utf16le(Bin,[],[]);
from_utf16le(List) -> from_utf16le(list_to_binary(List),[],[]).

from_utf16le(Bin,Tail) when is_binary(Bin) -> from_utf16le(Bin,[],Tail);
from_utf16le(List,Tail) -> from_utf16le(list_to_binary(List),[],Tail).


%%% UTF-8 encoding and decoding
to_utf8(List) when is_list(List) -> lists:flatmap(fun to_utf8/1, List);
to_utf8(Ch) -> char_to_utf8(Ch).

from_utf8(Bin) when is_binary(Bin) -> from_utf8(binary_to_list(Bin));
from_utf8(List) ->
    case expand_utf8(List) of
	{Result,0} -> Result;
	{_Res,_NumBadChar} ->
	    exit({ucs,{bad_utf8_character_code}})
    end.

%%% Latin9 support
from_latin9(Bin) when is_binary(Bin) -> from_latin9(binary_to_list(Bin));
from_latin9(List) ->
    [ latin9_to_ucs4(Char) || Char <- List].

latin9_to_ucs4(16#A4) -> 16#20AC;
latin9_to_ucs4(16#A6) -> 16#160;
latin9_to_ucs4(16#A8) -> 16#161;
latin9_to_ucs4(16#B4) -> 16#17D;
latin9_to_ucs4(16#B8) -> 16#17E;
latin9_to_ucs4(16#BC) -> 16#152;
latin9_to_ucs4(16#BD) -> 16#153;
latin9_to_ucs4(16#BE) -> 16#178;
latin9_to_ucs4(Other) -> Other.



%%% UCS-4 support
%%% Possible errors encoding UCS-4:
%%%	- Non-character values (something other than 0 .. 2^31-1)
%%%	- Surrogate-pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
%%% Possible errors decoding UCS-4:
%%%	- Element out of range (i.e. the "sign" bit is set).
%%%	- Surrogate-pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
char_to_ucs4be(Ch) ->
    true = is_iso10646(Ch),
    [(Ch bsr 24),
     (Ch bsr 16) band 16#FF,
     (Ch bsr 8) band 16#FF,
     Ch band 16#FF].

from_ucs4be(<<Ch:32/big-signed-integer, Rest/binary>>,Acc,Tail) ->
    if Ch < 0; Ch >= 16#D800, Ch < 16#E000; Ch =:= 16#FFFE; Ch =:= 16#FFFF ->
	    exit({bad_character_code,Ch});
       true ->
	    from_ucs4be(Rest,[Ch|Acc],Tail)
    end;
from_ucs4be(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_ucs4be(Bin,Acc,Tail) ->
    ucs_error(Bin,Acc,Tail),
    {error,not_ucs4be}.

char_to_ucs4le(Ch) ->
    true = is_iso10646(Ch),
    [Ch band 16#FF,
     (Ch bsr 8) band 16#FF,
     (Ch bsr 16) band 16#FF,
     (Ch bsr 24)].


from_ucs4le(<<Ch:32/little-signed-integer, Rest/binary>>,Acc,Tail) ->
    if Ch < 0; Ch >= 16#D800, Ch < 16#E000; Ch =:= 16#FFFE; Ch =:= 16#FFFF ->
	    exit({bad_character_code,Ch});
       true ->
	    from_ucs4le(Rest,[Ch|Acc],Tail)
    end;
from_ucs4le(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_ucs4le(Bin,Acc,Tail) ->
    ucs_error(Bin,Acc,Tail),
    {error,not_ucs4le}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UCS-2 support
%%% FIXME! Don't know how to encode UCS-2!!
%%% Currently I just encode as UCS-4, but strips the 16 higher bits.
char_to_ucs2be(Ch) ->
    true = is_iso10646(Ch),
    [(Ch bsr 8) band 16#FF,
     Ch band 16#FF].

from_ucs2be(<<Ch:16/big-signed-integer, Rest/binary>>,Acc,Tail) ->
    if Ch < 0; Ch >= 16#D800, Ch < 16#E000; Ch =:= 16#FFFE; Ch =:= 16#FFFF ->
	    exit({bad_character_code,Ch});
       true ->
	    from_ucs2be(Rest,[Ch|Acc],Tail)
    end;
from_ucs2be(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_ucs2be(Bin,Acc,Tail) ->
    ucs_error(Bin,Acc,Tail),
    {error,not_ucs2be}.

char_to_ucs2le(Ch) ->
    true = is_iso10646(Ch),
    [Ch band 16#FF,
     (Ch bsr 8) band 16#FF].


from_ucs2le(<<Ch:16/little-signed-integer, Rest/binary>>,Acc,Tail) ->
    if Ch < 0; Ch >= 16#D800, Ch < 16#E000; Ch =:= 16#FFFE; Ch =:= 16#FFFF ->
	    exit({bad_character_code,Ch});
       true ->
	    from_ucs2le(Rest,[Ch|Acc],Tail)
    end;
from_ucs2le(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_ucs2le(Bin,Acc,Tail) ->
    ucs_error(Bin,Acc,Tail),
    {error,not_ucs2le}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UTF-16 support
%%% Possible errors encoding UTF-16
%%%	- Non-character values (something other than 0 .. 2^31-1)
%%%	- Surrogate-pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
%%% NB: the UCS replacement char (U+FFFD) will be quietly substituted
%%% for unrepresentable chars (i.e. those geq to 2^20+2^16).
%%% Possible errors decoding UTF-16:
%%%	- Unmatched surrogate-pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
char_to_utf16be(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch =< 16#FFFF ->
	    if Ch < 16#D800; Ch >= 16#E000, Ch < 16#FFFE ->
		    [Ch bsr 8, Ch band 16#FF]
	    end;
       Ch < 16#110000 ->
	    %% Encode with surrogate pair
	    X = Ch - 16#10000,
	    [16#D8 + (X bsr 18),
	     (X bsr 10) band 16#FF,
	     16#DC + ((X bsr 8) band 3),
	     X band 16#FF];
       Ch =< 16#7FFFFFFF ->
	    %% Unrepresentable char: use REPLACEMENT CHARACTER (U+FFFD)
	    [16#FF, 16#FD]
    end.

from_utf16be(<<Ch:16/big-unsigned-integer, Rest/binary>>, Acc, Tail)
  when Ch < 16#D800; Ch > 16#DFFF ->
    if Ch < 16#FFFE -> from_utf16be(Rest,[Ch|Acc],Tail) end;
from_utf16be(<<Hi:16/big-unsigned-integer, Lo:16/big-unsigned-integer,
	       Rest/binary>>, Acc, Tail)
  when Hi >= 16#D800, Hi < 16#DC00, Lo >= 16#DC00, Lo =< 16#DFFF ->
    %% Surrogate pair
    Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    from_utf16be(Rest, [Ch|Acc], Tail);
from_utf16be(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_utf16be(Bin,Acc,Tail) ->
    ucs_error(Bin,Acc,Tail),
    {error,not_utf16be}.

char_to_utf16le(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch =< 16#FFFF ->
	    if Ch < 16#D800; Ch >= 16#E000, Ch < 16#FFFE ->
		    [Ch band 16#FF, Ch bsr 8]
	    end;
       Ch < 16#110000 ->
	    %% Encode with surrogate pair
	    X = Ch - 16#10000,
	    [(X bsr 10) band 16#FF,
	     16#D8 + (X bsr 18),
	     X band 16#FF,
	     16#DC + ((X bsr 8) band 3)];
       Ch =< 16#7FFFFFFF ->
	    %% Unrepresentable char: use REPLACEMENT CHARACTER (U+FFFD)
	    [16#FD, 16#FF]
    end.

from_utf16le(<<Ch:16/little-unsigned-integer, Rest/binary>>, Acc, Tail)
  when Ch < 16#D800; Ch > 16#DFFF ->
    if Ch < 16#FFFE -> from_utf16le(Rest, [Ch|Acc], Tail) end;
from_utf16le(<<Hi:16/little-unsigned-integer, Lo:16/little-unsigned-integer,
	       Rest/binary>>, Acc, Tail)
  when Hi >= 16#D800, Hi < 16#DC00, Lo >= 16#DC00, Lo =< 16#DFFF ->
    %% Surrogate pair
    Ch = ((Hi band 16#3FF) bsl 10) + (Lo band 16#3FF) + 16#10000,
    from_utf16le(Rest, [Ch|Acc], Tail);
from_utf16le(<<>>,Acc,Tail) ->
    lists:reverse(Acc,Tail);
from_utf16le(Bin,Acc,Tail) ->
    ucs_error(Bin,Acc,Tail),
    {error,not_utf16le}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% UTF-8 support
%%% Possible errors encoding UTF-8:
%%%	- Non-character values (something other than 0 .. 2^31-1).
%%%	- Surrogate pair code in string.
%%%	- 16#FFFE or 16#FFFF character in string.
%%% Possible errors decoding UTF-8:
%%%	- 10xxxxxx or 1111111x as initial byte.
%%%	- Insufficient number of 10xxxxxx octets following an initial octet of
%%%	multi-octet sequence.
%%% 	- Non-canonical encoding used.
%%%	- Surrogate-pair code encoded as UTF-8.
%%%	- 16#FFFE or 16#FFFF character in string.
char_to_utf8(Ch) when is_integer(Ch), Ch >= 0 ->
    if Ch < 128 ->
	    %% 0yyyyyyy
	    [Ch];
       Ch < 16#800 ->
	    %% 110xxxxy 10yyyyyy
	    [16#C0 + (Ch bsr 6),
	     128+(Ch band 16#3F)];
       Ch < 16#10000 ->
	    %% 1110xxxx 10xyyyyy 10yyyyyy
	    if Ch < 16#D800; Ch > 16#DFFF, Ch < 16#FFFE ->
		    [16#E0 + (Ch bsr 12),
		     128+((Ch bsr 6) band 16#3F),
		     128+(Ch band 16#3F)]
	    end;
       Ch < 16#200000 ->
	    %% 11110xxx 10xxyyyy 10yyyyyy 10yyyyyy
	    [16#F0+(Ch bsr 18),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#4000000 ->
	    %% 111110xx 10xxxyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#F8+(Ch bsr 24),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)];
       Ch < 16#80000000 ->
	    %% 1111110x 10xxxxyy 10yyyyyy 10yyyyyy 10yyyyyy 10yyyyyy
	    [16#FC+(Ch bsr 30),
	     128+((Ch bsr 24) band 16#3F),
	     128+((Ch bsr 18) band 16#3F),
	     128+((Ch bsr 12) band 16#3F),
	     128+((Ch bsr 6) band 16#3F),
	     128+(Ch band 16#3F)]
    end.




%% expand_utf8([Byte]) -> {[UnicodeChar],NumberOfBadBytes}
%%  Expand UTF8 byte sequences to ISO 10646/Unicode
%%  charactes. Any illegal bytes are removed and the number of
%%  bad bytes are returned.
%%
%%  Reference:
%%     RFC 3629: "UTF-8, a transformation format of ISO 10646".

expand_utf8(Str) ->
    expand_utf8_1(Str, [], 0).

expand_utf8_1([C|Cs], Acc, Bad) when C < 16#80 ->
    %% Plain Ascii character.
    expand_utf8_1(Cs, [C|Acc], Bad);
expand_utf8_1([C1,C2|Cs], Acc, Bad) when C1 band 16#E0 =:= 16#C0,
					 C2 band 16#C0 =:= 16#80 ->
    case ((C1 band 16#1F) bsl 6) bor (C2 band 16#3F) of
	C when 16#80 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3|Cs], Acc, Bad) when C1 band 16#F0 =:= 16#E0,
					    C2 band 16#C0 =:= 16#80,
					    C3 band 16#C0 =:= 16#80 ->
    case ((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F) of
	C when 16#800 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([C1,C2,C3,C4|Cs], Acc, Bad) when C1 band 16#F8 =:= 16#F0,
					       C2 band 16#C0 =:= 16#80,
					       C3 band 16#C0 =:= 16#80,
					       C4 band 16#C0 =:= 16#80 ->
    case ((((((C1 band 16#0F) bsl 6) bor (C2 band 16#3F)) bsl 6) bor
	(C3 band 16#3F)) bsl 6) bor (C4 band 16#3F) of
	C when 16#10000 =< C ->
	    expand_utf8_1(Cs, [C|Acc], Bad);
	_ ->
	    %% Bad range.
	    expand_utf8_1(Cs, Acc, Bad+1)
    end;
expand_utf8_1([_|Cs], Acc, Bad) ->
    %% Ignore bad character.
    expand_utf8_1(Cs, Acc, Bad+1);
expand_utf8_1([], Acc, Bad) -> {lists:reverse(Acc),Bad}.



%%% ----------------------------------------------------------------------------
%%% Translation to/from any IANA defined character set, given that a mapping
%%% exists. Don't care about validating valid subsets of Unicode
to_unicode(Input,Cs) when Cs=='ansi_x3.4-1968';Cs=='iso-ir-6';
			  Cs=='ansi_x3.4-1986';Cs=='iso_646.irv:1991';
			  Cs=='ascii';Cs=='iso646-us';Cs=='us-ascii';Cs=='us';
			  Cs=='ibm367';Cs=='cp367';Cs=='csascii' -> % US-ASCII
    Input;
to_unicode(Input,Cs) when Cs=='iso-10646-utf-1';Cs=='csiso10646utf1' ->
    Input;
to_unicode(Input,Cs) when Cs=='iso_646.basic:1983';Cs=='ref';
			  Cs=='csiso646basic1983' ->
    Input;
to_unicode(Input,Cs) when Cs=='iso_8859-1:1987';Cs=='iso-ir-100';
			  Cs=='iso_8859-1';Cs=='iso-8859-1';Cs=='latin1';
			  Cs=='l1';Cs=='ibm819';
			  Cs=='cp819';Cs=='csisolatin1' ->
    Input;
to_unicode(Input,Cs) when Cs=='iso_8859-15';Cs=='iso-8859-15';Cs=='latin9' ->
    from_latin9(Input);
% to_unicode(Input,Cs) when Cs=='mnemonic';Cs=='"mnemonic+ascii+38';
% 			  Cs=='mnem';Cs=='"mnemonic+ascii+8200' ->
%     from_mnemonic(Input);
to_unicode(Input,Cs) when Cs=='iso-10646-ucs-2';Cs=='csunicode' ->
    from_ucs2be(Input); % Guess byteorder
to_unicode(Input,Cs) when Cs=='iso-10646-ucs-4';Cs=='csucs4' ->
    from_ucs4be(Input); % Guess byteorder
to_unicode(Input,Cs) when Cs=='utf-16be';Cs=='utf-16' ->
    from_utf16be(Input);
to_unicode(Input,'utf-16le') ->
    from_utf16le(Input);
to_unicode(Input,'utf-8') ->
    from_utf8(Input);
to_unicode(Input,Charset) ->
    exit({bad_character_code,Input,Charset}).
    %ucs_data:to_unicode(Input,Charset).




%%% Tests if Char is in Charset.
%%% Do this by trying to convert it into unicode, if possible a mapping was
%%% found and we are ok.
is_incharset(In,Cs) when Cs=='ansi_x3.4-1968';Cs=='iso-ir-6';
			 Cs=='ansi_x3.4-1986';Cs=='iso_646.irv:1991';
			 Cs=='ascii';Cs=='iso646-us';Cs=='us-ascii';Cs=='us';
			 Cs=='ibm367';Cs=='cp367';Cs=='csascii' -> % US-ASCII
    if
	is_integer(In) -> is_ascii(In);
	is_list(In) -> test_charset(fun is_ascii/1,In)
    end;
is_incharset(In,Cs) when Cs=='iso-10646-utf-1';Cs=='csiso10646utf1' ->
    if
	is_integer(In) -> is_unicode(In);
	is_list(In) -> test_charset(fun is_unicode/1, In)
    end;
is_incharset(In,Cs) when Cs=='iso_646.basic:1983';Cs=='ref';
			 Cs=='csiso646basic1983' ->
    if
	is_integer(In) -> is_iso646_basic(In);
	is_list(In) -> test_charset(fun is_iso646_basic/1, In)
    end;
is_incharset(In,Cs) when Cs=='iso_8859-1:1987';Cs=='iso-ir-100';
			 Cs=='iso_8859-1';Cs=='iso-8859-1';
			 Cs=='latin1';Cs=='l1';Cs=='ibm819';
			 Cs=='cp819';Cs=='csisolatin1' ->
    if
	is_integer(In) -> is_latin1(In);
	is_list(In) -> test_charset(fun is_latin1/1, In)
    end;
is_incharset(In,Charset) when is_integer(In) ->
    case to_unicode([In],Charset) of
	{error,unsupported_charset} ->
	    {error,unsupported_charset};
	{error,_} ->
	    false;
	[Int] when is_integer(Int) ->
	    true
    end;
is_incharset(In,Charset) when is_list(In) ->
    case to_unicode(In,Charset) of
	{error,unsupported_charset} ->
	    {error,unsupported_charset};
	{error,_} ->
	    false;
	[Int] when is_integer(Int) ->
	    true
    end.


test_charset(Fun,Input) ->
    case lists:all(Fun, Input) of
	true ->
	    true;
	_ ->
	    false
    end.

ucs_error(Bin,Acc,Tail) ->
    error_logger:error_msg("~w: Bin=~p~n     Acc=~p~n     Tail=~p~n",
                           [?MODULE,Bin,Acc,Tail]).
