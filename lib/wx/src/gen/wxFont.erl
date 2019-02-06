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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html">wxFont</a>.
%% @type wxFont().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxFont).
-include("wxe.hrl").
-export([destroy/1,getDefaultEncoding/0,getFaceName/1,getFamily/1,getNativeFontInfoDesc/1,
  getNativeFontInfoUserDesc/1,getPointSize/1,getStyle/1,getUnderlined/1,
  getWeight/1,isFixedWidth/1,new/0,new/1,new/4,new/5,ok/1,setDefaultEncoding/1,
  setFaceName/2,setFamily/2,setPointSize/2,setStyle/2,setUnderlined/2,
  setWeight/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxFont/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxFont() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
-spec new() -> wxFont().
new() ->
  wxe_util:construct(?wxFont_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
-spec new(Fontname) -> wxFont() when
	Fontname::unicode:chardata().
new(Fontname)
 when ?is_chardata(Fontname) ->
  Fontname_UC = unicode:characters_to_binary([Fontname,0]),
  wxe_util:construct(?wxFont_new_1,
  <<(byte_size(Fontname_UC)):32/?UI,(Fontname_UC)/binary, 0:(((8- ((4+byte_size(Fontname_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @equiv new(Size,Family,Style,Weight, [])
-spec new(Size, Family, Style, Weight) -> wxFont() when
	Size::integer(), Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::integer().

new(Size,Family,Style,Weight)
 when is_integer(Size),is_integer(Family),is_integer(Style),is_integer(Weight) ->
  new(Size,Family,Style,Weight, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
%%<br /> Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
%%<br /> Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec new(Size, Family, Style, Weight, [Option]) -> wxFont() when
	Size::integer(), Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::integer(),
	Option :: {'underlined', boolean()}
		 | {'face', unicode:chardata()}
		 | {'encoding', wx:wx_enum()}.
new(Size,Family,Style,Weight, Options)
 when is_integer(Size),is_integer(Family),is_integer(Style),is_integer(Weight),is_list(Options) ->
  MOpts = fun({underlined, Underlined}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Underlined)):32/?UI>>|Acc];
          ({face, Face}, Acc) ->   Face_UC = unicode:characters_to_binary([Face,0]),[<<2:32/?UI,(byte_size(Face_UC)):32/?UI,(Face_UC)/binary, 0:(((8- ((0+byte_size(Face_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({encoding, Encoding}, Acc) -> [<<3:32/?UI,Encoding:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxFont_new_5,
  <<Size:32/?UI,Family:32/?UI,Style:32/?UI,Weight:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontisfixedwidth">external documentation</a>.
-spec isFixedWidth(This) -> boolean() when
	This::wxFont().
isFixedWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_IsFixedWidth,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetdefaultencoding">external documentation</a>.
%%<br /> Res = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
-spec getDefaultEncoding() -> wx:wx_enum().
getDefaultEncoding() ->
  wxe_util:call(?wxFont_GetDefaultEncoding,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetfacename">external documentation</a>.
-spec getFaceName(This) -> unicode:charlist() when
	This::wxFont().
getFaceName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetFaceName,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetfamily">external documentation</a>.
%%<br /> Res = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-spec getFamily(This) -> wx:wx_enum() when
	This::wxFont().
getFamily(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetFamily,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetnativefontinfodesc">external documentation</a>.
-spec getNativeFontInfoDesc(This) -> unicode:charlist() when
	This::wxFont().
getNativeFontInfoDesc(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetNativeFontInfoDesc,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetnativefontinfouserdesc">external documentation</a>.
-spec getNativeFontInfoUserDesc(This) -> unicode:charlist() when
	This::wxFont().
getNativeFontInfoUserDesc(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetNativeFontInfoUserDesc,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetpointsize">external documentation</a>.
-spec getPointSize(This) -> integer() when
	This::wxFont().
getPointSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetPointSize,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetstyle">external documentation</a>.
%%<br /> Res = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec getStyle(This) -> wx:wx_enum() when
	This::wxFont().
getStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetStyle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetunderlined">external documentation</a>.
-spec getUnderlined(This) -> boolean() when
	This::wxFont().
getUnderlined(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetUnderlined,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetweight">external documentation</a>.
-spec getWeight(This) -> integer() when
	This::wxFont().
getWeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetWeight,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxFont().
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_Ok,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetdefaultencoding">external documentation</a>.
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
-spec setDefaultEncoding(Encoding) -> 'ok' when
	Encoding::wx:wx_enum().
setDefaultEncoding(Encoding)
 when is_integer(Encoding) ->
  wxe_util:cast(?wxFont_SetDefaultEncoding,
  <<Encoding:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetfacename">external documentation</a>.
-spec setFaceName(This, FaceName) -> boolean() when
	This::wxFont(), FaceName::unicode:chardata().
setFaceName(#wx_ref{type=ThisT,ref=ThisRef},FaceName)
 when ?is_chardata(FaceName) ->
  ?CLASS(ThisT,wxFont),
  FaceName_UC = unicode:characters_to_binary([FaceName,0]),
  wxe_util:call(?wxFont_SetFaceName,
  <<ThisRef:32/?UI,(byte_size(FaceName_UC)):32/?UI,(FaceName_UC)/binary, 0:(((8- ((0+byte_size(FaceName_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetfamily">external documentation</a>.
%%<br /> Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-spec setFamily(This, Family) -> 'ok' when
	This::wxFont(), Family::wx:wx_enum().
setFamily(#wx_ref{type=ThisT,ref=ThisRef},Family)
 when is_integer(Family) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetFamily,
  <<ThisRef:32/?UI,Family:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetpointsize">external documentation</a>.
-spec setPointSize(This, PointSize) -> 'ok' when
	This::wxFont(), PointSize::integer().
setPointSize(#wx_ref{type=ThisT,ref=ThisRef},PointSize)
 when is_integer(PointSize) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetPointSize,
  <<ThisRef:32/?UI,PointSize:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetstyle">external documentation</a>.
%%<br /> Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec setStyle(This, Style) -> 'ok' when
	This::wxFont(), Style::wx:wx_enum().
setStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetunderlined">external documentation</a>.
-spec setUnderlined(This, Underlined) -> 'ok' when
	This::wxFont(), Underlined::boolean().
setUnderlined(#wx_ref{type=ThisT,ref=ThisRef},Underlined)
 when is_boolean(Underlined) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetUnderlined,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Underlined)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetweight">external documentation</a>.
-spec setWeight(This, Weight) -> 'ok' when
	This::wxFont(), Weight::integer().
setWeight(#wx_ref{type=ThisT,ref=ThisRef},Weight)
 when is_integer(Weight) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetWeight,
  <<ThisRef:32/?UI,Weight:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFont()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFont),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
