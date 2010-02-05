%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html">wxFont</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontwxfont">external documentation</a>.
new() ->
  wxe_util:construct(?wxFont_new_0,
  <<>>).

%% @spec (Fontname::string()) -> wxFont()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontwxfont">external documentation</a>.
new(Fontname)
 when is_list(Fontname) ->
  Fontname_UC = unicode:characters_to_binary([Fontname,0]),
  wxe_util:construct(?wxFont_new_1,
  <<(byte_size(Fontname_UC)):32/?UI,(Fontname_UC)/binary, 0:(((8- ((4+byte_size(Fontname_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (Size::integer(), Family::WxFontFamily, Style::WxFontStyle, Weight::integer()) -> wxFont()
%% @equiv new(Size,Family,Style,Weight, [])
new(Size,Family,Style,Weight)
 when is_integer(Size),is_integer(Family),is_integer(Style),is_integer(Weight) ->
  new(Size,Family,Style,Weight, []).

%% @spec (Size::integer(), Family::WxFontFamily, Style::WxFontStyle, Weight::integer(), [Option]) -> wxFont()
%% Option = {underlined, bool()} | {face, string()} | {encoding, WxFontEncoding}
%% WxFontFamily = integer()
%% WxFontStyle = integer()
%% WxFontEncoding = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontwxfont">external documentation</a>.
%%<br /> WxFontFamily is one of ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
%%<br /> WxFontStyle is one of ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
%%<br /> WxFontEncoding is one of ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
new(Size,Family,Style,Weight, Options)
 when is_integer(Size),is_integer(Family),is_integer(Style),is_integer(Weight),is_list(Options) ->
  MOpts = fun({underlined, Underlined}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Underlined)):32/?UI>>|Acc];
          ({face, Face}, Acc) ->   Face_UC = unicode:characters_to_binary([Face,0]),[<<2:32/?UI,(byte_size(Face_UC)):32/?UI,(Face_UC)/binary, 0:(((8- ((0+byte_size(Face_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({encoding, Encoding}, Acc) -> [<<3:32/?UI,Encoding:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxFont_new_5,
  <<Size:32/?UI,Family:32/?UI,Style:32/?UI,Weight:32/?UI, BinOpt/binary>>).

%% @spec (This::wxFont()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontisfixedwidth">external documentation</a>.
isFixedWidth(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_IsFixedWidth,
  <<ThisRef:32/?UI>>).

%% @spec () -> WxFontEncoding
%% WxFontEncoding = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetdefaultencoding">external documentation</a>.
%%<br /> WxFontEncoding is one of ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
getDefaultEncoding() ->
  wxe_util:call(?wxFont_GetDefaultEncoding,
  <<>>).

%% @spec (This::wxFont()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetfacename">external documentation</a>.
getFaceName(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetFaceName,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> WxFontFamily
%% WxFontFamily = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetfamily">external documentation</a>.
%%<br /> WxFontFamily is one of ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
getFamily(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetFamily,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetnativefontinfodesc">external documentation</a>.
getNativeFontInfoDesc(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetNativeFontInfoDesc,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetnativefontinfouserdesc">external documentation</a>.
getNativeFontInfoUserDesc(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetNativeFontInfoUserDesc,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetpointsize">external documentation</a>.
getPointSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetPointSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> WxFontStyle
%% WxFontStyle = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetstyle">external documentation</a>.
%%<br /> WxFontStyle is one of ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
getStyle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetStyle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetunderlined">external documentation</a>.
getUnderlined(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetUnderlined,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontgetweight">external documentation</a>.
getWeight(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_GetWeight,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxFont()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontok">external documentation</a>.
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:call(?wxFont_Ok,
  <<ThisRef:32/?UI>>).

%% @spec (Encoding::WxFontEncoding) -> ok
%% WxFontEncoding = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetdefaultencoding">external documentation</a>.
%%<br /> WxFontEncoding is one of ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS
setDefaultEncoding(Encoding)
 when is_integer(Encoding) ->
  wxe_util:cast(?wxFont_SetDefaultEncoding,
  <<Encoding:32/?UI>>).

%% @spec (This::wxFont(), FaceName::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetfacename">external documentation</a>.
setFaceName(#wx_ref{type=ThisT,ref=ThisRef},FaceName)
 when is_list(FaceName) ->
  ?CLASS(ThisT,wxFont),
  FaceName_UC = unicode:characters_to_binary([FaceName,0]),
  wxe_util:call(?wxFont_SetFaceName,
  <<ThisRef:32/?UI,(byte_size(FaceName_UC)):32/?UI,(FaceName_UC)/binary, 0:(((8- ((0+byte_size(FaceName_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxFont(), Family::WxFontFamily) -> ok
%% WxFontFamily = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetfamily">external documentation</a>.
%%<br /> WxFontFamily is one of ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
setFamily(#wx_ref{type=ThisT,ref=ThisRef},Family)
 when is_integer(Family) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetFamily,
  <<ThisRef:32/?UI,Family:32/?UI>>).

%% @spec (This::wxFont(), PointSize::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetpointsize">external documentation</a>.
setPointSize(#wx_ref{type=ThisT,ref=ThisRef},PointSize)
 when is_integer(PointSize) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetPointSize,
  <<ThisRef:32/?UI,PointSize:32/?UI>>).

%% @spec (This::wxFont(), Style::WxFontStyle) -> ok
%% WxFontStyle = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetstyle">external documentation</a>.
%%<br /> WxFontStyle is one of ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
setStyle(#wx_ref{type=ThisT,ref=ThisRef},Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetStyle,
  <<ThisRef:32/?UI,Style:32/?UI>>).

%% @spec (This::wxFont(), Underlined::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetunderlined">external documentation</a>.
setUnderlined(#wx_ref{type=ThisT,ref=ThisRef},Underlined)
 when is_boolean(Underlined) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetUnderlined,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Underlined)):32/?UI>>).

%% @spec (This::wxFont(), Weight::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxfont.html#wxfontsetweight">external documentation</a>.
setWeight(#wx_ref{type=ThisT,ref=ThisRef},Weight)
 when is_integer(Weight) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:cast(?wxFont_SetWeight,
  <<ThisRef:32/?UI,Weight:32/?UI>>).

%% @spec (This::wxFont()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFont),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
