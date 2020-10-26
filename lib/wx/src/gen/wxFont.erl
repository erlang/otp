%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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

-module(wxFont).
-include("wxe.hrl").
-export([destroy/1,getDefaultEncoding/0,getFaceName/1,getFamily/1,getNativeFontInfoDesc/1,
  getNativeFontInfoUserDesc/1,getPointSize/1,getStyle/1,getUnderlined/1,
  getWeight/1,isFixedWidth/1,isOk/1,new/0,new/1,new/4,new/5,ok/1,setDefaultEncoding/1,
  setFaceName/2,setFamily/2,setPointSize/2,setStyle/2,setUnderlined/2,
  setWeight/2]).

%% inherited exports
-export([parent_class/1]).

-type wxFont() :: wx:wx_object().
-export_type([wxFont/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
-spec new() -> wxFont().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFont_new_0),
  wxe_util:rec(?wxFont_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
%% <br /> Also:<br />
%% new(Font) -> wxFont() when<br />
%% 	Font::wxFont().<br />
%% 
-spec new(NativeInfoString) -> wxFont() when
	NativeInfoString::unicode:chardata();
      (Font) -> wxFont() when
	Font::wxFont().
new(NativeInfoString)
 when ?is_chardata(NativeInfoString) ->
  NativeInfoString_UC = unicode:characters_to_binary(NativeInfoString),
  wxe_util:queue_cmd(NativeInfoString_UC,?get_env(),?wxFont_new_1_0),
  wxe_util:rec(?wxFont_new_1_0);
new(#wx_ref{type=FontT}=Font) ->
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(Font,?get_env(),?wxFont_new_1_1),
  wxe_util:rec(?wxFont_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
%% <br /> Also:<br />
%% new(PixelSize, Family, Style, Weight) -> wxFont() when<br />
%% 	PixelSize::{W::integer(), H::integer()}, Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::wx:wx_enum().<br />
%% 
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
%%<br /> Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
%%<br /> Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
%%<br /> Weight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-spec new(PointSize, Family, Style, Weight) -> wxFont() when
	PointSize::integer(), Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::wx:wx_enum();
      (PixelSize, Family, Style, Weight) -> wxFont() when
	PixelSize::{W::integer(), H::integer()}, Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::wx:wx_enum().

new(PointSize,Family,Style,Weight)
 when is_integer(PointSize),is_integer(Family),is_integer(Style),is_integer(Weight) ->
  new(PointSize,Family,Style,Weight, []);

new({PixelSizeW,PixelSizeH} = PixelSize,Family,Style,Weight)
 when is_integer(PixelSizeW),is_integer(PixelSizeH),is_integer(Family),is_integer(Style),is_integer(Weight) ->
  new(PixelSize,Family,Style,Weight, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontwxfont">external documentation</a>.
%% <br /> Also:<br />
%% new(PixelSize, Family, Style, Weight, [Option]) -> wxFont() when<br />
%% 	PixelSize::{W::integer(), H::integer()}, Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::wx:wx_enum(),<br />
%% 	Option :: {'underline', boolean()}<br />
%% 		 | {'faceName', unicode:chardata()}<br />
%% 		 | {'encoding', wx:wx_enum()}.<br />
%% 
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
%%<br /> Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
%%<br /> Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
%%<br /> Weight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-spec new(PointSize, Family, Style, Weight, [Option]) -> wxFont() when
	PointSize::integer(), Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::wx:wx_enum(),
	Option :: {'underlined', boolean()}
		 | {'face', unicode:chardata()}
		 | {'encoding', wx:wx_enum()};
      (PixelSize, Family, Style, Weight, [Option]) -> wxFont() when
	PixelSize::{W::integer(), H::integer()}, Family::wx:wx_enum(), Style::wx:wx_enum(), Weight::wx:wx_enum(),
	Option :: {'underline', boolean()}
		 | {'faceName', unicode:chardata()}
		 | {'encoding', wx:wx_enum()}.
new(PointSize,Family,Style,Weight, Options)
 when is_integer(PointSize),is_integer(Family),is_integer(Style),is_integer(Weight),is_list(Options) ->
  MOpts = fun({underlined, _underlined} = Arg) -> Arg;
          ({face, Face}) ->   Face_UC = unicode:characters_to_binary(Face),{face,Face_UC};
          ({encoding, _encoding} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(PointSize,Family,Style,Weight, Opts,?get_env(),?wxFont_new_5_0),
  wxe_util:rec(?wxFont_new_5_0);
new({PixelSizeW,PixelSizeH} = PixelSize,Family,Style,Weight, Options)
 when is_integer(PixelSizeW),is_integer(PixelSizeH),is_integer(Family),is_integer(Style),is_integer(Weight),is_list(Options) ->
  MOpts = fun({underline, _underline} = Arg) -> Arg;
          ({faceName, FaceName}) ->   FaceName_UC = unicode:characters_to_binary(FaceName),{faceName,FaceName_UC};
          ({encoding, _encoding} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(PixelSize,Family,Style,Weight, Opts,?get_env(),?wxFont_new_5_1),
  wxe_util:rec(?wxFont_new_5_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontisfixedwidth">external documentation</a>.
-spec isFixedWidth(This) -> boolean() when
	This::wxFont().
isFixedWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_IsFixedWidth),
  wxe_util:rec(?wxFont_IsFixedWidth).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetdefaultencoding">external documentation</a>.
%%<br /> Res = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec getDefaultEncoding() -> wx:wx_enum().
getDefaultEncoding() ->
  wxe_util:queue_cmd(?get_env(), ?wxFont_GetDefaultEncoding),
  wxe_util:rec(?wxFont_GetDefaultEncoding).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetfacename">external documentation</a>.
-spec getFaceName(This) -> unicode:charlist() when
	This::wxFont().
getFaceName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetFaceName),
  wxe_util:rec(?wxFont_GetFaceName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetfamily">external documentation</a>.
%%<br /> Res = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-spec getFamily(This) -> wx:wx_enum() when
	This::wxFont().
getFamily(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetFamily),
  wxe_util:rec(?wxFont_GetFamily).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetnativefontinfodesc">external documentation</a>.
-spec getNativeFontInfoDesc(This) -> unicode:charlist() when
	This::wxFont().
getNativeFontInfoDesc(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetNativeFontInfoDesc),
  wxe_util:rec(?wxFont_GetNativeFontInfoDesc).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetnativefontinfouserdesc">external documentation</a>.
-spec getNativeFontInfoUserDesc(This) -> unicode:charlist() when
	This::wxFont().
getNativeFontInfoUserDesc(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetNativeFontInfoUserDesc),
  wxe_util:rec(?wxFont_GetNativeFontInfoUserDesc).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetpointsize">external documentation</a>.
-spec getPointSize(This) -> integer() when
	This::wxFont().
getPointSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetPointSize),
  wxe_util:rec(?wxFont_GetPointSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetstyle">external documentation</a>.
%%<br /> Res = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec getStyle(This) -> wx:wx_enum() when
	This::wxFont().
getStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetStyle),
  wxe_util:rec(?wxFont_GetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetunderlined">external documentation</a>.
-spec getUnderlined(This) -> boolean() when
	This::wxFont().
getUnderlined(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetUnderlined),
  wxe_util:rec(?wxFont_GetUnderlined).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontgetweight">external documentation</a>.
%%<br /> Res = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-spec getWeight(This) -> wx:wx_enum() when
	This::wxFont().
getWeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetWeight),
  wxe_util:rec(?wxFont_GetWeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontisok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxFont().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxFont().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_IsOk),
  wxe_util:rec(?wxFont_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetdefaultencoding">external documentation</a>.
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec setDefaultEncoding(Encoding) -> 'ok' when
	Encoding::wx:wx_enum().
setDefaultEncoding(Encoding)
 when is_integer(Encoding) ->
  wxe_util:queue_cmd(Encoding,?get_env(),?wxFont_SetDefaultEncoding).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetfacename">external documentation</a>.
-spec setFaceName(This, FaceName) -> boolean() when
	This::wxFont(), FaceName::unicode:chardata().
setFaceName(#wx_ref{type=ThisT}=This,FaceName)
 when ?is_chardata(FaceName) ->
  ?CLASS(ThisT,wxFont),
  FaceName_UC = unicode:characters_to_binary(FaceName),
  wxe_util:queue_cmd(This,FaceName_UC,?get_env(),?wxFont_SetFaceName),
  wxe_util:rec(?wxFont_SetFaceName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetfamily">external documentation</a>.
%%<br /> Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-spec setFamily(This, Family) -> 'ok' when
	This::wxFont(), Family::wx:wx_enum().
setFamily(#wx_ref{type=ThisT}=This,Family)
 when is_integer(Family) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Family,?get_env(),?wxFont_SetFamily).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetpointsize">external documentation</a>.
-spec setPointSize(This, PointSize) -> 'ok' when
	This::wxFont(), PointSize::integer().
setPointSize(#wx_ref{type=ThisT}=This,PointSize)
 when is_integer(PointSize) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,PointSize,?get_env(),?wxFont_SetPointSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetstyle">external documentation</a>.
%%<br /> Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec setStyle(This, Style) -> 'ok' when
	This::wxFont(), Style::wx:wx_enum().
setStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxFont_SetStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetunderlined">external documentation</a>.
-spec setUnderlined(This, Underlined) -> 'ok' when
	This::wxFont(), Underlined::boolean().
setUnderlined(#wx_ref{type=ThisT}=This,Underlined)
 when is_boolean(Underlined) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Underlined,?get_env(),?wxFont_SetUnderlined).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxfont.html#wxfontsetweight">external documentation</a>.
%%<br /> Weight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-spec setWeight(This, Weight) -> 'ok' when
	This::wxFont(), Weight::wx:wx_enum().
setWeight(#wx_ref{type=ThisT}=This,Weight)
 when is_integer(Weight) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Weight,?get_env(),?wxFont_SetWeight).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxFont()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFont),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
