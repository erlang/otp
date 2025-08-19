%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxFont).
-moduledoc """
A font is an object which determines the appearance of text.

Fonts are used for drawing text to a device context, and setting the appearance of a
window's text, see `wxDC:setFont/2` and `wxWindow:setFont/2`.

The easiest way to create a custom font is to use `wxFontInfo` (not implemented in wx)
object to specify the font attributes and then use `new/5` constructor. Alternatively, you could
start with one of the pre-defined fonts or use `wxWindow:getFont/1` and modify the font, e.g. by increasing
its size using `MakeLarger()` (not implemented in wx) or changing its weight using `MakeBold()`
(not implemented in wx).

This class uses reference counting and copy-on-write internally so that assignments
between two instances of this class are very cheap. You can therefore use actual objects
instead of pointers without efficiency problems. If an instance of this class is changed
it will create its own data internally so that other instances, which previously shared
the data using the reference counting, are not affected.

You can retrieve the current system font settings with `m:wxSystemSettings`.

Predefined objects (include wx.hrl): ?wxNullFont, ?wxNORMAL\_FONT, ?wxSMALL\_FONT,
?wxITALIC\_FONT, ?wxSWISS\_FONT

See:
* [Overview font](https://docs.wxwidgets.org/3.2/overview_font.html#overview_font)

* `wxDC:setFont/2`

* `wxDC:drawText/3`

* `wxDC:getTextExtent/3`

* `m:wxFontDialog`

* `m:wxSystemSettings`

wxWidgets docs: [wxFont](https://docs.wxwidgets.org/3.2/classwx_font.html)
""".
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
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default ctor.".
-spec new() -> wxFont().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxFont_new_0),
  wxe_util:rec(?wxFont_new_0).

-doc "Copy constructor, uses reference counting.".
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

-doc "Equivalent to: `new/5`".
%%  Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
%%  Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
%%  Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
%%  Weight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
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

-doc """
Creates a font object with the specified attributes and size in pixels.

Notice that the use of this constructor is often more verbose and less readable than the
use of constructor from `wxFontInfo` (not implemented in wx), consider using that
constructor instead.

Remark: If the desired font does not exist, the closest match will be chosen. Under
Windows, only scalable TrueType fonts are used.
""".
%%  Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
%%  Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
%%  Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
%%  Weight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
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

-doc """
Returns true if the font is a fixed width (or monospaced) font, false if it is a
proportional one or font is invalid.

Note that this function under some platforms is different from just testing for the font
family being equal to `wxFONTFAMILY_TELETYPE` because native platform-specific functions
are used for the check (resulting in a more accurate return value).
""".
-spec isFixedWidth(This) -> boolean() when
	This::wxFont().
isFixedWidth(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_IsFixedWidth),
  wxe_util:rec(?wxFont_IsFixedWidth).

-doc """
Returns the current application's default encoding.

See:
* [Overview fontencoding](https://docs.wxwidgets.org/3.2/overview_fontencoding.html#overview_fontencoding)

* `setDefaultEncoding/1`
""".
%%  Res = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec getDefaultEncoding() -> wx:wx_enum().
getDefaultEncoding() ->
  wxe_util:queue_cmd(?get_env(), ?wxFont_GetDefaultEncoding),
  wxe_util:rec(?wxFont_GetDefaultEncoding).

-doc """
Returns the face name associated with the font, or the empty string if there is no face
information.

See: `setFaceName/2`
""".
-spec getFaceName(This) -> unicode:charlist() when
	This::wxFont().
getFaceName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetFaceName),
  wxe_util:rec(?wxFont_GetFaceName).

-doc """
Gets the font family if possible.

As described in ?wxFontFamily docs the returned value acts as a rough, basic
classification of the main font properties (look, spacing).

If the current font face name is not recognized by `m:wxFont` or by the underlying
system, `wxFONTFAMILY_DEFAULT` is returned.

Note that currently this function is not very precise and so not particularly useful.
Font families mostly make sense only for font creation, see `setFamily/2`.

See: `setFamily/2`
""".
%%  Res = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-spec getFamily(This) -> wx:wx_enum() when
	This::wxFont().
getFamily(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetFamily),
  wxe_util:rec(?wxFont_GetFamily).

-doc """
Returns the platform-dependent string completely describing this font.

Returned string is always non-empty unless the font is invalid (in which case an assert
is triggered).

Note that the returned string is not meant to be shown or edited by the user: a typical
use of this function is for serializing in string-form a `m:wxFont` object.

See: `getNativeFontInfoUserDesc/1`
""".
-spec getNativeFontInfoDesc(This) -> unicode:charlist() when
	This::wxFont().
getNativeFontInfoDesc(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetNativeFontInfoDesc),
  wxe_util:rec(?wxFont_GetNativeFontInfoDesc).

-doc """
Returns a user-friendly string for this font object.

Returned string is always non-empty unless the font is invalid (in which case an assert
is triggered).

The string does not encode all `m:wxFont` infos under all platforms; e.g. under wxMSW the
font family is not present in the returned string.

Some examples of the formats of returned strings (which are platform-dependent) are in `SetNativeFontInfoUserDesc()`
(not implemented in wx).

See: `getNativeFontInfoDesc/1`
""".
-spec getNativeFontInfoUserDesc(This) -> unicode:charlist() when
	This::wxFont().
getNativeFontInfoUserDesc(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetNativeFontInfoUserDesc),
  wxe_util:rec(?wxFont_GetNativeFontInfoUserDesc).

-doc """
Gets the point size as an integer number.

This function is kept for compatibility reasons. New code should use `GetFractionalPointSize()`
(not implemented in wx) and support fractional point sizes.

See: `setPointSize/2`
""".
-spec getPointSize(This) -> integer() when
	This::wxFont().
getPointSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetPointSize),
  wxe_util:rec(?wxFont_GetPointSize).

-doc """
Gets the font style.

See ?wxFontStyle for a list of valid styles.

See: `setStyle/2`
""".
%%  Res = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec getStyle(This) -> wx:wx_enum() when
	This::wxFont().
getStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetStyle),
  wxe_util:rec(?wxFont_GetStyle).

-doc """
Returns true if the font is underlined, false otherwise.

See: `setUnderlined/2`
""".
-spec getUnderlined(This) -> boolean() when
	This::wxFont().
getUnderlined(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetUnderlined),
  wxe_util:rec(?wxFont_GetUnderlined).

-doc """
Gets the font weight.

See ?wxFontWeight for a list of valid weight identifiers.

See: `setWeight/2`
""".
%%  Res = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-spec getWeight(This) -> wx:wx_enum() when
	This::wxFont().
getWeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_GetWeight),
  wxe_util:rec(?wxFont_GetWeight).

-doc "Equivalent to: `isOk/1`".
-spec ok(This) -> boolean() when
	This::wxFont().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

-doc "Returns true if this object is a valid font, false otherwise.".
-spec isOk(This) -> boolean() when
	This::wxFont().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,?get_env(),?wxFont_IsOk),
  wxe_util:rec(?wxFont_IsOk).

-doc """
Sets the default font encoding.

See:
* [Overview fontencoding](https://docs.wxwidgets.org/3.2/overview_fontencoding.html#overview_fontencoding)

* `getDefaultEncoding/0`
""".
%%  Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-spec setDefaultEncoding(Encoding) -> 'ok' when
	Encoding::wx:wx_enum().
setDefaultEncoding(Encoding)
 when is_integer(Encoding) ->
  wxe_util:queue_cmd(Encoding,?get_env(),?wxFont_SetDefaultEncoding).

-doc """
Sets the facename for the font.

Remark: To avoid portability problems, don't rely on a specific face, but specify the
font family instead (see ?wxFontFamily and `setFamily/2`).

Return: true if the given face name exists; if the face name doesn't exist in the user's
system then the font is invalidated (so that `isOk/1` will return false) and false is returned.

See:
* `getFaceName/1`

* `setFamily/2`
""".
-spec setFaceName(This, FaceName) -> boolean() when
	This::wxFont(), FaceName::unicode:chardata().
setFaceName(#wx_ref{type=ThisT}=This,FaceName)
 when ?is_chardata(FaceName) ->
  ?CLASS(ThisT,wxFont),
  FaceName_UC = unicode:characters_to_binary(FaceName),
  wxe_util:queue_cmd(This,FaceName_UC,?get_env(),?wxFont_SetFaceName),
  wxe_util:rec(?wxFont_SetFaceName).

-doc """
Sets the font family.

As described in ?wxFontFamily docs the given `family` value acts as a rough, basic
indication of the main font properties (look, spacing).

Note that changing the font family results in changing the font face name.

See:
* `getFamily/1`

* `setFaceName/2`
""".
%%  Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-spec setFamily(This, Family) -> 'ok' when
	This::wxFont(), Family::wx:wx_enum().
setFamily(#wx_ref{type=ThisT}=This,Family)
 when is_integer(Family) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Family,?get_env(),?wxFont_SetFamily).

-doc """
Sets the font size in points to an integer value.

This is a legacy version of the function only supporting integer point sizes. It can
still be used, but to avoid unnecessarily restricting the font size in points to integer
values, consider using the new (added in wxWidgets 3.1.2) `SetFractionalPointSize()` (not
implemented in wx) function instead.
""".
-spec setPointSize(This, PointSize) -> 'ok' when
	This::wxFont(), PointSize::integer().
setPointSize(#wx_ref{type=ThisT}=This,PointSize)
 when is_integer(PointSize) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,PointSize,?get_env(),?wxFont_SetPointSize).

-doc """
Sets the font style.

See: `getStyle/1`
""".
%%  Style = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-spec setStyle(This, Style) -> 'ok' when
	This::wxFont(), Style::wx:wx_enum().
setStyle(#wx_ref{type=ThisT}=This,Style)
 when is_integer(Style) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Style,?get_env(),?wxFont_SetStyle).

-doc """
Sets underlining.

See: `getUnderlined/1`
""".
-spec setUnderlined(This, Underlined) -> 'ok' when
	This::wxFont(), Underlined::boolean().
setUnderlined(#wx_ref{type=ThisT}=This,Underlined)
 when is_boolean(Underlined) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Underlined,?get_env(),?wxFont_SetUnderlined).

-doc """
Sets the font weight.

See: `getWeight/1`
""".
%%  Weight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-spec setWeight(This, Weight) -> 'ok' when
	This::wxFont(), Weight::wx:wx_enum().
setWeight(#wx_ref{type=ThisT}=This,Weight)
 when is_integer(Weight) ->
  ?CLASS(ThisT,wxFont),
  wxe_util:queue_cmd(This,Weight,?get_env(),?wxFont_SetWeight).

-doc "Destroys the object".
-spec destroy(This::wxFont()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxFont),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
