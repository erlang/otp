%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxTextAttr).
-moduledoc """
Functions for wxTextAttr class

`m:wxTextAttr` represents the character and paragraph attributes, or style, for
a range of text in a `m:wxTextCtrl` or `wxRichTextCtrl` (not implemented in wx).

When setting up a `m:wxTextAttr` object, pass a bitlist mask to `setFlags/2` to
indicate which style elements should be changed. As a convenience, when you call
a setter such as SetFont, the relevant bit will be set.

See: `m:wxTextCtrl`, `wxRichTextCtrl` (not implemented in wx)

wxWidgets docs:
[wxTextAttr](https://docs.wxwidgets.org/3.1/classwx_text_attr.html)
""".
-include("wxe.hrl").
-export([destroy/1,getAlignment/1,getBackgroundColour/1,getFlags/1,getFont/1,
  getFontEncoding/1,getFontFaceName/1,getFontSize/1,getFontStyle/1,
  getFontUnderlined/1,getFontWeight/1,getLeftIndent/1,getLeftSubIndent/1,
  getRightIndent/1,getTabs/1,getTextColour/1,hasBackgroundColour/1,
  hasFont/1,hasTextColour/1,isDefault/1,new/0,new/1,new/2,setAlignment/2,
  setBackgroundColour/2,setFlags/2,setFont/2,setFont/3,setFontEncoding/2,
  setFontFaceName/2,setFontFamily/2,setFontPixelSize/2,setFontPointSize/2,
  setFontSize/2,setFontStyle/2,setFontUnderlined/2,setFontWeight/2,
  setLeftIndent/2,setLeftIndent/3,setRightIndent/2,setTabs/2,setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxTextAttr() :: wx:wx_object().
-export_type([wxTextAttr/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
-doc "Constructors.".
-spec new() -> wxTextAttr().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxTextAttr_new_0),
  wxe_util:rec(?wxTextAttr_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
%% <br /> Also:<br />
%% new(Attr) -> wxTextAttr() when<br />
%% 	Attr::wxTextAttr().<br />
%% 
%%<br /> Alignment = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-spec new(ColText) -> wxTextAttr() when
	ColText::wx:wx_colour();
      (Attr) -> wxTextAttr() when
	Attr::wxTextAttr().

new(ColText)
 when ?is_colordata(ColText) ->
  new(ColText, []);
new(#wx_ref{type=AttrT}=Attr) ->
  ?CLASS(AttrT,wxTextAttr),
  wxe_util:queue_cmd(Attr,?get_env(),?wxTextAttr_new_1),
  wxe_util:rec(?wxTextAttr_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrwxtextattr">external documentation</a>.
%%<br /> Alignment = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-spec new(ColText, [Option]) -> wxTextAttr() when
	ColText::wx:wx_colour(),
	Option :: {'colBack', wx:wx_colour()}
		 | {'font', wxFont:wxFont()}
		 | {'alignment', wx:wx_enum()}.
new(ColText, Options)
 when ?is_colordata(ColText),is_list(Options) ->
  MOpts = fun({colBack, ColBack}) -> {colBack,wxe_util:color(ColBack)};
          ({font, #wx_ref{type=FontT}} = Arg) ->   ?CLASS(FontT,wxFont),Arg;
          ({alignment, _alignment} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(wxe_util:color(ColText), Opts,?get_env(),?wxTextAttr_new_2),
  wxe_util:rec(?wxTextAttr_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetalignment">external documentation</a>.
%%<br /> Res = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-doc """
Returns the alignment flags.

See ?wxTextAttrAlignment for a list of available styles.
""".
-spec getAlignment(This) -> wx:wx_enum() when
	This::wxTextAttr().
getAlignment(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetAlignment),
  wxe_util:rec(?wxTextAttr_GetAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetbackgroundcolour">external documentation</a>.
-doc "Returns the background colour.".
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxTextAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetBackgroundColour),
  wxe_util:rec(?wxTextAttr_GetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfont">external documentation</a>.
-doc """
Creates and returns a font specified by the font attributes in the
`m:wxTextAttr` object.

Note that `m:wxTextAttr` does not store a `m:wxFont` object, so this is only a
temporary font.

For greater efficiency, access the font attributes directly.
""".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxTextAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFont),
  wxe_util:rec(?wxTextAttr_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfontencoding">external documentation</a>.
%%<br /> Res = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-doc "Returns the font encoding.".
-spec getFontEncoding(This) -> wx:wx_enum() when
	This::wxTextAttr().
getFontEncoding(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFontEncoding),
  wxe_util:rec(?wxTextAttr_GetFontEncoding).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfontfacename">external documentation</a>.
-doc "Returns the font face name.".
-spec getFontFaceName(This) -> unicode:charlist() when
	This::wxTextAttr().
getFontFaceName(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFontFaceName),
  wxe_util:rec(?wxTextAttr_GetFontFaceName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfontsize">external documentation</a>.
-doc "Returns the font size in points.".
-spec getFontSize(This) -> integer() when
	This::wxTextAttr().
getFontSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFontSize),
  wxe_util:rec(?wxTextAttr_GetFontSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfontstyle">external documentation</a>.
%%<br /> Res = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-doc "Returns the font style.".
-spec getFontStyle(This) -> wx:wx_enum() when
	This::wxTextAttr().
getFontStyle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFontStyle),
  wxe_util:rec(?wxTextAttr_GetFontStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfontunderlined">external documentation</a>.
-doc "Returns true if the font is underlined.".
-spec getFontUnderlined(This) -> boolean() when
	This::wxTextAttr().
getFontUnderlined(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFontUnderlined),
  wxe_util:rec(?wxTextAttr_GetFontUnderlined).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetfontweight">external documentation</a>.
%%<br /> Res = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-doc "Returns the font weight.".
-spec getFontWeight(This) -> wx:wx_enum() when
	This::wxTextAttr().
getFontWeight(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFontWeight),
  wxe_util:rec(?wxTextAttr_GetFontWeight).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetleftindent">external documentation</a>.
-doc "Returns the left indent in tenths of a millimetre.".
-spec getLeftIndent(This) -> integer() when
	This::wxTextAttr().
getLeftIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetLeftIndent),
  wxe_util:rec(?wxTextAttr_GetLeftIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetleftsubindent">external documentation</a>.
-doc "Returns the left sub-indent in tenths of a millimetre.".
-spec getLeftSubIndent(This) -> integer() when
	This::wxTextAttr().
getLeftSubIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetLeftSubIndent),
  wxe_util:rec(?wxTextAttr_GetLeftSubIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetrightindent">external documentation</a>.
-doc "Returns the right indent in tenths of a millimeter.".
-spec getRightIndent(This) -> integer() when
	This::wxTextAttr().
getRightIndent(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetRightIndent),
  wxe_util:rec(?wxTextAttr_GetRightIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgettabs">external documentation</a>.
-doc """
Returns an array of tab stops, each expressed in tenths of a millimeter.

Each stop is measured from the left margin and therefore each value must be
larger than the last.
""".
-spec getTabs(This) -> [integer()] when
	This::wxTextAttr().
getTabs(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetTabs),
  wxe_util:rec(?wxTextAttr_GetTabs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgettextcolour">external documentation</a>.
-doc "Returns the text foreground colour.".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxTextAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetTextColour),
  wxe_util:rec(?wxTextAttr_GetTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrhasbackgroundcolour">external documentation</a>.
-doc "Returns true if the attribute object specifies a background colour.".
-spec hasBackgroundColour(This) -> boolean() when
	This::wxTextAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_HasBackgroundColour),
  wxe_util:rec(?wxTextAttr_HasBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrhasfont">external documentation</a>.
-doc "Returns true if the attribute object specifies any font attributes.".
-spec hasFont(This) -> boolean() when
	This::wxTextAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_HasFont),
  wxe_util:rec(?wxTextAttr_HasFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrhastextcolour">external documentation</a>.
-doc "Returns true if the attribute object specifies a text foreground colour.".
-spec hasTextColour(This) -> boolean() when
	This::wxTextAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_HasTextColour),
  wxe_util:rec(?wxTextAttr_HasTextColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrgetflags">external documentation</a>.
-doc """
Returns flags indicating which attributes are applicable.

See `setFlags/2` for a list of available flags.
""".
-spec getFlags(This) -> integer() when
	This::wxTextAttr().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_GetFlags),
  wxe_util:rec(?wxTextAttr_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrisdefault">external documentation</a>.
-doc "Returns false if we have any attributes set, true otherwise.".
-spec isDefault(This) -> boolean() when
	This::wxTextAttr().
isDefault(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxTextAttr_IsDefault),
  wxe_util:rec(?wxTextAttr_IsDefault).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetalignment">external documentation</a>.
%%<br /> Alignment = ?wxTEXT_ALIGNMENT_DEFAULT | ?wxTEXT_ALIGNMENT_LEFT | ?wxTEXT_ALIGNMENT_CENTRE | ?wxTEXT_ALIGNMENT_CENTER | ?wxTEXT_ALIGNMENT_RIGHT | ?wxTEXT_ALIGNMENT_JUSTIFIED
-doc """
Sets the paragraph alignment.

See ?wxTextAttrAlignment enumeration values.

Of these, wxTEXT_ALIGNMENT_JUSTIFIED is unimplemented. In future justification
may be supported when printing or previewing, only.
""".
-spec setAlignment(This, Alignment) -> 'ok' when
	This::wxTextAttr(), Alignment::wx:wx_enum().
setAlignment(#wx_ref{type=ThisT}=This,Alignment)
 when is_integer(Alignment) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Alignment,?get_env(),?wxTextAttr_SetAlignment).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetbackgroundcolour">external documentation</a>.
-doc "Sets the background colour.".
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxTextAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when ?is_colordata(ColBack) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxTextAttr_SetBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetflags">external documentation</a>.
-doc """
Sets the flags determining which styles are being specified.

The ?wxTextAttrFlags values can be passed in a bitlist.
""".
-spec setFlags(This, Flags) -> 'ok' when
	This::wxTextAttr(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxTextAttr_SetFlags).

%% @equiv setFont(This,Font, [])
-spec setFont(This, Font) -> 'ok' when
	This::wxTextAttr(), Font::wxFont:wxFont().

setFont(This,Font)
 when is_record(This, wx_ref),is_record(Font, wx_ref) ->
  setFont(This,Font, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfont">external documentation</a>.
-doc """
Sets the attributes for the given font.

Note that `m:wxTextAttr` does not store an actual `m:wxFont` object.
""".
-spec setFont(This, Font, [Option]) -> 'ok' when
	This::wxTextAttr(), Font::wxFont:wxFont(),
	Option :: {'flags', integer()}.
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTextAttr),
  ?CLASS(FontT,wxFont),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Font, Opts,?get_env(),?wxTextAttr_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontencoding">external documentation</a>.
%%<br /> Encoding = ?wxFONTENCODING_SYSTEM | ?wxFONTENCODING_DEFAULT | ?wxFONTENCODING_ISO8859_1 | ?wxFONTENCODING_ISO8859_2 | ?wxFONTENCODING_ISO8859_3 | ?wxFONTENCODING_ISO8859_4 | ?wxFONTENCODING_ISO8859_5 | ?wxFONTENCODING_ISO8859_6 | ?wxFONTENCODING_ISO8859_7 | ?wxFONTENCODING_ISO8859_8 | ?wxFONTENCODING_ISO8859_9 | ?wxFONTENCODING_ISO8859_10 | ?wxFONTENCODING_ISO8859_11 | ?wxFONTENCODING_ISO8859_12 | ?wxFONTENCODING_ISO8859_13 | ?wxFONTENCODING_ISO8859_14 | ?wxFONTENCODING_ISO8859_15 | ?wxFONTENCODING_ISO8859_MAX | ?wxFONTENCODING_KOI8 | ?wxFONTENCODING_KOI8_U | ?wxFONTENCODING_ALTERNATIVE | ?wxFONTENCODING_BULGARIAN | ?wxFONTENCODING_CP437 | ?wxFONTENCODING_CP850 | ?wxFONTENCODING_CP852 | ?wxFONTENCODING_CP855 | ?wxFONTENCODING_CP866 | ?wxFONTENCODING_CP874 | ?wxFONTENCODING_CP932 | ?wxFONTENCODING_CP936 | ?wxFONTENCODING_CP949 | ?wxFONTENCODING_CP950 | ?wxFONTENCODING_CP1250 | ?wxFONTENCODING_CP1251 | ?wxFONTENCODING_CP1252 | ?wxFONTENCODING_CP1253 | ?wxFONTENCODING_CP1254 | ?wxFONTENCODING_CP1255 | ?wxFONTENCODING_CP1256 | ?wxFONTENCODING_CP1257 | ?wxFONTENCODING_CP1258 | ?wxFONTENCODING_CP1361 | ?wxFONTENCODING_CP12_MAX | ?wxFONTENCODING_UTF7 | ?wxFONTENCODING_UTF8 | ?wxFONTENCODING_EUC_JP | ?wxFONTENCODING_UTF16BE | ?wxFONTENCODING_UTF16LE | ?wxFONTENCODING_UTF32BE | ?wxFONTENCODING_UTF32LE | ?wxFONTENCODING_MACROMAN | ?wxFONTENCODING_MACJAPANESE | ?wxFONTENCODING_MACCHINESETRAD | ?wxFONTENCODING_MACKOREAN | ?wxFONTENCODING_MACARABIC | ?wxFONTENCODING_MACHEBREW | ?wxFONTENCODING_MACGREEK | ?wxFONTENCODING_MACCYRILLIC | ?wxFONTENCODING_MACDEVANAGARI | ?wxFONTENCODING_MACGURMUKHI | ?wxFONTENCODING_MACGUJARATI | ?wxFONTENCODING_MACORIYA | ?wxFONTENCODING_MACBENGALI | ?wxFONTENCODING_MACTAMIL | ?wxFONTENCODING_MACTELUGU | ?wxFONTENCODING_MACKANNADA | ?wxFONTENCODING_MACMALAJALAM | ?wxFONTENCODING_MACSINHALESE | ?wxFONTENCODING_MACBURMESE | ?wxFONTENCODING_MACKHMER | ?wxFONTENCODING_MACTHAI | ?wxFONTENCODING_MACLAOTIAN | ?wxFONTENCODING_MACGEORGIAN | ?wxFONTENCODING_MACARMENIAN | ?wxFONTENCODING_MACCHINESESIMP | ?wxFONTENCODING_MACTIBETAN | ?wxFONTENCODING_MACMONGOLIAN | ?wxFONTENCODING_MACETHIOPIC | ?wxFONTENCODING_MACCENTRALEUR | ?wxFONTENCODING_MACVIATNAMESE | ?wxFONTENCODING_MACARABICEXT | ?wxFONTENCODING_MACSYMBOL | ?wxFONTENCODING_MACDINGBATS | ?wxFONTENCODING_MACTURKISH | ?wxFONTENCODING_MACCROATIAN | ?wxFONTENCODING_MACICELANDIC | ?wxFONTENCODING_MACROMANIAN | ?wxFONTENCODING_MACCELTIC | ?wxFONTENCODING_MACGAELIC | ?wxFONTENCODING_MACKEYBOARD | ?wxFONTENCODING_ISO2022_JP | ?wxFONTENCODING_MAX | ?wxFONTENCODING_MACMIN | ?wxFONTENCODING_MACMAX | ?wxFONTENCODING_UTF16 | ?wxFONTENCODING_UTF32 | ?wxFONTENCODING_UNICODE | ?wxFONTENCODING_GB2312 | ?wxFONTENCODING_BIG5 | ?wxFONTENCODING_SHIFT_JIS | ?wxFONTENCODING_EUC_KR | ?wxFONTENCODING_JOHAB | ?wxFONTENCODING_VIETNAMESE
-doc "Sets the font encoding.".
-spec setFontEncoding(This, Encoding) -> 'ok' when
	This::wxTextAttr(), Encoding::wx:wx_enum().
setFontEncoding(#wx_ref{type=ThisT}=This,Encoding)
 when is_integer(Encoding) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Encoding,?get_env(),?wxTextAttr_SetFontEncoding).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontfacename">external documentation</a>.
-doc "Sets the font face name.".
-spec setFontFaceName(This, FaceName) -> 'ok' when
	This::wxTextAttr(), FaceName::unicode:chardata().
setFontFaceName(#wx_ref{type=ThisT}=This,FaceName)
 when ?is_chardata(FaceName) ->
  ?CLASS(ThisT,wxTextAttr),
  FaceName_UC = unicode:characters_to_binary(FaceName),
  wxe_util:queue_cmd(This,FaceName_UC,?get_env(),?wxTextAttr_SetFontFaceName).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontfamily">external documentation</a>.
%%<br /> Family = ?wxFONTFAMILY_DEFAULT | ?wxFONTFAMILY_DECORATIVE | ?wxFONTFAMILY_ROMAN | ?wxFONTFAMILY_SCRIPT | ?wxFONTFAMILY_SWISS | ?wxFONTFAMILY_MODERN | ?wxFONTFAMILY_TELETYPE | ?wxFONTFAMILY_MAX | ?wxFONTFAMILY_UNKNOWN
-doc "Sets the font family.".
-spec setFontFamily(This, Family) -> 'ok' when
	This::wxTextAttr(), Family::wx:wx_enum().
setFontFamily(#wx_ref{type=ThisT}=This,Family)
 when is_integer(Family) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Family,?get_env(),?wxTextAttr_SetFontFamily).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontsize">external documentation</a>.
-doc "Sets the font size in points.".
-spec setFontSize(This, PointSize) -> 'ok' when
	This::wxTextAttr(), PointSize::integer().
setFontSize(#wx_ref{type=ThisT}=This,PointSize)
 when is_integer(PointSize) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,PointSize,?get_env(),?wxTextAttr_SetFontSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontpointsize">external documentation</a>.
-doc "Sets the font size in points.".
-spec setFontPointSize(This, PointSize) -> 'ok' when
	This::wxTextAttr(), PointSize::integer().
setFontPointSize(#wx_ref{type=ThisT}=This,PointSize)
 when is_integer(PointSize) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,PointSize,?get_env(),?wxTextAttr_SetFontPointSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontpixelsize">external documentation</a>.
-doc "Sets the font size in pixels.".
-spec setFontPixelSize(This, PixelSize) -> 'ok' when
	This::wxTextAttr(), PixelSize::integer().
setFontPixelSize(#wx_ref{type=ThisT}=This,PixelSize)
 when is_integer(PixelSize) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,PixelSize,?get_env(),?wxTextAttr_SetFontPixelSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontstyle">external documentation</a>.
%%<br /> FontStyle = ?wxFONTSTYLE_NORMAL | ?wxFONTSTYLE_ITALIC | ?wxFONTSTYLE_SLANT | ?wxFONTSTYLE_MAX
-doc "Sets the font style (normal, italic or slanted).".
-spec setFontStyle(This, FontStyle) -> 'ok' when
	This::wxTextAttr(), FontStyle::wx:wx_enum().
setFontStyle(#wx_ref{type=ThisT}=This,FontStyle)
 when is_integer(FontStyle) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,FontStyle,?get_env(),?wxTextAttr_SetFontStyle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontunderlined">external documentation</a>.
-doc "Sets the font underlining (solid line, text colour).".
-spec setFontUnderlined(This, Underlined) -> 'ok' when
	This::wxTextAttr(), Underlined::boolean().
setFontUnderlined(#wx_ref{type=ThisT}=This,Underlined)
 when is_boolean(Underlined) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Underlined,?get_env(),?wxTextAttr_SetFontUnderlined).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetfontweight">external documentation</a>.
%%<br /> FontWeight = ?wxFONTWEIGHT_INVALID | ?wxFONTWEIGHT_THIN | ?wxFONTWEIGHT_EXTRALIGHT | ?wxFONTWEIGHT_LIGHT | ?wxFONTWEIGHT_NORMAL | ?wxFONTWEIGHT_MEDIUM | ?wxFONTWEIGHT_SEMIBOLD | ?wxFONTWEIGHT_BOLD | ?wxFONTWEIGHT_EXTRABOLD | ?wxFONTWEIGHT_HEAVY | ?wxFONTWEIGHT_EXTRAHEAVY | ?wxFONTWEIGHT_MAX
-doc "Sets the font weight.".
-spec setFontWeight(This, FontWeight) -> 'ok' when
	This::wxTextAttr(), FontWeight::wx:wx_enum().
setFontWeight(#wx_ref{type=ThisT}=This,FontWeight)
 when is_integer(FontWeight) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,FontWeight,?get_env(),?wxTextAttr_SetFontWeight).

%% @equiv setLeftIndent(This,Indent, [])
-spec setLeftIndent(This, Indent) -> 'ok' when
	This::wxTextAttr(), Indent::integer().

setLeftIndent(This,Indent)
 when is_record(This, wx_ref),is_integer(Indent) ->
  setLeftIndent(This,Indent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetleftindent">external documentation</a>.
-doc """
Sets the left indent and left subindent in tenths of a millimetre.

The sub-indent is an offset from the left of the paragraph, and is used for all
but the first line in a paragraph.

A positive value will cause the first line to appear to the left of the
subsequent lines, and a negative value will cause the first line to be indented
relative to the subsequent lines.

`wxRichTextBuffer` (not implemented in wx) uses indentation to render a bulleted
item. The left indent is the distance between the margin and the bullet. The
content of the paragraph, including the first line, starts at leftMargin +
leftSubIndent. So the distance between the left edge of the bullet and the left
of the actual paragraph is leftSubIndent.
""".
-spec setLeftIndent(This, Indent, [Option]) -> 'ok' when
	This::wxTextAttr(), Indent::integer(),
	Option :: {'subIndent', integer()}.
setLeftIndent(#wx_ref{type=ThisT}=This,Indent, Options)
 when is_integer(Indent),is_list(Options) ->
  ?CLASS(ThisT,wxTextAttr),
  MOpts = fun({subIndent, _subIndent} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Indent, Opts,?get_env(),?wxTextAttr_SetLeftIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsetrightindent">external documentation</a>.
-doc "Sets the right indent in tenths of a millimetre.".
-spec setRightIndent(This, Indent) -> 'ok' when
	This::wxTextAttr(), Indent::integer().
setRightIndent(#wx_ref{type=ThisT}=This,Indent)
 when is_integer(Indent) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Indent,?get_env(),?wxTextAttr_SetRightIndent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsettabs">external documentation</a>.
-doc """
Sets the tab stops, expressed in tenths of a millimetre.

Each stop is measured from the left margin and therefore each value must be
larger than the last.
""".
-spec setTabs(This, Tabs) -> 'ok' when
	This::wxTextAttr(), Tabs::[integer()].
setTabs(#wx_ref{type=ThisT}=This,Tabs)
 when is_list(Tabs) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,Tabs,?get_env(),?wxTextAttr_SetTabs).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtextattr.html#wxtextattrsettextcolour">external documentation</a>.
-doc "Sets the text foreground colour.".
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxTextAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when ?is_colordata(ColText) ->
  ?CLASS(ThisT,wxTextAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxTextAttr_SetTextColour).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxTextAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTextAttr),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxTextAttr_destroy),
  ok.
