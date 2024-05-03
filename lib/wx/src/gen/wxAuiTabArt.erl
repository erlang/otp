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

-module(wxAuiTabArt).
-moduledoc """
Functions for wxAuiTabArt class

Tab art provider defines all the drawing functions used by `m:wxAuiNotebook`.

This allows the `m:wxAuiNotebook` to have a pluggable look-and-feel.

By default, a `m:wxAuiNotebook` uses an instance of this class called
`wxAuiDefaultTabArt` (not implemented in wx) which provides bitmap art and a
colour scheme that is adapted to the major platforms' look. You can either
derive from that class to alter its behaviour or write a completely new tab art
class.

Another example of creating a new `m:wxAuiNotebook` tab bar is
`m:wxAuiSimpleTabArt`.

Call `wxAuiNotebook:setArtProvider/2` to make use of this new tab art.

wxWidgets docs:
[wxAuiTabArt](https://docs.wxwidgets.org/3.1/classwx_aui_tab_art.html)
""".
-include("wxe.hrl").
-export([setActiveColour/2,setColour/2,setFlags/2,setMeasuringFont/2,setNormalFont/2,
  setSelectedFont/2]).

%% inherited exports
-export([parent_class/1]).

-type wxAuiTabArt() :: wx:wx_object().
-export_type([wxAuiTabArt/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetflags">external documentation</a>.
-doc "Sets flags.".
-spec setFlags(This, Flags) -> 'ok' when
	This::wxAuiTabArt(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxAuiTabArt),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxAuiTabArt_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetmeasuringfont">external documentation</a>.
-doc "Sets the font used for calculating measurements.".
-spec setMeasuringFont(This, Font) -> 'ok' when
	This::wxAuiTabArt(), Font::wxFont:wxFont().
setMeasuringFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxAuiTabArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxAuiTabArt_SetMeasuringFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetnormalfont">external documentation</a>.
-doc "Sets the normal font for drawing labels.".
-spec setNormalFont(This, Font) -> 'ok' when
	This::wxAuiTabArt(), Font::wxFont:wxFont().
setNormalFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxAuiTabArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxAuiTabArt_SetNormalFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetselectedfont">external documentation</a>.
-doc "Sets the font for drawing text for selected UI elements.".
-spec setSelectedFont(This, Font) -> 'ok' when
	This::wxAuiTabArt(), Font::wxFont:wxFont().
setSelectedFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxAuiTabArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxAuiTabArt_SetSelectedFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetcolour">external documentation</a>.
-doc """
Sets the colour of the inactive tabs.

Since: 2.9.2
""".
-spec setColour(This, Colour) -> 'ok' when
	This::wxAuiTabArt(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxAuiTabArt),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxAuiTabArt_SetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauitabart.html#wxauitabartsetactivecolour">external documentation</a>.
-doc """
Sets the colour of the selected tab.

Since: 2.9.2
""".
-spec setActiveColour(This, Colour) -> 'ok' when
	This::wxAuiTabArt(), Colour::wx:wx_colour().
setActiveColour(#wx_ref{type=ThisT}=This,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxAuiTabArt),
  wxe_util:queue_cmd(This,wxe_util:color(Colour),?get_env(),?wxAuiTabArt_SetActiveColour).

