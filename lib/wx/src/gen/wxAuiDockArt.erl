%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

-module(wxAuiDockArt).
-include("wxe.hrl").
-export([getColour/2,getFont/2,getMetric/2,setColour/3,setFont/3,setMetric/3]).

%% inherited exports
-export([parent_class/1]).

-type wxAuiDockArt() :: wx:wx_object().
-export_type([wxAuiDockArt/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauidockart.html#wxauidockartgetcolour">external documentation</a>.
-spec getColour(This, Id) -> wx:wx_colour4() when
	This::wxAuiDockArt(), Id::integer().
getColour(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxAuiDockArt_GetColour),
  wxe_util:rec(?wxAuiDockArt_GetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauidockart.html#wxauidockartgetfont">external documentation</a>.
-spec getFont(This, Id) -> wxFont:wxFont() when
	This::wxAuiDockArt(), Id::integer().
getFont(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxAuiDockArt_GetFont),
  wxe_util:rec(?wxAuiDockArt_GetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauidockart.html#wxauidockartgetmetric">external documentation</a>.
-spec getMetric(This, Id) -> integer() when
	This::wxAuiDockArt(), Id::integer().
getMetric(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxAuiDockArt_GetMetric),
  wxe_util:rec(?wxAuiDockArt_GetMetric).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauidockart.html#wxauidockartsetcolour">external documentation</a>.
-spec setColour(This, Id, Colour) -> 'ok' when
	This::wxAuiDockArt(), Id::integer(), Colour::wx:wx_colour().
setColour(#wx_ref{type=ThisT}=This,Id,Colour)
 when is_integer(Id),?is_colordata(Colour) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,wxe_util:color(Colour),?get_env(),?wxAuiDockArt_SetColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauidockart.html#wxauidockartsetfont">external documentation</a>.
-spec setFont(This, Id, Font) -> 'ok' when
	This::wxAuiDockArt(), Id::integer(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,Id,#wx_ref{type=FontT}=Font)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxAuiDockArt),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Id,Font,?get_env(),?wxAuiDockArt_SetFont).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauidockart.html#wxauidockartsetmetric">external documentation</a>.
-spec setMetric(This, Id, New_val) -> 'ok' when
	This::wxAuiDockArt(), Id::integer(), New_val::integer().
setMetric(#wx_ref{type=ThisT}=This,Id,New_val)
 when is_integer(Id),is_integer(New_val) ->
  ?CLASS(ThisT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Id,New_val,?get_env(),?wxAuiDockArt_SetMetric).

