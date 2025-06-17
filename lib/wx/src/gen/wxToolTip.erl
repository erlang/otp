%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxToolTip).
-moduledoc """
This class holds information about a tooltip associated with a window (see `wxWindow:setToolTip/2`).

The four static methods, `enable/1`, `setDelay/1` `wxToolTip::SetAutoPop()` (not implemented in wx) and `wxToolTip::SetReshow()`
(not implemented in wx) can be used to globally alter tooltips behaviour.

wxWidgets docs: [wxToolTip](https://docs.wxwidgets.org/3.2/classwx_tool_tip.html)
""".
-include("wxe.hrl").
-export([destroy/1,enable/1,getTip/1,getWindow/1,new/1,setDelay/1,setTip/2]).

%% inherited exports
-export([parent_class/1]).

-type wxToolTip() :: wx:wx_object().
-export_type([wxToolTip/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Enable or disable tooltips globally.

Note: May not be supported on all platforms (eg. wxCocoa).
""".
-spec enable(Flag) -> 'ok' when
	Flag::boolean().
enable(Flag)
 when is_boolean(Flag) ->
  wxe_util:queue_cmd(Flag,?get_env(),?wxToolTip_Enable).

-doc """
Set the delay after which the tooltip appears.

Note: May not be supported on all platforms.
""".
-spec setDelay(Msecs) -> 'ok' when
	Msecs::integer().
setDelay(Msecs)
 when is_integer(Msecs) ->
  wxe_util:queue_cmd(Msecs,?get_env(),?wxToolTip_SetDelay).

-doc "Constructor.".
-spec new(Tip) -> wxToolTip() when
	Tip::unicode:chardata().
new(Tip)
 when ?is_chardata(Tip) ->
  Tip_UC = unicode:characters_to_binary(Tip),
  wxe_util:queue_cmd(Tip_UC,?get_env(),?wxToolTip_new),
  wxe_util:rec(?wxToolTip_new).

-doc "Set the tooltip text.".
-spec setTip(This, Tip) -> 'ok' when
	This::wxToolTip(), Tip::unicode:chardata().
setTip(#wx_ref{type=ThisT}=This,Tip)
 when ?is_chardata(Tip) ->
  ?CLASS(ThisT,wxToolTip),
  Tip_UC = unicode:characters_to_binary(Tip),
  wxe_util:queue_cmd(This,Tip_UC,?get_env(),?wxToolTip_SetTip).

-doc "Get the tooltip text.".
-spec getTip(This) -> unicode:charlist() when
	This::wxToolTip().
getTip(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolTip),
  wxe_util:queue_cmd(This,?get_env(),?wxToolTip_GetTip),
  wxe_util:rec(?wxToolTip_GetTip).

-doc "Get the associated window.".
-spec getWindow(This) -> wxWindow:wxWindow() when
	This::wxToolTip().
getWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolTip),
  wxe_util:queue_cmd(This,?get_env(),?wxToolTip_GetWindow),
  wxe_util:rec(?wxToolTip_GetWindow).

-doc "Destroys the object".
-spec destroy(This::wxToolTip()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxToolTip),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
