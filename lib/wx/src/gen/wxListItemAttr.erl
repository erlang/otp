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

-module(wxListItemAttr).
-moduledoc """
Functions for wxListItemAttr class

wxWidgets docs: [wxListItemAttr](https://docs.wxwidgets.org/3.2/classwx_list_item_attr.html)
""".
-include("wxe.hrl").
-export([destroy/1,getBackgroundColour/1,getFont/1,getTextColour/1,hasBackgroundColour/1,
  hasFont/1,hasTextColour/1,new/0,new/3,setBackgroundColour/2,setFont/2,
  setTextColour/2]).

%% inherited exports
-export([parent_class/1]).

-type wxListItemAttr() :: wx:wx_object().
-export_type([wxListItemAttr/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "".
-spec new() -> wxListItemAttr().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxListItemAttr_new_0),
  wxe_util:rec(?wxListItemAttr_new_0).

-doc "".
-spec new(ColText, ColBack, Font) -> wxListItemAttr() when
	ColText::wx:wx_colour(), ColBack::wx:wx_colour(), Font::wxFont:wxFont().
new(ColText,ColBack,#wx_ref{type=FontT}=Font)
 when ?is_colordata(ColText),?is_colordata(ColBack) ->
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(wxe_util:color(ColText),wxe_util:color(ColBack),Font,?get_env(),?wxListItemAttr_new_3),
  wxe_util:rec(?wxListItemAttr_new_3).

-doc "".
-spec getBackgroundColour(This) -> wx:wx_colour4() when
	This::wxListItemAttr().
getBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_GetBackgroundColour),
  wxe_util:rec(?wxListItemAttr_GetBackgroundColour).

-doc "".
-spec getFont(This) -> wxFont:wxFont() when
	This::wxListItemAttr().
getFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_GetFont),
  wxe_util:rec(?wxListItemAttr_GetFont).

-doc "".
-spec getTextColour(This) -> wx:wx_colour4() when
	This::wxListItemAttr().
getTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_GetTextColour),
  wxe_util:rec(?wxListItemAttr_GetTextColour).

-doc "".
-spec hasBackgroundColour(This) -> boolean() when
	This::wxListItemAttr().
hasBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_HasBackgroundColour),
  wxe_util:rec(?wxListItemAttr_HasBackgroundColour).

-doc "".
-spec hasFont(This) -> boolean() when
	This::wxListItemAttr().
hasFont(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_HasFont),
  wxe_util:rec(?wxListItemAttr_HasFont).

-doc "".
-spec hasTextColour(This) -> boolean() when
	This::wxListItemAttr().
hasTextColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,?get_env(),?wxListItemAttr_HasTextColour),
  wxe_util:rec(?wxListItemAttr_HasTextColour).

-doc "".
-spec setBackgroundColour(This, ColBack) -> 'ok' when
	This::wxListItemAttr(), ColBack::wx:wx_colour().
setBackgroundColour(#wx_ref{type=ThisT}=This,ColBack)
 when ?is_colordata(ColBack) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColBack),?get_env(),?wxListItemAttr_SetBackgroundColour).

-doc "".
-spec setFont(This, Font) -> 'ok' when
	This::wxListItemAttr(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT}=This,#wx_ref{type=FontT}=Font) ->
  ?CLASS(ThisT,wxListItemAttr),
  ?CLASS(FontT,wxFont),
  wxe_util:queue_cmd(This,Font,?get_env(),?wxListItemAttr_SetFont).

-doc "".
-spec setTextColour(This, ColText) -> 'ok' when
	This::wxListItemAttr(), ColText::wx:wx_colour().
setTextColour(#wx_ref{type=ThisT}=This,ColText)
 when ?is_colordata(ColText) ->
  ?CLASS(ThisT,wxListItemAttr),
  wxe_util:queue_cmd(This,wxe_util:color(ColText),?get_env(),?wxListItemAttr_SetTextColour).

-doc "Destroys the object".
-spec destroy(This::wxListItemAttr()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxListItemAttr),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxListItemAttr_destroy),
  ok.
