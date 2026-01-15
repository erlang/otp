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

-module(wxAuiSimpleTabArt).
-moduledoc """
Another standard tab art provider for `m:wxAuiNotebook`.

`m:wxAuiSimpleTabArt` is derived from `m:wxAuiTabArt` demonstrating how to write a
completely new tab art class. It can also be used as alternative to `wxAuiDefaultTabArt`
(not implemented in wx).

This class is derived, and can use functions, from:

* `m:wxAuiTabArt`

wxWidgets docs: [wxAuiSimpleTabArt](https://docs.wxwidgets.org/3.2/classwx_aui_simple_tab_art.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/0]).

%% inherited exports
-export([parent_class/1,setActiveColour/2,setColour/2,setFlags/2,setMeasuringFont/2,
  setNormalFont/2,setSelectedFont/2]).

-type wxAuiSimpleTabArt() :: wx:wx_object().
-export_type([wxAuiSimpleTabArt/0]).
-doc false.
parent_class(wxAuiTabArt) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "".
-spec new() -> wxAuiSimpleTabArt().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAuiSimpleTabArt_new),
  wxe_util:rec(?wxAuiSimpleTabArt_new).

-doc "Destroys the object".
-spec destroy(This::wxAuiSimpleTabArt()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiSimpleTabArt),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxAuiSimpleTabArt_destroy),
  ok.
 %% From wxAuiTabArt
-doc false.
setActiveColour(This,Colour) -> wxAuiTabArt:setActiveColour(This,Colour).
-doc false.
setColour(This,Colour) -> wxAuiTabArt:setColour(This,Colour).
-doc false.
setSelectedFont(This,Font) -> wxAuiTabArt:setSelectedFont(This,Font).
-doc false.
setNormalFont(This,Font) -> wxAuiTabArt:setNormalFont(This,Font).
-doc false.
setMeasuringFont(This,Font) -> wxAuiTabArt:setMeasuringFont(This,Font).
-doc false.
setFlags(This,Flags) -> wxAuiTabArt:setFlags(This,Flags).
