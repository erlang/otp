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

-module(wxGraphicsObject).
-moduledoc """
This class is the superclass of native graphics objects like pens etc.

It allows reference counting. Not instantiated by user code.

See:
* `m:wxGraphicsBrush`

* `m:wxGraphicsPen`

* `m:wxGraphicsMatrix`

* `m:wxGraphicsPath`

wxWidgets docs: [wxGraphicsObject](https://docs.wxwidgets.org/3.2/classwx_graphics_object.html)
""".
-include("wxe.hrl").
-export([destroy/1,getRenderer/1,isNull/1]).

%% inherited exports
-export([parent_class/1]).

-type wxGraphicsObject() :: wx:wx_object().
-export_type([wxGraphicsObject/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns the renderer that was used to create this instance, or NULL if it has not been
initialized yet.
""".
-spec getRenderer(This) -> wxGraphicsRenderer:wxGraphicsRenderer() when
	This::wxGraphicsObject().
getRenderer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsObject),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsObject_GetRenderer),
  wxe_util:rec(?wxGraphicsObject_GetRenderer).

-doc "Return: false if this object is valid, otherwise returns true.".
-spec isNull(This) -> boolean() when
	This::wxGraphicsObject().
isNull(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsObject),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsObject_IsNull),
  wxe_util:rec(?wxGraphicsObject_IsNull).

-doc "Destroys the object".
-spec destroy(This::wxGraphicsObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
