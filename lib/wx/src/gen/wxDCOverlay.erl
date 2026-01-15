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

-module(wxDCOverlay).
-moduledoc """
Connects an overlay with a drawing DC.

See:
* `m:wxOverlay`

* `m:wxDC`

wxWidgets docs: [wxDCOverlay](https://docs.wxwidgets.org/3.2/classwx_d_c_overlay.html)
""".
-include("wxe.hrl").
-export([clear/1,destroy/1,new/2,new/6]).

%% inherited exports
-export([parent_class/1]).

-type wxDCOverlay() :: wx:wx_object().
-export_type([wxDCOverlay/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Convenience wrapper that behaves the same using the entire area of the dc.".
-spec new(Overlay, Dc) -> wxDCOverlay() when
	Overlay::wxOverlay:wxOverlay(), Dc::wxDC:wxDC().
new(#wx_ref{type=OverlayT}=Overlay,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(OverlayT,wxOverlay),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(Overlay,Dc,?get_env(),?wxDCOverlay_new_2),
  wxe_util:rec(?wxDCOverlay_new_2).

-doc """
Connects this overlay to the corresponding drawing dc, if the overlay is not initialized
yet this call will do so.
""".
-spec new(Overlay, Dc, X, Y, Width, Height) -> wxDCOverlay() when
	Overlay::wxOverlay:wxOverlay(), Dc::wxDC:wxDC(), X::integer(), Y::integer(), Width::integer(), Height::integer().
new(#wx_ref{type=OverlayT}=Overlay,#wx_ref{type=DcT}=Dc,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(OverlayT,wxOverlay),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(Overlay,Dc,X,Y,Width,Height,?get_env(),?wxDCOverlay_new_6),
  wxe_util:rec(?wxDCOverlay_new_6).

-doc "Clears the layer, restoring the state at the last init.".
-spec clear(This) -> 'ok' when
	This::wxDCOverlay().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDCOverlay),
  wxe_util:queue_cmd(This,?get_env(),?wxDCOverlay_Clear).

-doc "Destroys the object".
-spec destroy(This::wxDCOverlay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDCOverlay),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxDCOverlay_destruct),
  ok.
