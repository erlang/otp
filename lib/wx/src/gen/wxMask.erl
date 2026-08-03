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

-module(wxMask).
-moduledoc """
This class encapsulates a monochrome mask bitmap, where the masked area is black and the
unmasked area is white.

When associated with a bitmap and drawn in a device context, the unmasked area of the
bitmap will be drawn, and the masked area will not be drawn.

Note: A mask can be associated also with a bitmap with an alpha channel but drawing such
bitmaps under wxMSW may be slow so using them should be avoided if drawing performance is
an important factor.

See:
* `m:wxBitmap`

* `wxDC:blit/6`

* `m:wxMemoryDC`

wxWidgets docs: [wxMask](https://docs.wxwidgets.org/3.2/classwx_mask.html)
""".
-include("wxe.hrl").
-export([create/2,create/3,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

-type wxMask() :: wx:wx_object().
-export_type([wxMask/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default constructor.".
-spec new() -> wxMask().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMask_new_0),
  wxe_util:rec(?wxMask_new_0).

-doc "Constructs a mask from a monochrome bitmap.".
-spec new(Bitmap) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap().
new(#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(Bitmap,?get_env(),?wxMask_new_1),
  wxe_util:rec(?wxMask_new_1).

-doc "Constructs a mask from a bitmap and a colour that indicates the background.".
-spec new(Bitmap, Index) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap(), Index::integer();
      (Bitmap, Colour) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().
new(#wx_ref{type=BitmapT}=Bitmap,Index)
 when is_integer(Index) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(Bitmap,Index,?get_env(),?wxMask_new_2_0),
  wxe_util:rec(?wxMask_new_2_0);
new(#wx_ref{type=BitmapT}=Bitmap,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(Bitmap,wxe_util:color(Colour),?get_env(),?wxMask_new_2_1),
  wxe_util:rec(?wxMask_new_2_1).

-doc "Constructs a mask from a monochrome bitmap.".
-spec create(This, Bitmap) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap().
create(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxMask_Create_1),
  wxe_util:rec(?wxMask_Create_1).

-doc "Constructs a mask from a bitmap and a colour that indicates the background.".
-spec create(This, Bitmap, Index) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap(), Index::integer();
      (This, Bitmap, Colour) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().
create(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,Index,?get_env(),?wxMask_Create_2_0),
  wxe_util:rec(?wxMask_Create_2_0);
create(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap,Colour)
 when ?is_colordata(Colour) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,wxe_util:color(Colour),?get_env(),?wxMask_Create_2_1),
  wxe_util:rec(?wxMask_Create_2_1).

-doc "Destroys the object".
-spec destroy(This::wxMask()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMask),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
