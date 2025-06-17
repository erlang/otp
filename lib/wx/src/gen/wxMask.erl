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
