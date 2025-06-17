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

-module(wxBitmapDataObject).
-moduledoc """
`m:wxBitmapDataObject` is a specialization of `m:wxDataObject` for bitmap data.

It can be used without change to paste data into the `m:wxClipboard` or a `wxDropSource`
(not implemented in wx). A user may wish to derive a new class from this class for
providing a bitmap on-demand in order to minimize memory consumption when offering data in
several formats, such as a bitmap and GIF.

This class may be used as is, but `getBitmap/1` may be overridden to increase efficiency.

See:
* [Overview dnd](https://docs.wxwidgets.org/3.2/overview_dnd.html#overview_dnd)

* `m:wxDataObject`

* `m:wxFileDataObject`

* `m:wxTextDataObject`

* `m:wxDataObject`

This class is derived, and can use functions, from:

* `m:wxDataObject`

wxWidgets docs: [wxBitmapDataObject](https://docs.wxwidgets.org/3.2/classwx_bitmap_data_object.html)
""".
-include("wxe.hrl").
-export([destroy/1,getBitmap/1,new/0,new/1,setBitmap/2]).

%% inherited exports
-export([parent_class/1]).

-type wxBitmapDataObject() :: wx:wx_object().
-export_type([wxBitmapDataObject/0]).
-doc false.
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxBitmapDataObject().

new() ->
  new([]).

-doc "Constructor, optionally passing a bitmap (otherwise use `setBitmap/2` later).".
-spec new([Option]) -> wxBitmapDataObject() when
	Option :: {'bitmap', wxBitmap:wxBitmap()};
      (Bitmap) -> wxBitmapDataObject() when
	Bitmap::wxBitmap:wxBitmap().
new(Options)
 when is_list(Options) ->
  MOpts = fun({bitmap, #wx_ref{type=BitmapT}} = Arg) ->   ?CLASS(BitmapT,wxBitmap),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxBitmapDataObject_new_1_0),
  wxe_util:rec(?wxBitmapDataObject_new_1_0);
new(#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(Bitmap,?get_env(),?wxBitmapDataObject_new_1_1),
  wxe_util:rec(?wxBitmapDataObject_new_1_1).

-doc """
Returns the bitmap associated with the data object.

You may wish to override this method when offering data on-demand, but this is not
required by wxWidgets' internals. Use this method to get data in bitmap form from the `m:wxClipboard`.
""".
-spec getBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxBitmapDataObject().
getBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmapDataObject_GetBitmap),
  wxe_util:rec(?wxBitmapDataObject_GetBitmap).

-doc """
Sets the bitmap associated with the data object.

This method is called when the data object receives data. Usually there will be no reason
to override this function.
""".
-spec setBitmap(This, Bitmap) -> 'ok' when
	This::wxBitmapDataObject(), Bitmap::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxBitmapDataObject_SetBitmap).

-doc "Destroys the object".
-spec destroy(This::wxBitmapDataObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmapDataObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxBitmapDataObject_destroy),
  ok.
 %% From wxDataObject
