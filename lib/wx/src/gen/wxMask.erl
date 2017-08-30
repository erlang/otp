%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html">wxMask</a>.
%% @type wxMask().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxMask).
-include("wxe.hrl").
-export([create/2,create/3,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxMask/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxMask() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
-spec new() -> wxMask().
new() ->
  wxe_util:construct(?wxMask_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
-spec new(Bitmap) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap().
new(#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:construct(?wxMask_new_1,
  <<BitmapRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
%% <br /> Also:<br />
%% new(Bitmap, Colour) -> wxMask() when<br />
%% 	Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().<br />
%% 
-spec new(Bitmap, PaletteIndex) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap(), PaletteIndex::integer();
      (Bitmap, Colour) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().
new(#wx_ref{type=BitmapT,ref=BitmapRef},PaletteIndex)
 when is_integer(PaletteIndex) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:construct(?wxMask_new_2_0,
  <<BitmapRef:32/?UI,PaletteIndex:32/?UI>>);
new(#wx_ref{type=BitmapT,ref=BitmapRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:construct(?wxMask_new_2_1,
  <<BitmapRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskcreate">external documentation</a>.
-spec create(This, Bitmap) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap().
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxMask_Create_1,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskcreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Bitmap, Colour) -> boolean() when<br />
%% 	This::wxMask(), Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().<br />
%% 
-spec create(This, Bitmap, PaletteIndex) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap(), PaletteIndex::integer();
      (This, Bitmap, Colour) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef},PaletteIndex)
 when is_integer(PaletteIndex) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxMask_Create_2_0,
  <<ThisRef:32/?UI,BitmapRef:32/?UI,PaletteIndex:32/?UI>>);
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef},Colour)
 when tuple_size(Colour) =:= 3; tuple_size(Colour) =:= 4 ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxMask_Create_2_1,
  <<ThisRef:32/?UI,BitmapRef:32/?UI,(wxe_util:colour_bin(Colour)):16/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMask()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMask),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
