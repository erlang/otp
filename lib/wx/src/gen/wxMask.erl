%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmask.html">wxMask</a>.
%% @type wxMask().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxMask).
-include("wxe.hrl").
-export([create/2,create/3,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxMask()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
new() ->
  wxe_util:construct(?wxMask_new_0,
  <<>>).

%% @spec (Bitmap::wxBitmap:wxBitmap()) -> wxMask()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
new(#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:construct(?wxMask_new_1,
  <<BitmapRef:32/?UI>>).

%% @spec (Bitmap::wxBitmap:wxBitmap(),X::integer()|term()) -> wxMask()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(Bitmap::wxBitmap:wxBitmap(), PaletteIndex::integer()) -> wxMask() </c>
%% </p>
%% <p><c>
%% new(Bitmap::wxBitmap:wxBitmap(), Colour::wx:colour()) -> wxMask() </c>
%% </p>
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

%% @spec (This::wxMask(), Bitmap::wxBitmap:wxBitmap()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmask.html#wxmaskcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxMask_Create_1,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @spec (This::wxMask(),Bitmap::wxBitmap:wxBitmap(),X::integer()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxmask.html#wxmaskcreate">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% create(This::wxMask(), Bitmap::wxBitmap:wxBitmap(), PaletteIndex::integer()) -> bool() </c>
%% </p>
%% <p><c>
%% create(This::wxMask(), Bitmap::wxBitmap:wxBitmap(), Colour::wx:colour()) -> bool() </c>
%% </p>
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

%% @spec (This::wxMask()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMask),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
