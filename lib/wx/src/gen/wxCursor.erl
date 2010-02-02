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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcursor.html">wxCursor</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxBitmap}
%% </p>
%% @type wxCursor().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxCursor).
-include("wxe.hrl").
-export([destroy/1,new/0,new/1,new/3,new/4,ok/1]).

%% inherited exports
-export([convertToImage/1,copyFromIcon/2,getDepth/1,getHeight/1,getMask/1,getPalette/1,
  getSubBitmap/2,getWidth/1,loadFile/2,loadFile/3,parent_class/1,saveFile/3,
  saveFile/4,setDepth/2,setHeight/2,setMask/2,setPalette/2,setWidth/2]).

%% @hidden
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxCursor()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
new() ->
  wxe_util:construct(?wxCursor_new_0,
  <<>>).

%% @spec (X::integer()|term()) -> wxCursor()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new(CursorId::integer()) -> wxCursor() </c>
%% </p>
%% <p><c>
%% new(Image::wxImage:wxImage()) -> wxCursor() </c>
%% </p>
new(CursorId)
 when is_integer(CursorId) ->
  wxe_util:construct(?wxCursor_new_1_0,
  <<CursorId:32/?UI>>);
new(#wx_ref{type=ImageT,ref=ImageRef}) ->
  ?CLASS(ImageT,wxImage),
  wxe_util:construct(?wxCursor_new_1_1,
  <<ImageRef:32/?UI>>).

%% @spec (Bits::binary(), Width::integer(), Height::integer()) -> wxCursor()
%% @equiv new(Bits,Width,Height, [])
new(Bits,Width,Height)
 when is_binary(Bits),is_integer(Width),is_integer(Height) ->
  new(Bits,Width,Height, []).

%% @spec (Bits::binary(), Width::integer(), Height::integer(), [Option]) -> wxCursor()
%% Option = {hotSpotX, integer()} | {hotSpotY, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
new(Bits,Width,Height, Options)
 when is_binary(Bits),is_integer(Width),is_integer(Height),is_list(Options) ->
  wxe_util:send_bin(Bits),
  MOpts = fun({hotSpotX, HotSpotX}, Acc) -> [<<1:32/?UI,HotSpotX:32/?UI>>|Acc];
          ({hotSpotY, HotSpotY}, Acc) -> [<<2:32/?UI,HotSpotY:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxCursor_new_4,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @spec (This::wxCursor()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxcursor.html#wxcursorok">external documentation</a>.
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCursor),
  wxe_util:call(?wxCursor_Ok,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxCursor()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCursor),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxBitmap
%% @hidden
setWidth(This,Width) -> wxBitmap:setWidth(This,Width).
%% @hidden
setPalette(This,Palette) -> wxBitmap:setPalette(This,Palette).
%% @hidden
setMask(This,Mask) -> wxBitmap:setMask(This,Mask).
%% @hidden
setHeight(This,Height) -> wxBitmap:setHeight(This,Height).
%% @hidden
setDepth(This,Depth) -> wxBitmap:setDepth(This,Depth).
%% @hidden
saveFile(This,Name,Type, Options) -> wxBitmap:saveFile(This,Name,Type, Options).
%% @hidden
saveFile(This,Name,Type) -> wxBitmap:saveFile(This,Name,Type).
%% @hidden
loadFile(This,Name, Options) -> wxBitmap:loadFile(This,Name, Options).
%% @hidden
loadFile(This,Name) -> wxBitmap:loadFile(This,Name).
%% @hidden
getSubBitmap(This,Rect) -> wxBitmap:getSubBitmap(This,Rect).
%% @hidden
getWidth(This) -> wxBitmap:getWidth(This).
%% @hidden
getMask(This) -> wxBitmap:getMask(This).
%% @hidden
getPalette(This) -> wxBitmap:getPalette(This).
%% @hidden
getHeight(This) -> wxBitmap:getHeight(This).
%% @hidden
getDepth(This) -> wxBitmap:getDepth(This).
%% @hidden
copyFromIcon(This,Icon) -> wxBitmap:copyFromIcon(This,Icon).
%% @hidden
convertToImage(This) -> wxBitmap:convertToImage(This).
