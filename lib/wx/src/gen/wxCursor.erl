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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html">wxCursor</a>.
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

-export_type([wxCursor/0]).
-deprecated([new/3,new/4]).

%% @hidden
parent_class(wxBitmap) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxCursor() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
-spec new() -> wxCursor().
new() ->
  wxe_util:construct(?wxCursor_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
%% <br /> Also:<br />
%% new(Image) -> wxCursor() when<br />
%% 	Image::wxImage:wxImage().<br />
%% 
-spec new(CursorId) -> wxCursor() when
	CursorId::integer();
      (Image) -> wxCursor() when
	Image::wxImage:wxImage().
new(CursorId)
 when is_integer(CursorId) ->
  wxe_util:construct(?wxCursor_new_1_0,
  <<CursorId:32/?UI>>);
new(#wx_ref{type=ImageT,ref=ImageRef}) ->
  ?CLASS(ImageT,wxImage),
  wxe_util:construct(?wxCursor_new_1_1,
  <<ImageRef:32/?UI>>).

%% @equiv new(Bits,Width,Height, [])
-spec new(Bits, Width, Height) -> wxCursor() when
	Bits::binary(), Width::integer(), Height::integer().

new(Bits,Width,Height)
 when is_binary(Bits),is_integer(Width),is_integer(Height) ->
  new(Bits,Width,Height, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorwxcursor">external documentation</a>.
-spec new(Bits, Width, Height, [Option]) -> wxCursor() when
	Bits::binary(), Width::integer(), Height::integer(),
	Option :: {'hotSpotX', integer()}
		 | {'hotSpotY', integer()}.
new(Bits,Width,Height, Options)
 when is_binary(Bits),is_integer(Width),is_integer(Height),is_list(Options) ->
  wxe_util:send_bin(Bits),
  MOpts = fun({hotSpotX, HotSpotX}, Acc) -> [<<1:32/?UI,HotSpotX:32/?UI>>|Acc];
          ({hotSpotY, HotSpotY}, Acc) -> [<<2:32/?UI,HotSpotY:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxCursor_new_4,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcursor.html#wxcursorok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxCursor().
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxCursor),
  wxe_util:call(?wxCursor_Ok,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxCursor()) -> 'ok'.
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
