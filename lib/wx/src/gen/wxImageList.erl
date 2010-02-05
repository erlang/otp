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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html">wxImageList</a>.
%% @type wxImageList().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxImageList).
-include("wxe.hrl").
-export([add/2,add/3,create/3,create/4,destroy/1,draw/5,draw/6,getBitmap/2,getIcon/2,
  getImageCount/1,getSize/2,new/0,new/2,new/3,remove/2,removeAll/1,replace/3,
  replace/4]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxImageList()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistwximagelist">external documentation</a>.
new() ->
  wxe_util:construct(?wxImageList_new_0,
  <<>>).

%% @spec (Width::integer(), Height::integer()) -> wxImageList()
%% @equiv new(Width,Height, [])
new(Width,Height)
 when is_integer(Width),is_integer(Height) ->
  new(Width,Height, []).

%% @spec (Width::integer(), Height::integer(), [Option]) -> wxImageList()
%% Option = {mask, bool()} | {initialCount, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistwximagelist">external documentation</a>.
new(Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  MOpts = fun({mask, Mask}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Mask)):32/?UI>>|Acc];
          ({initialCount, InitialCount}, Acc) -> [<<2:32/?UI,InitialCount:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxImageList_new_3,
  <<Width:32/?UI,Height:32/?UI, BinOpt/binary>>).

%% @spec (This::wxImageList(), Bitmap::wxBitmap:wxBitmap()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistadd">external documentation</a>.
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxImageList_Add_1,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @spec (This::wxImageList(),Bitmap::wxBitmap:wxBitmap(),X::term()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistadd">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% add(This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), Mask::wxBitmap:wxBitmap()) -> integer() </c>
%% </p>
%% <p><c>
%% add(This::wxImageList(), Bitmap::wxBitmap:wxBitmap(), MaskColour::wx:colour()) -> integer() </c>
%% </p>
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=MaskT,ref=MaskRef}) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(MaskT,wxBitmap),
  wxe_util:call(?wxImageList_Add_2_0,
  <<ThisRef:32/?UI,BitmapRef:32/?UI,MaskRef:32/?UI>>);
add(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef},MaskColour)
 when tuple_size(MaskColour) =:= 3; tuple_size(MaskColour) =:= 4 ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxImageList_Add_2_1,
  <<ThisRef:32/?UI,BitmapRef:32/?UI,(wxe_util:colour_bin(MaskColour)):16/binary>>).

%% @spec (This::wxImageList(), Width::integer(), Height::integer()) -> bool()
%% @equiv create(This,Width,Height, [])
create(This,Width,Height)
 when is_record(This, wx_ref),is_integer(Width),is_integer(Height) ->
  create(This,Width,Height, []).

%% @spec (This::wxImageList(), Width::integer(), Height::integer(), [Option]) -> bool()
%% Option = {mask, bool()} | {initialCount, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistcreate">external documentation</a>.
create(#wx_ref{type=ThisT,ref=ThisRef},Width,Height, Options)
 when is_integer(Width),is_integer(Height),is_list(Options) ->
  ?CLASS(ThisT,wxImageList),
  MOpts = fun({mask, Mask}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Mask)):32/?UI>>|Acc];
          ({initialCount, InitialCount}, Acc) -> [<<2:32/?UI,InitialCount:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImageList_Create,
  <<ThisRef:32/?UI,Width:32/?UI,Height:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImageList(), Index::integer(), Dc::wxDC:wxDC(), X::integer(), Y::integer()) -> bool()
%% @equiv draw(This,Index,Dc,X,Y, [])
draw(This,Index,Dc,X,Y)
 when is_record(This, wx_ref),is_integer(Index),is_record(Dc, wx_ref),is_integer(X),is_integer(Y) ->
  draw(This,Index,Dc,X,Y, []).

%% @spec (This::wxImageList(), Index::integer(), Dc::wxDC:wxDC(), X::integer(), Y::integer(), [Option]) -> bool()
%% Option = {flags, integer()} | {solidBackground, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistdraw">external documentation</a>.
draw(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=DcT,ref=DcRef},X,Y, Options)
 when is_integer(Index),is_integer(X),is_integer(Y),is_list(Options) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(DcT,wxDC),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          ({solidBackground, SolidBackground}, Acc) -> [<<2:32/?UI,(wxe_util:from_bool(SolidBackground)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxImageList_Draw,
  <<ThisRef:32/?UI,Index:32/?UI,DcRef:32/?UI,X:32/?UI,Y:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxImageList(), Index::integer()) -> wxBitmap:wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistgetbitmap">external documentation</a>.
getBitmap(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetBitmap,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @spec (This::wxImageList(), Index::integer()) -> wxIcon:wxIcon()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistgeticon">external documentation</a>.
getIcon(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetIcon,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @spec (This::wxImageList()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistgetimagecount">external documentation</a>.
getImageCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetImageCount,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImageList(), Index::integer()) -> {bool(),Width::integer(),Height::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistgetsize">external documentation</a>.
getSize(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_GetSize,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @spec (This::wxImageList(), Index::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistremove">external documentation</a>.
remove(#wx_ref{type=ThisT,ref=ThisRef},Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_Remove,
  <<ThisRef:32/?UI,Index:32/?UI>>).

%% @spec (This::wxImageList()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistremoveall">external documentation</a>.
removeAll(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxImageList),
  wxe_util:call(?wxImageList_RemoveAll,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxImageList(), Index::integer(), Bitmap::wxBitmap:wxBitmap()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistreplace">external documentation</a>.
replace(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=BitmapT,ref=BitmapRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxImageList_Replace_2,
  <<ThisRef:32/?UI,Index:32/?UI,BitmapRef:32/?UI>>).

%% @spec (This::wxImageList(), Index::integer(), Bitmap::wxBitmap:wxBitmap(), Mask::wxBitmap:wxBitmap()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wximagelist.html#wximagelistreplace">external documentation</a>.
replace(#wx_ref{type=ThisT,ref=ThisRef},Index,#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=MaskT,ref=MaskRef})
 when is_integer(Index) ->
  ?CLASS(ThisT,wxImageList),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(MaskT,wxBitmap),
  wxe_util:call(?wxImageList_Replace_3,
  <<ThisRef:32/?UI,Index:32/?UI,BitmapRef:32/?UI,MaskRef:32/?UI>>).

%% @spec (This::wxImageList()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxImageList),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
