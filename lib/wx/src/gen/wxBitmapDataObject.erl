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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmapdataobject.html">wxBitmapDataObject</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxDataObject}
%% </p>
%% @type wxBitmapDataObject().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxBitmapDataObject).
-include("wxe.hrl").
-export([destroy/1,getBitmap/1,new/0,new/1,setBitmap/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxBitmapDataObject()
%% @equiv new([])
new() ->
  new([]).

%% @spec (X::term()) -> wxBitmapDataObject()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmapdataobject.html#wxbitmapdataobjectwxbitmapdataobject">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% new([Option]) -> wxBitmapDataObject() </c>
%%<br /> Option = {bitmap, wxBitmap:wxBitmap()}
%% </p>
%% <p><c>
%% new(Bitmap::wxBitmap:wxBitmap()) -> wxBitmapDataObject() </c>
%% </p>
new(Options)
 when is_list(Options) ->
  MOpts = fun({bitmap, #wx_ref{type=BitmapT,ref=BitmapRef}}, Acc) ->   ?CLASS(BitmapT,wxBitmap),[<<1:32/?UI,BitmapRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxBitmapDataObject_new_1_0,
  <<BinOpt/binary>>);
new(#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:construct(?wxBitmapDataObject_new_1_1,
  <<BitmapRef:32/?UI>>).

%% @spec (This::wxBitmapDataObject()) -> wxBitmap:wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmapdataobject.html#wxbitmapdataobjectgetbitmap">external documentation</a>.
getBitmap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  wxe_util:call(?wxBitmapDataObject_GetBitmap,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxBitmapDataObject(), Bitmap::wxBitmap:wxBitmap()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxbitmapdataobject.html#wxbitmapdataobjectsetbitmap">external documentation</a>.
setBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:cast(?wxBitmapDataObject_SetBitmap,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @spec (This::wxBitmapDataObject()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmapDataObject),
  wxe_util:destroy(?wxBitmapDataObject_destroy,Obj),
  ok.
 %% From wxDataObject
