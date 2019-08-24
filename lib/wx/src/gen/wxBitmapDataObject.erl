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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmapdataobject.html">wxBitmapDataObject</a>.
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

-export_type([wxBitmapDataObject/0]).
%% @hidden
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxBitmapDataObject() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxBitmapDataObject().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmapdataobject.html#wxbitmapdataobjectwxbitmapdataobject">external documentation</a>.
%% <br /> Also:<br />
%% new(Bitmap) -> wxBitmapDataObject() when<br />
%% 	Bitmap::wxBitmap:wxBitmap().<br />
%% 
-spec new([Option]) -> wxBitmapDataObject() when
	Option :: {'bitmap', wxBitmap:wxBitmap()};
      (Bitmap) -> wxBitmapDataObject() when
	Bitmap::wxBitmap:wxBitmap().
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmapdataobject.html#wxbitmapdataobjectgetbitmap">external documentation</a>.
-spec getBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxBitmapDataObject().
getBitmap(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  wxe_util:call(?wxBitmapDataObject_GetBitmap,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmapdataobject.html#wxbitmapdataobjectsetbitmap">external documentation</a>.
-spec setBitmap(This, Bitmap) -> 'ok' when
	This::wxBitmapDataObject(), Bitmap::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=BitmapT,ref=BitmapRef}) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:cast(?wxBitmapDataObject_SetBitmap,
  <<ThisRef:32/?UI,BitmapRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxBitmapDataObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmapDataObject),
  wxe_util:destroy(?wxBitmapDataObject_destroy,Obj),
  ok.
 %% From wxDataObject
