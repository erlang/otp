%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
-include("wxe.hrl").
-export([destroy/1,getBitmap/1,new/0,new/1,setBitmap/2]).

%% inherited exports
-export([parent_class/1]).

-type wxBitmapDataObject() :: wx:wx_object().
-export_type([wxBitmapDataObject/0]).
%% @hidden
parent_class(wxDataObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

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
  MOpts = fun({bitmap, #wx_ref{type=BitmapT}} = Arg) ->   ?CLASS(BitmapT,wxBitmap),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxBitmapDataObject_new_1_0),
  wxe_util:rec(?wxBitmapDataObject_new_1_0);
new(#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(Bitmap,?get_env(),?wxBitmapDataObject_new_1_1),
  wxe_util:rec(?wxBitmapDataObject_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmapdataobject.html#wxbitmapdataobjectgetbitmap">external documentation</a>.
-spec getBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxBitmapDataObject().
getBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  wxe_util:queue_cmd(This,?get_env(),?wxBitmapDataObject_GetBitmap),
  wxe_util:rec(?wxBitmapDataObject_GetBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxbitmapdataobject.html#wxbitmapdataobjectsetbitmap">external documentation</a>.
-spec setBitmap(This, Bitmap) -> 'ok' when
	This::wxBitmapDataObject(), Bitmap::wxBitmap:wxBitmap().
setBitmap(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxBitmapDataObject),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxBitmapDataObject_SetBitmap).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxBitmapDataObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxBitmapDataObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxBitmapDataObject_destroy),
  ok.
 %% From wxDataObject
