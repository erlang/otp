%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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
-include("wxe.hrl").
-export([create/2,create/3,destroy/1,new/0,new/1,new/2]).

%% inherited exports
-export([parent_class/1]).

-type wxMask() :: wx:wx_object().
-export_type([wxMask/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
-spec new() -> wxMask().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMask_new_0),
  wxe_util:rec(?wxMask_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
-spec new(Bitmap) -> wxMask() when
	Bitmap::wxBitmap:wxBitmap().
new(#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(Bitmap,?get_env(),?wxMask_new_1),
  wxe_util:rec(?wxMask_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskwxmask">external documentation</a>.
%% <br /> Also:<br />
%% new(Bitmap, Colour) -> wxMask() when<br />
%% 	Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().<br />
%% 
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskcreate">external documentation</a>.
-spec create(This, Bitmap) -> boolean() when
	This::wxMask(), Bitmap::wxBitmap:wxBitmap().
create(#wx_ref{type=ThisT}=This,#wx_ref{type=BitmapT}=Bitmap) ->
  ?CLASS(ThisT,wxMask),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:queue_cmd(This,Bitmap,?get_env(),?wxMask_Create_1),
  wxe_util:rec(?wxMask_Create_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmask.html#wxmaskcreate">external documentation</a>.
%% <br /> Also:<br />
%% create(This, Bitmap, Colour) -> boolean() when<br />
%% 	This::wxMask(), Bitmap::wxBitmap:wxBitmap(), Colour::wx:wx_colour().<br />
%% 
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

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMask()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMask),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
