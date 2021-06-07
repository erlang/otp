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

-module(wxRegion).
-include("wxe.hrl").
-export(['Xor'/2,'Xor'/5,clear/1,contains/2,contains/3,contains/5,convertToBitmap/1,
  destroy/1,getBox/1,intersect/2,intersect/5,isEmpty/1,new/0,new/1,new/2,
  new/4,offset/2,offset/3,subtract/2,union/2,union/3,union/4,union/5]).

%% inherited exports
-export([parent_class/1]).

-type wxRegion() :: wx:wx_object().
-export_type([wxRegion/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
-spec new() -> wxRegion().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxRegion_new_0),
  wxe_util:rec(?wxRegion_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
%% <br /> Also:<br />
%% new(Bmp) -> wxRegion() when<br />
%% 	Bmp::wxBitmap:wxBitmap().<br />
%% 
-spec new(Rect) -> wxRegion() when
	Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (Bmp) -> wxRegion() when
	Bmp::wxBitmap:wxBitmap().
new({RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  wxe_util:queue_cmd(Rect,?get_env(),?wxRegion_new_1_0),
  wxe_util:rec(?wxRegion_new_1_0);
new(#wx_ref{type=BmpT}=Bmp) ->
  ?CLASS(BmpT,wxBitmap),
  wxe_util:queue_cmd(Bmp,?get_env(),?wxRegion_new_1_1),
  wxe_util:rec(?wxRegion_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
-spec new(TopLeft, BottomRight) -> wxRegion() when
	TopLeft::{X::integer(), Y::integer()}, BottomRight::{X::integer(), Y::integer()}.
new({TopLeftX,TopLeftY} = TopLeft,{BottomRightX,BottomRightY} = BottomRight)
 when is_integer(TopLeftX),is_integer(TopLeftY),is_integer(BottomRightX),is_integer(BottomRightY) ->
  wxe_util:queue_cmd(TopLeft,BottomRight,?get_env(),?wxRegion_new_2),
  wxe_util:rec(?wxRegion_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionwxregion">external documentation</a>.
-spec new(X, Y, Width, Height) -> wxRegion() when
	X::integer(), Y::integer(), Width::integer(), Height::integer().
new(X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  wxe_util:queue_cmd(X,Y,Width,Height,?get_env(),?wxRegion_new_4),
  wxe_util:rec(?wxRegion_new_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionclear">external documentation</a>.
-spec clear(This) -> 'ok' when
	This::wxRegion().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_Clear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregioncontains">external documentation</a>.
%% <br /> Also:<br />
%% contains(This, Rect) -> wx:wx_enum() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
%%<br /> Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, Pt) -> wx:wx_enum() when
	This::wxRegion(), Pt::{X::integer(), Y::integer()};
      (This, Rect) -> wx:wx_enum() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
contains(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxRegion_Contains_1_0),
  wxe_util:rec(?wxRegion_Contains_1_0);
contains(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Contains_1_1),
  wxe_util:rec(?wxRegion_Contains_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregioncontains">external documentation</a>.
%%<br /> Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, X, Y) -> wx:wx_enum() when
	This::wxRegion(), X::integer(), Y::integer().
contains(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxRegion_Contains_2),
  wxe_util:rec(?wxRegion_Contains_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregioncontains">external documentation</a>.
%%<br /> Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, X, Y, Width, Height) -> wx:wx_enum() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
contains(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Contains_4),
  wxe_util:rec(?wxRegion_Contains_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionconverttobitmap">external documentation</a>.
-spec convertToBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxRegion().
convertToBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_ConvertToBitmap),
  wxe_util:rec(?wxRegion_ConvertToBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregiongetbox">external documentation</a>.
-spec getBox(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxRegion().
getBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_GetBox),
  wxe_util:rec(?wxRegion_GetBox).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionintersect">external documentation</a>.
%% <br /> Also:<br />
%% intersect(This, Region) -> boolean() when<br />
%% 	This::wxRegion(), Region::wxRegion().<br />
%% 
-spec intersect(This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion().
intersect(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Intersect_1_0),
  wxe_util:rec(?wxRegion_Intersect_1_0);
intersect(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxRegion_Intersect_1_1),
  wxe_util:rec(?wxRegion_Intersect_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionintersect">external documentation</a>.
-spec intersect(This, X, Y, Width, Height) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
intersect(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Intersect_4),
  wxe_util:rec(?wxRegion_Intersect_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionisempty">external documentation</a>.
-spec isEmpty(This) -> boolean() when
	This::wxRegion().
isEmpty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_IsEmpty),
  wxe_util:rec(?wxRegion_IsEmpty).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionsubtract">external documentation</a>.
%% <br /> Also:<br />
%% subtract(This, Region) -> boolean() when<br />
%% 	This::wxRegion(), Region::wxRegion().<br />
%% 
-spec subtract(This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion().
subtract(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Subtract_1_0),
  wxe_util:rec(?wxRegion_Subtract_1_0);
subtract(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxRegion_Subtract_1_1),
  wxe_util:rec(?wxRegion_Subtract_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionoffset">external documentation</a>.
-spec offset(This, Pt) -> boolean() when
	This::wxRegion(), Pt::{X::integer(), Y::integer()}.
offset(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxRegion_Offset_1),
  wxe_util:rec(?wxRegion_Offset_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionoffset">external documentation</a>.
-spec offset(This, X, Y) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer().
offset(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxRegion_Offset_2),
  wxe_util:rec(?wxRegion_Offset_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionunion">external documentation</a>.
%% <br /> Also:<br />
%% union(This, Rect) -> boolean() when<br />
%% 	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.<br />
%% 
-spec union(This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion:wxRegion() | wxBitmap:wxBitmap();
      (This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
union(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  IswxRegion = ?CLASS_T(RegionT,wxRegion),
  IswxBitmap = ?CLASS_T(RegionT,wxBitmap),
  RegionType = if
    IswxRegion ->   wxRegion;
    IswxBitmap ->   wxBitmap;
    true -> error({badarg, RegionT})
  end,
  wxe_util:queue_cmd(This,wx:typeCast(Region, RegionType),?get_env(),?wxRegion_Union_1_0),
  wxe_util:rec(?wxRegion_Union_1_0);
union(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Union_1_1),
  wxe_util:rec(?wxRegion_Union_1_1).

%% @equiv union(This,Bmp,TransColour, [])
-spec union(This, Bmp, TransColour) -> boolean() when
	This::wxRegion(), Bmp::wxBitmap:wxBitmap(), TransColour::wx:wx_colour().

union(This,Bmp,TransColour)
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),tuple_size(TransColour) =:= 3; tuple_size(TransColour) =:= 4 ->
  union(This,Bmp,TransColour, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionunion">external documentation</a>.
-spec union(This, Bmp, TransColour, [Option]) -> boolean() when
	This::wxRegion(), Bmp::wxBitmap:wxBitmap(), TransColour::wx:wx_colour(),
	Option :: {'tolerance', integer()}.
union(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp,TransColour, Options)
 when tuple_size(TransColour) =:= 3; tuple_size(TransColour) =:= 4,is_list(Options) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(BmpT,wxBitmap),
  MOpts = fun({tolerance, _tolerance} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Bmp,wxe_util:color(TransColour), Opts,?get_env(),?wxRegion_Union_3),
  wxe_util:rec(?wxRegion_Union_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionunion">external documentation</a>.
-spec union(This, X, Y, Width, Height) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
union(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Union_4),
  wxe_util:rec(?wxRegion_Union_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionxor">external documentation</a>.
%% <br /> Also:<br />
%% 'Xor'(This, Region) -> boolean() when<br />
%% 	This::wxRegion(), Region::wxRegion().<br />
%% 
-spec 'Xor'(This, Rect) -> boolean() when
	This::wxRegion(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()};
      (This, Region) -> boolean() when
	This::wxRegion(), Region::wxRegion().
'Xor'(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxRegion_Xor_1_0),
  wxe_util:rec(?wxRegion_Xor_1_0);
'Xor'(#wx_ref{type=ThisT}=This,#wx_ref{type=RegionT}=Region) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(RegionT,wxRegion),
  wxe_util:queue_cmd(This,Region,?get_env(),?wxRegion_Xor_1_1),
  wxe_util:rec(?wxRegion_Xor_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxregion.html#wxregionxor">external documentation</a>.
-spec 'Xor'(This, X, Y, Width, Height) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
'Xor'(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Xor_4),
  wxe_util:rec(?wxRegion_Xor_4).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxRegion()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxRegion),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
