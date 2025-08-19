%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2008-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxRegion).
-moduledoc """
A `m:wxRegion` represents a simple or complex region on a device context or window.

This class uses reference counting and copy-on-write internally so that assignments
between two instances of this class are very cheap. You can therefore use actual objects
instead of pointers without efficiency problems. If an instance of this class is changed
it will create its own data internally so that other instances, which previously shared
the data using the reference counting, are not affected.

Predefined objects (include wx.hrl):

* ?wxNullRegion

wxWidgets docs: [wxRegion](https://docs.wxwidgets.org/3.2/classwx_region.html)
""".
-include("wxe.hrl").
-export(['Xor'/2,'Xor'/5,clear/1,contains/2,contains/3,contains/5,convertToBitmap/1,
  destroy/1,getBox/1,intersect/2,intersect/5,isEmpty/1,new/0,new/1,new/2,
  new/4,offset/2,offset/3,subtract/2,union/2,union/3,union/4,union/5]).

%% inherited exports
-export([parent_class/1]).

-type wxRegion() :: wx:wx_object().
-export_type([wxRegion/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Default constructor.

This constructor creates an invalid, or null, object, i.e. calling IsOk() on it returns
false and `isEmpty/1` returns true.
""".
-spec new() -> wxRegion().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxRegion_new_0),
  wxe_util:rec(?wxRegion_new_0).

-doc """
Constructs a region using a bitmap.

See `union/5` for more details.
""".
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

-doc "Constructs a rectangular region from the top left point and the bottom right point.".
-spec new(TopLeft, BottomRight) -> wxRegion() when
	TopLeft::{X::integer(), Y::integer()}, BottomRight::{X::integer(), Y::integer()}.
new({TopLeftX,TopLeftY} = TopLeft,{BottomRightX,BottomRightY} = BottomRight)
 when is_integer(TopLeftX),is_integer(TopLeftY),is_integer(BottomRightX),is_integer(BottomRightY) ->
  wxe_util:queue_cmd(TopLeft,BottomRight,?get_env(),?wxRegion_new_2),
  wxe_util:rec(?wxRegion_new_2).

-doc "Constructs a rectangular region with the given position and size.".
-spec new(X, Y, Width, Height) -> wxRegion() when
	X::integer(), Y::integer(), Width::integer(), Height::integer().
new(X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  wxe_util:queue_cmd(X,Y,Width,Height,?get_env(),?wxRegion_new_4),
  wxe_util:rec(?wxRegion_new_4).

-doc """
Clears the current region.

The object becomes invalid, or null, after being cleared.
""".
-spec clear(This) -> 'ok' when
	This::wxRegion().
clear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_Clear).

-doc """
Returns a value indicating whether the given rectangle is contained within the region.

This method always returns `wxOutRegion` for an invalid region but may, nevertheless, be
safely called in this case.

Return: One of ?wxOutRegion, ?wxPartRegion or ?wxInRegion.

Note: On Windows, only ?wxOutRegion and ?wxInRegion are returned; a value ?wxInRegion
then indicates that all or some part of the region is contained in this region.
""".
%%  Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
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

-doc """
Returns a value indicating whether the given point is contained within the region.

This method always returns `wxOutRegion` for an invalid region but may, nevertheless, be
safely called in this case.

Return: The return value is one of `wxOutRegion` and `wxInRegion`.
""".
%%  Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, X, Y) -> wx:wx_enum() when
	This::wxRegion(), X::integer(), Y::integer().
contains(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxRegion_Contains_2),
  wxe_util:rec(?wxRegion_Contains_2).

-doc """
Returns a value indicating whether the given rectangle is contained within the region.

This method always returns `wxOutRegion` for an invalid region but may, nevertheless, be
safely called in this case.

Return: One of ?wxOutRegion, ?wxPartRegion or ?wxInRegion.

Note: On Windows, only ?wxOutRegion and ?wxInRegion are returned; a value ?wxInRegion
then indicates that all or some part of the region is contained in this region.
""".
%%  Res = ?wxOutRegion | ?wxPartRegion | ?wxInRegion
-spec contains(This, X, Y, Width, Height) -> wx:wx_enum() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
contains(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Contains_4),
  wxe_util:rec(?wxRegion_Contains_4).

-doc """
Convert the region to a black and white bitmap with the white pixels being inside the
region.

This method can't be used for invalid region.
""".
-spec convertToBitmap(This) -> wxBitmap:wxBitmap() when
	This::wxRegion().
convertToBitmap(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_ConvertToBitmap),
  wxe_util:rec(?wxRegion_ConvertToBitmap).

-doc "".
-spec getBox(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxRegion().
getBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_GetBox),
  wxe_util:rec(?wxRegion_GetBox).

-doc """
Finds the intersection of this region and another region.

This method always fails, i.e. returns false, if this region is invalid but may
nevertheless be safely used even in this case.

Return: true if successful, false otherwise.

Remark: Creates the intersection of the two regions, that is, the parts which are in both
regions. The result is stored in this region.
""".
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

-doc """
Finds the intersection of this region and another, rectangular region, specified using
position and size.

This method always fails, i.e. returns false, if this region is invalid but may
nevertheless be safely used even in this case.

Return: true if successful, false otherwise.

Remark: Creates the intersection of the two regions, that is, the parts which are in both
regions. The result is stored in this region.
""".
-spec intersect(This, X, Y, Width, Height) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
intersect(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Intersect_4),
  wxe_util:rec(?wxRegion_Intersect_4).

-doc """
Returns true if the region is empty, false otherwise.

Always returns true if the region is invalid.
""".
-spec isEmpty(This) -> boolean() when
	This::wxRegion().
isEmpty(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,?get_env(),?wxRegion_IsEmpty),
  wxe_util:rec(?wxRegion_IsEmpty).

-doc """
Subtracts a region from this region.

This method always fails, i.e. returns false, if this region is invalid but may
nevertheless be safely used even in this case.

Return: true if successful, false otherwise.

Remark: This operation combines the parts of 'this' region that are not part of the
second region. The result is stored in this region.
""".
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

-doc "".
-spec offset(This, Pt) -> boolean() when
	This::wxRegion(), Pt::{X::integer(), Y::integer()}.
offset(#wx_ref{type=ThisT}=This,{PtX,PtY} = Pt)
 when is_integer(PtX),is_integer(PtY) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,Pt,?get_env(),?wxRegion_Offset_1),
  wxe_util:rec(?wxRegion_Offset_1).

-doc """
Moves the region by the specified offsets in horizontal and vertical directions.

This method can't be called if the region is invalid as it doesn't make sense to offset
it then. Attempts to do it will result in assert failure.

Return: true if successful, false otherwise (the region is unchanged then).
""".
-spec offset(This, X, Y) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer().
offset(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxRegion_Offset_2),
  wxe_util:rec(?wxRegion_Offset_2).

-doc """
Finds the union of this region and another, rectangular region.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given rectangle.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region. The result is stored in this region.
""".
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

-doc(#{equiv => union(This,Bmp,TransColour, [])}).
-spec union(This, Bmp, TransColour) -> boolean() when
	This::wxRegion(), Bmp::wxBitmap:wxBitmap(), TransColour::wx:wx_colour().

union(This,Bmp,TransColour)
 when is_record(This, wx_ref),is_record(Bmp, wx_ref),?is_colordata(TransColour) ->
  union(This,Bmp,TransColour, []).

-doc """
Finds the union of this region and the non-transparent pixels of a bitmap.

Colour to be treated as transparent is specified in the `transColour` argument, along
with an optional colour tolerance value.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region. The result is stored in this region.
""".
-spec union(This, Bmp, TransColour, [Option]) -> boolean() when
	This::wxRegion(), Bmp::wxBitmap:wxBitmap(), TransColour::wx:wx_colour(),
	Option :: {'tolerance', integer()}.
union(#wx_ref{type=ThisT}=This,#wx_ref{type=BmpT}=Bmp,TransColour, Options)
 when ?is_colordata(TransColour),is_list(Options) ->
  ?CLASS(ThisT,wxRegion),
  ?CLASS(BmpT,wxBitmap),
  MOpts = fun({tolerance, _tolerance} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Bmp,wxe_util:color(TransColour), Opts,?get_env(),?wxRegion_Union_3),
  wxe_util:rec(?wxRegion_Union_3).

-doc """
Finds the union of this region and another, rectangular region, specified using position
and size.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given rectangle.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region. The result is stored in this region.
""".
-spec union(This, X, Y, Width, Height) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
union(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Union_4),
  wxe_util:rec(?wxRegion_Union_4).

-doc """
Finds the Xor of this region and another region.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given `region`.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region, except for any overlapping areas. The result is stored in this region.
""".
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

-doc """
Finds the Xor of this region and another, rectangular region, specified using position
and size.

This method can be used even if this region is invalid and has the natural behaviour in
this case, i.e. makes this region equal to the given rectangle.

Return: true if successful, false otherwise.

Remark: This operation creates a region that combines all of this region and the second
region, except for any overlapping areas. The result is stored in this region.
""".
-spec 'Xor'(This, X, Y, Width, Height) -> boolean() when
	This::wxRegion(), X::integer(), Y::integer(), Width::integer(), Height::integer().
'Xor'(#wx_ref{type=ThisT}=This,X,Y,Width,Height)
 when is_integer(X),is_integer(Y),is_integer(Width),is_integer(Height) ->
  ?CLASS(ThisT,wxRegion),
  wxe_util:queue_cmd(This,X,Y,Width,Height,?get_env(),?wxRegion_Xor_4),
  wxe_util:rec(?wxRegion_Xor_4).

-doc "Destroys the object".
-spec destroy(This::wxRegion()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxRegion),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
