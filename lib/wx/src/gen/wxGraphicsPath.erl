%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
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
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxGraphicsPath).
-moduledoc """
A `m:wxGraphicsPath` is a native representation of a geometric path.

The contents are specific and private to the respective renderer. Instances are reference
counted and can therefore be assigned as usual. The only way to get a valid instance is by
using `wxGraphicsContext:createPath/1` or `wxGraphicsRenderer:createPath/1`.

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsPath](https://docs.wxwidgets.org/3.2/classwx_graphics_path.html)
""".
-include("wxe.hrl").
-export([addArc/6,addArc/7,addArcToPoint/6,addCircle/4,addCurveToPoint/4,addCurveToPoint/7,
  addEllipse/5,addLineToPoint/2,addLineToPoint/3,addPath/2,addQuadCurveToPoint/5,
  addRectangle/5,addRoundedRectangle/6,closeSubpath/1,contains/2,contains/3,
  contains/4,getBox/1,getCurrentPoint/1,moveToPoint/2,moveToPoint/3,
  transform/2]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-type wxGraphicsPath() :: wx:wx_object().
-export_type([wxGraphicsPath/0]).
-doc false.
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Begins a new subpath at `p`.".
-spec moveToPoint(This, P) -> 'ok' when
	This::wxGraphicsPath(), P::{X::float(), Y::float()}.
moveToPoint(#wx_ref{type=ThisT}=This,{PX,PY} = P)
 when is_number(PX),is_number(PY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,P,?get_env(),?wxGraphicsPath_MoveToPoint_1).

-doc "Begins a new subpath at (`x`,`y`).".
-spec moveToPoint(This, X, Y) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number().
moveToPoint(#wx_ref{type=ThisT}=This,X,Y)
 when is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxGraphicsPath_MoveToPoint_2).

-doc "".
-spec addArc(This, C, R, StartAngle, EndAngle, Clockwise) -> 'ok' when
	This::wxGraphicsPath(), C::{X::float(), Y::float()}, R::number(), StartAngle::number(), EndAngle::number(), Clockwise::boolean().
addArc(#wx_ref{type=ThisT}=This,{CX,CY} = C,R,StartAngle,EndAngle,Clockwise)
 when is_number(CX),is_number(CY),is_number(R),is_number(StartAngle),is_number(EndAngle),is_boolean(Clockwise) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,C,R,StartAngle,EndAngle,Clockwise,?get_env(),?wxGraphicsPath_AddArc_5).

-doc """
Adds an arc of a circle.

The circle is defined by the coordinates of its centre (`x`, `y`) or `c` and its radius `r`.
The arc goes from the starting angle `startAngle` to `endAngle` either clockwise or
counter-clockwise depending on the value of `clockwise` argument.

The angles are measured in radians but, contrary to the usual mathematical convention,
are always `clockwise` from the horizontal axis.

If for clockwise arc `endAngle` is less than `startAngle` it will be progressively
increased by 2*pi until it is greater than `startAngle`. If for counter-clockwise arc `endAngle`
is greater than `startAngle` it will be progressively decreased by 2*pi until it is less
than `startAngle`.

If there is a current point set, an initial line segment will be added to the path to
connect the current point to the beginning of the arc.
""".
-spec addArc(This, X, Y, R, StartAngle, EndAngle, Clockwise) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), R::number(), StartAngle::number(), EndAngle::number(), Clockwise::boolean().
addArc(#wx_ref{type=ThisT}=This,X,Y,R,StartAngle,EndAngle,Clockwise)
 when is_number(X),is_number(Y),is_number(R),is_number(StartAngle),is_number(EndAngle),is_boolean(Clockwise) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,R,StartAngle,EndAngle,Clockwise,?get_env(),?wxGraphicsPath_AddArc_6).

-doc """
Adds an arc (of a circle with radius `r`) that is tangent to the line connecting current
point and (`x1`, `y1`) and to the line connecting (`x1`, `y1`) and (`x2`, `y2`).

If the current point and the starting point of the arc are different, a straight line
connecting these points is also appended. If there is no current point before the call to `addArcToPoint/6`
this function will behave as if preceded by a call to MoveToPoint(0, 0). After this call
the current point will be at the ending point of the arc.
""".
-spec addArcToPoint(This, X1, Y1, X2, Y2, R) -> 'ok' when
	This::wxGraphicsPath(), X1::number(), Y1::number(), X2::number(), Y2::number(), R::number().
addArcToPoint(#wx_ref{type=ThisT}=This,X1,Y1,X2,Y2,R)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),is_number(R) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X1,Y1,X2,Y2,R,?get_env(),?wxGraphicsPath_AddArcToPoint).

-doc """
Appends a circle around (`x`,`y`) with radius `r` as a new closed subpath.

After this call the current point will be at (x+`r`, `y`).
""".
-spec addCircle(This, X, Y, R) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), R::number().
addCircle(#wx_ref{type=ThisT}=This,X,Y,R)
 when is_number(X),is_number(Y),is_number(R) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,R,?get_env(),?wxGraphicsPath_AddCircle).

-doc """
Adds a cubic bezier curve from the current point, using two control points and an end
point.

If there is no current point before the call to `addCurveToPoint/7` this function will behave as if preceded
by a call to MoveToPoint(`c1`).
""".
-spec addCurveToPoint(This, C1, C2, E) -> 'ok' when
	This::wxGraphicsPath(), C1::{X::float(), Y::float()}, C2::{X::float(), Y::float()}, E::{X::float(), Y::float()}.
addCurveToPoint(#wx_ref{type=ThisT}=This,{C1X,C1Y} = C1,{C2X,C2Y} = C2,{EX,EY} = E)
 when is_number(C1X),is_number(C1Y),is_number(C2X),is_number(C2Y),is_number(EX),is_number(EY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,C1,C2,E,?get_env(),?wxGraphicsPath_AddCurveToPoint_3).

-doc """
Adds a cubic bezier curve from the current point, using two control points and an end
point.

If there is no current point before the call to `addCurveToPoint/7` this function will behave as if preceded
by a call to MoveToPoint(`cx1`, `cy1`).
""".
-spec addCurveToPoint(This, Cx1, Cy1, Cx2, Cy2, X, Y) -> 'ok' when
	This::wxGraphicsPath(), Cx1::number(), Cy1::number(), Cx2::number(), Cy2::number(), X::number(), Y::number().
addCurveToPoint(#wx_ref{type=ThisT}=This,Cx1,Cy1,Cx2,Cy2,X,Y)
 when is_number(Cx1),is_number(Cy1),is_number(Cx2),is_number(Cy2),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,Cx1,Cy1,Cx2,Cy2,X,Y,?get_env(),?wxGraphicsPath_AddCurveToPoint_6).

-doc """
Appends an ellipse fitting into the passed in rectangle as a new closed subpath.

After this call the current point will be at (x+`w`, y+`h/2`).
""".
-spec addEllipse(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), W::number(), H::number().
addEllipse(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsPath_AddEllipse).

-doc """
Adds a straight line from the current point to `p`.

If current point is not yet set before the call to `addLineToPoint/3` this function will behave as `moveToPoint/3`.
""".
-spec addLineToPoint(This, P) -> 'ok' when
	This::wxGraphicsPath(), P::{X::float(), Y::float()}.
addLineToPoint(#wx_ref{type=ThisT}=This,{PX,PY} = P)
 when is_number(PX),is_number(PY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,P,?get_env(),?wxGraphicsPath_AddLineToPoint_1).

-doc """
Adds a straight line from the current point to (`x`,`y`).

If current point is not yet set before the call to `addLineToPoint/3` this function will behave as `moveToPoint/3`.
""".
-spec addLineToPoint(This, X, Y) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number().
addLineToPoint(#wx_ref{type=ThisT}=This,X,Y)
 when is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxGraphicsPath_AddLineToPoint_2).

-doc """
Adds another path onto the current path.

After this call the current point will be at the added path's current point. For Direct2D
the path being appended shouldn't contain a started non-empty subpath when this function
is called.
""".
-spec addPath(This, Path) -> 'ok' when
	This::wxGraphicsPath(), Path::wxGraphicsPath().
addPath(#wx_ref{type=ThisT}=This,#wx_ref{type=PathT}=Path) ->
  ?CLASS(ThisT,wxGraphicsPath),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:queue_cmd(This,Path,?get_env(),?wxGraphicsPath_AddPath).

-doc """
Adds a quadratic bezier curve from the current point, using a control point and an end
point.

If there is no current point before the call to `addQuadCurveToPoint/5` this function will behave as if preceded
by a call to MoveToPoint(`cx`, `cy`).
""".
-spec addQuadCurveToPoint(This, Cx, Cy, X, Y) -> 'ok' when
	This::wxGraphicsPath(), Cx::number(), Cy::number(), X::number(), Y::number().
addQuadCurveToPoint(#wx_ref{type=ThisT}=This,Cx,Cy,X,Y)
 when is_number(Cx),is_number(Cy),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,Cx,Cy,X,Y,?get_env(),?wxGraphicsPath_AddQuadCurveToPoint).

-doc """
Appends a rectangle as a new closed subpath.

After this call the current point will be at (`x`, `y`).
""".
-spec addRectangle(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), W::number(), H::number().
addRectangle(#wx_ref{type=ThisT}=This,X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,W,H,?get_env(),?wxGraphicsPath_AddRectangle).

-doc """
Appends a rounded rectangle as a new closed subpath.

If `radius` equals 0 this function will behave as `addRectangle/5`, otherwise after this call the current
point will be at (x+`w`, y+`h/2`).
""".
-spec addRoundedRectangle(This, X, Y, W, H, Radius) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), W::number(), H::number(), Radius::number().
addRoundedRectangle(#wx_ref{type=ThisT}=This,X,Y,W,H,Radius)
 when is_number(X),is_number(Y),is_number(W),is_number(H),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,X,Y,W,H,Radius,?get_env(),?wxGraphicsPath_AddRoundedRectangle).

-doc """
Closes the current sub-path.

After this call the current point will be at the joined endpoint of the sub-path.
""".
-spec closeSubpath(This) -> 'ok' when
	This::wxGraphicsPath().
closeSubpath(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsPath_CloseSubpath).

-doc(#{equiv => contains(This,C, [])}).
-spec contains(This, C) -> boolean() when
	This::wxGraphicsPath(), C::{X::float(), Y::float()}.

contains(This,{CX,CY} = C)
 when is_record(This, wx_ref),is_number(CX),is_number(CY) ->
  contains(This,C, []).

-doc "Return: true if the point is within the path.".
%%  FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
-spec contains(This, X, Y) -> boolean() when
	This::wxGraphicsPath(), X::number(), Y::number();
      (This, C, [Option]) -> boolean() when
	This::wxGraphicsPath(), C::{X::float(), Y::float()},
	Option :: {'fillStyle', wx:wx_enum()}.

contains(This,X,Y)
 when is_record(This, wx_ref),is_number(X),is_number(Y) ->
  contains(This,X,Y, []);
contains(#wx_ref{type=ThisT}=This,{CX,CY} = C, Options)
 when is_number(CX),is_number(CY),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsPath),
  MOpts = fun({fillStyle, _fillStyle} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,C, Opts,?get_env(),?wxGraphicsPath_Contains_2),
  wxe_util:rec(?wxGraphicsPath_Contains_2).

-doc "Return: true if the point is within the path.".
%%  FillStyle = ?wxODDEVEN_RULE | ?wxWINDING_RULE
-spec contains(This, X, Y, [Option]) -> boolean() when
	This::wxGraphicsPath(), X::number(), Y::number(),
	Option :: {'fillStyle', wx:wx_enum()}.
contains(#wx_ref{type=ThisT}=This,X,Y, Options)
 when is_number(X),is_number(Y),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsPath),
  MOpts = fun({fillStyle, _fillStyle} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,X,Y, Opts,?get_env(),?wxGraphicsPath_Contains_3),
  wxe_util:rec(?wxGraphicsPath_Contains_3).

-doc "Gets the bounding box enclosing all points (possibly including control points).".
-spec getBox(This) -> {X::float(), Y::float(), W::float(), H::float()} when
	This::wxGraphicsPath().
getBox(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsPath_GetBox),
  wxe_util:rec(?wxGraphicsPath_GetBox).

-doc "Gets the last point of the current path, (0,0) if not yet set.".
-spec getCurrentPoint(This) -> {X::float(), Y::float()} when
	This::wxGraphicsPath().
getCurrentPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsPath_GetCurrentPoint),
  wxe_util:rec(?wxGraphicsPath_GetCurrentPoint).

-doc """
Transforms each point of this path by the matrix.

For Direct2D the current path shouldn't contain a started non-empty subpath when this
function is called.
""".
-spec transform(This, Matrix) -> 'ok' when
	This::wxGraphicsPath(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
transform(#wx_ref{type=ThisT}=This,#wx_ref{type=MatrixT}=Matrix) ->
  ?CLASS(ThisT,wxGraphicsPath),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Matrix,?get_env(),?wxGraphicsPath_Transform).

 %% From wxGraphicsObject
-doc false.
isNull(This) -> wxGraphicsObject:isNull(This).
-doc false.
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
