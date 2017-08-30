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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html">wxGraphicsPath</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGraphicsObject}
%% </p>
%% @type wxGraphicsPath().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsPath).
-include("wxe.hrl").
-export([addArc/6,addArc/7,addArcToPoint/6,addCircle/4,addCurveToPoint/4,addCurveToPoint/7,
  addEllipse/5,addLineToPoint/2,addLineToPoint/3,addPath/2,addQuadCurveToPoint/5,
  addRectangle/5,addRoundedRectangle/6,closeSubpath/1,contains/2,contains/3,
  contains/4,getBox/1,getCurrentPoint/1,moveToPoint/2,moveToPoint/3,
  transform/2]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-export_type([wxGraphicsPath/0]).
%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGraphicsPath() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathmovetopoint">external documentation</a>.
-spec moveToPoint(This, P) -> 'ok' when
	This::wxGraphicsPath(), P::{X::float(), Y::float()}.
moveToPoint(#wx_ref{type=ThisT,ref=ThisRef},{PX,PY})
 when is_number(PX),is_number(PY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_MoveToPoint_1,
  <<ThisRef:32/?UI,0:32,PX:64/?F,PY:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathmovetopoint">external documentation</a>.
-spec moveToPoint(This, X, Y) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number().
moveToPoint(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_MoveToPoint_2,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddarc">external documentation</a>.
-spec addArc(This, C, R, StartAngle, EndAngle, Clockwise) -> 'ok' when
	This::wxGraphicsPath(), C::{X::float(), Y::float()}, R::number(), StartAngle::number(), EndAngle::number(), Clockwise::boolean().
addArc(#wx_ref{type=ThisT,ref=ThisRef},{CX,CY},R,StartAngle,EndAngle,Clockwise)
 when is_number(CX),is_number(CY),is_number(R),is_number(StartAngle),is_number(EndAngle),is_boolean(Clockwise) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddArc_5,
  <<ThisRef:32/?UI,0:32,CX:64/?F,CY:64/?F,R:64/?F,StartAngle:64/?F,EndAngle:64/?F,(wxe_util:from_bool(Clockwise)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddarc">external documentation</a>.
-spec addArc(This, X, Y, R, StartAngle, EndAngle, Clockwise) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), R::number(), StartAngle::number(), EndAngle::number(), Clockwise::boolean().
addArc(#wx_ref{type=ThisT,ref=ThisRef},X,Y,R,StartAngle,EndAngle,Clockwise)
 when is_number(X),is_number(Y),is_number(R),is_number(StartAngle),is_number(EndAngle),is_boolean(Clockwise) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddArc_6,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,R:64/?F,StartAngle:64/?F,EndAngle:64/?F,(wxe_util:from_bool(Clockwise)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddarctopoint">external documentation</a>.
-spec addArcToPoint(This, X1, Y1, X2, Y2, R) -> 'ok' when
	This::wxGraphicsPath(), X1::number(), Y1::number(), X2::number(), Y2::number(), R::number().
addArcToPoint(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2,R)
 when is_number(X1),is_number(Y1),is_number(X2),is_number(Y2),is_number(R) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddArcToPoint,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F,R:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddcircle">external documentation</a>.
-spec addCircle(This, X, Y, R) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), R::number().
addCircle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,R)
 when is_number(X),is_number(Y),is_number(R) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddCircle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,R:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddcurvetopoint">external documentation</a>.
-spec addCurveToPoint(This, C1, C2, E) -> 'ok' when
	This::wxGraphicsPath(), C1::{X::float(), Y::float()}, C2::{X::float(), Y::float()}, E::{X::float(), Y::float()}.
addCurveToPoint(#wx_ref{type=ThisT,ref=ThisRef},{C1X,C1Y},{C2X,C2Y},{EX,EY})
 when is_number(C1X),is_number(C1Y),is_number(C2X),is_number(C2Y),is_number(EX),is_number(EY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddCurveToPoint_3,
  <<ThisRef:32/?UI,0:32,C1X:64/?F,C1Y:64/?F,C2X:64/?F,C2Y:64/?F,EX:64/?F,EY:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddcurvetopoint">external documentation</a>.
-spec addCurveToPoint(This, Cx1, Cy1, Cx2, Cy2, X, Y) -> 'ok' when
	This::wxGraphicsPath(), Cx1::number(), Cy1::number(), Cx2::number(), Cy2::number(), X::number(), Y::number().
addCurveToPoint(#wx_ref{type=ThisT,ref=ThisRef},Cx1,Cy1,Cx2,Cy2,X,Y)
 when is_number(Cx1),is_number(Cy1),is_number(Cx2),is_number(Cy2),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddCurveToPoint_6,
  <<ThisRef:32/?UI,0:32,Cx1:64/?F,Cy1:64/?F,Cx2:64/?F,Cy2:64/?F,X:64/?F,Y:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddellipse">external documentation</a>.
-spec addEllipse(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), W::number(), H::number().
addEllipse(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddEllipse,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddlinetopoint">external documentation</a>.
-spec addLineToPoint(This, P) -> 'ok' when
	This::wxGraphicsPath(), P::{X::float(), Y::float()}.
addLineToPoint(#wx_ref{type=ThisT,ref=ThisRef},{PX,PY})
 when is_number(PX),is_number(PY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddLineToPoint_1,
  <<ThisRef:32/?UI,0:32,PX:64/?F,PY:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddlinetopoint">external documentation</a>.
-spec addLineToPoint(This, X, Y) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number().
addLineToPoint(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddLineToPoint_2,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddpath">external documentation</a>.
-spec addPath(This, Path) -> 'ok' when
	This::wxGraphicsPath(), Path::wxGraphicsPath().
addPath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddPath,
  <<ThisRef:32/?UI,PathRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddquadcurvetopoint">external documentation</a>.
-spec addQuadCurveToPoint(This, Cx, Cy, X, Y) -> 'ok' when
	This::wxGraphicsPath(), Cx::number(), Cy::number(), X::number(), Y::number().
addQuadCurveToPoint(#wx_ref{type=ThisT,ref=ThisRef},Cx,Cy,X,Y)
 when is_number(Cx),is_number(Cy),is_number(X),is_number(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddQuadCurveToPoint,
  <<ThisRef:32/?UI,0:32,Cx:64/?F,Cy:64/?F,X:64/?F,Y:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddrectangle">external documentation</a>.
-spec addRectangle(This, X, Y, W, H) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), W::number(), H::number().
addRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_number(X),is_number(Y),is_number(W),is_number(H) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathaddroundedrectangle">external documentation</a>.
-spec addRoundedRectangle(This, X, Y, W, H, Radius) -> 'ok' when
	This::wxGraphicsPath(), X::number(), Y::number(), W::number(), H::number(), Radius::number().
addRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H,Radius)
 when is_number(X),is_number(Y),is_number(W),is_number(H),is_number(Radius) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddRoundedRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F,Radius:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathclosesubpath">external documentation</a>.
-spec closeSubpath(This) -> 'ok' when
	This::wxGraphicsPath().
closeSubpath(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_CloseSubpath,
  <<ThisRef:32/?UI>>).

%% @equiv contains(This,C, [])
-spec contains(This, C) -> boolean() when
	This::wxGraphicsPath(), C::{X::float(), Y::float()}.

contains(This,C={CX,CY})
 when is_record(This, wx_ref),is_number(CX),is_number(CY) ->
  contains(This,C, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathcontains">external documentation</a>.
%% <br /> Also:<br />
%% contains(This, C, [Option]) -> boolean() when<br />
%% 	This::wxGraphicsPath(), C::{X::float(), Y::float()},<br />
%% 	Option :: {'fillStyle', wx:wx_enum()}.<br />
%% 
%%<br /> FillStyle = integer
-spec contains(This, X, Y) -> boolean() when
	This::wxGraphicsPath(), X::number(), Y::number();
      (This, C, [Option]) -> boolean() when
	This::wxGraphicsPath(), C::{X::float(), Y::float()},
	Option :: {'fillStyle', wx:wx_enum()}.

contains(This,X,Y)
 when is_record(This, wx_ref),is_number(X),is_number(Y) ->
  contains(This,X,Y, []);
contains(#wx_ref{type=ThisT,ref=ThisRef},{CX,CY}, Options)
 when is_number(CX),is_number(CY),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsPath_Contains_2,
  <<ThisRef:32/?UI,0:32,CX:64/?F,CY:64/?F, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathcontains">external documentation</a>.
%%<br /> FillStyle = integer
-spec contains(This, X, Y, [Option]) -> boolean() when
	This::wxGraphicsPath(), X::number(), Y::number(),
	Option :: {'fillStyle', wx:wx_enum()}.
contains(#wx_ref{type=ThisT,ref=ThisRef},X,Y, Options)
 when is_number(X),is_number(Y),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsPath_Contains_3,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathgetbox">external documentation</a>.
-spec getBox(This) -> {X::float(), Y::float(), W::float(), H::float()} when
	This::wxGraphicsPath().
getBox(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:call(?wxGraphicsPath_GetBox,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathgetcurrentpoint">external documentation</a>.
-spec getCurrentPoint(This) -> {X::float(), Y::float()} when
	This::wxGraphicsPath().
getCurrentPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:call(?wxGraphicsPath_GetCurrentPoint,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicspath.html#wxgraphicspathtransform">external documentation</a>.
-spec transform(This, Matrix) -> 'ok' when
	This::wxGraphicsPath(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix().
transform(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MatrixT,ref=MatrixRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  ?CLASS(MatrixT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsPath_Transform,
  <<ThisRef:32/?UI,MatrixRef:32/?UI>>).

 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
