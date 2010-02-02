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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html">wxGraphicsPath</a>.
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

%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxGraphicsPath(), P::{X::float(),Y::float()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathmovetopoint">external documentation</a>.
moveToPoint(#wx_ref{type=ThisT,ref=ThisRef},{PX,PY})
 when is_number(PX),is_number(PY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_MoveToPoint_1,
  <<ThisRef:32/?UI,0:32,PX:64/float,PY:64/float>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathmovetopoint">external documentation</a>.
moveToPoint(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_MoveToPoint_2,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F>>).

%% @spec (This::wxGraphicsPath(), C::{X::float(),Y::float()}, R::float(), StartAngle::float(), EndAngle::float(), Clockwise::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddarc">external documentation</a>.
addArc(#wx_ref{type=ThisT,ref=ThisRef},{CX,CY},R,StartAngle,EndAngle,Clockwise)
 when is_number(CX),is_number(CY),is_float(R),is_float(StartAngle),is_float(EndAngle),is_boolean(Clockwise) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddArc_5,
  <<ThisRef:32/?UI,0:32,CX:64/float,CY:64/float,R:64/?F,StartAngle:64/?F,EndAngle:64/?F,(wxe_util:from_bool(Clockwise)):32/?UI>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float(), R::float(), StartAngle::float(), EndAngle::float(), Clockwise::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddarc">external documentation</a>.
addArc(#wx_ref{type=ThisT,ref=ThisRef},X,Y,R,StartAngle,EndAngle,Clockwise)
 when is_float(X),is_float(Y),is_float(R),is_float(StartAngle),is_float(EndAngle),is_boolean(Clockwise) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddArc_6,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,R:64/?F,StartAngle:64/?F,EndAngle:64/?F,(wxe_util:from_bool(Clockwise)):32/?UI>>).

%% @spec (This::wxGraphicsPath(), X1::float(), Y1::float(), X2::float(), Y2::float(), R::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddarctopoint">external documentation</a>.
addArcToPoint(#wx_ref{type=ThisT,ref=ThisRef},X1,Y1,X2,Y2,R)
 when is_float(X1),is_float(Y1),is_float(X2),is_float(Y2),is_float(R) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddArcToPoint,
  <<ThisRef:32/?UI,0:32,X1:64/?F,Y1:64/?F,X2:64/?F,Y2:64/?F,R:64/?F>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float(), R::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddcircle">external documentation</a>.
addCircle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,R)
 when is_float(X),is_float(Y),is_float(R) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddCircle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,R:64/?F>>).

%% @spec (This::wxGraphicsPath(), C1::{X::float(),Y::float()}, C2::{X::float(),Y::float()}, E::{X::float(),Y::float()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddcurvetopoint">external documentation</a>.
addCurveToPoint(#wx_ref{type=ThisT,ref=ThisRef},{C1X,C1Y},{C2X,C2Y},{EX,EY})
 when is_number(C1X),is_number(C1Y),is_number(C2X),is_number(C2Y),is_number(EX),is_number(EY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddCurveToPoint_3,
  <<ThisRef:32/?UI,0:32,C1X:64/float,C1Y:64/float,C2X:64/float,C2Y:64/float,EX:64/float,EY:64/float>>).

%% @spec (This::wxGraphicsPath(), Cx1::float(), Cy1::float(), Cx2::float(), Cy2::float(), X::float(), Y::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddcurvetopoint">external documentation</a>.
addCurveToPoint(#wx_ref{type=ThisT,ref=ThisRef},Cx1,Cy1,Cx2,Cy2,X,Y)
 when is_float(Cx1),is_float(Cy1),is_float(Cx2),is_float(Cy2),is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddCurveToPoint_6,
  <<ThisRef:32/?UI,0:32,Cx1:64/?F,Cy1:64/?F,Cx2:64/?F,Cy2:64/?F,X:64/?F,Y:64/?F>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddellipse">external documentation</a>.
addEllipse(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddEllipse,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsPath(), P::{X::float(),Y::float()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddlinetopoint">external documentation</a>.
addLineToPoint(#wx_ref{type=ThisT,ref=ThisRef},{PX,PY})
 when is_number(PX),is_number(PY) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddLineToPoint_1,
  <<ThisRef:32/?UI,0:32,PX:64/float,PY:64/float>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddlinetopoint">external documentation</a>.
addLineToPoint(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddLineToPoint_2,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F>>).

%% @spec (This::wxGraphicsPath(), Path::wxGraphicsPath()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddpath">external documentation</a>.
addPath(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PathT,ref=PathRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  ?CLASS(PathT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddPath,
  <<ThisRef:32/?UI,PathRef:32/?UI>>).

%% @spec (This::wxGraphicsPath(), Cx::float(), Cy::float(), X::float(), Y::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddquadcurvetopoint">external documentation</a>.
addQuadCurveToPoint(#wx_ref{type=ThisT,ref=ThisRef},Cx,Cy,X,Y)
 when is_float(Cx),is_float(Cy),is_float(X),is_float(Y) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddQuadCurveToPoint,
  <<ThisRef:32/?UI,0:32,Cx:64/?F,Cy:64/?F,X:64/?F,Y:64/?F>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float(), W::float(), H::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddrectangle">external documentation</a>.
addRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H)
 when is_float(X),is_float(Y),is_float(W),is_float(H) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float(), W::float(), H::float(), Radius::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathaddroundedrectangle">external documentation</a>.
addRoundedRectangle(#wx_ref{type=ThisT,ref=ThisRef},X,Y,W,H,Radius)
 when is_float(X),is_float(Y),is_float(W),is_float(H),is_float(Radius) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_AddRoundedRectangle,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F,W:64/?F,H:64/?F,Radius:64/?F>>).

%% @spec (This::wxGraphicsPath()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathclosesubpath">external documentation</a>.
closeSubpath(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:cast(?wxGraphicsPath_CloseSubpath,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsPath(), C::{X::float(),Y::float()}) -> bool()
%% @equiv contains(This,C, [])
contains(This,C={CX,CY})
 when is_record(This, wx_ref),is_number(CX),is_number(CY) ->
  contains(This,C, []).

%% @spec (This::wxGraphicsPath(),X::float()|term(),X::float()|term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathcontains">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% contains(This::wxGraphicsPath(), X::float(), Y::float()) -> contains(This,X,Y, []) </c></p>
%% <p><c>
%% contains(This::wxGraphicsPath(), C::{X::float(),Y::float()}, [Option]) -> bool() </c>
%%<br /> Option = {fillStyle, integer()}
%% </p>

contains(This,X,Y)
 when is_record(This, wx_ref),is_float(X),is_float(Y) ->
  contains(This,X,Y, []);
contains(#wx_ref{type=ThisT,ref=ThisRef},{CX,CY}, Options)
 when is_number(CX),is_number(CY),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsPath_Contains_2,
  <<ThisRef:32/?UI,0:32,CX:64/float,CY:64/float, BinOpt/binary>>).

%% @spec (This::wxGraphicsPath(), X::float(), Y::float(), [Option]) -> bool()
%% Option = {fillStyle, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathcontains">external documentation</a>.
contains(#wx_ref{type=ThisT,ref=ThisRef},X,Y, Options)
 when is_float(X),is_float(Y),is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsPath),
  MOpts = fun({fillStyle, FillStyle}, Acc) -> [<<1:32/?UI,FillStyle:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxGraphicsPath_Contains_3,
  <<ThisRef:32/?UI,0:32,X:64/?F,Y:64/?F, BinOpt/binary>>).

%% @spec (This::wxGraphicsPath()) -> {X::float(),Y::float(),W::float(),H::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathgetbox">external documentation</a>.
getBox(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:call(?wxGraphicsPath_GetBox,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsPath()) -> {X::float(),Y::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathgetcurrentpoint">external documentation</a>.
getCurrentPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsPath),
  wxe_util:call(?wxGraphicsPath_GetCurrentPoint,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsPath(), Matrix::wxGraphicsMatrix:wxGraphicsMatrix()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicspath.html#wxgraphicspathtransform">external documentation</a>.
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
