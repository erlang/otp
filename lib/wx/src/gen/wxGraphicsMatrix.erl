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

-module(wxGraphicsMatrix).
-moduledoc """
A `m:wxGraphicsMatrix` is a native representation of an affine matrix.

The contents are specific and private to the respective renderer. Instances are ref
counted and can therefore be assigned as usual. The only way to get a valid instance is
via `wxGraphicsContext:createMatrix/2` or `wxGraphicsRenderer:createMatrix/2`.

This class is derived, and can use functions, from:

* `m:wxGraphicsObject`

wxWidgets docs: [wxGraphicsMatrix](https://docs.wxwidgets.org/3.2/classwx_graphics_matrix.html)
""".
-include("wxe.hrl").
-export([concat/2,get/1,invert/1,isEqual/2,isIdentity/1,rotate/2,scale/3,set/1,
  set/2,transformDistance/1,transformPoint/1,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-type wxGraphicsMatrix() :: wx:wx_object().
-export_type([wxGraphicsMatrix/0]).
-doc false.
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Concatenates the matrix passed with the current matrix.

The effect of the resulting transformation is to first apply the transformation in `t` to
the coordinates and then apply the transformation in the current matrix to the coordinates.
""".
-spec concat(This, T) -> 'ok' when
	This::wxGraphicsMatrix(), T::wxGraphicsMatrix().
concat(#wx_ref{type=ThisT}=This,#wx_ref{type=TT}=T) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,T,?get_env(),?wxGraphicsMatrix_Concat).

-doc "Returns the component values of the matrix via the argument pointers.".
-spec get(This) -> Result when
	Result ::{A::number(), B::number(), C::number(), D::number(), Tx::number(), Ty::number()},
	This::wxGraphicsMatrix().
get(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_Get),
  wxe_util:rec(?wxGraphicsMatrix_Get).

-doc "Inverts the matrix.".
-spec invert(This) -> 'ok' when
	This::wxGraphicsMatrix().
invert(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_Invert).

-doc "Returns true if the elements of the transformation matrix are equal.".
-spec isEqual(This, T) -> boolean() when
	This::wxGraphicsMatrix(), T::wxGraphicsMatrix().
isEqual(#wx_ref{type=ThisT}=This,#wx_ref{type=TT}=T) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,T,?get_env(),?wxGraphicsMatrix_IsEqual),
  wxe_util:rec(?wxGraphicsMatrix_IsEqual).

-doc "Return true if this is the identity matrix.".
-spec isIdentity(This) -> boolean() when
	This::wxGraphicsMatrix().
isIdentity(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_IsIdentity),
  wxe_util:rec(?wxGraphicsMatrix_IsIdentity).

-doc "Rotates this matrix clockwise (in radians).".
-spec rotate(This, Angle) -> 'ok' when
	This::wxGraphicsMatrix(), Angle::number().
rotate(#wx_ref{type=ThisT}=This,Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Angle,?get_env(),?wxGraphicsMatrix_Rotate).

-doc "Scales this matrix.".
-spec scale(This, XScale, YScale) -> 'ok' when
	This::wxGraphicsMatrix(), XScale::number(), YScale::number().
scale(#wx_ref{type=ThisT}=This,XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,XScale,YScale,?get_env(),?wxGraphicsMatrix_Scale).

-doc "Translates this matrix.".
-spec translate(This, Dx, Dy) -> 'ok' when
	This::wxGraphicsMatrix(), Dx::number(), Dy::number().
translate(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_number(Dx),is_number(Dy) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxGraphicsMatrix_Translate).

-doc(#{equiv => set(This, [])}).
-spec set(This) -> 'ok' when
	This::wxGraphicsMatrix().

set(This)
 when is_record(This, wx_ref) ->
  set(This, []).

-doc "Sets the matrix to the respective values (default values are the identity matrix).".
-spec set(This, [Option]) -> 'ok' when
	This::wxGraphicsMatrix(),
	Option :: {'a', number()}
		 | {'b', number()}
		 | {'c', number()}
		 | {'d', number()}
		 | {'tx', number()}
		 | {'ty', number()}.
set(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  MOpts = fun({a, _a} = Arg) -> Arg;
          ({b, _b} = Arg) -> Arg;
          ({c, _c} = Arg) -> Arg;
          ({d, _d} = Arg) -> Arg;
          ({tx, _tx} = Arg) -> Arg;
          ({ty, _ty} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxGraphicsMatrix_Set).

-doc "Applies this matrix to a point.".
-spec transformPoint(This) -> {X::number(), Y::number()} when
	This::wxGraphicsMatrix().
transformPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_TransformPoint),
  wxe_util:rec(?wxGraphicsMatrix_TransformPoint).

-doc """
Applies this matrix to a distance (ie.

performs all transforms except translations).
""".
-spec transformDistance(This) -> {Dx::number(), Dy::number()} when
	This::wxGraphicsMatrix().
transformDistance(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_TransformDistance),
  wxe_util:rec(?wxGraphicsMatrix_TransformDistance).

 %% From wxGraphicsObject
-doc false.
isNull(This) -> wxGraphicsObject:isNull(This).
-doc false.
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
