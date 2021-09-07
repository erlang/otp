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

-module(wxGraphicsMatrix).
-include("wxe.hrl").
-export([concat/2,get/1,invert/1,isEqual/2,isIdentity/1,rotate/2,scale/3,set/1,
  set/2,transformDistance/1,transformPoint/1,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-type wxGraphicsMatrix() :: wx:wx_object().
-export_type([wxGraphicsMatrix/0]).
%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixconcat">external documentation</a>.
-spec concat(This, T) -> 'ok' when
	This::wxGraphicsMatrix(), T::wxGraphicsMatrix().
concat(#wx_ref{type=ThisT}=This,#wx_ref{type=TT}=T) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,T,?get_env(),?wxGraphicsMatrix_Concat).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixget">external documentation</a>.
-spec get(This) -> Result when
	Result ::{A::number(), B::number(), C::number(), D::number(), Tx::number(), Ty::number()},
	This::wxGraphicsMatrix().
get(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_Get),
  wxe_util:rec(?wxGraphicsMatrix_Get).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixinvert">external documentation</a>.
-spec invert(This) -> 'ok' when
	This::wxGraphicsMatrix().
invert(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_Invert).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixisequal">external documentation</a>.
-spec isEqual(This, T) -> boolean() when
	This::wxGraphicsMatrix(), T::wxGraphicsMatrix().
isEqual(#wx_ref{type=ThisT}=This,#wx_ref{type=TT}=T) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,T,?get_env(),?wxGraphicsMatrix_IsEqual),
  wxe_util:rec(?wxGraphicsMatrix_IsEqual).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixisidentity">external documentation</a>.
-spec isIdentity(This) -> boolean() when
	This::wxGraphicsMatrix().
isIdentity(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_IsIdentity),
  wxe_util:rec(?wxGraphicsMatrix_IsIdentity).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixrotate">external documentation</a>.
-spec rotate(This, Angle) -> 'ok' when
	This::wxGraphicsMatrix(), Angle::number().
rotate(#wx_ref{type=ThisT}=This,Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Angle,?get_env(),?wxGraphicsMatrix_Rotate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixscale">external documentation</a>.
-spec scale(This, XScale, YScale) -> 'ok' when
	This::wxGraphicsMatrix(), XScale::number(), YScale::number().
scale(#wx_ref{type=ThisT}=This,XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,XScale,YScale,?get_env(),?wxGraphicsMatrix_Scale).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtranslate">external documentation</a>.
-spec translate(This, Dx, Dy) -> 'ok' when
	This::wxGraphicsMatrix(), Dx::number(), Dy::number().
translate(#wx_ref{type=ThisT}=This,Dx,Dy)
 when is_number(Dx),is_number(Dy) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,Dx,Dy,?get_env(),?wxGraphicsMatrix_Translate).

%% @equiv set(This, [])
-spec set(This) -> 'ok' when
	This::wxGraphicsMatrix().

set(This)
 when is_record(This, wx_ref) ->
  set(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixset">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtransformpoint">external documentation</a>.
-spec transformPoint(This) -> {X::number(), Y::number()} when
	This::wxGraphicsMatrix().
transformPoint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_TransformPoint),
  wxe_util:rec(?wxGraphicsMatrix_TransformPoint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtransformdistance">external documentation</a>.
-spec transformDistance(This) -> {Dx::number(), Dy::number()} when
	This::wxGraphicsMatrix().
transformDistance(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsMatrix_TransformDistance),
  wxe_util:rec(?wxGraphicsMatrix_TransformDistance).

 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
