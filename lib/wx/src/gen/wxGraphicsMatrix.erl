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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html">wxGraphicsMatrix</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGraphicsObject}
%% </p>
%% @type wxGraphicsMatrix().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsMatrix).
-include("wxe.hrl").
-export([concat/2,get/1,invert/1,isEqual/2,isIdentity/1,rotate/2,scale/3,set/1,
  set/2,transformDistance/1,transformPoint/1,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

-export_type([wxGraphicsMatrix/0]).
%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxGraphicsMatrix() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixconcat">external documentation</a>.
-spec concat(This, T) -> 'ok' when
	This::wxGraphicsMatrix(), T::wxGraphicsMatrix().
concat(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=TT,ref=TRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Concat,
  <<ThisRef:32/?UI,TRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixget">external documentation</a>.
-spec get(This) -> Result when
	Result ::{A::number(), B::number(), C::number(), D::number(), Tx::number(), Ty::number()},
	This::wxGraphicsMatrix().
get(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_Get,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixinvert">external documentation</a>.
-spec invert(This) -> 'ok' when
	This::wxGraphicsMatrix().
invert(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Invert,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixisequal">external documentation</a>.
-spec isEqual(This, T) -> boolean() when
	This::wxGraphicsMatrix(), T::wxGraphicsMatrix().
isEqual(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=TT,ref=TRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_IsEqual,
  <<ThisRef:32/?UI,TRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixisidentity">external documentation</a>.
-spec isIdentity(This) -> boolean() when
	This::wxGraphicsMatrix().
isIdentity(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_IsIdentity,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixrotate">external documentation</a>.
-spec rotate(This, Angle) -> 'ok' when
	This::wxGraphicsMatrix(), Angle::number().
rotate(#wx_ref{type=ThisT,ref=ThisRef},Angle)
 when is_number(Angle) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Rotate,
  <<ThisRef:32/?UI,0:32,Angle:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixscale">external documentation</a>.
-spec scale(This, XScale, YScale) -> 'ok' when
	This::wxGraphicsMatrix(), XScale::number(), YScale::number().
scale(#wx_ref{type=ThisT,ref=ThisRef},XScale,YScale)
 when is_number(XScale),is_number(YScale) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Scale,
  <<ThisRef:32/?UI,0:32,XScale:64/?F,YScale:64/?F>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtranslate">external documentation</a>.
-spec translate(This, Dx, Dy) -> 'ok' when
	This::wxGraphicsMatrix(), Dx::number(), Dy::number().
translate(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy)
 when is_number(Dx),is_number(Dy) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Translate,
  <<ThisRef:32/?UI,0:32,Dx:64/?F,Dy:64/?F>>).

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
set(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  MOpts = fun({a, A}, Acc) -> [<<1:32/?UI,0:32,A:64/?F>>|Acc];
          ({b, B}, Acc) -> [<<2:32/?UI,0:32,B:64/?F>>|Acc];
          ({c, C}, Acc) -> [<<3:32/?UI,0:32,C:64/?F>>|Acc];
          ({d, D}, Acc) -> [<<4:32/?UI,0:32,D:64/?F>>|Acc];
          ({tx, Tx}, Acc) -> [<<5:32/?UI,0:32,Tx:64/?F>>|Acc];
          ({ty, Ty}, Acc) -> [<<6:32/?UI,0:32,Ty:64/?F>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxGraphicsMatrix_Set,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtransformpoint">external documentation</a>.
-spec transformPoint(This) -> {X::number(), Y::number()} when
	This::wxGraphicsMatrix().
transformPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_TransformPoint,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtransformdistance">external documentation</a>.
-spec transformDistance(This) -> {Dx::number(), Dy::number()} when
	This::wxGraphicsMatrix().
transformDistance(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_TransformDistance,
  <<ThisRef:32/?UI>>).

 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
