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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html">wxGraphicsMatrix</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxGraphicsObject}
%% </p>
%% @type wxGraphicsMatrix().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsMatrix).
-include("wxe.hrl").
-export([concat/2,get/1,getNativeMatrix/1,invert/1,isEqual/2,isIdentity/1,rotate/2,
  scale/3,set/1,set/2,transformDistance/1,transformPoint/1,translate/3]).

%% inherited exports
-export([getRenderer/1,isNull/1,parent_class/1]).

%% @hidden
parent_class(wxGraphicsObject) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxGraphicsMatrix(), T::wxGraphicsMatrix()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixconcat">external documentation</a>.
concat(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=TT,ref=TRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Concat,
  <<ThisRef:32/?UI,TRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix()) -> {A::float(),B::float(),C::float(),D::float(),Tx::float(),Ty::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixget">external documentation</a>.
get(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_Get,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixgetnativematrix">external documentation</a>.
getNativeMatrix(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_GetNativeMatrix,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixinvert">external documentation</a>.
invert(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Invert,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix(), T::wxGraphicsMatrix()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixisequal">external documentation</a>.
isEqual(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=TT,ref=TRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  ?CLASS(TT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_IsEqual,
  <<ThisRef:32/?UI,TRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixisidentity">external documentation</a>.
isIdentity(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_IsIdentity,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix(), Angle::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixrotate">external documentation</a>.
rotate(#wx_ref{type=ThisT,ref=ThisRef},Angle)
 when is_float(Angle) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Rotate,
  <<ThisRef:32/?UI,0:32,Angle:64/?F>>).

%% @spec (This::wxGraphicsMatrix(), XScale::float(), YScale::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixscale">external documentation</a>.
scale(#wx_ref{type=ThisT,ref=ThisRef},XScale,YScale)
 when is_float(XScale),is_float(YScale) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Scale,
  <<ThisRef:32/?UI,0:32,XScale:64/?F,YScale:64/?F>>).

%% @spec (This::wxGraphicsMatrix(), Dx::float(), Dy::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtranslate">external documentation</a>.
translate(#wx_ref{type=ThisT,ref=ThisRef},Dx,Dy)
 when is_float(Dx),is_float(Dy) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:cast(?wxGraphicsMatrix_Translate,
  <<ThisRef:32/?UI,0:32,Dx:64/?F,Dy:64/?F>>).

%% @spec (This::wxGraphicsMatrix()) -> ok
%% @equiv set(This, [])
set(This)
 when is_record(This, wx_ref) ->
  set(This, []).

%% @spec (This::wxGraphicsMatrix(), [Option]) -> ok
%% Option = {a, float()} | {b, float()} | {c, float()} | {d, float()} | {tx, float()} | {ty, float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixset">external documentation</a>.
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

%% @spec (This::wxGraphicsMatrix()) -> {X::float(),Y::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtransformpoint">external documentation</a>.
transformPoint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_TransformPoint,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsMatrix()) -> {Dx::float(),Dy::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsmatrix.html#wxgraphicsmatrixtransformdistance">external documentation</a>.
transformDistance(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsMatrix),
  wxe_util:call(?wxGraphicsMatrix_TransformDistance,
  <<ThisRef:32/?UI>>).

 %% From wxGraphicsObject
%% @hidden
isNull(This) -> wxGraphicsObject:isNull(This).
%% @hidden
getRenderer(This) -> wxGraphicsObject:getRenderer(This).
