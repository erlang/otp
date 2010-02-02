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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsobject.html">wxGraphicsObject</a>.
%% @type wxGraphicsObject().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxGraphicsObject).
-include("wxe.hrl").
-export([getRenderer/1,isNull/1]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxGraphicsObject()) -> wxGraphicsRenderer:wxGraphicsRenderer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsobject.html#wxgraphicsobjectgetrenderer">external documentation</a>.
getRenderer(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsObject),
  wxe_util:call(?wxGraphicsObject_GetRenderer,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxGraphicsObject()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxgraphicsobject.html#wxgraphicsobjectisnull">external documentation</a>.
isNull(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxGraphicsObject),
  wxe_util:call(?wxGraphicsObject_IsNull,
  <<ThisRef:32/?UI>>).

