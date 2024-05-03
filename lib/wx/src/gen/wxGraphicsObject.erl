%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(wxGraphicsObject).
-moduledoc """
Functions for wxGraphicsObject class

This class is the superclass of native graphics objects like pens etc. It allows
reference counting. Not instantiated by user code.

See: `m:wxGraphicsBrush`, `m:wxGraphicsPen`, `m:wxGraphicsMatrix`,
`m:wxGraphicsPath`

wxWidgets docs:
[wxGraphicsObject](https://docs.wxwidgets.org/3.1/classwx_graphics_object.html)
""".
-include("wxe.hrl").
-export([destroy/1,getRenderer/1,isNull/1]).

%% inherited exports
-export([parent_class/1]).

-type wxGraphicsObject() :: wx:wx_object().
-export_type([wxGraphicsObject/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsobject.html#wxgraphicsobjectgetrenderer">external documentation</a>.
-doc """
Returns the renderer that was used to create this instance, or NULL if it has
not been initialized yet.
""".
-spec getRenderer(This) -> wxGraphicsRenderer:wxGraphicsRenderer() when
	This::wxGraphicsObject().
getRenderer(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsObject),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsObject_GetRenderer),
  wxe_util:rec(?wxGraphicsObject_GetRenderer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxgraphicsobject.html#wxgraphicsobjectisnull">external documentation</a>.
-doc "Return: false if this object is valid, otherwise returns true.".
-spec isNull(This) -> boolean() when
	This::wxGraphicsObject().
isNull(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGraphicsObject),
  wxe_util:queue_cmd(This,?get_env(),?wxGraphicsObject_IsNull),
  wxe_util:rec(?wxGraphicsObject_IsNull).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGraphicsObject()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGraphicsObject),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
