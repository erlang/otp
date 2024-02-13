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

-module(wxGLContext).
-moduledoc """
Functions for wxGLContext class

An instance of a `m:wxGLContext` represents the state of an OpenGL state machine
and the connection between OpenGL and the system.

The OpenGL state includes everything that can be set with the OpenGL API:
colors, rendering variables, buffer data ids, texture objects, etc. It is
possible to have multiple rendering contexts share buffer data and textures.
This feature is specially useful when the application use multiple threads for
updating data into the memory of the graphics card.

Whether one only rendering context is used with or bound to multiple output
windows or if each window has its own bound context is a developer decision. It
is important to take into account that GPU makers may set different pointers to
the same OGL function for different contexts. The way these pointers are
retrieved from the OGL driver should be used again for each new context.

Binding (making current) a rendering context with another instance of a
`m:wxGLCanvas` however works only if the both `m:wxGLCanvas` instances were
created with the same attributes.

OpenGL version 3 introduced a new type of specification profile, the modern core
profile. The old compatibility profile maintains all legacy features. Since
wxWidgets 3.1.0 you can choose the type of context and even ask for a specified
OGL version number. However, its advised to use only core profile as the
compatibility profile may run a bit slower.

OpenGL core profile specification defines several flags at context creation that
determine not only the type of context but also some features. Some of these
flags can be set in the list of attributes used at `m:wxGLCanvas` ctor. But
since wxWidgets 3.1.0 it is strongly encouraged to use the new mechanism:
setting the context attributes with a `wxGLContextAttrs` (not implemented in wx)
object and the canvas attributes with a `wxGLAttributes` (not implemented in wx)
object.

The best way of knowing if your OpenGL environment supports a specific type of
context is creating a `m:wxGLContext` instance and checking `isOK/1`. If it
returns false, then simply delete that instance and create a new one with other
attributes.

wxHAS_OPENGL_ES is defined on platforms that only have this implementation
available (e.g. the iPhone) and don't support the full specification.

See: `m:wxGLCanvas`, `wxGLContextAttrs` (not implemented in wx),
`wxGLAttributes` (not implemented in wx)

wxWidgets docs:
[wxGLContext](https://docs.wxwidgets.org/3.1/classwx_g_l_context.html)
""".
-include("wxe.hrl").
-export([destroy/1,isOK/1,new/1,new/2,setCurrent/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGLContext() :: wx:wx_object().
-export_type([wxGLContext/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Win, [])
-spec new(Win) -> wxGLContext() when
	Win::wxGLCanvas:wxGLCanvas().

new(Win)
 when is_record(Win, wx_ref) ->
  new(Win, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxglcontext.html#wxglcontextwxglcontext">external documentation</a>.
-doc "Constructor.".
-spec new(Win, [Option]) -> wxGLContext() when
	Win::wxGLCanvas:wxGLCanvas(),
	Option :: {'other', wxGLContext()}.
new(#wx_ref{type=WinT}=Win, Options)
 when is_list(Options) ->
  ?CLASS(WinT,wxGLCanvas),
  MOpts = fun({other, #wx_ref{type=OtherT}} = Arg) ->   ?CLASS(OtherT,wxGLContext),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Win, Opts,?get_env(),?wxGLContext_new),
  wxe_util:rec(?wxGLContext_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxglcontext.html#wxglcontextsetcurrent">external documentation</a>.
-doc """
Makes the OpenGL state that is represented by this rendering context current
with the `m:wxGLCanvas` `win`.

Note: `win` can be a different `m:wxGLCanvas` window than the one that was
passed to the constructor of this rendering context. If `RC` is an object of
type `m:wxGLContext`, the statements `"RC.SetCurrent(win);"` and
`"win.SetCurrent(RC);"` are equivalent, see `wxGLCanvas:setCurrent/2`.
""".
-spec setCurrent(This, Win) -> boolean() when
	This::wxGLContext(), Win::wxGLCanvas:wxGLCanvas().
setCurrent(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxGLContext),
  ?CLASS(WinT,wxGLCanvas),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxGLContext_SetCurrent),
  wxe_util:rec(?wxGLContext_SetCurrent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxglcontext.html#wxglcontextisok">external documentation</a>.
-doc """
Checks if the underlying OpenGL rendering context was correctly created by the
system with the requested attributes.

If this function returns false then the `m:wxGLContext` object is useless and
should be deleted and recreated with different attributes.

Since: 3.1.0
""".
-spec isOK(This) -> boolean() when
	This::wxGLContext().
isOK(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGLContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGLContext_IsOK),
  wxe_util:rec(?wxGLContext_IsOK).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxGLContext()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGLContext),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
