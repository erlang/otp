%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxGLContext).
-moduledoc """
An instance of a `m:wxGLContext` represents the state of an OpenGL state machine and the
connection between OpenGL and the system.

The OpenGL state includes everything that can be set with the OpenGL API: colors,
rendering variables, buffer data ids, texture objects, etc. It is possible to have
multiple rendering contexts share buffer data and textures. This feature is specially
useful when the application use multiple threads for updating data into the memory of the
graphics card.

Whether one only rendering context is used with or bound to multiple output windows or if
each window has its own bound context is a developer decision. It is important to take
into account that GPU makers may set different pointers to the same OGL function for
different contexts. The way these pointers are retrieved from the OGL driver should be
used again for each new context.

Binding (making current) a rendering context with another instance of a `m:wxGLCanvas`
however works only if the both `m:wxGLCanvas` instances were created with the same attributes.

OpenGL version 3 introduced a new type of specification profile, the modern core profile.
The old compatibility profile maintains all legacy features. Since wxWidgets 3.1.0 you can
choose the type of context and even ask for a specified OGL version number. However, its
advised to use only core profile as the compatibility profile may run a bit slower.

OpenGL core profile specification defines several flags at context creation that
determine not only the type of context but also some features. Some of these flags can be
set in the list of attributes used at `m:wxGLCanvas` ctor. But since wxWidgets 3.1.0 it is
strongly encouraged to use the new mechanism: setting the context attributes with a `wxGLContextAttrs`
(not implemented in wx) object and the canvas attributes with a `wxGLAttributes` (not
implemented in wx) object.

The best way of knowing if your OpenGL environment supports a specific type of context is
creating a `m:wxGLContext` instance and checking `isOK/1`. If it returns false, then simply delete
that instance and create a new one with other attributes.

wxHAS_OPENGL_ES is defined on platforms that only have this implementation available
(e.g. the iPhone) and don't support the full specification.

See: `m:wxGLCanvas`

wxWidgets docs: [wxGLContext](https://docs.wxwidgets.org/3.2/classwx_g_l_context.html)
""".
-include("wxe.hrl").
-export([destroy/1,isOK/1,new/1,new/2,setCurrent/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGLContext() :: wx:wx_object().
-export_type([wxGLContext/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new(Win, [])}).
-spec new(Win) -> wxGLContext() when
	Win::wxGLCanvas:wxGLCanvas().

new(Win)
 when is_record(Win, wx_ref) ->
  new(Win, []).

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

-doc """
Makes the OpenGL state that is represented by this rendering context current with the `m:wxGLCanvas`
`win`.

Note: `win` can be a different `m:wxGLCanvas` window than the one that was passed to the
constructor of this rendering context. If `RC` is an object of type `m:wxGLContext`, the
statements `"RC.SetCurrent(win);"` and `"win.SetCurrent(RC);"` are equivalent, see `wxGLCanvas:setCurrent/2`.
""".
-spec setCurrent(This, Win) -> boolean() when
	This::wxGLContext(), Win::wxGLCanvas:wxGLCanvas().
setCurrent(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxGLContext),
  ?CLASS(WinT,wxGLCanvas),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxGLContext_SetCurrent),
  wxe_util:rec(?wxGLContext_SetCurrent).

-doc """
Checks if the underlying OpenGL rendering context was correctly created by the system
with the requested attributes.

If this function returns false then the `m:wxGLContext` object is useless and should be
deleted and recreated with different attributes.

Since: 3.1.0
""".
-spec isOK(This) -> boolean() when
	This::wxGLContext().
isOK(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxGLContext),
  wxe_util:queue_cmd(This,?get_env(),?wxGLContext_IsOK),
  wxe_util:rec(?wxGLContext_IsOK).

-doc "Destroys the object".
-spec destroy(This::wxGLContext()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGLContext),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
