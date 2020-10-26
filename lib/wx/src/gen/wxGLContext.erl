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

-module(wxGLContext).
-include("wxe.hrl").
-export([destroy/1,new/1,new/2,setCurrent/2]).

%% inherited exports
-export([parent_class/1]).

-type wxGLContext() :: wx:wx_object().
-export_type([wxGLContext/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Win, [])
-spec new(Win) -> wxGLContext() when
	Win::wxGLCanvas:wxGLCanvas().

new(Win)
 when is_record(Win, wx_ref) ->
  new(Win, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxglcontext.html#wxglcontextwxglcontext">external documentation</a>.
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
-spec setCurrent(This, Win) -> boolean() when
	This::wxGLContext(), Win::wxGLCanvas:wxGLCanvas().
setCurrent(#wx_ref{type=ThisT}=This,#wx_ref{type=WinT}=Win) ->
  ?CLASS(ThisT,wxGLContext),
  ?CLASS(WinT,wxGLCanvas),
  wxe_util:queue_cmd(This,Win,?get_env(),?wxGLContext_SetCurrent),
  wxe_util:rec(?wxGLContext_SetCurrent).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxGLContext()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxGLContext),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
