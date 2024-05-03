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

-module(wxOverlay).
-moduledoc """
Functions for wxOverlay class

Creates an overlay over an existing window, allowing for manipulations like
rubberbanding, etc. On wxOSX the overlay is implemented with native platform
APIs, on the other platforms it is simulated using `m:wxMemoryDC`.

See: `m:wxDCOverlay`, `m:wxDC`

wxWidgets docs: [wxOverlay](https://docs.wxwidgets.org/3.1/classwx_overlay.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/0,reset/1]).

%% inherited exports
-export([parent_class/1]).

-type wxOverlay() :: wx:wx_object().
-export_type([wxOverlay/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxoverlay.html#wxoverlaywxoverlay">external documentation</a>.
-spec new() -> wxOverlay().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxOverlay_new),
  wxe_util:rec(?wxOverlay_new).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxoverlay.html#wxoverlayreset">external documentation</a>.
-doc """
Clears the overlay without restoring the former state.

To be done, for example, when the window content has been changed and repainted.
""".
-spec reset(This) -> 'ok' when
	This::wxOverlay().
reset(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxOverlay),
  wxe_util:queue_cmd(This,?get_env(),?wxOverlay_Reset).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxOverlay()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxOverlay),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxOverlay_destruct),
  ok.
