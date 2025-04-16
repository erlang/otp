%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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

-module(wxLogNull).
-moduledoc """
This class allows you to temporarily suspend logging.

All calls to the log functions during the life time of an object of this class are just ignored.

In particular, it can be used to suppress the log messages given by wxWidgets itself but
it should be noted that it is rarely the best way to cope with this problem as `all` log
messages are suppressed, even if they indicate a completely different error than the one
the programmer wanted to suppress.

For instance, the example of the overview:

would be better written as:

wxWidgets docs: [wxLogNull](https://docs.wxwidgets.org/3.2/classwx_log_null.html)
""".
-include("wxe.hrl").
-export([destroy/1,new/0]).

%% inherited exports
-export([parent_class/1]).

-type wxLogNull() :: wx:wx_object().
-export_type([wxLogNull/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Suspends logging.".
-spec new() -> wxLogNull().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxLogNull_new),
  wxe_util:rec(?wxLogNull_new).

-doc "Destroys the object".
-spec destroy(This::wxLogNull()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxLogNull),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxLogNull_destruct),
  ok.
