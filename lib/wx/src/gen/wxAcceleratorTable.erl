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

-module(wxAcceleratorTable).
-moduledoc """
Functions for wxAcceleratorTable class

An accelerator table allows the application to specify a table of keyboard
shortcuts for menu or button commands.

The object ?wxNullAcceleratorTable is defined to be a table with no data, and is
the initial accelerator table for a window.

Example:

Remark: An accelerator takes precedence over normal processing and can be a
convenient way to program some event handling. For example, you can use an
accelerator table to enable a dialog with a multi-line text control to accept
CTRL-Enter as meaning 'OK'.

Predefined objects (include wx.hrl): ?wxNullAcceleratorTable

See: `m:wxAcceleratorEntry`, `wxWindow:setAcceleratorTable/2`

wxWidgets docs:
[wxAcceleratorTable](https://docs.wxwidgets.org/3.1/classwx_accelerator_table.html)
""".
-include("wxe.hrl").
-export([destroy/1,isOk/1,new/0,new/2,ok/1]).

%% inherited exports
-export([parent_class/1]).

-type wxAcceleratorTable() :: wx:wx_object().
-export_type([wxAcceleratorTable/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
-doc "Default ctor.".
-spec new() -> wxAcceleratorTable().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAcceleratorTable_new_0),
  wxe_util:rec(?wxAcceleratorTable_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
-doc "Initializes the accelerator table from an array of `m:wxAcceleratorEntry`.".
-spec new(N, Entries) -> wxAcceleratorTable() when
	N::integer(), Entries::[wxAcceleratorEntry:wxAcceleratorEntry()].
new(N,Entries)
 when is_integer(N),is_list(Entries) ->
 _ = [?CLASS(EntriesT,wxAcceleratorEntry) || #wx_ref{type=EntriesT} <- Entries],
  wxe_util:queue_cmd(N,Entries,?get_env(),?wxAcceleratorTable_new_2),
  wxe_util:rec(?wxAcceleratorTable_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortableisok">external documentation</a>.
-doc "See: `isOk/1`.".
-spec ok(This) -> boolean() when
	This::wxAcceleratorTable().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortableisok">external documentation</a>.
-doc "Returns true if the accelerator table is valid.".
-spec isOk(This) -> boolean() when
	This::wxAcceleratorTable().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorTable),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorTable_IsOk),
  wxe_util:rec(?wxAcceleratorTable_IsOk).

%% @doc Destroys this object, do not use object again
-doc """
Destroys the `m:wxAcceleratorTable` object.

See overview_refcount_destruct for more info.
""".
-spec destroy(This::wxAcceleratorTable()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorTable),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
