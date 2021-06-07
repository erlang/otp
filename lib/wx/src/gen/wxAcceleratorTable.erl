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

-module(wxAcceleratorTable).
-include("wxe.hrl").
-export([destroy/1,isOk/1,new/0,new/2,ok/1]).

%% inherited exports
-export([parent_class/1]).

-type wxAcceleratorTable() :: wx:wx_object().
-export_type([wxAcceleratorTable/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
-spec new() -> wxAcceleratorTable().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxAcceleratorTable_new_0),
  wxe_util:rec(?wxAcceleratorTable_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
-spec new(N, Entries) -> wxAcceleratorTable() when
	N::integer(), Entries::[wxAcceleratorEntry:wxAcceleratorEntry()].
new(N,Entries)
 when is_integer(N),is_list(Entries) ->
 _ = [?CLASS(EntriesT,wxAcceleratorEntry) || #wx_ref{type=EntriesT} <- Entries],
  wxe_util:queue_cmd(N,Entries,?get_env(),?wxAcceleratorTable_new_2),
  wxe_util:rec(?wxAcceleratorTable_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortableisok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxAcceleratorTable().

ok(This)
 when is_record(This, wx_ref) ->
  isOk(This).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortableisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxAcceleratorTable().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorTable),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorTable_IsOk),
  wxe_util:rec(?wxAcceleratorTable_IsOk).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAcceleratorTable()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorTable),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
