%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html">wxAcceleratorTable</a>.
%% @type wxAcceleratorTable().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAcceleratorTable).
-include("wxe.hrl").
-export([destroy/1,new/0,new/2,ok/1]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxAcceleratorTable/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAcceleratorTable() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
-spec new() -> wxAcceleratorTable().
new() ->
  wxe_util:construct(?wxAcceleratorTable_new_0,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
-spec new(N, Entries) -> wxAcceleratorTable() when
	N::integer(), Entries::[wxAcceleratorEntry:wxAcceleratorEntry()].
new(N,Entries)
 when is_integer(N),is_list(Entries) ->
 _ = [?CLASS(EntriesT,wxAcceleratorEntry) || #wx_ref{type=EntriesT} <- Entries],
  wxe_util:construct(?wxAcceleratorTable_new_2,
  <<N:32/?UI,(length(Entries)):32/?UI,
     (<< <<(C#wx_ref.ref):32/?UI>> || C <- Entries>>)/binary, 0:(((0+length(Entries)) rem 2)*32)>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratortable.html#wxacceleratortableok">external documentation</a>.
-spec ok(This) -> boolean() when
	This::wxAcceleratorTable().
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAcceleratorTable),
  wxe_util:call(?wxAcceleratorTable_Ok,
  <<ThisRef:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAcceleratorTable()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorTable),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
