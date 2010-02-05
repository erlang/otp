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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxacceleratortable.html">wxAcceleratorTable</a>.
%% @type wxAcceleratorTable().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAcceleratorTable).
-include("wxe.hrl").
-export([destroy/1,new/0,new/2,ok/1]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxAcceleratorTable()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
new() ->
  wxe_util:construct(?wxAcceleratorTable_new_0,
  <<>>).

%% @spec (N::integer(), Entries::[wxAcceleratorEntry:wxAcceleratorEntry()]) -> wxAcceleratorTable()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxacceleratortable.html#wxacceleratortablewxacceleratortable">external documentation</a>.
new(N,Entries)
 when is_integer(N),is_list(Entries) ->
  [?CLASS(EntriesT,wxAcceleratorEntry) || #wx_ref{type=EntriesT} <- Entries],
  wxe_util:construct(?wxAcceleratorTable_new_2,
  <<N:32/?UI,(length(Entries)):32/?UI,
     (<< <<(C#wx_ref.ref):32/?UI>> || C <- Entries>>)/binary, 0:(((0+length(Entries)) rem 2)*32)>>).

%% @spec (This::wxAcceleratorTable()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxacceleratortable.html#wxacceleratortableok">external documentation</a>.
ok(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAcceleratorTable),
  wxe_util:call(?wxAcceleratorTable_Ok,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAcceleratorTable()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorTable),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
