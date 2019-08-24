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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html">wxAcceleratorEntry</a>.
%% @type wxAcceleratorEntry().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAcceleratorEntry).
-include("wxe.hrl").
-export([destroy/1,getCommand/1,getFlags/1,getKeyCode/1,new/0,new/1,set/4,set/5]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxAcceleratorEntry/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAcceleratorEntry() :: wx:wx_object().
%% @equiv new([])
-spec new() -> wxAcceleratorEntry().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrywxacceleratorentry">external documentation</a>.
%% <br /> Also:<br />
%% new(Entry) -> wxAcceleratorEntry() when<br />
%% 	Entry::wxAcceleratorEntry().<br />
%% 
-spec new([Option]) -> wxAcceleratorEntry() when
	Option :: {'flags', integer()}
		 | {'keyCode', integer()}
		 | {'cmd', integer()}
		 | {'item', wxMenuItem:wxMenuItem()};
      (Entry) -> wxAcceleratorEntry() when
	Entry::wxAcceleratorEntry().
new(Options)
 when is_list(Options) ->
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          ({keyCode, KeyCode}, Acc) -> [<<2:32/?UI,KeyCode:32/?UI>>|Acc];
          ({cmd, Cmd}, Acc) -> [<<3:32/?UI,Cmd:32/?UI>>|Acc];
          ({item, #wx_ref{type=ItemT,ref=ItemRef}}, Acc) ->   ?CLASS(ItemT,wxMenuItem),[<<4:32/?UI,ItemRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxAcceleratorEntry_new_1_0,
  <<BinOpt/binary>>);
new(#wx_ref{type=EntryT,ref=EntryRef}) ->
  ?CLASS(EntryT,wxAcceleratorEntry),
  wxe_util:construct(?wxAcceleratorEntry_new_1_1,
  <<EntryRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrygetcommand">external documentation</a>.
-spec getCommand(This) -> integer() when
	This::wxAcceleratorEntry().
getCommand(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:call(?wxAcceleratorEntry_GetCommand,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrygetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxAcceleratorEntry().
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:call(?wxAcceleratorEntry_GetFlags,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrygetkeycode">external documentation</a>.
-spec getKeyCode(This) -> integer() when
	This::wxAcceleratorEntry().
getKeyCode(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:call(?wxAcceleratorEntry_GetKeyCode,
  <<ThisRef:32/?UI>>).

%% @equiv set(This,Flags,KeyCode,Cmd, [])
-spec set(This, Flags, KeyCode, Cmd) -> 'ok' when
	This::wxAcceleratorEntry(), Flags::integer(), KeyCode::integer(), Cmd::integer().

set(This,Flags,KeyCode,Cmd)
 when is_record(This, wx_ref),is_integer(Flags),is_integer(KeyCode),is_integer(Cmd) ->
  set(This,Flags,KeyCode,Cmd, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentryset">external documentation</a>.
-spec set(This, Flags, KeyCode, Cmd, [Option]) -> 'ok' when
	This::wxAcceleratorEntry(), Flags::integer(), KeyCode::integer(), Cmd::integer(),
	Option :: {'item', wxMenuItem:wxMenuItem()}.
set(#wx_ref{type=ThisT,ref=ThisRef},Flags,KeyCode,Cmd, Options)
 when is_integer(Flags),is_integer(KeyCode),is_integer(Cmd),is_list(Options) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  MOpts = fun({item, #wx_ref{type=ItemT,ref=ItemRef}}, Acc) ->   ?CLASS(ItemT,wxMenuItem),[<<1:32/?UI,ItemRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxAcceleratorEntry_Set,
  <<ThisRef:32/?UI,Flags:32/?UI,KeyCode:32/?UI,Cmd:32/?UI, BinOpt/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAcceleratorEntry()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorEntry),
  wxe_util:destroy(?wxAcceleratorEntry_destroy,Obj),
  ok.
