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

-module(wxAcceleratorEntry).
-moduledoc """
Functions for wxAcceleratorEntry class

An object used by an application wishing to create an accelerator table (see
`m:wxAcceleratorTable`).

See: `m:wxAcceleratorTable`, `wxWindow:setAcceleratorTable/2`

wxWidgets docs:
[wxAcceleratorEntry](https://docs.wxwidgets.org/3.1/classwx_accelerator_entry.html)
""".
-include("wxe.hrl").
-export([destroy/1,getCommand/1,getFlags/1,getKeyCode/1,new/0,new/1,set/4,set/5]).

%% inherited exports
-export([parent_class/1]).

-type wxAcceleratorEntry() :: wx:wx_object().
-export_type([wxAcceleratorEntry/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxAcceleratorEntry().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrywxacceleratorentry">external documentation</a>.
%% <br /> Also:<br />
%% new(Entry) -> wxAcceleratorEntry() when<br />
%% 	Entry::wxAcceleratorEntry().<br />
%% 
-doc "Copy ctor.".
-spec new([Option]) -> wxAcceleratorEntry() when
	Option :: {'flags', integer()}
		 | {'keyCode', integer()}
		 | {'cmd', integer()}
		 | {'item', wxMenuItem:wxMenuItem()};
      (Entry) -> wxAcceleratorEntry() when
	Entry::wxAcceleratorEntry().
new(Options)
 when is_list(Options) ->
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          ({keyCode, _keyCode} = Arg) -> Arg;
          ({cmd, _cmd} = Arg) -> Arg;
          ({item, #wx_ref{type=ItemT}} = Arg) ->   ?CLASS(ItemT,wxMenuItem),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxAcceleratorEntry_new_1_0),
  wxe_util:rec(?wxAcceleratorEntry_new_1_0);
new(#wx_ref{type=EntryT}=Entry) ->
  ?CLASS(EntryT,wxAcceleratorEntry),
  wxe_util:queue_cmd(Entry,?get_env(),?wxAcceleratorEntry_new_1_1),
  wxe_util:rec(?wxAcceleratorEntry_new_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrygetcommand">external documentation</a>.
-doc "Returns the command identifier for the accelerator table entry.".
-spec getCommand(This) -> integer() when
	This::wxAcceleratorEntry().
getCommand(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorEntry_GetCommand),
  wxe_util:rec(?wxAcceleratorEntry_GetCommand).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrygetflags">external documentation</a>.
-doc "Returns the flags for the accelerator table entry.".
-spec getFlags(This) -> integer() when
	This::wxAcceleratorEntry().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorEntry_GetFlags),
  wxe_util:rec(?wxAcceleratorEntry_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentrygetkeycode">external documentation</a>.
-doc "Returns the keycode for the accelerator table entry.".
-spec getKeyCode(This) -> integer() when
	This::wxAcceleratorEntry().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorEntry_GetKeyCode),
  wxe_util:rec(?wxAcceleratorEntry_GetKeyCode).

%% @equiv set(This,Flags,KeyCode,Cmd, [])
-spec set(This, Flags, KeyCode, Cmd) -> 'ok' when
	This::wxAcceleratorEntry(), Flags::integer(), KeyCode::integer(), Cmd::integer().

set(This,Flags,KeyCode,Cmd)
 when is_record(This, wx_ref),is_integer(Flags),is_integer(KeyCode),is_integer(Cmd) ->
  set(This,Flags,KeyCode,Cmd, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxacceleratorentry.html#wxacceleratorentryset">external documentation</a>.
-doc "Sets the accelerator entry parameters.".
-spec set(This, Flags, KeyCode, Cmd, [Option]) -> 'ok' when
	This::wxAcceleratorEntry(), Flags::integer(), KeyCode::integer(), Cmd::integer(),
	Option :: {'item', wxMenuItem:wxMenuItem()}.
set(#wx_ref{type=ThisT}=This,Flags,KeyCode,Cmd, Options)
 when is_integer(Flags),is_integer(KeyCode),is_integer(Cmd),is_list(Options) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  MOpts = fun({item, #wx_ref{type=ItemT}} = Arg) ->   ?CLASS(ItemT,wxMenuItem),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Flags,KeyCode,Cmd, Opts,?get_env(),?wxAcceleratorEntry_Set).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxAcceleratorEntry()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorEntry),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxAcceleratorEntry_destroy),
  ok.
