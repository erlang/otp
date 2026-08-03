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

-module(wxAcceleratorEntry).
-moduledoc """
An object used by an application wishing to create an accelerator table (see `m:wxAcceleratorTable`).

See:
* `m:wxAcceleratorTable`

* `wxWindow:setAcceleratorTable/2`

wxWidgets docs: [wxAcceleratorEntry](https://docs.wxwidgets.org/3.2/classwx_accelerator_entry.html)
""".
-include("wxe.hrl").
-export([destroy/1,getCommand/1,getFlags/1,getKeyCode/1,new/0,new/1,set/4,set/5]).

%% inherited exports
-export([parent_class/1]).

-type wxAcceleratorEntry() :: wx:wx_object().
-export_type([wxAcceleratorEntry/0]).
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc(#{equiv => new([])}).
-spec new() -> wxAcceleratorEntry().

new() ->
  new([]).

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

-doc "Returns the command identifier for the accelerator table entry.".
-spec getCommand(This) -> integer() when
	This::wxAcceleratorEntry().
getCommand(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorEntry_GetCommand),
  wxe_util:rec(?wxAcceleratorEntry_GetCommand).

-doc "Returns the flags for the accelerator table entry.".
-spec getFlags(This) -> integer() when
	This::wxAcceleratorEntry().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorEntry_GetFlags),
  wxe_util:rec(?wxAcceleratorEntry_GetFlags).

-doc "Returns the keycode for the accelerator table entry.".
-spec getKeyCode(This) -> integer() when
	This::wxAcceleratorEntry().
getKeyCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAcceleratorEntry),
  wxe_util:queue_cmd(This,?get_env(),?wxAcceleratorEntry_GetKeyCode),
  wxe_util:rec(?wxAcceleratorEntry_GetKeyCode).

-doc(#{equiv => set(This,Flags,KeyCode,Cmd, [])}).
-spec set(This, Flags, KeyCode, Cmd) -> 'ok' when
	This::wxAcceleratorEntry(), Flags::integer(), KeyCode::integer(), Cmd::integer().

set(This,Flags,KeyCode,Cmd)
 when is_record(This, wx_ref),is_integer(Flags),is_integer(KeyCode),is_integer(Cmd) ->
  set(This,Flags,KeyCode,Cmd, []).

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

-doc "Destroys the object".
-spec destroy(This::wxAcceleratorEntry()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAcceleratorEntry),
  wxe_util:queue_cmd(Obj, ?get_env(), ?wxAcceleratorEntry_destroy),
  ok.
