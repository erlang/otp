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

-module(wxCloseEvent).
-moduledoc """
This event class contains information about window and session close events.

The handler function for EVT_CLOSE is called when the user has tried to close a a frame
or dialog box using the window manager (X) or system menu (Windows). It can also be
invoked by the application itself programmatically, for example by calling the `wxWindow:close/2` function.

You should check whether the application is forcing the deletion of the window using `canVeto/1`. If
this is false, you `must` destroy the window using `wxWindow:'Destroy'/1`.

If the return value is true, it is up to you whether you respond by destroying the window.

If you don't destroy the window, you should call `veto/2` to let the calling code know that you
did not destroy the window. This allows the `wxWindow:close/2` function to return true or false depending on
whether the close instruction was honoured or not.

Example of a `m:wxCloseEvent` handler:

The EVT_END_SESSION event is slightly different as it is sent by the system when the user
session is ending (e.g. because of log out or shutdown) and so all windows are being
forcefully closed. At least under MSW, after the handler for this event is executed the
program is simply killed by the system. Because of this, the default handler for this
event provided by wxWidgets calls all the usual cleanup code (including `wxApp::OnExit()`
(not implemented in wx)) so that it could still be executed and exit()s the process
itself, without waiting for being killed. If this behaviour is for some reason
undesirable, make sure that you define a handler for this event in your wxApp-derived
class and do not call `event.Skip()` in it (but be aware that the system will still kill
your application).

See:
* `wxWindow:close/2`

* [Overview windowdeletion](https://docs.wxwidgets.org/3.2/overview_windowdeletion.html#overview_windowdeletion)

This class is derived, and can use functions, from:

* `m:wxEvent`

wxWidgets docs: [wxCloseEvent](https://docs.wxwidgets.org/3.2/classwx_close_event.html)

## Events

Use `wxEvtHandler:connect/3` with `wxCloseEventType` to subscribe to events of this type.
""".
-include("wxe.hrl").
-export([canVeto/1,getLoggingOff/1,setCanVeto/2,setLoggingOff/2,veto/1,veto/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-type wxCloseEvent() :: wx:wx_object().
-include("wx.hrl").
-type wxCloseEventType() :: 'close_window' | 'end_session' | 'query_end_session'.
-export_type([wxCloseEvent/0, wxClose/0, wxCloseEventType/0]).
-doc false.
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc """
Returns true if you can veto a system shutdown or a window close event.

Vetoing a window close event is not possible if the calling code wishes to force the
application to exit, and so this function must be called to check this.
""".
-spec canVeto(This) -> boolean() when
	This::wxCloseEvent().
canVeto(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCloseEvent_CanVeto),
  wxe_util:rec(?wxCloseEvent_CanVeto).

-doc """
Returns true if the user is just logging off or false if the system is shutting down.

This method can only be called for end session and query end session events, it doesn't
make sense for close window event.
""".
-spec getLoggingOff(This) -> boolean() when
	This::wxCloseEvent().
getLoggingOff(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,?get_env(),?wxCloseEvent_GetLoggingOff),
  wxe_util:rec(?wxCloseEvent_GetLoggingOff).

-doc "Sets the 'can veto' flag.".
-spec setCanVeto(This, CanVeto) -> 'ok' when
	This::wxCloseEvent(), CanVeto::boolean().
setCanVeto(#wx_ref{type=ThisT}=This,CanVeto)
 when is_boolean(CanVeto) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,CanVeto,?get_env(),?wxCloseEvent_SetCanVeto).

-doc "Sets the 'logging off' flag.".
-spec setLoggingOff(This, LoggingOff) -> 'ok' when
	This::wxCloseEvent(), LoggingOff::boolean().
setLoggingOff(#wx_ref{type=ThisT}=This,LoggingOff)
 when is_boolean(LoggingOff) ->
  ?CLASS(ThisT,wxCloseEvent),
  wxe_util:queue_cmd(This,LoggingOff,?get_env(),?wxCloseEvent_SetLoggingOff).

-doc(#{equiv => veto(This, [])}).
-spec veto(This) -> 'ok' when
	This::wxCloseEvent().

veto(This)
 when is_record(This, wx_ref) ->
  veto(This, []).

-doc """
Call this from your event handler to veto a system shutdown or to signal to the calling
application that a window close did not happen.

You can only veto a shutdown if `canVeto/1` returns true.
""".
-spec veto(This, [Option]) -> 'ok' when
	This::wxCloseEvent(),
	Option :: {'veto', boolean()}.
veto(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCloseEvent),
  MOpts = fun({veto, _veto} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxCloseEvent_Veto).

 %% From wxEvent
-doc false.
stopPropagation(This) -> wxEvent:stopPropagation(This).
-doc false.
skip(This, Options) -> wxEvent:skip(This, Options).
-doc false.
skip(This) -> wxEvent:skip(This).
-doc false.
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
-doc false.
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
-doc false.
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
-doc false.
getTimestamp(This) -> wxEvent:getTimestamp(This).
-doc false.
getSkipped(This) -> wxEvent:getSkipped(This).
-doc false.
getId(This) -> wxEvent:getId(This).
