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

-module(wxNotificationMessage).
-moduledoc """
Functions for wxNotificationMessage class

This class allows showing the user a message non intrusively.

Currently it is implemented natively for Windows, macOS, GTK and uses generic
toast notifications under the other platforms. It's not recommended but
`wxGenericNotificationMessage` can be used instead of the native ones. This
might make sense if your application requires features not available in the
native implementation.

Notice that this class is not a window and so doesn't derive from `m:wxWindow`.

Platform Notes

Par: Up to Windows 8 balloon notifications are displayed from an icon in the
notification area of the taskbar. If your application uses a `m:wxTaskBarIcon`
you should call `useTaskBarIcon/1` to ensure that only one icon is shown in the
notification area. Windows 10 displays all notifications as popup toasts. To
suppress the additional icon in the notification area on Windows 10 and for
toast notification support on Windows 8 it is recommended to call
`mSWUseToasts/1` before showing the first notification message.

Par: The macOS implementation uses Notification Center to display native
notifications. In order to use actions your notifications must use the alert
style. This can be enabled by the user in system settings or by setting the
`NSUserNotificationAlertStyle` value in Info.plist to `alert`. Please note that
the user always has the option to change the notification style.

Since: 2.9.0

This class is derived (and can use functions) from: `m:wxEvtHandler`

wxWidgets docs:
[wxNotificationMessage](https://docs.wxwidgets.org/3.1/classwx_notification_message.html)

## Events

Event types emitted from this class:
[`notification_message_click`](`m:wxCommandEvent`),
[`notification_message_dismissed`](`m:wxCommandEvent`),
[`notification_message_action`](`m:wxCommandEvent`)
""".
-include("wxe.hrl").
-export([addAction/2,addAction/3,close/1,destroy/1,mSWUseToasts/0,mSWUseToasts/1,
  new/0,new/1,new/2,setFlags/2,setIcon/2,setMessage/2,setParent/2,setTitle/2,
  show/1,show/2,useTaskBarIcon/1]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxNotificationMessage() :: wx:wx_object().
-export_type([wxNotificationMessage/0]).
%% @hidden
-doc false.
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagewxnotificationmessage">external documentation</a>.
-doc """
Default constructor, use `setParent/2`, `setTitle/2` and `setMessage/2` to
initialize the object before showing it.
""".
-spec new() -> wxNotificationMessage().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxNotificationMessage_new_0),
  wxe_util:rec(?wxNotificationMessage_new_0).

%% @equiv new(Title, [])
-spec new(Title) -> wxNotificationMessage() when
	Title::unicode:chardata().

new(Title)
 when ?is_chardata(Title) ->
  new(Title, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagewxnotificationmessage">external documentation</a>.
-doc """
Create a notification object with the given attributes.

See `setTitle/2`, `setMessage/2`, `setParent/2` and `setFlags/2` for the
description of the corresponding parameters.
""".
-spec new(Title, [Option]) -> wxNotificationMessage() when
	Title::unicode:chardata(),
	Option :: {'message', unicode:chardata()}
		 | {'parent', wxWindow:wxWindow()}
		 | {'flags', integer()}.
new(Title, Options)
 when ?is_chardata(Title),is_list(Options) ->
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({message, Message}) ->   Message_UC = unicode:characters_to_binary(Message),{message,Message_UC};
          ({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          ({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Title_UC, Opts,?get_env(),?wxNotificationMessage_new_2),
  wxe_util:rec(?wxNotificationMessage_new_2).

%% @equiv addAction(This,Actionid, [])
-spec addAction(This, Actionid) -> boolean() when
	This::wxNotificationMessage(), Actionid::integer().

addAction(This,Actionid)
 when is_record(This, wx_ref),is_integer(Actionid) ->
  addAction(This,Actionid, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessageaddaction">external documentation</a>.
-doc """
Add an action to the notification.

If supported by the implementation this are usually buttons in the notification
selectable by the user.

Return: false if the current implementation or OS version does not support
actions in notifications.

Since: 3.1.0
""".
-spec addAction(This, Actionid, [Option]) -> boolean() when
	This::wxNotificationMessage(), Actionid::integer(),
	Option :: {'label', unicode:chardata()}.
addAction(#wx_ref{type=ThisT}=This,Actionid, Options)
 when is_integer(Actionid),is_list(Options) ->
  ?CLASS(ThisT,wxNotificationMessage),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Actionid, Opts,?get_env(),?wxNotificationMessage_AddAction),
  wxe_util:rec(?wxNotificationMessage_AddAction).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessageclose">external documentation</a>.
-doc """
Hides the notification.

Returns true if it was hidden or false if it couldn't be done (e.g. on some
systems automatically hidden notifications can't be hidden manually).
""".
-spec close(This) -> boolean() when
	This::wxNotificationMessage().
close(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotificationMessage),
  wxe_util:queue_cmd(This,?get_env(),?wxNotificationMessage_Close),
  wxe_util:rec(?wxNotificationMessage_Close).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesetflags">external documentation</a>.
-doc """
This parameter can be currently used to specify the icon to show in the
notification.

Valid values are `wxICON_INFORMATION`, `wxICON_WARNING` and `wxICON_ERROR`
(notice that `wxICON_QUESTION` is not allowed here). Some implementations of
this class may not support the icons.

See: `setIcon/2`
""".
-spec setFlags(This, Flags) -> 'ok' when
	This::wxNotificationMessage(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxNotificationMessage),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxNotificationMessage_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessageseticon">external documentation</a>.
-doc """
Specify a custom icon to be displayed in the notification.

Some implementations of this class may not support custom icons.

See: `setFlags/2`

Since: 3.1.0
""".
-spec setIcon(This, Icon) -> 'ok' when
	This::wxNotificationMessage(), Icon::wxIcon:wxIcon().
setIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxNotificationMessage),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,?get_env(),?wxNotificationMessage_SetIcon).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesetmessage">external documentation</a>.
-doc """
Set the main text of the notification.

This should be a more detailed description than the title but still limited to
reasonable length (not more than 256 characters).
""".
-spec setMessage(This, Message) -> 'ok' when
	This::wxNotificationMessage(), Message::unicode:chardata().
setMessage(#wx_ref{type=ThisT}=This,Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxNotificationMessage),
  Message_UC = unicode:characters_to_binary(Message),
  wxe_util:queue_cmd(This,Message_UC,?get_env(),?wxNotificationMessage_SetMessage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesetparent">external documentation</a>.
-doc """
Set the parent for this notification: the notification will be associated with
the top level parent of this window or, if this method is not called, with the
main application window by default.
""".
-spec setParent(This, Parent) -> 'ok' when
	This::wxNotificationMessage(), Parent::wxWindow:wxWindow().
setParent(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent) ->
  ?CLASS(ThisT,wxNotificationMessage),
  ?CLASS(ParentT,wxWindow),
  wxe_util:queue_cmd(This,Parent,?get_env(),?wxNotificationMessage_SetParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesettitle">external documentation</a>.
-doc """
Set the title, it must be a concise string (not more than 64 characters), use
`setMessage/2` to give the user more details.
""".
-spec setTitle(This, Title) -> 'ok' when
	This::wxNotificationMessage(), Title::unicode:chardata().
setTitle(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxNotificationMessage),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxNotificationMessage_SetTitle).

%% @equiv show(This, [])
-spec show(This) -> boolean() when
	This::wxNotificationMessage().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessageshow">external documentation</a>.
-doc """
Show the notification to the user and hides it after `timeout` seconds are
elapsed.

Special values `Timeout_Auto` and `Timeout_Never` can be used here, notice that
you shouldn't rely on `timeout` being exactly respected because the current
platform may only support default timeout value and also because the user may be
able to close the notification.

Note: When using native notifications in wxGTK, the timeout is ignored for the
notifications with `wxICON_WARNING` or `wxICON_ERROR` flags, they always remain
shown unless they're explicitly hidden by the user, i.e. behave as if
Timeout_Auto were given.

Return: false if an error occurred.
""".
-spec show(This, [Option]) -> boolean() when
	This::wxNotificationMessage(),
	Option :: {'timeout', integer()}.
show(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxNotificationMessage),
  MOpts = fun({timeout, _timeout} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxNotificationMessage_Show),
  wxe_util:rec(?wxNotificationMessage_Show).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessageusetaskbaricon">external documentation</a>.
-doc """
If the application already uses a `m:wxTaskBarIcon`, it should be connected to
notifications by using this method.

This has no effect if toast notifications are used.

Return: the task bar icon which was used previously (may be `NULL`)

Only for:wxmsw
""".
-spec useTaskBarIcon(Icon) -> wxTaskBarIcon:wxTaskBarIcon() when
	Icon::wxTaskBarIcon:wxTaskBarIcon().
useTaskBarIcon(#wx_ref{type=IconT}=Icon) ->
  ?CLASS(IconT,wxTaskBarIcon),
  wxe_util:queue_cmd(Icon,?get_env(),?wxNotificationMessage_UseTaskBarIcon),
  wxe_util:rec(?wxNotificationMessage_UseTaskBarIcon).

%% @equiv mSWUseToasts([])
-spec mSWUseToasts() -> boolean().

mSWUseToasts() ->
  mSWUseToasts([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagemswusetoasts">external documentation</a>.
-doc """
Enables toast notifications available since Windows 8 and suppresses the
additional icon in the notification area on Windows 10.

Toast notifications `require` a shortcut to the application in the start menu.
The start menu shortcut needs to contain an Application User Model ID. It is
recommended that the applications setup creates the shortcut and the application
specifies the setup created shortcut in `shortcutPath`. A call to this method
will verify (and if necessary modify) the shortcut before enabling toast
notifications.

Return: false if toast notifications could not be enabled.

Only for:wxmsw

See: `wxAppConsole::SetAppName()` (not implemented in wx),
`wxAppConsole::SetVendorName()` (not implemented in wx)

Since: 3.1.0
""".
-spec mSWUseToasts([Option]) -> boolean() when
	Option :: {'shortcutPath', unicode:chardata()}
		 | {'appId', unicode:chardata()}.
mSWUseToasts(Options)
 when is_list(Options) ->
  MOpts = fun({shortcutPath, ShortcutPath}) ->   ShortcutPath_UC = unicode:characters_to_binary(ShortcutPath),{shortcutPath,ShortcutPath_UC};
          ({appId, AppId}) ->   AppId_UC = unicode:characters_to_binary(AppId),{appId,AppId_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxNotificationMessage_MSWUseToasts),
  wxe_util:rec(?wxNotificationMessage_MSWUseToasts).

%% @doc Destroys this object, do not use object again
-doc """
Destructor does not hide the notification.

The notification can continue to be shown even after the C++ object was
destroyed, call `close/1` explicitly if it needs to be hidden.
""".
-spec destroy(This::wxNotificationMessage()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxNotificationMessage),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
%% @hidden
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
