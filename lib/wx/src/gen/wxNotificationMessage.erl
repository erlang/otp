%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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
-include("wxe.hrl").
-export([addAction/2,addAction/3,close/1,destroy/1,mSWUseToasts/0,mSWUseToasts/1,
  new/0,new/1,new/2,setFlags/2,setIcon/2,setMessage/2,setParent/2,setTitle/2,
  show/1,show/2,useTaskBarIcon/1]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxNotificationMessage() :: wx:wx_object().
-export_type([wxNotificationMessage/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagewxnotificationmessage">external documentation</a>.
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
-spec close(This) -> boolean() when
	This::wxNotificationMessage().
close(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotificationMessage),
  wxe_util:queue_cmd(This,?get_env(),?wxNotificationMessage_Close),
  wxe_util:rec(?wxNotificationMessage_Close).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxNotificationMessage(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxNotificationMessage),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxNotificationMessage_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessageseticon">external documentation</a>.
-spec setIcon(This, Icon) -> 'ok' when
	This::wxNotificationMessage(), Icon::wxIcon:wxIcon().
setIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon) ->
  ?CLASS(ThisT,wxNotificationMessage),
  ?CLASS(IconT,wxIcon),
  wxe_util:queue_cmd(This,Icon,?get_env(),?wxNotificationMessage_SetIcon).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesetmessage">external documentation</a>.
-spec setMessage(This, Message) -> 'ok' when
	This::wxNotificationMessage(), Message::unicode:chardata().
setMessage(#wx_ref{type=ThisT}=This,Message)
 when ?is_chardata(Message) ->
  ?CLASS(ThisT,wxNotificationMessage),
  Message_UC = unicode:characters_to_binary(Message),
  wxe_util:queue_cmd(This,Message_UC,?get_env(),?wxNotificationMessage_SetMessage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesetparent">external documentation</a>.
-spec setParent(This, Parent) -> 'ok' when
	This::wxNotificationMessage(), Parent::wxWindow:wxWindow().
setParent(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent) ->
  ?CLASS(ThisT,wxNotificationMessage),
  ?CLASS(ParentT,wxWindow),
  wxe_util:queue_cmd(This,Parent,?get_env(),?wxNotificationMessage_SetParent).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotificationmessage.html#wxnotificationmessagesettitle">external documentation</a>.
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
-spec destroy(This::wxNotificationMessage()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxNotificationMessage),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxEvtHandler
%% @hidden
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
