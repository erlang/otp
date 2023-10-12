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

-module(wxTaskBarIcon).
-include("wxe.hrl").
-export([ new/0, new/1 ,destroy/1,popupMenu/2,removeIcon/1,setIcon/2,setIcon/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxTaskBarIcon() :: wx:wx_object().
-export_type([wxTaskBarIcon/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


-spec new() -> wxTaskBarIcon().
new() ->
    new([]).

%% @doc Creates a TaskBarIcon with a callback function for CreatePopupMenu:
%%   <pre>Callback() -> term()</pre>
%%
-spec new([Option]) -> wxTaskBarIcon() when
      Option :: {'iconType', wx:wx_enum()} |
                {'createPopupMenu', fun(() -> wxMenu:wxMenu())}.

new(Options) when is_list(Options) ->
    Op = ?wxTaskBarIcon_new,
    MOpts = fun({iconType, _iconType} = Arg) -> Arg;
               ({createPopupMenu, Fun}) when is_function(Fun) -> {createPopupMenu,  wxe_util:get_cbId(Fun)};
               (BadOpt) -> erlang:error({badoption, BadOpt}) end,
    Opts = lists:map(MOpts, Options),
    wxe_util:queue_cmd(Opts,?get_env(), Op),
    wxe_util:rec(Op).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconpopupmenu">external documentation</a>.
-spec popupMenu(This, Menu) -> boolean() when
	This::wxTaskBarIcon(), Menu::wxMenu:wxMenu().
popupMenu(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  ?CLASS(MenuT,wxMenu),
  wxe_util:queue_cmd(This,Menu,?get_env(),?wxTaskBarIcon_PopupMenu),
  wxe_util:rec(?wxTaskBarIcon_PopupMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconremoveicon">external documentation</a>.
-spec removeIcon(This) -> boolean() when
	This::wxTaskBarIcon().
removeIcon(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  wxe_util:queue_cmd(This,?get_env(),?wxTaskBarIcon_RemoveIcon),
  wxe_util:rec(?wxTaskBarIcon_RemoveIcon).

%% @equiv setIcon(This,Icon, [])
-spec setIcon(This, Icon) -> boolean() when
	This::wxTaskBarIcon(), Icon::wxIcon:wxIcon().

setIcon(This,Icon)
 when is_record(This, wx_ref),is_record(Icon, wx_ref) ->
  setIcon(This,Icon, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconseticon">external documentation</a>.
-spec setIcon(This, Icon, [Option]) -> boolean() when
	This::wxTaskBarIcon(), Icon::wxIcon:wxIcon(),
	Option :: {'tooltip', unicode:chardata()}.
setIcon(#wx_ref{type=ThisT}=This,#wx_ref{type=IconT}=Icon, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  ?CLASS(IconT,wxIcon),
  MOpts = fun({tooltip, Tooltip}) ->   Tooltip_UC = unicode:characters_to_binary(Tooltip),{tooltip,Tooltip_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Icon, Opts,?get_env(),?wxTaskBarIcon_SetIcon),
  wxe_util:rec(?wxTaskBarIcon_SetIcon).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxTaskBarIcon()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTaskBarIcon),
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
