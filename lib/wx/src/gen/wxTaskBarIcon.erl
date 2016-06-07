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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html">wxTaskBarIcon</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxTaskBarIcon().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxTaskBarIcon).
-include("wxe.hrl").
-export([destroy/1,new/0,popupMenu/2,removeIcon/1,setIcon/2,setIcon/3]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-export_type([wxTaskBarIcon/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxTaskBarIcon() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconwxtaskbaricon">external documentation</a>.
-spec new() -> wxTaskBarIcon().
new() ->
  wxe_util:construct(?wxTaskBarIcon_new,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconpopupmenu">external documentation</a>.
-spec popupMenu(This, Menu) -> boolean() when
	This::wxTaskBarIcon(), Menu::wxMenu:wxMenu().
popupMenu(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MenuT,ref=MenuRef}) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  ?CLASS(MenuT,wxMenu),
  wxe_util:call(?wxTaskBarIcon_PopupMenu,
  <<ThisRef:32/?UI,MenuRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtaskbaricon.html#wxtaskbariconremoveicon">external documentation</a>.
-spec removeIcon(This) -> boolean() when
	This::wxTaskBarIcon().
removeIcon(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  wxe_util:call(?wxTaskBarIcon_RemoveIcon,
  <<ThisRef:32/?UI>>).

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
setIcon(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=IconT,ref=IconRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxTaskBarIcon),
  ?CLASS(IconT,wxIcon),
  MOpts = fun({tooltip, Tooltip}, Acc) ->   Tooltip_UC = unicode:characters_to_binary([Tooltip,0]),[<<1:32/?UI,(byte_size(Tooltip_UC)):32/?UI,(Tooltip_UC)/binary, 0:(((8- ((0+byte_size(Tooltip_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxTaskBarIcon_SetIcon,
  <<ThisRef:32/?UI,IconRef:32/?UI, BinOpt/binary>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxTaskBarIcon()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxTaskBarIcon),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
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
