%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html">wxAuiManagerEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>aui_pane_button</em>, <em>aui_pane_close</em>, <em>aui_pane_maximize</em>, <em>aui_pane_restore</em>, <em>aui_pane_activated</em>, <em>aui_render</em>, <em>aui_find_manager</em></dd></dl>
%% See also the message variant {@link wxEvtHandler:wxAuiManager(). #wxAuiManager{}} event record type.
%%
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvent}
%% </p>
%% @type wxAuiManagerEvent().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAuiManagerEvent).
-include("wxe.hrl").
-export([canVeto/1,getButton/1,getDC/1,getManager/1,getPane/1,getVeto/1,setButton/2,
  setCanVeto/2,setDC/2,setManager/2,setPane/2,veto/1,veto/2]).

%% inherited exports
-export([getId/1,getSkipped/1,getTimestamp/1,isCommandEvent/1,parent_class/1,
  resumePropagation/2,shouldPropagate/1,skip/1,skip/2,stopPropagation/1]).

-export_type([wxAuiManagerEvent/0]).
%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAuiManagerEvent() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetmanager">external documentation</a>.
-spec setManager(This, Mgr) -> 'ok' when
	This::wxAuiManagerEvent(), Mgr::wxAuiManager:wxAuiManager().
setManager(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MgrT,ref=MgrRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(MgrT,wxAuiManager),
  wxe_util:cast(?wxAuiManagerEvent_SetManager,
  <<ThisRef:32/?UI,MgrRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetmanager">external documentation</a>.
-spec getManager(This) -> wxAuiManager:wxAuiManager() when
	This::wxAuiManagerEvent().
getManager(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetManager,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetpane">external documentation</a>.
-spec setPane(This, P) -> 'ok' when
	This::wxAuiManagerEvent(), P::wxAuiPaneInfo:wxAuiPaneInfo().
setPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PT,ref=PRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PT,wxAuiPaneInfo),
  wxe_util:cast(?wxAuiManagerEvent_SetPane,
  <<ThisRef:32/?UI,PRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetpane">external documentation</a>.
-spec getPane(This) -> wxAuiPaneInfo:wxAuiPaneInfo() when
	This::wxAuiManagerEvent().
getPane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetPane,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetbutton">external documentation</a>.
-spec setButton(This, B) -> 'ok' when
	This::wxAuiManagerEvent(), B::integer().
setButton(#wx_ref{type=ThisT,ref=ThisRef},B)
 when is_integer(B) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:cast(?wxAuiManagerEvent_SetButton,
  <<ThisRef:32/?UI,B:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetbutton">external documentation</a>.
-spec getButton(This) -> integer() when
	This::wxAuiManagerEvent().
getButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetButton,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetdc">external documentation</a>.
-spec setDC(This, Pdc) -> 'ok' when
	This::wxAuiManagerEvent(), Pdc::wxDC:wxDC().
setDC(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PdcT,ref=PdcRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PdcT,wxDC),
  wxe_util:cast(?wxAuiManagerEvent_SetDC,
  <<ThisRef:32/?UI,PdcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetdc">external documentation</a>.
-spec getDC(This) -> wxDC:wxDC() when
	This::wxAuiManagerEvent().
getDC(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetDC,
  <<ThisRef:32/?UI>>).

%% @equiv veto(This, [])
-spec veto(This) -> 'ok' when
	This::wxAuiManagerEvent().

veto(This)
 when is_record(This, wx_ref) ->
  veto(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventveto">external documentation</a>.
-spec veto(This, [Option]) -> 'ok' when
	This::wxAuiManagerEvent(),
	Option :: {'veto', boolean()}.
veto(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  MOpts = fun({veto, Veto}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Veto)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxAuiManagerEvent_Veto,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventgetveto">external documentation</a>.
-spec getVeto(This) -> boolean() when
	This::wxAuiManagerEvent().
getVeto(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetVeto,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventsetcanveto">external documentation</a>.
-spec setCanVeto(This, Can_veto) -> 'ok' when
	This::wxAuiManagerEvent(), Can_veto::boolean().
setCanVeto(#wx_ref{type=ThisT,ref=ThisRef},Can_veto)
 when is_boolean(Can_veto) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:cast(?wxAuiManagerEvent_SetCanVeto,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Can_veto)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanagerevent.html#wxauimanagereventcanveto">external documentation</a>.
-spec canVeto(This) -> boolean() when
	This::wxAuiManagerEvent().
canVeto(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_CanVeto,
  <<ThisRef:32/?UI>>).

 %% From wxEvent
%% @hidden
stopPropagation(This) -> wxEvent:stopPropagation(This).
%% @hidden
skip(This, Options) -> wxEvent:skip(This, Options).
%% @hidden
skip(This) -> wxEvent:skip(This).
%% @hidden
shouldPropagate(This) -> wxEvent:shouldPropagate(This).
%% @hidden
resumePropagation(This,PropagationLevel) -> wxEvent:resumePropagation(This,PropagationLevel).
%% @hidden
isCommandEvent(This) -> wxEvent:isCommandEvent(This).
%% @hidden
getTimestamp(This) -> wxEvent:getTimestamp(This).
%% @hidden
getSkipped(This) -> wxEvent:getSkipped(This).
%% @hidden
getId(This) -> wxEvent:getId(This).
