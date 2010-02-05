%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html">wxAuiManagerEvent</a>.
%% <dl><dt>Use {@link wxEvtHandler:connect/3.} with EventType:</dt>
%% <dd><em>aui_pane_button</em>, <em>aui_pane_close</em>, <em>aui_pane_maximize</em>, <em>aui_pane_restore</em>, <em>aui_render</em>, <em>aui_find_manager</em></dd></dl>
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

%% @hidden
parent_class(wxEvent) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxAuiManagerEvent(), Mgr::wxAuiManager:wxAuiManager()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventsetmanager">external documentation</a>.
setManager(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=MgrT,ref=MgrRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(MgrT,wxAuiManager),
  wxe_util:cast(?wxAuiManagerEvent_SetManager,
  <<ThisRef:32/?UI,MgrRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent()) -> wxAuiManager:wxAuiManager()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventgetmanager">external documentation</a>.
getManager(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetManager,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent(), P::wxAuiPaneInfo:wxAuiPaneInfo()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventsetpane">external documentation</a>.
setPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PT,ref=PRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PT,wxAuiPaneInfo),
  wxe_util:cast(?wxAuiManagerEvent_SetPane,
  <<ThisRef:32/?UI,PRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent()) -> wxAuiPaneInfo:wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventgetpane">external documentation</a>.
getPane(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetPane,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent(), B::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventsetbutton">external documentation</a>.
setButton(#wx_ref{type=ThisT,ref=ThisRef},B)
 when is_integer(B) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:cast(?wxAuiManagerEvent_SetButton,
  <<ThisRef:32/?UI,B:32/?UI>>).

%% @spec (This::wxAuiManagerEvent()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventgetbutton">external documentation</a>.
getButton(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetButton,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent(), Pdc::wxDC:wxDC()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventsetdc">external documentation</a>.
setDC(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PdcT,ref=PdcRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  ?CLASS(PdcT,wxDC),
  wxe_util:cast(?wxAuiManagerEvent_SetDC,
  <<ThisRef:32/?UI,PdcRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent()) -> wxDC:wxDC()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventgetdc">external documentation</a>.
getDC(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetDC,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent()) -> ok
%% @equiv veto(This, [])
veto(This)
 when is_record(This, wx_ref) ->
  veto(This, []).

%% @spec (This::wxAuiManagerEvent(), [Option]) -> ok
%% Option = {veto, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventveto">external documentation</a>.
veto(#wx_ref{type=ThisT,ref=ThisRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  MOpts = fun({veto, Veto}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Veto)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:cast(?wxAuiManagerEvent_Veto,
  <<ThisRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiManagerEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventgetveto">external documentation</a>.
getVeto(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:call(?wxAuiManagerEvent_GetVeto,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManagerEvent(), Can_veto::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventsetcanveto">external documentation</a>.
setCanVeto(#wx_ref{type=ThisT,ref=ThisRef},Can_veto)
 when is_boolean(Can_veto) ->
  ?CLASS(ThisT,wxAuiManagerEvent),
  wxe_util:cast(?wxAuiManagerEvent_SetCanVeto,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Can_veto)):32/?UI>>).

%% @spec (This::wxAuiManagerEvent()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanagerevent.html#wxauimanagereventcanveto">external documentation</a>.
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
