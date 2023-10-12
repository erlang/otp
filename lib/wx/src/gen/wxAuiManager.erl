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

-module(wxAuiManager).
-include("wxe.hrl").
-export([addPane/2,addPane/3,addPane/4,destroy/1,detachPane/2,getAllPanes/1,
  getArtProvider/1,getDockSizeConstraint/1,getFlags/1,getManagedWindow/1,
  getManager/1,getPane/2,hideHint/1,insertPane/3,insertPane/4,loadPaneInfo/3,
  loadPerspective/2,loadPerspective/3,new/0,new/1,savePaneInfo/2,savePerspective/1,
  setArtProvider/2,setDockSizeConstraint/3,setFlags/2,setManagedWindow/2,
  showHint/2,unInit/1,update/1]).

%% inherited exports
-export([connect/2,connect/3,disconnect/1,disconnect/2,disconnect/3,parent_class/1]).

-type wxAuiManager() :: wx:wx_object().
-export_type([wxAuiManager/0]).
%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxAuiManager().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerwxauimanager">external documentation</a>.
-spec new([Option]) -> wxAuiManager() when
	Option :: {'managed_wnd', wxWindow:wxWindow()}
		 | {'flags', integer()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({managed_wnd, #wx_ref{type=Managed_wndT}} = Arg) ->   ?CLASS(Managed_wndT,wxWindow),Arg;
          ({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxAuiManager_new),
  wxe_util:rec(?wxAuiManager_new).

%% @equiv addPane(This,Window, [])
-spec addPane(This, Window) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow().

addPane(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  addPane(This,Window, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanageraddpane">external documentation</a>.
%% <br /> Also:<br />
%% addPane(This, Window, Pane_info) -> boolean() when<br />
%% 	This::wxAuiManager(), Window::wxWindow:wxWindow(), Pane_info::wxAuiPaneInfo:wxAuiPaneInfo().<br />
%% 
-spec addPane(This, Window, [Option]) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow(),
	Option :: {'direction', integer()}
		 | {'caption', unicode:chardata()};
      (This, Window, Pane_info) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow(), Pane_info::wxAuiPaneInfo:wxAuiPaneInfo().
addPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  MOpts = fun({direction, _direction} = Arg) -> Arg;
          ({caption, Caption}) ->   Caption_UC = unicode:characters_to_binary(Caption),{caption,Caption_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Window, Opts,?get_env(),?wxAuiManager_AddPane_2_0),
  wxe_util:rec(?wxAuiManager_AddPane_2_0);
addPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,#wx_ref{type=Pane_infoT}=Pane_info) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  ?CLASS(Pane_infoT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Window,Pane_info,?get_env(),?wxAuiManager_AddPane_2_1),
  wxe_util:rec(?wxAuiManager_AddPane_2_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanageraddpane">external documentation</a>.
-spec addPane(This, Window, Pane_info, Drop_pos) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow(), Pane_info::wxAuiPaneInfo:wxAuiPaneInfo(), Drop_pos::{X::integer(), Y::integer()}.
addPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,#wx_ref{type=Pane_infoT}=Pane_info,{Drop_posX,Drop_posY} = Drop_pos)
 when is_integer(Drop_posX),is_integer(Drop_posY) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  ?CLASS(Pane_infoT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Window,Pane_info,Drop_pos,?get_env(),?wxAuiManager_AddPane_3),
  wxe_util:rec(?wxAuiManager_AddPane_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerdetachpane">external documentation</a>.
-spec detachPane(This, Window) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow().
detachPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxAuiManager_DetachPane),
  wxe_util:rec(?wxAuiManager_DetachPane).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetallpanes">external documentation</a>.
-spec getAllPanes(This) -> [wxAuiPaneInfo:wxAuiPaneInfo()] when
	This::wxAuiManager().
getAllPanes(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetAllPanes),
  wxe_util:rec(?wxAuiManager_GetAllPanes).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetartprovider">external documentation</a>.
-spec getArtProvider(This) -> wxAuiDockArt:wxAuiDockArt() when
	This::wxAuiManager().
getArtProvider(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetArtProvider),
  wxe_util:rec(?wxAuiManager_GetArtProvider).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetdocksizeconstraint">external documentation</a>.
-spec getDockSizeConstraint(This) -> {Widthpct::number(), Heightpct::number()} when
	This::wxAuiManager().
getDockSizeConstraint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetDockSizeConstraint),
  wxe_util:rec(?wxAuiManager_GetDockSizeConstraint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxAuiManager().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetFlags),
  wxe_util:rec(?wxAuiManager_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetmanagedwindow">external documentation</a>.
-spec getManagedWindow(This) -> wxWindow:wxWindow() when
	This::wxAuiManager().
getManagedWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetManagedWindow),
  wxe_util:rec(?wxAuiManager_GetManagedWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetmanager">external documentation</a>.
-spec getManager(Window) -> wxAuiManager() when
	Window::wxWindow:wxWindow().
getManager(#wx_ref{type=WindowT}=Window) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(Window,?get_env(),?wxAuiManager_GetManager),
  wxe_util:rec(?wxAuiManager_GetManager).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetpane">external documentation</a>.
%% <br /> Also:<br />
%% getPane(This, Window) -> wxAuiPaneInfo:wxAuiPaneInfo() when<br />
%% 	This::wxAuiManager(), Window::wxWindow:wxWindow().<br />
%% 
-spec getPane(This, Name) -> wxAuiPaneInfo:wxAuiPaneInfo() when
	This::wxAuiManager(), Name::unicode:chardata();
      (This, Window) -> wxAuiPaneInfo:wxAuiPaneInfo() when
	This::wxAuiManager(), Window::wxWindow:wxWindow().
getPane(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxAuiManager),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxAuiManager_GetPane_1_0),
  wxe_util:rec(?wxAuiManager_GetPane_1_0);
getPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxAuiManager_GetPane_1_1),
  wxe_util:rec(?wxAuiManager_GetPane_1_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerhidehint">external documentation</a>.
-spec hideHint(This) -> 'ok' when
	This::wxAuiManager().
hideHint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_HideHint).

%% @equiv insertPane(This,Window,Insert_location, [])
-spec insertPane(This, Window, Insert_location) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow(), Insert_location::wxAuiPaneInfo:wxAuiPaneInfo().

insertPane(This,Window,Insert_location)
 when is_record(This, wx_ref),is_record(Window, wx_ref),is_record(Insert_location, wx_ref) ->
  insertPane(This,Window,Insert_location, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerinsertpane">external documentation</a>.
-spec insertPane(This, Window, Insert_location, [Option]) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow(), Insert_location::wxAuiPaneInfo:wxAuiPaneInfo(),
	Option :: {'insert_level', integer()}.
insertPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window,#wx_ref{type=Insert_locationT}=Insert_location, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  ?CLASS(Insert_locationT,wxAuiPaneInfo),
  MOpts = fun({insert_level, _insert_level} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Window,Insert_location, Opts,?get_env(),?wxAuiManager_InsertPane),
  wxe_util:rec(?wxAuiManager_InsertPane).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerloadpaneinfo">external documentation</a>.
-spec loadPaneInfo(This, Pane_part, Pane) -> 'ok' when
	This::wxAuiManager(), Pane_part::unicode:chardata(), Pane::wxAuiPaneInfo:wxAuiPaneInfo().
loadPaneInfo(#wx_ref{type=ThisT}=This,Pane_part,#wx_ref{type=PaneT}=Pane)
 when ?is_chardata(Pane_part) ->
  ?CLASS(ThisT,wxAuiManager),
  Pane_part_UC = unicode:characters_to_binary(Pane_part),
  ?CLASS(PaneT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Pane_part_UC,Pane,?get_env(),?wxAuiManager_LoadPaneInfo).

%% @equiv loadPerspective(This,Perspective, [])
-spec loadPerspective(This, Perspective) -> boolean() when
	This::wxAuiManager(), Perspective::unicode:chardata().

loadPerspective(This,Perspective)
 when is_record(This, wx_ref),?is_chardata(Perspective) ->
  loadPerspective(This,Perspective, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerloadperspective">external documentation</a>.
-spec loadPerspective(This, Perspective, [Option]) -> boolean() when
	This::wxAuiManager(), Perspective::unicode:chardata(),
	Option :: {'update', boolean()}.
loadPerspective(#wx_ref{type=ThisT}=This,Perspective, Options)
 when ?is_chardata(Perspective),is_list(Options) ->
  ?CLASS(ThisT,wxAuiManager),
  Perspective_UC = unicode:characters_to_binary(Perspective),
  MOpts = fun({update, _update} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Perspective_UC, Opts,?get_env(),?wxAuiManager_LoadPerspective),
  wxe_util:rec(?wxAuiManager_LoadPerspective).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersavepaneinfo">external documentation</a>.
-spec savePaneInfo(This, Pane) -> unicode:charlist() when
	This::wxAuiManager(), Pane::wxAuiPaneInfo:wxAuiPaneInfo().
savePaneInfo(#wx_ref{type=ThisT}=This,#wx_ref{type=PaneT}=Pane) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(PaneT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Pane,?get_env(),?wxAuiManager_SavePaneInfo),
  wxe_util:rec(?wxAuiManager_SavePaneInfo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersaveperspective">external documentation</a>.
-spec savePerspective(This) -> unicode:charlist() when
	This::wxAuiManager().
savePerspective(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_SavePerspective),
  wxe_util:rec(?wxAuiManager_SavePerspective).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetartprovider">external documentation</a>.
-spec setArtProvider(This, Art_provider) -> 'ok' when
	This::wxAuiManager(), Art_provider::wxAuiDockArt:wxAuiDockArt().
setArtProvider(#wx_ref{type=ThisT}=This,#wx_ref{type=Art_providerT}=Art_provider) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(Art_providerT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Art_provider,?get_env(),?wxAuiManager_SetArtProvider).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetdocksizeconstraint">external documentation</a>.
-spec setDockSizeConstraint(This, Widthpct, Heightpct) -> 'ok' when
	This::wxAuiManager(), Widthpct::number(), Heightpct::number().
setDockSizeConstraint(#wx_ref{type=ThisT}=This,Widthpct,Heightpct)
 when is_number(Widthpct),is_number(Heightpct) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,Widthpct,Heightpct,?get_env(),?wxAuiManager_SetDockSizeConstraint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxAuiManager(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxAuiManager_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetmanagedwindow">external documentation</a>.
-spec setManagedWindow(This, Managed_wnd) -> 'ok' when
	This::wxAuiManager(), Managed_wnd::wxWindow:wxWindow().
setManagedWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=Managed_wndT}=Managed_wnd) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(Managed_wndT,wxWindow),
  wxe_util:queue_cmd(This,Managed_wnd,?get_env(),?wxAuiManager_SetManagedWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagershowhint">external documentation</a>.
-spec showHint(This, Rect) -> 'ok' when
	This::wxAuiManager(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
showHint(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxAuiManager_ShowHint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanageruninit">external documentation</a>.
-spec unInit(This) -> 'ok' when
	This::wxAuiManager().
unInit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_UnInit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerupdate">external documentation</a>.
-spec update(This) -> 'ok' when
	This::wxAuiManager().
update(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_Update).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAuiManager()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiManager),
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
