%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html">wxAuiManager</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxAuiManager().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

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

%% @hidden
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxAuiManager()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxAuiManager()
%% Option = {managed_wnd, wxWindow:wxWindow()} | {flags, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerwxauimanager">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({managed_wnd, #wx_ref{type=Managed_wndT,ref=Managed_wndRef}}, Acc) ->   ?CLASS(Managed_wndT,wxWindow),[<<1:32/?UI,Managed_wndRef:32/?UI>>|Acc];
          ({flags, Flags}, Acc) -> [<<2:32/?UI,Flags:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxAuiManager_new,
  <<BinOpt/binary>>).

%% @spec (This::wxAuiManager(), Window::wxWindow:wxWindow()) -> bool()
%% @equiv addPane(This,Window, [])
addPane(This,Window)
 when is_record(This, wx_ref),is_record(Window, wx_ref) ->
  addPane(This,Window, []).

%% @spec (This::wxAuiManager(),Window::wxWindow:wxWindow(),X::term()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanageraddpane">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% addPane(This::wxAuiManager(), Window::wxWindow:wxWindow(), [Option]) -> bool() </c>
%%<br /> Option = {direction, integer()} | {caption, string()}
%% </p>
%% <p><c>
%% addPane(This::wxAuiManager(), Window::wxWindow:wxWindow(), Pane_info::wxAuiPaneInfo:wxAuiPaneInfo()) -> bool() </c>
%% </p>
addPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  MOpts = fun({direction, Direction}, Acc) -> [<<1:32/?UI,Direction:32/?UI>>|Acc];
          ({caption, Caption}, Acc) ->   Caption_UC = unicode:characters_to_binary([Caption,0]),[<<2:32/?UI,(byte_size(Caption_UC)):32/?UI,(Caption_UC)/binary, 0:(((8- ((0+byte_size(Caption_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiManager_AddPane_2_0,
  <<ThisRef:32/?UI,WindowRef:32/?UI, BinOpt/binary>>);
addPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=Pane_infoT,ref=Pane_infoRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  ?CLASS(Pane_infoT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiManager_AddPane_2_1,
  <<ThisRef:32/?UI,WindowRef:32/?UI,Pane_infoRef:32/?UI>>).

%% @spec (This::wxAuiManager(), Window::wxWindow:wxWindow(), Pane_info::wxAuiPaneInfo:wxAuiPaneInfo(), Drop_pos::{X::integer(),Y::integer()}) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanageraddpane">external documentation</a>.
addPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=Pane_infoT,ref=Pane_infoRef},{Drop_posX,Drop_posY})
 when is_integer(Drop_posX),is_integer(Drop_posY) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  ?CLASS(Pane_infoT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiManager_AddPane_3,
  <<ThisRef:32/?UI,WindowRef:32/?UI,Pane_infoRef:32/?UI,Drop_posX:32/?UI,Drop_posY:32/?UI>>).

%% @spec (This::wxAuiManager(), Window::wxWindow:wxWindow()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerdetachpane">external documentation</a>.
detachPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxAuiManager_DetachPane,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> wxAuiPaneInfoArray:wxAuiPaneInfoArray()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetallpanes">external documentation</a>.
getAllPanes(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:call(?wxAuiManager_GetAllPanes,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> wxAuiDockArt:wxAuiDockArt()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetartprovider">external documentation</a>.
getArtProvider(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:call(?wxAuiManager_GetArtProvider,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> {Width_pct::float(),Height_pct::float()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetdocksizeconstraint">external documentation</a>.
getDockSizeConstraint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:call(?wxAuiManager_GetDockSizeConstraint,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetflags">external documentation</a>.
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:call(?wxAuiManager_GetFlags,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> wxWindow:wxWindow()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetmanagedwindow">external documentation</a>.
getManagedWindow(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:call(?wxAuiManager_GetManagedWindow,
  <<ThisRef:32/?UI>>).

%% @spec (Window::wxWindow:wxWindow()) -> wxAuiManager()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetmanager">external documentation</a>.
getManager(#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxAuiManager_GetManager,
  <<WindowRef:32/?UI>>).

%% @spec (This::wxAuiManager(),X::string()|term()) -> wxAuiPaneInfo:wxAuiPaneInfo()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagergetpane">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% getPane(This::wxAuiManager(), Name::string()) -> wxAuiPaneInfo:wxAuiPaneInfo() </c>
%% </p>
%% <p><c>
%% getPane(This::wxAuiManager(), Window::wxWindow:wxWindow()) -> wxAuiPaneInfo:wxAuiPaneInfo() </c>
%% </p>
getPane(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxAuiManager),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxAuiManager_GetPane_1_0,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>);
getPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  wxe_util:call(?wxAuiManager_GetPane_1_1,
  <<ThisRef:32/?UI,WindowRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerhidehint">external documentation</a>.
hideHint(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:cast(?wxAuiManager_HideHint,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager(), Window::wxWindow:wxWindow(), Insert_location::wxAuiPaneInfo:wxAuiPaneInfo()) -> bool()
%% @equiv insertPane(This,Window,Insert_location, [])
insertPane(This,Window,Insert_location)
 when is_record(This, wx_ref),is_record(Window, wx_ref),is_record(Insert_location, wx_ref) ->
  insertPane(This,Window,Insert_location, []).

%% @spec (This::wxAuiManager(), Window::wxWindow:wxWindow(), Insert_location::wxAuiPaneInfo:wxAuiPaneInfo(), [Option]) -> bool()
%% Option = {insert_level, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerinsertpane">external documentation</a>.
insertPane(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=WindowT,ref=WindowRef},#wx_ref{type=Insert_locationT,ref=Insert_locationRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  ?CLASS(Insert_locationT,wxAuiPaneInfo),
  MOpts = fun({insert_level, Insert_level}, Acc) -> [<<1:32/?UI,Insert_level:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiManager_InsertPane,
  <<ThisRef:32/?UI,WindowRef:32/?UI,Insert_locationRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxAuiManager(), Pane_part::string(), Pane::wxAuiPaneInfo:wxAuiPaneInfo()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerloadpaneinfo">external documentation</a>.
loadPaneInfo(#wx_ref{type=ThisT,ref=ThisRef},Pane_part,#wx_ref{type=PaneT,ref=PaneRef})
 when is_list(Pane_part) ->
  ?CLASS(ThisT,wxAuiManager),
  Pane_part_UC = unicode:characters_to_binary([Pane_part,0]),
  ?CLASS(PaneT,wxAuiPaneInfo),
  wxe_util:cast(?wxAuiManager_LoadPaneInfo,
  <<ThisRef:32/?UI,(byte_size(Pane_part_UC)):32/?UI,(Pane_part_UC)/binary, 0:(((8- ((0+byte_size(Pane_part_UC)) band 16#7)) band 16#7))/unit:8,PaneRef:32/?UI>>).

%% @spec (This::wxAuiManager(), Perspective::string()) -> bool()
%% @equiv loadPerspective(This,Perspective, [])
loadPerspective(This,Perspective)
 when is_record(This, wx_ref),is_list(Perspective) ->
  loadPerspective(This,Perspective, []).

%% @spec (This::wxAuiManager(), Perspective::string(), [Option]) -> bool()
%% Option = {update, bool()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerloadperspective">external documentation</a>.
loadPerspective(#wx_ref{type=ThisT,ref=ThisRef},Perspective, Options)
 when is_list(Perspective),is_list(Options) ->
  ?CLASS(ThisT,wxAuiManager),
  Perspective_UC = unicode:characters_to_binary([Perspective,0]),
  MOpts = fun({update, Update}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Update)):32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiManager_LoadPerspective,
  <<ThisRef:32/?UI,(byte_size(Perspective_UC)):32/?UI,(Perspective_UC)/binary, 0:(((8- ((0+byte_size(Perspective_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxAuiManager(), Pane::wxAuiPaneInfo:wxAuiPaneInfo()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagersavepaneinfo">external documentation</a>.
savePaneInfo(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PaneT,ref=PaneRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(PaneT,wxAuiPaneInfo),
  wxe_util:call(?wxAuiManager_SavePaneInfo,
  <<ThisRef:32/?UI,PaneRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagersaveperspective">external documentation</a>.
savePerspective(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:call(?wxAuiManager_SavePerspective,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager(), Art_provider::wxAuiDockArt:wxAuiDockArt()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagersetartprovider">external documentation</a>.
setArtProvider(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=Art_providerT,ref=Art_providerRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(Art_providerT,wxAuiDockArt),
  wxe_util:cast(?wxAuiManager_SetArtProvider,
  <<ThisRef:32/?UI,Art_providerRef:32/?UI>>).

%% @spec (This::wxAuiManager(), Width_pct::float(), Height_pct::float()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagersetdocksizeconstraint">external documentation</a>.
setDockSizeConstraint(#wx_ref{type=ThisT,ref=ThisRef},Width_pct,Height_pct)
 when is_float(Width_pct),is_float(Height_pct) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:cast(?wxAuiManager_SetDockSizeConstraint,
  <<ThisRef:32/?UI,0:32,Width_pct:64/?F,Height_pct:64/?F>>).

%% @spec (This::wxAuiManager(), Flags::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagersetflags">external documentation</a>.
setFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:cast(?wxAuiManager_SetFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @spec (This::wxAuiManager(), Managed_wnd::wxWindow:wxWindow()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagersetmanagedwindow">external documentation</a>.
setManagedWindow(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=Managed_wndT,ref=Managed_wndRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(Managed_wndT,wxWindow),
  wxe_util:cast(?wxAuiManager_SetManagedWindow,
  <<ThisRef:32/?UI,Managed_wndRef:32/?UI>>).

%% @spec (This::wxAuiManager(), Rect::{X::integer(),Y::integer(),W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagershowhint">external documentation</a>.
showHint(#wx_ref{type=ThisT,ref=ThisRef},{RectX,RectY,RectW,RectH})
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:cast(?wxAuiManager_ShowHint,
  <<ThisRef:32/?UI,RectX:32/?UI,RectY:32/?UI,RectW:32/?UI,RectH:32/?UI>>).

%% @spec (This::wxAuiManager()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanageruninit">external documentation</a>.
unInit(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:cast(?wxAuiManager_UnInit,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxauimanager.html#wxauimanagerupdate">external documentation</a>.
update(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:cast(?wxAuiManager_Update,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxAuiManager()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiManager),
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
