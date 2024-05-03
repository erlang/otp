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

-module(wxAuiManager).
-moduledoc """
Functions for wxAuiManager class

`m:wxAuiManager` is the central class of the wxAUI class framework.

`m:wxAuiManager` manages the panes associated with it for a particular
`m:wxFrame`, using a pane's `m:wxAuiPaneInfo` information to determine each
pane's docking and floating behaviour.

`m:wxAuiManager` uses wxWidgets' sizer mechanism to plan the layout of each
frame. It uses a replaceable dock art class to do all drawing, so all drawing is
localized in one area, and may be customized depending on an application's
specific needs.

`m:wxAuiManager` works as follows: the programmer adds panes to the class, or
makes changes to existing pane properties (dock position, floating state, show
state, etc.). To apply these changes, `m:wxAuiManager`'s `update/1` function is
called. This batch processing can be used to avoid flicker, by modifying more
than one pane at a time, and then "committing" all of the changes at once by
calling `update/1`.

Panes can be added quite easily:

Later on, the positions can be modified easily. The following will float an
existing pane in a tool window:

Layers, Rows and Directions, Positions

Inside wxAUI, the docking layout is figured out by checking several pane
parameters. Four of these are important for determining where a pane will end
up:

Styles

This class supports the following styles:

See:
[Overview aui](https://docs.wxwidgets.org/3.1/overview_aui.html#overview_aui),
`m:wxAuiNotebook`, `m:wxAuiDockArt`, `m:wxAuiPaneInfo`

This class is derived (and can use functions) from: `m:wxEvtHandler`

wxWidgets docs:
[wxAuiManager](https://docs.wxwidgets.org/3.1/classwx_aui_manager.html)

## Events

Event types emitted from this class: [`aui_pane_button`](`m:wxAuiManagerEvent`),
[`aui_pane_close`](`m:wxAuiManagerEvent`),
[`aui_pane_maximize`](`m:wxAuiManagerEvent`),
[`aui_pane_restore`](`m:wxAuiManagerEvent`),
[`aui_pane_activated`](`m:wxAuiManagerEvent`),
[`aui_render`](`m:wxAuiManagerEvent`)
""".
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
-doc false.
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxAuiManager().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerwxauimanager">external documentation</a>.
-doc "Constructor.".
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
-doc """
`addPane/4` tells the frame manager to start managing a child window.

There are several versions of this function. The first version allows the full
spectrum of pane parameter possibilities. The second version is used for simpler
user interfaces which do not require as much configuration. The last version
allows a drop position to be specified, which will determine where the pane will
be added.
""".
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
-doc """
Tells the `m:wxAuiManager` to stop managing the pane specified by window.

The window, if in a floated frame, is reparented to the frame managed by
`m:wxAuiManager`.
""".
-spec detachPane(This, Window) -> boolean() when
	This::wxAuiManager(), Window::wxWindow:wxWindow().
detachPane(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(WindowT,wxWindow),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxAuiManager_DetachPane),
  wxe_util:rec(?wxAuiManager_DetachPane).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetallpanes">external documentation</a>.
-doc "Returns an array of all panes managed by the frame manager.".
-spec getAllPanes(This) -> [wxAuiPaneInfo:wxAuiPaneInfo()] when
	This::wxAuiManager().
getAllPanes(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetAllPanes),
  wxe_util:rec(?wxAuiManager_GetAllPanes).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetartprovider">external documentation</a>.
-doc """
Returns the current art provider being used.

See: `m:wxAuiDockArt`
""".
-spec getArtProvider(This) -> wxAuiDockArt:wxAuiDockArt() when
	This::wxAuiManager().
getArtProvider(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetArtProvider),
  wxe_util:rec(?wxAuiManager_GetArtProvider).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetdocksizeconstraint">external documentation</a>.
-doc """
Returns the current dock constraint values.

See `setDockSizeConstraint/3` for more information.
""".
-spec getDockSizeConstraint(This) -> {Widthpct::number(), Heightpct::number()} when
	This::wxAuiManager().
getDockSizeConstraint(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetDockSizeConstraint),
  wxe_util:rec(?wxAuiManager_GetDockSizeConstraint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetflags">external documentation</a>.
-doc "Returns the current ?wxAuiManagerOption's flags.".
-spec getFlags(This) -> integer() when
	This::wxAuiManager().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetFlags),
  wxe_util:rec(?wxAuiManager_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetmanagedwindow">external documentation</a>.
-doc "Returns the frame currently being managed by `m:wxAuiManager`.".
-spec getManagedWindow(This) -> wxWindow:wxWindow() when
	This::wxAuiManager().
getManagedWindow(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_GetManagedWindow),
  wxe_util:rec(?wxAuiManager_GetManagedWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagergetmanager">external documentation</a>.
-doc """
Calling this method will return the `m:wxAuiManager` for a given window.

The `window` parameter should specify any child window or sub-child window of
the frame or window managed by `m:wxAuiManager`.

The `window` parameter need not be managed by the manager itself, nor does it
even need to be a child or sub-child of a managed window. It must however be
inside the window hierarchy underneath the managed window.
""".
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
-doc """
`getPane/2` is used to lookup a `m:wxAuiPaneInfo` object either by window
pointer or by pane name, which acts as a unique id for a window pane.

The returned `m:wxAuiPaneInfo` object may then be modified to change a pane's
look, state or position. After one or more modifications to `m:wxAuiPaneInfo`,
`update/1` should be called to commit the changes to the user interface. If the
lookup failed (meaning the pane could not be found in the manager), a call to
the returned `m:wxAuiPaneInfo`'s IsOk() method will return false.
""".
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
-doc "`hideHint/1` hides any docking hint that may be visible.".
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
-doc """
This method is used to insert either a previously unmanaged pane window into the
frame manager, or to insert a currently managed pane somewhere else.

`insertPane/4` will push all panes, rows, or docks aside and insert the window
into the position specified by `insert_location`.

Because `insert_location` can specify either a pane, dock row, or dock layer,
the `insert_level` parameter is used to disambiguate this. The parameter
`insert_level` can take a value of wxAUI_INSERT_PANE, wxAUI_INSERT_ROW or
wxAUI_INSERT_DOCK.
""".
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
-doc """
`loadPaneInfo/3` is similar to LoadPerspective, with the exception that it only
loads information about a single pane.

This method writes the serialized data into the passed pane. Pointers to UI
elements are not modified.

Note: This operation also changes the name in the pane information\!

See: `loadPerspective/3`

See: `savePaneInfo/2`

See: `savePerspective/1`
""".
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
-doc """
Loads a saved perspective.

A perspective is the layout state of an AUI managed window.

All currently existing panes that have an object in "perspective" with the same
name ("equivalent") will receive the layout parameters of the object in
"perspective". Existing panes that do not have an equivalent in "perspective"
remain unchanged, objects in "perspective" having no equivalent in the manager
are ignored.

See: `loadPaneInfo/3`

See: `loadPerspective/3`

See: `savePerspective/1`
""".
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
-doc """
`savePaneInfo/2` is similar to SavePerspective, with the exception that it only
saves information about a single pane.

Return: The serialized layout parameters of the pane are returned within the
string. Information about the pointers to UI elements stored in the pane are not
serialized.

See: `loadPaneInfo/3`

See: `loadPerspective/3`

See: `savePerspective/1`
""".
-spec savePaneInfo(This, Pane) -> unicode:charlist() when
	This::wxAuiManager(), Pane::wxAuiPaneInfo:wxAuiPaneInfo().
savePaneInfo(#wx_ref{type=ThisT}=This,#wx_ref{type=PaneT}=Pane) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(PaneT,wxAuiPaneInfo),
  wxe_util:queue_cmd(This,Pane,?get_env(),?wxAuiManager_SavePaneInfo),
  wxe_util:rec(?wxAuiManager_SavePaneInfo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersaveperspective">external documentation</a>.
-doc """
Saves the entire user interface layout into an encoded `wxString` (not
implemented in wx), which can then be stored by the application (probably using
wxConfig).

See: `loadPerspective/3`

See: `loadPaneInfo/3`

See: `savePaneInfo/2`
""".
-spec savePerspective(This) -> unicode:charlist() when
	This::wxAuiManager().
savePerspective(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_SavePerspective),
  wxe_util:rec(?wxAuiManager_SavePerspective).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetartprovider">external documentation</a>.
-doc """
Instructs `m:wxAuiManager` to use art provider specified by parameter
`art_provider` for all drawing calls.

This allows pluggable look-and-feel features. The previous art provider object,
if any, will be deleted by `m:wxAuiManager`.

See: `m:wxAuiDockArt`
""".
-spec setArtProvider(This, Art_provider) -> 'ok' when
	This::wxAuiManager(), Art_provider::wxAuiDockArt:wxAuiDockArt().
setArtProvider(#wx_ref{type=ThisT}=This,#wx_ref{type=Art_providerT}=Art_provider) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(Art_providerT,wxAuiDockArt),
  wxe_util:queue_cmd(This,Art_provider,?get_env(),?wxAuiManager_SetArtProvider).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetdocksizeconstraint">external documentation</a>.
-doc """
When a user creates a new dock by dragging a window into a docked position,
often times the large size of the window will create a dock that is unwieldy
large.

`m:wxAuiManager` by default limits the size of any new dock to 1/3 of the window
size. For horizontal docks, this would be 1/3 of the window height. For vertical
docks, 1/3 of the width.

Calling this function will adjust this constraint value. The numbers must be
between 0.0 and 1.0. For instance, calling SetDockSizeContraint with 0.5, 0.5
will cause new docks to be limited to half of the size of the entire managed
window.
""".
-spec setDockSizeConstraint(This, Widthpct, Heightpct) -> 'ok' when
	This::wxAuiManager(), Widthpct::number(), Heightpct::number().
setDockSizeConstraint(#wx_ref{type=ThisT}=This,Widthpct,Heightpct)
 when is_number(Widthpct),is_number(Heightpct) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,Widthpct,Heightpct,?get_env(),?wxAuiManager_SetDockSizeConstraint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetflags">external documentation</a>.
-doc """
This method is used to specify ?wxAuiManagerOption's flags.

`flags` specifies options which allow the frame management behaviour to be
modified.
""".
-spec setFlags(This, Flags) -> 'ok' when
	This::wxAuiManager(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxAuiManager_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagersetmanagedwindow">external documentation</a>.
-doc """
Called to specify the frame or window which is to be managed by
`m:wxAuiManager`.

Frame management is not restricted to just frames. Child windows or custom
controls are also allowed.
""".
-spec setManagedWindow(This, Managed_wnd) -> 'ok' when
	This::wxAuiManager(), Managed_wnd::wxWindow:wxWindow().
setManagedWindow(#wx_ref{type=ThisT}=This,#wx_ref{type=Managed_wndT}=Managed_wnd) ->
  ?CLASS(ThisT,wxAuiManager),
  ?CLASS(Managed_wndT,wxWindow),
  wxe_util:queue_cmd(This,Managed_wnd,?get_env(),?wxAuiManager_SetManagedWindow).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagershowhint">external documentation</a>.
-doc """
This function is used by controls to explicitly show a hint window at the
specified rectangle.

It is rarely called, and is mostly used by controls implementing custom pane
drag/drop behaviour. The specified rectangle should be in screen coordinates.
""".
-spec showHint(This, Rect) -> 'ok' when
	This::wxAuiManager(), Rect::{X::integer(), Y::integer(), W::integer(), H::integer()}.
showHint(#wx_ref{type=ThisT}=This,{RectX,RectY,RectW,RectH} = Rect)
 when is_integer(RectX),is_integer(RectY),is_integer(RectW),is_integer(RectH) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,Rect,?get_env(),?wxAuiManager_ShowHint).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanageruninit">external documentation</a>.
-doc """
Dissociate the managed window from the manager.

This function may be called before the managed frame or window is destroyed,
but, since wxWidgets 3.1.4, it's unnecessary to call it explicitly, as it will
be called automatically when this window is destroyed, as well as when the
manager itself is.
""".
-spec unInit(This) -> 'ok' when
	This::wxAuiManager().
unInit(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_UnInit).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauimanager.html#wxauimanagerupdate">external documentation</a>.
-doc """
This method is called after any number of changes are made to any of the managed
panes.

`update/1` must be invoked after `addPane/4` or `insertPane/4` are called in
order to "realize" or "commit" the changes. In addition, any number of changes
may be made to `m:wxAuiPaneInfo` structures (retrieved with `getPane/2`), but to
realize the changes, `update/1` must be called. This construction allows pane
flicker to be avoided by updating the whole layout at one time.
""".
-spec update(This) -> 'ok' when
	This::wxAuiManager().
update(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxAuiManager),
  wxe_util:queue_cmd(This,?get_env(),?wxAuiManager_Update).

%% @doc Destroys this object, do not use object again
-doc "Dtor.".
-spec destroy(This::wxAuiManager()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiManager),
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
