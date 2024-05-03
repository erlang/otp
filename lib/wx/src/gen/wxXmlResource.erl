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

-module(wxXmlResource).
-moduledoc """
Functions for wxXmlResource class

This is the main class for interacting with the XML-based resource system.

The class holds XML resources from one or more .xml files, binary files or zip
archive files.

Note that this is a singleton class and you'll never allocate/deallocate it.
Just use the static `get/0` getter.

See:
[Overview xrc](https://docs.wxwidgets.org/3.1/overview_xrc.html#overview_xrc),
[Overview xrcformat](https://docs.wxwidgets.org/3.1/overview_xrcformat.html#overview_xrcformat)

wxWidgets docs:
[wxXmlResource](https://docs.wxwidgets.org/3.1/classwxXml_resource.html)
""".
-include("wxe.hrl").
-export([ xrcctrl/3 ,attachUnknownControl/3,attachUnknownControl/4,clearHandlers/1,
  compareVersion/5,destroy/1,get/0,getFlags/1,getVersion/1,getXRCID/1,
  getXRCID/2,initAllHandlers/1,load/2,loadBitmap/2,loadDialog/3,loadDialog/4,
  loadFrame/3,loadFrame/4,loadIcon/2,loadMenu/2,loadMenuBar/2,loadMenuBar/3,
  loadPanel/3,loadPanel/4,loadToolBar/3,new/0,new/1,new/2,set/1,setFlags/2,
  unload/2]).

%% inherited exports
-export([parent_class/1]).

-type wxXmlResource() :: wx:wx_object().
-export_type([wxXmlResource/0]).
%% @hidden
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxXmlResource().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
-doc "Constructor.".
-spec new([Option]) -> wxXmlResource() when
	Option :: {'flags', integer()}
		 | {'domain', unicode:chardata()}.
new(Options)
 when is_list(Options) ->
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          ({domain, Domain}) ->   Domain_UC = unicode:characters_to_binary(Domain),{domain,Domain_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Opts,?get_env(),?wxXmlResource_new_1),
  wxe_util:rec(?wxXmlResource_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
-doc "Constructor.".
-spec new(Filemask, [Option]) -> wxXmlResource() when
	Filemask::unicode:chardata(),
	Option :: {'flags', integer()}
		 | {'domain', unicode:chardata()}.
new(Filemask, Options)
 when ?is_chardata(Filemask),is_list(Options) ->
  Filemask_UC = unicode:characters_to_binary(Filemask),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          ({domain, Domain}) ->   Domain_UC = unicode:characters_to_binary(Domain),{domain,Domain_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Filemask_UC, Opts,?get_env(),?wxXmlResource_new_2),
  wxe_util:rec(?wxXmlResource_new_2).

%% @equiv attachUnknownControl(This,Name,Control, [])
-spec attachUnknownControl(This, Name, Control) -> boolean() when
	This::wxXmlResource(), Name::unicode:chardata(), Control::wxWindow:wxWindow().

attachUnknownControl(This,Name,Control)
 when is_record(This, wx_ref),?is_chardata(Name),is_record(Control, wx_ref) ->
  attachUnknownControl(This,Name,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceattachunknowncontrol">external documentation</a>.
-doc """
Attaches an unknown control to the given panel/window/dialog.

Unknown controls are used in conjunction with <object class="unknown">.
""".
-spec attachUnknownControl(This, Name, Control, [Option]) -> boolean() when
	This::wxXmlResource(), Name::unicode:chardata(), Control::wxWindow:wxWindow(),
	Option :: {'parent', wxWindow:wxWindow()}.
attachUnknownControl(#wx_ref{type=ThisT}=This,Name,#wx_ref{type=ControlT}=Control, Options)
 when ?is_chardata(Name),is_list(Options) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  ?CLASS(ControlT,wxWindow),
  MOpts = fun({parent, #wx_ref{type=ParentT}} = Arg) ->   ?CLASS(ParentT,wxWindow),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Name_UC,Control, Opts,?get_env(),?wxXmlResource_AttachUnknownControl),
  wxe_util:rec(?wxXmlResource_AttachUnknownControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceclearhandlers">external documentation</a>.
-doc """
Removes all handlers and deletes them (this means that any handlers added using
`AddHandler()` (not implemented in wx) must be allocated on the heap).
""".
-spec clearHandlers(This) -> 'ok' when
	This::wxXmlResource().
clearHandlers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_ClearHandlers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcecompareversion">external documentation</a>.
-doc """
Compares the XRC version to the argument.

Returns -1 if the XRC version is less than the argument, +1 if greater, and 0 if
they are equal.
""".
-spec compareVersion(This, Major, Minor, Release, Revision) -> integer() when
	This::wxXmlResource(), Major::integer(), Minor::integer(), Release::integer(), Revision::integer().
compareVersion(#wx_ref{type=ThisT}=This,Major,Minor,Release,Revision)
 when is_integer(Major),is_integer(Minor),is_integer(Release),is_integer(Revision) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,Major,Minor,Release,Revision,?get_env(),?wxXmlResource_CompareVersion),
  wxe_util:rec(?wxXmlResource_CompareVersion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceget">external documentation</a>.
-doc "Gets the global resources object or creates one if none exists.".
-spec get() -> wxXmlResource().
get() ->
  wxe_util:queue_cmd(?get_env(), ?wxXmlResource_Get),
  wxe_util:rec(?wxXmlResource_Get).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetflags">external documentation</a>.
-doc "Returns flags, which may be a bitlist of ?wxXmlResourceFlags enumeration values.".
-spec getFlags(This) -> integer() when
	This::wxXmlResource().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_GetFlags),
  wxe_util:rec(?wxXmlResource_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetversion">external documentation</a>.
-doc """
Returns version information (a.b.c.d = d + 256*c + 2562*b + 2563\*a).
""".
-spec getVersion(This) -> integer() when
	This::wxXmlResource().
getVersion(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_GetVersion),
  wxe_util:rec(?wxXmlResource_GetVersion).

%% @equiv getXRCID(Str_id, [])
-spec getXRCID(Str_id) -> integer() when
	Str_id::unicode:chardata().

getXRCID(Str_id)
 when ?is_chardata(Str_id) ->
  getXRCID(Str_id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetxrcid">external documentation</a>.
-doc """
Returns a numeric ID that is equivalent to the string ID used in an XML
resource.

If an unknown `str_id` is requested (i.e. other than wxID_XXX or integer), a new
record is created which associates the given string with a number.

If `value_if_not_found` is `wxID_NONE`, the number is obtained via
`wx_misc:newId/0`. Otherwise `value_if_not_found` is used.

Macro `XRCID(name)` is provided for convenient use in event tables.

Note: IDs returned by XRCID() cannot be used with the `EVT_*_RANGE` macros,
because the order in which they are assigned to symbolic `name` values is not
guaranteed.
""".
-spec getXRCID(Str_id, [Option]) -> integer() when
	Str_id::unicode:chardata(),
	Option :: {'value_if_not_found', integer()}.
getXRCID(Str_id, Options)
 when ?is_chardata(Str_id),is_list(Options) ->
  Str_id_UC = unicode:characters_to_binary(Str_id),
  MOpts = fun({value_if_not_found, _value_if_not_found} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Str_id_UC, Opts,?get_env(),?wxXmlResource_GetXRCID),
  wxe_util:rec(?wxXmlResource_GetXRCID).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceinitallhandlers">external documentation</a>.
-doc """
Initializes handlers for all supported controls/windows.

This will make the executable quite big because it forces linking against most
of the wxWidgets library.
""".
-spec initAllHandlers(This) -> 'ok' when
	This::wxXmlResource().
initAllHandlers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_InitAllHandlers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceload">external documentation</a>.
-doc """
Loads resources from XML files that match given filemask.

Example:

Note: If wxUSE_FILESYS is enabled, this method understands `wxFileSystem` (not
implemented in wx) URLs (see `wxFileSystem::FindFirst()` (not implemented in
wx)).

Note: If you are sure that the argument is name of single XRC file (rather than
an URL or a wildcard), use `LoadFile()` (not implemented in wx) instead.

See: `LoadFile()` (not implemented in wx), `LoadAllFiles()` (not implemented in
wx)
""".
-spec load(This, Filemask) -> boolean() when
	This::wxXmlResource(), Filemask::unicode:chardata().
load(#wx_ref{type=ThisT}=This,Filemask)
 when ?is_chardata(Filemask) ->
  ?CLASS(ThisT,wxXmlResource),
  Filemask_UC = unicode:characters_to_binary(Filemask),
  wxe_util:queue_cmd(This,Filemask_UC,?get_env(),?wxXmlResource_Load),
  wxe_util:rec(?wxXmlResource_Load).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadbitmap">external documentation</a>.
-doc "Loads a bitmap resource from a file.".
-spec loadBitmap(This, Name) -> wxBitmap:wxBitmap() when
	This::wxXmlResource(), Name::unicode:chardata().
loadBitmap(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxXmlResource_LoadBitmap),
  wxe_util:rec(?wxXmlResource_LoadBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
-doc """
Loads a dialog.

`parent` points to parent window (if any).
""".
-spec loadDialog(This, Parent, Name) -> wxDialog:wxDialog() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadDialog(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Parent,Name_UC,?get_env(),?wxXmlResource_LoadDialog_2),
  wxe_util:rec(?wxXmlResource_LoadDialog_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
-doc """
Loads a dialog.

`parent` points to parent window (if any).

This form is used to finish creation of an already existing instance (the main
reason for this is that you may want to use derived class with a new event
table). Example:
""".
-spec loadDialog(This, Dlg, Parent, Name) -> boolean() when
	This::wxXmlResource(), Dlg::wxDialog:wxDialog(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadDialog(#wx_ref{type=ThisT}=This,#wx_ref{type=DlgT}=Dlg,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(DlgT,wxDialog),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Dlg,Parent,Name_UC,?get_env(),?wxXmlResource_LoadDialog_3),
  wxe_util:rec(?wxXmlResource_LoadDialog_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
-doc """
Loads a frame from the resource.

`parent` points to parent window (if any).
""".
-spec loadFrame(This, Parent, Name) -> wxFrame:wxFrame() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Parent,Name_UC,?get_env(),?wxXmlResource_LoadFrame_2),
  wxe_util:rec(?wxXmlResource_LoadFrame_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
-doc """
Loads the contents of a frame onto an existing `m:wxFrame`.

This form is used to finish creation of an already existing instance (the main
reason for this is that you may want to use derived class with a new event
table).
""".
-spec loadFrame(This, Frame, Parent, Name) -> boolean() when
	This::wxXmlResource(), Frame::wxFrame:wxFrame(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(FrameT,wxFrame),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Frame,Parent,Name_UC,?get_env(),?wxXmlResource_LoadFrame_3),
  wxe_util:rec(?wxXmlResource_LoadFrame_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadicon">external documentation</a>.
-doc "Loads an icon resource from a file.".
-spec loadIcon(This, Name) -> wxIcon:wxIcon() when
	This::wxXmlResource(), Name::unicode:chardata().
loadIcon(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxXmlResource_LoadIcon),
  wxe_util:rec(?wxXmlResource_LoadIcon).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenu">external documentation</a>.
-doc """
Loads menu from resource.

Returns NULL on failure.
""".
-spec loadMenu(This, Name) -> wxMenu:wxMenu() when
	This::wxXmlResource(), Name::unicode:chardata().
loadMenu(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxXmlResource_LoadMenu),
  wxe_util:rec(?wxXmlResource_LoadMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
-spec loadMenuBar(This, Name) -> wxMenuBar:wxMenuBar() when
	This::wxXmlResource(), Name::unicode:chardata().
loadMenuBar(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxXmlResource_LoadMenuBar_1),
  wxe_util:rec(?wxXmlResource_LoadMenuBar_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
-doc """
Loads a menubar from resource.

Returns NULL on failure.
""".
-spec loadMenuBar(This, Parent, Name) -> wxMenuBar:wxMenuBar() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadMenuBar(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Parent,Name_UC,?get_env(),?wxXmlResource_LoadMenuBar_2),
  wxe_util:rec(?wxXmlResource_LoadMenuBar_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
-doc """
Loads a panel.

`parent` points to the parent window.
""".
-spec loadPanel(This, Parent, Name) -> wxPanel:wxPanel() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadPanel(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Parent,Name_UC,?get_env(),?wxXmlResource_LoadPanel_2),
  wxe_util:rec(?wxXmlResource_LoadPanel_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
-doc """
Loads a panel.

`parent` points to the parent window. This form is used to finish creation of an
already existing instance.
""".
-spec loadPanel(This, Panel, Parent, Name) -> boolean() when
	This::wxXmlResource(), Panel::wxPanel:wxPanel(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadPanel(#wx_ref{type=ThisT}=This,#wx_ref{type=PanelT}=Panel,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(PanelT,wxPanel),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Panel,Parent,Name_UC,?get_env(),?wxXmlResource_LoadPanel_3),
  wxe_util:rec(?wxXmlResource_LoadPanel_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadtoolbar">external documentation</a>.
-doc "Loads a toolbar.".
-spec loadToolBar(This, Parent, Name) -> wxToolBar:wxToolBar() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadToolBar(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Parent,Name_UC,?get_env(),?wxXmlResource_LoadToolBar),
  wxe_util:rec(?wxXmlResource_LoadToolBar).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceset">external documentation</a>.
-doc """
Sets the global resources object and returns a pointer to the previous one (may
be NULL).
""".
-spec set(Res) -> wxXmlResource() when
	Res::wxXmlResource().
set(#wx_ref{type=ResT}=Res) ->
  ?CLASS(ResT,wxXmlResource),
  wxe_util:queue_cmd(Res,?get_env(),?wxXmlResource_Set),
  wxe_util:rec(?wxXmlResource_Set).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcesetflags">external documentation</a>.
-doc "Sets flags (bitlist of ?wxXmlResourceFlags enumeration values).".
-spec setFlags(This, Flags) -> 'ok' when
	This::wxXmlResource(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxXmlResource_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceunload">external documentation</a>.
-doc """
This function unloads a resource previously loaded by `load/2`.

Returns true if the resource was successfully unloaded and false if it hasn't
been found in the list of loaded resources.
""".
-spec unload(This, Filename) -> boolean() when
	This::wxXmlResource(), Filename::unicode:chardata().
unload(#wx_ref{type=ThisT}=This,Filename)
 when ?is_chardata(Filename) ->
  ?CLASS(ThisT,wxXmlResource),
  Filename_UC = unicode:characters_to_binary(Filename),
  wxe_util:queue_cmd(This,Filename_UC,?get_env(),?wxXmlResource_Unload),
  wxe_util:rec(?wxXmlResource_Unload).


-doc """
Looks up a control.

Get a control with `Name` in a window created with XML resources. You can use it
to set/get values from controls. The object is type casted to `Type`. Example:
""".
-spec xrcctrl(Window, Name, Type) -> wx:wx_object() when
      Window::wxWindow:wxWindow(),
      Name::string(),
      Type::atom().

xrcctrl(Window = #wx_ref{}, Name, Type) when is_list(Name), is_atom(Type) ->
    %% Func Id ?wxXmlResource_xrcctrl 
    ID  = wxXmlResource:getXRCID(Name),
    Res = wxWindow:findWindow(Window,ID),
    wx:typeCast(Res, Type).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxXmlResource()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxXmlResource),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
