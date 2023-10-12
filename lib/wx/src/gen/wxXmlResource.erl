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

-module(wxXmlResource).
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
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new([])
-spec new() -> wxXmlResource().

new() ->
  new([]).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
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
-spec clearHandlers(This) -> 'ok' when
	This::wxXmlResource().
clearHandlers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_ClearHandlers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcecompareversion">external documentation</a>.
-spec compareVersion(This, Major, Minor, Release, Revision) -> integer() when
	This::wxXmlResource(), Major::integer(), Minor::integer(), Release::integer(), Revision::integer().
compareVersion(#wx_ref{type=ThisT}=This,Major,Minor,Release,Revision)
 when is_integer(Major),is_integer(Minor),is_integer(Release),is_integer(Revision) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,Major,Minor,Release,Revision,?get_env(),?wxXmlResource_CompareVersion),
  wxe_util:rec(?wxXmlResource_CompareVersion).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceget">external documentation</a>.
-spec get() -> wxXmlResource().
get() ->
  wxe_util:queue_cmd(?get_env(), ?wxXmlResource_Get),
  wxe_util:rec(?wxXmlResource_Get).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxXmlResource().
getFlags(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_GetFlags),
  wxe_util:rec(?wxXmlResource_GetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetversion">external documentation</a>.
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
-spec initAllHandlers(This) -> 'ok' when
	This::wxXmlResource().
initAllHandlers(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,?get_env(),?wxXmlResource_InitAllHandlers).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceload">external documentation</a>.
-spec load(This, Filemask) -> boolean() when
	This::wxXmlResource(), Filemask::unicode:chardata().
load(#wx_ref{type=ThisT}=This,Filemask)
 when ?is_chardata(Filemask) ->
  ?CLASS(ThisT,wxXmlResource),
  Filemask_UC = unicode:characters_to_binary(Filemask),
  wxe_util:queue_cmd(This,Filemask_UC,?get_env(),?wxXmlResource_Load),
  wxe_util:rec(?wxXmlResource_Load).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadbitmap">external documentation</a>.
-spec loadBitmap(This, Name) -> wxBitmap:wxBitmap() when
	This::wxXmlResource(), Name::unicode:chardata().
loadBitmap(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxXmlResource_LoadBitmap),
  wxe_util:rec(?wxXmlResource_LoadBitmap).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
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
-spec loadIcon(This, Name) -> wxIcon:wxIcon() when
	This::wxXmlResource(), Name::unicode:chardata().
loadIcon(#wx_ref{type=ThisT}=This,Name)
 when ?is_chardata(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary(Name),
  wxe_util:queue_cmd(This,Name_UC,?get_env(),?wxXmlResource_LoadIcon),
  wxe_util:rec(?wxXmlResource_LoadIcon).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenu">external documentation</a>.
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
-spec set(Res) -> wxXmlResource() when
	Res::wxXmlResource().
set(#wx_ref{type=ResT}=Res) ->
  ?CLASS(ResT,wxXmlResource),
  wxe_util:queue_cmd(Res,?get_env(),?wxXmlResource_Set),
  wxe_util:rec(?wxXmlResource_Set).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcesetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxXmlResource(), Flags::integer().
setFlags(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxXmlResource_SetFlags).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceunload">external documentation</a>.
-spec unload(This, Filename) -> boolean() when
	This::wxXmlResource(), Filename::unicode:chardata().
unload(#wx_ref{type=ThisT}=This,Filename)
 when ?is_chardata(Filename) ->
  ?CLASS(ThisT,wxXmlResource),
  Filename_UC = unicode:characters_to_binary(Filename),
  wxe_util:queue_cmd(This,Filename_UC,?get_env(),?wxXmlResource_Unload),
  wxe_util:rec(?wxXmlResource_Unload).


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
-spec destroy(This::wxXmlResource()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxXmlResource),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
