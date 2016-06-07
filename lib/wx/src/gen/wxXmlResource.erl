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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html">wxXmlResource</a>.
%% @type wxXmlResource().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

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

-export_type([wxXmlResource/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxXmlResource() :: wx:wx_object().
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
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          ({domain, Domain}, Acc) ->   Domain_UC = unicode:characters_to_binary([Domain,0]),[<<2:32/?UI,(byte_size(Domain_UC)):32/?UI,(Domain_UC)/binary, 0:(((8- ((0+byte_size(Domain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxXmlResource_new_1,
  <<BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
-spec new(Filemask, [Option]) -> wxXmlResource() when
	Filemask::unicode:chardata(),
	Option :: {'flags', integer()}
		 | {'domain', unicode:chardata()}.
new(Filemask, Options)
 when is_list(Filemask),is_list(Options) ->
  Filemask_UC = unicode:characters_to_binary([Filemask,0]),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          ({domain, Domain}, Acc) ->   Domain_UC = unicode:characters_to_binary([Domain,0]),[<<2:32/?UI,(byte_size(Domain_UC)):32/?UI,(Domain_UC)/binary, 0:(((8- ((0+byte_size(Domain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxXmlResource_new_2,
  <<(byte_size(Filemask_UC)):32/?UI,(Filemask_UC)/binary, 0:(((8- ((4+byte_size(Filemask_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv attachUnknownControl(This,Name,Control, [])
-spec attachUnknownControl(This, Name, Control) -> boolean() when
	This::wxXmlResource(), Name::unicode:chardata(), Control::wxWindow:wxWindow().

attachUnknownControl(This,Name,Control)
 when is_record(This, wx_ref),is_list(Name),is_record(Control, wx_ref) ->
  attachUnknownControl(This,Name,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceattachunknowncontrol">external documentation</a>.
-spec attachUnknownControl(This, Name, Control, [Option]) -> boolean() when
	This::wxXmlResource(), Name::unicode:chardata(), Control::wxWindow:wxWindow(),
	Option :: {'parent', wxWindow:wxWindow()}.
attachUnknownControl(#wx_ref{type=ThisT,ref=ThisRef},Name,#wx_ref{type=ControlT,ref=ControlRef}, Options)
 when is_list(Name),is_list(Options) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  ?CLASS(ControlT,wxWindow),
  MOpts = fun({parent, #wx_ref{type=ParentT,ref=ParentRef}}, Acc) ->   ?CLASS(ParentT,wxWindow),[<<1:32/?UI,ParentRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxXmlResource_AttachUnknownControl,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8,ControlRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceclearhandlers">external documentation</a>.
-spec clearHandlers(This) -> 'ok' when
	This::wxXmlResource().
clearHandlers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_ClearHandlers,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcecompareversion">external documentation</a>.
-spec compareVersion(This, Major, Minor, Release, Revision) -> integer() when
	This::wxXmlResource(), Major::integer(), Minor::integer(), Release::integer(), Revision::integer().
compareVersion(#wx_ref{type=ThisT,ref=ThisRef},Major,Minor,Release,Revision)
 when is_integer(Major),is_integer(Minor),is_integer(Release),is_integer(Revision) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_CompareVersion,
  <<ThisRef:32/?UI,Major:32/?UI,Minor:32/?UI,Release:32/?UI,Revision:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceget">external documentation</a>.
-spec get() -> wxXmlResource().
get() ->
  wxe_util:call(?wxXmlResource_Get,
  <<>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetflags">external documentation</a>.
-spec getFlags(This) -> integer() when
	This::wxXmlResource().
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_GetFlags,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetversion">external documentation</a>.
-spec getVersion(This) -> integer() when
	This::wxXmlResource().
getVersion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_GetVersion,
  <<ThisRef:32/?UI>>).

%% @equiv getXRCID(Str_id, [])
-spec getXRCID(Str_id) -> integer() when
	Str_id::[unicode:chardata()].

getXRCID(Str_id)
 when is_list(Str_id) ->
  getXRCID(Str_id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcegetxrcid">external documentation</a>.
-spec getXRCID(Str_id, [Option]) -> integer() when
	Str_id::[unicode:chardata()],
	Option :: {'value_if_not_found', integer()}.
getXRCID(Str_id, Options)
 when is_list(Str_id),is_list(Options) ->
  Str_id_UC = unicode:characters_to_binary([Str_id,0]),
  MOpts = fun({value_if_not_found, Value_if_not_found}, Acc) -> [<<1:32/?UI,Value_if_not_found:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxXmlResource_GetXRCID,
  <<(byte_size(Str_id_UC)):32/?UI,(Str_id_UC)/binary, 0:(((8- ((4+byte_size(Str_id_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceinitallhandlers">external documentation</a>.
-spec initAllHandlers(This) -> 'ok' when
	This::wxXmlResource().
initAllHandlers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_InitAllHandlers,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceload">external documentation</a>.
-spec load(This, Filemask) -> boolean() when
	This::wxXmlResource(), Filemask::unicode:chardata().
load(#wx_ref{type=ThisT,ref=ThisRef},Filemask)
 when is_list(Filemask) ->
  ?CLASS(ThisT,wxXmlResource),
  Filemask_UC = unicode:characters_to_binary([Filemask,0]),
  wxe_util:call(?wxXmlResource_Load,
  <<ThisRef:32/?UI,(byte_size(Filemask_UC)):32/?UI,(Filemask_UC)/binary, 0:(((8- ((0+byte_size(Filemask_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadbitmap">external documentation</a>.
-spec loadBitmap(This, Name) -> wxBitmap:wxBitmap() when
	This::wxXmlResource(), Name::unicode:chardata().
loadBitmap(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadBitmap,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
-spec loadDialog(This, Parent, Name) -> wxDialog:wxDialog() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadDialog_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
-spec loadDialog(This, Dlg, Parent, Name) -> boolean() when
	This::wxXmlResource(), Dlg::wxDialog:wxDialog(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DlgT,ref=DlgRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(DlgT,wxDialog),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadDialog_3,
  <<ThisRef:32/?UI,DlgRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
-spec loadFrame(This, Parent, Name) -> wxFrame:wxFrame() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadFrame_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
-spec loadFrame(This, Frame, Parent, Name) -> boolean() when
	This::wxXmlResource(), Frame::wxFrame:wxFrame(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(FrameT,wxFrame),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadFrame_3,
  <<ThisRef:32/?UI,FrameRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadicon">external documentation</a>.
-spec loadIcon(This, Name) -> wxIcon:wxIcon() when
	This::wxXmlResource(), Name::unicode:chardata().
loadIcon(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadIcon,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenu">external documentation</a>.
-spec loadMenu(This, Name) -> wxMenu:wxMenu() when
	This::wxXmlResource(), Name::unicode:chardata().
loadMenu(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenu,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
-spec loadMenuBar(This, Name) -> wxMenuBar:wxMenuBar() when
	This::wxXmlResource(), Name::unicode:chardata().
loadMenuBar(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenuBar_1,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
-spec loadMenuBar(This, Parent, Name) -> wxMenuBar:wxMenuBar() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadMenuBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenuBar_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
-spec loadPanel(This, Parent, Name) -> wxPanel:wxPanel() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadPanel(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadPanel_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
-spec loadPanel(This, Panel, Parent, Name) -> boolean() when
	This::wxXmlResource(), Panel::wxPanel:wxPanel(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadPanel(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PanelT,ref=PanelRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(PanelT,wxPanel),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadPanel_3,
  <<ThisRef:32/?UI,PanelRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceloadtoolbar">external documentation</a>.
-spec loadToolBar(This, Parent, Name) -> wxToolBar:wxToolBar() when
	This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::unicode:chardata().
loadToolBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadToolBar,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceset">external documentation</a>.
-spec set(Res) -> wxXmlResource() when
	Res::wxXmlResource().
set(#wx_ref{type=ResT,ref=ResRef}) ->
  ?CLASS(ResT,wxXmlResource),
  wxe_util:call(?wxXmlResource_Set,
  <<ResRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourcesetflags">external documentation</a>.
-spec setFlags(This, Flags) -> 'ok' when
	This::wxXmlResource(), Flags::integer().
setFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_SetFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxxmlresource.html#wxxmlresourceunload">external documentation</a>.
-spec unload(This, Filename) -> boolean() when
	This::wxXmlResource(), Filename::unicode:chardata().
unload(#wx_ref{type=ThisT,ref=ThisRef},Filename)
 when is_list(Filename) ->
  ?CLASS(ThisT,wxXmlResource),
  Filename_UC = unicode:characters_to_binary([Filename,0]),
  wxe_util:call(?wxXmlResource_Unload,
  <<ThisRef:32/?UI,(byte_size(Filename_UC)):32/?UI,(Filename_UC)/binary, 0:(((8- ((0+byte_size(Filename_UC)) band 16#7)) band 16#7))/unit:8>>).


%% @spec (Window::wxWindow:wxWindow(),Name::string(), Type::atom()) -> wx:wxObject()

%% @doc Looks up a control with Name in a window created with XML
%% resources. You can use it to set/get values from controls.
%% The object is type casted to <b>Type</b>.
%% Example: <br />
%%  Xrc = wxXmlResource:get(), <br />
%%  Dlg = wxDialog:new(), <br />
%%  true = wxXmlResource:loadDialog(Xrc, Dlg, Frame, "controls_dialog"), <br />
%%  LCtrl = xrcctrl(Dlg, "controls_listctrl", wxListCtrl), <br />
%%  wxListCtrl:insertColumn(LCtrl, 0, "Name", [{width, 200}]), <br />

xrcctrl(Window = #wx_ref{}, Name, Type) when is_list(Name), is_atom(Type) ->
    %% Func Id ?wxXmlResource_xrcctrl 
    ID  = wxXmlResource:getXRCID(Name),
    Res = wxWindow:findWindow(Window,ID),
    wx:typeCast(Res, Type).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxXmlResource()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxXmlResource),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
