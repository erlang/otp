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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html">wxXmlResource</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec () -> wxXmlResource()
%% @equiv new([])
new() ->
  new([]).

%% @spec ([Option]) -> wxXmlResource()
%% Option = {flags, integer()} | {domain, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
new(Options)
 when is_list(Options) ->
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          ({domain, Domain}, Acc) ->   Domain_UC = unicode:characters_to_binary([Domain,0]),[<<2:32/?UI,(byte_size(Domain_UC)):32/?UI,(Domain_UC)/binary, 0:(((8- ((0+byte_size(Domain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxXmlResource_new_1,
  <<BinOpt/binary>>).

%% @spec (Filemask::string(), [Option]) -> wxXmlResource()
%% Option = {flags, integer()} | {domain, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcewxxmlresource">external documentation</a>.
new(Filemask, Options)
 when is_list(Filemask),is_list(Options) ->
  Filemask_UC = unicode:characters_to_binary([Filemask,0]),
  MOpts = fun({flags, Flags}, Acc) -> [<<1:32/?UI,Flags:32/?UI>>|Acc];
          ({domain, Domain}, Acc) ->   Domain_UC = unicode:characters_to_binary([Domain,0]),[<<2:32/?UI,(byte_size(Domain_UC)):32/?UI,(Domain_UC)/binary, 0:(((8- ((0+byte_size(Domain_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxXmlResource_new_2,
  <<(byte_size(Filemask_UC)):32/?UI,(Filemask_UC)/binary, 0:(((8- ((4+byte_size(Filemask_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxXmlResource(), Name::string(), Control::wxWindow:wxWindow()) -> bool()
%% @equiv attachUnknownControl(This,Name,Control, [])
attachUnknownControl(This,Name,Control)
 when is_record(This, wx_ref),is_list(Name),is_record(Control, wx_ref) ->
  attachUnknownControl(This,Name,Control, []).

%% @spec (This::wxXmlResource(), Name::string(), Control::wxWindow:wxWindow(), [Option]) -> bool()
%% Option = {parent, wxWindow:wxWindow()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceattachunknowncontrol">external documentation</a>.
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

%% @spec (This::wxXmlResource()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceclearhandlers">external documentation</a>.
clearHandlers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_ClearHandlers,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxXmlResource(), Major::integer(), Minor::integer(), Release::integer(), Revision::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcecompareversion">external documentation</a>.
compareVersion(#wx_ref{type=ThisT,ref=ThisRef},Major,Minor,Release,Revision)
 when is_integer(Major),is_integer(Minor),is_integer(Release),is_integer(Revision) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_CompareVersion,
  <<ThisRef:32/?UI,Major:32/?UI,Minor:32/?UI,Release:32/?UI,Revision:32/?UI>>).

%% @spec () -> wxXmlResource()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceget">external documentation</a>.
get() ->
  wxe_util:call(?wxXmlResource_Get,
  <<>>).

%% @spec (This::wxXmlResource()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcegetflags">external documentation</a>.
getFlags(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_GetFlags,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxXmlResource()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcegetversion">external documentation</a>.
getVersion(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:call(?wxXmlResource_GetVersion,
  <<ThisRef:32/?UI>>).

%% @spec (Str_id::[string()]) -> integer()
%% @equiv getXRCID(Str_id, [])
getXRCID(Str_id)
 when is_list(Str_id) ->
  getXRCID(Str_id, []).

%% @spec (Str_id::[string()], [Option]) -> integer()
%% Option = {value_if_not_found, integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcegetxrcid">external documentation</a>.
getXRCID(Str_id, Options)
 when is_list(Str_id),is_list(Options) ->
  Str_id_UC = unicode:characters_to_binary([Str_id,0]),
  MOpts = fun({value_if_not_found, Value_if_not_found}, Acc) -> [<<1:32/?UI,Value_if_not_found:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxXmlResource_GetXRCID,
  <<(byte_size(Str_id_UC)):32/?UI,(Str_id_UC)/binary, 0:(((8- ((4+byte_size(Str_id_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @spec (This::wxXmlResource()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceinitallhandlers">external documentation</a>.
initAllHandlers(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_InitAllHandlers,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxXmlResource(), Filemask::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceload">external documentation</a>.
load(#wx_ref{type=ThisT,ref=ThisRef},Filemask)
 when is_list(Filemask) ->
  ?CLASS(ThisT,wxXmlResource),
  Filemask_UC = unicode:characters_to_binary([Filemask,0]),
  wxe_util:call(?wxXmlResource_Load,
  <<ThisRef:32/?UI,(byte_size(Filemask_UC)):32/?UI,(Filemask_UC)/binary, 0:(((8- ((0+byte_size(Filemask_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Name::string()) -> wxBitmap:wxBitmap()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadbitmap">external documentation</a>.
loadBitmap(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadBitmap,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::string()) -> wxDialog:wxDialog()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
loadDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadDialog_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Dlg::wxDialog:wxDialog(), Parent::wxWindow:wxWindow(), Name::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloaddialog">external documentation</a>.
loadDialog(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=DlgT,ref=DlgRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(DlgT,wxDialog),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadDialog_3,
  <<ThisRef:32/?UI,DlgRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::string()) -> wxFrame:wxFrame()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
loadFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadFrame_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Frame::wxFrame:wxFrame(), Parent::wxWindow:wxWindow(), Name::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadframe">external documentation</a>.
loadFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(FrameT,wxFrame),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadFrame_3,
  <<ThisRef:32/?UI,FrameRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Name::string()) -> wxIcon:wxIcon()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadicon">external documentation</a>.
loadIcon(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadIcon,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Name::string()) -> wxMenu:wxMenu()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadmenu">external documentation</a>.
loadMenu(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenu,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Name::string()) -> wxMenuBar:wxMenuBar()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
loadMenuBar(#wx_ref{type=ThisT,ref=ThisRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenuBar_1,
  <<ThisRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::string()) -> wxMenuBar:wxMenuBar()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadmenubar">external documentation</a>.
loadMenuBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadMenuBar_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::string()) -> wxPanel:wxPanel()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
loadPanel(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadPanel_2,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Panel::wxPanel:wxPanel(), Parent::wxWindow:wxWindow(), Name::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadpanel">external documentation</a>.
loadPanel(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PanelT,ref=PanelRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(PanelT,wxPanel),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadPanel_3,
  <<ThisRef:32/?UI,PanelRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((0+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxXmlResource(), Parent::wxWindow:wxWindow(), Name::string()) -> wxToolBar:wxToolBar()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceloadtoolbar">external documentation</a>.
loadToolBar(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef},Name)
 when is_list(Name) ->
  ?CLASS(ThisT,wxXmlResource),
  ?CLASS(ParentT,wxWindow),
  Name_UC = unicode:characters_to_binary([Name,0]),
  wxe_util:call(?wxXmlResource_LoadToolBar,
  <<ThisRef:32/?UI,ParentRef:32/?UI,(byte_size(Name_UC)):32/?UI,(Name_UC)/binary, 0:(((8- ((4+byte_size(Name_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (Res::wxXmlResource()) -> wxXmlResource()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceset">external documentation</a>.
set(#wx_ref{type=ResT,ref=ResRef}) ->
  ?CLASS(ResT,wxXmlResource),
  wxe_util:call(?wxXmlResource_Set,
  <<ResRef:32/?UI>>).

%% @spec (This::wxXmlResource(), Flags::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourcesetflags">external documentation</a>.
setFlags(#wx_ref{type=ThisT,ref=ThisRef},Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxXmlResource),
  wxe_util:cast(?wxXmlResource_SetFlags,
  <<ThisRef:32/?UI,Flags:32/?UI>>).

%% @spec (This::wxXmlResource(), Filename::string()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxxmlresource.html#wxxmlresourceunload">external documentation</a>.
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

%% @spec (This::wxXmlResource()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxXmlResource),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
