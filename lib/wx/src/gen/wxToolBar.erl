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

-module(wxToolBar).
-include("wxe.hrl").
-export([addCheckTool/4,addCheckTool/5,addControl/2,addControl/3,addRadioTool/4,
  addRadioTool/5,addSeparator/1,addStretchableSpace/1,addTool/2,addTool/4,
  addTool/5,addTool/6,deleteTool/2,deleteToolByPos/2,enableTool/3,findById/2,
  findControl/2,findToolForPosition/3,getMargins/1,getToolBitmapSize/1,
  getToolEnabled/2,getToolLongHelp/2,getToolPacking/1,getToolPos/2,
  getToolSeparation/1,getToolShortHelp/2,getToolSize/1,getToolState/2,
  insertControl/3,insertControl/4,insertSeparator/2,insertStretchableSpace/2,
  insertTool/3,insertTool/5,insertTool/6,realize/1,removeTool/2,setMargins/3,
  setToolBitmapSize/2,setToolLongHelp/3,setToolPacking/2,setToolSeparation/2,
  setToolShortHelp/3,toggleTool/3]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,
  getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,
  isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popupMenu/2,
  popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,
  releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,screenToClient/2,
  scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxToolBar() :: wx:wx_object().
-export_type([wxToolBar/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv addControl(This,Control, [])
-spec addControl(This, Control) -> wx:wx_object() when
	This::wxToolBar(), Control::wxControl:wxControl().

addControl(This,Control)
 when is_record(This, wx_ref),is_record(Control, wx_ref) ->
  addControl(This,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddcontrol">external documentation</a>.
-spec addControl(This, Control, [Option]) -> wx:wx_object() when
	This::wxToolBar(), Control::wxControl:wxControl(),
	Option :: {'label', unicode:chardata()}.
addControl(#wx_ref{type=ThisT}=This,#wx_ref{type=ControlT}=Control, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ControlT,wxControl),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Control, Opts,?get_env(),?wxToolBar_AddControl),
  wxe_util:rec(?wxToolBar_AddControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddseparator">external documentation</a>.
-spec addSeparator(This) -> wx:wx_object() when
	This::wxToolBar().
addSeparator(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_AddSeparator),
  wxe_util:rec(?wxToolBar_AddSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
-spec addTool(This, Tool) -> wx:wx_object() when
	This::wxToolBar(), Tool::wx:wx_object().
addTool(#wx_ref{type=ThisT}=This,#wx_ref{type=ToolT}=Tool) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:queue_cmd(This,Tool,?get_env(),?wxToolBar_AddTool_1),
  wxe_util:rec(?wxToolBar_AddTool_1).

%% @equiv addTool(This,ToolId,Label,Bitmap, [])
-spec addTool(This, ToolId, Label, Bitmap) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap().

addTool(This,ToolId,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref) ->
  addTool(This,ToolId,Label,Bitmap, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%% <br /> Also:<br />
%% addTool(This, ToolId, Label, Bitmap, [Option]) -> wx:wx_object() when<br />
%% 	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(),<br />
%% 	Option :: {'shortHelp', unicode:chardata()}<br />
%% 		 | {'kind', wx:wx_enum()}.<br />
%% 
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec addTool(This, ToolId, Label, Bitmap, BmpDisabled) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap();
      (This, ToolId, Label, Bitmap, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(),
	Option :: {'shortHelp', unicode:chardata()}
		 | {'kind', wx:wx_enum()}.

addTool(This,ToolId,Label,Bitmap,BmpDisabled)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref),is_record(BmpDisabled, wx_ref) ->
  addTool(This,ToolId,Label,Bitmap,BmpDisabled, []);
addTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=BitmapT}=Bitmap, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({kind, _kind} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap, Opts,?get_env(),?wxToolBar_AddTool_4),
  wxe_util:rec(?wxToolBar_AddTool_4).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec addTool(This, ToolId, Label, Bitmap, BmpDisabled, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap(),
	Option :: {'kind', wx:wx_enum()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'data', wx:wx_object()}.
addTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=BitmapT}=Bitmap,#wx_ref{type=BmpDisabledT}=BmpDisabled, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(BmpDisabledT,wxBitmap),
  MOpts = fun({kind, _kind} = Arg) -> Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap,BmpDisabled, Opts,?get_env(),?wxToolBar_AddTool_5),
  wxe_util:rec(?wxToolBar_AddTool_5).

%% @equiv addCheckTool(This,ToolId,Label,Bitmap1, [])
-spec addCheckTool(This, ToolId, Label, Bitmap1) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap().

addCheckTool(This,ToolId,Label,Bitmap1)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap1, wx_ref) ->
  addCheckTool(This,ToolId,Label,Bitmap1, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddchecktool">external documentation</a>.
-spec addCheckTool(This, ToolId, Label, Bitmap1, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap(),
	Option :: {'bmpDisabled', wxBitmap:wxBitmap()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'data', wx:wx_object()}.
addCheckTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=Bitmap1T}=Bitmap1, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(Bitmap1T,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT}} = Arg) ->   ?CLASS(BmpDisabledT,wxBitmap),Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap1, Opts,?get_env(),?wxToolBar_AddCheckTool),
  wxe_util:rec(?wxToolBar_AddCheckTool).

%% @equiv addRadioTool(This,ToolId,Label,Bitmap1, [])
-spec addRadioTool(This, ToolId, Label, Bitmap1) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap().

addRadioTool(This,ToolId,Label,Bitmap1)
 when is_record(This, wx_ref),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap1, wx_ref) ->
  addRadioTool(This,ToolId,Label,Bitmap1, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddradiotool">external documentation</a>.
-spec addRadioTool(This, ToolId, Label, Bitmap1, [Option]) -> wx:wx_object() when
	This::wxToolBar(), ToolId::integer(), Label::unicode:chardata(), Bitmap1::wxBitmap:wxBitmap(),
	Option :: {'bmpDisabled', wxBitmap:wxBitmap()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'data', wx:wx_object()}.
addRadioTool(#wx_ref{type=ThisT}=This,ToolId,Label,#wx_ref{type=Bitmap1T}=Bitmap1, Options)
 when is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(Bitmap1T,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT}} = Arg) ->   ?CLASS(BmpDisabledT,wxBitmap),Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,ToolId,Label_UC,Bitmap1, Opts,?get_env(),?wxToolBar_AddRadioTool),
  wxe_util:rec(?wxToolBar_AddRadioTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbaraddstretchablespace">external documentation</a>.
-spec addStretchableSpace(This) -> wx:wx_object() when
	This::wxToolBar().
addStretchableSpace(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_AddStretchableSpace),
  wxe_util:rec(?wxToolBar_AddStretchableSpace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinsertstretchablespace">external documentation</a>.
-spec insertStretchableSpace(This, Pos) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer().
insertStretchableSpace(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_InsertStretchableSpace),
  wxe_util:rec(?wxToolBar_InsertStretchableSpace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbardeletetool">external documentation</a>.
-spec deleteTool(This, ToolId) -> boolean() when
	This::wxToolBar(), ToolId::integer().
deleteTool(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_DeleteTool),
  wxe_util:rec(?wxToolBar_DeleteTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbardeletetoolbypos">external documentation</a>.
-spec deleteToolByPos(This, Pos) -> boolean() when
	This::wxToolBar(), Pos::integer().
deleteToolByPos(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_DeleteToolByPos),
  wxe_util:rec(?wxToolBar_DeleteToolByPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarenabletool">external documentation</a>.
-spec enableTool(This, ToolId, Enable) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), Enable::boolean().
enableTool(#wx_ref{type=ThisT}=This,ToolId,Enable)
 when is_integer(ToolId),is_boolean(Enable) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,Enable,?get_env(),?wxToolBar_EnableTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarfindbyid">external documentation</a>.
-spec findById(This, Id) -> wx:wx_object() when
	This::wxToolBar(), Id::integer().
findById(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_FindById),
  wxe_util:rec(?wxToolBar_FindById).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarfindcontrol">external documentation</a>.
-spec findControl(This, Id) -> wxControl:wxControl() when
	This::wxToolBar(), Id::integer().
findControl(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_FindControl),
  wxe_util:rec(?wxToolBar_FindControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarfindtoolforposition">external documentation</a>.
-spec findToolForPosition(This, X, Y) -> wx:wx_object() when
	This::wxToolBar(), X::integer(), Y::integer().
findToolForPosition(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxToolBar_FindToolForPosition),
  wxe_util:rec(?wxToolBar_FindToolForPosition).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolsize">external documentation</a>.
-spec getToolSize(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getToolSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolSize),
  wxe_util:rec(?wxToolBar_GetToolSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolbitmapsize">external documentation</a>.
-spec getToolBitmapSize(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getToolBitmapSize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolBitmapSize),
  wxe_util:rec(?wxToolBar_GetToolBitmapSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargetmargins">external documentation</a>.
-spec getMargins(This) -> {W::integer(), H::integer()} when
	This::wxToolBar().
getMargins(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetMargins),
  wxe_util:rec(?wxToolBar_GetMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolenabled">external documentation</a>.
-spec getToolEnabled(This, ToolId) -> boolean() when
	This::wxToolBar(), ToolId::integer().
getToolEnabled(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolEnabled),
  wxe_util:rec(?wxToolBar_GetToolEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoollonghelp">external documentation</a>.
-spec getToolLongHelp(This, ToolId) -> unicode:charlist() when
	This::wxToolBar(), ToolId::integer().
getToolLongHelp(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolLongHelp),
  wxe_util:rec(?wxToolBar_GetToolLongHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolpacking">external documentation</a>.
-spec getToolPacking(This) -> integer() when
	This::wxToolBar().
getToolPacking(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolPacking),
  wxe_util:rec(?wxToolBar_GetToolPacking).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolpos">external documentation</a>.
-spec getToolPos(This, ToolId) -> integer() when
	This::wxToolBar(), ToolId::integer().
getToolPos(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolPos),
  wxe_util:rec(?wxToolBar_GetToolPos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolseparation">external documentation</a>.
-spec getToolSeparation(This) -> integer() when
	This::wxToolBar().
getToolSeparation(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_GetToolSeparation),
  wxe_util:rec(?wxToolBar_GetToolSeparation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolshorthelp">external documentation</a>.
-spec getToolShortHelp(This, ToolId) -> unicode:charlist() when
	This::wxToolBar(), ToolId::integer().
getToolShortHelp(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolShortHelp),
  wxe_util:rec(?wxToolBar_GetToolShortHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbargettoolstate">external documentation</a>.
-spec getToolState(This, ToolId) -> boolean() when
	This::wxToolBar(), ToolId::integer().
getToolState(#wx_ref{type=ThisT}=This,ToolId)
 when is_integer(ToolId) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,?get_env(),?wxToolBar_GetToolState),
  wxe_util:rec(?wxToolBar_GetToolState).

%% @equiv insertControl(This,Pos,Control, [])
-spec insertControl(This, Pos, Control) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Control::wxControl:wxControl().

insertControl(This,Pos,Control)
 when is_record(This, wx_ref),is_integer(Pos),is_record(Control, wx_ref) ->
  insertControl(This,Pos,Control, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinsertcontrol">external documentation</a>.
-spec insertControl(This, Pos, Control, [Option]) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Control::wxControl:wxControl(),
	Option :: {'label', unicode:chardata()}.
insertControl(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=ControlT}=Control, Options)
 when is_integer(Pos),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ControlT,wxControl),
  MOpts = fun({label, Label}) ->   Label_UC = unicode:characters_to_binary(Label),{label,Label_UC};
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,Control, Opts,?get_env(),?wxToolBar_InsertControl),
  wxe_util:rec(?wxToolBar_InsertControl).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinsertseparator">external documentation</a>.
-spec insertSeparator(This, Pos) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer().
insertSeparator(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxToolBar_InsertSeparator),
  wxe_util:rec(?wxToolBar_InsertSeparator).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
-spec insertTool(This, Pos, Tool) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), Tool::wx:wx_object().
insertTool(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=ToolT}=Tool)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:queue_cmd(This,Pos,Tool,?get_env(),?wxToolBar_InsertTool_2),
  wxe_util:rec(?wxToolBar_InsertTool_2).

%% @equiv insertTool(This,Pos,ToolId,Label,Bitmap, [])
-spec insertTool(This, Pos, ToolId, Label, Bitmap) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap().

insertTool(This,Pos,ToolId,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(ToolId),?is_chardata(Label),is_record(Bitmap, wx_ref) ->
  insertTool(This,Pos,ToolId,Label,Bitmap, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
%%<br /> Kind = ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_DROPDOWN | ?wxITEM_MAX
-spec insertTool(This, Pos, ToolId, Label, Bitmap, [Option]) -> wx:wx_object() when
	This::wxToolBar(), Pos::integer(), ToolId::integer(), Label::unicode:chardata(), Bitmap::wxBitmap:wxBitmap(),
	Option :: {'bmpDisabled', wxBitmap:wxBitmap()}
		 | {'kind', wx:wx_enum()}
		 | {'shortHelp', unicode:chardata()}
		 | {'longHelp', unicode:chardata()}
		 | {'clientData', wx:wx_object()}.
insertTool(#wx_ref{type=ThisT}=This,Pos,ToolId,Label,#wx_ref{type=BitmapT}=Bitmap, Options)
 when is_integer(Pos),is_integer(ToolId),?is_chardata(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary(Label),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT}} = Arg) ->   ?CLASS(BmpDisabledT,wxBitmap),Arg;
          ({kind, _kind} = Arg) -> Arg;
          ({shortHelp, ShortHelp}) ->   ShortHelp_UC = unicode:characters_to_binary(ShortHelp),{shortHelp,ShortHelp_UC};
          ({longHelp, LongHelp}) ->   LongHelp_UC = unicode:characters_to_binary(LongHelp),{longHelp,LongHelp_UC};
          ({clientData, #wx_ref{type=ClientDataT}} = Arg) ->   ?CLASS(ClientDataT,wx),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Pos,ToolId,Label_UC,Bitmap, Opts,?get_env(),?wxToolBar_InsertTool_5),
  wxe_util:rec(?wxToolBar_InsertTool_5).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarrealize">external documentation</a>.
-spec realize(This) -> boolean() when
	This::wxToolBar().
realize(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,?get_env(),?wxToolBar_Realize),
  wxe_util:rec(?wxToolBar_Realize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarremovetool">external documentation</a>.
-spec removeTool(This, Id) -> wx:wx_object() when
	This::wxToolBar(), Id::integer().
removeTool(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxToolBar_RemoveTool),
  wxe_util:rec(?wxToolBar_RemoveTool).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsetmargins">external documentation</a>.
-spec setMargins(This, X, Y) -> 'ok' when
	This::wxToolBar(), X::integer(), Y::integer().
setMargins(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxToolBar_SetMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolbitmapsize">external documentation</a>.
-spec setToolBitmapSize(This, Size) -> 'ok' when
	This::wxToolBar(), Size::{W::integer(), H::integer()}.
setToolBitmapSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxToolBar_SetToolBitmapSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoollonghelp">external documentation</a>.
-spec setToolLongHelp(This, ToolId, HelpString) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), HelpString::unicode:chardata().
setToolLongHelp(#wx_ref{type=ThisT}=This,ToolId,HelpString)
 when is_integer(ToolId),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,ToolId,HelpString_UC,?get_env(),?wxToolBar_SetToolLongHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolpacking">external documentation</a>.
-spec setToolPacking(This, Packing) -> 'ok' when
	This::wxToolBar(), Packing::integer().
setToolPacking(#wx_ref{type=ThisT}=This,Packing)
 when is_integer(Packing) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Packing,?get_env(),?wxToolBar_SetToolPacking).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolshorthelp">external documentation</a>.
-spec setToolShortHelp(This, ToolId, HelpString) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), HelpString::unicode:chardata().
setToolShortHelp(#wx_ref{type=ThisT}=This,ToolId,HelpString)
 when is_integer(ToolId),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,ToolId,HelpString_UC,?get_env(),?wxToolBar_SetToolShortHelp).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbarsettoolseparation">external documentation</a>.
-spec setToolSeparation(This, Separation) -> 'ok' when
	This::wxToolBar(), Separation::integer().
setToolSeparation(#wx_ref{type=ThisT}=This,Separation)
 when is_integer(Separation) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,Separation,?get_env(),?wxToolBar_SetToolSeparation).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxtoolbar.html#wxtoolbartoggletool">external documentation</a>.
-spec toggleTool(This, ToolId, Toggle) -> 'ok' when
	This::wxToolBar(), ToolId::integer(), Toggle::boolean().
toggleTool(#wx_ref{type=ThisT}=This,ToolId,Toggle)
 when is_integer(ToolId),is_boolean(Toggle) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:queue_cmd(This,ToolId,Toggle,?get_env(),?wxToolBar_ToggleTool).

 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
 %% From wxWindow
%% @hidden
getDPI(This) -> wxWindow:getDPI(This).
%% @hidden
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
%% @hidden
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
%% @hidden
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
%% @hidden
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
%% @hidden
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
%% @hidden
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
%% @hidden
validate(This) -> wxWindow:validate(This).
%% @hidden
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
%% @hidden
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
%% @hidden
update(This) -> wxWindow:update(This).
%% @hidden
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
%% @hidden
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
%% @hidden
thaw(This) -> wxWindow:thaw(This).
%% @hidden
show(This, Options) -> wxWindow:show(This, Options).
%% @hidden
show(This) -> wxWindow:show(This).
%% @hidden
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
%% @hidden
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
%% @hidden
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
%% @hidden
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
%% @hidden
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
%% @hidden
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
%% @hidden
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
%% @hidden
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
%% @hidden
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
%% @hidden
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
%% @hidden
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
%% @hidden
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
%% @hidden
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
%% @hidden
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
%% @hidden
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
%% @hidden
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
%% @hidden
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
%% @hidden
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
%% @hidden
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
%% @hidden
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
%% @hidden
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
%% @hidden
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
%% @hidden
setFont(This,Font) -> wxWindow:setFont(This,Font).
%% @hidden
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
%% @hidden
setFocus(This) -> wxWindow:setFocus(This).
%% @hidden
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
%% @hidden
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
%% @hidden
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
%% @hidden
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
%% @hidden
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
%% @hidden
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
%% @hidden
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
%% @hidden
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
%% @hidden
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
%% @hidden
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
%% @hidden
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
%% @hidden
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
%% @hidden
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
%% @hidden
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
%% @hidden
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
%% @hidden
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
%% @hidden
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
%% @hidden
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
%% @hidden
screenToClient(This) -> wxWindow:screenToClient(This).
%% @hidden
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
%% @hidden
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
%% @hidden
releaseMouse(This) -> wxWindow:releaseMouse(This).
%% @hidden
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
%% @hidden
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
%% @hidden
refresh(This, Options) -> wxWindow:refresh(This, Options).
%% @hidden
refresh(This) -> wxWindow:refresh(This).
%% @hidden
raise(This) -> wxWindow:raise(This).
%% @hidden
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
%% @hidden
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
%% @hidden
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
%% @hidden
pageUp(This) -> wxWindow:pageUp(This).
%% @hidden
pageDown(This) -> wxWindow:pageDown(This).
%% @hidden
navigate(This, Options) -> wxWindow:navigate(This, Options).
%% @hidden
navigate(This) -> wxWindow:navigate(This).
%% @hidden
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
%% @hidden
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
%% @hidden
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
%% @hidden
move(This,X,Y) -> wxWindow:move(This,X,Y).
%% @hidden
move(This,Pt) -> wxWindow:move(This,Pt).
%% @hidden
lower(This) -> wxWindow:lower(This).
%% @hidden
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
layout(This) -> wxWindow:layout(This).
%% @hidden
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
%% @hidden
isTopLevel(This) -> wxWindow:isTopLevel(This).
%% @hidden
isShown(This) -> wxWindow:isShown(This).
%% @hidden
isRetained(This) -> wxWindow:isRetained(This).
%% @hidden
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
%% @hidden
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
%% @hidden
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
%% @hidden
isEnabled(This) -> wxWindow:isEnabled(This).
%% @hidden
isFrozen(This) -> wxWindow:isFrozen(This).
%% @hidden
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
%% @hidden
initDialog(This) -> wxWindow:initDialog(This).
%% @hidden
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
%% @hidden
hide(This) -> wxWindow:hide(This).
%% @hidden
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
%% @hidden
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
%% @hidden
hasCapture(This) -> wxWindow:hasCapture(This).
%% @hidden
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
%% @hidden
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
%% @hidden
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
%% @hidden
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
%% @hidden
getToolTip(This) -> wxWindow:getToolTip(This).
%% @hidden
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
%% @hidden
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
getSize(This) -> wxWindow:getSize(This).
%% @hidden
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
%% @hidden
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
%% @hidden
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
%% @hidden
getScreenRect(This) -> wxWindow:getScreenRect(This).
%% @hidden
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
%% @hidden
getRect(This) -> wxWindow:getRect(This).
%% @hidden
getPosition(This) -> wxWindow:getPosition(This).
%% @hidden
getParent(This) -> wxWindow:getParent(This).
%% @hidden
getName(This) -> wxWindow:getName(This).
%% @hidden
getMinSize(This) -> wxWindow:getMinSize(This).
%% @hidden
getMaxSize(This) -> wxWindow:getMaxSize(This).
%% @hidden
getId(This) -> wxWindow:getId(This).
%% @hidden
getHelpText(This) -> wxWindow:getHelpText(This).
%% @hidden
getHandle(This) -> wxWindow:getHandle(This).
%% @hidden
getGrandParent(This) -> wxWindow:getGrandParent(This).
%% @hidden
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
%% @hidden
getFont(This) -> wxWindow:getFont(This).
%% @hidden
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
%% @hidden
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
%% @hidden
getDropTarget(This) -> wxWindow:getDropTarget(This).
%% @hidden
getCursor(This) -> wxWindow:getCursor(This).
%% @hidden
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
%% @hidden
getClientSize(This) -> wxWindow:getClientSize(This).
%% @hidden
getChildren(This) -> wxWindow:getChildren(This).
%% @hidden
getCharWidth(This) -> wxWindow:getCharWidth(This).
%% @hidden
getCharHeight(This) -> wxWindow:getCharHeight(This).
%% @hidden
getCaret(This) -> wxWindow:getCaret(This).
%% @hidden
getBestSize(This) -> wxWindow:getBestSize(This).
%% @hidden
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
%% @hidden
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
%% @hidden
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
%% @hidden
freeze(This) -> wxWindow:freeze(This).
%% @hidden
fitInside(This) -> wxWindow:fitInside(This).
%% @hidden
fit(This) -> wxWindow:fit(This).
%% @hidden
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
%% @hidden
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
enable(This) -> wxWindow:enable(This).
%% @hidden
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
%% @hidden
disable(This) -> wxWindow:disable(This).
%% @hidden
destroyChildren(This) -> wxWindow:destroyChildren(This).
%% @hidden
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
%% @hidden
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
%% @hidden
close(This, Options) -> wxWindow:close(This, Options).
%% @hidden
close(This) -> wxWindow:close(This).
%% @hidden
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
%% @hidden
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
%% @hidden
clearBackground(This) -> wxWindow:clearBackground(This).
%% @hidden
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
%% @hidden
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
centre(This) -> wxWindow:centre(This).
%% @hidden
center(This) -> wxWindow:center(This).
%% @hidden
captureMouse(This) -> wxWindow:captureMouse(This).
%% @hidden
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
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
