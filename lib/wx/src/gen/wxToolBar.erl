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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html">wxToolBar</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxControl}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxToolBar().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxToolBar).
-include("wxe.hrl").
-export([addCheckTool/4,addCheckTool/5,addControl/2,addRadioTool/4,addRadioTool/5,
  addSeparator/1,addTool/2,addTool/3,addTool/4,addTool/5,addTool/6,addTool/7,
  deleteTool/2,deleteToolByPos/2,enableTool/3,findById/2,findControl/2,
  findToolForPosition/3,getMargins/1,getToolBitmapSize/1,getToolEnabled/2,
  getToolLongHelp/2,getToolPacking/1,getToolPos/2,getToolSeparation/1,
  getToolShortHelp/2,getToolSize/1,getToolState/2,insertControl/3,insertSeparator/2,
  insertTool/3,insertTool/4,insertTool/5,insertTool/6,realize/1,removeTool/2,
  setMargins/3,setToolBitmapSize/2,setToolLongHelp/3,setToolPacking/2,
  setToolSeparation/2,setToolShortHelp/3,toggleTool/3]).

%% inherited exports
-export([cacheBestSize/2,captureMouse/1,center/1,center/2,centerOnParent/1,
  centerOnParent/2,centre/1,centre/2,centreOnParent/1,centreOnParent/2,
  clearBackground/1,clientToScreen/2,clientToScreen/3,close/1,close/2,
  connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  enable/1,enable/2,findWindow/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
  getBackgroundColour/1,getBackgroundStyle/1,getBestSize/1,getCaret/1,
  getCharHeight/1,getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getCursor/1,getDropTarget/1,getEventHandler/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,
  getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isRetained/1,isShown/1,isTopLevel/1,
  layout/1,lineDown/1,lineUp/1,lower/1,makeModal/1,makeModal/2,move/2,
  move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,navigate/1,
  navigate/2,pageDown/1,pageUp/1,parent_class/1,popEventHandler/1,popEventHandler/2,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,
  setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,setFont/2,
  setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setVirtualSize/2,setVirtualSize/3,
  setVirtualSizeHints/2,setVirtualSizeHints/3,setVirtualSizeHints/4,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (This::wxToolBar(), Control::wxControl:wxControl()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddcontrol">external documentation</a>.
addControl(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ControlT,ref=ControlRef}) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ControlT,wxControl),
  wxe_util:call(?wxToolBar_AddControl,
  <<ThisRef:32/?UI,ControlRef:32/?UI>>).

%% @spec (This::wxToolBar()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddseparator">external documentation</a>.
addSeparator(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_AddSeparator,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar(), Tool::wx:wx()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
addTool(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ToolT,ref=ToolRef}) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:call(?wxToolBar_AddTool_1,
  <<ThisRef:32/?UI,ToolRef:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap()) -> wx:wx()
%% @equiv addTool(This,Toolid,Bitmap, [])
addTool(This,Toolid,Bitmap)
 when is_record(This, wx_ref),is_integer(Toolid),is_record(Bitmap, wx_ref) ->
  addTool(This,Toolid,Bitmap, []).

%% @spec (This::wxToolBar(),Toolid::integer(),X::string()|term(),X::term()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap()) -> addTool(This,Toolid,Label,Bitmap, []) </c></p>
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap()) -> addTool(This,Toolid,Bitmap,BmpDisabled, []) </c></p>
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap(), [Option]) -> wx:wx() </c>
%%<br /> Option = {shortHelpString, string()} | {longHelpString, string()}
%% </p>

addTool(This,Toolid,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Toolid),is_list(Label),is_record(Bitmap, wx_ref) ->
  addTool(This,Toolid,Label,Bitmap, []);

addTool(This,Toolid,Bitmap,BmpDisabled)
 when is_record(This, wx_ref),is_integer(Toolid),is_record(Bitmap, wx_ref),is_record(BmpDisabled, wx_ref) ->
  addTool(This,Toolid,Bitmap,BmpDisabled, []);
addTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,#wx_ref{type=BitmapT,ref=BitmapRef}, Options)
 when is_integer(Toolid),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({shortHelpString, ShortHelpString}, Acc) ->   ShortHelpString_UC = unicode:characters_to_binary([ShortHelpString,0]),[<<1:32/?UI,(byte_size(ShortHelpString_UC)):32/?UI,(ShortHelpString_UC)/binary, 0:(((8- ((0+byte_size(ShortHelpString_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelpString, LongHelpString}, Acc) ->   LongHelpString_UC = unicode:characters_to_binary([LongHelpString,0]),[<<2:32/?UI,(byte_size(LongHelpString_UC)):32/?UI,(LongHelpString_UC)/binary, 0:(((8- ((0+byte_size(LongHelpString_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddTool_3,
  <<ThisRef:32/?UI,Toolid:32/?UI,BitmapRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxToolBar(),Toolid::integer(),X::string()|term(),X::term(),X::term()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap()) -> addTool(This,Toolid,Label,Bitmap,BmpDisabled, []) </c></p>
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap(), [Option]) -> wx:wx() </c>
%%<br /> Option = {shortHelp, string()} | {kind, WxItemKind}
%%<br /> WxItemKind = integer()
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
%% </p>
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap(), [Option]) -> wx:wx() </c>
%%<br /> Option = {toggle, bool()} | {clientData, wx:wx()} | {shortHelpString, string()} | {longHelpString, string()}
%% </p>

addTool(This,Toolid,Label,Bitmap,BmpDisabled)
 when is_record(This, wx_ref),is_integer(Toolid),is_list(Label),is_record(Bitmap, wx_ref),is_record(BmpDisabled, wx_ref) ->
  addTool(This,Toolid,Label,Bitmap,BmpDisabled, []);
addTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,Label,#wx_ref{type=BitmapT,ref=BitmapRef}, Options)
 when is_integer(Toolid),is_list(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary([Label,0]),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<1:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({kind, Kind}, Acc) -> [<<2:32/?UI,Kind:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddTool_4_0,
  <<ThisRef:32/?UI,Toolid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8,BitmapRef:32/?UI, 0:32,BinOpt/binary>>);
addTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=BmpDisabledT,ref=BmpDisabledRef}, Options)
 when is_integer(Toolid),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(BmpDisabledT,wxBitmap),
  MOpts = fun({toggle, Toggle}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Toggle)):32/?UI>>|Acc];
          ({clientData, #wx_ref{type=ClientDataT,ref=ClientDataRef}}, Acc) ->   ?CLASS(ClientDataT,wx),[<<2:32/?UI,ClientDataRef:32/?UI>>|Acc];
          ({shortHelpString, ShortHelpString}, Acc) ->   ShortHelpString_UC = unicode:characters_to_binary([ShortHelpString,0]),[<<3:32/?UI,(byte_size(ShortHelpString_UC)):32/?UI,(ShortHelpString_UC)/binary, 0:(((8- ((0+byte_size(ShortHelpString_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelpString, LongHelpString}, Acc) ->   LongHelpString_UC = unicode:characters_to_binary([LongHelpString,0]),[<<4:32/?UI,(byte_size(LongHelpString_UC)):32/?UI,(LongHelpString_UC)/binary, 0:(((8- ((0+byte_size(LongHelpString_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddTool_4_1,
  <<ThisRef:32/?UI,Toolid:32/?UI,BitmapRef:32/?UI,BmpDisabledRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxToolBar(),Toolid::integer(),X::term()|string(),X::term(),X::bool()|term(),X::integer()|term()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap(), Toggle::bool(), XPos::integer()) -> addTool(This,Toolid,Bitmap,BmpDisabled,Toggle,XPos, []) </c></p>
%% <p><c>
%% addTool(This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap(), [Option]) -> wx:wx() </c>
%%<br /> Option = {kind, WxItemKind} | {shortHelp, string()} | {longHelp, string()} | {data, wx:wx()}
%%<br /> WxItemKind = integer()
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
%% </p>

addTool(This,Toolid,Bitmap,BmpDisabled,Toggle,XPos)
 when is_record(This, wx_ref),is_integer(Toolid),is_record(Bitmap, wx_ref),is_record(BmpDisabled, wx_ref),is_boolean(Toggle),is_integer(XPos) ->
  addTool(This,Toolid,Bitmap,BmpDisabled,Toggle,XPos, []);
addTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,Label,#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=BmpDisabledT,ref=BmpDisabledRef}, Options)
 when is_integer(Toolid),is_list(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary([Label,0]),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(BmpDisabledT,wxBitmap),
  MOpts = fun({kind, Kind}, Acc) -> [<<1:32/?UI,Kind:32/?UI>>|Acc];
          ({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<2:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelp, LongHelp}, Acc) ->   LongHelp_UC = unicode:characters_to_binary([LongHelp,0]),[<<3:32/?UI,(byte_size(LongHelp_UC)):32/?UI,(LongHelp_UC)/binary, 0:(((8- ((0+byte_size(LongHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wx),[<<4:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddTool_5,
  <<ThisRef:32/?UI,Toolid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8,BitmapRef:32/?UI,BmpDisabledRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxToolBar(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap(), BmpDisabled::wxBitmap:wxBitmap(), Toggle::bool(), XPos::integer(), [Option]) -> wx:wx()
%% Option = {yPos, integer()} | {clientData, wx:wx()} | {shortHelp, string()} | {longHelp, string()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddtool">external documentation</a>.
addTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,#wx_ref{type=BitmapT,ref=BitmapRef},#wx_ref{type=BmpDisabledT,ref=BmpDisabledRef},Toggle,XPos, Options)
 when is_integer(Toolid),is_boolean(Toggle),is_integer(XPos),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(BitmapT,wxBitmap),
  ?CLASS(BmpDisabledT,wxBitmap),
  MOpts = fun({yPos, YPos}, Acc) -> [<<1:32/?UI,YPos:32/?UI>>|Acc];
          ({clientData, #wx_ref{type=ClientDataT,ref=ClientDataRef}}, Acc) ->   ?CLASS(ClientDataT,wx),[<<2:32/?UI,ClientDataRef:32/?UI>>|Acc];
          ({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<3:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelp, LongHelp}, Acc) ->   LongHelp_UC = unicode:characters_to_binary([LongHelp,0]),[<<4:32/?UI,(byte_size(LongHelp_UC)):32/?UI,(LongHelp_UC)/binary, 0:(((8- ((0+byte_size(LongHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddTool_6,
  <<ThisRef:32/?UI,Toolid:32/?UI,BitmapRef:32/?UI,BmpDisabledRef:32/?UI,(wxe_util:from_bool(Toggle)):32/?UI,XPos:32/?UI, BinOpt/binary>>).

%% @spec (This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap()) -> wx:wx()
%% @equiv addCheckTool(This,Toolid,Label,Bitmap, [])
addCheckTool(This,Toolid,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Toolid),is_list(Label),is_record(Bitmap, wx_ref) ->
  addCheckTool(This,Toolid,Label,Bitmap, []).

%% @spec (This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap(), [Option]) -> wx:wx()
%% Option = {bmpDisabled, wxBitmap:wxBitmap()} | {shortHelp, string()} | {longHelp, string()} | {data, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddchecktool">external documentation</a>.
addCheckTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,Label,#wx_ref{type=BitmapT,ref=BitmapRef}, Options)
 when is_integer(Toolid),is_list(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary([Label,0]),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT,ref=BmpDisabledRef}}, Acc) ->   ?CLASS(BmpDisabledT,wxBitmap),[<<1:32/?UI,BmpDisabledRef:32/?UI>>|Acc];
          ({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<2:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelp, LongHelp}, Acc) ->   LongHelp_UC = unicode:characters_to_binary([LongHelp,0]),[<<3:32/?UI,(byte_size(LongHelp_UC)):32/?UI,(LongHelp_UC)/binary, 0:(((8- ((0+byte_size(LongHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wx),[<<4:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddCheckTool,
  <<ThisRef:32/?UI,Toolid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8,BitmapRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap()) -> wx:wx()
%% @equiv addRadioTool(This,Toolid,Label,Bitmap, [])
addRadioTool(This,Toolid,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Toolid),is_list(Label),is_record(Bitmap, wx_ref) ->
  addRadioTool(This,Toolid,Label,Bitmap, []).

%% @spec (This::wxToolBar(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap(), [Option]) -> wx:wx()
%% Option = {bmpDisabled, wxBitmap:wxBitmap()} | {shortHelp, string()} | {longHelp, string()} | {data, wx:wx()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbaraddradiotool">external documentation</a>.
addRadioTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,Label,#wx_ref{type=BitmapT,ref=BitmapRef}, Options)
 when is_integer(Toolid),is_list(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary([Label,0]),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT,ref=BmpDisabledRef}}, Acc) ->   ?CLASS(BmpDisabledT,wxBitmap),[<<1:32/?UI,BmpDisabledRef:32/?UI>>|Acc];
          ({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<2:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelp, LongHelp}, Acc) ->   LongHelp_UC = unicode:characters_to_binary([LongHelp,0]),[<<3:32/?UI,(byte_size(LongHelp_UC)):32/?UI,(LongHelp_UC)/binary, 0:(((8- ((0+byte_size(LongHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wx),[<<4:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_AddRadioTool,
  <<ThisRef:32/?UI,Toolid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((4+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8,BitmapRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbardeletetool">external documentation</a>.
deleteTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_DeleteTool,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), Pos::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbardeletetoolbypos">external documentation</a>.
deleteToolByPos(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_DeleteToolByPos,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer(), Enable::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarenabletool">external documentation</a>.
enableTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,Enable)
 when is_integer(Toolid),is_boolean(Enable) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:cast(?wxToolBar_EnableTool,
  <<ThisRef:32/?UI,Toolid:32/?UI,(wxe_util:from_bool(Enable)):32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarfindbyid">external documentation</a>.
findById(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_FindById,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> wxControl:wxControl()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarfindcontrol">external documentation</a>.
findControl(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_FindControl,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), X::integer(), Y::integer()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarfindtoolforposition">external documentation</a>.
findToolForPosition(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_FindToolForPosition,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxToolBar()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolsize">external documentation</a>.
getToolSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolbitmapsize">external documentation</a>.
getToolBitmapSize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolBitmapSize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargetmargins">external documentation</a>.
getMargins(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetMargins,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolenabled">external documentation</a>.
getToolEnabled(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolEnabled,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoollonghelp">external documentation</a>.
getToolLongHelp(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolLongHelp,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolpacking">external documentation</a>.
getToolPacking(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolPacking,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar(), Id::integer()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolpos">external documentation</a>.
getToolPos(#wx_ref{type=ThisT,ref=ThisRef},Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolPos,
  <<ThisRef:32/?UI,Id:32/?UI>>).

%% @spec (This::wxToolBar()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolseparation">external documentation</a>.
getToolSeparation(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolSeparation,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolshorthelp">external documentation</a>.
getToolShortHelp(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolShortHelp,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbargettoolstate">external documentation</a>.
getToolState(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_GetToolState,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), Pos::integer(), Control::wxControl:wxControl()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarinsertcontrol">external documentation</a>.
insertControl(#wx_ref{type=ThisT,ref=ThisRef},Pos,#wx_ref{type=ControlT,ref=ControlRef})
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ControlT,wxControl),
  wxe_util:call(?wxToolBar_InsertControl,
  <<ThisRef:32/?UI,Pos:32/?UI,ControlRef:32/?UI>>).

%% @spec (This::wxToolBar(), Pos::integer()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarinsertseparator">external documentation</a>.
insertSeparator(#wx_ref{type=ThisT,ref=ThisRef},Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_InsertSeparator,
  <<ThisRef:32/?UI,Pos:32/?UI>>).

%% @spec (This::wxToolBar(), Pos::integer(), Tool::wx:wx()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
insertTool(#wx_ref{type=ThisT,ref=ThisRef},Pos,#wx_ref{type=ToolT,ref=ToolRef})
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(ToolT,wx),
  wxe_util:call(?wxToolBar_InsertTool_2,
  <<ThisRef:32/?UI,Pos:32/?UI,ToolRef:32/?UI>>).

%% @spec (This::wxToolBar(), Pos::integer(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap()) -> wx:wx()
%% @equiv insertTool(This,Pos,Toolid,Bitmap, [])
insertTool(This,Pos,Toolid,Bitmap)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Toolid),is_record(Bitmap, wx_ref) ->
  insertTool(This,Pos,Toolid,Bitmap, []).

%% @spec (This::wxToolBar(),Pos::integer(),Toolid::integer(),X::string()|term(),X::term()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
%% <br /> Alternatives:
%% <p><c>
%% insertTool(This::wxToolBar(), Pos::integer(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap()) -> insertTool(This,Pos,Toolid,Label,Bitmap, []) </c></p>
%% <p><c>
%% insertTool(This::wxToolBar(), Pos::integer(), Toolid::integer(), Bitmap::wxBitmap:wxBitmap(), [Option]) -> wx:wx() </c>
%%<br /> Option = {bmpDisabled, wxBitmap:wxBitmap()} | {toggle, bool()} | {clientData, wx:wx()} | {shortHelp, string()} | {longHelp, string()}
%% </p>

insertTool(This,Pos,Toolid,Label,Bitmap)
 when is_record(This, wx_ref),is_integer(Pos),is_integer(Toolid),is_list(Label),is_record(Bitmap, wx_ref) ->
  insertTool(This,Pos,Toolid,Label,Bitmap, []);
insertTool(#wx_ref{type=ThisT,ref=ThisRef},Pos,Toolid,#wx_ref{type=BitmapT,ref=BitmapRef}, Options)
 when is_integer(Pos),is_integer(Toolid),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT,ref=BmpDisabledRef}}, Acc) ->   ?CLASS(BmpDisabledT,wxBitmap),[<<1:32/?UI,BmpDisabledRef:32/?UI>>|Acc];
          ({toggle, Toggle}, Acc) -> [<<2:32/?UI,(wxe_util:from_bool(Toggle)):32/?UI>>|Acc];
          ({clientData, #wx_ref{type=ClientDataT,ref=ClientDataRef}}, Acc) ->   ?CLASS(ClientDataT,wx),[<<3:32/?UI,ClientDataRef:32/?UI>>|Acc];
          ({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<4:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelp, LongHelp}, Acc) ->   LongHelp_UC = unicode:characters_to_binary([LongHelp,0]),[<<5:32/?UI,(byte_size(LongHelp_UC)):32/?UI,(LongHelp_UC)/binary, 0:(((8- ((0+byte_size(LongHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_InsertTool_4,
  <<ThisRef:32/?UI,Pos:32/?UI,Toolid:32/?UI,BitmapRef:32/?UI, BinOpt/binary>>).

%% @spec (This::wxToolBar(), Pos::integer(), Toolid::integer(), Label::string(), Bitmap::wxBitmap:wxBitmap(), [Option]) -> wx:wx()
%% Option = {bmpDisabled, wxBitmap:wxBitmap()} | {kind, WxItemKind} | {shortHelp, string()} | {longHelp, string()} | {clientData, wx:wx()}
%% WxItemKind = integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarinserttool">external documentation</a>.
%%<br /> WxItemKind is one of ?wxITEM_SEPARATOR | ?wxITEM_NORMAL | ?wxITEM_CHECK | ?wxITEM_RADIO | ?wxITEM_MAX
insertTool(#wx_ref{type=ThisT,ref=ThisRef},Pos,Toolid,Label,#wx_ref{type=BitmapT,ref=BitmapRef}, Options)
 when is_integer(Pos),is_integer(Toolid),is_list(Label),is_list(Options) ->
  ?CLASS(ThisT,wxToolBar),
  Label_UC = unicode:characters_to_binary([Label,0]),
  ?CLASS(BitmapT,wxBitmap),
  MOpts = fun({bmpDisabled, #wx_ref{type=BmpDisabledT,ref=BmpDisabledRef}}, Acc) ->   ?CLASS(BmpDisabledT,wxBitmap),[<<1:32/?UI,BmpDisabledRef:32/?UI>>|Acc];
          ({kind, Kind}, Acc) -> [<<2:32/?UI,Kind:32/?UI>>|Acc];
          ({shortHelp, ShortHelp}, Acc) ->   ShortHelp_UC = unicode:characters_to_binary([ShortHelp,0]),[<<3:32/?UI,(byte_size(ShortHelp_UC)):32/?UI,(ShortHelp_UC)/binary, 0:(((8- ((0+byte_size(ShortHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({longHelp, LongHelp}, Acc) ->   LongHelp_UC = unicode:characters_to_binary([LongHelp,0]),[<<4:32/?UI,(byte_size(LongHelp_UC)):32/?UI,(LongHelp_UC)/binary, 0:(((8- ((0+byte_size(LongHelp_UC)) band 16#7)) band 16#7))/unit:8>>|Acc];
          ({clientData, #wx_ref{type=ClientDataT,ref=ClientDataRef}}, Acc) ->   ?CLASS(ClientDataT,wx),[<<5:32/?UI,ClientDataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxToolBar_InsertTool_5,
  <<ThisRef:32/?UI,Pos:32/?UI,Toolid:32/?UI,(byte_size(Label_UC)):32/?UI,(Label_UC)/binary, 0:(((8- ((0+byte_size(Label_UC)) band 16#7)) band 16#7))/unit:8,BitmapRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (This::wxToolBar()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarrealize">external documentation</a>.
realize(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_Realize,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer()) -> wx:wx()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarremovetool">external documentation</a>.
removeTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid)
 when is_integer(Toolid) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:call(?wxToolBar_RemoveTool,
  <<ThisRef:32/?UI,Toolid:32/?UI>>).

%% @spec (This::wxToolBar(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarsetmargins">external documentation</a>.
setMargins(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:cast(?wxToolBar_SetMargins,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxToolBar(), Size::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarsettoolbitmapsize">external documentation</a>.
setToolBitmapSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:cast(?wxToolBar_SetToolBitmapSize,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer(), HelpString::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarsettoollonghelp">external documentation</a>.
setToolLongHelp(#wx_ref{type=ThisT,ref=ThisRef},Toolid,HelpString)
 when is_integer(Toolid),is_list(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary([HelpString,0]),
  wxe_util:cast(?wxToolBar_SetToolLongHelp,
  <<ThisRef:32/?UI,Toolid:32/?UI,(byte_size(HelpString_UC)):32/?UI,(HelpString_UC)/binary, 0:(((8- ((4+byte_size(HelpString_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxToolBar(), Packing::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarsettoolpacking">external documentation</a>.
setToolPacking(#wx_ref{type=ThisT,ref=ThisRef},Packing)
 when is_integer(Packing) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:cast(?wxToolBar_SetToolPacking,
  <<ThisRef:32/?UI,Packing:32/?UI>>).

%% @spec (This::wxToolBar(), Id::integer(), HelpString::string()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarsettoolshorthelp">external documentation</a>.
setToolShortHelp(#wx_ref{type=ThisT,ref=ThisRef},Id,HelpString)
 when is_integer(Id),is_list(HelpString) ->
  ?CLASS(ThisT,wxToolBar),
  HelpString_UC = unicode:characters_to_binary([HelpString,0]),
  wxe_util:cast(?wxToolBar_SetToolShortHelp,
  <<ThisRef:32/?UI,Id:32/?UI,(byte_size(HelpString_UC)):32/?UI,(HelpString_UC)/binary, 0:(((8- ((4+byte_size(HelpString_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @spec (This::wxToolBar(), Separation::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbarsettoolseparation">external documentation</a>.
setToolSeparation(#wx_ref{type=ThisT,ref=ThisRef},Separation)
 when is_integer(Separation) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:cast(?wxToolBar_SetToolSeparation,
  <<ThisRef:32/?UI,Separation:32/?UI>>).

%% @spec (This::wxToolBar(), Toolid::integer(), Toggle::bool()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxtoolbar.html#wxtoolbartoggletool">external documentation</a>.
toggleTool(#wx_ref{type=ThisT,ref=ThisRef},Toolid,Toggle)
 when is_integer(Toolid),is_boolean(Toggle) ->
  ?CLASS(ThisT,wxToolBar),
  wxe_util:cast(?wxToolBar_ToggleTool,
  <<ThisRef:32/?UI,Toolid:32/?UI,(wxe_util:from_bool(Toggle)):32/?UI>>).

 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
 %% From wxWindow
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
setVirtualSizeHints(This,MinW,MinH, Options) -> wxWindow:setVirtualSizeHints(This,MinW,MinH, Options).
%% @hidden
setVirtualSizeHints(This,MinW,MinH) -> wxWindow:setVirtualSizeHints(This,MinW,MinH).
%% @hidden
setVirtualSizeHints(This,MinSize) -> wxWindow:setVirtualSizeHints(This,MinSize).
%% @hidden
setVirtualSize(This,X,Y) -> wxWindow:setVirtualSize(This,X,Y).
%% @hidden
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
setToolTip(This,Tip) -> wxWindow:setToolTip(This,Tip).
%% @hidden
setThemeEnabled(This,EnableTheme) -> wxWindow:setThemeEnabled(This,EnableTheme).
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
setScrollPos(This,Orient,Pos, Options) -> wxWindow:setScrollPos(This,Orient,Pos, Options).
%% @hidden
setScrollPos(This,Orient,Pos) -> wxWindow:setScrollPos(This,Orient,Pos).
%% @hidden
setScrollbar(This,Orient,Pos,ThumbVisible,Range, Options) -> wxWindow:setScrollbar(This,Orient,Pos,ThumbVisible,Range, Options).
%% @hidden
setScrollbar(This,Orient,Pos,ThumbVisible,Range) -> wxWindow:setScrollbar(This,Orient,Pos,ThumbVisible,Range).
%% @hidden
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
setHelpText(This,Text) -> wxWindow:setHelpText(This,Text).
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
setDropTarget(This,DropTarget) -> wxWindow:setDropTarget(This,DropTarget).
%% @hidden
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
setMinSize(This,MinSize) -> wxWindow:setMinSize(This,MinSize).
%% @hidden
setMaxSize(This,MaxSize) -> wxWindow:setMaxSize(This,MaxSize).
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
popEventHandler(This, Options) -> wxWindow:popEventHandler(This, Options).
%% @hidden
popEventHandler(This) -> wxWindow:popEventHandler(This).
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
makeModal(This, Options) -> wxWindow:makeModal(This, Options).
%% @hidden
makeModal(This) -> wxWindow:makeModal(This).
%% @hidden
lower(This) -> wxWindow:lower(This).
%% @hidden
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
layout(This) -> wxWindow:layout(This).
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
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
getSize(This) -> wxWindow:getSize(This).
%% @hidden
getScrollThumb(This,Orient) -> wxWindow:getScrollThumb(This,Orient).
%% @hidden
getScrollRange(This,Orient) -> wxWindow:getScrollRange(This,Orient).
%% @hidden
getScrollPos(This,Orient) -> wxWindow:getScrollPos(This,Orient).
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
getEventHandler(This) -> wxWindow:getEventHandler(This).
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
findWindow(This,Winid) -> wxWindow:findWindow(This,Winid).
%% @hidden
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
enable(This) -> wxWindow:enable(This).
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
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
centre(This) -> wxWindow:centre(This).
%% @hidden
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
center(This, Options) -> wxWindow:center(This, Options).
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
