%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

-module(wxWebView).
-include("wxe.hrl").
-export([canCopy/1,canCut/1,canGoBack/1,canGoForward/1,canPaste/1,canRedo/1,
  canSetZoomType/2,canUndo/1,clearHistory/1,clearSelection/1,copy/1,
  cut/1,deleteSelection/1,enableContextMenu/1,enableContextMenu/2,enableHistory/1,
  enableHistory/2,find/2,find/3,getCurrentTitle/1,getCurrentURL/1,getPageSource/1,
  getPageText/1,getSelectedSource/1,getSelectedText/1,getZoom/1,getZoomFactor/1,
  getZoomType/1,goBack/1,goForward/1,hasSelection/1,isBackendAvailable/1,
  isBusy/1,isContextMenuEnabled/1,isEditable/1,loadURL/2,new/2,new/3,
  paste/1,print/1,redo/1,reload/1,reload/2,runScript/2,selectAll/1,setEditable/1,
  setEditable/2,setPage/3,setZoom/2,setZoomFactor/2,setZoomType/2,stop/1,
  undo/1]).

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

-type wxWebView() :: wx:wx_object().
-export_type([wxWebView/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxWebView() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewnew">external documentation</a>.
-spec new(Parent, Id, [Option]) -> wxWebView() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'url', unicode:chardata()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'backend', unicode:chardata()}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({url, Url}) ->   Url_UC = unicode:characters_to_binary(Url),{url,Url_UC};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({backend, Backend}) ->   Backend_UC = unicode:characters_to_binary(Backend),{backend,Backend_UC};
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxWebView_New),
  wxe_util:rec(?wxWebView_New).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetcurrenttitle">external documentation</a>.
-spec getCurrentTitle(This) -> unicode:charlist() when
	This::wxWebView().
getCurrentTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetCurrentTitle),
  wxe_util:rec(?wxWebView_GetCurrentTitle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetcurrenturl">external documentation</a>.
-spec getCurrentURL(This) -> unicode:charlist() when
	This::wxWebView().
getCurrentURL(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetCurrentURL),
  wxe_util:rec(?wxWebView_GetCurrentURL).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetpagesource">external documentation</a>.
-spec getPageSource(This) -> unicode:charlist() when
	This::wxWebView().
getPageSource(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetPageSource),
  wxe_util:rec(?wxWebView_GetPageSource).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetpagetext">external documentation</a>.
-spec getPageText(This) -> unicode:charlist() when
	This::wxWebView().
getPageText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetPageText),
  wxe_util:rec(?wxWebView_GetPageText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewisbusy">external documentation</a>.
-spec isBusy(This) -> boolean() when
	This::wxWebView().
isBusy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_IsBusy),
  wxe_util:rec(?wxWebView_IsBusy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewiseditable">external documentation</a>.
-spec isEditable(This) -> boolean() when
	This::wxWebView().
isEditable(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_IsEditable),
  wxe_util:rec(?wxWebView_IsEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewloadurl">external documentation</a>.
-spec loadURL(This, Url) -> 'ok' when
	This::wxWebView(), Url::unicode:chardata().
loadURL(#wx_ref{type=ThisT}=This,Url)
 when ?is_chardata(Url) ->
  ?CLASS(ThisT,wxWebView),
  Url_UC = unicode:characters_to_binary(Url),
  wxe_util:queue_cmd(This,Url_UC,?get_env(),?wxWebView_LoadURL).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewprint">external documentation</a>.
-spec print(This) -> 'ok' when
	This::wxWebView().
print(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Print).

%% @equiv reload(This, [])
-spec reload(This) -> 'ok' when
	This::wxWebView().

reload(This)
 when is_record(This, wx_ref) ->
  reload(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewreload">external documentation</a>.
%%<br /> Flags = ?wxWEBVIEW_RELOAD_DEFAULT | ?wxWEBVIEW_RELOAD_NO_CACHE
-spec reload(This, [Option]) -> 'ok' when
	This::wxWebView(),
	Option :: {'flags', wx:wx_enum()}.
reload(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWebView),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWebView_Reload).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewrunscript">external documentation</a>.
-spec runScript(This, Javascript) -> Result when
	Result ::{Res ::boolean(), Output::unicode:charlist()},
	This::wxWebView(), Javascript::unicode:chardata().
runScript(#wx_ref{type=ThisT}=This,Javascript)
 when ?is_chardata(Javascript) ->
  ?CLASS(ThisT,wxWebView),
  Javascript_UC = unicode:characters_to_binary(Javascript),
  wxe_util:queue_cmd(This,Javascript_UC,?get_env(),?wxWebView_RunScript),
  wxe_util:rec(?wxWebView_RunScript).

%% @equiv setEditable(This, [])
-spec setEditable(This) -> 'ok' when
	This::wxWebView().

setEditable(This)
 when is_record(This, wx_ref) ->
  setEditable(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewseteditable">external documentation</a>.
-spec setEditable(This, [Option]) -> 'ok' when
	This::wxWebView(),
	Option :: {'enable', boolean()}.
setEditable(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWebView),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWebView_SetEditable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewsetpage">external documentation</a>.
-spec setPage(This, Html, BaseUrl) -> 'ok' when
	This::wxWebView(), Html::unicode:chardata(), BaseUrl::unicode:chardata().
setPage(#wx_ref{type=ThisT}=This,Html,BaseUrl)
 when ?is_chardata(Html),?is_chardata(BaseUrl) ->
  ?CLASS(ThisT,wxWebView),
  Html_UC = unicode:characters_to_binary(Html),
  BaseUrl_UC = unicode:characters_to_binary(BaseUrl),
  wxe_util:queue_cmd(This,Html_UC,BaseUrl_UC,?get_env(),?wxWebView_SetPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewstop">external documentation</a>.
-spec stop(This) -> 'ok' when
	This::wxWebView().
stop(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Stop).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcancopy">external documentation</a>.
-spec canCopy(This) -> boolean() when
	This::wxWebView().
canCopy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanCopy),
  wxe_util:rec(?wxWebView_CanCopy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcancut">external documentation</a>.
-spec canCut(This) -> boolean() when
	This::wxWebView().
canCut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanCut),
  wxe_util:rec(?wxWebView_CanCut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcanpaste">external documentation</a>.
-spec canPaste(This) -> boolean() when
	This::wxWebView().
canPaste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanPaste),
  wxe_util:rec(?wxWebView_CanPaste).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcopy">external documentation</a>.
-spec copy(This) -> 'ok' when
	This::wxWebView().
copy(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Copy).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcut">external documentation</a>.
-spec cut(This) -> 'ok' when
	This::wxWebView().
cut(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Cut).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewpaste">external documentation</a>.
-spec paste(This) -> 'ok' when
	This::wxWebView().
paste(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Paste).

%% @equiv enableContextMenu(This, [])
-spec enableContextMenu(This) -> 'ok' when
	This::wxWebView().

enableContextMenu(This)
 when is_record(This, wx_ref) ->
  enableContextMenu(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewenablecontextmenu">external documentation</a>.
-spec enableContextMenu(This, [Option]) -> 'ok' when
	This::wxWebView(),
	Option :: {'enable', boolean()}.
enableContextMenu(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWebView),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWebView_EnableContextMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewiscontextmenuenabled">external documentation</a>.
-spec isContextMenuEnabled(This) -> boolean() when
	This::wxWebView().
isContextMenuEnabled(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_IsContextMenuEnabled),
  wxe_util:rec(?wxWebView_IsContextMenuEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcangoback">external documentation</a>.
-spec canGoBack(This) -> boolean() when
	This::wxWebView().
canGoBack(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanGoBack),
  wxe_util:rec(?wxWebView_CanGoBack).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcangoforward">external documentation</a>.
-spec canGoForward(This) -> boolean() when
	This::wxWebView().
canGoForward(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanGoForward),
  wxe_util:rec(?wxWebView_CanGoForward).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewclearhistory">external documentation</a>.
-spec clearHistory(This) -> 'ok' when
	This::wxWebView().
clearHistory(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_ClearHistory).

%% @equiv enableHistory(This, [])
-spec enableHistory(This) -> 'ok' when
	This::wxWebView().

enableHistory(This)
 when is_record(This, wx_ref) ->
  enableHistory(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewenablehistory">external documentation</a>.
-spec enableHistory(This, [Option]) -> 'ok' when
	This::wxWebView(),
	Option :: {'enable', boolean()}.
enableHistory(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxWebView),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxWebView_EnableHistory).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgoback">external documentation</a>.
-spec goBack(This) -> 'ok' when
	This::wxWebView().
goBack(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GoBack).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgoforward">external documentation</a>.
-spec goForward(This) -> 'ok' when
	This::wxWebView().
goForward(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GoForward).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewclearselection">external documentation</a>.
-spec clearSelection(This) -> 'ok' when
	This::wxWebView().
clearSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_ClearSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewdeleteselection">external documentation</a>.
-spec deleteSelection(This) -> 'ok' when
	This::wxWebView().
deleteSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_DeleteSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetselectedsource">external documentation</a>.
-spec getSelectedSource(This) -> unicode:charlist() when
	This::wxWebView().
getSelectedSource(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetSelectedSource),
  wxe_util:rec(?wxWebView_GetSelectedSource).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetselectedtext">external documentation</a>.
-spec getSelectedText(This) -> unicode:charlist() when
	This::wxWebView().
getSelectedText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetSelectedText),
  wxe_util:rec(?wxWebView_GetSelectedText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewhasselection">external documentation</a>.
-spec hasSelection(This) -> boolean() when
	This::wxWebView().
hasSelection(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_HasSelection),
  wxe_util:rec(?wxWebView_HasSelection).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewselectall">external documentation</a>.
-spec selectAll(This) -> 'ok' when
	This::wxWebView().
selectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_SelectAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcanredo">external documentation</a>.
-spec canRedo(This) -> boolean() when
	This::wxWebView().
canRedo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanRedo),
  wxe_util:rec(?wxWebView_CanRedo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcanundo">external documentation</a>.
-spec canUndo(This) -> boolean() when
	This::wxWebView().
canUndo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_CanUndo),
  wxe_util:rec(?wxWebView_CanUndo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewredo">external documentation</a>.
-spec redo(This) -> 'ok' when
	This::wxWebView().
redo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Redo).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewundo">external documentation</a>.
-spec undo(This) -> 'ok' when
	This::wxWebView().
undo(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_Undo).

%% @equiv find(This,Text, [])
-spec find(This, Text) -> integer() when
	This::wxWebView(), Text::unicode:chardata().

find(This,Text)
 when is_record(This, wx_ref),?is_chardata(Text) ->
  find(This,Text, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewfind">external documentation</a>.
%%<br /> Flags = ?wxWEBVIEW_FIND_WRAP | ?wxWEBVIEW_FIND_ENTIRE_WORD | ?wxWEBVIEW_FIND_MATCH_CASE | ?wxWEBVIEW_FIND_HIGHLIGHT_RESULT | ?wxWEBVIEW_FIND_BACKWARDS | ?wxWEBVIEW_FIND_DEFAULT
-spec find(This, Text, [Option]) -> integer() when
	This::wxWebView(), Text::unicode:chardata(),
	Option :: {'flags', wx:wx_enum()}.
find(#wx_ref{type=ThisT}=This,Text, Options)
 when ?is_chardata(Text),is_list(Options) ->
  ?CLASS(ThisT,wxWebView),
  Text_UC = unicode:characters_to_binary(Text),
  MOpts = fun({flags, _flags} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Text_UC, Opts,?get_env(),?wxWebView_Find),
  wxe_util:rec(?wxWebView_Find).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewcansetzoomtype">external documentation</a>.
%%<br /> Type = ?wxWEBVIEW_ZOOM_TYPE_LAYOUT | ?wxWEBVIEW_ZOOM_TYPE_TEXT
-spec canSetZoomType(This, Type) -> boolean() when
	This::wxWebView(), Type::wx:wx_enum().
canSetZoomType(#wx_ref{type=ThisT}=This,Type)
 when is_integer(Type) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,Type,?get_env(),?wxWebView_CanSetZoomType),
  wxe_util:rec(?wxWebView_CanSetZoomType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetzoom">external documentation</a>.
%%<br /> Res = ?wxWEBVIEW_ZOOM_TINY | ?wxWEBVIEW_ZOOM_SMALL | ?wxWEBVIEW_ZOOM_MEDIUM | ?wxWEBVIEW_ZOOM_LARGE | ?wxWEBVIEW_ZOOM_LARGEST
-spec getZoom(This) -> wx:wx_enum() when
	This::wxWebView().
getZoom(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetZoom),
  wxe_util:rec(?wxWebView_GetZoom).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetzoomtype">external documentation</a>.
%%<br /> Res = ?wxWEBVIEW_ZOOM_TYPE_LAYOUT | ?wxWEBVIEW_ZOOM_TYPE_TEXT
-spec getZoomType(This) -> wx:wx_enum() when
	This::wxWebView().
getZoomType(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetZoomType),
  wxe_util:rec(?wxWebView_GetZoomType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewsetzoom">external documentation</a>.
%%<br /> Zoom = ?wxWEBVIEW_ZOOM_TINY | ?wxWEBVIEW_ZOOM_SMALL | ?wxWEBVIEW_ZOOM_MEDIUM | ?wxWEBVIEW_ZOOM_LARGE | ?wxWEBVIEW_ZOOM_LARGEST
-spec setZoom(This, Zoom) -> 'ok' when
	This::wxWebView(), Zoom::wx:wx_enum().
setZoom(#wx_ref{type=ThisT}=This,Zoom)
 when is_integer(Zoom) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,Zoom,?get_env(),?wxWebView_SetZoom).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewsetzoomtype">external documentation</a>.
%%<br /> ZoomType = ?wxWEBVIEW_ZOOM_TYPE_LAYOUT | ?wxWEBVIEW_ZOOM_TYPE_TEXT
-spec setZoomType(This, ZoomType) -> 'ok' when
	This::wxWebView(), ZoomType::wx:wx_enum().
setZoomType(#wx_ref{type=ThisT}=This,ZoomType)
 when is_integer(ZoomType) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,ZoomType,?get_env(),?wxWebView_SetZoomType).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewgetzoomfactor">external documentation</a>.
-spec getZoomFactor(This) -> number() when
	This::wxWebView().
getZoomFactor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,?get_env(),?wxWebView_GetZoomFactor),
  wxe_util:rec(?wxWebView_GetZoomFactor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewsetzoomfactor">external documentation</a>.
-spec setZoomFactor(This, Zoom) -> 'ok' when
	This::wxWebView(), Zoom::number().
setZoomFactor(#wx_ref{type=ThisT}=This,Zoom)
 when is_number(Zoom) ->
  ?CLASS(ThisT,wxWebView),
  wxe_util:queue_cmd(This,Zoom,?get_env(),?wxWebView_SetZoomFactor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxwebview.html#wxwebviewisbackendavailable">external documentation</a>.
-spec isBackendAvailable(Backend) -> boolean() when
	Backend::unicode:chardata().
isBackendAvailable(Backend)
 when ?is_chardata(Backend) ->
  Backend_UC = unicode:characters_to_binary(Backend),
  wxe_util:queue_cmd(Backend_UC,?get_env(),?wxWebView_IsBackendAvailable),
  wxe_util:rec(?wxWebView_IsBackendAvailable).

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
