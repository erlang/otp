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

-module(wxMenuBar).
-include("wxe.hrl").
-export([append/3,check/3,destroy/1,enable/3,enableTop/3,findItem/2,findMenu/2,
  findMenuItem/3,getAutoWindowMenu/0,getHelpString/2,getLabel/2,getLabelTop/2,
  getMenu/2,getMenuCount/1,getMenuLabel/2,getMenuLabelText/2,insert/4,
  isChecked/2,isEnabled/2,new/0,new/1,oSXGetAppleMenu/1,remove/2,replace/4,
  setAutoWindowMenu/1,setHelpString/3,setLabel/3,setLabelTop/3,setMenuLabel/3]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,findWindow/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,
  getBackgroundColour/1,getBackgroundStyle/1,getBestSize/1,getCaret/1,
  getCharHeight/1,getCharWidth/1,getChildren/1,getClientSize/1,getContainingSizer/1,
  getContentScaleFactor/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,
  getDropTarget/1,getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getMaxSize/1,getMinSize/1,getName/1,
  getParent/1,getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,
  getScrollPos/2,getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isExposed/2,
  isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,isShownOnScreen/1,
  isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,move/2,move/3,move/4,
  moveAfterInTabOrder/2,moveBeforeInTabOrder/2,navigate/1,navigate/2,
  pageDown/1,pageUp/1,parent_class/1,popupMenu/2,popupMenu/3,popupMenu/4,
  raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,
  removeChild/2,reparent/2,screenToClient/1,screenToClient/2,scrollLines/2,
  scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-type wxMenuBar() :: wx:wx_object().
-export_type([wxMenuBar/0]).
%% @hidden
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarwxmenubar">external documentation</a>.
-spec new() -> wxMenuBar().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_new_0),
  wxe_util:rec(?wxMenuBar_new_0).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarwxmenubar">external documentation</a>.
-spec new(Style) -> wxMenuBar() when
	Style::integer().
new(Style)
 when is_integer(Style) ->
  wxe_util:queue_cmd(Style,?get_env(),?wxMenuBar_new_1),
  wxe_util:rec(?wxMenuBar_new_1).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarappend">external documentation</a>.
-spec append(This, Menu, Title) -> boolean() when
	This::wxMenuBar(), Menu::wxMenu:wxMenu(), Title::unicode:chardata().
append(#wx_ref{type=ThisT}=This,#wx_ref{type=MenuT}=Menu,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  ?CLASS(MenuT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Menu,Title_UC,?get_env(),?wxMenuBar_Append),
  wxe_util:rec(?wxMenuBar_Append).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarcheck">external documentation</a>.
-spec check(This, Id, Check) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Check::boolean().
check(#wx_ref{type=ThisT}=This,Id,Check)
 when is_integer(Id),is_boolean(Check) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,Check,?get_env(),?wxMenuBar_Check).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarenable">external documentation</a>.
-spec enable(This, Id, Enable) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Enable::boolean().
enable(#wx_ref{type=ThisT}=This,Id,Enable)
 when is_integer(Id),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,Enable,?get_env(),?wxMenuBar_Enable).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarenabletop">external documentation</a>.
-spec enableTop(This, Pos, Enable) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Enable::boolean().
enableTop(#wx_ref{type=ThisT}=This,Pos,Enable)
 when is_integer(Pos),is_boolean(Enable) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,Enable,?get_env(),?wxMenuBar_EnableTop).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarfindmenu">external documentation</a>.
-spec findMenu(This, Title) -> integer() when
	This::wxMenuBar(), Title::unicode:chardata().
findMenu(#wx_ref{type=ThisT}=This,Title)
 when ?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Title_UC,?get_env(),?wxMenuBar_FindMenu),
  wxe_util:rec(?wxMenuBar_FindMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarfindmenuitem">external documentation</a>.
-spec findMenuItem(This, MenuString, ItemString) -> integer() when
	This::wxMenuBar(), MenuString::unicode:chardata(), ItemString::unicode:chardata().
findMenuItem(#wx_ref{type=ThisT}=This,MenuString,ItemString)
 when ?is_chardata(MenuString),?is_chardata(ItemString) ->
  ?CLASS(ThisT,wxMenuBar),
  MenuString_UC = unicode:characters_to_binary(MenuString),
  ItemString_UC = unicode:characters_to_binary(ItemString),
  wxe_util:queue_cmd(This,MenuString_UC,ItemString_UC,?get_env(),?wxMenuBar_FindMenuItem),
  wxe_util:rec(?wxMenuBar_FindMenuItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarfinditem">external documentation</a>.
-spec findItem(This, Id) -> wxMenuItem:wxMenuItem() when
	This::wxMenuBar(), Id::integer().
findItem(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_FindItem),
  wxe_util:rec(?wxMenuBar_FindItem).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargethelpstring">external documentation</a>.
-spec getHelpString(This, Id) -> unicode:charlist() when
	This::wxMenuBar(), Id::integer().
getHelpString(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_GetHelpString),
  wxe_util:rec(?wxMenuBar_GetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetlabel">external documentation</a>.
-spec getLabel(This, Id) -> unicode:charlist() when
	This::wxMenuBar(), Id::integer().
getLabel(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_GetLabel),
  wxe_util:rec(?wxMenuBar_GetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenulabel">external documentation</a>.
-spec getLabelTop(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().

getLabelTop(This,Pos)
 when is_record(This, wx_ref),is_integer(Pos) ->
  getMenuLabel(This,Pos).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenulabel">external documentation</a>.
-spec getMenuLabel(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().
getMenuLabel(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_GetMenuLabel),
  wxe_util:rec(?wxMenuBar_GetMenuLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenulabeltext">external documentation</a>.
-spec getMenuLabelText(This, Pos) -> unicode:charlist() when
	This::wxMenuBar(), Pos::integer().
getMenuLabelText(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_GetMenuLabelText),
  wxe_util:rec(?wxMenuBar_GetMenuLabelText).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenu">external documentation</a>.
-spec getMenu(This, MenuIndex) -> wxMenu:wxMenu() when
	This::wxMenuBar(), MenuIndex::integer().
getMenu(#wx_ref{type=ThisT}=This,MenuIndex)
 when is_integer(MenuIndex) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,MenuIndex,?get_env(),?wxMenuBar_GetMenu),
  wxe_util:rec(?wxMenuBar_GetMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetmenucount">external documentation</a>.
-spec getMenuCount(This) -> integer() when
	This::wxMenuBar().
getMenuCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuBar_GetMenuCount),
  wxe_util:rec(?wxMenuBar_GetMenuCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarinsert">external documentation</a>.
-spec insert(This, Pos, Menu, Title) -> boolean() when
	This::wxMenuBar(), Pos::integer(), Menu::wxMenu:wxMenu(), Title::unicode:chardata().
insert(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=MenuT}=Menu,Title)
 when is_integer(Pos),?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  ?CLASS(MenuT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Pos,Menu,Title_UC,?get_env(),?wxMenuBar_Insert),
  wxe_util:rec(?wxMenuBar_Insert).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarischecked">external documentation</a>.
-spec isChecked(This, Id) -> boolean() when
	This::wxMenuBar(), Id::integer().
isChecked(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_IsChecked),
  wxe_util:rec(?wxMenuBar_IsChecked).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetautowindowmenu">external documentation</a>.
-spec setAutoWindowMenu(Enable) -> 'ok' when
	Enable::boolean().
setAutoWindowMenu(Enable)
 when is_boolean(Enable) ->
  wxe_util:queue_cmd(Enable,?get_env(),?wxMenuBar_SetAutoWindowMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubargetautowindowmenu">external documentation</a>.
-spec getAutoWindowMenu() -> boolean().
getAutoWindowMenu() ->
  wxe_util:queue_cmd(?get_env(), ?wxMenuBar_GetAutoWindowMenu),
  wxe_util:rec(?wxMenuBar_GetAutoWindowMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarosxgetapplemenu">external documentation</a>.
-spec oSXGetAppleMenu(This) -> wxMenu:wxMenu() when
	This::wxMenuBar().
oSXGetAppleMenu(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,?get_env(),?wxMenuBar_OSXGetAppleMenu),
  wxe_util:rec(?wxMenuBar_OSXGetAppleMenu).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarisenabled">external documentation</a>.
-spec isEnabled(This, Id) -> boolean() when
	This::wxMenuBar(), Id::integer().
isEnabled(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxMenuBar_IsEnabled),
  wxe_util:rec(?wxMenuBar_IsEnabled).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarremove">external documentation</a>.
-spec remove(This, Pos) -> wxMenu:wxMenu() when
	This::wxMenuBar(), Pos::integer().
remove(#wx_ref{type=ThisT}=This,Pos)
 when is_integer(Pos) ->
  ?CLASS(ThisT,wxMenuBar),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxMenuBar_Remove),
  wxe_util:rec(?wxMenuBar_Remove).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarreplace">external documentation</a>.
-spec replace(This, Pos, Menu, Title) -> wxMenu:wxMenu() when
	This::wxMenuBar(), Pos::integer(), Menu::wxMenu:wxMenu(), Title::unicode:chardata().
replace(#wx_ref{type=ThisT}=This,Pos,#wx_ref{type=MenuT}=Menu,Title)
 when is_integer(Pos),?is_chardata(Title) ->
  ?CLASS(ThisT,wxMenuBar),
  ?CLASS(MenuT,wxMenu),
  Title_UC = unicode:characters_to_binary(Title),
  wxe_util:queue_cmd(This,Pos,Menu,Title_UC,?get_env(),?wxMenuBar_Replace),
  wxe_util:rec(?wxMenuBar_Replace).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsethelpstring">external documentation</a>.
-spec setHelpString(This, Id, HelpString) -> 'ok' when
	This::wxMenuBar(), Id::integer(), HelpString::unicode:chardata().
setHelpString(#wx_ref{type=ThisT}=This,Id,HelpString)
 when is_integer(Id),?is_chardata(HelpString) ->
  ?CLASS(ThisT,wxMenuBar),
  HelpString_UC = unicode:characters_to_binary(HelpString),
  wxe_util:queue_cmd(This,Id,HelpString_UC,?get_env(),?wxMenuBar_SetHelpString).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetlabel">external documentation</a>.
-spec setLabel(This, Id, Label) -> 'ok' when
	This::wxMenuBar(), Id::integer(), Label::unicode:chardata().
setLabel(#wx_ref{type=ThisT}=This,Id,Label)
 when is_integer(Id),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuBar),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Id,Label_UC,?get_env(),?wxMenuBar_SetLabel).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetmenulabel">external documentation</a>.
-spec setLabelTop(This, Pos, Label) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Label::unicode:chardata().

setLabelTop(This,Pos,Label)
 when is_record(This, wx_ref),is_integer(Pos),?is_chardata(Label) ->
  setMenuLabel(This,Pos,Label).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxmenubar.html#wxmenubarsetmenulabel">external documentation</a>.
-spec setMenuLabel(This, Pos, Label) -> 'ok' when
	This::wxMenuBar(), Pos::integer(), Label::unicode:chardata().
setMenuLabel(#wx_ref{type=ThisT}=This,Pos,Label)
 when is_integer(Pos),?is_chardata(Label) ->
  ?CLASS(ThisT,wxMenuBar),
  Label_UC = unicode:characters_to_binary(Label),
  wxe_util:queue_cmd(This,Pos,Label_UC,?get_env(),?wxMenuBar_SetMenuLabel).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxMenuBar()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxMenuBar),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
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
