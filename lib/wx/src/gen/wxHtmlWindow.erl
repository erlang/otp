%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 AND LicenseRef-scancode-wxwindows-free-doc-3
%%
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%% For documentation, wxWindow Free Documentation License, Version 3 applies.
%% wxWindows Free Documentation Licence, Version 3, as follows.
%% ===============================================
%%
%% Everyone is permitted to copy and distribute verbatim copies
%% of this licence document, but changing it is not allowed.
%%
%%                  WXWINDOWS FREE DOCUMENTATION LICENCE
%%    TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
%%
%% 1. Permission is granted to make and distribute verbatim copies of this
%% manual or piece of documentation provided any copyright notice and this
%% permission notice are preserved on all copies.
%%
%% 2. Permission is granted to process this file or document through a
%% document processing system and, at your option and the option of any third
%% party, print the results, provided a printed document carries a copying
%% permission notice identical to this one.
%%
%% 3. Permission is granted to copy and distribute modified versions of this
%% manual or piece of documentation under the conditions for verbatim copying,
%% provided also that any sections describing licensing conditions for this
%% manual, such as, in particular, the GNU General Public Licence, the GNU
%% Library General Public Licence, and any wxWindows Licence are included
%% exactly as in the original, and provided that the entire resulting derived
%% work is distributed under the terms of a permission notice identical to
%% this one.
%%
%% 4. Permission is granted to copy and distribute translations of this manual
%% or piece of documentation into another language, under the above conditions
%% for modified versions, except that sections related to licensing, including
%% this paragraph, may also be included in translations approved by the
%% copyright holders of the respective licence documents in addition to the
%% original English.
%%
%% %CopyrightEnd%
%% This file is generated DO NOT EDIT

-module(wxHtmlWindow).
-moduledoc """
`m:wxHtmlWindow` is probably the only class you will directly use unless you want to do
something special (like adding new tag handlers or MIME filters).

The purpose of this class is to display rich content pages (either local file or
downloaded via HTTP protocol) in a window based on a subset of the HTML standard. The
width of the window is constant, given in the constructor and virtual height is changed
dynamically depending on page size. Once the window is created you can set its content by
calling `setPage/2` with raw HTML, `loadPage/2` with a `wxFileSystem` (not implemented in wx) location or `loadFile/2` with a filename.

Note: If you want complete HTML/CSS support as well as a Javascript engine, consider
using `m:wxWebView` instead.

`m:wxHtmlWindow` uses the `m:wxImage` class for displaying images, so you need to
initialize the handlers for any image formats you use before loading a page. See
?wxInitAllImageHandlers and `wxImage::AddHandler` (not implemented in wx).

## Styles

This class supports the following styles:

* wxHW_SCROLLBAR_NEVER: Never display scrollbars, not even when the page is larger than the
window.

* wxHW_SCROLLBAR_AUTO: Display scrollbars only if page's size exceeds window's size.

* wxHW_NO_SELECTION: Don't allow the user to select text.

See: `m:wxHtmlLinkEvent`

This class is derived, and can use functions, from:

* `m:wxScrolledWindow`

* `m:wxPanel`

* `m:wxWindow`

* `m:wxEvtHandler`

wxWidgets docs: [wxHtmlWindow](https://docs.wxwidgets.org/3.2/classwx_html_window.html)

## Events

Event types emitted from this class:

* [`html_cell_clicked`](`m:wxHtmlLinkEvent`)

* [`html_cell_hover`](`m:wxHtmlLinkEvent`)

* [`command_html_link_clicked`](`m:wxHtmlLinkEvent`)
""".
-include("wxe.hrl").
-export([appendToPage/2,destroy/1,getOpenedAnchor/1,getOpenedPage/1,getOpenedPageTitle/1,
  getRelatedFrame/1,historyBack/1,historyCanBack/1,historyCanForward/1,
  historyClear/1,historyForward/1,loadFile/2,loadPage/2,new/0,new/1,new/2,
  selectAll/1,selectLine/2,selectWord/2,selectionToText/1,setBorders/2,
  setFonts/3,setFonts/4,setPage/2,setRelatedFrame/3,setRelatedStatusBar/2,
  setRelatedStatusBar/3,toText/1]).

%% inherited exports
-export([cacheBestSize/2,calcScrolledPosition/2,calcScrolledPosition/3,calcUnscrolledPosition/2,
  calcUnscrolledPosition/3,canSetTransparent/1,captureMouse/1,center/1,
  center/2,centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  doPrepareDC/2,dragAcceptFiles/2,enable/1,enable/2,enableScrolling/3,
  findWindow/2,fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,getBackgroundColour/1,
  getBackgroundStyle/1,getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,
  getChildren/1,getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,
  getCursor/1,getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,
  getFont/1,getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPixelsPerUnit/1,
  getScrollPos/2,getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getViewStart/1,getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,
  hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,
  isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popupMenu/2,
  popupMenu/3,popupMenu/4,prepareDC/2,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,
  screenToClient/2,scroll/2,scroll/3,scrollLines/2,scrollPages/2,scrollWindow/3,
  scrollWindow/4,setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,
  setBackgroundStyle/2,setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,
  setCursor/2,setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,
  setFocusFromKbd/1,setFocusIgnoringChildren/1,setFont/2,setForegroundColour/2,
  setHelpText/2,setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,
  setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,setPalette/2,
  setScrollPos/3,setScrollPos/4,setScrollRate/3,setScrollbar/5,setScrollbar/6,
  setScrollbars/5,setScrollbars/6,setSize/2,setSize/3,setSize/5,setSize/6,
  setSizeHints/2,setSizeHints/3,setSizeHints/4,setSizer/2,setSizer/3,
  setSizerAndFit/2,setSizerAndFit/3,setTargetWindow/2,setThemeEnabled/2,
  setToolTip/2,setTransparent/2,setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,
  setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,show/1,
  show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,update/1,
  updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxHtmlWindow() :: wx:wx_object().
-export_type([wxHtmlWindow/0]).
-doc false.
parent_class(wxScrolledWindow) -> true;
parent_class(wxPanel) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-doc "Default ctor.".
-spec new() -> wxHtmlWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxHtmlWindow_new_0),
  wxe_util:rec(?wxHtmlWindow_new_0).

-doc(#{equiv => new(Parent, [])}).
-spec new(Parent) -> wxHtmlWindow() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

-doc """
Constructor.

The parameters are the same as `wxScrolled::wxScrolled()` (not implemented in wx)
constructor.
""".
-spec new(Parent, [Option]) -> wxHtmlWindow() when
	Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, _id} = Arg) -> Arg;
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent, Opts,?get_env(),?wxHtmlWindow_new_2),
  wxe_util:rec(?wxHtmlWindow_new_2).

-doc """
Appends HTML fragment to currently displayed text and refreshes the window.

Return: false if an error occurred, true otherwise.
""".
-spec appendToPage(This, Source) -> boolean() when
	This::wxHtmlWindow(), Source::unicode:chardata().
appendToPage(#wx_ref{type=ThisT}=This,Source)
 when ?is_chardata(Source) ->
  ?CLASS(ThisT,wxHtmlWindow),
  Source_UC = unicode:characters_to_binary(Source),
  wxe_util:queue_cmd(This,Source_UC,?get_env(),?wxHtmlWindow_AppendToPage),
  wxe_util:rec(?wxHtmlWindow_AppendToPage).

-doc """
Returns anchor within currently opened page (see `getOpenedPage/1`).

If no page is opened or if the displayed page wasn't produced by call to `loadPage/2`, empty string
is returned.
""".
-spec getOpenedAnchor(This) -> unicode:charlist() when
	This::wxHtmlWindow().
getOpenedAnchor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetOpenedAnchor),
  wxe_util:rec(?wxHtmlWindow_GetOpenedAnchor).

-doc """
Returns full location of the opened page.

If no page is opened or if the displayed page wasn't produced by call to `loadPage/2`, empty string
is returned.
""".
-spec getOpenedPage(This) -> unicode:charlist() when
	This::wxHtmlWindow().
getOpenedPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetOpenedPage),
  wxe_util:rec(?wxHtmlWindow_GetOpenedPage).

-doc """
Returns title of the opened page or wxEmptyString if the current page does not contain
*<TITLE>* tag.
""".
-spec getOpenedPageTitle(This) -> unicode:charlist() when
	This::wxHtmlWindow().
getOpenedPageTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetOpenedPageTitle),
  wxe_util:rec(?wxHtmlWindow_GetOpenedPageTitle).

-doc "Returns the related frame.".
-spec getRelatedFrame(This) -> wxFrame:wxFrame() when
	This::wxHtmlWindow().
getRelatedFrame(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetRelatedFrame),
  wxe_util:rec(?wxHtmlWindow_GetRelatedFrame).

-doc """
Moves back to the previous page.

Only pages displayed using `loadPage/2` are stored in history list.
""".
-spec historyBack(This) -> boolean() when
	This::wxHtmlWindow().
historyBack(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_HistoryBack),
  wxe_util:rec(?wxHtmlWindow_HistoryBack).

-doc """
Returns true if it is possible to go back in the history i.e.

`historyBack/1` won't fail.
""".
-spec historyCanBack(This) -> boolean() when
	This::wxHtmlWindow().
historyCanBack(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_HistoryCanBack),
  wxe_util:rec(?wxHtmlWindow_HistoryCanBack).

-doc """
Returns true if it is possible to go forward in the history i.e.

`historyForward/1` won't fail.
""".
-spec historyCanForward(This) -> boolean() when
	This::wxHtmlWindow().
historyCanForward(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_HistoryCanForward),
  wxe_util:rec(?wxHtmlWindow_HistoryCanForward).

-doc "Clears history.".
-spec historyClear(This) -> 'ok' when
	This::wxHtmlWindow().
historyClear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_HistoryClear).

-doc """
Moves to next page in history.

Only pages displayed using `loadPage/2` are stored in history list.
""".
-spec historyForward(This) -> boolean() when
	This::wxHtmlWindow().
historyForward(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_HistoryForward),
  wxe_util:rec(?wxHtmlWindow_HistoryForward).

-doc """
Loads an HTML page from a file and displays it.

Return: false if an error occurred, true otherwise

See: `loadPage/2`
""".
-spec loadFile(This, Filename) -> boolean() when
	This::wxHtmlWindow(), Filename::unicode:chardata().
loadFile(#wx_ref{type=ThisT}=This,Filename)
 when ?is_chardata(Filename) ->
  ?CLASS(ThisT,wxHtmlWindow),
  Filename_UC = unicode:characters_to_binary(Filename),
  wxe_util:queue_cmd(This,Filename_UC,?get_env(),?wxHtmlWindow_LoadFile),
  wxe_util:rec(?wxHtmlWindow_LoadFile).

-doc """
Unlike `setPage/2` this function first loads the HTML page from `location` and then
displays it.

Return: false if an error occurred, true otherwise

See: `loadFile/2`
""".
-spec loadPage(This, Location) -> boolean() when
	This::wxHtmlWindow(), Location::unicode:chardata().
loadPage(#wx_ref{type=ThisT}=This,Location)
 when ?is_chardata(Location) ->
  ?CLASS(ThisT,wxHtmlWindow),
  Location_UC = unicode:characters_to_binary(Location),
  wxe_util:queue_cmd(This,Location_UC,?get_env(),?wxHtmlWindow_LoadPage),
  wxe_util:rec(?wxHtmlWindow_LoadPage).

-doc """
Selects all text in the window.

See:
* `selectLine/2`

* `selectWord/2`
""".
-spec selectAll(This) -> 'ok' when
	This::wxHtmlWindow().
selectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_SelectAll).

-doc """
Returns the current selection as plain text.

Returns an empty string if no text is currently selected.
""".
-spec selectionToText(This) -> unicode:charlist() when
	This::wxHtmlWindow().
selectionToText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_SelectionToText),
  wxe_util:rec(?wxHtmlWindow_SelectionToText).

-doc """
Selects the line of text that `pos` points at.

Note that `pos` is relative to the top of displayed page, not to window's origin, use `wxScrolledWindow:calcUnscrolledPosition/3` to
convert physical coordinate.

See:
* `selectAll/1`

* `selectWord/2`
""".
-spec selectLine(This, Pos) -> 'ok' when
	This::wxHtmlWindow(), Pos::{X::integer(), Y::integer()}.
selectLine(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxHtmlWindow_SelectLine).

-doc """
Selects the word at position `pos`.

Note that `pos` is relative to the top of displayed page, not to window's origin, use `wxScrolledWindow:calcUnscrolledPosition/3` to
convert physical coordinate.

See:
* `selectAll/1`

* `selectLine/2`
""".
-spec selectWord(This, Pos) -> 'ok' when
	This::wxHtmlWindow(), Pos::{X::integer(), Y::integer()}.
selectWord(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxHtmlWindow_SelectWord).

-doc """
This function sets the space between border of window and HTML contents.

See image:
""".
-spec setBorders(This, B) -> 'ok' when
	This::wxHtmlWindow(), B::integer().
setBorders(#wx_ref{type=ThisT}=This,B)
 when is_integer(B) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,B,?get_env(),?wxHtmlWindow_SetBorders).

-doc(#{equiv => setFonts(This,Normal_face,Fixed_face, [])}).
-spec setFonts(This, Normal_face, Fixed_face) -> 'ok' when
	This::wxHtmlWindow(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata().

setFonts(This,Normal_face,Fixed_face)
 when is_record(This, wx_ref),?is_chardata(Normal_face),?is_chardata(Fixed_face) ->
  setFonts(This,Normal_face,Fixed_face, []).

-doc """
This function sets font sizes and faces.

See `wxHtmlDCRenderer::SetFonts` (not implemented in wx) for detailed description.
""".
-spec setFonts(This, Normal_face, Fixed_face, [Option]) -> 'ok' when
	This::wxHtmlWindow(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata(),
	Option :: {'sizes', [integer()]}.
setFonts(#wx_ref{type=ThisT}=This,Normal_face,Fixed_face, Options)
 when ?is_chardata(Normal_face),?is_chardata(Fixed_face),is_list(Options) ->
  ?CLASS(ThisT,wxHtmlWindow),
  Normal_face_UC = unicode:characters_to_binary(Normal_face),
  Fixed_face_UC = unicode:characters_to_binary(Fixed_face),
  MOpts = fun({sizes, _sizes} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Normal_face_UC,Fixed_face_UC, Opts,?get_env(),?wxHtmlWindow_SetFonts).

-doc """
Sets the source of a page and displays it, for example:

If you want to load a document from some location use `loadPage/2` instead.

Return: false if an error occurred, true otherwise.
""".
-spec setPage(This, Source) -> boolean() when
	This::wxHtmlWindow(), Source::unicode:chardata().
setPage(#wx_ref{type=ThisT}=This,Source)
 when ?is_chardata(Source) ->
  ?CLASS(ThisT,wxHtmlWindow),
  Source_UC = unicode:characters_to_binary(Source),
  wxe_util:queue_cmd(This,Source_UC,?get_env(),?wxHtmlWindow_SetPage),
  wxe_util:rec(?wxHtmlWindow_SetPage).

-doc """
Sets the frame in which page title will be displayed.

`format` is the format of the frame title, e.g. "HtmlHelp : %s". It must contain exactly
one s. This s is substituted with HTML page title.
""".
-spec setRelatedFrame(This, Frame, Format) -> 'ok' when
	This::wxHtmlWindow(), Frame::wxFrame:wxFrame(), Format::unicode:chardata().
setRelatedFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame,Format)
 when ?is_chardata(Format) ->
  ?CLASS(ThisT,wxHtmlWindow),
  ?CLASS(FrameT,wxFrame),
  Format_UC = unicode:characters_to_binary(Format),
  wxe_util:queue_cmd(This,Frame,Format_UC,?get_env(),?wxHtmlWindow_SetRelatedFrame).

-doc """
`After` calling `setRelatedFrame/3`, this sets statusbar slot where messages will be
displayed.

(Default is -1 = no messages.)
""".
-spec setRelatedStatusBar(This, Statusbar) -> 'ok' when
	This::wxHtmlWindow(), Statusbar::wxStatusBar:wxStatusBar();
      (This, Index) -> 'ok' when
	This::wxHtmlWindow(), Index::integer().

setRelatedStatusBar(This,Statusbar)
 when is_record(This, wx_ref),is_record(Statusbar, wx_ref) ->
  setRelatedStatusBar(This,Statusbar, []);
setRelatedStatusBar(#wx_ref{type=ThisT}=This,Index)
 when is_integer(Index) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,Index,?get_env(),?wxHtmlWindow_SetRelatedStatusBar_1).

-doc """
`Sets` the associated statusbar where messages will be displayed.

Call this instead of `setRelatedFrame/3` if you want statusbar updates only, no changing of the frame title.

Since: 2.9.0
""".
-spec setRelatedStatusBar(This, Statusbar, [Option]) -> 'ok' when
	This::wxHtmlWindow(), Statusbar::wxStatusBar:wxStatusBar(),
	Option :: {'index', integer()}.
setRelatedStatusBar(#wx_ref{type=ThisT}=This,#wx_ref{type=StatusbarT}=Statusbar, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxHtmlWindow),
  ?CLASS(StatusbarT,wxStatusBar),
  MOpts = fun({index, _index} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Statusbar, Opts,?get_env(),?wxHtmlWindow_SetRelatedStatusBar_2).

-doc "Returns content of currently displayed page as plain text.".
-spec toText(This) -> unicode:charlist() when
	This::wxHtmlWindow().
toText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_ToText),
  wxe_util:rec(?wxHtmlWindow_ToText).

-doc "Destroys the object".
-spec destroy(This::wxHtmlWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxHtmlWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxScrolledWindow
-doc false.
setTargetWindow(This,Window) -> wxScrolledWindow:setTargetWindow(This,Window).
-doc false.
setScrollRate(This,Xstep,Ystep) -> wxScrolledWindow:setScrollRate(This,Xstep,Ystep).
-doc false.
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options).
-doc false.
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY).
-doc false.
scroll(This,X,Y) -> wxScrolledWindow:scroll(This,X,Y).
-doc false.
scroll(This,Pt) -> wxScrolledWindow:scroll(This,Pt).
-doc false.
prepareDC(This,Dc) -> wxScrolledWindow:prepareDC(This,Dc).
-doc false.
doPrepareDC(This,Dc) -> wxScrolledWindow:doPrepareDC(This,Dc).
-doc false.
getViewStart(This) -> wxScrolledWindow:getViewStart(This).
-doc false.
getScrollPixelsPerUnit(This) -> wxScrolledWindow:getScrollPixelsPerUnit(This).
-doc false.
enableScrolling(This,XScrolling,YScrolling) -> wxScrolledWindow:enableScrolling(This,XScrolling,YScrolling).
-doc false.
calcUnscrolledPosition(This,X,Y) -> wxScrolledWindow:calcUnscrolledPosition(This,X,Y).
-doc false.
calcUnscrolledPosition(This,Pt) -> wxScrolledWindow:calcUnscrolledPosition(This,Pt).
-doc false.
calcScrolledPosition(This,X,Y) -> wxScrolledWindow:calcScrolledPosition(This,X,Y).
-doc false.
calcScrolledPosition(This,Pt) -> wxScrolledWindow:calcScrolledPosition(This,Pt).
 %% From wxPanel
-doc false.
setFocusIgnoringChildren(This) -> wxPanel:setFocusIgnoringChildren(This).
-doc false.
initDialog(This) -> wxPanel:initDialog(This).
 %% From wxWindow
-doc false.
getDPI(This) -> wxWindow:getDPI(This).
-doc false.
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
-doc false.
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
-doc false.
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
-doc false.
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
-doc false.
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
-doc false.
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
-doc false.
validate(This) -> wxWindow:validate(This).
-doc false.
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
-doc false.
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
-doc false.
update(This) -> wxWindow:update(This).
-doc false.
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
-doc false.
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
-doc false.
thaw(This) -> wxWindow:thaw(This).
-doc false.
show(This, Options) -> wxWindow:show(This, Options).
-doc false.
show(This) -> wxWindow:show(This).
-doc false.
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
-doc false.
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
-doc false.
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
-doc false.
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
-doc false.
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
-doc false.
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
-doc false.
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
-doc false.
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
-doc false.
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
-doc false.
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
-doc false.
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
-doc false.
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
-doc false.
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
-doc false.
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
-doc false.
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
-doc false.
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
-doc false.
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
-doc false.
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
-doc false.
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
-doc false.
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
-doc false.
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
-doc false.
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
-doc false.
setName(This,Name) -> wxWindow:setName(This,Name).
-doc false.
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
-doc false.
setId(This,Winid) -> wxWindow:setId(This,Winid).
-doc false.
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
-doc false.
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
-doc false.
setFont(This,Font) -> wxWindow:setFont(This,Font).
-doc false.
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
-doc false.
setFocus(This) -> wxWindow:setFocus(This).
-doc false.
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
-doc false.
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
-doc false.
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
-doc false.
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
-doc false.
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
-doc false.
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
-doc false.
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
-doc false.
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
-doc false.
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
-doc false.
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
-doc false.
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
-doc false.
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
-doc false.
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
-doc false.
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
-doc false.
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
-doc false.
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
-doc false.
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
-doc false.
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
-doc false.
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
-doc false.
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
-doc false.
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
-doc false.
screenToClient(This) -> wxWindow:screenToClient(This).
-doc false.
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
-doc false.
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
-doc false.
releaseMouse(This) -> wxWindow:releaseMouse(This).
-doc false.
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
-doc false.
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
-doc false.
refresh(This, Options) -> wxWindow:refresh(This, Options).
-doc false.
refresh(This) -> wxWindow:refresh(This).
-doc false.
raise(This) -> wxWindow:raise(This).
-doc false.
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
-doc false.
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
-doc false.
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
-doc false.
pageUp(This) -> wxWindow:pageUp(This).
-doc false.
pageDown(This) -> wxWindow:pageDown(This).
-doc false.
navigate(This, Options) -> wxWindow:navigate(This, Options).
-doc false.
navigate(This) -> wxWindow:navigate(This).
-doc false.
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
-doc false.
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
-doc false.
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
-doc false.
move(This,X,Y) -> wxWindow:move(This,X,Y).
-doc false.
move(This,Pt) -> wxWindow:move(This,Pt).
-doc false.
lower(This) -> wxWindow:lower(This).
-doc false.
lineUp(This) -> wxWindow:lineUp(This).
-doc false.
lineDown(This) -> wxWindow:lineDown(This).
-doc false.
layout(This) -> wxWindow:layout(This).
-doc false.
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
-doc false.
isTopLevel(This) -> wxWindow:isTopLevel(This).
-doc false.
isShown(This) -> wxWindow:isShown(This).
-doc false.
isRetained(This) -> wxWindow:isRetained(This).
-doc false.
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
-doc false.
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
-doc false.
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
-doc false.
isEnabled(This) -> wxWindow:isEnabled(This).
-doc false.
isFrozen(This) -> wxWindow:isFrozen(This).
-doc false.
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
-doc false.
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
-doc false.
hide(This) -> wxWindow:hide(This).
-doc false.
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
-doc false.
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
-doc false.
hasCapture(This) -> wxWindow:hasCapture(This).
-doc false.
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
-doc false.
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
-doc false.
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
-doc false.
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
-doc false.
getToolTip(This) -> wxWindow:getToolTip(This).
-doc false.
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
-doc false.
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
-doc false.
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
-doc false.
getSizer(This) -> wxWindow:getSizer(This).
-doc false.
getSize(This) -> wxWindow:getSize(This).
-doc false.
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
-doc false.
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
-doc false.
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
-doc false.
getScreenRect(This) -> wxWindow:getScreenRect(This).
-doc false.
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
-doc false.
getRect(This) -> wxWindow:getRect(This).
-doc false.
getPosition(This) -> wxWindow:getPosition(This).
-doc false.
getParent(This) -> wxWindow:getParent(This).
-doc false.
getName(This) -> wxWindow:getName(This).
-doc false.
getMinSize(This) -> wxWindow:getMinSize(This).
-doc false.
getMaxSize(This) -> wxWindow:getMaxSize(This).
-doc false.
getLabel(This) -> wxWindow:getLabel(This).
-doc false.
getId(This) -> wxWindow:getId(This).
-doc false.
getHelpText(This) -> wxWindow:getHelpText(This).
-doc false.
getHandle(This) -> wxWindow:getHandle(This).
-doc false.
getGrandParent(This) -> wxWindow:getGrandParent(This).
-doc false.
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
-doc false.
getFont(This) -> wxWindow:getFont(This).
-doc false.
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
-doc false.
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
-doc false.
getDropTarget(This) -> wxWindow:getDropTarget(This).
-doc false.
getCursor(This) -> wxWindow:getCursor(This).
-doc false.
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
-doc false.
getClientSize(This) -> wxWindow:getClientSize(This).
-doc false.
getChildren(This) -> wxWindow:getChildren(This).
-doc false.
getCharWidth(This) -> wxWindow:getCharWidth(This).
-doc false.
getCharHeight(This) -> wxWindow:getCharHeight(This).
-doc false.
getCaret(This) -> wxWindow:getCaret(This).
-doc false.
getBestSize(This) -> wxWindow:getBestSize(This).
-doc false.
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
-doc false.
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
-doc false.
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
-doc false.
freeze(This) -> wxWindow:freeze(This).
-doc false.
fitInside(This) -> wxWindow:fitInside(This).
-doc false.
fit(This) -> wxWindow:fit(This).
-doc false.
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
-doc false.
enable(This, Options) -> wxWindow:enable(This, Options).
-doc false.
enable(This) -> wxWindow:enable(This).
-doc false.
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
-doc false.
disable(This) -> wxWindow:disable(This).
-doc false.
destroyChildren(This) -> wxWindow:destroyChildren(This).
-doc false.
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
-doc false.
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
-doc false.
close(This, Options) -> wxWindow:close(This, Options).
-doc false.
close(This) -> wxWindow:close(This).
-doc false.
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
-doc false.
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
-doc false.
clearBackground(This) -> wxWindow:clearBackground(This).
-doc false.
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
-doc false.
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
-doc false.
centreOnParent(This) -> wxWindow:centreOnParent(This).
-doc false.
centerOnParent(This) -> wxWindow:centerOnParent(This).
-doc false.
centre(This, Options) -> wxWindow:centre(This, Options).
-doc false.
center(This, Options) -> wxWindow:center(This, Options).
-doc false.
centre(This) -> wxWindow:centre(This).
-doc false.
center(This) -> wxWindow:center(This).
-doc false.
captureMouse(This) -> wxWindow:captureMouse(This).
-doc false.
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
 %% From wxEvtHandler
-doc false.
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
-doc false.
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
-doc false.
disconnect(This) -> wxEvtHandler:disconnect(This).
-doc false.
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
-doc false.
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).
