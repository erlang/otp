%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2024. All Rights Reserved.
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

-module(wxHtmlWindow).
-moduledoc """
Functions for wxHtmlWindow class

`m:wxHtmlWindow` is probably the only class you will directly use unless you
want to do something special (like adding new tag handlers or MIME filters).

The purpose of this class is to display rich content pages (either local file or
downloaded via HTTP protocol) in a window based on a subset of the HTML
standard. The width of the window is constant, given in the constructor and
virtual height is changed dynamically depending on page size. Once the window is
created you can set its content by calling `setPage/2` with raw HTML,
`loadPage/2` with a `wxFileSystem` (not implemented in wx) location or
`loadFile/2` with a filename.

Note: If you want complete HTML/CSS support as well as a Javascript engine,
consider using `m:wxWebView` instead.

`m:wxHtmlWindow` uses the `m:wxImage` class for displaying images, so you need
to initialize the handlers for any image formats you use before loading a page.
See ?wxInitAllImageHandlers and `wxImage::AddHandler` (not implemented in wx).

Styles

This class supports the following styles:

See: `m:wxHtmlLinkEvent`, `wxHtmlCellEvent` (not implemented in wx)

This class is derived (and can use functions) from: `m:wxScrolledWindow`
`m:wxPanel` `m:wxWindow` `m:wxEvtHandler`

wxWidgets docs:
[wxHtmlWindow](https://docs.wxwidgets.org/3.1/classwx_html_window.html)

## Events

Event types emitted from this class: [`html_cell_clicked`](`m:wxHtmlLinkEvent`),
[`html_cell_hover`](`m:wxHtmlLinkEvent`),
[`command_html_link_clicked`](`m:wxHtmlLinkEvent`)
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
%% @hidden
-doc false.
parent_class(wxScrolledWindow) -> true;
parent_class(wxPanel) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowwxhtmlwindow">external documentation</a>.
-doc "Default ctor.".
-spec new() -> wxHtmlWindow().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxHtmlWindow_new_0),
  wxe_util:rec(?wxHtmlWindow_new_0).

%% @equiv new(Parent, [])
-spec new(Parent) -> wxHtmlWindow() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowwxhtmlwindow">external documentation</a>.
-doc """
Constructor.

The parameters are the same as `wxScrolled::wxScrolled()` (not implemented in
wx) constructor.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowappendtopage">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowgetopenedanchor">external documentation</a>.
-doc """
Returns anchor within currently opened page (see `getOpenedPage/1`).

If no page is opened or if the displayed page wasn't produced by call to
`loadPage/2`, empty string is returned.
""".
-spec getOpenedAnchor(This) -> unicode:charlist() when
	This::wxHtmlWindow().
getOpenedAnchor(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetOpenedAnchor),
  wxe_util:rec(?wxHtmlWindow_GetOpenedAnchor).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowgetopenedpage">external documentation</a>.
-doc """
Returns full location of the opened page.

If no page is opened or if the displayed page wasn't produced by call to
`loadPage/2`, empty string is returned.
""".
-spec getOpenedPage(This) -> unicode:charlist() when
	This::wxHtmlWindow().
getOpenedPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetOpenedPage),
  wxe_util:rec(?wxHtmlWindow_GetOpenedPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowgetopenedpagetitle">external documentation</a>.
-doc """
Returns title of the opened page or wxEmptyString if the current page does not
contain <TITLE> tag.
""".
-spec getOpenedPageTitle(This) -> unicode:charlist() when
	This::wxHtmlWindow().
getOpenedPageTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetOpenedPageTitle),
  wxe_util:rec(?wxHtmlWindow_GetOpenedPageTitle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowgetrelatedframe">external documentation</a>.
-doc "Returns the related frame.".
-spec getRelatedFrame(This) -> wxFrame:wxFrame() when
	This::wxHtmlWindow().
getRelatedFrame(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_GetRelatedFrame),
  wxe_util:rec(?wxHtmlWindow_GetRelatedFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowhistoryback">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowhistorycanback">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowhistorycanforward">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowhistoryclear">external documentation</a>.
-doc "Clears history.".
-spec historyClear(This) -> 'ok' when
	This::wxHtmlWindow().
historyClear(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_HistoryClear).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowhistoryforward">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowloadfile">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowloadpage">external documentation</a>.
-doc """
Unlike `setPage/2` this function first loads the HTML page from `location` and
then displays it.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowselectall">external documentation</a>.
-doc """
Selects all text in the window.

See: `selectLine/2`, `selectWord/2`
""".
-spec selectAll(This) -> 'ok' when
	This::wxHtmlWindow().
selectAll(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_SelectAll).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowselectiontotext">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowselectline">external documentation</a>.
-doc """
Selects the line of text that `pos` points at.

Note that `pos` is relative to the top of displayed page, not to window's
origin, use `wxScrolledWindow:calcUnscrolledPosition/3` to convert physical
coordinate.

See: `selectAll/1`, `selectWord/2`
""".
-spec selectLine(This, Pos) -> 'ok' when
	This::wxHtmlWindow(), Pos::{X::integer(), Y::integer()}.
selectLine(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxHtmlWindow_SelectLine).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowselectword">external documentation</a>.
-doc """
Selects the word at position `pos`.

Note that `pos` is relative to the top of displayed page, not to window's
origin, use `wxScrolledWindow:calcUnscrolledPosition/3` to convert physical
coordinate.

See: `selectAll/1`, `selectLine/2`
""".
-spec selectWord(This, Pos) -> 'ok' when
	This::wxHtmlWindow(), Pos::{X::integer(), Y::integer()}.
selectWord(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxHtmlWindow_SelectWord).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowsetborders">external documentation</a>.
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

%% @equiv setFonts(This,Normal_face,Fixed_face, [])
-spec setFonts(This, Normal_face, Fixed_face) -> 'ok' when
	This::wxHtmlWindow(), Normal_face::unicode:chardata(), Fixed_face::unicode:chardata().

setFonts(This,Normal_face,Fixed_face)
 when is_record(This, wx_ref),?is_chardata(Normal_face),?is_chardata(Fixed_face) ->
  setFonts(This,Normal_face,Fixed_face, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowsetfonts">external documentation</a>.
-doc """
This function sets font sizes and faces.

See `wxHtmlDCRenderer::SetFonts` (not implemented in wx) for detailed
description.

See: SetSize()
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowsetpage">external documentation</a>.
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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowsetrelatedframe">external documentation</a>.
-doc """
Sets the frame in which page title will be displayed.

`format` is the format of the frame title, e.g. "HtmlHelp : %s". It must contain
exactly one s. This s is substituted with HTML page title.
""".
-spec setRelatedFrame(This, Frame, Format) -> 'ok' when
	This::wxHtmlWindow(), Frame::wxFrame:wxFrame(), Format::unicode:chardata().
setRelatedFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame,Format)
 when ?is_chardata(Format) ->
  ?CLASS(ThisT,wxHtmlWindow),
  ?CLASS(FrameT,wxFrame),
  Format_UC = unicode:characters_to_binary(Format),
  wxe_util:queue_cmd(This,Frame,Format_UC,?get_env(),?wxHtmlWindow_SetRelatedFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowsetrelatedstatusbar">external documentation</a>.
%% <br /> Also:<br />
%% setRelatedStatusBar(This, Index) -> 'ok' when<br />
%% 	This::wxHtmlWindow(), Index::integer().<br />
%% 
-doc """
`After` calling `setRelatedFrame/3`, this sets statusbar slot where messages
will be displayed.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowsetrelatedstatusbar">external documentation</a>.
-doc """
`Sets` the associated statusbar where messages will be displayed.

Call this instead of `setRelatedFrame/3` if you want statusbar updates only, no
changing of the frame title.

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

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxhtmlwindow.html#wxhtmlwindowtotext">external documentation</a>.
-doc "Returns content of currently displayed page as plain text.".
-spec toText(This) -> unicode:charlist() when
	This::wxHtmlWindow().
toText(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxHtmlWindow),
  wxe_util:queue_cmd(This,?get_env(),?wxHtmlWindow_ToText),
  wxe_util:rec(?wxHtmlWindow_ToText).

%% @doc Destroys this object, do not use object again
-doc "Destroys the object.".
-spec destroy(This::wxHtmlWindow()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxHtmlWindow),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxScrolledWindow
%% @hidden
-doc false.
setTargetWindow(This,Window) -> wxScrolledWindow:setTargetWindow(This,Window).
%% @hidden
-doc false.
setScrollRate(This,Xstep,Ystep) -> wxScrolledWindow:setScrollRate(This,Xstep,Ystep).
%% @hidden
-doc false.
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY, Options).
%% @hidden
-doc false.
setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY) -> wxScrolledWindow:setScrollbars(This,PixelsPerUnitX,PixelsPerUnitY,NoUnitsX,NoUnitsY).
%% @hidden
-doc false.
scroll(This,X,Y) -> wxScrolledWindow:scroll(This,X,Y).
%% @hidden
-doc false.
scroll(This,Pt) -> wxScrolledWindow:scroll(This,Pt).
%% @hidden
-doc false.
prepareDC(This,Dc) -> wxScrolledWindow:prepareDC(This,Dc).
%% @hidden
-doc false.
doPrepareDC(This,Dc) -> wxScrolledWindow:doPrepareDC(This,Dc).
%% @hidden
-doc false.
getViewStart(This) -> wxScrolledWindow:getViewStart(This).
%% @hidden
-doc false.
getScrollPixelsPerUnit(This) -> wxScrolledWindow:getScrollPixelsPerUnit(This).
%% @hidden
-doc false.
enableScrolling(This,XScrolling,YScrolling) -> wxScrolledWindow:enableScrolling(This,XScrolling,YScrolling).
%% @hidden
-doc false.
calcUnscrolledPosition(This,X,Y) -> wxScrolledWindow:calcUnscrolledPosition(This,X,Y).
%% @hidden
-doc false.
calcUnscrolledPosition(This,Pt) -> wxScrolledWindow:calcUnscrolledPosition(This,Pt).
%% @hidden
-doc false.
calcScrolledPosition(This,X,Y) -> wxScrolledWindow:calcScrolledPosition(This,X,Y).
%% @hidden
-doc false.
calcScrolledPosition(This,Pt) -> wxScrolledWindow:calcScrolledPosition(This,Pt).
 %% From wxPanel
%% @hidden
-doc false.
setFocusIgnoringChildren(This) -> wxPanel:setFocusIgnoringChildren(This).
%% @hidden
-doc false.
initDialog(This) -> wxPanel:initDialog(This).
 %% From wxWindow
%% @hidden
-doc false.
getDPI(This) -> wxWindow:getDPI(This).
%% @hidden
-doc false.
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
%% @hidden
-doc false.
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
%% @hidden
-doc false.
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
%% @hidden
-doc false.
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
%% @hidden
-doc false.
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
%% @hidden
-doc false.
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
%% @hidden
-doc false.
validate(This) -> wxWindow:validate(This).
%% @hidden
-doc false.
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
%% @hidden
-doc false.
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
%% @hidden
-doc false.
update(This) -> wxWindow:update(This).
%% @hidden
-doc false.
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
%% @hidden
-doc false.
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
%% @hidden
-doc false.
thaw(This) -> wxWindow:thaw(This).
%% @hidden
-doc false.
show(This, Options) -> wxWindow:show(This, Options).
%% @hidden
-doc false.
show(This) -> wxWindow:show(This).
%% @hidden
-doc false.
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
%% @hidden
-doc false.
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
%% @hidden
-doc false.
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
%% @hidden
-doc false.
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
%% @hidden
-doc false.
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
%% @hidden
-doc false.
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
-doc false.
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
%% @hidden
-doc false.
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
%% @hidden
-doc false.
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
%% @hidden
-doc false.
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
%% @hidden
-doc false.
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
%% @hidden
-doc false.
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
%% @hidden
-doc false.
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
%% @hidden
-doc false.
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
%% @hidden
-doc false.
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
%% @hidden
-doc false.
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
%% @hidden
-doc false.
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
%% @hidden
-doc false.
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
%% @hidden
-doc false.
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
%% @hidden
-doc false.
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
%% @hidden
-doc false.
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
%% @hidden
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
%% @hidden
-doc false.
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
%% @hidden
-doc false.
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
-doc false.
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
-doc false.
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
%% @hidden
-doc false.
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
-doc false.
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
%% @hidden
-doc false.
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
%% @hidden
-doc false.
setFont(This,Font) -> wxWindow:setFont(This,Font).
%% @hidden
-doc false.
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
%% @hidden
-doc false.
setFocus(This) -> wxWindow:setFocus(This).
%% @hidden
-doc false.
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
%% @hidden
-doc false.
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
%% @hidden
-doc false.
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
-doc false.
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
-doc false.
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
-doc false.
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
%% @hidden
-doc false.
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
%% @hidden
-doc false.
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
%% @hidden
-doc false.
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
%% @hidden
-doc false.
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
%% @hidden
-doc false.
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
%% @hidden
-doc false.
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
%% @hidden
-doc false.
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
%% @hidden
-doc false.
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
%% @hidden
-doc false.
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
%% @hidden
-doc false.
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
%% @hidden
-doc false.
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
%% @hidden
-doc false.
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
%% @hidden
-doc false.
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
%% @hidden
-doc false.
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
%% @hidden
-doc false.
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
%% @hidden
-doc false.
screenToClient(This) -> wxWindow:screenToClient(This).
%% @hidden
-doc false.
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
%% @hidden
-doc false.
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
%% @hidden
-doc false.
releaseMouse(This) -> wxWindow:releaseMouse(This).
%% @hidden
-doc false.
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
%% @hidden
-doc false.
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
%% @hidden
-doc false.
refresh(This, Options) -> wxWindow:refresh(This, Options).
%% @hidden
-doc false.
refresh(This) -> wxWindow:refresh(This).
%% @hidden
-doc false.
raise(This) -> wxWindow:raise(This).
%% @hidden
-doc false.
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
%% @hidden
-doc false.
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
%% @hidden
-doc false.
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
%% @hidden
-doc false.
pageUp(This) -> wxWindow:pageUp(This).
%% @hidden
-doc false.
pageDown(This) -> wxWindow:pageDown(This).
%% @hidden
-doc false.
navigate(This, Options) -> wxWindow:navigate(This, Options).
%% @hidden
-doc false.
navigate(This) -> wxWindow:navigate(This).
%% @hidden
-doc false.
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
%% @hidden
-doc false.
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
%% @hidden
-doc false.
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
%% @hidden
-doc false.
move(This,X,Y) -> wxWindow:move(This,X,Y).
%% @hidden
-doc false.
move(This,Pt) -> wxWindow:move(This,Pt).
%% @hidden
-doc false.
lower(This) -> wxWindow:lower(This).
%% @hidden
-doc false.
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
-doc false.
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
-doc false.
layout(This) -> wxWindow:layout(This).
%% @hidden
-doc false.
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
%% @hidden
-doc false.
isTopLevel(This) -> wxWindow:isTopLevel(This).
%% @hidden
-doc false.
isShown(This) -> wxWindow:isShown(This).
%% @hidden
-doc false.
isRetained(This) -> wxWindow:isRetained(This).
%% @hidden
-doc false.
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
%% @hidden
-doc false.
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
%% @hidden
-doc false.
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
%% @hidden
-doc false.
isEnabled(This) -> wxWindow:isEnabled(This).
%% @hidden
-doc false.
isFrozen(This) -> wxWindow:isFrozen(This).
%% @hidden
-doc false.
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
%% @hidden
-doc false.
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
%% @hidden
-doc false.
hide(This) -> wxWindow:hide(This).
%% @hidden
-doc false.
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
%% @hidden
-doc false.
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
%% @hidden
-doc false.
hasCapture(This) -> wxWindow:hasCapture(This).
%% @hidden
-doc false.
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
%% @hidden
-doc false.
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
%% @hidden
-doc false.
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
%% @hidden
-doc false.
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
%% @hidden
-doc false.
getToolTip(This) -> wxWindow:getToolTip(This).
%% @hidden
-doc false.
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
%% @hidden
-doc false.
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
-doc false.
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
-doc false.
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
-doc false.
getSize(This) -> wxWindow:getSize(This).
%% @hidden
-doc false.
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
%% @hidden
-doc false.
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
%% @hidden
-doc false.
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
%% @hidden
-doc false.
getScreenRect(This) -> wxWindow:getScreenRect(This).
%% @hidden
-doc false.
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
%% @hidden
-doc false.
getRect(This) -> wxWindow:getRect(This).
%% @hidden
-doc false.
getPosition(This) -> wxWindow:getPosition(This).
%% @hidden
-doc false.
getParent(This) -> wxWindow:getParent(This).
%% @hidden
-doc false.
getName(This) -> wxWindow:getName(This).
%% @hidden
-doc false.
getMinSize(This) -> wxWindow:getMinSize(This).
%% @hidden
-doc false.
getMaxSize(This) -> wxWindow:getMaxSize(This).
%% @hidden
-doc false.
getLabel(This) -> wxWindow:getLabel(This).
%% @hidden
-doc false.
getId(This) -> wxWindow:getId(This).
%% @hidden
-doc false.
getHelpText(This) -> wxWindow:getHelpText(This).
%% @hidden
-doc false.
getHandle(This) -> wxWindow:getHandle(This).
%% @hidden
-doc false.
getGrandParent(This) -> wxWindow:getGrandParent(This).
%% @hidden
-doc false.
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
%% @hidden
-doc false.
getFont(This) -> wxWindow:getFont(This).
%% @hidden
-doc false.
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
%% @hidden
-doc false.
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
%% @hidden
-doc false.
getDropTarget(This) -> wxWindow:getDropTarget(This).
%% @hidden
-doc false.
getCursor(This) -> wxWindow:getCursor(This).
%% @hidden
-doc false.
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
%% @hidden
-doc false.
getClientSize(This) -> wxWindow:getClientSize(This).
%% @hidden
-doc false.
getChildren(This) -> wxWindow:getChildren(This).
%% @hidden
-doc false.
getCharWidth(This) -> wxWindow:getCharWidth(This).
%% @hidden
-doc false.
getCharHeight(This) -> wxWindow:getCharHeight(This).
%% @hidden
-doc false.
getCaret(This) -> wxWindow:getCaret(This).
%% @hidden
-doc false.
getBestSize(This) -> wxWindow:getBestSize(This).
%% @hidden
-doc false.
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
%% @hidden
-doc false.
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
%% @hidden
-doc false.
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
%% @hidden
-doc false.
freeze(This) -> wxWindow:freeze(This).
%% @hidden
-doc false.
fitInside(This) -> wxWindow:fitInside(This).
%% @hidden
-doc false.
fit(This) -> wxWindow:fit(This).
%% @hidden
-doc false.
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
%% @hidden
-doc false.
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
-doc false.
enable(This) -> wxWindow:enable(This).
%% @hidden
-doc false.
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
%% @hidden
-doc false.
disable(This) -> wxWindow:disable(This).
%% @hidden
-doc false.
destroyChildren(This) -> wxWindow:destroyChildren(This).
%% @hidden
-doc false.
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
%% @hidden
-doc false.
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
%% @hidden
-doc false.
close(This, Options) -> wxWindow:close(This, Options).
%% @hidden
-doc false.
close(This) -> wxWindow:close(This).
%% @hidden
-doc false.
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
%% @hidden
-doc false.
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
%% @hidden
-doc false.
clearBackground(This) -> wxWindow:clearBackground(This).
%% @hidden
-doc false.
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
%% @hidden
-doc false.
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
-doc false.
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
-doc false.
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
-doc false.
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
-doc false.
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
-doc false.
centre(This) -> wxWindow:centre(This).
%% @hidden
-doc false.
center(This) -> wxWindow:center(This).
%% @hidden
-doc false.
captureMouse(This) -> wxWindow:captureMouse(This).
%% @hidden
-doc false.
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
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
