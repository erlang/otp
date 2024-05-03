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

-module(wxNotebook).
-moduledoc """
Functions for wxNotebook class

This class represents a notebook control, which manages multiple windows with
associated tabs.

To use the class, create a `m:wxNotebook` object and call
`wxBookCtrlBase:addPage/4` or `wxBookCtrlBase:insertPage/5`, passing a window to
be used as the page. Do not explicitly delete the window for a page that is
currently managed by `m:wxNotebook`.

`wxNotebookPage` is a typedef for `m:wxWindow`.

Styles

This class supports the following styles:

Page backgrounds

On Windows, the default theme paints a background on the notebook's pages. If
you wish to suppress this theme, for aesthetic or performance reasons, there are
three ways of doing it. You can use `wxNB_NOPAGETHEME` to disable themed drawing
for a particular notebook, you can call `wxSystemOptions:setOption/2` to disable
it for the whole application, or you can disable it for individual pages by
using `wxWindow:setBackgroundColour/2`.

To disable themed pages globally:

Set the value to 1 to enable it again. To give a single page a solid background
that more or less fits in with the overall theme, use:

On platforms other than Windows, or if the application is not using Windows
themes, `getThemeBackgroundColour/1` will return an uninitialised colour object,
and the above code will therefore work on all platforms.

See: ?wxBookCtrl, `m:wxBookCtrlEvent`, `m:wxImageList`,
[Examples](https://docs.wxwidgets.org/3.1/page_samples.html#page_samples_notebook)

This class is derived (and can use functions) from: `m:wxBookCtrlBase`
`m:wxControl` `m:wxWindow` `m:wxEvtHandler`

wxWidgets docs:
[wxNotebook](https://docs.wxwidgets.org/3.1/classwx_notebook.html)

## Events

Event types emitted from this class:
[`command_notebook_page_changed`](`m:wxBookCtrlEvent`),
[`command_notebook_page_changing`](`m:wxBookCtrlEvent`)
""".
-include("wxe.hrl").
-export([assignImageList/2,create/3,create/4,destroy/1,getImageList/1,getPageImage/2,
  getRowCount/1,getThemeBackgroundColour/1,new/0,new/2,new/3,setImageList/2,
  setPadding/2,setPageImage/3,setPageSize/2]).

%% inherited exports
-export([addPage/3,addPage/4,advanceSelection/1,advanceSelection/2,cacheBestSize/2,
  canSetTransparent/1,captureMouse/1,center/1,center/2,centerOnParent/1,
  centerOnParent/2,centre/1,centre/2,centreOnParent/1,centreOnParent/2,
  changeSelection/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  deleteAllPages/1,deletePage/2,destroyChildren/1,disable/1,disconnect/1,
  disconnect/2,disconnect/3,dragAcceptFiles/2,enable/1,enable/2,findWindow/2,
  fit/1,fitInside/1,freeze/1,getAcceleratorTable/1,getBackgroundColour/1,
  getBackgroundStyle/1,getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,
  getChildren/1,getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,
  getCurrentPage/1,getCursor/1,getDPI/1,getDPIScaleFactor/1,getDropTarget/1,
  getExtraStyle/1,getFont/1,getForegroundColour/1,getGrandParent/1,
  getHandle/1,getHelpText/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,
  getName/1,getPage/2,getPageCount/1,getPageText/2,getParent/1,getPosition/1,
  getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,getScrollRange/2,
  getScrollThumb/2,getSelection/1,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,hitTest/2,inheritAttributes/1,
  initDialog/1,insertPage/4,insertPage/5,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popupMenu/2,
  popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,
  releaseMouse/1,removeChild/2,removePage/2,reparent/2,screenToClient/1,
  screenToClient/2,scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,
  setAcceleratorTable/2,setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,
  setCaret/2,setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPageText/3,setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,
  setScrollbar/6,setSelection/2,setSize/2,setSize/3,setSize/5,setSize/6,
  setSizeHints/2,setSizeHints/3,setSizeHints/4,setSizer/2,setSizer/3,
  setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,setToolTip/2,
  setTransparent/2,setVirtualSize/2,setVirtualSize/3,setWindowStyle/2,
  setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,show/1,
  show/2,thaw/1,transferDataFromWindow/1,transferDataToWindow/1,update/1,
  updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-type wxNotebook() :: wx:wx_object().
-export_type([wxNotebook/0]).
%% @hidden
-doc false.
parent_class(wxBookCtrlBase) -> true;
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookwxnotebook">external documentation</a>.
-doc "Constructs a notebook control.".
-spec new() -> wxNotebook().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxNotebook_new_0),
  wxe_util:rec(?wxNotebook_new_0).

%% @equiv new(Parent,Id, [])
-spec new(Parent, Id) -> wxNotebook() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookwxnotebook">external documentation</a>.
-doc """
Constructs a notebook control.

Note that sometimes you can reduce flicker by passing the wxCLIP_CHILDREN window
style.
""".
-spec new(Parent, Id, [Option]) -> wxNotebook() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxNotebook_new_3),
  wxe_util:rec(?wxNotebook_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookassignimagelist">external documentation</a>.
-doc """
Sets the image list for the page control and takes ownership of the list.

See: `m:wxImageList`, `setImageList/2`
""".
-spec assignImageList(This, ImageList) -> 'ok' when
	This::wxNotebook(), ImageList::wxImageList:wxImageList().
assignImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxNotebook),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxNotebook_AssignImageList).

%% @equiv create(This,Parent,Id, [])
-spec create(This, Parent, Id) -> boolean() when
	This::wxNotebook(), Parent::wxWindow:wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookcreate">external documentation</a>.
-doc """
Creates a notebook control.

See `new/3` for a description of the parameters.
""".
-spec create(This, Parent, Id, [Option]) -> boolean() when
	This::wxNotebook(), Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxNotebook),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id, Opts,?get_env(),?wxNotebook_Create),
  wxe_util:rec(?wxNotebook_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookgetimagelist">external documentation</a>.
-doc """
Returns the associated image list, may be NULL.

See: `m:wxImageList`, `setImageList/2`
""".
-spec getImageList(This) -> wxImageList:wxImageList() when
	This::wxNotebook().
getImageList(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,?get_env(),?wxNotebook_GetImageList),
  wxe_util:rec(?wxNotebook_GetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookgetpageimage">external documentation</a>.
-doc "Returns the image index for the given page.".
-spec getPageImage(This, NPage) -> integer() when
	This::wxNotebook(), NPage::integer().
getPageImage(#wx_ref{type=ThisT}=This,NPage)
 when is_integer(NPage) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,NPage,?get_env(),?wxNotebook_GetPageImage),
  wxe_util:rec(?wxNotebook_GetPageImage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookgetrowcount">external documentation</a>.
-doc "Returns the number of rows in the notebook control.".
-spec getRowCount(This) -> integer() when
	This::wxNotebook().
getRowCount(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,?get_env(),?wxNotebook_GetRowCount),
  wxe_util:rec(?wxNotebook_GetRowCount).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebookgetthemebackgroundcolour">external documentation</a>.
-doc """
If running under Windows and themes are enabled for the application, this
function returns a suitable colour for painting the background of a notebook
page, and can be passed to `wxWindow:setBackgroundColour/2`.

Otherwise, an uninitialised colour will be returned.
""".
-spec getThemeBackgroundColour(This) -> wx:wx_colour4() when
	This::wxNotebook().
getThemeBackgroundColour(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,?get_env(),?wxNotebook_GetThemeBackgroundColour),
  wxe_util:rec(?wxNotebook_GetThemeBackgroundColour).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebooksetimagelist">external documentation</a>.
-doc """
Sets the image list to use.

It does not take ownership of the image list, you must delete it yourself.

See: `m:wxImageList`, `assignImageList/2`
""".
-spec setImageList(This, ImageList) -> 'ok' when
	This::wxNotebook(), ImageList::wxImageList:wxImageList().
setImageList(#wx_ref{type=ThisT}=This,#wx_ref{type=ImageListT}=ImageList) ->
  ?CLASS(ThisT,wxNotebook),
  ?CLASS(ImageListT,wxImageList),
  wxe_util:queue_cmd(This,ImageList,?get_env(),?wxNotebook_SetImageList).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebooksetpadding">external documentation</a>.
-doc """
Sets the amount of space around each page's icon and label, in pixels.

Note: The vertical padding cannot be changed in wxGTK.
""".
-spec setPadding(This, Padding) -> 'ok' when
	This::wxNotebook(), Padding::{W::integer(), H::integer()}.
setPadding(#wx_ref{type=ThisT}=This,{PaddingW,PaddingH} = Padding)
 when is_integer(PaddingW),is_integer(PaddingH) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,Padding,?get_env(),?wxNotebook_SetPadding).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebooksetpagesize">external documentation</a>.
-doc """
Sets the width and height of the pages.

Note: This method is currently not implemented for wxGTK.
""".
-spec setPageSize(This, Size) -> 'ok' when
	This::wxNotebook(), Size::{W::integer(), H::integer()}.
setPageSize(#wx_ref{type=ThisT}=This,{SizeW,SizeH} = Size)
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,Size,?get_env(),?wxNotebook_SetPageSize).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxnotebook.html#wxnotebooksetpageimage">external documentation</a>.
-doc """
Sets the image index for the given page.

`image` is an index into the image list which was set with `setImageList/2`.
""".
-spec setPageImage(This, Page, Image) -> boolean() when
	This::wxNotebook(), Page::integer(), Image::integer().
setPageImage(#wx_ref{type=ThisT}=This,Page,Image)
 when is_integer(Page),is_integer(Image) ->
  ?CLASS(ThisT,wxNotebook),
  wxe_util:queue_cmd(This,Page,Image,?get_env(),?wxNotebook_SetPageImage),
  wxe_util:rec(?wxNotebook_SetPageImage).

%% @doc Destroys this object, do not use object again
-doc "Destroys the `m:wxNotebook` object.".
-spec destroy(This::wxNotebook()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxNotebook),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxBookCtrlBase
%% @hidden
-doc false.
setPageText(This,Page,Text) -> wxBookCtrlBase:setPageText(This,Page,Text).
%% @hidden
-doc false.
getPageText(This,NPage) -> wxBookCtrlBase:getPageText(This,NPage).
%% @hidden
-doc false.
hitTest(This,Pt) -> wxBookCtrlBase:hitTest(This,Pt).
%% @hidden
-doc false.
changeSelection(This,Page) -> wxBookCtrlBase:changeSelection(This,Page).
%% @hidden
-doc false.
getSelection(This) -> wxBookCtrlBase:getSelection(This).
%% @hidden
-doc false.
setSelection(This,Page) -> wxBookCtrlBase:setSelection(This,Page).
%% @hidden
-doc false.
advanceSelection(This, Options) -> wxBookCtrlBase:advanceSelection(This, Options).
%% @hidden
-doc false.
advanceSelection(This) -> wxBookCtrlBase:advanceSelection(This).
%% @hidden
-doc false.
getCurrentPage(This) -> wxBookCtrlBase:getCurrentPage(This).
%% @hidden
-doc false.
getPageCount(This) -> wxBookCtrlBase:getPageCount(This).
%% @hidden
-doc false.
getPage(This,Page) -> wxBookCtrlBase:getPage(This,Page).
%% @hidden
-doc false.
deleteAllPages(This) -> wxBookCtrlBase:deleteAllPages(This).
%% @hidden
-doc false.
removePage(This,Page) -> wxBookCtrlBase:removePage(This,Page).
%% @hidden
-doc false.
deletePage(This,Page) -> wxBookCtrlBase:deletePage(This,Page).
%% @hidden
-doc false.
insertPage(This,Index,Page,Text, Options) -> wxBookCtrlBase:insertPage(This,Index,Page,Text, Options).
%% @hidden
-doc false.
insertPage(This,Index,Page,Text) -> wxBookCtrlBase:insertPage(This,Index,Page,Text).
%% @hidden
-doc false.
addPage(This,Page,Text, Options) -> wxBookCtrlBase:addPage(This,Page,Text, Options).
%% @hidden
-doc false.
addPage(This,Page,Text) -> wxBookCtrlBase:addPage(This,Page,Text).
 %% From wxControl
%% @hidden
-doc false.
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
-doc false.
getLabel(This) -> wxControl:getLabel(This).
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
initDialog(This) -> wxWindow:initDialog(This).
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
