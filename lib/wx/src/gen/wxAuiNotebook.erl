%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2019. All Rights Reserved.
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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html">wxAuiNotebook</a>.
%% <p>This class is derived (and can use functions) from:
%% <br />{@link wxControl}
%% <br />{@link wxWindow}
%% <br />{@link wxEvtHandler}
%% </p>
%% @type wxAuiNotebook().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxAuiNotebook).
-include("wxe.hrl").
-export([addPage/3,addPage/4,create/2,create/3,deletePage/2,destroy/1,getArtProvider/1,
  getPage/2,getPageBitmap/2,getPageCount/1,getPageIndex/2,getPageText/2,
  getSelection/1,insertPage/4,insertPage/5,new/0,new/1,new/2,removePage/2,
  setArtProvider/2,setFont/2,setPageBitmap/3,setPageText/3,setSelection/2,
  setTabCtrlHeight/2,setUniformBitmapSize/2]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,fromDIP/2,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDropTarget/1,getEventHandler/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,
  getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getToolTip/1,getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,
  getWindowVariant/1,hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,
  hide/1,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,
  isEnabled/1,isExposed/2,isExposed/3,isExposed/5,isRetained/1,isShown/1,
  isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,
  makeModal/1,makeModal/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popEventHandler/1,popEventHandler/2,popupMenu/2,popupMenu/3,popupMenu/4,
  raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,releaseMouse/1,
  removeChild/2,reparent/2,screenToClient/1,screenToClient/2,scrollLines/2,
  scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setVirtualSizeHints/2,setVirtualSizeHints/3,setVirtualSizeHints/4,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  show/1,show/2,thaw/1,toDIP/2,transferDataFromWindow/1,transferDataToWindow/1,
  update/1,updateWindowUI/1,updateWindowUI/2,validate/1,warpPointer/3]).

-export_type([wxAuiNotebook/0]).
%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxAuiNotebook() :: wx:wx_object().
%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookwxauinotebook">external documentation</a>.
-spec new() -> wxAuiNotebook().
new() ->
  wxe_util:construct(?wxAuiNotebook_new_0,
  <<>>).

%% @equiv new(Parent, [])
-spec new(Parent) -> wxAuiNotebook() when
	Parent::wxWindow:wxWindow().

new(Parent)
 when is_record(Parent, wx_ref) ->
  new(Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookwxauinotebook">external documentation</a>.
-spec new(Parent, [Option]) -> wxAuiNotebook() when
	Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, Id}, Acc) -> [<<1:32/?UI,Id:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxAuiNotebook_new_2,
  <<ParentRef:32/?UI, 0:32,BinOpt/binary>>).

%% @equiv addPage(This,Page,Caption, [])
-spec addPage(This, Page, Caption) -> boolean() when
	This::wxAuiNotebook(), Page::wxWindow:wxWindow(), Caption::unicode:chardata().

addPage(This,Page,Caption)
 when is_record(This, wx_ref),is_record(Page, wx_ref),?is_chardata(Caption) ->
  addPage(This,Page,Caption, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookaddpage">external documentation</a>.
-spec addPage(This, Page, Caption, [Option]) -> boolean() when
	This::wxAuiNotebook(), Page::wxWindow:wxWindow(), Caption::unicode:chardata(),
	Option :: {'select', boolean()}
		 | {'bitmap', wxBitmap:wxBitmap()}.
addPage(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PageT,ref=PageRef},Caption, Options)
 when ?is_chardata(Caption),is_list(Options) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(PageT,wxWindow),
  Caption_UC = unicode:characters_to_binary([Caption,0]),
  MOpts = fun({select, Select}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Select)):32/?UI>>|Acc];
          ({bitmap, #wx_ref{type=BitmapT,ref=BitmapRef}}, Acc) ->   ?CLASS(BitmapT,wxBitmap),[<<2:32/?UI,BitmapRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiNotebook_AddPage,
  <<ThisRef:32/?UI,PageRef:32/?UI,(byte_size(Caption_UC)):32/?UI,(Caption_UC)/binary, 0:(((8- ((4+byte_size(Caption_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @equiv create(This,Parent, [])
-spec create(This, Parent) -> boolean() when
	This::wxAuiNotebook(), Parent::wxWindow:wxWindow().

create(This,Parent)
 when is_record(This, wx_ref),is_record(Parent, wx_ref) ->
  create(This,Parent, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookcreate">external documentation</a>.
-spec create(This, Parent, [Option]) -> boolean() when
	This::wxAuiNotebook(), Parent::wxWindow:wxWindow(),
	Option :: {'id', integer()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ParentT,ref=ParentRef}, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({id, Id}, Acc) -> [<<1:32/?UI,Id:32/?UI>>|Acc];
          ({pos, {PosX,PosY}}, Acc) -> [<<2:32/?UI,PosX:32/?UI,PosY:32/?UI,0:32>>|Acc];
          ({size, {SizeW,SizeH}}, Acc) -> [<<3:32/?UI,SizeW:32/?UI,SizeH:32/?UI,0:32>>|Acc];
          ({style, Style}, Acc) -> [<<4:32/?UI,Style:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiNotebook_Create,
  <<ThisRef:32/?UI,ParentRef:32/?UI, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookdeletepage">external documentation</a>.
-spec deletePage(This, Page) -> boolean() when
	This::wxAuiNotebook(), Page::integer().
deletePage(#wx_ref{type=ThisT,ref=ThisRef},Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_DeletePage,
  <<ThisRef:32/?UI,Page:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetartprovider">external documentation</a>.
-spec getArtProvider(This) -> wxAuiTabArt:wxAuiTabArt() when
	This::wxAuiNotebook().
getArtProvider(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_GetArtProvider,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetpage">external documentation</a>.
-spec getPage(This, Page_idx) -> wxWindow:wxWindow() when
	This::wxAuiNotebook(), Page_idx::integer().
getPage(#wx_ref{type=ThisT,ref=ThisRef},Page_idx)
 when is_integer(Page_idx) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_GetPage,
  <<ThisRef:32/?UI,Page_idx:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetpagebitmap">external documentation</a>.
-spec getPageBitmap(This, Page_idx) -> wxBitmap:wxBitmap() when
	This::wxAuiNotebook(), Page_idx::integer().
getPageBitmap(#wx_ref{type=ThisT,ref=ThisRef},Page_idx)
 when is_integer(Page_idx) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_GetPageBitmap,
  <<ThisRef:32/?UI,Page_idx:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetpagecount">external documentation</a>.
-spec getPageCount(This) -> integer() when
	This::wxAuiNotebook().
getPageCount(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_GetPageCount,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetpageindex">external documentation</a>.
-spec getPageIndex(This, Page_wnd) -> integer() when
	This::wxAuiNotebook(), Page_wnd::wxWindow:wxWindow().
getPageIndex(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=Page_wndT,ref=Page_wndRef}) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(Page_wndT,wxWindow),
  wxe_util:call(?wxAuiNotebook_GetPageIndex,
  <<ThisRef:32/?UI,Page_wndRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetpagetext">external documentation</a>.
-spec getPageText(This, Page_idx) -> unicode:charlist() when
	This::wxAuiNotebook(), Page_idx::integer().
getPageText(#wx_ref{type=ThisT,ref=ThisRef},Page_idx)
 when is_integer(Page_idx) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_GetPageText,
  <<ThisRef:32/?UI,Page_idx:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookgetselection">external documentation</a>.
-spec getSelection(This) -> integer() when
	This::wxAuiNotebook().
getSelection(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_GetSelection,
  <<ThisRef:32/?UI>>).

%% @equiv insertPage(This,Page_idx,Page,Caption, [])
-spec insertPage(This, Page_idx, Page, Caption) -> boolean() when
	This::wxAuiNotebook(), Page_idx::integer(), Page::wxWindow:wxWindow(), Caption::unicode:chardata().

insertPage(This,Page_idx,Page,Caption)
 when is_record(This, wx_ref),is_integer(Page_idx),is_record(Page, wx_ref),?is_chardata(Caption) ->
  insertPage(This,Page_idx,Page,Caption, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookinsertpage">external documentation</a>.
-spec insertPage(This, Page_idx, Page, Caption, [Option]) -> boolean() when
	This::wxAuiNotebook(), Page_idx::integer(), Page::wxWindow:wxWindow(), Caption::unicode:chardata(),
	Option :: {'select', boolean()}
		 | {'bitmap', wxBitmap:wxBitmap()}.
insertPage(#wx_ref{type=ThisT,ref=ThisRef},Page_idx,#wx_ref{type=PageT,ref=PageRef},Caption, Options)
 when is_integer(Page_idx),?is_chardata(Caption),is_list(Options) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(PageT,wxWindow),
  Caption_UC = unicode:characters_to_binary([Caption,0]),
  MOpts = fun({select, Select}, Acc) -> [<<1:32/?UI,(wxe_util:from_bool(Select)):32/?UI>>|Acc];
          ({bitmap, #wx_ref{type=BitmapT,ref=BitmapRef}}, Acc) ->   ?CLASS(BitmapT,wxBitmap),[<<2:32/?UI,BitmapRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:call(?wxAuiNotebook_InsertPage,
  <<ThisRef:32/?UI,Page_idx:32/?UI,PageRef:32/?UI,(byte_size(Caption_UC)):32/?UI,(Caption_UC)/binary, 0:(((8- ((0+byte_size(Caption_UC)) band 16#7)) band 16#7))/unit:8, BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebookremovepage">external documentation</a>.
-spec removePage(This, Page) -> boolean() when
	This::wxAuiNotebook(), Page::integer().
removePage(#wx_ref{type=ThisT,ref=ThisRef},Page)
 when is_integer(Page) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_RemovePage,
  <<ThisRef:32/?UI,Page:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksetartprovider">external documentation</a>.
-spec setArtProvider(This, Art) -> 'ok' when
	This::wxAuiNotebook(), Art::wxAuiTabArt:wxAuiTabArt().
setArtProvider(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=ArtT,ref=ArtRef}) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(ArtT,wxAuiTabArt),
  wxe_util:cast(?wxAuiNotebook_SetArtProvider,
  <<ThisRef:32/?UI,ArtRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksetfont">external documentation</a>.
-spec setFont(This, Font) -> boolean() when
	This::wxAuiNotebook(), Font::wxFont:wxFont().
setFont(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FontT,ref=FontRef}) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(FontT,wxFont),
  wxe_util:call(?wxAuiNotebook_SetFont,
  <<ThisRef:32/?UI,FontRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksetpagebitmap">external documentation</a>.
-spec setPageBitmap(This, Page, Bitmap) -> boolean() when
	This::wxAuiNotebook(), Page::integer(), Bitmap::wxBitmap:wxBitmap().
setPageBitmap(#wx_ref{type=ThisT,ref=ThisRef},Page,#wx_ref{type=BitmapT,ref=BitmapRef})
 when is_integer(Page) ->
  ?CLASS(ThisT,wxAuiNotebook),
  ?CLASS(BitmapT,wxBitmap),
  wxe_util:call(?wxAuiNotebook_SetPageBitmap,
  <<ThisRef:32/?UI,Page:32/?UI,BitmapRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksetpagetext">external documentation</a>.
-spec setPageText(This, Page, Text) -> boolean() when
	This::wxAuiNotebook(), Page::integer(), Text::unicode:chardata().
setPageText(#wx_ref{type=ThisT,ref=ThisRef},Page,Text)
 when is_integer(Page),?is_chardata(Text) ->
  ?CLASS(ThisT,wxAuiNotebook),
  Text_UC = unicode:characters_to_binary([Text,0]),
  wxe_util:call(?wxAuiNotebook_SetPageText,
  <<ThisRef:32/?UI,Page:32/?UI,(byte_size(Text_UC)):32/?UI,(Text_UC)/binary, 0:(((8- ((4+byte_size(Text_UC)) band 16#7)) band 16#7))/unit:8>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksetselection">external documentation</a>.
-spec setSelection(This, New_page) -> integer() when
	This::wxAuiNotebook(), New_page::integer().
setSelection(#wx_ref{type=ThisT,ref=ThisRef},New_page)
 when is_integer(New_page) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:call(?wxAuiNotebook_SetSelection,
  <<ThisRef:32/?UI,New_page:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksettabctrlheight">external documentation</a>.
-spec setTabCtrlHeight(This, Height) -> 'ok' when
	This::wxAuiNotebook(), Height::integer().
setTabCtrlHeight(#wx_ref{type=ThisT,ref=ThisRef},Height)
 when is_integer(Height) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:cast(?wxAuiNotebook_SetTabCtrlHeight,
  <<ThisRef:32/?UI,Height:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxauinotebook.html#wxauinotebooksetuniformbitmapsize">external documentation</a>.
-spec setUniformBitmapSize(This, Size) -> 'ok' when
	This::wxAuiNotebook(), Size::{W::integer(), H::integer()}.
setUniformBitmapSize(#wx_ref{type=ThisT,ref=ThisRef},{SizeW,SizeH})
 when is_integer(SizeW),is_integer(SizeH) ->
  ?CLASS(ThisT,wxAuiNotebook),
  wxe_util:cast(?wxAuiNotebook_SetUniformBitmapSize,
  <<ThisRef:32/?UI,SizeW:32/?UI,SizeH:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxAuiNotebook()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxAuiNotebook),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
 %% From wxWindow
%% @hidden
toDIP(This,Sz) -> wxWindow:toDIP(This,Sz).
%% @hidden
fromDIP(This,Sz) -> wxWindow:fromDIP(This,Sz).
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
