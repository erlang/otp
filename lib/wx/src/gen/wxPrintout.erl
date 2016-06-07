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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html">wxPrintout</a>.
%% @type wxPrintout().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPrintout).
-include("wxe.hrl").
-export([ new/2,new/3 ,destroy/1,fitThisSizeToPage/2,fitThisSizeToPageMargins/3,
  fitThisSizeToPaper/2,getDC/1,getLogicalPageMarginsRect/2,getLogicalPageRect/1,
  getLogicalPaperRect/1,getPPIPrinter/1,getPPIScreen/1,getPageSizeMM/1,
  getPageSizePixels/1,getPaperRectPixels/1,getTitle/1,isPreview/1,mapScreenSizeToDevice/1,
  mapScreenSizeToPage/1,mapScreenSizeToPageMargins/2,mapScreenSizeToPaper/1,
  offsetLogicalOrigin/3,setLogicalOrigin/3]).

%% inherited exports
-export([parent_class/1]).

-export_type([wxPrintout/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPrintout() :: wx:wx_object().

%% @spec (Title::string(), OnPrintPage::function()) -> wxPrintout:wxPrintout()
%% @doc @equiv new(Title, OnPrintPage, [])
new(Title, OnPrintPage) ->
    new(Title, OnPrintPage, []).

%% @spec (Title::string(), OnPrintPage::function(), [Option]) -> wxPrintout:wxPrintout()
%% Option = {onPreparePrinting, OnPreparePrinting::function()} | 
%%          {onBeginPrinting,   OnBeginPrinting::function()} | 
%%          {onEndPrinting,     OnEndPrinting::function()} | 
%%          {onBeginDocument,   OnBeginDocument::function()} | 
%%          {onEndDocument,     OnEndDocument::function()} | 
%%          {hasPage,           HasPage::function()} | 
%%          {getPageInfo,       GetPageInfo::function()}
%% @doc Creates a wxPrintout object with a callback fun and optionally other callback funs.<br />
%%   <pre>OnPrintPage(This,Page) -> boolean() </pre>
%%   <pre>OnPreparePrinting(This) -> term()   </pre>
%%   <pre>OnBeginPrinting(This) -> term()   </pre>
%%   <pre>OnEndPrinting(This) -> term()   </pre>
%%   <pre>OnBeginDocument(This,StartPage,EndPage) -> boolean()  </pre>
%%   <pre>OnEndDocument(This) -> term()  </pre>
%%   <pre>HasPage(This,Page)} -> boolean()   </pre>
%%   <pre>GetPageInfo(This) -> {MinPage::integer(), MaxPage::integer(),
%%                              PageFrom::integer(), PageTo::integer()}  </pre>
%%  The <b>This</b> argument is the wxPrintout object reference to this object
%%  <br /> NOTE: The callbacks may not call other processes. 
new(Title, OnPrintPage, Opts) when is_list(Title), is_function(OnPrintPage), is_list(Opts) ->
    OnPrint = fun([This,Page]) -> 
		      Bool = OnPrintPage(This,Page), 
		      <<(wxe_util:from_bool(Bool)):32/?UI>>
	      end,
    OnPrintPageId = wxe_util:get_cbId(OnPrint),
    MOpts = fun({onPreparePrinting, F},Acc) when is_function(F) ->
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>> 
			  end,
		    [<<1:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onBeginPrinting, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>>
			  end,
		    [<<2:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onEndPrinting, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>> 
			  end,
		    [<<3:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onBeginDocument, F},Acc) when is_function(F) -> 
		    Fun = fun([This,S,E]) -> 
				  BegD = F(This,S,E), 
				  <<(wxe_util:from_bool(BegD)):32/?UI>>
			  end,
		    [<<4:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({onEndDocument, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) -> 
				  F(This), 
				  <<>> 
			  end,
		    [<<5:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({hasPage, F},Acc) when is_function(F) -> 
		    Fun = fun([This,Page]) -> 
				  HasP = F(This,Page),
				  <<(wxe_util:from_bool(HasP)):32/?UI>>
			  end,
		    [<<6:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc];
	       ({getPageInfo, F},Acc) when is_function(F) -> 
		    Fun = fun([This]) ->
				  {Min,Max,PF,PT} = F(This),
				  <<Min:32/?UI,Max:32/?UI,PF:32/?UI,PT:32/?UI>>
			  end,
		    [<<7:32/?UI,(wxe_util:get_cbId(Fun)):32/?UI>>|Acc]
	    end,
    BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Opts)),
    Title_UC = unicode:characters_to_binary([Title,0]),
    wxe_util:call(?wxPrintout_new, << (byte_size(Title_UC)):32/?UI,Title_UC/binary,
			  0:(((8- ((4+byte_size(Title_UC)) band 16#7)) band 16#7))/unit:8,
			  OnPrintPageId:32/?UI,
			  BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetdc">external documentation</a>.
-spec getDC(This) -> wxDC:wxDC() when
	This::wxPrintout().
getDC(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetDC,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpagesizemm">external documentation</a>.
-spec getPageSizeMM(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPageSizeMM(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPageSizeMM,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpagesizepixels">external documentation</a>.
-spec getPageSizePixels(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPageSizePixels(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPageSizePixels,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpaperrectpixels">external documentation</a>.
-spec getPaperRectPixels(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getPaperRectPixels(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPaperRectPixels,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetppiprinter">external documentation</a>.
-spec getPPIPrinter(This) -> {X::integer(), Y::integer()} when
	This::wxPrintout().
getPPIPrinter(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPPIPrinter,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetppiscreen">external documentation</a>.
-spec getPPIScreen(This) -> {X::integer(), Y::integer()} when
	This::wxPrintout().
getPPIScreen(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPPIScreen,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgettitle">external documentation</a>.
-spec getTitle(This) -> unicode:charlist() when
	This::wxPrintout().
getTitle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetTitle,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutispreview">external documentation</a>.
-spec isPreview(This) -> boolean() when
	This::wxPrintout().
isPreview(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_IsPreview,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopaper">external documentation</a>.
-spec fitThisSizeToPaper(This, ImageSize) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}.
fitThisSizeToPaper(#wx_ref{type=ThisT,ref=ThisRef},{ImageSizeW,ImageSizeH})
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_FitThisSizeToPaper,
  <<ThisRef:32/?UI,ImageSizeW:32/?UI,ImageSizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopage">external documentation</a>.
-spec fitThisSizeToPage(This, ImageSize) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}.
fitThisSizeToPage(#wx_ref{type=ThisT,ref=ThisRef},{ImageSizeW,ImageSizeH})
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_FitThisSizeToPage,
  <<ThisRef:32/?UI,ImageSizeW:32/?UI,ImageSizeH:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopagemargins">external documentation</a>.
-spec fitThisSizeToPageMargins(This, ImageSize, PageSetupData) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}, PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
fitThisSizeToPageMargins(#wx_ref{type=ThisT,ref=ThisRef},{ImageSizeW,ImageSizeH},#wx_ref{type=PageSetupDataT,ref=PageSetupDataRef})
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:cast(?wxPrintout_FitThisSizeToPageMargins,
  <<ThisRef:32/?UI,ImageSizeW:32/?UI,ImageSizeH:32/?UI,PageSetupDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopaper">external documentation</a>.
-spec mapScreenSizeToPaper(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToPaper(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_MapScreenSizeToPaper,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopage">external documentation</a>.
-spec mapScreenSizeToPage(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_MapScreenSizeToPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopagemargins">external documentation</a>.
-spec mapScreenSizeToPageMargins(This, PageSetupData) -> 'ok' when
	This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
mapScreenSizeToPageMargins(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PageSetupDataT,ref=PageSetupDataRef}) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:cast(?wxPrintout_MapScreenSizeToPageMargins,
  <<ThisRef:32/?UI,PageSetupDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetodevice">external documentation</a>.
-spec mapScreenSizeToDevice(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToDevice(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_MapScreenSizeToDevice,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpaperrect">external documentation</a>.
-spec getLogicalPaperRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getLogicalPaperRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetLogicalPaperRect,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpagerect">external documentation</a>.
-spec getLogicalPageRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getLogicalPageRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetLogicalPageRect,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpagemarginsrect">external documentation</a>.
-spec getLogicalPageMarginsRect(This, PageSetupData) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
getLogicalPageMarginsRect(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PageSetupDataT,ref=PageSetupDataRef}) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:call(?wxPrintout_GetLogicalPageMarginsRect,
  <<ThisRef:32/?UI,PageSetupDataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutsetlogicalorigin">external documentation</a>.
-spec setLogicalOrigin(This, X, Y) -> 'ok' when
	This::wxPrintout(), X::integer(), Y::integer().
setLogicalOrigin(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_SetLogicalOrigin,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutoffsetlogicalorigin">external documentation</a>.
-spec offsetLogicalOrigin(This, Xoff, Yoff) -> 'ok' when
	This::wxPrintout(), Xoff::integer(), Yoff::integer().
offsetLogicalOrigin(#wx_ref{type=ThisT,ref=ThisRef},Xoff,Yoff)
 when is_integer(Xoff),is_integer(Yoff) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_OffsetLogicalOrigin,
  <<ThisRef:32/?UI,Xoff:32/?UI,Yoff:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintout()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintout),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
