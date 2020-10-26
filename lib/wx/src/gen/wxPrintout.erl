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

-type wxPrintout() :: wx:wx_object().
-export_type([wxPrintout/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


%% @doc @equiv new(Title, OnPrintPage, [])
-spec new(Title::string(), OnPrintPage::function()) -> wxPrintout:wxPrintout().
new(Title, OnPrintPage) ->
    new(Title, OnPrintPage, []).

-spec new(Title::string(), OnPrintPage, [Option]) ->
          wxPrintout:wxPrintout() when
      OnPrintPage :: fun((wxPrintout(), Page::integer()) -> boolean()),
      Option ::{onPreparePrinting, fun((wxPrintout()) -> ok)} |
               {onBeginPrinting,   fun((wxPrintout()) -> ok)} |
               {onEndPrinting,     fun((wxPrintout()) -> ok)} |
               {onBeginDocument,   fun((wxPrintout(), StartPage::integer(), EndPage::integer()) -> boolean())} |
               {onEndDocument,     fun((wxPrintout()) -> ok)} |
               {hasPage,           fun((wxPrintout(), Page::integer()) -> ok)} |
               {getPageInfo,       fun((wxPrintout()) ->
                                              {MinPage::integer(), MaxPage::integer(),
                                               PageFrom::integer(), PageTo::integer()})}.

new(Title, OnPrintPage, Opts) when is_list(Title), is_function(OnPrintPage), is_list(Opts) ->
    OnPrintPageId = wxe_util:get_cbId(OnPrintPage),
    MOpts = fun({onPreparePrinting, F},Acc) when is_function(F) ->
		    [{onPreparePrinting, wxe_util:get_cbId(F)}|Acc];
	       ({onBeginPrinting, F},Acc) when is_function(F) ->
		    [{onBeginPrinting, wxe_util:get_cbId(F)}|Acc];
	       ({onEndPrinting, F},Acc) when is_function(F) ->
		    [{onEndPrinting, wxe_util:get_cbId(F)}|Acc];
	       ({onBeginDocument, F},Acc) when is_function(F) ->
		    [{onBeginDocument, wxe_util:get_cbId(F)}|Acc];
	       ({onEndDocument, F},Acc) when is_function(F) ->
		    [{onEndDocument, wxe_util:get_cbId(F)}|Acc];
	       ({hasPage, F},Acc) when is_function(F) ->
		    [{hasPage, wxe_util:get_cbId(F)}|Acc];
	       ({getPageInfo, F},Acc) when is_function(F) ->
		    [{getPageInfo,wxe_util:get_cbId(F)}|Acc]
	    end,
    OptsMod = lists:foldl(MOpts, [], Opts),
    Title_UC = unicode:characters_to_binary(Title),
    Op = ?wxPrintout_new,
    wxe_util:queue_cmd(Title_UC, OnPrintPageId, OptsMod, ?get_env(), Op),
    wxe_util:rec(Op).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetdc">external documentation</a>.
-spec getDC(This) -> wxDC:wxDC() when
	This::wxPrintout().
getDC(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetDC),
  wxe_util:rec(?wxPrintout_GetDC).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpagesizemm">external documentation</a>.
-spec getPageSizeMM(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPageSizeMM(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPageSizeMM),
  wxe_util:rec(?wxPrintout_GetPageSizeMM).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpagesizepixels">external documentation</a>.
-spec getPageSizePixels(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPageSizePixels(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPageSizePixels),
  wxe_util:rec(?wxPrintout_GetPageSizePixels).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpaperrectpixels">external documentation</a>.
-spec getPaperRectPixels(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getPaperRectPixels(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPaperRectPixels),
  wxe_util:rec(?wxPrintout_GetPaperRectPixels).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetppiprinter">external documentation</a>.
-spec getPPIPrinter(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPPIPrinter(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPPIPrinter),
  wxe_util:rec(?wxPrintout_GetPPIPrinter).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetppiscreen">external documentation</a>.
-spec getPPIScreen(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPPIScreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPPIScreen),
  wxe_util:rec(?wxPrintout_GetPPIScreen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgettitle">external documentation</a>.
-spec getTitle(This) -> unicode:charlist() when
	This::wxPrintout().
getTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetTitle),
  wxe_util:rec(?wxPrintout_GetTitle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutispreview">external documentation</a>.
-spec isPreview(This) -> boolean() when
	This::wxPrintout().
isPreview(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_IsPreview),
  wxe_util:rec(?wxPrintout_IsPreview).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopaper">external documentation</a>.
-spec fitThisSizeToPaper(This, ImageSize) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}.
fitThisSizeToPaper(#wx_ref{type=ThisT}=This,{ImageSizeW,ImageSizeH} = ImageSize)
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,ImageSize,?get_env(),?wxPrintout_FitThisSizeToPaper).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopage">external documentation</a>.
-spec fitThisSizeToPage(This, ImageSize) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}.
fitThisSizeToPage(#wx_ref{type=ThisT}=This,{ImageSizeW,ImageSizeH} = ImageSize)
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,ImageSize,?get_env(),?wxPrintout_FitThisSizeToPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopagemargins">external documentation</a>.
-spec fitThisSizeToPageMargins(This, ImageSize, PageSetupData) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}, PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
fitThisSizeToPageMargins(#wx_ref{type=ThisT}=This,{ImageSizeW,ImageSizeH} = ImageSize,#wx_ref{type=PageSetupDataT}=PageSetupData)
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,ImageSize,PageSetupData,?get_env(),?wxPrintout_FitThisSizeToPageMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopaper">external documentation</a>.
-spec mapScreenSizeToPaper(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToPaper(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_MapScreenSizeToPaper).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopage">external documentation</a>.
-spec mapScreenSizeToPage(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_MapScreenSizeToPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopagemargins">external documentation</a>.
-spec mapScreenSizeToPageMargins(This, PageSetupData) -> 'ok' when
	This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
mapScreenSizeToPageMargins(#wx_ref{type=ThisT}=This,#wx_ref{type=PageSetupDataT}=PageSetupData) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,PageSetupData,?get_env(),?wxPrintout_MapScreenSizeToPageMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetodevice">external documentation</a>.
-spec mapScreenSizeToDevice(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToDevice(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_MapScreenSizeToDevice).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpaperrect">external documentation</a>.
-spec getLogicalPaperRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getLogicalPaperRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetLogicalPaperRect),
  wxe_util:rec(?wxPrintout_GetLogicalPaperRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpagerect">external documentation</a>.
-spec getLogicalPageRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getLogicalPageRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetLogicalPageRect),
  wxe_util:rec(?wxPrintout_GetLogicalPageRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpagemarginsrect">external documentation</a>.
-spec getLogicalPageMarginsRect(This, PageSetupData) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
getLogicalPageMarginsRect(#wx_ref{type=ThisT}=This,#wx_ref{type=PageSetupDataT}=PageSetupData) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,PageSetupData,?get_env(),?wxPrintout_GetLogicalPageMarginsRect),
  wxe_util:rec(?wxPrintout_GetLogicalPageMarginsRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutsetlogicalorigin">external documentation</a>.
-spec setLogicalOrigin(This, X, Y) -> 'ok' when
	This::wxPrintout(), X::integer(), Y::integer().
setLogicalOrigin(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxPrintout_SetLogicalOrigin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutoffsetlogicalorigin">external documentation</a>.
-spec offsetLogicalOrigin(This, Xoff, Yoff) -> 'ok' when
	This::wxPrintout(), Xoff::integer(), Yoff::integer().
offsetLogicalOrigin(#wx_ref{type=ThisT}=This,Xoff,Yoff)
 when is_integer(Xoff),is_integer(Yoff) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,Xoff,Yoff,?get_env(),?wxPrintout_OffsetLogicalOrigin).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintout()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintout),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
