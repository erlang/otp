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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html">wxPrintout</a>.
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

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


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
%%   <pre>GetPageInfo(This) -> {MinPage:.integer(), MaxPage::integer(), PageFrom::integer(), PageTo::integer()}  </pre>
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

%% @spec (This::wxPrintout()) -> wxDC:wxDC()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetdc">external documentation</a>.
getDC(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetDC,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetpagesizemm">external documentation</a>.
getPageSizeMM(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPageSizeMM,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetpagesizepixels">external documentation</a>.
getPageSizePixels(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPageSizePixels,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetpaperrectpixels">external documentation</a>.
getPaperRectPixels(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPaperRectPixels,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetppiprinter">external documentation</a>.
getPPIPrinter(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPPIPrinter,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {X::integer(),Y::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetppiscreen">external documentation</a>.
getPPIScreen(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetPPIScreen,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> string()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgettitle">external documentation</a>.
getTitle(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetTitle,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutispreview">external documentation</a>.
isPreview(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_IsPreview,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout(), ImageSize::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutfitthissizetopaper">external documentation</a>.
fitThisSizeToPaper(#wx_ref{type=ThisT,ref=ThisRef},{ImageSizeW,ImageSizeH})
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_FitThisSizeToPaper,
  <<ThisRef:32/?UI,ImageSizeW:32/?UI,ImageSizeH:32/?UI>>).

%% @spec (This::wxPrintout(), ImageSize::{W::integer(),H::integer()}) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutfitthissizetopage">external documentation</a>.
fitThisSizeToPage(#wx_ref{type=ThisT,ref=ThisRef},{ImageSizeW,ImageSizeH})
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_FitThisSizeToPage,
  <<ThisRef:32/?UI,ImageSizeW:32/?UI,ImageSizeH:32/?UI>>).

%% @spec (This::wxPrintout(), ImageSize::{W::integer(),H::integer()}, PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutfitthissizetopagemargins">external documentation</a>.
fitThisSizeToPageMargins(#wx_ref{type=ThisT,ref=ThisRef},{ImageSizeW,ImageSizeH},#wx_ref{type=PageSetupDataT,ref=PageSetupDataRef})
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:cast(?wxPrintout_FitThisSizeToPageMargins,
  <<ThisRef:32/?UI,ImageSizeW:32/?UI,ImageSizeH:32/?UI,PageSetupDataRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutmapscreensizetopaper">external documentation</a>.
mapScreenSizeToPaper(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_MapScreenSizeToPaper,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutmapscreensizetopage">external documentation</a>.
mapScreenSizeToPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_MapScreenSizeToPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutmapscreensizetopagemargins">external documentation</a>.
mapScreenSizeToPageMargins(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PageSetupDataT,ref=PageSetupDataRef}) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:cast(?wxPrintout_MapScreenSizeToPageMargins,
  <<ThisRef:32/?UI,PageSetupDataRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutmapscreensizetodevice">external documentation</a>.
mapScreenSizeToDevice(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_MapScreenSizeToDevice,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetlogicalpaperrect">external documentation</a>.
getLogicalPaperRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetLogicalPaperRect,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetlogicalpagerect">external documentation</a>.
getLogicalPageRect(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:call(?wxPrintout_GetLogicalPageRect,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData()) -> {X::integer(),Y::integer(),W::integer(),H::integer()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutgetlogicalpagemarginsrect">external documentation</a>.
getLogicalPageMarginsRect(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PageSetupDataT,ref=PageSetupDataRef}) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:call(?wxPrintout_GetLogicalPageMarginsRect,
  <<ThisRef:32/?UI,PageSetupDataRef:32/?UI>>).

%% @spec (This::wxPrintout(), X::integer(), Y::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutsetlogicalorigin">external documentation</a>.
setLogicalOrigin(#wx_ref{type=ThisT,ref=ThisRef},X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_SetLogicalOrigin,
  <<ThisRef:32/?UI,X:32/?UI,Y:32/?UI>>).

%% @spec (This::wxPrintout(), Xoff::integer(), Yoff::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintout.html#wxprintoutoffsetlogicalorigin">external documentation</a>.
offsetLogicalOrigin(#wx_ref{type=ThisT,ref=ThisRef},Xoff,Yoff)
 when is_integer(Xoff),is_integer(Yoff) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:cast(?wxPrintout_OffsetLogicalOrigin,
  <<ThisRef:32/?UI,Xoff:32/?UI,Yoff:32/?UI>>).

%% @spec (This::wxPrintout()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintout),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
