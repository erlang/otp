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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html">wxPrintPreview</a>.
%% @type wxPrintPreview().  An object reference, The representation is internal
%% and can be changed without notice. It can't be used for comparsion
%% stored on disc or distributed for use on other nodes.

-module(wxPrintPreview).
-include("wxe.hrl").
-export([destroy/1,getCanvas/1,getCurrentPage/1,getFrame/1,getMaxPage/1,getMinPage/1,
  getPrintout/1,getPrintoutForPrinting/1,isOk/1,new/1,new/2,new/3,paintPage/3,
  print/2,renderPage/2,setCanvas/2,setCurrentPage/2,setFrame/2,setPrintout/2,
  setZoom/2]).

%% inherited exports
-export([parent_class/1]).

%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @spec (Printout::wxPrintout:wxPrintout()) -> wxPrintPreview()
%% @equiv new(Printout, [])
new(Printout)
 when is_record(Printout, wx_ref) ->
  new(Printout, []).

%% @spec (Printout::wxPrintout:wxPrintout(), [Option]) -> wxPrintPreview()
%% Option = {printoutForPrinting, wxPrintout:wxPrintout()} | {data, wxPrintDialogData:wxPrintDialogData()}
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewwxprintpreview">external documentation</a>.
new(#wx_ref{type=PrintoutT,ref=PrintoutRef}, Options)
 when is_list(Options) ->
  ?CLASS(PrintoutT,wxPrintout),
  MOpts = fun({printoutForPrinting, #wx_ref{type=PrintoutForPrintingT,ref=PrintoutForPrintingRef}}, Acc) ->   ?CLASS(PrintoutForPrintingT,wxPrintout),[<<1:32/?UI,PrintoutForPrintingRef:32/?UI>>|Acc];
          ({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wxPrintDialogData),[<<2:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPrintPreview_new_2,
  <<PrintoutRef:32/?UI, 0:32,BinOpt/binary>>).

%% @spec (Printout::wxPrintout:wxPrintout(), PrintoutForPrinting::wxPrintout:wxPrintout(), Data::wxPrintData:wxPrintData()) -> wxPrintPreview()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewwxprintpreview">external documentation</a>.
new(#wx_ref{type=PrintoutT,ref=PrintoutRef},#wx_ref{type=PrintoutForPrintingT,ref=PrintoutForPrintingRef},#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(PrintoutT,wxPrintout),
  ?CLASS(PrintoutForPrintingT,wxPrintout),
  ?CLASS(DataT,wxPrintData),
  wxe_util:construct(?wxPrintPreview_new_3,
  <<PrintoutRef:32/?UI,PrintoutForPrintingRef:32/?UI,DataRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> wxPreviewCanvas:wxPreviewCanvas()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetcanvas">external documentation</a>.
getCanvas(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetCanvas,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetcurrentpage">external documentation</a>.
getCurrentPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetCurrentPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> wxFrame:wxFrame()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetframe">external documentation</a>.
getFrame(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetFrame,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetmaxpage">external documentation</a>.
getMaxPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetMaxPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> integer()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetminpage">external documentation</a>.
getMinPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetMinPage,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> wxPrintout:wxPrintout()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetprintout">external documentation</a>.
getPrintout(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetPrintout,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> wxPrintout:wxPrintout()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewgetprintoutforprinting">external documentation</a>.
getPrintoutForPrinting(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetPrintoutForPrinting,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewisok">external documentation</a>.
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_IsOk,
  <<ThisRef:32/?UI>>).

%% @spec (This::wxPrintPreview(), Canvas::wxPreviewCanvas:wxPreviewCanvas(), Dc::wxDC:wxDC()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewpaintpage">external documentation</a>.
paintPage(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CanvasT,ref=CanvasRef},#wx_ref{type=DcT,ref=DcRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(CanvasT,wxPreviewCanvas),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxPrintPreview_PaintPage,
  <<ThisRef:32/?UI,CanvasRef:32/?UI,DcRef:32/?UI>>).

%% @spec (This::wxPrintPreview(), Interactive::bool()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewprint">external documentation</a>.
print(#wx_ref{type=ThisT,ref=ThisRef},Interactive)
 when is_boolean(Interactive) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_Print,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Interactive)):32/?UI>>).

%% @spec (This::wxPrintPreview(), PageNum::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewrenderpage">external documentation</a>.
renderPage(#wx_ref{type=ThisT,ref=ThisRef},PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_RenderPage,
  <<ThisRef:32/?UI,PageNum:32/?UI>>).

%% @spec (This::wxPrintPreview(), Canvas::wxPreviewCanvas:wxPreviewCanvas()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewsetcanvas">external documentation</a>.
setCanvas(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CanvasT,ref=CanvasRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(CanvasT,wxPreviewCanvas),
  wxe_util:cast(?wxPrintPreview_SetCanvas,
  <<ThisRef:32/?UI,CanvasRef:32/?UI>>).

%% @spec (This::wxPrintPreview(), PageNum::integer()) -> bool()
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewsetcurrentpage">external documentation</a>.
setCurrentPage(#wx_ref{type=ThisT,ref=ThisRef},PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_SetCurrentPage,
  <<ThisRef:32/?UI,PageNum:32/?UI>>).

%% @spec (This::wxPrintPreview(), Frame::wxFrame:wxFrame()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewsetframe">external documentation</a>.
setFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(FrameT,wxFrame),
  wxe_util:cast(?wxPrintPreview_SetFrame,
  <<ThisRef:32/?UI,FrameRef:32/?UI>>).

%% @spec (This::wxPrintPreview(), Printout::wxPrintout:wxPrintout()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewsetprintout">external documentation</a>.
setPrintout(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PrintoutT,ref=PrintoutRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:cast(?wxPrintPreview_SetPrintout,
  <<ThisRef:32/?UI,PrintoutRef:32/?UI>>).

%% @spec (This::wxPrintPreview(), Percent::integer()) -> ok
%% @doc See <a href="http://www.wxwidgets.org/manuals/stable/wx_wxprintpreview.html#wxprintpreviewsetzoom">external documentation</a>.
setZoom(#wx_ref{type=ThisT,ref=ThisRef},Percent)
 when is_integer(Percent) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:cast(?wxPrintPreview_SetZoom,
  <<ThisRef:32/?UI,Percent:32/?UI>>).

%% @spec (This::wxPrintPreview()) -> ok
%% @doc Destroys this object, do not use object again
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintPreview),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
