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

%% @doc See external documentation: <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html">wxPrintPreview</a>.
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

-export_type([wxPrintPreview/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

-type wxPrintPreview() :: wx:wx_object().
%% @equiv new(Printout, [])
-spec new(Printout) -> wxPrintPreview() when
	Printout::wxPrintout:wxPrintout().

new(Printout)
 when is_record(Printout, wx_ref) ->
  new(Printout, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewwxprintpreview">external documentation</a>.
-spec new(Printout, [Option]) -> wxPrintPreview() when
	Printout::wxPrintout:wxPrintout(),
	Option :: {'printoutForPrinting', wxPrintout:wxPrintout()}
		 | {'data', wxPrintDialogData:wxPrintDialogData()}.
new(#wx_ref{type=PrintoutT,ref=PrintoutRef}, Options)
 when is_list(Options) ->
  ?CLASS(PrintoutT,wxPrintout),
  MOpts = fun({printoutForPrinting, #wx_ref{type=PrintoutForPrintingT,ref=PrintoutForPrintingRef}}, Acc) ->   ?CLASS(PrintoutForPrintingT,wxPrintout),[<<1:32/?UI,PrintoutForPrintingRef:32/?UI>>|Acc];
          ({data, #wx_ref{type=DataT,ref=DataRef}}, Acc) ->   ?CLASS(DataT,wxPrintDialogData),[<<2:32/?UI,DataRef:32/?UI>>|Acc];
          (BadOpt, _) -> erlang:error({badoption, BadOpt}) end,
  BinOpt = list_to_binary(lists:foldl(MOpts, [<<0:32>>], Options)),
  wxe_util:construct(?wxPrintPreview_new_2,
  <<PrintoutRef:32/?UI, 0:32,BinOpt/binary>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewwxprintpreview">external documentation</a>.
-spec new(Printout, PrintoutForPrinting, Data) -> wxPrintPreview() when
	Printout::wxPrintout:wxPrintout(), PrintoutForPrinting::wxPrintout:wxPrintout(), Data::wxPrintData:wxPrintData().
new(#wx_ref{type=PrintoutT,ref=PrintoutRef},#wx_ref{type=PrintoutForPrintingT,ref=PrintoutForPrintingRef},#wx_ref{type=DataT,ref=DataRef}) ->
  ?CLASS(PrintoutT,wxPrintout),
  ?CLASS(PrintoutForPrintingT,wxPrintout),
  ?CLASS(DataT,wxPrintData),
  wxe_util:construct(?wxPrintPreview_new_3,
  <<PrintoutRef:32/?UI,PrintoutForPrintingRef:32/?UI,DataRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetcanvas">external documentation</a>.
-spec getCanvas(This) -> wxPreviewCanvas:wxPreviewCanvas() when
	This::wxPrintPreview().
getCanvas(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetCanvas,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetcurrentpage">external documentation</a>.
-spec getCurrentPage(This) -> integer() when
	This::wxPrintPreview().
getCurrentPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetCurrentPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetframe">external documentation</a>.
-spec getFrame(This) -> wxFrame:wxFrame() when
	This::wxPrintPreview().
getFrame(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetFrame,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetmaxpage">external documentation</a>.
-spec getMaxPage(This) -> integer() when
	This::wxPrintPreview().
getMaxPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetMaxPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetminpage">external documentation</a>.
-spec getMinPage(This) -> integer() when
	This::wxPrintPreview().
getMinPage(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetMinPage,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetprintout">external documentation</a>.
-spec getPrintout(This) -> wxPrintout:wxPrintout() when
	This::wxPrintPreview().
getPrintout(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetPrintout,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetprintoutforprinting">external documentation</a>.
-spec getPrintoutForPrinting(This) -> wxPrintout:wxPrintout() when
	This::wxPrintPreview().
getPrintoutForPrinting(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_GetPrintoutForPrinting,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPrintPreview().
isOk(#wx_ref{type=ThisT,ref=ThisRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_IsOk,
  <<ThisRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewpaintpage">external documentation</a>.
-spec paintPage(This, Canvas, Dc) -> boolean() when
	This::wxPrintPreview(), Canvas::wxPreviewCanvas:wxPreviewCanvas(), Dc::wxDC:wxDC().
paintPage(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CanvasT,ref=CanvasRef},#wx_ref{type=DcT,ref=DcRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(CanvasT,wxPreviewCanvas),
  ?CLASS(DcT,wxDC),
  wxe_util:call(?wxPrintPreview_PaintPage,
  <<ThisRef:32/?UI,CanvasRef:32/?UI,DcRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewprint">external documentation</a>.
-spec print(This, Interactive) -> boolean() when
	This::wxPrintPreview(), Interactive::boolean().
print(#wx_ref{type=ThisT,ref=ThisRef},Interactive)
 when is_boolean(Interactive) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_Print,
  <<ThisRef:32/?UI,(wxe_util:from_bool(Interactive)):32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewrenderpage">external documentation</a>.
-spec renderPage(This, PageNum) -> boolean() when
	This::wxPrintPreview(), PageNum::integer().
renderPage(#wx_ref{type=ThisT,ref=ThisRef},PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_RenderPage,
  <<ThisRef:32/?UI,PageNum:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetcanvas">external documentation</a>.
-spec setCanvas(This, Canvas) -> 'ok' when
	This::wxPrintPreview(), Canvas::wxPreviewCanvas:wxPreviewCanvas().
setCanvas(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=CanvasT,ref=CanvasRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(CanvasT,wxPreviewCanvas),
  wxe_util:cast(?wxPrintPreview_SetCanvas,
  <<ThisRef:32/?UI,CanvasRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetcurrentpage">external documentation</a>.
-spec setCurrentPage(This, PageNum) -> boolean() when
	This::wxPrintPreview(), PageNum::integer().
setCurrentPage(#wx_ref{type=ThisT,ref=ThisRef},PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:call(?wxPrintPreview_SetCurrentPage,
  <<ThisRef:32/?UI,PageNum:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetframe">external documentation</a>.
-spec setFrame(This, Frame) -> 'ok' when
	This::wxPrintPreview(), Frame::wxFrame:wxFrame().
setFrame(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=FrameT,ref=FrameRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(FrameT,wxFrame),
  wxe_util:cast(?wxPrintPreview_SetFrame,
  <<ThisRef:32/?UI,FrameRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetprintout">external documentation</a>.
-spec setPrintout(This, Printout) -> 'ok' when
	This::wxPrintPreview(), Printout::wxPrintout:wxPrintout().
setPrintout(#wx_ref{type=ThisT,ref=ThisRef},#wx_ref{type=PrintoutT,ref=PrintoutRef}) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:cast(?wxPrintPreview_SetPrintout,
  <<ThisRef:32/?UI,PrintoutRef:32/?UI>>).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetzoom">external documentation</a>.
-spec setZoom(This, Percent) -> 'ok' when
	This::wxPrintPreview(), Percent::integer().
setZoom(#wx_ref{type=ThisT,ref=ThisRef},Percent)
 when is_integer(Percent) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:cast(?wxPrintPreview_SetZoom,
  <<ThisRef:32/?UI,Percent:32/?UI>>).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintPreview()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintPreview),
  wxe_util:destroy(?DESTROY_OBJECT,Obj),
  ok.
