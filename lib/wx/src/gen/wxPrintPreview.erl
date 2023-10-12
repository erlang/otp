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

-module(wxPrintPreview).
-include("wxe.hrl").
-export([destroy/1,getCanvas/1,getCurrentPage/1,getFrame/1,getMaxPage/1,getMinPage/1,
  getPrintout/1,getPrintoutForPrinting/1,isOk/1,new/1,new/2,new/3,paintPage/3,
  print/2,renderPage/2,setCanvas/2,setCurrentPage/2,setFrame/2,setPrintout/2,
  setZoom/2]).

%% inherited exports
-export([parent_class/1]).

-type wxPrintPreview() :: wx:wx_object().
-export_type([wxPrintPreview/0]).
%% @hidden
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

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
new(#wx_ref{type=PrintoutT}=Printout, Options)
 when is_list(Options) ->
  ?CLASS(PrintoutT,wxPrintout),
  MOpts = fun({printoutForPrinting, #wx_ref{type=PrintoutForPrintingT}} = Arg) ->   ?CLASS(PrintoutForPrintingT,wxPrintout),Arg;
          ({data, #wx_ref{type=DataT}} = Arg) ->   ?CLASS(DataT,wxPrintDialogData),Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Printout, Opts,?get_env(),?wxPrintPreview_new_2),
  wxe_util:rec(?wxPrintPreview_new_2).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewwxprintpreview">external documentation</a>.
-spec new(Printout, PrintoutForPrinting, Data) -> wxPrintPreview() when
	Printout::wxPrintout:wxPrintout(), PrintoutForPrinting::wxPrintout:wxPrintout(), Data::wxPrintData:wxPrintData().
new(#wx_ref{type=PrintoutT}=Printout,#wx_ref{type=PrintoutForPrintingT}=PrintoutForPrinting,#wx_ref{type=DataT}=Data) ->
  ?CLASS(PrintoutT,wxPrintout),
  ?CLASS(PrintoutForPrintingT,wxPrintout),
  ?CLASS(DataT,wxPrintData),
  wxe_util:queue_cmd(Printout,PrintoutForPrinting,Data,?get_env(),?wxPrintPreview_new_3),
  wxe_util:rec(?wxPrintPreview_new_3).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetcanvas">external documentation</a>.
-spec getCanvas(This) -> wxPreviewCanvas:wxPreviewCanvas() when
	This::wxPrintPreview().
getCanvas(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetCanvas),
  wxe_util:rec(?wxPrintPreview_GetCanvas).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetcurrentpage">external documentation</a>.
-spec getCurrentPage(This) -> integer() when
	This::wxPrintPreview().
getCurrentPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetCurrentPage),
  wxe_util:rec(?wxPrintPreview_GetCurrentPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetframe">external documentation</a>.
-spec getFrame(This) -> wxFrame:wxFrame() when
	This::wxPrintPreview().
getFrame(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetFrame),
  wxe_util:rec(?wxPrintPreview_GetFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetmaxpage">external documentation</a>.
-spec getMaxPage(This) -> integer() when
	This::wxPrintPreview().
getMaxPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetMaxPage),
  wxe_util:rec(?wxPrintPreview_GetMaxPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetminpage">external documentation</a>.
-spec getMinPage(This) -> integer() when
	This::wxPrintPreview().
getMinPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetMinPage),
  wxe_util:rec(?wxPrintPreview_GetMinPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetprintout">external documentation</a>.
-spec getPrintout(This) -> wxPrintout:wxPrintout() when
	This::wxPrintPreview().
getPrintout(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetPrintout),
  wxe_util:rec(?wxPrintPreview_GetPrintout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetprintoutforprinting">external documentation</a>.
-spec getPrintoutForPrinting(This) -> wxPrintout:wxPrintout() when
	This::wxPrintPreview().
getPrintoutForPrinting(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetPrintoutForPrinting),
  wxe_util:rec(?wxPrintPreview_GetPrintoutForPrinting).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewisok">external documentation</a>.
-spec isOk(This) -> boolean() when
	This::wxPrintPreview().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_IsOk),
  wxe_util:rec(?wxPrintPreview_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewpaintpage">external documentation</a>.
-spec paintPage(This, Canvas, Dc) -> boolean() when
	This::wxPrintPreview(), Canvas::wxPreviewCanvas:wxPreviewCanvas(), Dc::wxDC:wxDC().
paintPage(#wx_ref{type=ThisT}=This,#wx_ref{type=CanvasT}=Canvas,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(CanvasT,wxPreviewCanvas),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Canvas,Dc,?get_env(),?wxPrintPreview_PaintPage),
  wxe_util:rec(?wxPrintPreview_PaintPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewprint">external documentation</a>.
-spec print(This, Prompt) -> boolean() when
	This::wxPrintPreview(), Prompt::boolean().
print(#wx_ref{type=ThisT}=This,Prompt)
 when is_boolean(Prompt) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,Prompt,?get_env(),?wxPrintPreview_Print),
  wxe_util:rec(?wxPrintPreview_Print).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewrenderpage">external documentation</a>.
-spec renderPage(This, PageNum) -> boolean() when
	This::wxPrintPreview(), PageNum::integer().
renderPage(#wx_ref{type=ThisT}=This,PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,PageNum,?get_env(),?wxPrintPreview_RenderPage),
  wxe_util:rec(?wxPrintPreview_RenderPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetcanvas">external documentation</a>.
-spec setCanvas(This, Window) -> 'ok' when
	This::wxPrintPreview(), Window::wxPreviewCanvas:wxPreviewCanvas().
setCanvas(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(WindowT,wxPreviewCanvas),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxPrintPreview_SetCanvas).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetcurrentpage">external documentation</a>.
-spec setCurrentPage(This, PageNum) -> boolean() when
	This::wxPrintPreview(), PageNum::integer().
setCurrentPage(#wx_ref{type=ThisT}=This,PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,PageNum,?get_env(),?wxPrintPreview_SetCurrentPage),
  wxe_util:rec(?wxPrintPreview_SetCurrentPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetframe">external documentation</a>.
-spec setFrame(This, Frame) -> 'ok' when
	This::wxPrintPreview(), Frame::wxFrame:wxFrame().
setFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(FrameT,wxFrame),
  wxe_util:queue_cmd(This,Frame,?get_env(),?wxPrintPreview_SetFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetprintout">external documentation</a>.
-spec setPrintout(This, Printout) -> 'ok' when
	This::wxPrintPreview(), Printout::wxPrintout:wxPrintout().
setPrintout(#wx_ref{type=ThisT}=This,#wx_ref{type=PrintoutT}=Printout) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:queue_cmd(This,Printout,?get_env(),?wxPrintPreview_SetPrintout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetzoom">external documentation</a>.
-spec setZoom(This, Percent) -> 'ok' when
	This::wxPrintPreview(), Percent::integer().
setZoom(#wx_ref{type=ThisT}=This,Percent)
 when is_integer(Percent) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,Percent,?get_env(),?wxPrintPreview_SetZoom).

%% @doc Destroys this object, do not use object again
-spec destroy(This::wxPrintPreview()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintPreview),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
