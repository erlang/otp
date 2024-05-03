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

-module(wxPrintPreview).
-moduledoc """
Functions for wxPrintPreview class

Objects of this class manage the print preview process. The object is passed a
`m:wxPrintout` object, and the `m:wxPrintPreview` object itself is passed to a
`m:wxPreviewFrame` object. Previewing is started by initializing and showing the
preview frame. Unlike `wxPrinter:print/4`, flow of control returns to the
application immediately after the frame is shown.

Note: The preview shown is only exact on Windows. On other platforms, the
`m:wxDC` used for preview is different from what is used for printing and the
results may be significantly different, depending on how is the output created.
In particular, printing code relying on `wxDC:getTextExtent/3` heavily (for
example, `m:wxHtmlEasyPrinting` and other wxHTML classes do) is affected. It is
recommended to use native preview functionality on platforms that offer it
(macOS, GTK+).

See:
[Overview printing](https://docs.wxwidgets.org/3.1/overview_printing.html#overview_printing),
`wxPrinterDC` (not implemented in wx), `m:wxPrintDialog`, `m:wxPrintout`,
`m:wxPrinter`, `m:wxPreviewCanvas`, `m:wxPreviewControlBar`, `m:wxPreviewFrame`

wxWidgets docs:
[wxPrintPreview](https://docs.wxwidgets.org/3.1/classwx_print_preview.html)
""".
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
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @equiv new(Printout, [])
-spec new(Printout) -> wxPrintPreview() when
	Printout::wxPrintout:wxPrintout().

new(Printout)
 when is_record(Printout, wx_ref) ->
  new(Printout, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewwxprintpreview">external documentation</a>.
-doc """
Constructor.

Pass a printout object, an optional printout object to be used for actual
printing, and the address of an optional block of printer data, which will be
copied to the print preview object's print data.

If `printoutForPrinting` is non-NULL, a `"Print..."` button will be placed on
the preview frame so that the user can print directly from the preview
interface.

Remark: Do not explicitly delete the printout objects once this constructor has
been called, since they will be deleted in the `m:wxPrintPreview` destructor.
The same does not apply to the `data` argument.

Use `isOk/1` to check whether the `m:wxPrintPreview` object was created
correctly.
""".
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
-doc "Gets the preview window used for displaying the print preview image.".
-spec getCanvas(This) -> wxPreviewCanvas:wxPreviewCanvas() when
	This::wxPrintPreview().
getCanvas(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetCanvas),
  wxe_util:rec(?wxPrintPreview_GetCanvas).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetcurrentpage">external documentation</a>.
-doc "Gets the page currently being previewed.".
-spec getCurrentPage(This) -> integer() when
	This::wxPrintPreview().
getCurrentPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetCurrentPage),
  wxe_util:rec(?wxPrintPreview_GetCurrentPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetframe">external documentation</a>.
-doc "Gets the frame used for displaying the print preview canvas and control bar.".
-spec getFrame(This) -> wxFrame:wxFrame() when
	This::wxPrintPreview().
getFrame(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetFrame),
  wxe_util:rec(?wxPrintPreview_GetFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetmaxpage">external documentation</a>.
-doc "Returns the maximum page number.".
-spec getMaxPage(This) -> integer() when
	This::wxPrintPreview().
getMaxPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetMaxPage),
  wxe_util:rec(?wxPrintPreview_GetMaxPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetminpage">external documentation</a>.
-doc "Returns the minimum page number.".
-spec getMinPage(This) -> integer() when
	This::wxPrintPreview().
getMinPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetMinPage),
  wxe_util:rec(?wxPrintPreview_GetMinPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetprintout">external documentation</a>.
-doc "Gets the preview printout object associated with the `m:wxPrintPreview` object.".
-spec getPrintout(This) -> wxPrintout:wxPrintout() when
	This::wxPrintPreview().
getPrintout(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetPrintout),
  wxe_util:rec(?wxPrintPreview_GetPrintout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewgetprintoutforprinting">external documentation</a>.
-doc """
Gets the printout object to be used for printing from within the preview
interface, or NULL if none exists.
""".
-spec getPrintoutForPrinting(This) -> wxPrintout:wxPrintout() when
	This::wxPrintPreview().
getPrintoutForPrinting(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_GetPrintoutForPrinting),
  wxe_util:rec(?wxPrintPreview_GetPrintoutForPrinting).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewisok">external documentation</a>.
-doc """
Returns true if the `m:wxPrintPreview` is valid, false otherwise.

It could return false if there was a problem initializing the printer device
context (current printer not set, for example).
""".
-spec isOk(This) -> boolean() when
	This::wxPrintPreview().
isOk(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintPreview_IsOk),
  wxe_util:rec(?wxPrintPreview_IsOk).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewpaintpage">external documentation</a>.
-doc """
This refreshes the preview window with the preview image.

It must be called from the preview window's OnPaint member.

The implementation simply blits the preview bitmap onto the canvas, creating a
new preview bitmap if none exists.
""".
-spec paintPage(This, Canvas, Dc) -> boolean() when
	This::wxPrintPreview(), Canvas::wxPreviewCanvas:wxPreviewCanvas(), Dc::wxDC:wxDC().
paintPage(#wx_ref{type=ThisT}=This,#wx_ref{type=CanvasT}=Canvas,#wx_ref{type=DcT}=Dc) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(CanvasT,wxPreviewCanvas),
  ?CLASS(DcT,wxDC),
  wxe_util:queue_cmd(This,Canvas,Dc,?get_env(),?wxPrintPreview_PaintPage),
  wxe_util:rec(?wxPrintPreview_PaintPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewprint">external documentation</a>.
-doc """
Invokes the print process using the second `m:wxPrintout` object supplied in the
`m:wxPrintPreview` constructor.

Will normally be called by the `Print`... panel item on the preview frame's
control bar.

Returns false in case of error - call `wxPrinter:getLastError/0` to get detailed
information about the kind of the error.
""".
-spec print(This, Prompt) -> boolean() when
	This::wxPrintPreview(), Prompt::boolean().
print(#wx_ref{type=ThisT}=This,Prompt)
 when is_boolean(Prompt) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,Prompt,?get_env(),?wxPrintPreview_Print),
  wxe_util:rec(?wxPrintPreview_Print).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewrenderpage">external documentation</a>.
-doc """
Renders a page into a `m:wxMemoryDC`.

Used internally by `m:wxPrintPreview`.
""".
-spec renderPage(This, PageNum) -> boolean() when
	This::wxPrintPreview(), PageNum::integer().
renderPage(#wx_ref{type=ThisT}=This,PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,PageNum,?get_env(),?wxPrintPreview_RenderPage),
  wxe_util:rec(?wxPrintPreview_RenderPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetcanvas">external documentation</a>.
-doc "Sets the window to be used for displaying the print preview image.".
-spec setCanvas(This, Window) -> 'ok' when
	This::wxPrintPreview(), Window::wxPreviewCanvas:wxPreviewCanvas().
setCanvas(#wx_ref{type=ThisT}=This,#wx_ref{type=WindowT}=Window) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(WindowT,wxPreviewCanvas),
  wxe_util:queue_cmd(This,Window,?get_env(),?wxPrintPreview_SetCanvas).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetcurrentpage">external documentation</a>.
-doc "Sets the current page to be previewed.".
-spec setCurrentPage(This, PageNum) -> boolean() when
	This::wxPrintPreview(), PageNum::integer().
setCurrentPage(#wx_ref{type=ThisT}=This,PageNum)
 when is_integer(PageNum) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,PageNum,?get_env(),?wxPrintPreview_SetCurrentPage),
  wxe_util:rec(?wxPrintPreview_SetCurrentPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetframe">external documentation</a>.
-doc """
Sets the frame to be used for displaying the print preview canvas and control
bar.
""".
-spec setFrame(This, Frame) -> 'ok' when
	This::wxPrintPreview(), Frame::wxFrame:wxFrame().
setFrame(#wx_ref{type=ThisT}=This,#wx_ref{type=FrameT}=Frame) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(FrameT,wxFrame),
  wxe_util:queue_cmd(This,Frame,?get_env(),?wxPrintPreview_SetFrame).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetprintout">external documentation</a>.
-doc "Associates a printout object with the `m:wxPrintPreview` object.".
-spec setPrintout(This, Printout) -> 'ok' when
	This::wxPrintPreview(), Printout::wxPrintout:wxPrintout().
setPrintout(#wx_ref{type=ThisT}=This,#wx_ref{type=PrintoutT}=Printout) ->
  ?CLASS(ThisT,wxPrintPreview),
  ?CLASS(PrintoutT,wxPrintout),
  wxe_util:queue_cmd(This,Printout,?get_env(),?wxPrintPreview_SetPrintout).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintpreview.html#wxprintpreviewsetzoom">external documentation</a>.
-doc "Sets the percentage preview zoom, and refreshes the preview canvas accordingly.".
-spec setZoom(This, Percent) -> 'ok' when
	This::wxPrintPreview(), Percent::integer().
setZoom(#wx_ref{type=ThisT}=This,Percent)
 when is_integer(Percent) ->
  ?CLASS(ThisT,wxPrintPreview),
  wxe_util:queue_cmd(This,Percent,?get_env(),?wxPrintPreview_SetZoom).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

Deletes both print preview objects, so do not destroy these objects in your
application.
""".
-spec destroy(This::wxPrintPreview()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintPreview),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
