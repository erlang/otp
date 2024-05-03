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

-module(wxPrintout).
-moduledoc """
Functions for wxPrintout class

This class encapsulates the functionality of printing out an application
document.

A new class must be derived and members overridden to respond to calls such as
`OnPrintPage()` (not implemented in wx) and `HasPage()` (not implemented in wx)
and to render the print image onto an associated `m:wxDC`. Instances of this
class are passed to `wxPrinter:print/4` or to a `m:wxPrintPreview` object to
initiate printing or previewing.

Your derived `m:wxPrintout` is responsible for drawing both the preview image
and the printed page. If your windows' drawing routines accept an arbitrary DC
as an argument, you can re-use those routines within your `m:wxPrintout`
subclass to draw the printout image. You may also add additional drawing
elements within your `m:wxPrintout` subclass, like headers, footers, and/or page
numbers. However, the image on the printed page will often differ from the image
drawn on the screen, as will the print preview image - not just in the presence
of headers and footers, but typically in scale. A high-resolution printer
presents a much larger drawing surface (i.e., a higher-resolution DC); a
zoomed-out preview image presents a much smaller drawing surface
(lower-resolution DC). By using the routines FitThisSizeToXXX() and/or
MapScreenSizeToXXX() within your `m:wxPrintout` subclass to set the user scale
and origin of the associated DC, you can easily use a single drawing routine to
draw on your application's windows, to create the print preview image, and to
create the printed paper image, and achieve a common appearance to the preview
image and the printed page.

See:
[Overview printing](https://docs.wxwidgets.org/3.1/overview_printing.html#overview_printing),
`wxPrinterDC` (not implemented in wx), `m:wxPrintDialog`, `m:wxPageSetupDialog`,
`m:wxPrinter`, `m:wxPrintPreview`

wxWidgets docs:
[wxPrintout](https://docs.wxwidgets.org/3.1/classwx_printout.html)
""".
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
-doc false.
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).


%% @doc @equiv new(Title, OnPrintPage, [])
-doc false.
-spec new(Title::string(), OnPrintPage::function()) -> wxPrintout:wxPrintout().
new(Title, OnPrintPage) ->
    new(Title, OnPrintPage, []).

-doc """
Constructor.

Creates a `m:wxPrintout` object with a callback fun and optionally other
callback funs. The `This` argument is the `m:wxPrintout` object reference to
this object

Notice: The callbacks may not call other processes.
""".
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
-doc """
Returns the device context associated with the printout (given to the printout
at start of printing or previewing).

The application can use `getDC/1` to obtain a device context to draw on.

This will be a `wxPrinterDC` (not implemented in wx) if printing under Windows
or Mac, a `m:wxPostScriptDC` if printing on other platforms, and a
`m:wxMemoryDC` if previewing.
""".
-spec getDC(This) -> wxDC:wxDC() when
	This::wxPrintout().
getDC(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetDC),
  wxe_util:rec(?wxPrintout_GetDC).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpagesizemm">external documentation</a>.
-doc "Returns the size of the printer page in millimetres.".
-spec getPageSizeMM(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPageSizeMM(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPageSizeMM),
  wxe_util:rec(?wxPrintout_GetPageSizeMM).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpagesizepixels">external documentation</a>.
-doc """
Returns the size of the printer page in pixels, called the page rectangle.

The page rectangle has a top left corner at (0,0) and a bottom right corner at
(w,h). These values may not be the same as the values returned from
`wxDC:getSize/1`; if the printout is being used for previewing, a memory device
context is used, which uses a bitmap size reflecting the current preview zoom.
The application must take this discrepancy into account if previewing is to be
supported.
""".
-spec getPageSizePixels(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPageSizePixels(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPageSizePixels),
  wxe_util:rec(?wxPrintout_GetPageSizePixels).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetpaperrectpixels">external documentation</a>.
-doc """
Returns the rectangle that corresponds to the entire paper in pixels, called the
paper rectangle.

This distinction between paper rectangle and page rectangle reflects the fact
that most printers cannot print all the way to the edge of the paper. The page
rectangle is a rectangle whose top left corner is at (0,0) and whose width and
height are given by wxDC::GetPageSizePixels().

On MSW and Mac, the page rectangle gives the printable area of the paper, while
the paper rectangle represents the entire paper, including non-printable
borders. Thus, the rectangle returned by wxDC::GetPaperRectPixels() will have a
top left corner whose coordinates are small negative numbers and the bottom
right corner will have values somewhat larger than the width and height given by
wxDC::GetPageSizePixels().

On other platforms and for PostScript printing, the paper is treated as if its
entire area were printable, so this function will return the same rectangle as
the page rectangle.
""".
-spec getPaperRectPixels(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getPaperRectPixels(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPaperRectPixels),
  wxe_util:rec(?wxPrintout_GetPaperRectPixels).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetppiprinter">external documentation</a>.
-doc """
Returns the number of pixels per logical inch of the printer device context.

Dividing the printer PPI by the screen PPI can give a suitable scaling factor
for drawing text onto the printer.

Remember to multiply this by a scaling factor to take the preview DC size into
account. Or you can just use the FitThisSizeToXXX() and MapScreenSizeToXXX
routines below, which do most of the scaling calculations for you.
""".
-spec getPPIPrinter(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPPIPrinter(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPPIPrinter),
  wxe_util:rec(?wxPrintout_GetPPIPrinter).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetppiscreen">external documentation</a>.
-doc """
Returns the number of pixels per logical inch of the screen device context.

Dividing the printer PPI by the screen PPI can give a suitable scaling factor
for drawing text onto the printer.

If you are doing your own scaling, remember to multiply this by a scaling factor
to take the preview DC size into account.
""".
-spec getPPIScreen(This) -> {W::integer(), H::integer()} when
	This::wxPrintout().
getPPIScreen(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetPPIScreen),
  wxe_util:rec(?wxPrintout_GetPPIScreen).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgettitle">external documentation</a>.
-doc "Returns the title of the printout.".
-spec getTitle(This) -> unicode:charlist() when
	This::wxPrintout().
getTitle(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetTitle),
  wxe_util:rec(?wxPrintout_GetTitle).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutispreview">external documentation</a>.
-doc """
Returns true if the printout is currently being used for previewing.

See: `GetPreview()` (not implemented in wx)
""".
-spec isPreview(This) -> boolean() when
	This::wxPrintout().
isPreview(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_IsPreview),
  wxe_util:rec(?wxPrintout_IsPreview).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopaper">external documentation</a>.
-doc """
Set the user scale and device origin of the `m:wxDC` associated with this
`m:wxPrintout` so that the given image size fits entirely within the paper and
the origin is at the top left corner of the paper.

Use this if you're managing your own page margins.

Note: With most printers, the region around the edges of the paper are not
printable so that the edges of the image could be cut off.
""".
-spec fitThisSizeToPaper(This, ImageSize) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}.
fitThisSizeToPaper(#wx_ref{type=ThisT}=This,{ImageSizeW,ImageSizeH} = ImageSize)
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,ImageSize,?get_env(),?wxPrintout_FitThisSizeToPaper).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopage">external documentation</a>.
-doc """
Set the user scale and device origin of the `m:wxDC` associated with this
`m:wxPrintout` so that the given image size fits entirely within the page
rectangle and the origin is at the top left corner of the page rectangle.

On MSW and Mac, the page rectangle is the printable area of the page. On other
platforms and PostScript printing, the page rectangle is the entire paper.

Use this if you want your printed image as large as possible, but with the
caveat that on some platforms, portions of the image might be cut off at the
edges.
""".
-spec fitThisSizeToPage(This, ImageSize) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}.
fitThisSizeToPage(#wx_ref{type=ThisT}=This,{ImageSizeW,ImageSizeH} = ImageSize)
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,ImageSize,?get_env(),?wxPrintout_FitThisSizeToPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutfitthissizetopagemargins">external documentation</a>.
-doc """
Set the user scale and device origin of the `m:wxDC` associated with this
`m:wxPrintout` so that the given image size fits entirely within the page
margins set in the given `m:wxPageSetupDialogData` object.

This function provides the greatest consistency across all platforms because it
does not depend on having access to the printable area of the paper.

Remark: On Mac, the native `m:wxPageSetupDialog` does not let you set the page
margins; you'll have to provide your own mechanism, or you can use the Mac-only
class wxMacPageMarginsDialog.
""".
-spec fitThisSizeToPageMargins(This, ImageSize, PageSetupData) -> 'ok' when
	This::wxPrintout(), ImageSize::{W::integer(), H::integer()}, PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
fitThisSizeToPageMargins(#wx_ref{type=ThisT}=This,{ImageSizeW,ImageSizeH} = ImageSize,#wx_ref{type=PageSetupDataT}=PageSetupData)
 when is_integer(ImageSizeW),is_integer(ImageSizeH) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,ImageSize,PageSetupData,?get_env(),?wxPrintout_FitThisSizeToPageMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopaper">external documentation</a>.
-doc """
Set the user scale and device origin of the `m:wxDC` associated with this
`m:wxPrintout` so that the printed page matches the screen size as closely as
possible and the logical origin is in the top left corner of the paper
rectangle.

That is, a 100-pixel object on screen should appear at the same size on the
printed page. (It will, of course, be larger or smaller in the preview image,
depending on the zoom factor.)

Use this if you want WYSIWYG behaviour, e.g., in a text editor.
""".
-spec mapScreenSizeToPaper(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToPaper(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_MapScreenSizeToPaper).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopage">external documentation</a>.
-doc """
This sets the user scale of the `m:wxDC` associated with this `m:wxPrintout` to
the same scale as `mapScreenSizeToPaper/1` but sets the logical origin to the
top left corner of the page rectangle.
""".
-spec mapScreenSizeToPage(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToPage(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_MapScreenSizeToPage).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetopagemargins">external documentation</a>.
-doc """
This sets the user scale of the `m:wxDC` associated with this `m:wxPrintout` to
the same scale as `mapScreenSizeToPageMargins/2` but sets the logical origin to
the top left corner of the page margins specified by the given
`m:wxPageSetupDialogData` object.
""".
-spec mapScreenSizeToPageMargins(This, PageSetupData) -> 'ok' when
	This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
mapScreenSizeToPageMargins(#wx_ref{type=ThisT}=This,#wx_ref{type=PageSetupDataT}=PageSetupData) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,PageSetupData,?get_env(),?wxPrintout_MapScreenSizeToPageMargins).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutmapscreensizetodevice">external documentation</a>.
-doc """
Set the user scale and device origin of the `m:wxDC` associated with this
`m:wxPrintout` so that one screen pixel maps to one device pixel on the DC.

That is, the user scale is set to (1,1) and the device origin is set to (0,0).

Use this if you want to do your own scaling prior to calling `m:wxDC` drawing
calls, for example, if your underlying model is floating-point and you want to
achieve maximum drawing precision on high-resolution printers.

You can use the GetLogicalXXXRect() routines below to obtain the paper
rectangle, page rectangle, or page margins rectangle to perform your own
scaling.

Note: While the underlying drawing model of macOS is floating-point, wxWidgets's
drawing model scales from integer coordinates.
""".
-spec mapScreenSizeToDevice(This) -> 'ok' when
	This::wxPrintout().
mapScreenSizeToDevice(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_MapScreenSizeToDevice).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpaperrect">external documentation</a>.
-doc """
Return the rectangle corresponding to the paper in the associated `m:wxDC` 's
logical coordinates for the current user scale and device origin.
""".
-spec getLogicalPaperRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getLogicalPaperRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetLogicalPaperRect),
  wxe_util:rec(?wxPrintout_GetLogicalPaperRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpagerect">external documentation</a>.
-doc """
Return the rectangle corresponding to the page in the associated `m:wxDC` 's
logical coordinates for the current user scale and device origin.

On MSW and Mac, this will be the printable area of the paper. On other platforms
and PostScript printing, this will be the full paper rectangle.
""".
-spec getLogicalPageRect(This) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout().
getLogicalPageRect(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,?get_env(),?wxPrintout_GetLogicalPageRect),
  wxe_util:rec(?wxPrintout_GetLogicalPageRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutgetlogicalpagemarginsrect">external documentation</a>.
-doc """
Return the rectangle corresponding to the page margins specified by the given
`m:wxPageSetupDialogData` object in the associated `m:wxDC`'s logical
coordinates for the current user scale and device origin.

The page margins are specified with respect to the edges of the paper on all
platforms.
""".
-spec getLogicalPageMarginsRect(This, PageSetupData) -> {X::integer(), Y::integer(), W::integer(), H::integer()} when
	This::wxPrintout(), PageSetupData::wxPageSetupDialogData:wxPageSetupDialogData().
getLogicalPageMarginsRect(#wx_ref{type=ThisT}=This,#wx_ref{type=PageSetupDataT}=PageSetupData) ->
  ?CLASS(ThisT,wxPrintout),
  ?CLASS(PageSetupDataT,wxPageSetupDialogData),
  wxe_util:queue_cmd(This,PageSetupData,?get_env(),?wxPrintout_GetLogicalPageMarginsRect),
  wxe_util:rec(?wxPrintout_GetLogicalPageMarginsRect).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutsetlogicalorigin">external documentation</a>.
-doc """
Set the device origin of the associated `m:wxDC` so that the current logical
point becomes the new logical origin.
""".
-spec setLogicalOrigin(This, X, Y) -> 'ok' when
	This::wxPrintout(), X::integer(), Y::integer().
setLogicalOrigin(#wx_ref{type=ThisT}=This,X,Y)
 when is_integer(X),is_integer(Y) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,X,Y,?get_env(),?wxPrintout_SetLogicalOrigin).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxprintout.html#wxprintoutoffsetlogicalorigin">external documentation</a>.
-doc "Shift the device origin by an amount specified in logical coordinates.".
-spec offsetLogicalOrigin(This, Xoff, Yoff) -> 'ok' when
	This::wxPrintout(), Xoff::integer(), Yoff::integer().
offsetLogicalOrigin(#wx_ref{type=ThisT}=This,Xoff,Yoff)
 when is_integer(Xoff),is_integer(Yoff) ->
  ?CLASS(ThisT,wxPrintout),
  wxe_util:queue_cmd(This,Xoff,Yoff,?get_env(),?wxPrintout_OffsetLogicalOrigin).

%% @doc Destroys this object, do not use object again
-doc "Destructor.".
-spec destroy(This::wxPrintout()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxPrintout),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
