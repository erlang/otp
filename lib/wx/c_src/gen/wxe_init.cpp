/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2011. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd% 
*/
 /* This file is also generated */
#include <wx/wx.h>
#include "../wxe_impl.h"
#include "wxe_macros.h"
#include "../wxe_return.h"
void WxeApp::init_nonconsts(wxeMemEnv *memenv, ErlDrvTermData caller) {
  wxeReturn rt = wxeReturn(WXE_DRV_PORT, caller);
 rt.addAtom((char*)"wx_consts");
 rt.addAtom("wxALWAYS_NATIVE_DOUBLE_BUFFER"); rt.addInt(wxALWAYS_NATIVE_DOUBLE_BUFFER);
 rt.addTupleCount(2);
 rt.addAtom("wxBYTE_ORDER"); rt.addInt(wxBYTE_ORDER);
 rt.addTupleCount(2);
 rt.addAtom("wxDEFAULT_CONTROL_BORDER"); rt.addInt(wxDEFAULT_CONTROL_BORDER);
 rt.addTupleCount(2);
 rt.addAtom("wxHAS_INT64"); rt.addInt(wxHAS_INT64);
 rt.addTupleCount(2);
 rt.addAtom("wxRETAINED"); rt.addInt(wxRETAINED);
 rt.addTupleCount(2);
 rt.addAtom("wxGAUGE_EMULATE_INDETERMINATE_MODE"); rt.addInt(wxGAUGE_EMULATE_INDETERMINATE_MODE);
 rt.addTupleCount(2);
 rt.addAtom("wxTR_DEFAULT_STYLE"); rt.addInt(wxTR_DEFAULT_STYLE);
 rt.addTupleCount(2);
 rt.addAtom("wxBETA_NUMBER"); rt.addInt(wxBETA_NUMBER);
 rt.addTupleCount(2);
 rt.addAtom("wxMAJOR_VERSION"); rt.addInt(wxMAJOR_VERSION);
 rt.addTupleCount(2);
 rt.addAtom("wxMINOR_VERSION"); rt.addInt(wxMINOR_VERSION);
 rt.addTupleCount(2);
 rt.addAtom("wxRELEASE_NUMBER"); rt.addInt(wxRELEASE_NUMBER);
 rt.addTupleCount(2);
 rt.addAtom("wxSUBRELEASE_NUMBER"); rt.addInt(wxSUBRELEASE_NUMBER);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTENCODING_UTF16"); rt.addInt(wxFONTENCODING_UTF16);
 rt.addTupleCount(2);
 rt.addAtom("wxFONTENCODING_UTF32"); rt.addInt(wxFONTENCODING_UTF32);
 rt.addTupleCount(2);
 rt.addAtom("wxMOD_CMD"); rt.addInt(wxMOD_CMD);
 rt.addTupleCount(2);
   rt.addAtom("wxBLACK"); rt.add(*(wxBLACK));
   rt.addTupleCount(2);
   rt.addAtom("wxBLACK_BRUSH"); rt.addRef(getRef((void *)wxBLACK_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxBLACK_DASHED_PEN"); rt.addRef(getRef((void *)wxBLACK_DASHED_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxBLACK_PEN"); rt.addRef(getRef((void *)wxBLACK_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxBLUE"); rt.add(*(wxBLUE));
   rt.addTupleCount(2);
   rt.addAtom("wxBLUE_BRUSH"); rt.addRef(getRef((void *)wxBLUE_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxCROSS_CURSOR"); rt.addRef(getRef((void *)wxCROSS_CURSOR,memenv),"wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxCYAN"); rt.add(*(wxCYAN));
   rt.addTupleCount(2);
   rt.addAtom("wxCYAN_BRUSH"); rt.addRef(getRef((void *)wxCYAN_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxCYAN_PEN"); rt.addRef(getRef((void *)wxCYAN_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxGREEN"); rt.add(*(wxGREEN));
   rt.addTupleCount(2);
   rt.addAtom("wxGREEN_BRUSH"); rt.addRef(getRef((void *)wxGREEN_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxGREEN_PEN"); rt.addRef(getRef((void *)wxGREEN_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxGREY_BRUSH"); rt.addRef(getRef((void *)wxGREY_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxGREY_PEN"); rt.addRef(getRef((void *)wxGREY_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxHOURGLASS_CURSOR"); rt.addRef(getRef((void *)wxHOURGLASS_CURSOR,memenv),"wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxITALIC_FONT"); rt.addRef(getRef((void *)wxITALIC_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxLIGHT_GREY"); rt.add(*(wxLIGHT_GREY));
   rt.addTupleCount(2);
   rt.addAtom("wxLIGHT_GREY_BRUSH"); rt.addRef(getRef((void *)wxLIGHT_GREY_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxLIGHT_GREY_PEN"); rt.addRef(getRef((void *)wxLIGHT_GREY_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxMEDIUM_GREY_BRUSH"); rt.addRef(getRef((void *)wxMEDIUM_GREY_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxMEDIUM_GREY_PEN"); rt.addRef(getRef((void *)wxMEDIUM_GREY_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxNORMAL_FONT"); rt.addRef(getRef((void *)wxNORMAL_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxNullBitmap"); rt.addRef(getRef((void *)&wxNullBitmap,memenv), "wxBitmap");
   rt.addTupleCount(2);
   rt.addAtom("wxNullBrush"); rt.addRef(getRef((void *)&wxNullBrush,memenv), "wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxNullCursor"); rt.addRef(getRef((void *)&wxNullCursor,memenv), "wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxNullFont"); rt.addRef(getRef((void *)&wxNullFont,memenv), "wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxNullIcon"); rt.addRef(getRef((void *)&wxNullIcon,memenv), "wxIcon");
   rt.addTupleCount(2);
   rt.addAtom("wxNullPalette"); rt.addRef(getRef((void *)&wxNullPalette,memenv), "wxPalette");
   rt.addTupleCount(2);
   rt.addAtom("wxNullPen"); rt.addRef(getRef((void *)&wxNullPen,memenv), "wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxRED"); rt.add(*(wxRED));
   rt.addTupleCount(2);
   rt.addAtom("wxRED_BRUSH"); rt.addRef(getRef((void *)wxRED_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxRED_PEN"); rt.addRef(getRef((void *)wxRED_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxSMALL_FONT"); rt.addRef(getRef((void *)wxSMALL_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxSTANDARD_CURSOR"); rt.addRef(getRef((void *)wxSTANDARD_CURSOR,memenv),"wxCursor");
   rt.addTupleCount(2);
   rt.addAtom("wxSWISS_FONT"); rt.addRef(getRef((void *)wxSWISS_FONT,memenv),"wxFont");
   rt.addTupleCount(2);
   rt.addAtom("wxTRANSPARENT_BRUSH"); rt.addRef(getRef((void *)wxTRANSPARENT_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxTRANSPARENT_PEN"); rt.addRef(getRef((void *)wxTRANSPARENT_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
   rt.addAtom("wxWHITE"); rt.add(*(wxWHITE));
   rt.addTupleCount(2);
   rt.addAtom("wxWHITE_BRUSH"); rt.addRef(getRef((void *)wxWHITE_BRUSH,memenv),"wxBrush");
   rt.addTupleCount(2);
   rt.addAtom("wxWHITE_PEN"); rt.addRef(getRef((void *)wxWHITE_PEN,memenv),"wxPen");
   rt.addTupleCount(2);
 rt.endList(56);
 rt.addTupleCount(2);
  rt.send();
}
