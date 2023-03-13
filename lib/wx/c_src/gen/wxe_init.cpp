/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
*/
 /* This file is also generated */
#include <wx/wx.h>
#include "../wxe_impl.h"
#include "wxe_macros.h"
#include "../wxe_return.h"

typedef struct {
  ERL_NIF_TERM type;
  const char * key;
  ERL_NIF_TERM value;
} wxe_defs;

void WxeApp::init_consts(wxeMetaCommand& event) {
  WxeApp * app = this;
  ErlNifPid caller = event.caller;
  wxeMemEnv * memenv = global_me;
  wxeReturn rt = wxeReturn(memenv, caller, true);
  wxe_defs defs[] = {
    { WXE_ATOM_global, "wxBLACK", rt.make(*(wxBLACK)) },
    { WXE_ATOM_global, "wxBLACK_BRUSH",rt.make_ref(app->getRef((void *)wxBLACK_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxBLACK_DASHED_PEN",rt.make_ref(app->getRef((void *)wxBLACK_DASHED_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxBLACK_PEN",rt.make_ref(app->getRef((void *)wxBLACK_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxBLUE", rt.make(*(wxBLUE)) },
    { WXE_ATOM_global, "wxBLUE_BRUSH",rt.make_ref(app->getRef((void *)wxBLUE_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxCROSS_CURSOR",rt.make_ref(app->getRef((void *)wxCROSS_CURSOR,memenv), "wxCursor") },
    { WXE_ATOM_global, "wxCYAN", rt.make(*(wxCYAN)) },
    { WXE_ATOM_global, "wxCYAN_BRUSH",rt.make_ref(app->getRef((void *)wxCYAN_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxCYAN_PEN",rt.make_ref(app->getRef((void *)wxCYAN_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxGREEN", rt.make(*(wxGREEN)) },
    { WXE_ATOM_global, "wxGREEN_BRUSH",rt.make_ref(app->getRef((void *)wxGREEN_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxGREEN_PEN",rt.make_ref(app->getRef((void *)wxGREEN_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxGREY_BRUSH",rt.make_ref(app->getRef((void *)wxGREY_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxGREY_PEN",rt.make_ref(app->getRef((void *)wxGREY_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxHOURGLASS_CURSOR",rt.make_ref(app->getRef((void *)wxHOURGLASS_CURSOR,memenv), "wxCursor") },
    { WXE_ATOM_global, "wxITALIC_FONT",rt.make_ref(app->getRef((void *)wxITALIC_FONT,memenv), "wxFont") },
    { WXE_ATOM_global, "wxLIGHT_GREY", rt.make(*(wxLIGHT_GREY)) },
    { WXE_ATOM_global, "wxLIGHT_GREY_BRUSH",rt.make_ref(app->getRef((void *)wxLIGHT_GREY_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxLIGHT_GREY_PEN",rt.make_ref(app->getRef((void *)wxLIGHT_GREY_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxMEDIUM_GREY_BRUSH",rt.make_ref(app->getRef((void *)wxMEDIUM_GREY_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxMEDIUM_GREY_PEN",rt.make_ref(app->getRef((void *)wxMEDIUM_GREY_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxNORMAL_FONT",rt.make_ref(app->getRef((void *)wxNORMAL_FONT,memenv), "wxFont") },
    { WXE_ATOM_global, "wxNullBitmap",rt.make_ref(app->getRef((void *)&wxNullBitmap,memenv), "wxBitmap") },
    { WXE_ATOM_global, "wxNullBrush",rt.make_ref(app->getRef((void *)&wxNullBrush,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxNullCursor",rt.make_ref(app->getRef((void *)&wxNullCursor,memenv), "wxCursor") },
    { WXE_ATOM_global, "wxNullFont",rt.make_ref(app->getRef((void *)&wxNullFont,memenv), "wxFont") },
    { WXE_ATOM_global, "wxNullIcon",rt.make_ref(app->getRef((void *)&wxNullIcon,memenv), "wxIcon") },
    { WXE_ATOM_global, "wxNullPalette",rt.make_ref(app->getRef((void *)&wxNullPalette,memenv), "wxPalette") },
    { WXE_ATOM_global, "wxNullPen",rt.make_ref(app->getRef((void *)&wxNullPen,memenv), "wxPen") },
    { WXE_ATOM_global, "wxRED", rt.make(*(wxRED)) },
    { WXE_ATOM_global, "wxRED_BRUSH",rt.make_ref(app->getRef((void *)wxRED_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxRED_PEN",rt.make_ref(app->getRef((void *)wxRED_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxSMALL_FONT",rt.make_ref(app->getRef((void *)wxSMALL_FONT,memenv), "wxFont") },
    { WXE_ATOM_global, "wxSTANDARD_CURSOR",rt.make_ref(app->getRef((void *)wxSTANDARD_CURSOR,memenv), "wxCursor") },
    { WXE_ATOM_global, "wxSWISS_FONT",rt.make_ref(app->getRef((void *)wxSWISS_FONT,memenv), "wxFont") },
    { WXE_ATOM_global, "wxTRANSPARENT_BRUSH",rt.make_ref(app->getRef((void *)wxTRANSPARENT_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxTRANSPARENT_PEN",rt.make_ref(app->getRef((void *)wxTRANSPARENT_PEN,memenv), "wxPen") },
    { WXE_ATOM_global, "wxWHITE", rt.make(*(wxWHITE)) },
    { WXE_ATOM_global, "wxWHITE_BRUSH",rt.make_ref(app->getRef((void *)wxWHITE_BRUSH,memenv), "wxBrush") },
    { WXE_ATOM_global, "wxWHITE_PEN",rt.make_ref(app->getRef((void *)wxWHITE_PEN,memenv), "wxPen") },
    { WXE_ATOM_define, "wxDefaultCoord", rt.make_int(-1) },
    { WXE_ATOM_define, "wxDefaultSize", enif_make_tuple2(rt.env, rt.make_int(-1), rt.make_int(-1)) },
    { WXE_ATOM_define, "wxDefaultPosition", enif_make_tuple2(rt.env, rt.make_int(-1), rt.make_int(-1)) },
//  From "anybutton.h"
    { WXE_ATOM_define, "wxBU_AUTODRAW", rt.make_int(wxBU_AUTODRAW) },
    { WXE_ATOM_define, "wxBU_NOTEXT", rt.make_int(wxBU_NOTEXT) },
    { WXE_ATOM_define, "wxBU_EXACTFIT", rt.make_int(wxBU_EXACTFIT) },
    { WXE_ATOM_define, "wxBU_ALIGN_MASK", rt.make_int(wxBU_ALIGN_MASK) },
    { WXE_ATOM_define, "wxBU_BOTTOM", rt.make_int(wxBU_BOTTOM) },
    { WXE_ATOM_define, "wxBU_RIGHT", rt.make_int(wxBU_RIGHT) },
    { WXE_ATOM_define, "wxBU_TOP", rt.make_int(wxBU_TOP) },
    { WXE_ATOM_define, "wxBU_LEFT", rt.make_int(wxBU_LEFT) },
//  From "bitmap.h"
    { WXE_ATOM_define, "wxBITMAP_SCREEN_DEPTH", rt.make_int(wxBITMAP_SCREEN_DEPTH) },
//  From "bookctrl.h"
    { WXE_ATOM_define, "wxBK_ALIGN_MASK", rt.make_int(wxBK_ALIGN_MASK) },
    { WXE_ATOM_define, "wxBK_RIGHT", rt.make_int(wxBK_RIGHT) },
    { WXE_ATOM_define, "wxBK_LEFT", rt.make_int(wxBK_LEFT) },
    { WXE_ATOM_define, "wxBK_BOTTOM", rt.make_int(wxBK_BOTTOM) },
    { WXE_ATOM_define, "wxBK_TOP", rt.make_int(wxBK_TOP) },
    { WXE_ATOM_define, "wxBK_DEFAULT", rt.make_int(wxBK_DEFAULT) },
//  From "checkbox.h"
    { WXE_ATOM_define, "wxCHK_ALLOW_3RD_STATE_FOR_USER", rt.make_int(wxCHK_ALLOW_3RD_STATE_FOR_USER) },
    { WXE_ATOM_define, "wxCHK_3STATE", rt.make_int(wxCHK_3STATE) },
    { WXE_ATOM_define, "wxCHK_2STATE", rt.make_int(wxCHK_2STATE) },
//  From "choicdlg.h"
    { WXE_ATOM_define, "wxCHOICEDLG_STYLE", rt.make_int(wxCHOICEDLG_STYLE) },
    { WXE_ATOM_define, "wxCHOICE_HEIGHT", rt.make_int(wxCHOICE_HEIGHT) },
    { WXE_ATOM_define, "wxCHOICE_WIDTH", rt.make_int(wxCHOICE_WIDTH) },
//  From "choicebk.h"
    { WXE_ATOM_define, "wxCHB_ALIGN_MASK", rt.make_int(wxCHB_ALIGN_MASK) },
    { WXE_ATOM_define, "wxCHB_RIGHT", rt.make_int(wxCHB_RIGHT) },
    { WXE_ATOM_define, "wxCHB_LEFT", rt.make_int(wxCHB_LEFT) },
    { WXE_ATOM_define, "wxCHB_BOTTOM", rt.make_int(wxCHB_BOTTOM) },
    { WXE_ATOM_define, "wxCHB_TOP", rt.make_int(wxCHB_TOP) },
    { WXE_ATOM_define, "wxCHB_DEFAULT", rt.make_int(wxCHB_DEFAULT) },
//  From "clrpicker.h"
#if wxCHECK_VERSION(3,1,0)
    { WXE_ATOM_define, "wxCLRP_SHOW_ALPHA", rt.make_int(wxCLRP_SHOW_ALPHA) },
#else
    { WXE_ATOM_define, "wxCLRP_SHOW_ALPHA", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxCLRP_SHOW_LABEL", rt.make_int(wxCLRP_SHOW_LABEL) },
    { WXE_ATOM_define, "wxCLRP_DEFAULT_STYLE", rt.make_int(wxCLRP_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxCLRP_USE_TEXTCTRL", rt.make_int(wxCLRP_USE_TEXTCTRL) },
//  From "datetime.h"
//  From "dcbuffer.h"
    { WXE_ATOM_define, "wxALWAYS_NATIVE_DOUBLE_BUFFER", rt.make_int(wxALWAYS_NATIVE_DOUBLE_BUFFER) },
    { WXE_ATOM_define, "wxBUFFER_USES_SHARED_BUFFER", rt.make_int(wxBUFFER_USES_SHARED_BUFFER) },
    { WXE_ATOM_define, "wxBUFFER_CLIENT_AREA", rt.make_int(wxBUFFER_CLIENT_AREA) },
    { WXE_ATOM_define, "wxBUFFER_VIRTUAL_AREA", rt.make_int(wxBUFFER_VIRTUAL_AREA) },
//  From "defs.h"
    { WXE_ATOM_define, "wxRESIZE_BORDER", rt.make_int(wxRESIZE_BORDER) },
    { WXE_ATOM_define, "wxTINY_CAPTION", rt.make_int(wxTINY_CAPTION) },
    { WXE_ATOM_define, "wxMAXIMIZE_BOX", rt.make_int(wxMAXIMIZE_BOX) },
    { WXE_ATOM_define, "wxMINIMIZE_BOX", rt.make_int(wxMINIMIZE_BOX) },
    { WXE_ATOM_define, "wxSYSTEM_MENU", rt.make_int(wxSYSTEM_MENU) },
    { WXE_ATOM_define, "wxCLOSE_BOX", rt.make_int(wxCLOSE_BOX) },
    { WXE_ATOM_define, "wxMAXIMIZE", rt.make_int(wxMAXIMIZE) },
    { WXE_ATOM_define, "wxMINIMIZE", rt.make_int(wxMINIMIZE) },
    { WXE_ATOM_define, "wxICONIZE", rt.make_int(wxICONIZE) },
    { WXE_ATOM_define, "wxSTAY_ON_TOP", rt.make_int(wxSTAY_ON_TOP) },
    { WXE_ATOM_define, "wxBETA_NUMBER", rt.make_int(wxBETA_NUMBER) },
    { WXE_ATOM_define, "wxSUBRELEASE_NUMBER", rt.make_int(wxSUBRELEASE_NUMBER) },
    { WXE_ATOM_define, "wxRELEASE_NUMBER", rt.make_int(wxRELEASE_NUMBER) },
    { WXE_ATOM_define, "wxMINOR_VERSION", rt.make_int(wxMINOR_VERSION) },
    { WXE_ATOM_define, "wxMAJOR_VERSION", rt.make_int(wxMAJOR_VERSION) },
    { WXE_ATOM_define, "wxBROWSER_NEW_WINDOW", rt.make_int(wxBROWSER_NEW_WINDOW) },
    { WXE_ATOM_define, "wxTOPLEVEL_EX_DIALOG", rt.make_int(wxTOPLEVEL_EX_DIALOG) },
    { WXE_ATOM_define, "wxWS_EX_VALIDATE_RECURSIVELY", rt.make_int(wxWS_EX_VALIDATE_RECURSIVELY) },
    { WXE_ATOM_define, "wxScrolledWindowStyle", rt.make_int(wxScrolledWindowStyle) },
    { WXE_ATOM_define, "wxLC_USER_TEXT", rt.make_int(wxLC_USER_TEXT) },
    { WXE_ATOM_define, "wxFRAME_SHAPED", rt.make_int(wxFRAME_SHAPED) },
    { WXE_ATOM_define, "wxID_TREECTRL", rt.make_int(wxID_TREECTRL) },
    { WXE_ATOM_define, "wxCENTER_ON_SCREEN", rt.make_int(wxCENTER_ON_SCREEN) },
    { WXE_ATOM_define, "wxCENTRE_ON_SCREEN", rt.make_int(wxCENTRE_ON_SCREEN) },
    { WXE_ATOM_define, "wxCENTER_FRAME", rt.make_int(wxCENTER_FRAME) },
    { WXE_ATOM_define, "wxPRINT_QUALITY_DRAFT", rt.make_int(wxPRINT_QUALITY_DRAFT) },
    { WXE_ATOM_define, "wxPRINT_QUALITY_LOW", rt.make_int(wxPRINT_QUALITY_LOW) },
    { WXE_ATOM_define, "wxPRINT_QUALITY_MEDIUM", rt.make_int(wxPRINT_QUALITY_MEDIUM) },
    { WXE_ATOM_define, "wxPRINT_QUALITY_HIGH", rt.make_int(wxPRINT_QUALITY_HIGH) },
    { WXE_ATOM_define, "wxBG_STYLE_CUSTOM", rt.make_int(wxBG_STYLE_CUSTOM) },
    { WXE_ATOM_define, "wxNOT_FOUND", rt.make_int(wxNOT_FOUND) },
    { WXE_ATOM_define, "wxICON_MASK", rt.make_int(wxICON_MASK) },
    { WXE_ATOM_define, "wxICON_AUTH_NEEDED", rt.make_int(wxICON_AUTH_NEEDED) },
    { WXE_ATOM_define, "wxICON_NONE", rt.make_int(wxICON_NONE) },
    { WXE_ATOM_define, "wxSETUP", rt.make_int(wxSETUP) },
    { WXE_ATOM_define, "wxMORE", rt.make_int(wxMORE) },
    { WXE_ATOM_define, "wxRESET", rt.make_int(wxRESET) },
    { WXE_ATOM_define, "wxBACKWARD", rt.make_int(wxBACKWARD) },
    { WXE_ATOM_define, "wxFORWARD", rt.make_int(wxFORWARD) },
    { WXE_ATOM_define, "wxHELP", rt.make_int(wxHELP) },
    { WXE_ATOM_define, "wxICON_ASTERISK", rt.make_int(wxICON_ASTERISK) },
    { WXE_ATOM_define, "wxICON_STOP", rt.make_int(wxICON_STOP) },
    { WXE_ATOM_define, "wxICON_INFORMATION", rt.make_int(wxICON_INFORMATION) },
    { WXE_ATOM_define, "wxICON_QUESTION", rt.make_int(wxICON_QUESTION) },
    { WXE_ATOM_define, "wxICON_ERROR", rt.make_int(wxICON_ERROR) },
    { WXE_ATOM_define, "wxICON_WARNING", rt.make_int(wxICON_WARNING) },
    { WXE_ATOM_define, "wxICON_HAND", rt.make_int(wxICON_HAND) },
    { WXE_ATOM_define, "wxICON_EXCLAMATION", rt.make_int(wxICON_EXCLAMATION) },
    { WXE_ATOM_define, "wxCANCEL_DEFAULT", rt.make_int(wxCANCEL_DEFAULT) },
    { WXE_ATOM_define, "wxNO_DEFAULT", rt.make_int(wxNO_DEFAULT) },
    { WXE_ATOM_define, "wxYES_DEFAULT", rt.make_int(wxYES_DEFAULT) },
    { WXE_ATOM_define, "wxOK_DEFAULT", rt.make_int(wxOK_DEFAULT) },
    { WXE_ATOM_define, "wxCLOSE", rt.make_int(wxCLOSE) },
    { WXE_ATOM_define, "wxAPPLY", rt.make_int(wxAPPLY) },
    { WXE_ATOM_define, "wxCANCEL", rt.make_int(wxCANCEL) },
    { WXE_ATOM_define, "wxYES_NO", rt.make_int(wxYES_NO) },
    { WXE_ATOM_define, "wxNO", rt.make_int(wxNO) },
    { WXE_ATOM_define, "wxOK", rt.make_int(wxOK) },
    { WXE_ATOM_define, "wxYES", rt.make_int(wxYES) },
    { WXE_ATOM_define, "wxLI_VERTICAL", rt.make_int(wxLI_VERTICAL) },
    { WXE_ATOM_define, "wxLI_HORIZONTAL", rt.make_int(wxLI_HORIZONTAL) },
    { WXE_ATOM_define, "wxBI_EXPAND", rt.make_int(wxBI_EXPAND) },
    { WXE_ATOM_define, "wxTC_OWNERDRAW", rt.make_int(wxTC_OWNERDRAW) },
    { WXE_ATOM_define, "wxTC_MULTILINE", rt.make_int(wxTC_MULTILINE) },
    { WXE_ATOM_define, "wxTC_BOTTOM", rt.make_int(wxTC_BOTTOM) },
    { WXE_ATOM_define, "wxTC_RIGHT", rt.make_int(wxTC_RIGHT) },
    { WXE_ATOM_define, "wxTC_LEFT", rt.make_int(wxTC_LEFT) },
    { WXE_ATOM_define, "wxTC_TOP", rt.make_int(wxTC_TOP) },
    { WXE_ATOM_define, "wxTC_FIXEDWIDTH", rt.make_int(wxTC_FIXEDWIDTH) },
    { WXE_ATOM_define, "wxTC_RIGHTJUSTIFY", rt.make_int(wxTC_RIGHTJUSTIFY) },
    { WXE_ATOM_define, "wxSP_WRAP", rt.make_int(wxSP_WRAP) },
    { WXE_ATOM_define, "wxSP_ARROW_KEYS", rt.make_int(wxSP_ARROW_KEYS) },
    { WXE_ATOM_define, "wxSP_VERTICAL", rt.make_int(wxSP_VERTICAL) },
    { WXE_ATOM_define, "wxSP_HORIZONTAL", rt.make_int(wxSP_HORIZONTAL) },
    { WXE_ATOM_define, "wxSB_VERTICAL", rt.make_int(wxSB_VERTICAL) },
    { WXE_ATOM_define, "wxSB_HORIZONTAL", rt.make_int(wxSB_HORIZONTAL) },
    { WXE_ATOM_define, "wxRB_SINGLE", rt.make_int(wxRB_SINGLE) },
    { WXE_ATOM_define, "wxRB_GROUP", rt.make_int(wxRB_GROUP) },
    { WXE_ATOM_define, "wxRA_VERTICAL", rt.make_int(wxRA_VERTICAL) },
    { WXE_ATOM_define, "wxRA_HORIZONTAL", rt.make_int(wxRA_HORIZONTAL) },
    { WXE_ATOM_define, "wxRA_SPECIFY_ROWS", rt.make_int(wxRA_SPECIFY_ROWS) },
    { WXE_ATOM_define, "wxRA_SPECIFY_COLS", rt.make_int(wxRA_SPECIFY_COLS) },
    { WXE_ATOM_define, "wxRA_TOPTOBOTTOM", rt.make_int(wxRA_TOPTOBOTTOM) },
    { WXE_ATOM_define, "wxRA_LEFTTORIGHT", rt.make_int(wxRA_LEFTTORIGHT) },
    { WXE_ATOM_define, "wxCB_DROPDOWN", rt.make_int(wxCB_DROPDOWN) },
    { WXE_ATOM_define, "wxCB_READONLY", rt.make_int(wxCB_READONLY) },
    { WXE_ATOM_define, "wxCB_SORT", rt.make_int(wxCB_SORT) },
    { WXE_ATOM_define, "wxCB_SIMPLE", rt.make_int(wxCB_SIMPLE) },
    { WXE_ATOM_define, "wxLB_INT_HEIGHT", rt.make_int(wxLB_INT_HEIGHT) },
    { WXE_ATOM_define, "wxLB_HSCROLL", rt.make_int(wxLB_HSCROLL) },
    { WXE_ATOM_define, "wxLB_NO_SB", rt.make_int(wxLB_NO_SB) },
    { WXE_ATOM_define, "wxLB_ALWAYS_SB", rt.make_int(wxLB_ALWAYS_SB) },
    { WXE_ATOM_define, "wxLB_OWNERDRAW", rt.make_int(wxLB_OWNERDRAW) },
    { WXE_ATOM_define, "wxLB_NEEDED_SB", rt.make_int(wxLB_NEEDED_SB) },
    { WXE_ATOM_define, "wxLB_EXTENDED", rt.make_int(wxLB_EXTENDED) },
    { WXE_ATOM_define, "wxLB_MULTIPLE", rt.make_int(wxLB_MULTIPLE) },
    { WXE_ATOM_define, "wxLB_SINGLE", rt.make_int(wxLB_SINGLE) },
    { WXE_ATOM_define, "wxLB_SORT", rt.make_int(wxLB_SORT) },
    { WXE_ATOM_define, "wxFIXED_LENGTH", rt.make_int(wxFIXED_LENGTH) },
    { WXE_ATOM_define, "wxCOLOURED", rt.make_int(wxCOLOURED) },
    { WXE_ATOM_define, "wxMENU_TEAROFF", rt.make_int(wxMENU_TEAROFF) },
    { WXE_ATOM_define, "wxMB_DOCKABLE", rt.make_int(wxMB_DOCKABLE) },
    { WXE_ATOM_define, "wxFRAME_NO_WINDOW_MENU", rt.make_int(wxFRAME_NO_WINDOW_MENU) },
    { WXE_ATOM_define, "wxFRAME_DRAWER", rt.make_int(wxFRAME_DRAWER) },
    { WXE_ATOM_define, "wxDIALOG_EX_CONTEXTHELP", rt.make_int(wxDIALOG_EX_CONTEXTHELP) },
    { WXE_ATOM_define, "wxFRAME_EX_CONTEXTHELP", rt.make_int(wxFRAME_EX_CONTEXTHELP) },
    { WXE_ATOM_define, "wxWS_EX_CONTEXTHELP", rt.make_int(wxWS_EX_CONTEXTHELP) },
    { WXE_ATOM_define, "wxDIALOG_EX_METAL", rt.make_int(wxDIALOG_EX_METAL) },
    { WXE_ATOM_define, "wxFRAME_EX_METAL", rt.make_int(wxFRAME_EX_METAL) },
    { WXE_ATOM_define, "wxWS_EX_PROCESS_UI_UPDATES", rt.make_int(wxWS_EX_PROCESS_UI_UPDATES) },
    { WXE_ATOM_define, "wxWS_EX_PROCESS_IDLE", rt.make_int(wxWS_EX_PROCESS_IDLE) },
    { WXE_ATOM_define, "wxWS_EX_THEMED_BACKGROUND", rt.make_int(wxWS_EX_THEMED_BACKGROUND) },
    { WXE_ATOM_define, "wxWS_EX_TRANSIENT", rt.make_int(wxWS_EX_TRANSIENT) },
    { WXE_ATOM_define, "wxWS_EX_BLOCK_EVENTS", rt.make_int(wxWS_EX_BLOCK_EVENTS) },
    { WXE_ATOM_define, "wxWINDOW_STYLE_MASK", rt.make_int(wxWINDOW_STYLE_MASK) },
    { WXE_ATOM_define, "wxNO_FULL_REPAINT_ON_RESIZE", rt.make_int(wxNO_FULL_REPAINT_ON_RESIZE) },
    { WXE_ATOM_define, "wxFULL_REPAINT_ON_RESIZE", rt.make_int(wxFULL_REPAINT_ON_RESIZE) },
    { WXE_ATOM_define, "wxPOPUP_WINDOW", rt.make_int(wxPOPUP_WINDOW) },
    { WXE_ATOM_define, "wxBACKINGSTORE", rt.make_int(wxBACKINGSTORE) },
    { WXE_ATOM_define, "wxRETAINED", rt.make_int(wxRETAINED) },
    { WXE_ATOM_define, "wxWANTS_CHARS", rt.make_int(wxWANTS_CHARS) },
    { WXE_ATOM_define, "wxTAB_TRAVERSAL", rt.make_int(wxTAB_TRAVERSAL) },
    { WXE_ATOM_define, "wxTRANSPARENT_WINDOW", rt.make_int(wxTRANSPARENT_WINDOW) },
    { WXE_ATOM_define, "wxCLIP_SIBLINGS", rt.make_int(wxCLIP_SIBLINGS) },
    { WXE_ATOM_define, "wxCLIP_CHILDREN", rt.make_int(wxCLIP_CHILDREN) },
    { WXE_ATOM_define, "wxALWAYS_SHOW_SB", rt.make_int(wxALWAYS_SHOW_SB) },
    { WXE_ATOM_define, "wxNO_BORDER", rt.make_int(wxNO_BORDER) },
    { WXE_ATOM_define, "wxSTATIC_BORDER", rt.make_int(wxSTATIC_BORDER) },
    { WXE_ATOM_define, "wxSIMPLE_BORDER", rt.make_int(wxSIMPLE_BORDER) },
    { WXE_ATOM_define, "wxBORDER", rt.make_int(wxBORDER) },
    { WXE_ATOM_define, "wxRAISED_BORDER", rt.make_int(wxRAISED_BORDER) },
    { WXE_ATOM_define, "wxSUNKEN_BORDER", rt.make_int(wxSUNKEN_BORDER) },
    { WXE_ATOM_define, "wxDOUBLE_BORDER", rt.make_int(wxDOUBLE_BORDER) },
    { WXE_ATOM_define, "wxCAPTION", rt.make_int(wxCAPTION) },
    { WXE_ATOM_define, "wxHSCROLL", rt.make_int(wxHSCROLL) },
    { WXE_ATOM_define, "wxVSCROLL", rt.make_int(wxVSCROLL) },
    { WXE_ATOM_define, "wxSIZE_FORCE_EVENT", rt.make_int(wxSIZE_FORCE_EVENT) },
    { WXE_ATOM_define, "wxSIZE_FORCE", rt.make_int(wxSIZE_FORCE) },
    { WXE_ATOM_define, "wxSIZE_NO_ADJUSTMENTS", rt.make_int(wxSIZE_NO_ADJUSTMENTS) },
    { WXE_ATOM_define, "wxSIZE_ALLOW_MINUS_ONE", rt.make_int(wxSIZE_ALLOW_MINUS_ONE) },
    { WXE_ATOM_define, "wxSIZE_USE_EXISTING", rt.make_int(wxSIZE_USE_EXISTING) },
    { WXE_ATOM_define, "wxSIZE_AUTO", rt.make_int(wxSIZE_AUTO) },
    { WXE_ATOM_define, "wxSIZE_AUTO_HEIGHT", rt.make_int(wxSIZE_AUTO_HEIGHT) },
    { WXE_ATOM_define, "wxSIZE_AUTO_WIDTH", rt.make_int(wxSIZE_AUTO_WIDTH) },
    { WXE_ATOM_define, "wxDEFAULT_CONTROL_BORDER", rt.make_int(wxDEFAULT_CONTROL_BORDER) },
//  From "dialog.h"
    { WXE_ATOM_define, "wxDIALOG_ADAPTATION_LOOSE_BUTTONS", rt.make_int(wxDIALOG_ADAPTATION_LOOSE_BUTTONS) },
    { WXE_ATOM_define, "wxDIALOG_ADAPTATION_ANY_SIZER", rt.make_int(wxDIALOG_ADAPTATION_ANY_SIZER) },
    { WXE_ATOM_define, "wxDIALOG_ADAPTATION_STANDARD_SIZER", rt.make_int(wxDIALOG_ADAPTATION_STANDARD_SIZER) },
    { WXE_ATOM_define, "wxDIALOG_ADAPTATION_NONE", rt.make_int(wxDIALOG_ADAPTATION_NONE) },
    { WXE_ATOM_define, "wxDEFAULT_DIALOG_STYLE", rt.make_int(wxDEFAULT_DIALOG_STYLE) },
    { WXE_ATOM_define, "wxDIALOG_NO_PARENT", rt.make_int(wxDIALOG_NO_PARENT) },
//  From "dirdlg.h"
    { WXE_ATOM_define, "wxDD_DEFAULT_STYLE", rt.make_int(wxDD_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxDD_NEW_DIR_BUTTON", rt.make_int(wxDD_NEW_DIR_BUTTON) },
#if wxCHECK_VERSION(3,1,4)
    { WXE_ATOM_define, "wxDD_SHOW_HIDDEN", rt.make_int(wxDD_SHOW_HIDDEN) },
#else
    { WXE_ATOM_define, "wxDD_SHOW_HIDDEN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,4)
    { WXE_ATOM_define, "wxDD_MULTIPLE", rt.make_int(wxDD_MULTIPLE) },
#else
    { WXE_ATOM_define, "wxDD_MULTIPLE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxDD_DIR_MUST_EXIST", rt.make_int(wxDD_DIR_MUST_EXIST) },
    { WXE_ATOM_define, "wxDD_CHANGE_DIR", rt.make_int(wxDD_CHANGE_DIR) },
//  From "filedlg.h"
    { WXE_ATOM_define, "wxFD_DEFAULT_STYLE", rt.make_int(wxFD_DEFAULT_STYLE) },
//  From "filepicker.h"
    { WXE_ATOM_define, "wxDIRP_DEFAULT_STYLE", rt.make_int(wxDIRP_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxDIRP_USE_TEXTCTRL", rt.make_int(wxDIRP_USE_TEXTCTRL) },
    { WXE_ATOM_define, "wxDIRP_SMALL", rt.make_int(wxDIRP_SMALL) },
    { WXE_ATOM_define, "wxDIRP_CHANGE_DIR", rt.make_int(wxDIRP_CHANGE_DIR) },
    { WXE_ATOM_define, "wxDIRP_DIR_MUST_EXIST", rt.make_int(wxDIRP_DIR_MUST_EXIST) },
    { WXE_ATOM_define, "wxFLP_DEFAULT_STYLE", rt.make_int(wxFLP_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxFLP_USE_TEXTCTRL", rt.make_int(wxFLP_USE_TEXTCTRL) },
    { WXE_ATOM_define, "wxFLP_SMALL", rt.make_int(wxFLP_SMALL) },
    { WXE_ATOM_define, "wxFLP_CHANGE_DIR", rt.make_int(wxFLP_CHANGE_DIR) },
    { WXE_ATOM_define, "wxFLP_FILE_MUST_EXIST", rt.make_int(wxFLP_FILE_MUST_EXIST) },
    { WXE_ATOM_define, "wxFLP_OVERWRITE_PROMPT", rt.make_int(wxFLP_OVERWRITE_PROMPT) },
    { WXE_ATOM_define, "wxFLP_SAVE", rt.make_int(wxFLP_SAVE) },
    { WXE_ATOM_define, "wxFLP_OPEN", rt.make_int(wxFLP_OPEN) },
//  From "fontpicker.h"
    { WXE_ATOM_define, "wxFNTP_DEFAULT_STYLE", rt.make_int(wxFNTP_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxFNTP_USE_TEXTCTRL", rt.make_int(wxFNTP_USE_TEXTCTRL) },
    { WXE_ATOM_define, "wxFONTBTN_DEFAULT_STYLE", rt.make_int(wxFONTBTN_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxFNTP_USEFONT_FOR_LABEL", rt.make_int(wxFNTP_USEFONT_FOR_LABEL) },
    { WXE_ATOM_define, "wxFNTP_FONTDESC_AS_LABEL", rt.make_int(wxFNTP_FONTDESC_AS_LABEL) },
//  From "frame.h"
    { WXE_ATOM_define, "wxFRAME_FLOAT_ON_PARENT", rt.make_int(wxFRAME_FLOAT_ON_PARENT) },
    { WXE_ATOM_define, "wxFRAME_TOOL_WINDOW", rt.make_int(wxFRAME_TOOL_WINDOW) },
    { WXE_ATOM_define, "wxFRAME_NO_TASKBAR", rt.make_int(wxFRAME_NO_TASKBAR) },
//  From "gauge.h"
#if wxCHECK_VERSION(3,1,0)
    { WXE_ATOM_define, "wxGA_TEXT", rt.make_int(wxGA_TEXT) },
#else
    { WXE_ATOM_define, "wxGA_TEXT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxGA_SMOOTH", rt.make_int(wxGA_SMOOTH) },
#if wxCHECK_VERSION(3,1,0)
    { WXE_ATOM_define, "wxGA_PROGRESS", rt.make_int(wxGA_PROGRESS) },
#else
    { WXE_ATOM_define, "wxGA_PROGRESS", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxGA_VERTICAL", rt.make_int(wxGA_VERTICAL) },
    { WXE_ATOM_define, "wxGA_HORIZONTAL", rt.make_int(wxGA_HORIZONTAL) },
//  From "grid.h"
    { WXE_ATOM_define, "wxGRID_AUTOSIZE", rt.make_int(wxGRID_AUTOSIZE) },
//  From "htmlwin.h"
    { WXE_ATOM_define, "wxHW_DEFAULT_STYLE", rt.make_int(wxHW_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxHW_NO_SELECTION", rt.make_int(wxHW_NO_SELECTION) },
    { WXE_ATOM_define, "wxHW_SCROLLBAR_AUTO", rt.make_int(wxHW_SCROLLBAR_AUTO) },
    { WXE_ATOM_define, "wxHW_SCROLLBAR_NEVER", rt.make_int(wxHW_SCROLLBAR_NEVER) },
//  From "icon.h"
    { WXE_ATOM_define, "wxICON_SCREEN_DEPTH", rt.make_int(wxICON_SCREEN_DEPTH) },
//  From "imaglist.h"
    { WXE_ATOM_define, "wxIMAGELIST_DRAW_FOCUSED", rt.make_int(wxIMAGELIST_DRAW_FOCUSED) },
    { WXE_ATOM_define, "wxIMAGELIST_DRAW_SELECTED", rt.make_int(wxIMAGELIST_DRAW_SELECTED) },
    { WXE_ATOM_define, "wxIMAGELIST_DRAW_TRANSPARENT", rt.make_int(wxIMAGELIST_DRAW_TRANSPARENT) },
    { WXE_ATOM_define, "wxIMAGELIST_DRAW_NORMAL", rt.make_int(wxIMAGELIST_DRAW_NORMAL) },
//  From "listbook.h"
    { WXE_ATOM_define, "wxLB_ALIGN_MASK", rt.make_int(wxLB_ALIGN_MASK) },
    { WXE_ATOM_define, "wxLB_RIGHT", rt.make_int(wxLB_RIGHT) },
    { WXE_ATOM_define, "wxLB_LEFT", rt.make_int(wxLB_LEFT) },
    { WXE_ATOM_define, "wxLB_BOTTOM", rt.make_int(wxLB_BOTTOM) },
    { WXE_ATOM_define, "wxLB_TOP", rt.make_int(wxLB_TOP) },
    { WXE_ATOM_define, "wxLB_DEFAULT", rt.make_int(wxLB_DEFAULT) },
//  From "listctrl.h"
    { WXE_ATOM_define, "wxLIST_HITTEST_ONITEM", rt.make_int(wxLIST_HITTEST_ONITEM) },
    { WXE_ATOM_define, "wxLIST_HITTEST_TORIGHT", rt.make_int(wxLIST_HITTEST_TORIGHT) },
    { WXE_ATOM_define, "wxLIST_HITTEST_TOLEFT", rt.make_int(wxLIST_HITTEST_TOLEFT) },
    { WXE_ATOM_define, "wxLIST_HITTEST_ONITEMSTATEICON", rt.make_int(wxLIST_HITTEST_ONITEMSTATEICON) },
    { WXE_ATOM_define, "wxLIST_HITTEST_ONITEMLABEL", rt.make_int(wxLIST_HITTEST_ONITEMLABEL) },
    { WXE_ATOM_define, "wxLIST_HITTEST_ONITEMICON", rt.make_int(wxLIST_HITTEST_ONITEMICON) },
    { WXE_ATOM_define, "wxLIST_HITTEST_NOWHERE", rt.make_int(wxLIST_HITTEST_NOWHERE) },
    { WXE_ATOM_define, "wxLIST_HITTEST_BELOW", rt.make_int(wxLIST_HITTEST_BELOW) },
    { WXE_ATOM_define, "wxLIST_HITTEST_ABOVE", rt.make_int(wxLIST_HITTEST_ABOVE) },
    { WXE_ATOM_define, "wxLIST_STATE_CUT", rt.make_int(wxLIST_STATE_CUT) },
    { WXE_ATOM_define, "wxLIST_STATE_SELECTED", rt.make_int(wxLIST_STATE_SELECTED) },
    { WXE_ATOM_define, "wxLIST_STATE_FOCUSED", rt.make_int(wxLIST_STATE_FOCUSED) },
    { WXE_ATOM_define, "wxLIST_STATE_DROPHILITED", rt.make_int(wxLIST_STATE_DROPHILITED) },
    { WXE_ATOM_define, "wxLIST_STATE_DONTCARE", rt.make_int(wxLIST_STATE_DONTCARE) },
    { WXE_ATOM_define, "wxLIST_MASK_FORMAT", rt.make_int(wxLIST_MASK_FORMAT) },
    { WXE_ATOM_define, "wxLIST_MASK_WIDTH", rt.make_int(wxLIST_MASK_WIDTH) },
    { WXE_ATOM_define, "wxLIST_SET_ITEM", rt.make_int(wxLIST_SET_ITEM) },
    { WXE_ATOM_define, "wxLIST_MASK_DATA", rt.make_int(wxLIST_MASK_DATA) },
    { WXE_ATOM_define, "wxLIST_MASK_IMAGE", rt.make_int(wxLIST_MASK_IMAGE) },
    { WXE_ATOM_define, "wxLIST_MASK_TEXT", rt.make_int(wxLIST_MASK_TEXT) },
    { WXE_ATOM_define, "wxLIST_MASK_STATE", rt.make_int(wxLIST_MASK_STATE) },
    { WXE_ATOM_define, "wxLC_MASK_SORT", rt.make_int(wxLC_MASK_SORT) },
    { WXE_ATOM_define, "wxLC_MASK_ALIGN", rt.make_int(wxLC_MASK_ALIGN) },
    { WXE_ATOM_define, "wxLC_MASK_TYPE", rt.make_int(wxLC_MASK_TYPE) },
    { WXE_ATOM_define, "wxLC_SORT_DESCENDING", rt.make_int(wxLC_SORT_DESCENDING) },
    { WXE_ATOM_define, "wxLC_SORT_ASCENDING", rt.make_int(wxLC_SORT_ASCENDING) },
    { WXE_ATOM_define, "wxLC_SINGLE_SEL", rt.make_int(wxLC_SINGLE_SEL) },
    { WXE_ATOM_define, "wxLC_NO_SORT_HEADER", rt.make_int(wxLC_NO_SORT_HEADER) },
    { WXE_ATOM_define, "wxLC_NO_HEADER", rt.make_int(wxLC_NO_HEADER) },
    { WXE_ATOM_define, "wxLC_EDIT_LABELS", rt.make_int(wxLC_EDIT_LABELS) },
    { WXE_ATOM_define, "wxLC_VIRTUAL", rt.make_int(wxLC_VIRTUAL) },
    { WXE_ATOM_define, "wxLC_AUTOARRANGE", rt.make_int(wxLC_AUTOARRANGE) },
    { WXE_ATOM_define, "wxLC_ALIGN_LEFT", rt.make_int(wxLC_ALIGN_LEFT) },
    { WXE_ATOM_define, "wxLC_ALIGN_TOP", rt.make_int(wxLC_ALIGN_TOP) },
    { WXE_ATOM_define, "wxLC_REPORT", rt.make_int(wxLC_REPORT) },
    { WXE_ATOM_define, "wxLC_LIST", rt.make_int(wxLC_LIST) },
    { WXE_ATOM_define, "wxLC_SMALL_ICON", rt.make_int(wxLC_SMALL_ICON) },
    { WXE_ATOM_define, "wxLC_ICON", rt.make_int(wxLC_ICON) },
    { WXE_ATOM_define, "wxLC_HRULES", rt.make_int(wxLC_HRULES) },
    { WXE_ATOM_define, "wxLC_VRULES", rt.make_int(wxLC_VRULES) },
//  From "nonownedwnd.h"
    { WXE_ATOM_define, "wxFRAME_SHAPED", rt.make_int(wxFRAME_SHAPED) },
//  From "notebook.h"
    { WXE_ATOM_define, "wxNB_NOPAGETHEME", rt.make_int(wxNB_NOPAGETHEME) },
    { WXE_ATOM_define, "wxNB_MULTILINE", rt.make_int(wxNB_MULTILINE) },
    { WXE_ATOM_define, "wxNB_FIXEDWIDTH", rt.make_int(wxNB_FIXEDWIDTH) },
    { WXE_ATOM_define, "wxNB_RIGHT", rt.make_int(wxNB_RIGHT) },
    { WXE_ATOM_define, "wxNB_LEFT", rt.make_int(wxNB_LEFT) },
    { WXE_ATOM_define, "wxNB_BOTTOM", rt.make_int(wxNB_BOTTOM) },
    { WXE_ATOM_define, "wxNB_TOP", rt.make_int(wxNB_TOP) },
    { WXE_ATOM_define, "wxNB_DEFAULT", rt.make_int(wxNB_DEFAULT) },
//  From "pickerbase.h"
    { WXE_ATOM_define, "wxPB_SMALL", rt.make_int(wxPB_SMALL) },
    { WXE_ATOM_define, "wxPB_USE_TEXTCTRL", rt.make_int(wxPB_USE_TEXTCTRL) },
//  From "popupwin.h"
#if wxCHECK_VERSION(3,1,3)
    { WXE_ATOM_define, "wxPU_CONTAINS_CONTROLS", rt.make_int(wxPU_CONTAINS_CONTROLS) },
#else
    { WXE_ATOM_define, "wxPU_CONTAINS_CONTROLS", WXE_ATOM_undefined },
#endif
//  From "print.h"
    { WXE_ATOM_define, "wxID_PREVIEW_ZOOM_OUT", rt.make_int(wxID_PREVIEW_ZOOM_OUT) },
    { WXE_ATOM_define, "wxID_PREVIEW_ZOOM_IN", rt.make_int(wxID_PREVIEW_ZOOM_IN) },
    { WXE_ATOM_define, "wxID_PREVIEW_GOTO", rt.make_int(wxID_PREVIEW_GOTO) },
    { WXE_ATOM_define, "wxID_PREVIEW_LAST", rt.make_int(wxID_PREVIEW_LAST) },
    { WXE_ATOM_define, "wxID_PREVIEW_FIRST", rt.make_int(wxID_PREVIEW_FIRST) },
    { WXE_ATOM_define, "wxID_PREVIEW_ZOOM", rt.make_int(wxID_PREVIEW_ZOOM) },
    { WXE_ATOM_define, "wxID_PREVIEW_PRINT", rt.make_int(wxID_PREVIEW_PRINT) },
    { WXE_ATOM_define, "wxID_PREVIEW_PREVIOUS", rt.make_int(wxID_PREVIEW_PREVIOUS) },
    { WXE_ATOM_define, "wxID_PREVIEW_NEXT", rt.make_int(wxID_PREVIEW_NEXT) },
    { WXE_ATOM_define, "wxID_PREVIEW_CLOSE", rt.make_int(wxID_PREVIEW_CLOSE) },
    { WXE_ATOM_define, "wxPREVIEW_DEFAULT", rt.make_int(wxPREVIEW_DEFAULT) },
    { WXE_ATOM_define, "wxPREVIEW_GOTO", rt.make_int(wxPREVIEW_GOTO) },
    { WXE_ATOM_define, "wxPREVIEW_LAST", rt.make_int(wxPREVIEW_LAST) },
    { WXE_ATOM_define, "wxPREVIEW_FIRST", rt.make_int(wxPREVIEW_FIRST) },
    { WXE_ATOM_define, "wxPREVIEW_ZOOM", rt.make_int(wxPREVIEW_ZOOM) },
    { WXE_ATOM_define, "wxPREVIEW_NEXT", rt.make_int(wxPREVIEW_NEXT) },
    { WXE_ATOM_define, "wxPREVIEW_PREVIOUS", rt.make_int(wxPREVIEW_PREVIOUS) },
    { WXE_ATOM_define, "wxPREVIEW_PRINT", rt.make_int(wxPREVIEW_PRINT) },
//  From "progdlg.h"
    { WXE_ATOM_define, "wxPD_CAN_SKIP", rt.make_int(wxPD_CAN_SKIP) },
    { WXE_ATOM_define, "wxPD_REMAINING_TIME", rt.make_int(wxPD_REMAINING_TIME) },
    { WXE_ATOM_define, "wxPD_SMOOTH", rt.make_int(wxPD_SMOOTH) },
    { WXE_ATOM_define, "wxPD_ESTIMATED_TIME", rt.make_int(wxPD_ESTIMATED_TIME) },
    { WXE_ATOM_define, "wxPD_ELAPSED_TIME", rt.make_int(wxPD_ELAPSED_TIME) },
    { WXE_ATOM_define, "wxPD_AUTO_HIDE", rt.make_int(wxPD_AUTO_HIDE) },
    { WXE_ATOM_define, "wxPD_APP_MODAL", rt.make_int(wxPD_APP_MODAL) },
    { WXE_ATOM_define, "wxPD_CAN_ABORT", rt.make_int(wxPD_CAN_ABORT) },
//  From "sashwin.h"
    { WXE_ATOM_define, "wxSW_3D", rt.make_int(wxSW_3D) },
    { WXE_ATOM_define, "wxSW_3DBORDER", rt.make_int(wxSW_3DBORDER) },
    { WXE_ATOM_define, "wxSW_3DSASH", rt.make_int(wxSW_3DSASH) },
    { WXE_ATOM_define, "wxSW_BORDER", rt.make_int(wxSW_BORDER) },
    { WXE_ATOM_define, "wxSW_NOBORDER", rt.make_int(wxSW_NOBORDER) },
//  From "slider.h"
    { WXE_ATOM_define, "wxSL_LABELS", rt.make_int(wxSL_LABELS) },
    { WXE_ATOM_define, "wxSL_VALUE_LABEL", rt.make_int(wxSL_VALUE_LABEL) },
    { WXE_ATOM_define, "wxSL_MIN_MAX_LABELS", rt.make_int(wxSL_MIN_MAX_LABELS) },
    { WXE_ATOM_define, "wxSL_INVERSE", rt.make_int(wxSL_INVERSE) },
    { WXE_ATOM_define, "wxSL_SELRANGE", rt.make_int(wxSL_SELRANGE) },
    { WXE_ATOM_define, "wxSL_BOTH", rt.make_int(wxSL_BOTH) },
    { WXE_ATOM_define, "wxSL_BOTTOM", rt.make_int(wxSL_BOTTOM) },
    { WXE_ATOM_define, "wxSL_RIGHT", rt.make_int(wxSL_RIGHT) },
    { WXE_ATOM_define, "wxSL_TOP", rt.make_int(wxSL_TOP) },
    { WXE_ATOM_define, "wxSL_LEFT", rt.make_int(wxSL_LEFT) },
    { WXE_ATOM_define, "wxSL_AUTOTICKS", rt.make_int(wxSL_AUTOTICKS) },
    { WXE_ATOM_define, "wxSL_TICKS", rt.make_int(wxSL_TICKS) },
    { WXE_ATOM_define, "wxSL_VERTICAL", rt.make_int(wxSL_VERTICAL) },
    { WXE_ATOM_define, "wxSL_HORIZONTAL", rt.make_int(wxSL_HORIZONTAL) },
//  From "splash.h"
    { WXE_ATOM_define, "wxSPLASH_NO_TIMEOUT", rt.make_int(wxSPLASH_NO_TIMEOUT) },
    { WXE_ATOM_define, "wxSPLASH_TIMEOUT", rt.make_int(wxSPLASH_TIMEOUT) },
    { WXE_ATOM_define, "wxSPLASH_NO_CENTRE", rt.make_int(wxSPLASH_NO_CENTRE) },
    { WXE_ATOM_define, "wxSPLASH_CENTRE_ON_SCREEN", rt.make_int(wxSPLASH_CENTRE_ON_SCREEN) },
    { WXE_ATOM_define, "wxSPLASH_CENTRE_ON_PARENT", rt.make_int(wxSPLASH_CENTRE_ON_PARENT) },
//  From "splitter.h"
    { WXE_ATOM_define, "wxSP_3D", rt.make_int(wxSP_3D) },
    { WXE_ATOM_define, "wxSP_BORDER", rt.make_int(wxSP_BORDER) },
    { WXE_ATOM_define, "wxSP_NO_XP_THEME", rt.make_int(wxSP_NO_XP_THEME) },
    { WXE_ATOM_define, "wxSP_3DBORDER", rt.make_int(wxSP_3DBORDER) },
    { WXE_ATOM_define, "wxSP_3DSASH", rt.make_int(wxSP_3DSASH) },
    { WXE_ATOM_define, "wxSP_LIVE_UPDATE", rt.make_int(wxSP_LIVE_UPDATE) },
    { WXE_ATOM_define, "wxSP_PERMIT_UNSPLIT", rt.make_int(wxSP_PERMIT_UNSPLIT) },
    { WXE_ATOM_define, "wxSP_NOSASH", rt.make_int(wxSP_NOSASH) },
    { WXE_ATOM_define, "wxSP_THIN_SASH", rt.make_int(wxSP_THIN_SASH) },
    { WXE_ATOM_define, "wxSP_NOBORDER", rt.make_int(wxSP_NOBORDER) },
//  From "stattext.h"
    { WXE_ATOM_define, "wxST_ELLIPSIZE_END", rt.make_int(wxST_ELLIPSIZE_END) },
    { WXE_ATOM_define, "wxST_ELLIPSIZE_MIDDLE", rt.make_int(wxST_ELLIPSIZE_MIDDLE) },
    { WXE_ATOM_define, "wxST_ELLIPSIZE_START", rt.make_int(wxST_ELLIPSIZE_START) },
    { WXE_ATOM_define, "wxST_NO_AUTORESIZE", rt.make_int(wxST_NO_AUTORESIZE) },
//  From "statusbr.h"
    { WXE_ATOM_define, "wxSB_SUNKEN", rt.make_int(wxSB_SUNKEN) },
    { WXE_ATOM_define, "wxSB_RAISED", rt.make_int(wxSB_RAISED) },
    { WXE_ATOM_define, "wxSB_FLAT", rt.make_int(wxSB_FLAT) },
    { WXE_ATOM_define, "wxSB_NORMAL", rt.make_int(wxSB_NORMAL) },
    { WXE_ATOM_define, "wxSTB_DEFAULT_STYLE", rt.make_int(wxSTB_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxSTB_ELLIPSIZE_END", rt.make_int(wxSTB_ELLIPSIZE_END) },
    { WXE_ATOM_define, "wxSTB_ELLIPSIZE_MIDDLE", rt.make_int(wxSTB_ELLIPSIZE_MIDDLE) },
    { WXE_ATOM_define, "wxSTB_ELLIPSIZE_START", rt.make_int(wxSTB_ELLIPSIZE_START) },
    { WXE_ATOM_define, "wxSTB_SHOW_TIPS", rt.make_int(wxSTB_SHOW_TIPS) },
    { WXE_ATOM_define, "wxSTB_SIZEGRIP", rt.make_int(wxSTB_SIZEGRIP) },
//  From "stc.h"
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEDISPLAYEXTEND", rt.make_int(wxSTC_CMD_VCHOMEDISPLAYEXTEND) },
#else
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEDISPLAYEXTEND", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEDISPLAY", rt.make_int(wxSTC_CMD_VCHOMEDISPLAY) },
#else
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEDISPLAY", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_CMD_SCROLLTOEND", rt.make_int(wxSTC_CMD_SCROLLTOEND) },
    { WXE_ATOM_define, "wxSTC_CMD_SCROLLTOSTART", rt.make_int(wxSTC_CMD_SCROLLTOSTART) },
    { WXE_ATOM_define, "wxSTC_CMD_MOVESELECTEDLINESDOWN", rt.make_int(wxSTC_CMD_MOVESELECTEDLINESDOWN) },
    { WXE_ATOM_define, "wxSTC_CMD_MOVESELECTEDLINESUP", rt.make_int(wxSTC_CMD_MOVESELECTEDLINESUP) },
    { WXE_ATOM_define, "wxSTC_CMD_VERTICALCENTRECARET", rt.make_int(wxSTC_CMD_VERTICALCENTRECARET) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDRIGHTENDEXTEND", rt.make_int(wxSTC_CMD_WORDRIGHTENDEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDRIGHTEND", rt.make_int(wxSTC_CMD_WORDRIGHTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDLEFTENDEXTEND", rt.make_int(wxSTC_CMD_WORDLEFTENDEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDLEFTEND", rt.make_int(wxSTC_CMD_WORDLEFTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_STUTTEREDPAGEDOWNEXTEND", rt.make_int(wxSTC_CMD_STUTTEREDPAGEDOWNEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_STUTTEREDPAGEDOWN", rt.make_int(wxSTC_CMD_STUTTEREDPAGEDOWN) },
    { WXE_ATOM_define, "wxSTC_CMD_STUTTEREDPAGEUPEXTEND", rt.make_int(wxSTC_CMD_STUTTEREDPAGEUPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_STUTTEREDPAGEUP", rt.make_int(wxSTC_CMD_STUTTEREDPAGEUP) },
    { WXE_ATOM_define, "wxSTC_CMD_PAGEDOWNRECTEXTEND", rt.make_int(wxSTC_CMD_PAGEDOWNRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_PAGEUPRECTEXTEND", rt.make_int(wxSTC_CMD_PAGEUPRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEENDRECTEXTEND", rt.make_int(wxSTC_CMD_LINEENDRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMERECTEXTEND", rt.make_int(wxSTC_CMD_VCHOMERECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_HOMERECTEXTEND", rt.make_int(wxSTC_CMD_HOMERECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_CHARRIGHTRECTEXTEND", rt.make_int(wxSTC_CMD_CHARRIGHTRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_CHARLEFTRECTEXTEND", rt.make_int(wxSTC_CMD_CHARLEFTRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEUPRECTEXTEND", rt.make_int(wxSTC_CMD_LINEUPRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEDOWNRECTEXTEND", rt.make_int(wxSTC_CMD_LINEDOWNRECTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_PARAUPEXTEND", rt.make_int(wxSTC_CMD_PARAUPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_PARAUP", rt.make_int(wxSTC_CMD_PARAUP) },
    { WXE_ATOM_define, "wxSTC_CMD_PARADOWNEXTEND", rt.make_int(wxSTC_CMD_PARADOWNEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_PARADOWN", rt.make_int(wxSTC_CMD_PARADOWN) },
    { WXE_ATOM_define, "wxSTC_CMD_DELLINERIGHT", rt.make_int(wxSTC_CMD_DELLINERIGHT) },
    { WXE_ATOM_define, "wxSTC_CMD_DELLINELEFT", rt.make_int(wxSTC_CMD_DELLINELEFT) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDPARTRIGHTEXTEND", rt.make_int(wxSTC_CMD_WORDPARTRIGHTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDPARTRIGHT", rt.make_int(wxSTC_CMD_WORDPARTRIGHT) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDPARTLEFTEXTEND", rt.make_int(wxSTC_CMD_WORDPARTLEFTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDPARTLEFT", rt.make_int(wxSTC_CMD_WORDPARTLEFT) },
    { WXE_ATOM_define, "wxSTC_CMD_LINECOPY", rt.make_int(wxSTC_CMD_LINECOPY) },
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEWRAPEXTEND", rt.make_int(wxSTC_CMD_VCHOMEWRAPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEWRAP", rt.make_int(wxSTC_CMD_VCHOMEWRAP) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEENDWRAPEXTEND", rt.make_int(wxSTC_CMD_LINEENDWRAPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEENDWRAP", rt.make_int(wxSTC_CMD_LINEENDWRAP) },
    { WXE_ATOM_define, "wxSTC_CMD_HOMEWRAPEXTEND", rt.make_int(wxSTC_CMD_HOMEWRAPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_HOMEWRAP", rt.make_int(wxSTC_CMD_HOMEWRAP) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEENDDISPLAYEXTEND", rt.make_int(wxSTC_CMD_LINEENDDISPLAYEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEENDDISPLAY", rt.make_int(wxSTC_CMD_LINEENDDISPLAY) },
    { WXE_ATOM_define, "wxSTC_CMD_HOMEDISPLAYEXTEND", rt.make_int(wxSTC_CMD_HOMEDISPLAYEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_HOMEDISPLAY", rt.make_int(wxSTC_CMD_HOMEDISPLAY) },
    { WXE_ATOM_define, "wxSTC_CMD_DELETEBACKNOTLINE", rt.make_int(wxSTC_CMD_DELETEBACKNOTLINE) },
    { WXE_ATOM_define, "wxSTC_CMD_LINESCROLLUP", rt.make_int(wxSTC_CMD_LINESCROLLUP) },
    { WXE_ATOM_define, "wxSTC_CMD_LINESCROLLDOWN", rt.make_int(wxSTC_CMD_LINESCROLLDOWN) },
    { WXE_ATOM_define, "wxSTC_CMD_UPPERCASE", rt.make_int(wxSTC_CMD_UPPERCASE) },
    { WXE_ATOM_define, "wxSTC_CMD_LOWERCASE", rt.make_int(wxSTC_CMD_LOWERCASE) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEDUPLICATE", rt.make_int(wxSTC_CMD_LINEDUPLICATE) },
    { WXE_ATOM_define, "wxSTC_CMD_LINETRANSPOSE", rt.make_int(wxSTC_CMD_LINETRANSPOSE) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEDELETE", rt.make_int(wxSTC_CMD_LINEDELETE) },
    { WXE_ATOM_define, "wxSTC_CMD_LINECUT", rt.make_int(wxSTC_CMD_LINECUT) },
    { WXE_ATOM_define, "wxSTC_CMD_DELWORDRIGHTEND", rt.make_int(wxSTC_CMD_DELWORDRIGHTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_DELWORDRIGHT", rt.make_int(wxSTC_CMD_DELWORDRIGHT) },
    { WXE_ATOM_define, "wxSTC_CMD_DELWORDLEFT", rt.make_int(wxSTC_CMD_DELWORDLEFT) },
    { WXE_ATOM_define, "wxSTC_CMD_ZOOMOUT", rt.make_int(wxSTC_CMD_ZOOMOUT) },
    { WXE_ATOM_define, "wxSTC_CMD_ZOOMIN", rt.make_int(wxSTC_CMD_ZOOMIN) },
    { WXE_ATOM_define, "wxSTC_CMD_VCHOMEEXTEND", rt.make_int(wxSTC_CMD_VCHOMEEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_VCHOME", rt.make_int(wxSTC_CMD_VCHOME) },
    { WXE_ATOM_define, "wxSTC_CMD_FORMFEED", rt.make_int(wxSTC_CMD_FORMFEED) },
    { WXE_ATOM_define, "wxSTC_CMD_NEWLINE", rt.make_int(wxSTC_CMD_NEWLINE) },
    { WXE_ATOM_define, "wxSTC_CMD_BACKTAB", rt.make_int(wxSTC_CMD_BACKTAB) },
    { WXE_ATOM_define, "wxSTC_CMD_TAB", rt.make_int(wxSTC_CMD_TAB) },
    { WXE_ATOM_define, "wxSTC_CMD_DELETEBACK", rt.make_int(wxSTC_CMD_DELETEBACK) },
    { WXE_ATOM_define, "wxSTC_CMD_CANCEL", rt.make_int(wxSTC_CMD_CANCEL) },
    { WXE_ATOM_define, "wxSTC_CMD_EDITTOGGLEOVERTYPE", rt.make_int(wxSTC_CMD_EDITTOGGLEOVERTYPE) },
    { WXE_ATOM_define, "wxSTC_CMD_PAGEDOWNEXTEND", rt.make_int(wxSTC_CMD_PAGEDOWNEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_PAGEDOWN", rt.make_int(wxSTC_CMD_PAGEDOWN) },
    { WXE_ATOM_define, "wxSTC_CMD_PAGEUPEXTEND", rt.make_int(wxSTC_CMD_PAGEUPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_PAGEUP", rt.make_int(wxSTC_CMD_PAGEUP) },
    { WXE_ATOM_define, "wxSTC_CMD_DOCUMENTENDEXTEND", rt.make_int(wxSTC_CMD_DOCUMENTENDEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_DOCUMENTEND", rt.make_int(wxSTC_CMD_DOCUMENTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_DOCUMENTSTARTEXTEND", rt.make_int(wxSTC_CMD_DOCUMENTSTARTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_DOCUMENTSTART", rt.make_int(wxSTC_CMD_DOCUMENTSTART) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEENDEXTEND", rt.make_int(wxSTC_CMD_LINEENDEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEEND", rt.make_int(wxSTC_CMD_LINEEND) },
    { WXE_ATOM_define, "wxSTC_CMD_HOMEEXTEND", rt.make_int(wxSTC_CMD_HOMEEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_HOME", rt.make_int(wxSTC_CMD_HOME) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDRIGHTEXTEND", rt.make_int(wxSTC_CMD_WORDRIGHTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDRIGHT", rt.make_int(wxSTC_CMD_WORDRIGHT) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDLEFTEXTEND", rt.make_int(wxSTC_CMD_WORDLEFTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_WORDLEFT", rt.make_int(wxSTC_CMD_WORDLEFT) },
    { WXE_ATOM_define, "wxSTC_CMD_CHARRIGHTEXTEND", rt.make_int(wxSTC_CMD_CHARRIGHTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_CHARRIGHT", rt.make_int(wxSTC_CMD_CHARRIGHT) },
    { WXE_ATOM_define, "wxSTC_CMD_CHARLEFTEXTEND", rt.make_int(wxSTC_CMD_CHARLEFTEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_CHARLEFT", rt.make_int(wxSTC_CMD_CHARLEFT) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEUPEXTEND", rt.make_int(wxSTC_CMD_LINEUPEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEUP", rt.make_int(wxSTC_CMD_LINEUP) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEDOWNEXTEND", rt.make_int(wxSTC_CMD_LINEDOWNEXTEND) },
    { WXE_ATOM_define, "wxSTC_CMD_LINEDOWN", rt.make_int(wxSTC_CMD_LINEDOWN) },
    { WXE_ATOM_define, "wxSTC_CMD_CLEAR", rt.make_int(wxSTC_CMD_CLEAR) },
    { WXE_ATOM_define, "wxSTC_CMD_PASTE", rt.make_int(wxSTC_CMD_PASTE) },
    { WXE_ATOM_define, "wxSTC_CMD_COPY", rt.make_int(wxSTC_CMD_COPY) },
    { WXE_ATOM_define, "wxSTC_CMD_CUT", rt.make_int(wxSTC_CMD_CUT) },
    { WXE_ATOM_define, "wxSTC_CMD_UNDO", rt.make_int(wxSTC_CMD_UNDO) },
    { WXE_ATOM_define, "wxSTC_CMD_SELECTALL", rt.make_int(wxSTC_CMD_SELECTALL) },
    { WXE_ATOM_define, "wxSTC_CMD_REDO", rt.make_int(wxSTC_CMD_REDO) },
    { WXE_ATOM_define, "wxSTC_INDICS_MASK", rt.make_int(wxSTC_INDICS_MASK) },
    { WXE_ATOM_define, "wxSTC_INDIC2_MASK", rt.make_int(wxSTC_INDIC2_MASK) },
    { WXE_ATOM_define, "wxSTC_INDIC1_MASK", rt.make_int(wxSTC_INDIC1_MASK) },
    { WXE_ATOM_define, "wxSTC_INDIC0_MASK", rt.make_int(wxSTC_INDIC0_MASK) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_BADSEGMENT", rt.make_int(wxSTC_EDI_BADSEGMENT) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_BADSEGMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_UNH", rt.make_int(wxSTC_EDI_UNH) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_UNH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_UNA", rt.make_int(wxSTC_EDI_UNA) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_UNA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_SEP_RELEASE", rt.make_int(wxSTC_EDI_SEP_RELEASE) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_SEP_RELEASE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_SEP_COMPOSITE", rt.make_int(wxSTC_EDI_SEP_COMPOSITE) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_SEP_COMPOSITE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_SEP_ELEMENT", rt.make_int(wxSTC_EDI_SEP_ELEMENT) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_SEP_ELEMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_SEGMENTEND", rt.make_int(wxSTC_EDI_SEGMENTEND) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_SEGMENTEND", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_SEGMENTSTART", rt.make_int(wxSTC_EDI_SEGMENTSTART) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_SEGMENTSTART", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDI_DEFAULT", rt.make_int(wxSTC_EDI_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_EDI_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_ERROR", rt.make_int(wxSTC_JSON_ERROR) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_ERROR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_LDKEYWORD", rt.make_int(wxSTC_JSON_LDKEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_LDKEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_KEYWORD", rt.make_int(wxSTC_JSON_KEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_KEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_COMPACTIRI", rt.make_int(wxSTC_JSON_COMPACTIRI) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_COMPACTIRI", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_URI", rt.make_int(wxSTC_JSON_URI) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_URI", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_OPERATOR", rt.make_int(wxSTC_JSON_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_BLOCKCOMMENT", rt.make_int(wxSTC_JSON_BLOCKCOMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_BLOCKCOMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_LINECOMMENT", rt.make_int(wxSTC_JSON_LINECOMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_LINECOMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_ESCAPESEQUENCE", rt.make_int(wxSTC_JSON_ESCAPESEQUENCE) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_ESCAPESEQUENCE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_PROPERTYNAME", rt.make_int(wxSTC_JSON_PROPERTYNAME) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_PROPERTYNAME", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_STRINGEOL", rt.make_int(wxSTC_JSON_STRINGEOL) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_STRINGEOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_STRING", rt.make_int(wxSTC_JSON_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_NUMBER", rt.make_int(wxSTC_JSON_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_JSON_DEFAULT", rt.make_int(wxSTC_JSON_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_JSON_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_GARBAGE", rt.make_int(wxSTC_HEX_GARBAGE) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_GARBAGE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_CHECKSUM_WRONG", rt.make_int(wxSTC_HEX_CHECKSUM_WRONG) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_CHECKSUM_WRONG", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_CHECKSUM", rt.make_int(wxSTC_HEX_CHECKSUM) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_CHECKSUM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_DATA_EMPTY", rt.make_int(wxSTC_HEX_DATA_EMPTY) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_DATA_EMPTY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_DATA_UNKNOWN", rt.make_int(wxSTC_HEX_DATA_UNKNOWN) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_DATA_UNKNOWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_DATA_EVEN", rt.make_int(wxSTC_HEX_DATA_EVEN) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_DATA_EVEN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_DATA_ODD", rt.make_int(wxSTC_HEX_DATA_ODD) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_DATA_ODD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_EXTENDEDADDRESS", rt.make_int(wxSTC_HEX_EXTENDEDADDRESS) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_EXTENDEDADDRESS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_ADDRESSFIELD_UNKNOWN", rt.make_int(wxSTC_HEX_ADDRESSFIELD_UNKNOWN) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_ADDRESSFIELD_UNKNOWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_STARTADDRESS", rt.make_int(wxSTC_HEX_STARTADDRESS) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_STARTADDRESS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_RECCOUNT", rt.make_int(wxSTC_HEX_RECCOUNT) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_RECCOUNT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_DATAADDRESS", rt.make_int(wxSTC_HEX_DATAADDRESS) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_DATAADDRESS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_NOADDRESS", rt.make_int(wxSTC_HEX_NOADDRESS) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_NOADDRESS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_BYTECOUNT_WRONG", rt.make_int(wxSTC_HEX_BYTECOUNT_WRONG) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_BYTECOUNT_WRONG", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_BYTECOUNT", rt.make_int(wxSTC_HEX_BYTECOUNT) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_BYTECOUNT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_RECTYPE_UNKNOWN", rt.make_int(wxSTC_HEX_RECTYPE_UNKNOWN) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_RECTYPE_UNKNOWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_RECTYPE", rt.make_int(wxSTC_HEX_RECTYPE) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_RECTYPE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_RECSTART", rt.make_int(wxSTC_HEX_RECSTART) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_RECSTART", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HEX_DEFAULT", rt.make_int(wxSTC_HEX_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_HEX_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_COMMENT", rt.make_int(wxSTC_BIBTEX_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_VALUE", rt.make_int(wxSTC_BIBTEX_VALUE) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_VALUE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_PARAMETER", rt.make_int(wxSTC_BIBTEX_PARAMETER) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_PARAMETER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_KEY", rt.make_int(wxSTC_BIBTEX_KEY) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_KEY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_UNKNOWN_ENTRY", rt.make_int(wxSTC_BIBTEX_UNKNOWN_ENTRY) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_UNKNOWN_ENTRY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_ENTRY", rt.make_int(wxSTC_BIBTEX_ENTRY) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_ENTRY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BIBTEX_DEFAULT", rt.make_int(wxSTC_BIBTEX_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_BIBTEX_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_OPERATOR", rt.make_int(wxSTC_REG_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_REG_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_PARAMETER", rt.make_int(wxSTC_REG_PARAMETER) },
#else
    { WXE_ATOM_define, "wxSTC_REG_PARAMETER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_STRING_GUID", rt.make_int(wxSTC_REG_STRING_GUID) },
#else
    { WXE_ATOM_define, "wxSTC_REG_STRING_GUID", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_KEYPATH_GUID", rt.make_int(wxSTC_REG_KEYPATH_GUID) },
#else
    { WXE_ATOM_define, "wxSTC_REG_KEYPATH_GUID", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_ESCAPED", rt.make_int(wxSTC_REG_ESCAPED) },
#else
    { WXE_ATOM_define, "wxSTC_REG_ESCAPED", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_DELETEDKEY", rt.make_int(wxSTC_REG_DELETEDKEY) },
#else
    { WXE_ATOM_define, "wxSTC_REG_DELETEDKEY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_ADDEDKEY", rt.make_int(wxSTC_REG_ADDEDKEY) },
#else
    { WXE_ATOM_define, "wxSTC_REG_ADDEDKEY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_VALUETYPE", rt.make_int(wxSTC_REG_VALUETYPE) },
#else
    { WXE_ATOM_define, "wxSTC_REG_VALUETYPE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_HEXDIGIT", rt.make_int(wxSTC_REG_HEXDIGIT) },
#else
    { WXE_ATOM_define, "wxSTC_REG_HEXDIGIT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_STRING", rt.make_int(wxSTC_REG_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_REG_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_VALUENAME", rt.make_int(wxSTC_REG_VALUENAME) },
#else
    { WXE_ATOM_define, "wxSTC_REG_VALUENAME", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_COMMENT", rt.make_int(wxSTC_REG_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_REG_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_REG_DEFAULT", rt.make_int(wxSTC_REG_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_REG_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_LABEL", rt.make_int(wxSTC_DMIS_LABEL) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_LABEL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_UNSUPPORTED_MINOR", rt.make_int(wxSTC_DMIS_UNSUPPORTED_MINOR) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_UNSUPPORTED_MINOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_UNSUPPORTED_MAJOR", rt.make_int(wxSTC_DMIS_UNSUPPORTED_MAJOR) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_UNSUPPORTED_MAJOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_MINORWORD", rt.make_int(wxSTC_DMIS_MINORWORD) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_MINORWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_MAJORWORD", rt.make_int(wxSTC_DMIS_MAJORWORD) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_MAJORWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_KEYWORD", rt.make_int(wxSTC_DMIS_KEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_KEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_NUMBER", rt.make_int(wxSTC_DMIS_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_STRING", rt.make_int(wxSTC_DMIS_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_COMMENT", rt.make_int(wxSTC_DMIS_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMIS_DEFAULT", rt.make_int(wxSTC_DMIS_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_DMIS_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_WORD3", rt.make_int(wxSTC_DMAP_WORD3) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_WORD3", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_WORD2", rt.make_int(wxSTC_DMAP_WORD2) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_WORD2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_WORD", rt.make_int(wxSTC_DMAP_WORD) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_WORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_IDENTIFIER", rt.make_int(wxSTC_DMAP_IDENTIFIER) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_IDENTIFIER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_OPERATOR", rt.make_int(wxSTC_DMAP_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_STRINGEOL", rt.make_int(wxSTC_DMAP_STRINGEOL) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_STRINGEOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_STRING2", rt.make_int(wxSTC_DMAP_STRING2) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_STRING2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_STRING1", rt.make_int(wxSTC_DMAP_STRING1) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_STRING1", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_NUMBER", rt.make_int(wxSTC_DMAP_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_COMMENT", rt.make_int(wxSTC_DMAP_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_DMAP_DEFAULT", rt.make_int(wxSTC_DMAP_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_DMAP_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_BYTECHARACTER", rt.make_int(wxSTC_RUST_BYTECHARACTER) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_BYTECHARACTER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_BYTESTRINGR", rt.make_int(wxSTC_RUST_BYTESTRINGR) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_BYTESTRINGR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_BYTESTRING", rt.make_int(wxSTC_RUST_BYTESTRING) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_BYTESTRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_LEXERROR", rt.make_int(wxSTC_RUST_LEXERROR) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_LEXERROR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_MACRO", rt.make_int(wxSTC_RUST_MACRO) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_MACRO", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_LIFETIME", rt.make_int(wxSTC_RUST_LIFETIME) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_LIFETIME", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_IDENTIFIER", rt.make_int(wxSTC_RUST_IDENTIFIER) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_IDENTIFIER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_OPERATOR", rt.make_int(wxSTC_RUST_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_CHARACTER", rt.make_int(wxSTC_RUST_CHARACTER) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_CHARACTER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_STRINGR", rt.make_int(wxSTC_RUST_STRINGR) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_STRINGR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_STRING", rt.make_int(wxSTC_RUST_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD7", rt.make_int(wxSTC_RUST_WORD7) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD7", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD6", rt.make_int(wxSTC_RUST_WORD6) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD6", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD5", rt.make_int(wxSTC_RUST_WORD5) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD5", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD4", rt.make_int(wxSTC_RUST_WORD4) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD4", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD3", rt.make_int(wxSTC_RUST_WORD3) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD3", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD2", rt.make_int(wxSTC_RUST_WORD2) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_WORD", rt.make_int(wxSTC_RUST_WORD) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_WORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_NUMBER", rt.make_int(wxSTC_RUST_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTLINEDOC", rt.make_int(wxSTC_RUST_COMMENTLINEDOC) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTLINEDOC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTBLOCKDOC", rt.make_int(wxSTC_RUST_COMMENTBLOCKDOC) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTBLOCKDOC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTLINE", rt.make_int(wxSTC_RUST_COMMENTLINE) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTLINE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTBLOCK", rt.make_int(wxSTC_RUST_COMMENTBLOCK) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_COMMENTBLOCK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_RUST_DEFAULT", rt.make_int(wxSTC_RUST_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_RUST_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_STRING_VARIABLE", rt.make_int(wxSTC_KVIRC_STRING_VARIABLE) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_STRING_VARIABLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_STRING_FUNCTION", rt.make_int(wxSTC_KVIRC_STRING_FUNCTION) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_STRING_FUNCTION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_OPERATOR", rt.make_int(wxSTC_KVIRC_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_NUMBER", rt.make_int(wxSTC_KVIRC_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_VARIABLE", rt.make_int(wxSTC_KVIRC_VARIABLE) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_VARIABLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_FUNCTION", rt.make_int(wxSTC_KVIRC_FUNCTION) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_FUNCTION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_FUNCTION_KEYWORD", rt.make_int(wxSTC_KVIRC_FUNCTION_KEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_FUNCTION_KEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_KEYWORD", rt.make_int(wxSTC_KVIRC_KEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_KEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_WORD", rt.make_int(wxSTC_KVIRC_WORD) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_WORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_STRING", rt.make_int(wxSTC_KVIRC_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_COMMENTBLOCK", rt.make_int(wxSTC_KVIRC_COMMENTBLOCK) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_COMMENTBLOCK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_COMMENT", rt.make_int(wxSTC_KVIRC_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KVIRC_DEFAULT", rt.make_int(wxSTC_KVIRC_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_KVIRC_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_PRAGMAS", rt.make_int(wxSTC_STTXT_PRAGMAS) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_PRAGMAS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_VARS", rt.make_int(wxSTC_STTXT_VARS) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_VARS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_DATETIME", rt.make_int(wxSTC_STTXT_DATETIME) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_DATETIME", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_IDENTIFIER", rt.make_int(wxSTC_STTXT_IDENTIFIER) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_IDENTIFIER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_STRINGEOL", rt.make_int(wxSTC_STTXT_STRINGEOL) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_STRINGEOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_STRING2", rt.make_int(wxSTC_STTXT_STRING2) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_STRING2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_STRING1", rt.make_int(wxSTC_STTXT_STRING1) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_STRING1", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_CHARACTER", rt.make_int(wxSTC_STTXT_CHARACTER) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_CHARACTER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_OPERATOR", rt.make_int(wxSTC_STTXT_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_PRAGMA", rt.make_int(wxSTC_STTXT_PRAGMA) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_PRAGMA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_HEXNUMBER", rt.make_int(wxSTC_STTXT_HEXNUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_HEXNUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_NUMBER", rt.make_int(wxSTC_STTXT_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_FB", rt.make_int(wxSTC_STTXT_FB) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_FB", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_FUNCTION", rt.make_int(wxSTC_STTXT_FUNCTION) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_FUNCTION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_TYPE", rt.make_int(wxSTC_STTXT_TYPE) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_TYPE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_KEYWORD", rt.make_int(wxSTC_STTXT_KEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_KEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_COMMENTLINE", rt.make_int(wxSTC_STTXT_COMMENTLINE) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_COMMENTLINE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_COMMENT", rt.make_int(wxSTC_STTXT_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STTXT_DEFAULT", rt.make_int(wxSTC_STTXT_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_STTXT_DEFAULT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING_VERBATIM_EOL", rt.make_int(wxSTC_VISUALPROLOG_STRING_VERBATIM_EOL) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING_VERBATIM_SPECIAL", rt.make_int(wxSTC_VISUALPROLOG_STRING_VERBATIM_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING_VERBATIM", rt.make_int(wxSTC_VISUALPROLOG_STRING_VERBATIM) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING_EOL_OPEN", rt.make_int(wxSTC_VISUALPROLOG_STRING_EOL_OPEN) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING_ESCAPE_ERROR", rt.make_int(wxSTC_VISUALPROLOG_STRING_ESCAPE_ERROR) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING_ESCAPE", rt.make_int(wxSTC_VISUALPROLOG_STRING_ESCAPE) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_STRING", rt.make_int(wxSTC_VISUALPROLOG_STRING) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_CHARACTER_ESCAPE_ERROR", rt.make_int(wxSTC_VISUALPROLOG_CHARACTER_ESCAPE_ERROR) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_CHARACTER_TOO_MANY", rt.make_int(wxSTC_VISUALPROLOG_CHARACTER_TOO_MANY) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_CHARACTER", rt.make_int(wxSTC_VISUALPROLOG_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_OPERATOR", rt.make_int(wxSTC_VISUALPROLOG_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_NUMBER", rt.make_int(wxSTC_VISUALPROLOG_NUMBER) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_ANONYMOUS", rt.make_int(wxSTC_VISUALPROLOG_ANONYMOUS) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_VARIABLE", rt.make_int(wxSTC_VISUALPROLOG_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_IDENTIFIER", rt.make_int(wxSTC_VISUALPROLOG_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_COMMENT_KEY_ERROR", rt.make_int(wxSTC_VISUALPROLOG_COMMENT_KEY_ERROR) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_COMMENT_KEY", rt.make_int(wxSTC_VISUALPROLOG_COMMENT_KEY) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_COMMENT_LINE", rt.make_int(wxSTC_VISUALPROLOG_COMMENT_LINE) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_COMMENT_BLOCK", rt.make_int(wxSTC_VISUALPROLOG_COMMENT_BLOCK) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_KEY_DIRECTIVE", rt.make_int(wxSTC_VISUALPROLOG_KEY_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_KEY_MINOR", rt.make_int(wxSTC_VISUALPROLOG_KEY_MINOR) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_KEY_MAJOR", rt.make_int(wxSTC_VISUALPROLOG_KEY_MAJOR) },
    { WXE_ATOM_define, "wxSTC_VISUALPROLOG_DEFAULT", rt.make_int(wxSTC_VISUALPROLOG_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_METHOD", rt.make_int(wxSTC_OSCRIPT_METHOD) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_PROPERTY", rt.make_int(wxSTC_OSCRIPT_PROPERTY) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_OBJECT", rt.make_int(wxSTC_OSCRIPT_OBJECT) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_FUNCTION", rt.make_int(wxSTC_OSCRIPT_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_TYPE", rt.make_int(wxSTC_OSCRIPT_TYPE) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_LABEL", rt.make_int(wxSTC_OSCRIPT_LABEL) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_OPERATOR", rt.make_int(wxSTC_OSCRIPT_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_KEYWORD", rt.make_int(wxSTC_OSCRIPT_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_GLOBAL", rt.make_int(wxSTC_OSCRIPT_GLOBAL) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_IDENTIFIER", rt.make_int(wxSTC_OSCRIPT_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_CONSTANT", rt.make_int(wxSTC_OSCRIPT_CONSTANT) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_DOUBLEQUOTE_STRING", rt.make_int(wxSTC_OSCRIPT_DOUBLEQUOTE_STRING) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_SINGLEQUOTE_STRING", rt.make_int(wxSTC_OSCRIPT_SINGLEQUOTE_STRING) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_NUMBER", rt.make_int(wxSTC_OSCRIPT_NUMBER) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_PREPROCESSOR", rt.make_int(wxSTC_OSCRIPT_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_DOC_COMMENT", rt.make_int(wxSTC_OSCRIPT_DOC_COMMENT) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_BLOCK_COMMENT", rt.make_int(wxSTC_OSCRIPT_BLOCK_COMMENT) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_LINE_COMMENT", rt.make_int(wxSTC_OSCRIPT_LINE_COMMENT) },
    { WXE_ATOM_define, "wxSTC_OSCRIPT_DEFAULT", rt.make_int(wxSTC_OSCRIPT_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ECL_MOVED", rt.make_int(wxSTC_ECL_MOVED) },
    { WXE_ATOM_define, "wxSTC_ECL_CHANGED", rt.make_int(wxSTC_ECL_CHANGED) },
    { WXE_ATOM_define, "wxSTC_ECL_DELETED", rt.make_int(wxSTC_ECL_DELETED) },
    { WXE_ATOM_define, "wxSTC_ECL_ADDED", rt.make_int(wxSTC_ECL_ADDED) },
    { WXE_ATOM_define, "wxSTC_ECL_COMMENTDOC", rt.make_int(wxSTC_ECL_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_ECL_WORD5", rt.make_int(wxSTC_ECL_WORD5) },
    { WXE_ATOM_define, "wxSTC_ECL_WORD4", rt.make_int(wxSTC_ECL_WORD4) },
    { WXE_ATOM_define, "wxSTC_ECL_WORD3", rt.make_int(wxSTC_ECL_WORD3) },
    { WXE_ATOM_define, "wxSTC_ECL_WORD2", rt.make_int(wxSTC_ECL_WORD2) },
    { WXE_ATOM_define, "wxSTC_ECL_COMMENTDOCKEYWORDERROR", rt.make_int(wxSTC_ECL_COMMENTDOCKEYWORDERROR) },
    { WXE_ATOM_define, "wxSTC_ECL_COMMENTDOCKEYWORD", rt.make_int(wxSTC_ECL_COMMENTDOCKEYWORD) },
    { WXE_ATOM_define, "wxSTC_ECL_WORD1", rt.make_int(wxSTC_ECL_WORD1) },
    { WXE_ATOM_define, "wxSTC_ECL_COMMENTLINEDOC", rt.make_int(wxSTC_ECL_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_ECL_REGEX", rt.make_int(wxSTC_ECL_REGEX) },
    { WXE_ATOM_define, "wxSTC_ECL_VERBATIM", rt.make_int(wxSTC_ECL_VERBATIM) },
    { WXE_ATOM_define, "wxSTC_ECL_STRINGEOL", rt.make_int(wxSTC_ECL_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_ECL_IDENTIFIER", rt.make_int(wxSTC_ECL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_ECL_UNKNOWN", rt.make_int(wxSTC_ECL_UNKNOWN) },
    { WXE_ATOM_define, "wxSTC_ECL_PREPROCESSOR", rt.make_int(wxSTC_ECL_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_ECL_UUID", rt.make_int(wxSTC_ECL_UUID) },
    { WXE_ATOM_define, "wxSTC_ECL_CHARACTER", rt.make_int(wxSTC_ECL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_ECL_OPERATOR", rt.make_int(wxSTC_ECL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ECL_WORD0", rt.make_int(wxSTC_ECL_WORD0) },
    { WXE_ATOM_define, "wxSTC_ECL_STRING", rt.make_int(wxSTC_ECL_STRING) },
    { WXE_ATOM_define, "wxSTC_ECL_NUMBER", rt.make_int(wxSTC_ECL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ECL_COMMENTLINE", rt.make_int(wxSTC_ECL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_ECL_COMMENT", rt.make_int(wxSTC_ECL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ECL_DEFAULT", rt.make_int(wxSTC_ECL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_AVS_USERDFN", rt.make_int(wxSTC_AVS_USERDFN) },
    { WXE_ATOM_define, "wxSTC_AVS_CLIPPROP", rt.make_int(wxSTC_AVS_CLIPPROP) },
    { WXE_ATOM_define, "wxSTC_AVS_FUNCTION", rt.make_int(wxSTC_AVS_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_AVS_PLUGIN", rt.make_int(wxSTC_AVS_PLUGIN) },
    { WXE_ATOM_define, "wxSTC_AVS_FILTER", rt.make_int(wxSTC_AVS_FILTER) },
    { WXE_ATOM_define, "wxSTC_AVS_KEYWORD", rt.make_int(wxSTC_AVS_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_AVS_TRIPLESTRING", rt.make_int(wxSTC_AVS_TRIPLESTRING) },
    { WXE_ATOM_define, "wxSTC_AVS_STRING", rt.make_int(wxSTC_AVS_STRING) },
    { WXE_ATOM_define, "wxSTC_AVS_IDENTIFIER", rt.make_int(wxSTC_AVS_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_AVS_OPERATOR", rt.make_int(wxSTC_AVS_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_AVS_NUMBER", rt.make_int(wxSTC_AVS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_AVS_COMMENTLINE", rt.make_int(wxSTC_AVS_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_AVS_COMMENTBLOCKN", rt.make_int(wxSTC_AVS_COMMENTBLOCKN) },
    { WXE_ATOM_define, "wxSTC_AVS_COMMENTBLOCK", rt.make_int(wxSTC_AVS_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_AVS_DEFAULT", rt.make_int(wxSTC_AVS_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_INSTANCEPROPERTY", rt.make_int(wxSTC_COFFEESCRIPT_INSTANCEPROPERTY) },
#else
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_INSTANCEPROPERTY", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_VERBOSE_REGEX_COMMENT", rt.make_int(wxSTC_COFFEESCRIPT_VERBOSE_REGEX_COMMENT) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_VERBOSE_REGEX", rt.make_int(wxSTC_COFFEESCRIPT_VERBOSE_REGEX) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENTBLOCK", rt.make_int(wxSTC_COFFEESCRIPT_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_TRIPLEVERBATIM", rt.make_int(wxSTC_COFFEESCRIPT_TRIPLEVERBATIM) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_STRINGRAW", rt.make_int(wxSTC_COFFEESCRIPT_STRINGRAW) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_GLOBALCLASS", rt.make_int(wxSTC_COFFEESCRIPT_GLOBALCLASS) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENTDOCKEYWORDERROR", rt.make_int(wxSTC_COFFEESCRIPT_COMMENTDOCKEYWORDERROR) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENTDOCKEYWORD", rt.make_int(wxSTC_COFFEESCRIPT_COMMENTDOCKEYWORD) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_WORD2", rt.make_int(wxSTC_COFFEESCRIPT_WORD2) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENTLINEDOC", rt.make_int(wxSTC_COFFEESCRIPT_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_REGEX", rt.make_int(wxSTC_COFFEESCRIPT_REGEX) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_VERBATIM", rt.make_int(wxSTC_COFFEESCRIPT_VERBATIM) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_STRINGEOL", rt.make_int(wxSTC_COFFEESCRIPT_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_IDENTIFIER", rt.make_int(wxSTC_COFFEESCRIPT_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_OPERATOR", rt.make_int(wxSTC_COFFEESCRIPT_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_PREPROCESSOR", rt.make_int(wxSTC_COFFEESCRIPT_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_UUID", rt.make_int(wxSTC_COFFEESCRIPT_UUID) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_CHARACTER", rt.make_int(wxSTC_COFFEESCRIPT_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_STRING", rt.make_int(wxSTC_COFFEESCRIPT_STRING) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_WORD", rt.make_int(wxSTC_COFFEESCRIPT_WORD) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_NUMBER", rt.make_int(wxSTC_COFFEESCRIPT_NUMBER) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENTDOC", rt.make_int(wxSTC_COFFEESCRIPT_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENTLINE", rt.make_int(wxSTC_COFFEESCRIPT_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_COMMENT", rt.make_int(wxSTC_COFFEESCRIPT_COMMENT) },
    { WXE_ATOM_define, "wxSTC_COFFEESCRIPT_DEFAULT", rt.make_int(wxSTC_COFFEESCRIPT_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MODULA_BADSTR", rt.make_int(wxSTC_MODULA_BADSTR) },
    { WXE_ATOM_define, "wxSTC_MODULA_OPERATOR", rt.make_int(wxSTC_MODULA_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MODULA_PRGKEY", rt.make_int(wxSTC_MODULA_PRGKEY) },
    { WXE_ATOM_define, "wxSTC_MODULA_PRAGMA", rt.make_int(wxSTC_MODULA_PRAGMA) },
    { WXE_ATOM_define, "wxSTC_MODULA_PROC", rt.make_int(wxSTC_MODULA_PROC) },
    { WXE_ATOM_define, "wxSTC_MODULA_CHARSPEC", rt.make_int(wxSTC_MODULA_CHARSPEC) },
    { WXE_ATOM_define, "wxSTC_MODULA_CHAR", rt.make_int(wxSTC_MODULA_CHAR) },
    { WXE_ATOM_define, "wxSTC_MODULA_STRSPEC", rt.make_int(wxSTC_MODULA_STRSPEC) },
    { WXE_ATOM_define, "wxSTC_MODULA_STRING", rt.make_int(wxSTC_MODULA_STRING) },
    { WXE_ATOM_define, "wxSTC_MODULA_FLOAT", rt.make_int(wxSTC_MODULA_FLOAT) },
    { WXE_ATOM_define, "wxSTC_MODULA_BASENUM", rt.make_int(wxSTC_MODULA_BASENUM) },
    { WXE_ATOM_define, "wxSTC_MODULA_NUMBER", rt.make_int(wxSTC_MODULA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MODULA_RESERVED", rt.make_int(wxSTC_MODULA_RESERVED) },
    { WXE_ATOM_define, "wxSTC_MODULA_KEYWORD", rt.make_int(wxSTC_MODULA_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_MODULA_DOXYKEY", rt.make_int(wxSTC_MODULA_DOXYKEY) },
    { WXE_ATOM_define, "wxSTC_MODULA_DOXYCOMM", rt.make_int(wxSTC_MODULA_DOXYCOMM) },
    { WXE_ATOM_define, "wxSTC_MODULA_COMMENT", rt.make_int(wxSTC_MODULA_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MODULA_DEFAULT", rt.make_int(wxSTC_MODULA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_A68K_COMMENT_DOXYGEN", rt.make_int(wxSTC_A68K_COMMENT_DOXYGEN) },
    { WXE_ATOM_define, "wxSTC_A68K_COMMENT_SPECIAL", rt.make_int(wxSTC_A68K_COMMENT_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_A68K_COMMENT_WORD", rt.make_int(wxSTC_A68K_COMMENT_WORD) },
    { WXE_ATOM_define, "wxSTC_A68K_MACRO_DECLARATION", rt.make_int(wxSTC_A68K_MACRO_DECLARATION) },
    { WXE_ATOM_define, "wxSTC_A68K_IDENTIFIER", rt.make_int(wxSTC_A68K_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_A68K_STRING2", rt.make_int(wxSTC_A68K_STRING2) },
    { WXE_ATOM_define, "wxSTC_A68K_LABEL", rt.make_int(wxSTC_A68K_LABEL) },
    { WXE_ATOM_define, "wxSTC_A68K_MACRO_ARG", rt.make_int(wxSTC_A68K_MACRO_ARG) },
    { WXE_ATOM_define, "wxSTC_A68K_DIRECTIVE", rt.make_int(wxSTC_A68K_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_A68K_REGISTER", rt.make_int(wxSTC_A68K_REGISTER) },
    { WXE_ATOM_define, "wxSTC_A68K_EXTINSTRUCTION", rt.make_int(wxSTC_A68K_EXTINSTRUCTION) },
    { WXE_ATOM_define, "wxSTC_A68K_CPUINSTRUCTION", rt.make_int(wxSTC_A68K_CPUINSTRUCTION) },
    { WXE_ATOM_define, "wxSTC_A68K_OPERATOR", rt.make_int(wxSTC_A68K_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_A68K_STRING1", rt.make_int(wxSTC_A68K_STRING1) },
    { WXE_ATOM_define, "wxSTC_A68K_NUMBER_HEX", rt.make_int(wxSTC_A68K_NUMBER_HEX) },
    { WXE_ATOM_define, "wxSTC_A68K_NUMBER_BIN", rt.make_int(wxSTC_A68K_NUMBER_BIN) },
    { WXE_ATOM_define, "wxSTC_A68K_NUMBER_DEC", rt.make_int(wxSTC_A68K_NUMBER_DEC) },
    { WXE_ATOM_define, "wxSTC_A68K_COMMENT", rt.make_int(wxSTC_A68K_COMMENT) },
    { WXE_ATOM_define, "wxSTC_A68K_DEFAULT", rt.make_int(wxSTC_A68K_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_POSTPROC", rt.make_int(wxSTC_TXT2TAGS_POSTPROC) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_PREPROC", rt.make_int(wxSTC_TXT2TAGS_PREPROC) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_OPTION", rt.make_int(wxSTC_TXT2TAGS_OPTION) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_COMMENT", rt.make_int(wxSTC_TXT2TAGS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_CODEBK", rt.make_int(wxSTC_TXT2TAGS_CODEBK) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_CODE2", rt.make_int(wxSTC_TXT2TAGS_CODE2) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_CODE", rt.make_int(wxSTC_TXT2TAGS_CODE) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_LINK", rt.make_int(wxSTC_TXT2TAGS_LINK) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HRULE", rt.make_int(wxSTC_TXT2TAGS_HRULE) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_STRIKEOUT", rt.make_int(wxSTC_TXT2TAGS_STRIKEOUT) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_BLOCKQUOTE", rt.make_int(wxSTC_TXT2TAGS_BLOCKQUOTE) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_OLIST_ITEM", rt.make_int(wxSTC_TXT2TAGS_OLIST_ITEM) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_ULIST_ITEM", rt.make_int(wxSTC_TXT2TAGS_ULIST_ITEM) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_PRECHAR", rt.make_int(wxSTC_TXT2TAGS_PRECHAR) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HEADER6", rt.make_int(wxSTC_TXT2TAGS_HEADER6) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HEADER5", rt.make_int(wxSTC_TXT2TAGS_HEADER5) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HEADER4", rt.make_int(wxSTC_TXT2TAGS_HEADER4) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HEADER3", rt.make_int(wxSTC_TXT2TAGS_HEADER3) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HEADER2", rt.make_int(wxSTC_TXT2TAGS_HEADER2) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_HEADER1", rt.make_int(wxSTC_TXT2TAGS_HEADER1) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_EM2", rt.make_int(wxSTC_TXT2TAGS_EM2) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_EM1", rt.make_int(wxSTC_TXT2TAGS_EM1) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_STRONG2", rt.make_int(wxSTC_TXT2TAGS_STRONG2) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_STRONG1", rt.make_int(wxSTC_TXT2TAGS_STRONG1) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_LINE_BEGIN", rt.make_int(wxSTC_TXT2TAGS_LINE_BEGIN) },
    { WXE_ATOM_define, "wxSTC_TXT2TAGS_DEFAULT", rt.make_int(wxSTC_TXT2TAGS_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_CODEBK", rt.make_int(wxSTC_MARKDOWN_CODEBK) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_CODE2", rt.make_int(wxSTC_MARKDOWN_CODE2) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_CODE", rt.make_int(wxSTC_MARKDOWN_CODE) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_LINK", rt.make_int(wxSTC_MARKDOWN_LINK) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HRULE", rt.make_int(wxSTC_MARKDOWN_HRULE) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_STRIKEOUT", rt.make_int(wxSTC_MARKDOWN_STRIKEOUT) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_BLOCKQUOTE", rt.make_int(wxSTC_MARKDOWN_BLOCKQUOTE) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_OLIST_ITEM", rt.make_int(wxSTC_MARKDOWN_OLIST_ITEM) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_ULIST_ITEM", rt.make_int(wxSTC_MARKDOWN_ULIST_ITEM) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_PRECHAR", rt.make_int(wxSTC_MARKDOWN_PRECHAR) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HEADER6", rt.make_int(wxSTC_MARKDOWN_HEADER6) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HEADER5", rt.make_int(wxSTC_MARKDOWN_HEADER5) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HEADER4", rt.make_int(wxSTC_MARKDOWN_HEADER4) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HEADER3", rt.make_int(wxSTC_MARKDOWN_HEADER3) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HEADER2", rt.make_int(wxSTC_MARKDOWN_HEADER2) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_HEADER1", rt.make_int(wxSTC_MARKDOWN_HEADER1) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_EM2", rt.make_int(wxSTC_MARKDOWN_EM2) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_EM1", rt.make_int(wxSTC_MARKDOWN_EM1) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_STRONG2", rt.make_int(wxSTC_MARKDOWN_STRONG2) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_STRONG1", rt.make_int(wxSTC_MARKDOWN_STRONG1) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_LINE_BEGIN", rt.make_int(wxSTC_MARKDOWN_LINE_BEGIN) },
    { WXE_ATOM_define, "wxSTC_MARKDOWN_DEFAULT", rt.make_int(wxSTC_MARKDOWN_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_SML_COMMENT3", rt.make_int(wxSTC_SML_COMMENT3) },
    { WXE_ATOM_define, "wxSTC_SML_COMMENT2", rt.make_int(wxSTC_SML_COMMENT2) },
    { WXE_ATOM_define, "wxSTC_SML_COMMENT1", rt.make_int(wxSTC_SML_COMMENT1) },
    { WXE_ATOM_define, "wxSTC_SML_COMMENT", rt.make_int(wxSTC_SML_COMMENT) },
    { WXE_ATOM_define, "wxSTC_SML_STRING", rt.make_int(wxSTC_SML_STRING) },
    { WXE_ATOM_define, "wxSTC_SML_CHAR", rt.make_int(wxSTC_SML_CHAR) },
    { WXE_ATOM_define, "wxSTC_SML_NUMBER", rt.make_int(wxSTC_SML_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SML_OPERATOR", rt.make_int(wxSTC_SML_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_SML_LINENUM", rt.make_int(wxSTC_SML_LINENUM) },
    { WXE_ATOM_define, "wxSTC_SML_KEYWORD3", rt.make_int(wxSTC_SML_KEYWORD3) },
    { WXE_ATOM_define, "wxSTC_SML_KEYWORD2", rt.make_int(wxSTC_SML_KEYWORD2) },
    { WXE_ATOM_define, "wxSTC_SML_KEYWORD", rt.make_int(wxSTC_SML_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_SML_TAGNAME", rt.make_int(wxSTC_SML_TAGNAME) },
    { WXE_ATOM_define, "wxSTC_SML_IDENTIFIER", rt.make_int(wxSTC_SML_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SML_DEFAULT", rt.make_int(wxSTC_SML_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_FUNCTION", rt.make_int(wxSTC_POWERPRO_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_ALTQUOTE", rt.make_int(wxSTC_POWERPRO_ALTQUOTE) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_VERBATIM", rt.make_int(wxSTC_POWERPRO_VERBATIM) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_STRINGEOL", rt.make_int(wxSTC_POWERPRO_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_IDENTIFIER", rt.make_int(wxSTC_POWERPRO_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_OPERATOR", rt.make_int(wxSTC_POWERPRO_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_LINECONTINUE", rt.make_int(wxSTC_POWERPRO_LINECONTINUE) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_SINGLEQUOTEDSTRING", rt.make_int(wxSTC_POWERPRO_SINGLEQUOTEDSTRING) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_DOUBLEQUOTEDSTRING", rt.make_int(wxSTC_POWERPRO_DOUBLEQUOTEDSTRING) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_WORD4", rt.make_int(wxSTC_POWERPRO_WORD4) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_WORD3", rt.make_int(wxSTC_POWERPRO_WORD3) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_WORD2", rt.make_int(wxSTC_POWERPRO_WORD2) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_WORD", rt.make_int(wxSTC_POWERPRO_WORD) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_NUMBER", rt.make_int(wxSTC_POWERPRO_NUMBER) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_COMMENTLINE", rt.make_int(wxSTC_POWERPRO_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_COMMENTBLOCK", rt.make_int(wxSTC_POWERPRO_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_POWERPRO_DEFAULT", rt.make_int(wxSTC_POWERPRO_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_SORCUS_CONSTANT", rt.make_int(wxSTC_SORCUS_CONSTANT) },
    { WXE_ATOM_define, "wxSTC_SORCUS_NUMBER", rt.make_int(wxSTC_SORCUS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SORCUS_OPERATOR", rt.make_int(wxSTC_SORCUS_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_SORCUS_IDENTIFIER", rt.make_int(wxSTC_SORCUS_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SORCUS_STRINGEOL", rt.make_int(wxSTC_SORCUS_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_SORCUS_STRING", rt.make_int(wxSTC_SORCUS_STRING) },
    { WXE_ATOM_define, "wxSTC_SORCUS_COMMENTLINE", rt.make_int(wxSTC_SORCUS_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_SORCUS_PARAMETER", rt.make_int(wxSTC_SORCUS_PARAMETER) },
    { WXE_ATOM_define, "wxSTC_SORCUS_COMMAND", rt.make_int(wxSTC_SORCUS_COMMAND) },
    { WXE_ATOM_define, "wxSTC_SORCUS_DEFAULT", rt.make_int(wxSTC_SORCUS_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_PAS_ASM", rt.make_int(wxSTC_PAS_ASM) },
    { WXE_ATOM_define, "wxSTC_PAS_OPERATOR", rt.make_int(wxSTC_PAS_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_PAS_CHARACTER", rt.make_int(wxSTC_PAS_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_PAS_STRINGEOL", rt.make_int(wxSTC_PAS_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_PAS_STRING", rt.make_int(wxSTC_PAS_STRING) },
    { WXE_ATOM_define, "wxSTC_PAS_WORD", rt.make_int(wxSTC_PAS_WORD) },
    { WXE_ATOM_define, "wxSTC_PAS_HEXNUMBER", rt.make_int(wxSTC_PAS_HEXNUMBER) },
    { WXE_ATOM_define, "wxSTC_PAS_NUMBER", rt.make_int(wxSTC_PAS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_PAS_PREPROCESSOR2", rt.make_int(wxSTC_PAS_PREPROCESSOR2) },
    { WXE_ATOM_define, "wxSTC_PAS_PREPROCESSOR", rt.make_int(wxSTC_PAS_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_PAS_COMMENTLINE", rt.make_int(wxSTC_PAS_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_PAS_COMMENT2", rt.make_int(wxSTC_PAS_COMMENT2) },
    { WXE_ATOM_define, "wxSTC_PAS_COMMENT", rt.make_int(wxSTC_PAS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_PAS_IDENTIFIER", rt.make_int(wxSTC_PAS_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_PAS_DEFAULT", rt.make_int(wxSTC_PAS_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_ERROR", rt.make_int(wxSTC_PO_ERROR) },
#else
    { WXE_ATOM_define, "wxSTC_PO_ERROR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGCTXT_TEXT_EOL", rt.make_int(wxSTC_PO_MSGCTXT_TEXT_EOL) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGCTXT_TEXT_EOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGSTR_TEXT_EOL", rt.make_int(wxSTC_PO_MSGSTR_TEXT_EOL) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGSTR_TEXT_EOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGID_TEXT_EOL", rt.make_int(wxSTC_PO_MSGID_TEXT_EOL) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGID_TEXT_EOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_FLAGS", rt.make_int(wxSTC_PO_FLAGS) },
#else
    { WXE_ATOM_define, "wxSTC_PO_FLAGS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_REFERENCE", rt.make_int(wxSTC_PO_REFERENCE) },
#else
    { WXE_ATOM_define, "wxSTC_PO_REFERENCE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_PROGRAMMER_COMMENT", rt.make_int(wxSTC_PO_PROGRAMMER_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_PROGRAMMER_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_FUZZY", rt.make_int(wxSTC_PO_FUZZY) },
#else
    { WXE_ATOM_define, "wxSTC_PO_FUZZY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGCTXT_TEXT", rt.make_int(wxSTC_PO_MSGCTXT_TEXT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGCTXT_TEXT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGCTXT", rt.make_int(wxSTC_PO_MSGCTXT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGCTXT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGSTR_TEXT", rt.make_int(wxSTC_PO_MSGSTR_TEXT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGSTR_TEXT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGSTR", rt.make_int(wxSTC_PO_MSGSTR) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGSTR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGID_TEXT", rt.make_int(wxSTC_PO_MSGID_TEXT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGID_TEXT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_MSGID", rt.make_int(wxSTC_PO_MSGID) },
#else
    { WXE_ATOM_define, "wxSTC_PO_MSGID", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_COMMENT", rt.make_int(wxSTC_PO_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PO_DEFAULT", rt.make_int(wxSTC_PO_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_PO_DEFAULT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MYSQL_PLACEHOLDER", rt.make_int(wxSTC_MYSQL_PLACEHOLDER) },
#else
    { WXE_ATOM_define, "wxSTC_MYSQL_PLACEHOLDER", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_MYSQL_HIDDENCOMMAND", rt.make_int(wxSTC_MYSQL_HIDDENCOMMAND) },
    { WXE_ATOM_define, "wxSTC_MYSQL_USER3", rt.make_int(wxSTC_MYSQL_USER3) },
    { WXE_ATOM_define, "wxSTC_MYSQL_USER2", rt.make_int(wxSTC_MYSQL_USER2) },
    { WXE_ATOM_define, "wxSTC_MYSQL_USER1", rt.make_int(wxSTC_MYSQL_USER1) },
    { WXE_ATOM_define, "wxSTC_MYSQL_QUOTEDIDENTIFIER", rt.make_int(wxSTC_MYSQL_QUOTEDIDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_MYSQL_IDENTIFIER", rt.make_int(wxSTC_MYSQL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_MYSQL_FUNCTION", rt.make_int(wxSTC_MYSQL_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_MYSQL_OPERATOR", rt.make_int(wxSTC_MYSQL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MYSQL_DQSTRING", rt.make_int(wxSTC_MYSQL_DQSTRING) },
    { WXE_ATOM_define, "wxSTC_MYSQL_SQSTRING", rt.make_int(wxSTC_MYSQL_SQSTRING) },
    { WXE_ATOM_define, "wxSTC_MYSQL_STRING", rt.make_int(wxSTC_MYSQL_STRING) },
    { WXE_ATOM_define, "wxSTC_MYSQL_PROCEDUREKEYWORD", rt.make_int(wxSTC_MYSQL_PROCEDUREKEYWORD) },
    { WXE_ATOM_define, "wxSTC_MYSQL_DATABASEOBJECT", rt.make_int(wxSTC_MYSQL_DATABASEOBJECT) },
    { WXE_ATOM_define, "wxSTC_MYSQL_KEYWORD", rt.make_int(wxSTC_MYSQL_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_MYSQL_MAJORKEYWORD", rt.make_int(wxSTC_MYSQL_MAJORKEYWORD) },
    { WXE_ATOM_define, "wxSTC_MYSQL_NUMBER", rt.make_int(wxSTC_MYSQL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MYSQL_KNOWNSYSTEMVARIABLE", rt.make_int(wxSTC_MYSQL_KNOWNSYSTEMVARIABLE) },
    { WXE_ATOM_define, "wxSTC_MYSQL_SYSTEMVARIABLE", rt.make_int(wxSTC_MYSQL_SYSTEMVARIABLE) },
    { WXE_ATOM_define, "wxSTC_MYSQL_VARIABLE", rt.make_int(wxSTC_MYSQL_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_MYSQL_COMMENTLINE", rt.make_int(wxSTC_MYSQL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_MYSQL_COMMENT", rt.make_int(wxSTC_MYSQL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MYSQL_DEFAULT", rt.make_int(wxSTC_MYSQL_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_POWERSHELL_COMMENTDOCKEYWORD", rt.make_int(wxSTC_POWERSHELL_COMMENTDOCKEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_POWERSHELL_COMMENTDOCKEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_POWERSHELL_HERE_CHARACTER", rt.make_int(wxSTC_POWERSHELL_HERE_CHARACTER) },
#else
    { WXE_ATOM_define, "wxSTC_POWERSHELL_HERE_CHARACTER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_POWERSHELL_HERE_STRING", rt.make_int(wxSTC_POWERSHELL_HERE_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_POWERSHELL_HERE_STRING", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_POWERSHELL_COMMENTSTREAM", rt.make_int(wxSTC_POWERSHELL_COMMENTSTREAM) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_USER1", rt.make_int(wxSTC_POWERSHELL_USER1) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_FUNCTION", rt.make_int(wxSTC_POWERSHELL_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_ALIAS", rt.make_int(wxSTC_POWERSHELL_ALIAS) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_CMDLET", rt.make_int(wxSTC_POWERSHELL_CMDLET) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_KEYWORD", rt.make_int(wxSTC_POWERSHELL_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_IDENTIFIER", rt.make_int(wxSTC_POWERSHELL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_OPERATOR", rt.make_int(wxSTC_POWERSHELL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_VARIABLE", rt.make_int(wxSTC_POWERSHELL_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_NUMBER", rt.make_int(wxSTC_POWERSHELL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_CHARACTER", rt.make_int(wxSTC_POWERSHELL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_STRING", rt.make_int(wxSTC_POWERSHELL_STRING) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_COMMENT", rt.make_int(wxSTC_POWERSHELL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_POWERSHELL_DEFAULT", rt.make_int(wxSTC_POWERSHELL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MAGIK_SYMBOL", rt.make_int(wxSTC_MAGIK_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_MAGIK_PRAGMA", rt.make_int(wxSTC_MAGIK_PRAGMA) },
    { WXE_ATOM_define, "wxSTC_MAGIK_KEYWORD", rt.make_int(wxSTC_MAGIK_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_MAGIK_UNKNOWN_KEYWORD", rt.make_int(wxSTC_MAGIK_UNKNOWN_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_MAGIK_SQBRACKET_BLOCK", rt.make_int(wxSTC_MAGIK_SQBRACKET_BLOCK) },
    { WXE_ATOM_define, "wxSTC_MAGIK_BRACE_BLOCK", rt.make_int(wxSTC_MAGIK_BRACE_BLOCK) },
    { WXE_ATOM_define, "wxSTC_MAGIK_BRACKET_BLOCK", rt.make_int(wxSTC_MAGIK_BRACKET_BLOCK) },
    { WXE_ATOM_define, "wxSTC_MAGIK_CONTAINER", rt.make_int(wxSTC_MAGIK_CONTAINER) },
    { WXE_ATOM_define, "wxSTC_MAGIK_FLOW", rt.make_int(wxSTC_MAGIK_FLOW) },
    { WXE_ATOM_define, "wxSTC_MAGIK_OPERATOR", rt.make_int(wxSTC_MAGIK_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MAGIK_IDENTIFIER", rt.make_int(wxSTC_MAGIK_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_MAGIK_NUMBER", rt.make_int(wxSTC_MAGIK_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MAGIK_CHARACTER", rt.make_int(wxSTC_MAGIK_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_MAGIK_STRING", rt.make_int(wxSTC_MAGIK_STRING) },
    { WXE_ATOM_define, "wxSTC_MAGIK_HYPER_COMMENT", rt.make_int(wxSTC_MAGIK_HYPER_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MAGIK_COMMENT", rt.make_int(wxSTC_MAGIK_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MAGIK_DEFAULT", rt.make_int(wxSTC_MAGIK_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_R_INFIXEOL", rt.make_int(wxSTC_R_INFIXEOL) },
    { WXE_ATOM_define, "wxSTC_R_INFIX", rt.make_int(wxSTC_R_INFIX) },
    { WXE_ATOM_define, "wxSTC_R_IDENTIFIER", rt.make_int(wxSTC_R_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_R_OPERATOR", rt.make_int(wxSTC_R_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_R_STRING2", rt.make_int(wxSTC_R_STRING2) },
    { WXE_ATOM_define, "wxSTC_R_STRING", rt.make_int(wxSTC_R_STRING) },
    { WXE_ATOM_define, "wxSTC_R_NUMBER", rt.make_int(wxSTC_R_NUMBER) },
    { WXE_ATOM_define, "wxSTC_R_OTHERKWORD", rt.make_int(wxSTC_R_OTHERKWORD) },
    { WXE_ATOM_define, "wxSTC_R_BASEKWORD", rt.make_int(wxSTC_R_BASEKWORD) },
    { WXE_ATOM_define, "wxSTC_R_KWORD", rt.make_int(wxSTC_R_KWORD) },
    { WXE_ATOM_define, "wxSTC_R_COMMENT", rt.make_int(wxSTC_R_COMMENT) },
    { WXE_ATOM_define, "wxSTC_R_DEFAULT", rt.make_int(wxSTC_R_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ASY_WORD2", rt.make_int(wxSTC_ASY_WORD2) },
    { WXE_ATOM_define, "wxSTC_ASY_COMMENTLINEDOC", rt.make_int(wxSTC_ASY_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_ASY_STRINGEOL", rt.make_int(wxSTC_ASY_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_ASY_IDENTIFIER", rt.make_int(wxSTC_ASY_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_ASY_OPERATOR", rt.make_int(wxSTC_ASY_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ASY_CHARACTER", rt.make_int(wxSTC_ASY_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_ASY_STRING", rt.make_int(wxSTC_ASY_STRING) },
    { WXE_ATOM_define, "wxSTC_ASY_WORD", rt.make_int(wxSTC_ASY_WORD) },
    { WXE_ATOM_define, "wxSTC_ASY_NUMBER", rt.make_int(wxSTC_ASY_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ASY_COMMENTLINE", rt.make_int(wxSTC_ASY_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_ASY_COMMENT", rt.make_int(wxSTC_ASY_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ASY_DEFAULT", rt.make_int(wxSTC_ASY_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_FUNCTION", rt.make_int(wxSTC_ABAQUS_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_ARGUMENT", rt.make_int(wxSTC_ABAQUS_ARGUMENT) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_STARCOMMAND", rt.make_int(wxSTC_ABAQUS_STARCOMMAND) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_SLASHCOMMAND", rt.make_int(wxSTC_ABAQUS_SLASHCOMMAND) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_COMMAND", rt.make_int(wxSTC_ABAQUS_COMMAND) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_PROCESSOR", rt.make_int(wxSTC_ABAQUS_PROCESSOR) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_WORD", rt.make_int(wxSTC_ABAQUS_WORD) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_OPERATOR", rt.make_int(wxSTC_ABAQUS_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_STRING", rt.make_int(wxSTC_ABAQUS_STRING) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_NUMBER", rt.make_int(wxSTC_ABAQUS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_COMMENTBLOCK", rt.make_int(wxSTC_ABAQUS_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_COMMENT", rt.make_int(wxSTC_ABAQUS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ABAQUS_DEFAULT", rt.make_int(wxSTC_ABAQUS_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_LINECOMMENT", rt.make_int(wxSTC_ABL_LINECOMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_LINECOMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_TASKMARKER", rt.make_int(wxSTC_ABL_TASKMARKER) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_TASKMARKER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_COMMENT", rt.make_int(wxSTC_ABL_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_END", rt.make_int(wxSTC_ABL_END) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_END", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_BLOCK", rt.make_int(wxSTC_ABL_BLOCK) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_BLOCK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_IDENTIFIER", rt.make_int(wxSTC_ABL_IDENTIFIER) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_IDENTIFIER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_OPERATOR", rt.make_int(wxSTC_ABL_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_PREPROCESSOR", rt.make_int(wxSTC_ABL_PREPROCESSOR) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_PREPROCESSOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_CHARACTER", rt.make_int(wxSTC_ABL_CHARACTER) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_CHARACTER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_STRING", rt.make_int(wxSTC_ABL_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_WORD", rt.make_int(wxSTC_ABL_WORD) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_WORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_NUMBER", rt.make_int(wxSTC_ABL_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ABL_DEFAULT", rt.make_int(wxSTC_ABL_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_ABL_DEFAULT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_PLM_KEYWORD", rt.make_int(wxSTC_PLM_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_PLM_CONTROL", rt.make_int(wxSTC_PLM_CONTROL) },
    { WXE_ATOM_define, "wxSTC_PLM_OPERATOR", rt.make_int(wxSTC_PLM_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_PLM_IDENTIFIER", rt.make_int(wxSTC_PLM_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_PLM_NUMBER", rt.make_int(wxSTC_PLM_NUMBER) },
    { WXE_ATOM_define, "wxSTC_PLM_STRING", rt.make_int(wxSTC_PLM_STRING) },
    { WXE_ATOM_define, "wxSTC_PLM_COMMENT", rt.make_int(wxSTC_PLM_COMMENT) },
    { WXE_ATOM_define, "wxSTC_PLM_DEFAULT", rt.make_int(wxSTC_PLM_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_GAP_STRINGEOL", rt.make_int(wxSTC_GAP_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_GAP_NUMBER", rt.make_int(wxSTC_GAP_NUMBER) },
    { WXE_ATOM_define, "wxSTC_GAP_COMMENT", rt.make_int(wxSTC_GAP_COMMENT) },
    { WXE_ATOM_define, "wxSTC_GAP_OPERATOR", rt.make_int(wxSTC_GAP_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_GAP_CHAR", rt.make_int(wxSTC_GAP_CHAR) },
    { WXE_ATOM_define, "wxSTC_GAP_STRING", rt.make_int(wxSTC_GAP_STRING) },
    { WXE_ATOM_define, "wxSTC_GAP_KEYWORD4", rt.make_int(wxSTC_GAP_KEYWORD4) },
    { WXE_ATOM_define, "wxSTC_GAP_KEYWORD3", rt.make_int(wxSTC_GAP_KEYWORD3) },
    { WXE_ATOM_define, "wxSTC_GAP_KEYWORD2", rt.make_int(wxSTC_GAP_KEYWORD2) },
    { WXE_ATOM_define, "wxSTC_GAP_KEYWORD", rt.make_int(wxSTC_GAP_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_GAP_IDENTIFIER", rt.make_int(wxSTC_GAP_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_GAP_DEFAULT", rt.make_int(wxSTC_GAP_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CMAKE_NUMBER", rt.make_int(wxSTC_CMAKE_NUMBER) },
    { WXE_ATOM_define, "wxSTC_CMAKE_STRINGVAR", rt.make_int(wxSTC_CMAKE_STRINGVAR) },
    { WXE_ATOM_define, "wxSTC_CMAKE_MACRODEF", rt.make_int(wxSTC_CMAKE_MACRODEF) },
    { WXE_ATOM_define, "wxSTC_CMAKE_IFDEFINEDEF", rt.make_int(wxSTC_CMAKE_IFDEFINEDEF) },
    { WXE_ATOM_define, "wxSTC_CMAKE_FOREACHDEF", rt.make_int(wxSTC_CMAKE_FOREACHDEF) },
    { WXE_ATOM_define, "wxSTC_CMAKE_WHILEDEF", rt.make_int(wxSTC_CMAKE_WHILEDEF) },
    { WXE_ATOM_define, "wxSTC_CMAKE_USERDEFINED", rt.make_int(wxSTC_CMAKE_USERDEFINED) },
    { WXE_ATOM_define, "wxSTC_CMAKE_VARIABLE", rt.make_int(wxSTC_CMAKE_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_CMAKE_PARAMETERS", rt.make_int(wxSTC_CMAKE_PARAMETERS) },
    { WXE_ATOM_define, "wxSTC_CMAKE_COMMANDS", rt.make_int(wxSTC_CMAKE_COMMANDS) },
    { WXE_ATOM_define, "wxSTC_CMAKE_STRINGRQ", rt.make_int(wxSTC_CMAKE_STRINGRQ) },
    { WXE_ATOM_define, "wxSTC_CMAKE_STRINGLQ", rt.make_int(wxSTC_CMAKE_STRINGLQ) },
    { WXE_ATOM_define, "wxSTC_CMAKE_STRINGDQ", rt.make_int(wxSTC_CMAKE_STRINGDQ) },
    { WXE_ATOM_define, "wxSTC_CMAKE_COMMENT", rt.make_int(wxSTC_CMAKE_COMMENT) },
    { WXE_ATOM_define, "wxSTC_CMAKE_DEFAULT", rt.make_int(wxSTC_CMAKE_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_SPICE_COMMENTLINE", rt.make_int(wxSTC_SPICE_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_SPICE_VALUE", rt.make_int(wxSTC_SPICE_VALUE) },
    { WXE_ATOM_define, "wxSTC_SPICE_DELIMITER", rt.make_int(wxSTC_SPICE_DELIMITER) },
    { WXE_ATOM_define, "wxSTC_SPICE_NUMBER", rt.make_int(wxSTC_SPICE_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SPICE_KEYWORD3", rt.make_int(wxSTC_SPICE_KEYWORD3) },
    { WXE_ATOM_define, "wxSTC_SPICE_KEYWORD2", rt.make_int(wxSTC_SPICE_KEYWORD2) },
    { WXE_ATOM_define, "wxSTC_SPICE_KEYWORD", rt.make_int(wxSTC_SPICE_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_SPICE_IDENTIFIER", rt.make_int(wxSTC_SPICE_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SPICE_DEFAULT", rt.make_int(wxSTC_SPICE_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_OPAL_DEFAULT", rt.make_int(wxSTC_OPAL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_OPAL_BOOL_CONST", rt.make_int(wxSTC_OPAL_BOOL_CONST) },
    { WXE_ATOM_define, "wxSTC_OPAL_PAR", rt.make_int(wxSTC_OPAL_PAR) },
    { WXE_ATOM_define, "wxSTC_OPAL_STRING", rt.make_int(wxSTC_OPAL_STRING) },
    { WXE_ATOM_define, "wxSTC_OPAL_SORT", rt.make_int(wxSTC_OPAL_SORT) },
    { WXE_ATOM_define, "wxSTC_OPAL_KEYWORD", rt.make_int(wxSTC_OPAL_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_OPAL_INTEGER", rt.make_int(wxSTC_OPAL_INTEGER) },
    { WXE_ATOM_define, "wxSTC_OPAL_COMMENT_LINE", rt.make_int(wxSTC_OPAL_COMMENT_LINE) },
    { WXE_ATOM_define, "wxSTC_OPAL_COMMENT_BLOCK", rt.make_int(wxSTC_OPAL_COMMENT_BLOCK) },
    { WXE_ATOM_define, "wxSTC_OPAL_SPACE", rt.make_int(wxSTC_OPAL_SPACE) },
    { WXE_ATOM_define, "wxSTC_INNO_IDENTIFIER", rt.make_int(wxSTC_INNO_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_INNO_STRING_SINGLE", rt.make_int(wxSTC_INNO_STRING_SINGLE) },
    { WXE_ATOM_define, "wxSTC_INNO_STRING_DOUBLE", rt.make_int(wxSTC_INNO_STRING_DOUBLE) },
    { WXE_ATOM_define, "wxSTC_INNO_KEYWORD_USER", rt.make_int(wxSTC_INNO_KEYWORD_USER) },
    { WXE_ATOM_define, "wxSTC_INNO_KEYWORD_PASCAL", rt.make_int(wxSTC_INNO_KEYWORD_PASCAL) },
    { WXE_ATOM_define, "wxSTC_INNO_COMMENT_PASCAL", rt.make_int(wxSTC_INNO_COMMENT_PASCAL) },
    { WXE_ATOM_define, "wxSTC_INNO_INLINE_EXPANSION", rt.make_int(wxSTC_INNO_INLINE_EXPANSION) },
    { WXE_ATOM_define, "wxSTC_INNO_PREPROC", rt.make_int(wxSTC_INNO_PREPROC) },
    { WXE_ATOM_define, "wxSTC_INNO_SECTION", rt.make_int(wxSTC_INNO_SECTION) },
    { WXE_ATOM_define, "wxSTC_INNO_PARAMETER", rt.make_int(wxSTC_INNO_PARAMETER) },
    { WXE_ATOM_define, "wxSTC_INNO_KEYWORD", rt.make_int(wxSTC_INNO_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_INNO_COMMENT", rt.make_int(wxSTC_INNO_COMMENT) },
    { WXE_ATOM_define, "wxSTC_INNO_DEFAULT", rt.make_int(wxSTC_INNO_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CSOUND_STRINGEOL", rt.make_int(wxSTC_CSOUND_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_CSOUND_GLOBAL_VAR", rt.make_int(wxSTC_CSOUND_GLOBAL_VAR) },
    { WXE_ATOM_define, "wxSTC_CSOUND_IRATE_VAR", rt.make_int(wxSTC_CSOUND_IRATE_VAR) },
    { WXE_ATOM_define, "wxSTC_CSOUND_KRATE_VAR", rt.make_int(wxSTC_CSOUND_KRATE_VAR) },
    { WXE_ATOM_define, "wxSTC_CSOUND_ARATE_VAR", rt.make_int(wxSTC_CSOUND_ARATE_VAR) },
    { WXE_ATOM_define, "wxSTC_CSOUND_PARAM", rt.make_int(wxSTC_CSOUND_PARAM) },
    { WXE_ATOM_define, "wxSTC_CSOUND_COMMENTBLOCK", rt.make_int(wxSTC_CSOUND_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_CSOUND_USERKEYWORD", rt.make_int(wxSTC_CSOUND_USERKEYWORD) },
    { WXE_ATOM_define, "wxSTC_CSOUND_HEADERSTMT", rt.make_int(wxSTC_CSOUND_HEADERSTMT) },
    { WXE_ATOM_define, "wxSTC_CSOUND_OPCODE", rt.make_int(wxSTC_CSOUND_OPCODE) },
    { WXE_ATOM_define, "wxSTC_CSOUND_IDENTIFIER", rt.make_int(wxSTC_CSOUND_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CSOUND_INSTR", rt.make_int(wxSTC_CSOUND_INSTR) },
    { WXE_ATOM_define, "wxSTC_CSOUND_OPERATOR", rt.make_int(wxSTC_CSOUND_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_CSOUND_NUMBER", rt.make_int(wxSTC_CSOUND_NUMBER) },
    { WXE_ATOM_define, "wxSTC_CSOUND_COMMENT", rt.make_int(wxSTC_CSOUND_COMMENT) },
    { WXE_ATOM_define, "wxSTC_CSOUND_DEFAULT", rt.make_int(wxSTC_CSOUND_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_FS_STRINGEOL_C", rt.make_int(wxSTC_FS_STRINGEOL_C) },
    { WXE_ATOM_define, "wxSTC_FS_IDENTIFIER_C", rt.make_int(wxSTC_FS_IDENTIFIER_C) },
    { WXE_ATOM_define, "wxSTC_FS_OPERATOR_C", rt.make_int(wxSTC_FS_OPERATOR_C) },
    { WXE_ATOM_define, "wxSTC_FS_PREPROCESSOR_C", rt.make_int(wxSTC_FS_PREPROCESSOR_C) },
    { WXE_ATOM_define, "wxSTC_FS_STRING_C", rt.make_int(wxSTC_FS_STRING_C) },
    { WXE_ATOM_define, "wxSTC_FS_NUMBER_C", rt.make_int(wxSTC_FS_NUMBER_C) },
    { WXE_ATOM_define, "wxSTC_FS_KEYWORD2_C", rt.make_int(wxSTC_FS_KEYWORD2_C) },
    { WXE_ATOM_define, "wxSTC_FS_KEYWORD_C", rt.make_int(wxSTC_FS_KEYWORD_C) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTLINEDOC_C", rt.make_int(wxSTC_FS_COMMENTLINEDOC_C) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTDOC_C", rt.make_int(wxSTC_FS_COMMENTDOC_C) },
    { WXE_ATOM_define, "wxSTC_FS_DEFAULT_C", rt.make_int(wxSTC_FS_DEFAULT_C) },
    { WXE_ATOM_define, "wxSTC_FS_DISABLEDCODE", rt.make_int(wxSTC_FS_DISABLEDCODE) },
    { WXE_ATOM_define, "wxSTC_FS_WORDOPERATOR", rt.make_int(wxSTC_FS_WORDOPERATOR) },
    { WXE_ATOM_define, "wxSTC_FS_CONSTANT", rt.make_int(wxSTC_FS_CONSTANT) },
    { WXE_ATOM_define, "wxSTC_FS_STRINGEOL", rt.make_int(wxSTC_FS_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_FS_DATE", rt.make_int(wxSTC_FS_DATE) },
    { WXE_ATOM_define, "wxSTC_FS_IDENTIFIER", rt.make_int(wxSTC_FS_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_FS_OPERATOR", rt.make_int(wxSTC_FS_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_FS_PREPROCESSOR", rt.make_int(wxSTC_FS_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_FS_STRING", rt.make_int(wxSTC_FS_STRING) },
    { WXE_ATOM_define, "wxSTC_FS_NUMBER", rt.make_int(wxSTC_FS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_FS_KEYWORD4", rt.make_int(wxSTC_FS_KEYWORD4) },
    { WXE_ATOM_define, "wxSTC_FS_KEYWORD3", rt.make_int(wxSTC_FS_KEYWORD3) },
    { WXE_ATOM_define, "wxSTC_FS_KEYWORD2", rt.make_int(wxSTC_FS_KEYWORD2) },
    { WXE_ATOM_define, "wxSTC_FS_KEYWORD", rt.make_int(wxSTC_FS_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTDOCKEYWORDERROR", rt.make_int(wxSTC_FS_COMMENTDOCKEYWORDERROR) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTDOCKEYWORD", rt.make_int(wxSTC_FS_COMMENTDOCKEYWORD) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTLINEDOC", rt.make_int(wxSTC_FS_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTDOC", rt.make_int(wxSTC_FS_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENTLINE", rt.make_int(wxSTC_FS_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_FS_COMMENT", rt.make_int(wxSTC_FS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_FS_DEFAULT", rt.make_int(wxSTC_FS_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ST_SPEC_SEL", rt.make_int(wxSTC_ST_SPEC_SEL) },
    { WXE_ATOM_define, "wxSTC_ST_CHARACTER", rt.make_int(wxSTC_ST_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_ST_ASSIGN", rt.make_int(wxSTC_ST_ASSIGN) },
    { WXE_ATOM_define, "wxSTC_ST_KWSEND", rt.make_int(wxSTC_ST_KWSEND) },
    { WXE_ATOM_define, "wxSTC_ST_SPECIAL", rt.make_int(wxSTC_ST_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_ST_RETURN", rt.make_int(wxSTC_ST_RETURN) },
    { WXE_ATOM_define, "wxSTC_ST_GLOBAL", rt.make_int(wxSTC_ST_GLOBAL) },
    { WXE_ATOM_define, "wxSTC_ST_NIL", rt.make_int(wxSTC_ST_NIL) },
    { WXE_ATOM_define, "wxSTC_ST_SUPER", rt.make_int(wxSTC_ST_SUPER) },
    { WXE_ATOM_define, "wxSTC_ST_SELF", rt.make_int(wxSTC_ST_SELF) },
    { WXE_ATOM_define, "wxSTC_ST_BOOL", rt.make_int(wxSTC_ST_BOOL) },
    { WXE_ATOM_define, "wxSTC_ST_BINARY", rt.make_int(wxSTC_ST_BINARY) },
    { WXE_ATOM_define, "wxSTC_ST_SYMBOL", rt.make_int(wxSTC_ST_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_ST_COMMENT", rt.make_int(wxSTC_ST_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ST_NUMBER", rt.make_int(wxSTC_ST_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ST_STRING", rt.make_int(wxSTC_ST_STRING) },
    { WXE_ATOM_define, "wxSTC_ST_DEFAULT", rt.make_int(wxSTC_ST_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_SQL_QOPERATOR", rt.make_int(wxSTC_SQL_QOPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_SQL_QOPERATOR", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_SQL_QUOTEDIDENTIFIER", rt.make_int(wxSTC_SQL_QUOTEDIDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SQL_USER4", rt.make_int(wxSTC_SQL_USER4) },
    { WXE_ATOM_define, "wxSTC_SQL_USER3", rt.make_int(wxSTC_SQL_USER3) },
    { WXE_ATOM_define, "wxSTC_SQL_USER2", rt.make_int(wxSTC_SQL_USER2) },
    { WXE_ATOM_define, "wxSTC_SQL_USER1", rt.make_int(wxSTC_SQL_USER1) },
    { WXE_ATOM_define, "wxSTC_SQL_COMMENTDOCKEYWORDERROR", rt.make_int(wxSTC_SQL_COMMENTDOCKEYWORDERROR) },
    { WXE_ATOM_define, "wxSTC_SQL_COMMENTDOCKEYWORD", rt.make_int(wxSTC_SQL_COMMENTDOCKEYWORD) },
    { WXE_ATOM_define, "wxSTC_SQL_WORD2", rt.make_int(wxSTC_SQL_WORD2) },
    { WXE_ATOM_define, "wxSTC_SQL_COMMENTLINEDOC", rt.make_int(wxSTC_SQL_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_SQL_SQLPLUS_COMMENT", rt.make_int(wxSTC_SQL_SQLPLUS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_SQL_IDENTIFIER", rt.make_int(wxSTC_SQL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SQL_OPERATOR", rt.make_int(wxSTC_SQL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_SQL_SQLPLUS_PROMPT", rt.make_int(wxSTC_SQL_SQLPLUS_PROMPT) },
    { WXE_ATOM_define, "wxSTC_SQL_SQLPLUS", rt.make_int(wxSTC_SQL_SQLPLUS) },
    { WXE_ATOM_define, "wxSTC_SQL_CHARACTER", rt.make_int(wxSTC_SQL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_SQL_STRING", rt.make_int(wxSTC_SQL_STRING) },
    { WXE_ATOM_define, "wxSTC_SQL_WORD", rt.make_int(wxSTC_SQL_WORD) },
    { WXE_ATOM_define, "wxSTC_SQL_NUMBER", rt.make_int(wxSTC_SQL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SQL_COMMENTDOC", rt.make_int(wxSTC_SQL_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_SQL_COMMENTLINE", rt.make_int(wxSTC_SQL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_SQL_COMMENT", rt.make_int(wxSTC_SQL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_SQL_DEFAULT", rt.make_int(wxSTC_SQL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD8", rt.make_int(wxSTC_REBOL_WORD8) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD7", rt.make_int(wxSTC_REBOL_WORD7) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD6", rt.make_int(wxSTC_REBOL_WORD6) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD5", rt.make_int(wxSTC_REBOL_WORD5) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD4", rt.make_int(wxSTC_REBOL_WORD4) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD3", rt.make_int(wxSTC_REBOL_WORD3) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD2", rt.make_int(wxSTC_REBOL_WORD2) },
    { WXE_ATOM_define, "wxSTC_REBOL_WORD", rt.make_int(wxSTC_REBOL_WORD) },
    { WXE_ATOM_define, "wxSTC_REBOL_IDENTIFIER", rt.make_int(wxSTC_REBOL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_REBOL_TIME", rt.make_int(wxSTC_REBOL_TIME) },
    { WXE_ATOM_define, "wxSTC_REBOL_DATE", rt.make_int(wxSTC_REBOL_DATE) },
    { WXE_ATOM_define, "wxSTC_REBOL_URL", rt.make_int(wxSTC_REBOL_URL) },
    { WXE_ATOM_define, "wxSTC_REBOL_EMAIL", rt.make_int(wxSTC_REBOL_EMAIL) },
    { WXE_ATOM_define, "wxSTC_REBOL_FILE", rt.make_int(wxSTC_REBOL_FILE) },
    { WXE_ATOM_define, "wxSTC_REBOL_TAG", rt.make_int(wxSTC_REBOL_TAG) },
    { WXE_ATOM_define, "wxSTC_REBOL_ISSUE", rt.make_int(wxSTC_REBOL_ISSUE) },
    { WXE_ATOM_define, "wxSTC_REBOL_MONEY", rt.make_int(wxSTC_REBOL_MONEY) },
    { WXE_ATOM_define, "wxSTC_REBOL_BINARY", rt.make_int(wxSTC_REBOL_BINARY) },
    { WXE_ATOM_define, "wxSTC_REBOL_TUPLE", rt.make_int(wxSTC_REBOL_TUPLE) },
    { WXE_ATOM_define, "wxSTC_REBOL_PAIR", rt.make_int(wxSTC_REBOL_PAIR) },
    { WXE_ATOM_define, "wxSTC_REBOL_NUMBER", rt.make_int(wxSTC_REBOL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_REBOL_BRACEDSTRING", rt.make_int(wxSTC_REBOL_BRACEDSTRING) },
    { WXE_ATOM_define, "wxSTC_REBOL_QUOTEDSTRING", rt.make_int(wxSTC_REBOL_QUOTEDSTRING) },
    { WXE_ATOM_define, "wxSTC_REBOL_CHARACTER", rt.make_int(wxSTC_REBOL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_REBOL_OPERATOR", rt.make_int(wxSTC_REBOL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_REBOL_PREFACE", rt.make_int(wxSTC_REBOL_PREFACE) },
    { WXE_ATOM_define, "wxSTC_REBOL_COMMENTBLOCK", rt.make_int(wxSTC_REBOL_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_REBOL_COMMENTLINE", rt.make_int(wxSTC_REBOL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_REBOL_DEFAULT", rt.make_int(wxSTC_REBOL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_T3_BRACE", rt.make_int(wxSTC_T3_BRACE) },
    { WXE_ATOM_define, "wxSTC_T3_USER3", rt.make_int(wxSTC_T3_USER3) },
    { WXE_ATOM_define, "wxSTC_T3_USER2", rt.make_int(wxSTC_T3_USER2) },
    { WXE_ATOM_define, "wxSTC_T3_USER1", rt.make_int(wxSTC_T3_USER1) },
    { WXE_ATOM_define, "wxSTC_T3_HTML_STRING", rt.make_int(wxSTC_T3_HTML_STRING) },
    { WXE_ATOM_define, "wxSTC_T3_HTML_DEFAULT", rt.make_int(wxSTC_T3_HTML_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_T3_HTML_TAG", rt.make_int(wxSTC_T3_HTML_TAG) },
    { WXE_ATOM_define, "wxSTC_T3_MSG_PARAM", rt.make_int(wxSTC_T3_MSG_PARAM) },
    { WXE_ATOM_define, "wxSTC_T3_LIB_DIRECTIVE", rt.make_int(wxSTC_T3_LIB_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_T3_X_STRING", rt.make_int(wxSTC_T3_X_STRING) },
    { WXE_ATOM_define, "wxSTC_T3_D_STRING", rt.make_int(wxSTC_T3_D_STRING) },
    { WXE_ATOM_define, "wxSTC_T3_S_STRING", rt.make_int(wxSTC_T3_S_STRING) },
    { WXE_ATOM_define, "wxSTC_T3_IDENTIFIER", rt.make_int(wxSTC_T3_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_T3_NUMBER", rt.make_int(wxSTC_T3_NUMBER) },
    { WXE_ATOM_define, "wxSTC_T3_KEYWORD", rt.make_int(wxSTC_T3_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_T3_OPERATOR", rt.make_int(wxSTC_T3_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_T3_LINE_COMMENT", rt.make_int(wxSTC_T3_LINE_COMMENT) },
    { WXE_ATOM_define, "wxSTC_T3_BLOCK_COMMENT", rt.make_int(wxSTC_T3_BLOCK_COMMENT) },
    { WXE_ATOM_define, "wxSTC_T3_PREPROCESSOR", rt.make_int(wxSTC_T3_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_T3_X_DEFAULT", rt.make_int(wxSTC_T3_X_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_T3_DEFAULT", rt.make_int(wxSTC_T3_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HA_LITERATE_CODEDELIM", rt.make_int(wxSTC_HA_LITERATE_CODEDELIM) },
#else
    { WXE_ATOM_define, "wxSTC_HA_LITERATE_CODEDELIM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HA_LITERATE_COMMENT", rt.make_int(wxSTC_HA_LITERATE_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_HA_LITERATE_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HA_RESERVED_OPERATOR", rt.make_int(wxSTC_HA_RESERVED_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_HA_RESERVED_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HA_STRINGEOL", rt.make_int(wxSTC_HA_STRINGEOL) },
#else
    { WXE_ATOM_define, "wxSTC_HA_STRINGEOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HA_PREPROCESSOR", rt.make_int(wxSTC_HA_PREPROCESSOR) },
#else
    { WXE_ATOM_define, "wxSTC_HA_PREPROCESSOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_HA_PRAGMA", rt.make_int(wxSTC_HA_PRAGMA) },
#else
    { WXE_ATOM_define, "wxSTC_HA_PRAGMA", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_HA_COMMENTBLOCK3", rt.make_int(wxSTC_HA_COMMENTBLOCK3) },
    { WXE_ATOM_define, "wxSTC_HA_COMMENTBLOCK2", rt.make_int(wxSTC_HA_COMMENTBLOCK2) },
    { WXE_ATOM_define, "wxSTC_HA_COMMENTBLOCK", rt.make_int(wxSTC_HA_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_HA_COMMENTLINE", rt.make_int(wxSTC_HA_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HA_INSTANCE", rt.make_int(wxSTC_HA_INSTANCE) },
    { WXE_ATOM_define, "wxSTC_HA_OPERATOR", rt.make_int(wxSTC_HA_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_HA_IMPORT", rt.make_int(wxSTC_HA_IMPORT) },
    { WXE_ATOM_define, "wxSTC_HA_DATA", rt.make_int(wxSTC_HA_DATA) },
    { WXE_ATOM_define, "wxSTC_HA_CAPITAL", rt.make_int(wxSTC_HA_CAPITAL) },
    { WXE_ATOM_define, "wxSTC_HA_MODULE", rt.make_int(wxSTC_HA_MODULE) },
    { WXE_ATOM_define, "wxSTC_HA_CLASS", rt.make_int(wxSTC_HA_CLASS) },
    { WXE_ATOM_define, "wxSTC_HA_CHARACTER", rt.make_int(wxSTC_HA_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_HA_STRING", rt.make_int(wxSTC_HA_STRING) },
    { WXE_ATOM_define, "wxSTC_HA_NUMBER", rt.make_int(wxSTC_HA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HA_KEYWORD", rt.make_int(wxSTC_HA_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_HA_IDENTIFIER", rt.make_int(wxSTC_HA_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_HA_DEFAULT", rt.make_int(wxSTC_HA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CAML_COMMENT3", rt.make_int(wxSTC_CAML_COMMENT3) },
    { WXE_ATOM_define, "wxSTC_CAML_COMMENT2", rt.make_int(wxSTC_CAML_COMMENT2) },
    { WXE_ATOM_define, "wxSTC_CAML_COMMENT1", rt.make_int(wxSTC_CAML_COMMENT1) },
    { WXE_ATOM_define, "wxSTC_CAML_COMMENT", rt.make_int(wxSTC_CAML_COMMENT) },
    { WXE_ATOM_define, "wxSTC_CAML_STRING", rt.make_int(wxSTC_CAML_STRING) },
    { WXE_ATOM_define, "wxSTC_CAML_WHITE", rt.make_int(wxSTC_CAML_WHITE) },
    { WXE_ATOM_define, "wxSTC_CAML_CHAR", rt.make_int(wxSTC_CAML_CHAR) },
    { WXE_ATOM_define, "wxSTC_CAML_NUMBER", rt.make_int(wxSTC_CAML_NUMBER) },
    { WXE_ATOM_define, "wxSTC_CAML_OPERATOR", rt.make_int(wxSTC_CAML_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_CAML_LINENUM", rt.make_int(wxSTC_CAML_LINENUM) },
    { WXE_ATOM_define, "wxSTC_CAML_KEYWORD3", rt.make_int(wxSTC_CAML_KEYWORD3) },
    { WXE_ATOM_define, "wxSTC_CAML_KEYWORD2", rt.make_int(wxSTC_CAML_KEYWORD2) },
    { WXE_ATOM_define, "wxSTC_CAML_KEYWORD", rt.make_int(wxSTC_CAML_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_CAML_TAGNAME", rt.make_int(wxSTC_CAML_TAGNAME) },
    { WXE_ATOM_define, "wxSTC_CAML_IDENTIFIER", rt.make_int(wxSTC_CAML_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CAML_DEFAULT", rt.make_int(wxSTC_CAML_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_VHDL_BLOCK_COMMENT", rt.make_int(wxSTC_VHDL_BLOCK_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_VHDL_BLOCK_COMMENT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_VHDL_USERWORD", rt.make_int(wxSTC_VHDL_USERWORD) },
    { WXE_ATOM_define, "wxSTC_VHDL_STDTYPE", rt.make_int(wxSTC_VHDL_STDTYPE) },
    { WXE_ATOM_define, "wxSTC_VHDL_STDPACKAGE", rt.make_int(wxSTC_VHDL_STDPACKAGE) },
    { WXE_ATOM_define, "wxSTC_VHDL_STDFUNCTION", rt.make_int(wxSTC_VHDL_STDFUNCTION) },
    { WXE_ATOM_define, "wxSTC_VHDL_ATTRIBUTE", rt.make_int(wxSTC_VHDL_ATTRIBUTE) },
    { WXE_ATOM_define, "wxSTC_VHDL_STDOPERATOR", rt.make_int(wxSTC_VHDL_STDOPERATOR) },
    { WXE_ATOM_define, "wxSTC_VHDL_KEYWORD", rt.make_int(wxSTC_VHDL_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_VHDL_STRINGEOL", rt.make_int(wxSTC_VHDL_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_VHDL_IDENTIFIER", rt.make_int(wxSTC_VHDL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_VHDL_OPERATOR", rt.make_int(wxSTC_VHDL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_VHDL_STRING", rt.make_int(wxSTC_VHDL_STRING) },
    { WXE_ATOM_define, "wxSTC_VHDL_NUMBER", rt.make_int(wxSTC_VHDL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_VHDL_COMMENTLINEBANG", rt.make_int(wxSTC_VHDL_COMMENTLINEBANG) },
    { WXE_ATOM_define, "wxSTC_VHDL_COMMENT", rt.make_int(wxSTC_VHDL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_VHDL_DEFAULT", rt.make_int(wxSTC_VHDL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ASN1_OPERATOR", rt.make_int(wxSTC_ASN1_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ASN1_TYPE", rt.make_int(wxSTC_ASN1_TYPE) },
    { WXE_ATOM_define, "wxSTC_ASN1_DESCRIPTOR", rt.make_int(wxSTC_ASN1_DESCRIPTOR) },
    { WXE_ATOM_define, "wxSTC_ASN1_ATTRIBUTE", rt.make_int(wxSTC_ASN1_ATTRIBUTE) },
    { WXE_ATOM_define, "wxSTC_ASN1_KEYWORD", rt.make_int(wxSTC_ASN1_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_ASN1_SCALAR", rt.make_int(wxSTC_ASN1_SCALAR) },
    { WXE_ATOM_define, "wxSTC_ASN1_OID", rt.make_int(wxSTC_ASN1_OID) },
    { WXE_ATOM_define, "wxSTC_ASN1_STRING", rt.make_int(wxSTC_ASN1_STRING) },
    { WXE_ATOM_define, "wxSTC_ASN1_IDENTIFIER", rt.make_int(wxSTC_ASN1_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_ASN1_COMMENT", rt.make_int(wxSTC_ASN1_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ASN1_DEFAULT", rt.make_int(wxSTC_ASN1_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_SH_HERE_Q", rt.make_int(wxSTC_SH_HERE_Q) },
    { WXE_ATOM_define, "wxSTC_SH_HERE_DELIM", rt.make_int(wxSTC_SH_HERE_DELIM) },
    { WXE_ATOM_define, "wxSTC_SH_BACKTICKS", rt.make_int(wxSTC_SH_BACKTICKS) },
    { WXE_ATOM_define, "wxSTC_SH_PARAM", rt.make_int(wxSTC_SH_PARAM) },
    { WXE_ATOM_define, "wxSTC_SH_SCALAR", rt.make_int(wxSTC_SH_SCALAR) },
    { WXE_ATOM_define, "wxSTC_SH_IDENTIFIER", rt.make_int(wxSTC_SH_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SH_OPERATOR", rt.make_int(wxSTC_SH_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_SH_CHARACTER", rt.make_int(wxSTC_SH_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_SH_STRING", rt.make_int(wxSTC_SH_STRING) },
    { WXE_ATOM_define, "wxSTC_SH_WORD", rt.make_int(wxSTC_SH_WORD) },
    { WXE_ATOM_define, "wxSTC_SH_NUMBER", rt.make_int(wxSTC_SH_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SH_COMMENTLINE", rt.make_int(wxSTC_SH_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_SH_ERROR", rt.make_int(wxSTC_SH_ERROR) },
    { WXE_ATOM_define, "wxSTC_SH_DEFAULT", rt.make_int(wxSTC_SH_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_APDL_FUNCTION", rt.make_int(wxSTC_APDL_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_APDL_ARGUMENT", rt.make_int(wxSTC_APDL_ARGUMENT) },
    { WXE_ATOM_define, "wxSTC_APDL_STARCOMMAND", rt.make_int(wxSTC_APDL_STARCOMMAND) },
    { WXE_ATOM_define, "wxSTC_APDL_SLASHCOMMAND", rt.make_int(wxSTC_APDL_SLASHCOMMAND) },
    { WXE_ATOM_define, "wxSTC_APDL_COMMAND", rt.make_int(wxSTC_APDL_COMMAND) },
    { WXE_ATOM_define, "wxSTC_APDL_PROCESSOR", rt.make_int(wxSTC_APDL_PROCESSOR) },
    { WXE_ATOM_define, "wxSTC_APDL_WORD", rt.make_int(wxSTC_APDL_WORD) },
    { WXE_ATOM_define, "wxSTC_APDL_OPERATOR", rt.make_int(wxSTC_APDL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_APDL_STRING", rt.make_int(wxSTC_APDL_STRING) },
    { WXE_ATOM_define, "wxSTC_APDL_NUMBER", rt.make_int(wxSTC_APDL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_APDL_COMMENTBLOCK", rt.make_int(wxSTC_APDL_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_APDL_COMMENT", rt.make_int(wxSTC_APDL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_APDL_DEFAULT", rt.make_int(wxSTC_APDL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_AU3_UDF", rt.make_int(wxSTC_AU3_UDF) },
    { WXE_ATOM_define, "wxSTC_AU3_COMOBJ", rt.make_int(wxSTC_AU3_COMOBJ) },
    { WXE_ATOM_define, "wxSTC_AU3_EXPAND", rt.make_int(wxSTC_AU3_EXPAND) },
    { WXE_ATOM_define, "wxSTC_AU3_SPECIAL", rt.make_int(wxSTC_AU3_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_AU3_PREPROCESSOR", rt.make_int(wxSTC_AU3_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_AU3_SENT", rt.make_int(wxSTC_AU3_SENT) },
    { WXE_ATOM_define, "wxSTC_AU3_VARIABLE", rt.make_int(wxSTC_AU3_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_AU3_OPERATOR", rt.make_int(wxSTC_AU3_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_AU3_STRING", rt.make_int(wxSTC_AU3_STRING) },
    { WXE_ATOM_define, "wxSTC_AU3_MACRO", rt.make_int(wxSTC_AU3_MACRO) },
    { WXE_ATOM_define, "wxSTC_AU3_KEYWORD", rt.make_int(wxSTC_AU3_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_AU3_FUNCTION", rt.make_int(wxSTC_AU3_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_AU3_NUMBER", rt.make_int(wxSTC_AU3_NUMBER) },
    { WXE_ATOM_define, "wxSTC_AU3_COMMENTBLOCK", rt.make_int(wxSTC_AU3_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_AU3_COMMENT", rt.make_int(wxSTC_AU3_COMMENT) },
    { WXE_ATOM_define, "wxSTC_AU3_DEFAULT", rt.make_int(wxSTC_AU3_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_SN_USER", rt.make_int(wxSTC_SN_USER) },
    { WXE_ATOM_define, "wxSTC_SN_SIGNAL", rt.make_int(wxSTC_SN_SIGNAL) },
    { WXE_ATOM_define, "wxSTC_SN_REGEXTAG", rt.make_int(wxSTC_SN_REGEXTAG) },
    { WXE_ATOM_define, "wxSTC_SN_STRINGEOL", rt.make_int(wxSTC_SN_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_SN_IDENTIFIER", rt.make_int(wxSTC_SN_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SN_OPERATOR", rt.make_int(wxSTC_SN_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_SN_PREPROCESSOR", rt.make_int(wxSTC_SN_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_SN_WORD3", rt.make_int(wxSTC_SN_WORD3) },
    { WXE_ATOM_define, "wxSTC_SN_WORD2", rt.make_int(wxSTC_SN_WORD2) },
    { WXE_ATOM_define, "wxSTC_SN_STRING", rt.make_int(wxSTC_SN_STRING) },
    { WXE_ATOM_define, "wxSTC_SN_WORD", rt.make_int(wxSTC_SN_WORD) },
    { WXE_ATOM_define, "wxSTC_SN_NUMBER", rt.make_int(wxSTC_SN_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SN_COMMENTLINEBANG", rt.make_int(wxSTC_SN_COMMENTLINEBANG) },
    { WXE_ATOM_define, "wxSTC_SN_COMMENTLINE", rt.make_int(wxSTC_SN_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_SN_CODE", rt.make_int(wxSTC_SN_CODE) },
    { WXE_ATOM_define, "wxSTC_SN_DEFAULT", rt.make_int(wxSTC_SN_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_GC_OPERATOR", rt.make_int(wxSTC_GC_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_GC_STRING", rt.make_int(wxSTC_GC_STRING) },
    { WXE_ATOM_define, "wxSTC_GC_COMMAND", rt.make_int(wxSTC_GC_COMMAND) },
    { WXE_ATOM_define, "wxSTC_GC_CONTROL", rt.make_int(wxSTC_GC_CONTROL) },
    { WXE_ATOM_define, "wxSTC_GC_ATTRIBUTE", rt.make_int(wxSTC_GC_ATTRIBUTE) },
    { WXE_ATOM_define, "wxSTC_GC_EVENT", rt.make_int(wxSTC_GC_EVENT) },
    { WXE_ATOM_define, "wxSTC_GC_GLOBAL", rt.make_int(wxSTC_GC_GLOBAL) },
    { WXE_ATOM_define, "wxSTC_GC_COMMENTBLOCK", rt.make_int(wxSTC_GC_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_GC_COMMENTLINE", rt.make_int(wxSTC_GC_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_GC_DEFAULT", rt.make_int(wxSTC_GC_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_KIX_IDENTIFIER", rt.make_int(wxSTC_KIX_IDENTIFIER) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KIX_COMMENTSTREAM", rt.make_int(wxSTC_KIX_COMMENTSTREAM) },
#else
    { WXE_ATOM_define, "wxSTC_KIX_COMMENTSTREAM", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_KIX_OPERATOR", rt.make_int(wxSTC_KIX_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_KIX_FUNCTIONS", rt.make_int(wxSTC_KIX_FUNCTIONS) },
    { WXE_ATOM_define, "wxSTC_KIX_KEYWORD", rt.make_int(wxSTC_KIX_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_KIX_MACRO", rt.make_int(wxSTC_KIX_MACRO) },
    { WXE_ATOM_define, "wxSTC_KIX_VAR", rt.make_int(wxSTC_KIX_VAR) },
    { WXE_ATOM_define, "wxSTC_KIX_NUMBER", rt.make_int(wxSTC_KIX_NUMBER) },
    { WXE_ATOM_define, "wxSTC_KIX_STRING2", rt.make_int(wxSTC_KIX_STRING2) },
    { WXE_ATOM_define, "wxSTC_KIX_STRING1", rt.make_int(wxSTC_KIX_STRING1) },
    { WXE_ATOM_define, "wxSTC_KIX_COMMENT", rt.make_int(wxSTC_KIX_COMMENT) },
    { WXE_ATOM_define, "wxSTC_KIX_DEFAULT", rt.make_int(wxSTC_KIX_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_V_PORT_CONNECT", rt.make_int(wxSTC_V_PORT_CONNECT) },
#else
    { WXE_ATOM_define, "wxSTC_V_PORT_CONNECT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_V_INOUT", rt.make_int(wxSTC_V_INOUT) },
#else
    { WXE_ATOM_define, "wxSTC_V_INOUT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_V_OUTPUT", rt.make_int(wxSTC_V_OUTPUT) },
#else
    { WXE_ATOM_define, "wxSTC_V_OUTPUT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_V_INPUT", rt.make_int(wxSTC_V_INPUT) },
#else
    { WXE_ATOM_define, "wxSTC_V_INPUT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_V_COMMENT_WORD", rt.make_int(wxSTC_V_COMMENT_WORD) },
#else
    { WXE_ATOM_define, "wxSTC_V_COMMENT_WORD", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_V_USER", rt.make_int(wxSTC_V_USER) },
    { WXE_ATOM_define, "wxSTC_V_STRINGEOL", rt.make_int(wxSTC_V_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_V_IDENTIFIER", rt.make_int(wxSTC_V_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_V_OPERATOR", rt.make_int(wxSTC_V_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_V_PREPROCESSOR", rt.make_int(wxSTC_V_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_V_WORD3", rt.make_int(wxSTC_V_WORD3) },
    { WXE_ATOM_define, "wxSTC_V_WORD2", rt.make_int(wxSTC_V_WORD2) },
    { WXE_ATOM_define, "wxSTC_V_STRING", rt.make_int(wxSTC_V_STRING) },
    { WXE_ATOM_define, "wxSTC_V_WORD", rt.make_int(wxSTC_V_WORD) },
    { WXE_ATOM_define, "wxSTC_V_NUMBER", rt.make_int(wxSTC_V_NUMBER) },
    { WXE_ATOM_define, "wxSTC_V_COMMENTLINEBANG", rt.make_int(wxSTC_V_COMMENTLINEBANG) },
    { WXE_ATOM_define, "wxSTC_V_COMMENTLINE", rt.make_int(wxSTC_V_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_V_COMMENT", rt.make_int(wxSTC_V_COMMENT) },
    { WXE_ATOM_define, "wxSTC_V_DEFAULT", rt.make_int(wxSTC_V_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MSSQL_COLUMN_NAME_2", rt.make_int(wxSTC_MSSQL_COLUMN_NAME_2) },
    { WXE_ATOM_define, "wxSTC_MSSQL_DEFAULT_PREF_DATATYPE", rt.make_int(wxSTC_MSSQL_DEFAULT_PREF_DATATYPE) },
    { WXE_ATOM_define, "wxSTC_MSSQL_STORED_PROCEDURE", rt.make_int(wxSTC_MSSQL_STORED_PROCEDURE) },
    { WXE_ATOM_define, "wxSTC_MSSQL_FUNCTION", rt.make_int(wxSTC_MSSQL_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_MSSQL_GLOBAL_VARIABLE", rt.make_int(wxSTC_MSSQL_GLOBAL_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_MSSQL_SYSTABLE", rt.make_int(wxSTC_MSSQL_SYSTABLE) },
    { WXE_ATOM_define, "wxSTC_MSSQL_DATATYPE", rt.make_int(wxSTC_MSSQL_DATATYPE) },
    { WXE_ATOM_define, "wxSTC_MSSQL_STATEMENT", rt.make_int(wxSTC_MSSQL_STATEMENT) },
    { WXE_ATOM_define, "wxSTC_MSSQL_COLUMN_NAME", rt.make_int(wxSTC_MSSQL_COLUMN_NAME) },
    { WXE_ATOM_define, "wxSTC_MSSQL_VARIABLE", rt.make_int(wxSTC_MSSQL_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_MSSQL_IDENTIFIER", rt.make_int(wxSTC_MSSQL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_MSSQL_OPERATOR", rt.make_int(wxSTC_MSSQL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MSSQL_STRING", rt.make_int(wxSTC_MSSQL_STRING) },
    { WXE_ATOM_define, "wxSTC_MSSQL_NUMBER", rt.make_int(wxSTC_MSSQL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MSSQL_LINE_COMMENT", rt.make_int(wxSTC_MSSQL_LINE_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MSSQL_COMMENT", rt.make_int(wxSTC_MSSQL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MSSQL_DEFAULT", rt.make_int(wxSTC_MSSQL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ERLANG_UNKNOWN", rt.make_int(wxSTC_ERLANG_UNKNOWN) },
    { WXE_ATOM_define, "wxSTC_ERLANG_MODULES_ATT", rt.make_int(wxSTC_ERLANG_MODULES_ATT) },
    { WXE_ATOM_define, "wxSTC_ERLANG_MODULES", rt.make_int(wxSTC_ERLANG_MODULES) },
    { WXE_ATOM_define, "wxSTC_ERLANG_BIFS", rt.make_int(wxSTC_ERLANG_BIFS) },
    { WXE_ATOM_define, "wxSTC_ERLANG_NODE_NAME_QUOTED", rt.make_int(wxSTC_ERLANG_NODE_NAME_QUOTED) },
    { WXE_ATOM_define, "wxSTC_ERLANG_RECORD_QUOTED", rt.make_int(wxSTC_ERLANG_RECORD_QUOTED) },
    { WXE_ATOM_define, "wxSTC_ERLANG_MACRO_QUOTED", rt.make_int(wxSTC_ERLANG_MACRO_QUOTED) },
    { WXE_ATOM_define, "wxSTC_ERLANG_ATOM_QUOTED", rt.make_int(wxSTC_ERLANG_ATOM_QUOTED) },
    { WXE_ATOM_define, "wxSTC_ERLANG_COMMENT_DOC_MACRO", rt.make_int(wxSTC_ERLANG_COMMENT_DOC_MACRO) },
    { WXE_ATOM_define, "wxSTC_ERLANG_COMMENT_DOC", rt.make_int(wxSTC_ERLANG_COMMENT_DOC) },
    { WXE_ATOM_define, "wxSTC_ERLANG_COMMENT_MODULE", rt.make_int(wxSTC_ERLANG_COMMENT_MODULE) },
    { WXE_ATOM_define, "wxSTC_ERLANG_COMMENT_FUNCTION", rt.make_int(wxSTC_ERLANG_COMMENT_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_ERLANG_NODE_NAME", rt.make_int(wxSTC_ERLANG_NODE_NAME) },
    { WXE_ATOM_define, "wxSTC_ERLANG_PREPROC", rt.make_int(wxSTC_ERLANG_PREPROC) },
    { WXE_ATOM_define, "wxSTC_ERLANG_RECORD", rt.make_int(wxSTC_ERLANG_RECORD) },
    { WXE_ATOM_define, "wxSTC_ERLANG_MACRO", rt.make_int(wxSTC_ERLANG_MACRO) },
    { WXE_ATOM_define, "wxSTC_ERLANG_CHARACTER", rt.make_int(wxSTC_ERLANG_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_ERLANG_FUNCTION_NAME", rt.make_int(wxSTC_ERLANG_FUNCTION_NAME) },
    { WXE_ATOM_define, "wxSTC_ERLANG_ATOM", rt.make_int(wxSTC_ERLANG_ATOM) },
    { WXE_ATOM_define, "wxSTC_ERLANG_OPERATOR", rt.make_int(wxSTC_ERLANG_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ERLANG_STRING", rt.make_int(wxSTC_ERLANG_STRING) },
    { WXE_ATOM_define, "wxSTC_ERLANG_KEYWORD", rt.make_int(wxSTC_ERLANG_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_ERLANG_NUMBER", rt.make_int(wxSTC_ERLANG_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ERLANG_VARIABLE", rt.make_int(wxSTC_ERLANG_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_ERLANG_COMMENT", rt.make_int(wxSTC_ERLANG_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ERLANG_DEFAULT", rt.make_int(wxSTC_ERLANG_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_METAPOST_EXTRA", rt.make_int(wxSTC_METAPOST_EXTRA) },
    { WXE_ATOM_define, "wxSTC_METAPOST_TEXT", rt.make_int(wxSTC_METAPOST_TEXT) },
    { WXE_ATOM_define, "wxSTC_METAPOST_COMMAND", rt.make_int(wxSTC_METAPOST_COMMAND) },
    { WXE_ATOM_define, "wxSTC_METAPOST_SYMBOL", rt.make_int(wxSTC_METAPOST_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_METAPOST_GROUP", rt.make_int(wxSTC_METAPOST_GROUP) },
    { WXE_ATOM_define, "wxSTC_METAPOST_SPECIAL", rt.make_int(wxSTC_METAPOST_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_METAPOST_DEFAULT", rt.make_int(wxSTC_METAPOST_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_TEX_TEXT", rt.make_int(wxSTC_TEX_TEXT) },
    { WXE_ATOM_define, "wxSTC_TEX_COMMAND", rt.make_int(wxSTC_TEX_COMMAND) },
    { WXE_ATOM_define, "wxSTC_TEX_SYMBOL", rt.make_int(wxSTC_TEX_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_TEX_GROUP", rt.make_int(wxSTC_TEX_GROUP) },
    { WXE_ATOM_define, "wxSTC_TEX_SPECIAL", rt.make_int(wxSTC_TEX_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_TEX_DEFAULT", rt.make_int(wxSTC_TEX_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_YAML_OPERATOR", rt.make_int(wxSTC_YAML_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_YAML_ERROR", rt.make_int(wxSTC_YAML_ERROR) },
    { WXE_ATOM_define, "wxSTC_YAML_TEXT", rt.make_int(wxSTC_YAML_TEXT) },
    { WXE_ATOM_define, "wxSTC_YAML_DOCUMENT", rt.make_int(wxSTC_YAML_DOCUMENT) },
    { WXE_ATOM_define, "wxSTC_YAML_REFERENCE", rt.make_int(wxSTC_YAML_REFERENCE) },
    { WXE_ATOM_define, "wxSTC_YAML_NUMBER", rt.make_int(wxSTC_YAML_NUMBER) },
    { WXE_ATOM_define, "wxSTC_YAML_KEYWORD", rt.make_int(wxSTC_YAML_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_YAML_IDENTIFIER", rt.make_int(wxSTC_YAML_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_YAML_COMMENT", rt.make_int(wxSTC_YAML_COMMENT) },
    { WXE_ATOM_define, "wxSTC_YAML_DEFAULT", rt.make_int(wxSTC_YAML_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_LOT_ABORT", rt.make_int(wxSTC_LOT_ABORT) },
    { WXE_ATOM_define, "wxSTC_LOT_FAIL", rt.make_int(wxSTC_LOT_FAIL) },
    { WXE_ATOM_define, "wxSTC_LOT_PASS", rt.make_int(wxSTC_LOT_PASS) },
    { WXE_ATOM_define, "wxSTC_LOT_SET", rt.make_int(wxSTC_LOT_SET) },
    { WXE_ATOM_define, "wxSTC_LOT_BREAK", rt.make_int(wxSTC_LOT_BREAK) },
    { WXE_ATOM_define, "wxSTC_LOT_HEADER", rt.make_int(wxSTC_LOT_HEADER) },
    { WXE_ATOM_define, "wxSTC_LOT_DEFAULT", rt.make_int(wxSTC_LOT_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CLW_DEPRECATED", rt.make_int(wxSTC_CLW_DEPRECATED) },
    { WXE_ATOM_define, "wxSTC_CLW_ERROR", rt.make_int(wxSTC_CLW_ERROR) },
    { WXE_ATOM_define, "wxSTC_CLW_STANDARD_EQUATE", rt.make_int(wxSTC_CLW_STANDARD_EQUATE) },
    { WXE_ATOM_define, "wxSTC_CLW_ATTRIBUTE", rt.make_int(wxSTC_CLW_ATTRIBUTE) },
    { WXE_ATOM_define, "wxSTC_CLW_STRUCTURE_DATA_TYPE", rt.make_int(wxSTC_CLW_STRUCTURE_DATA_TYPE) },
    { WXE_ATOM_define, "wxSTC_CLW_BUILTIN_PROCEDURES_FUNCTION", rt.make_int(wxSTC_CLW_BUILTIN_PROCEDURES_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_CLW_RUNTIME_EXPRESSIONS", rt.make_int(wxSTC_CLW_RUNTIME_EXPRESSIONS) },
    { WXE_ATOM_define, "wxSTC_CLW_COMPILER_DIRECTIVE", rt.make_int(wxSTC_CLW_COMPILER_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_CLW_KEYWORD", rt.make_int(wxSTC_CLW_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_CLW_PICTURE_STRING", rt.make_int(wxSTC_CLW_PICTURE_STRING) },
    { WXE_ATOM_define, "wxSTC_CLW_REAL_CONSTANT", rt.make_int(wxSTC_CLW_REAL_CONSTANT) },
    { WXE_ATOM_define, "wxSTC_CLW_INTEGER_CONSTANT", rt.make_int(wxSTC_CLW_INTEGER_CONSTANT) },
    { WXE_ATOM_define, "wxSTC_CLW_USER_IDENTIFIER", rt.make_int(wxSTC_CLW_USER_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CLW_STRING", rt.make_int(wxSTC_CLW_STRING) },
    { WXE_ATOM_define, "wxSTC_CLW_COMMENT", rt.make_int(wxSTC_CLW_COMMENT) },
    { WXE_ATOM_define, "wxSTC_CLW_LABEL", rt.make_int(wxSTC_CLW_LABEL) },
    { WXE_ATOM_define, "wxSTC_CLW_DEFAULT", rt.make_int(wxSTC_CLW_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_INCLUDE", rt.make_int(wxSTC_MMIXAL_INCLUDE) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_SYMBOL", rt.make_int(wxSTC_MMIXAL_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPERATOR", rt.make_int(wxSTC_MMIXAL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_HEX", rt.make_int(wxSTC_MMIXAL_HEX) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_REGISTER", rt.make_int(wxSTC_MMIXAL_REGISTER) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_STRING", rt.make_int(wxSTC_MMIXAL_STRING) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_CHAR", rt.make_int(wxSTC_MMIXAL_CHAR) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_REF", rt.make_int(wxSTC_MMIXAL_REF) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_NUMBER", rt.make_int(wxSTC_MMIXAL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPERANDS", rt.make_int(wxSTC_MMIXAL_OPERANDS) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPCODE_POST", rt.make_int(wxSTC_MMIXAL_OPCODE_POST) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPCODE_UNKNOWN", rt.make_int(wxSTC_MMIXAL_OPCODE_UNKNOWN) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPCODE_VALID", rt.make_int(wxSTC_MMIXAL_OPCODE_VALID) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPCODE_PRE", rt.make_int(wxSTC_MMIXAL_OPCODE_PRE) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_OPCODE", rt.make_int(wxSTC_MMIXAL_OPCODE) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_LABEL", rt.make_int(wxSTC_MMIXAL_LABEL) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_COMMENT", rt.make_int(wxSTC_MMIXAL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MMIXAL_LEADWS", rt.make_int(wxSTC_MMIXAL_LEADWS) },
    { WXE_ATOM_define, "wxSTC_NSIS_COMMENTBOX", rt.make_int(wxSTC_NSIS_COMMENTBOX) },
    { WXE_ATOM_define, "wxSTC_NSIS_FUNCTIONDEF", rt.make_int(wxSTC_NSIS_FUNCTIONDEF) },
    { WXE_ATOM_define, "wxSTC_NSIS_PAGEEX", rt.make_int(wxSTC_NSIS_PAGEEX) },
    { WXE_ATOM_define, "wxSTC_NSIS_SECTIONGROUP", rt.make_int(wxSTC_NSIS_SECTIONGROUP) },
    { WXE_ATOM_define, "wxSTC_NSIS_NUMBER", rt.make_int(wxSTC_NSIS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_NSIS_STRINGVAR", rt.make_int(wxSTC_NSIS_STRINGVAR) },
    { WXE_ATOM_define, "wxSTC_NSIS_MACRODEF", rt.make_int(wxSTC_NSIS_MACRODEF) },
    { WXE_ATOM_define, "wxSTC_NSIS_IFDEFINEDEF", rt.make_int(wxSTC_NSIS_IFDEFINEDEF) },
    { WXE_ATOM_define, "wxSTC_NSIS_SUBSECTIONDEF", rt.make_int(wxSTC_NSIS_SUBSECTIONDEF) },
    { WXE_ATOM_define, "wxSTC_NSIS_SECTIONDEF", rt.make_int(wxSTC_NSIS_SECTIONDEF) },
    { WXE_ATOM_define, "wxSTC_NSIS_USERDEFINED", rt.make_int(wxSTC_NSIS_USERDEFINED) },
    { WXE_ATOM_define, "wxSTC_NSIS_LABEL", rt.make_int(wxSTC_NSIS_LABEL) },
    { WXE_ATOM_define, "wxSTC_NSIS_VARIABLE", rt.make_int(wxSTC_NSIS_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_NSIS_FUNCTION", rt.make_int(wxSTC_NSIS_FUNCTION) },
    { WXE_ATOM_define, "wxSTC_NSIS_STRINGRQ", rt.make_int(wxSTC_NSIS_STRINGRQ) },
    { WXE_ATOM_define, "wxSTC_NSIS_STRINGLQ", rt.make_int(wxSTC_NSIS_STRINGLQ) },
    { WXE_ATOM_define, "wxSTC_NSIS_STRINGDQ", rt.make_int(wxSTC_NSIS_STRINGDQ) },
    { WXE_ATOM_define, "wxSTC_NSIS_COMMENT", rt.make_int(wxSTC_NSIS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_NSIS_DEFAULT", rt.make_int(wxSTC_NSIS_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_PS_BADSTRINGCHAR", rt.make_int(wxSTC_PS_BADSTRINGCHAR) },
    { WXE_ATOM_define, "wxSTC_PS_BASE85STRING", rt.make_int(wxSTC_PS_BASE85STRING) },
    { WXE_ATOM_define, "wxSTC_PS_HEXSTRING", rt.make_int(wxSTC_PS_HEXSTRING) },
    { WXE_ATOM_define, "wxSTC_PS_TEXT", rt.make_int(wxSTC_PS_TEXT) },
    { WXE_ATOM_define, "wxSTC_PS_PAREN_PROC", rt.make_int(wxSTC_PS_PAREN_PROC) },
    { WXE_ATOM_define, "wxSTC_PS_PAREN_DICT", rt.make_int(wxSTC_PS_PAREN_DICT) },
    { WXE_ATOM_define, "wxSTC_PS_PAREN_ARRAY", rt.make_int(wxSTC_PS_PAREN_ARRAY) },
    { WXE_ATOM_define, "wxSTC_PS_IMMEVAL", rt.make_int(wxSTC_PS_IMMEVAL) },
    { WXE_ATOM_define, "wxSTC_PS_LITERAL", rt.make_int(wxSTC_PS_LITERAL) },
    { WXE_ATOM_define, "wxSTC_PS_KEYWORD", rt.make_int(wxSTC_PS_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_PS_NAME", rt.make_int(wxSTC_PS_NAME) },
    { WXE_ATOM_define, "wxSTC_PS_NUMBER", rt.make_int(wxSTC_PS_NUMBER) },
    { WXE_ATOM_define, "wxSTC_PS_DSC_VALUE", rt.make_int(wxSTC_PS_DSC_VALUE) },
    { WXE_ATOM_define, "wxSTC_PS_DSC_COMMENT", rt.make_int(wxSTC_PS_DSC_COMMENT) },
    { WXE_ATOM_define, "wxSTC_PS_COMMENT", rt.make_int(wxSTC_PS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_PS_DEFAULT", rt.make_int(wxSTC_PS_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_WORD3", rt.make_int(wxSTC_ESCRIPT_WORD3) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_WORD2", rt.make_int(wxSTC_ESCRIPT_WORD2) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_BRACE", rt.make_int(wxSTC_ESCRIPT_BRACE) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_IDENTIFIER", rt.make_int(wxSTC_ESCRIPT_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_OPERATOR", rt.make_int(wxSTC_ESCRIPT_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_STRING", rt.make_int(wxSTC_ESCRIPT_STRING) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_WORD", rt.make_int(wxSTC_ESCRIPT_WORD) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_NUMBER", rt.make_int(wxSTC_ESCRIPT_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_COMMENTDOC", rt.make_int(wxSTC_ESCRIPT_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_COMMENTLINE", rt.make_int(wxSTC_ESCRIPT_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_COMMENT", rt.make_int(wxSTC_ESCRIPT_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ESCRIPT_DEFAULT", rt.make_int(wxSTC_ESCRIPT_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_LOUT_STRINGEOL", rt.make_int(wxSTC_LOUT_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_LOUT_IDENTIFIER", rt.make_int(wxSTC_LOUT_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_LOUT_OPERATOR", rt.make_int(wxSTC_LOUT_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_LOUT_STRING", rt.make_int(wxSTC_LOUT_STRING) },
    { WXE_ATOM_define, "wxSTC_LOUT_WORD4", rt.make_int(wxSTC_LOUT_WORD4) },
    { WXE_ATOM_define, "wxSTC_LOUT_WORD3", rt.make_int(wxSTC_LOUT_WORD3) },
    { WXE_ATOM_define, "wxSTC_LOUT_WORD2", rt.make_int(wxSTC_LOUT_WORD2) },
    { WXE_ATOM_define, "wxSTC_LOUT_WORD", rt.make_int(wxSTC_LOUT_WORD) },
    { WXE_ATOM_define, "wxSTC_LOUT_NUMBER", rt.make_int(wxSTC_LOUT_NUMBER) },
    { WXE_ATOM_define, "wxSTC_LOUT_COMMENT", rt.make_int(wxSTC_LOUT_COMMENT) },
    { WXE_ATOM_define, "wxSTC_LOUT_DEFAULT", rt.make_int(wxSTC_LOUT_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_POV_WORD8", rt.make_int(wxSTC_POV_WORD8) },
    { WXE_ATOM_define, "wxSTC_POV_WORD7", rt.make_int(wxSTC_POV_WORD7) },
    { WXE_ATOM_define, "wxSTC_POV_WORD6", rt.make_int(wxSTC_POV_WORD6) },
    { WXE_ATOM_define, "wxSTC_POV_WORD5", rt.make_int(wxSTC_POV_WORD5) },
    { WXE_ATOM_define, "wxSTC_POV_WORD4", rt.make_int(wxSTC_POV_WORD4) },
    { WXE_ATOM_define, "wxSTC_POV_WORD3", rt.make_int(wxSTC_POV_WORD3) },
    { WXE_ATOM_define, "wxSTC_POV_WORD2", rt.make_int(wxSTC_POV_WORD2) },
    { WXE_ATOM_define, "wxSTC_POV_BADDIRECTIVE", rt.make_int(wxSTC_POV_BADDIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_POV_DIRECTIVE", rt.make_int(wxSTC_POV_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_POV_STRINGEOL", rt.make_int(wxSTC_POV_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_POV_STRING", rt.make_int(wxSTC_POV_STRING) },
    { WXE_ATOM_define, "wxSTC_POV_IDENTIFIER", rt.make_int(wxSTC_POV_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_POV_OPERATOR", rt.make_int(wxSTC_POV_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_POV_NUMBER", rt.make_int(wxSTC_POV_NUMBER) },
    { WXE_ATOM_define, "wxSTC_POV_COMMENTLINE", rt.make_int(wxSTC_POV_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_POV_COMMENT", rt.make_int(wxSTC_POV_COMMENT) },
    { WXE_ATOM_define, "wxSTC_POV_DEFAULT", rt.make_int(wxSTC_POV_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CSS_VARIABLE", rt.make_int(wxSTC_CSS_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_CSS_MEDIA", rt.make_int(wxSTC_CSS_MEDIA) },
    { WXE_ATOM_define, "wxSTC_CSS_EXTENDED_PSEUDOELEMENT", rt.make_int(wxSTC_CSS_EXTENDED_PSEUDOELEMENT) },
    { WXE_ATOM_define, "wxSTC_CSS_EXTENDED_PSEUDOCLASS", rt.make_int(wxSTC_CSS_EXTENDED_PSEUDOCLASS) },
    { WXE_ATOM_define, "wxSTC_CSS_EXTENDED_IDENTIFIER", rt.make_int(wxSTC_CSS_EXTENDED_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CSS_PSEUDOELEMENT", rt.make_int(wxSTC_CSS_PSEUDOELEMENT) },
    { WXE_ATOM_define, "wxSTC_CSS_IDENTIFIER3", rt.make_int(wxSTC_CSS_IDENTIFIER3) },
    { WXE_ATOM_define, "wxSTC_CSS_ATTRIBUTE", rt.make_int(wxSTC_CSS_ATTRIBUTE) },
    { WXE_ATOM_define, "wxSTC_CSS_IDENTIFIER2", rt.make_int(wxSTC_CSS_IDENTIFIER2) },
    { WXE_ATOM_define, "wxSTC_CSS_SINGLESTRING", rt.make_int(wxSTC_CSS_SINGLESTRING) },
    { WXE_ATOM_define, "wxSTC_CSS_DOUBLESTRING", rt.make_int(wxSTC_CSS_DOUBLESTRING) },
    { WXE_ATOM_define, "wxSTC_CSS_DIRECTIVE", rt.make_int(wxSTC_CSS_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_CSS_IMPORTANT", rt.make_int(wxSTC_CSS_IMPORTANT) },
    { WXE_ATOM_define, "wxSTC_CSS_ID", rt.make_int(wxSTC_CSS_ID) },
    { WXE_ATOM_define, "wxSTC_CSS_COMMENT", rt.make_int(wxSTC_CSS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_CSS_VALUE", rt.make_int(wxSTC_CSS_VALUE) },
    { WXE_ATOM_define, "wxSTC_CSS_UNKNOWN_IDENTIFIER", rt.make_int(wxSTC_CSS_UNKNOWN_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CSS_IDENTIFIER", rt.make_int(wxSTC_CSS_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CSS_OPERATOR", rt.make_int(wxSTC_CSS_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_CSS_UNKNOWN_PSEUDOCLASS", rt.make_int(wxSTC_CSS_UNKNOWN_PSEUDOCLASS) },
    { WXE_ATOM_define, "wxSTC_CSS_PSEUDOCLASS", rt.make_int(wxSTC_CSS_PSEUDOCLASS) },
    { WXE_ATOM_define, "wxSTC_CSS_CLASS", rt.make_int(wxSTC_CSS_CLASS) },
    { WXE_ATOM_define, "wxSTC_CSS_TAG", rt.make_int(wxSTC_CSS_TAG) },
    { WXE_ATOM_define, "wxSTC_CSS_DEFAULT", rt.make_int(wxSTC_CSS_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_F_CONTINUATION", rt.make_int(wxSTC_F_CONTINUATION) },
    { WXE_ATOM_define, "wxSTC_F_LABEL", rt.make_int(wxSTC_F_LABEL) },
    { WXE_ATOM_define, "wxSTC_F_OPERATOR2", rt.make_int(wxSTC_F_OPERATOR2) },
    { WXE_ATOM_define, "wxSTC_F_PREPROCESSOR", rt.make_int(wxSTC_F_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_F_WORD3", rt.make_int(wxSTC_F_WORD3) },
    { WXE_ATOM_define, "wxSTC_F_WORD2", rt.make_int(wxSTC_F_WORD2) },
    { WXE_ATOM_define, "wxSTC_F_WORD", rt.make_int(wxSTC_F_WORD) },
    { WXE_ATOM_define, "wxSTC_F_IDENTIFIER", rt.make_int(wxSTC_F_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_F_OPERATOR", rt.make_int(wxSTC_F_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_F_STRINGEOL", rt.make_int(wxSTC_F_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_F_STRING2", rt.make_int(wxSTC_F_STRING2) },
    { WXE_ATOM_define, "wxSTC_F_STRING1", rt.make_int(wxSTC_F_STRING1) },
    { WXE_ATOM_define, "wxSTC_F_NUMBER", rt.make_int(wxSTC_F_NUMBER) },
    { WXE_ATOM_define, "wxSTC_F_COMMENT", rt.make_int(wxSTC_F_COMMENT) },
    { WXE_ATOM_define, "wxSTC_F_DEFAULT", rt.make_int(wxSTC_F_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_ASM_COMMENTDIRECTIVE", rt.make_int(wxSTC_ASM_COMMENTDIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_ASM_EXTINSTRUCTION", rt.make_int(wxSTC_ASM_EXTINSTRUCTION) },
    { WXE_ATOM_define, "wxSTC_ASM_STRINGEOL", rt.make_int(wxSTC_ASM_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_ASM_CHARACTER", rt.make_int(wxSTC_ASM_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_ASM_COMMENTBLOCK", rt.make_int(wxSTC_ASM_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_ASM_DIRECTIVEOPERAND", rt.make_int(wxSTC_ASM_DIRECTIVEOPERAND) },
    { WXE_ATOM_define, "wxSTC_ASM_DIRECTIVE", rt.make_int(wxSTC_ASM_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_ASM_REGISTER", rt.make_int(wxSTC_ASM_REGISTER) },
    { WXE_ATOM_define, "wxSTC_ASM_MATHINSTRUCTION", rt.make_int(wxSTC_ASM_MATHINSTRUCTION) },
    { WXE_ATOM_define, "wxSTC_ASM_CPUINSTRUCTION", rt.make_int(wxSTC_ASM_CPUINSTRUCTION) },
    { WXE_ATOM_define, "wxSTC_ASM_IDENTIFIER", rt.make_int(wxSTC_ASM_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_ASM_OPERATOR", rt.make_int(wxSTC_ASM_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_ASM_STRING", rt.make_int(wxSTC_ASM_STRING) },
    { WXE_ATOM_define, "wxSTC_ASM_NUMBER", rt.make_int(wxSTC_ASM_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ASM_COMMENT", rt.make_int(wxSTC_ASM_COMMENT) },
    { WXE_ATOM_define, "wxSTC_ASM_DEFAULT", rt.make_int(wxSTC_ASM_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_PREPROCESSOR", rt.make_int(wxSTC_SCRIPTOL_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_CLASSNAME", rt.make_int(wxSTC_SCRIPTOL_CLASSNAME) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_TRIPLE", rt.make_int(wxSTC_SCRIPTOL_TRIPLE) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_IDENTIFIER", rt.make_int(wxSTC_SCRIPTOL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_OPERATOR", rt.make_int(wxSTC_SCRIPTOL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_KEYWORD", rt.make_int(wxSTC_SCRIPTOL_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_STRINGEOL", rt.make_int(wxSTC_SCRIPTOL_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_CHARACTER", rt.make_int(wxSTC_SCRIPTOL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_STRING", rt.make_int(wxSTC_SCRIPTOL_STRING) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_NUMBER", rt.make_int(wxSTC_SCRIPTOL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_COMMENTBLOCK", rt.make_int(wxSTC_SCRIPTOL_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_CSTYLE", rt.make_int(wxSTC_SCRIPTOL_CSTYLE) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_PERSISTENT", rt.make_int(wxSTC_SCRIPTOL_PERSISTENT) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_COMMENTLINE", rt.make_int(wxSTC_SCRIPTOL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_WHITE", rt.make_int(wxSTC_SCRIPTOL_WHITE) },
    { WXE_ATOM_define, "wxSTC_SCRIPTOL_DEFAULT", rt.make_int(wxSTC_SCRIPTOL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MATLAB_DOUBLEQUOTESTRING", rt.make_int(wxSTC_MATLAB_DOUBLEQUOTESTRING) },
    { WXE_ATOM_define, "wxSTC_MATLAB_IDENTIFIER", rt.make_int(wxSTC_MATLAB_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_MATLAB_OPERATOR", rt.make_int(wxSTC_MATLAB_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MATLAB_STRING", rt.make_int(wxSTC_MATLAB_STRING) },
    { WXE_ATOM_define, "wxSTC_MATLAB_KEYWORD", rt.make_int(wxSTC_MATLAB_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_MATLAB_NUMBER", rt.make_int(wxSTC_MATLAB_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MATLAB_COMMAND", rt.make_int(wxSTC_MATLAB_COMMAND) },
    { WXE_ATOM_define, "wxSTC_MATLAB_COMMENT", rt.make_int(wxSTC_MATLAB_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MATLAB_DEFAULT", rt.make_int(wxSTC_MATLAB_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_FORTH_LOCALE", rt.make_int(wxSTC_FORTH_LOCALE) },
    { WXE_ATOM_define, "wxSTC_FORTH_STRING", rt.make_int(wxSTC_FORTH_STRING) },
    { WXE_ATOM_define, "wxSTC_FORTH_NUMBER", rt.make_int(wxSTC_FORTH_NUMBER) },
    { WXE_ATOM_define, "wxSTC_FORTH_PREWORD2", rt.make_int(wxSTC_FORTH_PREWORD2) },
    { WXE_ATOM_define, "wxSTC_FORTH_PREWORD1", rt.make_int(wxSTC_FORTH_PREWORD1) },
    { WXE_ATOM_define, "wxSTC_FORTH_DEFWORD", rt.make_int(wxSTC_FORTH_DEFWORD) },
    { WXE_ATOM_define, "wxSTC_FORTH_KEYWORD", rt.make_int(wxSTC_FORTH_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_FORTH_CONTROL", rt.make_int(wxSTC_FORTH_CONTROL) },
    { WXE_ATOM_define, "wxSTC_FORTH_IDENTIFIER", rt.make_int(wxSTC_FORTH_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_FORTH_COMMENT_ML", rt.make_int(wxSTC_FORTH_COMMENT_ML) },
    { WXE_ATOM_define, "wxSTC_FORTH_COMMENT", rt.make_int(wxSTC_FORTH_COMMENT) },
    { WXE_ATOM_define, "wxSTC_FORTH_DEFAULT", rt.make_int(wxSTC_FORTH_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_IDENTIFIER", rt.make_int(wxSTC_NNCRONTAB_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_ENVIRONMENT", rt.make_int(wxSTC_NNCRONTAB_ENVIRONMENT) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_STRING", rt.make_int(wxSTC_NNCRONTAB_STRING) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_NUMBER", rt.make_int(wxSTC_NNCRONTAB_NUMBER) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_ASTERISK", rt.make_int(wxSTC_NNCRONTAB_ASTERISK) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_MODIFIER", rt.make_int(wxSTC_NNCRONTAB_MODIFIER) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_KEYWORD", rt.make_int(wxSTC_NNCRONTAB_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_SECTION", rt.make_int(wxSTC_NNCRONTAB_SECTION) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_TASK", rt.make_int(wxSTC_NNCRONTAB_TASK) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_COMMENT", rt.make_int(wxSTC_NNCRONTAB_COMMENT) },
    { WXE_ATOM_define, "wxSTC_NNCRONTAB_DEFAULT", rt.make_int(wxSTC_NNCRONTAB_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_STRINGEOL", rt.make_int(wxSTC_EIFFEL_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_IDENTIFIER", rt.make_int(wxSTC_EIFFEL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_OPERATOR", rt.make_int(wxSTC_EIFFEL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_CHARACTER", rt.make_int(wxSTC_EIFFEL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_STRING", rt.make_int(wxSTC_EIFFEL_STRING) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_WORD", rt.make_int(wxSTC_EIFFEL_WORD) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_NUMBER", rt.make_int(wxSTC_EIFFEL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_COMMENTLINE", rt.make_int(wxSTC_EIFFEL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_EIFFEL_DEFAULT", rt.make_int(wxSTC_EIFFEL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_LISP_MULTI_COMMENT", rt.make_int(wxSTC_LISP_MULTI_COMMENT) },
    { WXE_ATOM_define, "wxSTC_LISP_SPECIAL", rt.make_int(wxSTC_LISP_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_LISP_OPERATOR", rt.make_int(wxSTC_LISP_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_LISP_IDENTIFIER", rt.make_int(wxSTC_LISP_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_LISP_STRINGEOL", rt.make_int(wxSTC_LISP_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_LISP_STRING", rt.make_int(wxSTC_LISP_STRING) },
    { WXE_ATOM_define, "wxSTC_LISP_SYMBOL", rt.make_int(wxSTC_LISP_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_LISP_KEYWORD_KW", rt.make_int(wxSTC_LISP_KEYWORD_KW) },
    { WXE_ATOM_define, "wxSTC_LISP_KEYWORD", rt.make_int(wxSTC_LISP_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_LISP_NUMBER", rt.make_int(wxSTC_LISP_NUMBER) },
    { WXE_ATOM_define, "wxSTC_LISP_COMMENT", rt.make_int(wxSTC_LISP_COMMENT) },
    { WXE_ATOM_define, "wxSTC_LISP_DEFAULT", rt.make_int(wxSTC_LISP_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_DEFINEDEF", rt.make_int(wxSTC_BAAN_DEFINEDEF) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_DEFINEDEF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_OBJECTDEF", rt.make_int(wxSTC_BAAN_OBJECTDEF) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_OBJECTDEF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_FUNCDEF", rt.make_int(wxSTC_BAAN_FUNCDEF) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_FUNCDEF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_DOMDEF", rt.make_int(wxSTC_BAAN_DOMDEF) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_DOMDEF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_FUNCTION", rt.make_int(wxSTC_BAAN_FUNCTION) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_FUNCTION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_TABLESQL", rt.make_int(wxSTC_BAAN_TABLESQL) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_TABLESQL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_TABLEDEF", rt.make_int(wxSTC_BAAN_TABLEDEF) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_TABLEDEF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD9", rt.make_int(wxSTC_BAAN_WORD9) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD9", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD8", rt.make_int(wxSTC_BAAN_WORD8) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD8", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD7", rt.make_int(wxSTC_BAAN_WORD7) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD7", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD6", rt.make_int(wxSTC_BAAN_WORD6) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD6", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD5", rt.make_int(wxSTC_BAAN_WORD5) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD5", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD4", rt.make_int(wxSTC_BAAN_WORD4) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD4", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD3", rt.make_int(wxSTC_BAAN_WORD3) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD3", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD2", rt.make_int(wxSTC_BAAN_WORD2) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_STRINGEOL", rt.make_int(wxSTC_BAAN_STRINGEOL) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_STRINGEOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_IDENTIFIER", rt.make_int(wxSTC_BAAN_IDENTIFIER) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_IDENTIFIER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_OPERATOR", rt.make_int(wxSTC_BAAN_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_PREPROCESSOR", rt.make_int(wxSTC_BAAN_PREPROCESSOR) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_PREPROCESSOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_STRING", rt.make_int(wxSTC_BAAN_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_WORD", rt.make_int(wxSTC_BAAN_WORD) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_WORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_NUMBER", rt.make_int(wxSTC_BAAN_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_COMMENTDOC", rt.make_int(wxSTC_BAAN_COMMENTDOC) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_COMMENTDOC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_COMMENT", rt.make_int(wxSTC_BAAN_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_BAAN_DEFAULT", rt.make_int(wxSTC_BAAN_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_BAAN_DEFAULT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_ADA_ILLEGAL", rt.make_int(wxSTC_ADA_ILLEGAL) },
    { WXE_ATOM_define, "wxSTC_ADA_COMMENTLINE", rt.make_int(wxSTC_ADA_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_ADA_LABEL", rt.make_int(wxSTC_ADA_LABEL) },
    { WXE_ATOM_define, "wxSTC_ADA_STRINGEOL", rt.make_int(wxSTC_ADA_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_ADA_STRING", rt.make_int(wxSTC_ADA_STRING) },
    { WXE_ATOM_define, "wxSTC_ADA_CHARACTEREOL", rt.make_int(wxSTC_ADA_CHARACTEREOL) },
    { WXE_ATOM_define, "wxSTC_ADA_CHARACTER", rt.make_int(wxSTC_ADA_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_ADA_DELIMITER", rt.make_int(wxSTC_ADA_DELIMITER) },
    { WXE_ATOM_define, "wxSTC_ADA_NUMBER", rt.make_int(wxSTC_ADA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_ADA_IDENTIFIER", rt.make_int(wxSTC_ADA_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_ADA_WORD", rt.make_int(wxSTC_ADA_WORD) },
    { WXE_ATOM_define, "wxSTC_ADA_DEFAULT", rt.make_int(wxSTC_ADA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD6", rt.make_int(wxSTC_AVE_WORD6) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD5", rt.make_int(wxSTC_AVE_WORD5) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD4", rt.make_int(wxSTC_AVE_WORD4) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD3", rt.make_int(wxSTC_AVE_WORD3) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD2", rt.make_int(wxSTC_AVE_WORD2) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD1", rt.make_int(wxSTC_AVE_WORD1) },
    { WXE_ATOM_define, "wxSTC_AVE_OPERATOR", rt.make_int(wxSTC_AVE_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_AVE_IDENTIFIER", rt.make_int(wxSTC_AVE_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_AVE_STRINGEOL", rt.make_int(wxSTC_AVE_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_AVE_ENUM", rt.make_int(wxSTC_AVE_ENUM) },
    { WXE_ATOM_define, "wxSTC_AVE_STRING", rt.make_int(wxSTC_AVE_STRING) },
    { WXE_ATOM_define, "wxSTC_AVE_WORD", rt.make_int(wxSTC_AVE_WORD) },
    { WXE_ATOM_define, "wxSTC_AVE_NUMBER", rt.make_int(wxSTC_AVE_NUMBER) },
    { WXE_ATOM_define, "wxSTC_AVE_COMMENT", rt.make_int(wxSTC_AVE_COMMENT) },
    { WXE_ATOM_define, "wxSTC_AVE_DEFAULT", rt.make_int(wxSTC_AVE_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CONF_DIRECTIVE", rt.make_int(wxSTC_CONF_DIRECTIVE) },
    { WXE_ATOM_define, "wxSTC_CONF_IP", rt.make_int(wxSTC_CONF_IP) },
    { WXE_ATOM_define, "wxSTC_CONF_OPERATOR", rt.make_int(wxSTC_CONF_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_CONF_STRING", rt.make_int(wxSTC_CONF_STRING) },
    { WXE_ATOM_define, "wxSTC_CONF_PARAMETER", rt.make_int(wxSTC_CONF_PARAMETER) },
    { WXE_ATOM_define, "wxSTC_CONF_EXTENSION", rt.make_int(wxSTC_CONF_EXTENSION) },
    { WXE_ATOM_define, "wxSTC_CONF_IDENTIFIER", rt.make_int(wxSTC_CONF_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_CONF_NUMBER", rt.make_int(wxSTC_CONF_NUMBER) },
    { WXE_ATOM_define, "wxSTC_CONF_COMMENT", rt.make_int(wxSTC_CONF_COMMENT) },
    { WXE_ATOM_define, "wxSTC_CONF_DEFAULT", rt.make_int(wxSTC_CONF_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_DIFF_CHANGED", rt.make_int(wxSTC_DIFF_CHANGED) },
    { WXE_ATOM_define, "wxSTC_DIFF_ADDED", rt.make_int(wxSTC_DIFF_ADDED) },
    { WXE_ATOM_define, "wxSTC_DIFF_DELETED", rt.make_int(wxSTC_DIFF_DELETED) },
    { WXE_ATOM_define, "wxSTC_DIFF_POSITION", rt.make_int(wxSTC_DIFF_POSITION) },
    { WXE_ATOM_define, "wxSTC_DIFF_HEADER", rt.make_int(wxSTC_DIFF_HEADER) },
    { WXE_ATOM_define, "wxSTC_DIFF_COMMAND", rt.make_int(wxSTC_DIFF_COMMAND) },
    { WXE_ATOM_define, "wxSTC_DIFF_COMMENT", rt.make_int(wxSTC_DIFF_COMMENT) },
    { WXE_ATOM_define, "wxSTC_DIFF_DEFAULT", rt.make_int(wxSTC_DIFF_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_MAKE_IDEOL", rt.make_int(wxSTC_MAKE_IDEOL) },
    { WXE_ATOM_define, "wxSTC_MAKE_TARGET", rt.make_int(wxSTC_MAKE_TARGET) },
    { WXE_ATOM_define, "wxSTC_MAKE_OPERATOR", rt.make_int(wxSTC_MAKE_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_MAKE_IDENTIFIER", rt.make_int(wxSTC_MAKE_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_MAKE_PREPROCESSOR", rt.make_int(wxSTC_MAKE_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_MAKE_COMMENT", rt.make_int(wxSTC_MAKE_COMMENT) },
    { WXE_ATOM_define, "wxSTC_MAKE_DEFAULT", rt.make_int(wxSTC_MAKE_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_TCMD_CLABEL", rt.make_int(wxSTC_TCMD_CLABEL) },
    { WXE_ATOM_define, "wxSTC_TCMD_EXPANSION", rt.make_int(wxSTC_TCMD_EXPANSION) },
    { WXE_ATOM_define, "wxSTC_TCMD_ENVIRONMENT", rt.make_int(wxSTC_TCMD_ENVIRONMENT) },
    { WXE_ATOM_define, "wxSTC_TCMD_OPERATOR", rt.make_int(wxSTC_TCMD_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_TCMD_IDENTIFIER", rt.make_int(wxSTC_TCMD_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_TCMD_COMMAND", rt.make_int(wxSTC_TCMD_COMMAND) },
    { WXE_ATOM_define, "wxSTC_TCMD_HIDE", rt.make_int(wxSTC_TCMD_HIDE) },
    { WXE_ATOM_define, "wxSTC_TCMD_LABEL", rt.make_int(wxSTC_TCMD_LABEL) },
    { WXE_ATOM_define, "wxSTC_TCMD_WORD", rt.make_int(wxSTC_TCMD_WORD) },
    { WXE_ATOM_define, "wxSTC_TCMD_COMMENT", rt.make_int(wxSTC_TCMD_COMMENT) },
    { WXE_ATOM_define, "wxSTC_TCMD_DEFAULT", rt.make_int(wxSTC_TCMD_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_BAT_OPERATOR", rt.make_int(wxSTC_BAT_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_BAT_IDENTIFIER", rt.make_int(wxSTC_BAT_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_BAT_COMMAND", rt.make_int(wxSTC_BAT_COMMAND) },
    { WXE_ATOM_define, "wxSTC_BAT_HIDE", rt.make_int(wxSTC_BAT_HIDE) },
    { WXE_ATOM_define, "wxSTC_BAT_LABEL", rt.make_int(wxSTC_BAT_LABEL) },
    { WXE_ATOM_define, "wxSTC_BAT_WORD", rt.make_int(wxSTC_BAT_WORD) },
    { WXE_ATOM_define, "wxSTC_BAT_COMMENT", rt.make_int(wxSTC_BAT_COMMENT) },
    { WXE_ATOM_define, "wxSTC_BAT_DEFAULT", rt.make_int(wxSTC_BAT_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_WHITE", rt.make_int(wxSTC_ERR_ES_WHITE) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_WHITE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_CYAN", rt.make_int(wxSTC_ERR_ES_BRIGHT_CYAN) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_CYAN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_MAGENTA", rt.make_int(wxSTC_ERR_ES_BRIGHT_MAGENTA) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_MAGENTA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_BLUE", rt.make_int(wxSTC_ERR_ES_BRIGHT_BLUE) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_BLUE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_YELLOW", rt.make_int(wxSTC_ERR_ES_YELLOW) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_YELLOW", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_GREEN", rt.make_int(wxSTC_ERR_ES_BRIGHT_GREEN) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_GREEN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_RED", rt.make_int(wxSTC_ERR_ES_BRIGHT_RED) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BRIGHT_RED", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_DARK_GRAY", rt.make_int(wxSTC_ERR_ES_DARK_GRAY) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_DARK_GRAY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_GRAY", rt.make_int(wxSTC_ERR_ES_GRAY) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_GRAY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_CYAN", rt.make_int(wxSTC_ERR_ES_CYAN) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_CYAN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_MAGENTA", rt.make_int(wxSTC_ERR_ES_MAGENTA) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_MAGENTA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BLUE", rt.make_int(wxSTC_ERR_ES_BLUE) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BLUE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BROWN", rt.make_int(wxSTC_ERR_ES_BROWN) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BROWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_GREEN", rt.make_int(wxSTC_ERR_ES_GREEN) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_GREEN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_RED", rt.make_int(wxSTC_ERR_ES_RED) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_RED", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ES_BLACK", rt.make_int(wxSTC_ERR_ES_BLACK) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ES_BLACK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ESCSEQ_UNKNOWN", rt.make_int(wxSTC_ERR_ESCSEQ_UNKNOWN) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ESCSEQ_UNKNOWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_ESCSEQ", rt.make_int(wxSTC_ERR_ESCSEQ) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_ESCSEQ", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ERR_GCC_INCLUDED_FROM", rt.make_int(wxSTC_ERR_GCC_INCLUDED_FROM) },
#else
    { WXE_ATOM_define, "wxSTC_ERR_GCC_INCLUDED_FROM", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_ERR_VALUE", rt.make_int(wxSTC_ERR_VALUE) },
    { WXE_ATOM_define, "wxSTC_ERR_JAVA_STACK", rt.make_int(wxSTC_ERR_JAVA_STACK) },
    { WXE_ATOM_define, "wxSTC_ERR_TIDY", rt.make_int(wxSTC_ERR_TIDY) },
    { WXE_ATOM_define, "wxSTC_ERR_ABSF", rt.make_int(wxSTC_ERR_ABSF) },
    { WXE_ATOM_define, "wxSTC_ERR_IFORT", rt.make_int(wxSTC_ERR_IFORT) },
    { WXE_ATOM_define, "wxSTC_ERR_IFC", rt.make_int(wxSTC_ERR_IFC) },
    { WXE_ATOM_define, "wxSTC_ERR_ELF", rt.make_int(wxSTC_ERR_ELF) },
    { WXE_ATOM_define, "wxSTC_ERR_PHP", rt.make_int(wxSTC_ERR_PHP) },
    { WXE_ATOM_define, "wxSTC_ERR_DIFF_MESSAGE", rt.make_int(wxSTC_ERR_DIFF_MESSAGE) },
    { WXE_ATOM_define, "wxSTC_ERR_DIFF_DELETION", rt.make_int(wxSTC_ERR_DIFF_DELETION) },
    { WXE_ATOM_define, "wxSTC_ERR_DIFF_ADDITION", rt.make_int(wxSTC_ERR_DIFF_ADDITION) },
    { WXE_ATOM_define, "wxSTC_ERR_DIFF_CHANGED", rt.make_int(wxSTC_ERR_DIFF_CHANGED) },
    { WXE_ATOM_define, "wxSTC_ERR_CTAG", rt.make_int(wxSTC_ERR_CTAG) },
    { WXE_ATOM_define, "wxSTC_ERR_LUA", rt.make_int(wxSTC_ERR_LUA) },
    { WXE_ATOM_define, "wxSTC_ERR_NET", rt.make_int(wxSTC_ERR_NET) },
    { WXE_ATOM_define, "wxSTC_ERR_PERL", rt.make_int(wxSTC_ERR_PERL) },
    { WXE_ATOM_define, "wxSTC_ERR_BORLAND", rt.make_int(wxSTC_ERR_BORLAND) },
    { WXE_ATOM_define, "wxSTC_ERR_CMD", rt.make_int(wxSTC_ERR_CMD) },
    { WXE_ATOM_define, "wxSTC_ERR_MS", rt.make_int(wxSTC_ERR_MS) },
    { WXE_ATOM_define, "wxSTC_ERR_GCC", rt.make_int(wxSTC_ERR_GCC) },
    { WXE_ATOM_define, "wxSTC_ERR_PYTHON", rt.make_int(wxSTC_ERR_PYTHON) },
    { WXE_ATOM_define, "wxSTC_ERR_DEFAULT", rt.make_int(wxSTC_ERR_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_LUA_LABEL", rt.make_int(wxSTC_LUA_LABEL) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD8", rt.make_int(wxSTC_LUA_WORD8) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD7", rt.make_int(wxSTC_LUA_WORD7) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD6", rt.make_int(wxSTC_LUA_WORD6) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD5", rt.make_int(wxSTC_LUA_WORD5) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD4", rt.make_int(wxSTC_LUA_WORD4) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD3", rt.make_int(wxSTC_LUA_WORD3) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD2", rt.make_int(wxSTC_LUA_WORD2) },
    { WXE_ATOM_define, "wxSTC_LUA_STRINGEOL", rt.make_int(wxSTC_LUA_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_LUA_IDENTIFIER", rt.make_int(wxSTC_LUA_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_LUA_OPERATOR", rt.make_int(wxSTC_LUA_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_LUA_PREPROCESSOR", rt.make_int(wxSTC_LUA_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_LUA_LITERALSTRING", rt.make_int(wxSTC_LUA_LITERALSTRING) },
    { WXE_ATOM_define, "wxSTC_LUA_CHARACTER", rt.make_int(wxSTC_LUA_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_LUA_STRING", rt.make_int(wxSTC_LUA_STRING) },
    { WXE_ATOM_define, "wxSTC_LUA_WORD", rt.make_int(wxSTC_LUA_WORD) },
    { WXE_ATOM_define, "wxSTC_LUA_NUMBER", rt.make_int(wxSTC_LUA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_LUA_COMMENTDOC", rt.make_int(wxSTC_LUA_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_LUA_COMMENTLINE", rt.make_int(wxSTC_LUA_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_LUA_COMMENT", rt.make_int(wxSTC_LUA_COMMENT) },
    { WXE_ATOM_define, "wxSTC_LUA_DEFAULT", rt.make_int(wxSTC_LUA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_L_ERROR", rt.make_int(wxSTC_L_ERROR) },
    { WXE_ATOM_define, "wxSTC_L_CMDOPT", rt.make_int(wxSTC_L_CMDOPT) },
    { WXE_ATOM_define, "wxSTC_L_SPECIAL", rt.make_int(wxSTC_L_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_L_SHORTCMD", rt.make_int(wxSTC_L_SHORTCMD) },
    { WXE_ATOM_define, "wxSTC_L_VERBATIM", rt.make_int(wxSTC_L_VERBATIM) },
    { WXE_ATOM_define, "wxSTC_L_COMMENT2", rt.make_int(wxSTC_L_COMMENT2) },
    { WXE_ATOM_define, "wxSTC_L_MATH2", rt.make_int(wxSTC_L_MATH2) },
    { WXE_ATOM_define, "wxSTC_L_TAG2", rt.make_int(wxSTC_L_TAG2) },
    { WXE_ATOM_define, "wxSTC_L_COMMENT", rt.make_int(wxSTC_L_COMMENT) },
    { WXE_ATOM_define, "wxSTC_L_MATH", rt.make_int(wxSTC_L_MATH) },
    { WXE_ATOM_define, "wxSTC_L_TAG", rt.make_int(wxSTC_L_TAG) },
    { WXE_ATOM_define, "wxSTC_L_COMMAND", rt.make_int(wxSTC_L_COMMAND) },
    { WXE_ATOM_define, "wxSTC_L_DEFAULT", rt.make_int(wxSTC_L_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_PROPS_KEY", rt.make_int(wxSTC_PROPS_KEY) },
    { WXE_ATOM_define, "wxSTC_PROPS_DEFVAL", rt.make_int(wxSTC_PROPS_DEFVAL) },
    { WXE_ATOM_define, "wxSTC_PROPS_ASSIGNMENT", rt.make_int(wxSTC_PROPS_ASSIGNMENT) },
    { WXE_ATOM_define, "wxSTC_PROPS_SECTION", rt.make_int(wxSTC_PROPS_SECTION) },
    { WXE_ATOM_define, "wxSTC_PROPS_COMMENT", rt.make_int(wxSTC_PROPS_COMMENT) },
    { WXE_ATOM_define, "wxSTC_PROPS_DEFAULT", rt.make_int(wxSTC_PROPS_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_DOCKEYWORD", rt.make_int(wxSTC_B_DOCKEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_B_DOCKEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_DOCBLOCK", rt.make_int(wxSTC_B_DOCBLOCK) },
#else
    { WXE_ATOM_define, "wxSTC_B_DOCBLOCK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_DOCLINE", rt.make_int(wxSTC_B_DOCLINE) },
#else
    { WXE_ATOM_define, "wxSTC_B_DOCLINE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_COMMENTBLOCK", rt.make_int(wxSTC_B_COMMENTBLOCK) },
#else
    { WXE_ATOM_define, "wxSTC_B_COMMENTBLOCK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_BINNUMBER", rt.make_int(wxSTC_B_BINNUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_B_BINNUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_HEXNUMBER", rt.make_int(wxSTC_B_HEXNUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_B_HEXNUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_ERROR", rt.make_int(wxSTC_B_ERROR) },
#else
    { WXE_ATOM_define, "wxSTC_B_ERROR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_LABEL", rt.make_int(wxSTC_B_LABEL) },
#else
    { WXE_ATOM_define, "wxSTC_B_LABEL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_ASM", rt.make_int(wxSTC_B_ASM) },
#else
    { WXE_ATOM_define, "wxSTC_B_ASM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_CONSTANT", rt.make_int(wxSTC_B_CONSTANT) },
#else
    { WXE_ATOM_define, "wxSTC_B_CONSTANT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_KEYWORD4", rt.make_int(wxSTC_B_KEYWORD4) },
#else
    { WXE_ATOM_define, "wxSTC_B_KEYWORD4", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_KEYWORD3", rt.make_int(wxSTC_B_KEYWORD3) },
#else
    { WXE_ATOM_define, "wxSTC_B_KEYWORD3", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_KEYWORD2", rt.make_int(wxSTC_B_KEYWORD2) },
#else
    { WXE_ATOM_define, "wxSTC_B_KEYWORD2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_STRINGEOL", rt.make_int(wxSTC_B_STRINGEOL) },
#else
    { WXE_ATOM_define, "wxSTC_B_STRINGEOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_DATE", rt.make_int(wxSTC_B_DATE) },
#else
    { WXE_ATOM_define, "wxSTC_B_DATE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_IDENTIFIER", rt.make_int(wxSTC_B_IDENTIFIER) },
#else
    { WXE_ATOM_define, "wxSTC_B_IDENTIFIER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_OPERATOR", rt.make_int(wxSTC_B_OPERATOR) },
#else
    { WXE_ATOM_define, "wxSTC_B_OPERATOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_PREPROCESSOR", rt.make_int(wxSTC_B_PREPROCESSOR) },
#else
    { WXE_ATOM_define, "wxSTC_B_PREPROCESSOR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_STRING", rt.make_int(wxSTC_B_STRING) },
#else
    { WXE_ATOM_define, "wxSTC_B_STRING", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_KEYWORD", rt.make_int(wxSTC_B_KEYWORD) },
#else
    { WXE_ATOM_define, "wxSTC_B_KEYWORD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_NUMBER", rt.make_int(wxSTC_B_NUMBER) },
#else
    { WXE_ATOM_define, "wxSTC_B_NUMBER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_COMMENT", rt.make_int(wxSTC_B_COMMENT) },
#else
    { WXE_ATOM_define, "wxSTC_B_COMMENT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_B_DEFAULT", rt.make_int(wxSTC_B_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_B_DEFAULT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_RB_UPPER_BOUND", rt.make_int(wxSTC_RB_UPPER_BOUND) },
    { WXE_ATOM_define, "wxSTC_RB_STDERR", rt.make_int(wxSTC_RB_STDERR) },
    { WXE_ATOM_define, "wxSTC_RB_STDOUT", rt.make_int(wxSTC_RB_STDOUT) },
    { WXE_ATOM_define, "wxSTC_RB_STDIN", rt.make_int(wxSTC_RB_STDIN) },
    { WXE_ATOM_define, "wxSTC_RB_WORD_DEMOTED", rt.make_int(wxSTC_RB_WORD_DEMOTED) },
    { WXE_ATOM_define, "wxSTC_RB_STRING_QW", rt.make_int(wxSTC_RB_STRING_QW) },
    { WXE_ATOM_define, "wxSTC_RB_STRING_QR", rt.make_int(wxSTC_RB_STRING_QR) },
    { WXE_ATOM_define, "wxSTC_RB_STRING_QX", rt.make_int(wxSTC_RB_STRING_QX) },
    { WXE_ATOM_define, "wxSTC_RB_STRING_QQ", rt.make_int(wxSTC_RB_STRING_QQ) },
    { WXE_ATOM_define, "wxSTC_RB_STRING_Q", rt.make_int(wxSTC_RB_STRING_Q) },
    { WXE_ATOM_define, "wxSTC_RB_HERE_QX", rt.make_int(wxSTC_RB_HERE_QX) },
    { WXE_ATOM_define, "wxSTC_RB_HERE_QQ", rt.make_int(wxSTC_RB_HERE_QQ) },
    { WXE_ATOM_define, "wxSTC_RB_HERE_Q", rt.make_int(wxSTC_RB_HERE_Q) },
    { WXE_ATOM_define, "wxSTC_RB_HERE_DELIM", rt.make_int(wxSTC_RB_HERE_DELIM) },
    { WXE_ATOM_define, "wxSTC_RB_DATASECTION", rt.make_int(wxSTC_RB_DATASECTION) },
    { WXE_ATOM_define, "wxSTC_RB_BACKTICKS", rt.make_int(wxSTC_RB_BACKTICKS) },
    { WXE_ATOM_define, "wxSTC_RB_CLASS_VAR", rt.make_int(wxSTC_RB_CLASS_VAR) },
    { WXE_ATOM_define, "wxSTC_RB_INSTANCE_VAR", rt.make_int(wxSTC_RB_INSTANCE_VAR) },
    { WXE_ATOM_define, "wxSTC_RB_MODULE_NAME", rt.make_int(wxSTC_RB_MODULE_NAME) },
    { WXE_ATOM_define, "wxSTC_RB_SYMBOL", rt.make_int(wxSTC_RB_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_RB_GLOBAL", rt.make_int(wxSTC_RB_GLOBAL) },
    { WXE_ATOM_define, "wxSTC_RB_REGEX", rt.make_int(wxSTC_RB_REGEX) },
    { WXE_ATOM_define, "wxSTC_RB_IDENTIFIER", rt.make_int(wxSTC_RB_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_RB_OPERATOR", rt.make_int(wxSTC_RB_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_RB_DEFNAME", rt.make_int(wxSTC_RB_DEFNAME) },
    { WXE_ATOM_define, "wxSTC_RB_CLASSNAME", rt.make_int(wxSTC_RB_CLASSNAME) },
    { WXE_ATOM_define, "wxSTC_RB_CHARACTER", rt.make_int(wxSTC_RB_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_RB_STRING", rt.make_int(wxSTC_RB_STRING) },
    { WXE_ATOM_define, "wxSTC_RB_WORD", rt.make_int(wxSTC_RB_WORD) },
    { WXE_ATOM_define, "wxSTC_RB_NUMBER", rt.make_int(wxSTC_RB_NUMBER) },
    { WXE_ATOM_define, "wxSTC_RB_POD", rt.make_int(wxSTC_RB_POD) },
    { WXE_ATOM_define, "wxSTC_RB_COMMENTLINE", rt.make_int(wxSTC_RB_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_RB_ERROR", rt.make_int(wxSTC_RB_ERROR) },
    { WXE_ATOM_define, "wxSTC_RB_DEFAULT", rt.make_int(wxSTC_RB_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QR_VAR", rt.make_int(wxSTC_PL_STRING_QR_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QX_VAR", rt.make_int(wxSTC_PL_STRING_QX_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QQ_VAR", rt.make_int(wxSTC_PL_STRING_QQ_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_HERE_QX_VAR", rt.make_int(wxSTC_PL_HERE_QX_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_HERE_QQ_VAR", rt.make_int(wxSTC_PL_HERE_QQ_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_BACKTICKS_VAR", rt.make_int(wxSTC_PL_BACKTICKS_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_REGSUBST_VAR", rt.make_int(wxSTC_PL_REGSUBST_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_REGEX_VAR", rt.make_int(wxSTC_PL_REGEX_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_XLAT", rt.make_int(wxSTC_PL_XLAT) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_VAR", rt.make_int(wxSTC_PL_STRING_VAR) },
    { WXE_ATOM_define, "wxSTC_PL_FORMAT", rt.make_int(wxSTC_PL_FORMAT) },
    { WXE_ATOM_define, "wxSTC_PL_FORMAT_IDENT", rt.make_int(wxSTC_PL_FORMAT_IDENT) },
    { WXE_ATOM_define, "wxSTC_PL_SUB_PROTOTYPE", rt.make_int(wxSTC_PL_SUB_PROTOTYPE) },
    { WXE_ATOM_define, "wxSTC_PL_POD_VERB", rt.make_int(wxSTC_PL_POD_VERB) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QW", rt.make_int(wxSTC_PL_STRING_QW) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QR", rt.make_int(wxSTC_PL_STRING_QR) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QX", rt.make_int(wxSTC_PL_STRING_QX) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_QQ", rt.make_int(wxSTC_PL_STRING_QQ) },
    { WXE_ATOM_define, "wxSTC_PL_STRING_Q", rt.make_int(wxSTC_PL_STRING_Q) },
    { WXE_ATOM_define, "wxSTC_PL_HERE_QX", rt.make_int(wxSTC_PL_HERE_QX) },
    { WXE_ATOM_define, "wxSTC_PL_HERE_QQ", rt.make_int(wxSTC_PL_HERE_QQ) },
    { WXE_ATOM_define, "wxSTC_PL_HERE_Q", rt.make_int(wxSTC_PL_HERE_Q) },
    { WXE_ATOM_define, "wxSTC_PL_HERE_DELIM", rt.make_int(wxSTC_PL_HERE_DELIM) },
    { WXE_ATOM_define, "wxSTC_PL_DATASECTION", rt.make_int(wxSTC_PL_DATASECTION) },
    { WXE_ATOM_define, "wxSTC_PL_BACKTICKS", rt.make_int(wxSTC_PL_BACKTICKS) },
    { WXE_ATOM_define, "wxSTC_PL_LONGQUOTE", rt.make_int(wxSTC_PL_LONGQUOTE) },
    { WXE_ATOM_define, "wxSTC_PL_REGSUBST", rt.make_int(wxSTC_PL_REGSUBST) },
    { WXE_ATOM_define, "wxSTC_PL_REGEX", rt.make_int(wxSTC_PL_REGEX) },
    { WXE_ATOM_define, "wxSTC_PL_VARIABLE_INDEXER", rt.make_int(wxSTC_PL_VARIABLE_INDEXER) },
    { WXE_ATOM_define, "wxSTC_PL_SYMBOLTABLE", rt.make_int(wxSTC_PL_SYMBOLTABLE) },
    { WXE_ATOM_define, "wxSTC_PL_HASH", rt.make_int(wxSTC_PL_HASH) },
    { WXE_ATOM_define, "wxSTC_PL_ARRAY", rt.make_int(wxSTC_PL_ARRAY) },
    { WXE_ATOM_define, "wxSTC_PL_SCALAR", rt.make_int(wxSTC_PL_SCALAR) },
    { WXE_ATOM_define, "wxSTC_PL_IDENTIFIER", rt.make_int(wxSTC_PL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_PL_OPERATOR", rt.make_int(wxSTC_PL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_PL_PREPROCESSOR", rt.make_int(wxSTC_PL_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_PL_PUNCTUATION", rt.make_int(wxSTC_PL_PUNCTUATION) },
    { WXE_ATOM_define, "wxSTC_PL_CHARACTER", rt.make_int(wxSTC_PL_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_PL_STRING", rt.make_int(wxSTC_PL_STRING) },
    { WXE_ATOM_define, "wxSTC_PL_WORD", rt.make_int(wxSTC_PL_WORD) },
    { WXE_ATOM_define, "wxSTC_PL_NUMBER", rt.make_int(wxSTC_PL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_PL_POD", rt.make_int(wxSTC_PL_POD) },
    { WXE_ATOM_define, "wxSTC_PL_COMMENTLINE", rt.make_int(wxSTC_PL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_PL_ERROR", rt.make_int(wxSTC_PL_ERROR) },
    { WXE_ATOM_define, "wxSTC_PL_DEFAULT", rt.make_int(wxSTC_PL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HPHP_OPERATOR", rt.make_int(wxSTC_HPHP_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_HPHP_HSTRING_VARIABLE", rt.make_int(wxSTC_HPHP_HSTRING_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_HPHP_COMMENTLINE", rt.make_int(wxSTC_HPHP_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HPHP_COMMENT", rt.make_int(wxSTC_HPHP_COMMENT) },
    { WXE_ATOM_define, "wxSTC_HPHP_VARIABLE", rt.make_int(wxSTC_HPHP_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_HPHP_NUMBER", rt.make_int(wxSTC_HPHP_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HPHP_WORD", rt.make_int(wxSTC_HPHP_WORD) },
    { WXE_ATOM_define, "wxSTC_HPHP_SIMPLESTRING", rt.make_int(wxSTC_HPHP_SIMPLESTRING) },
    { WXE_ATOM_define, "wxSTC_HPHP_HSTRING", rt.make_int(wxSTC_HPHP_HSTRING) },
    { WXE_ATOM_define, "wxSTC_HPHP_DEFAULT", rt.make_int(wxSTC_HPHP_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HPA_IDENTIFIER", rt.make_int(wxSTC_HPA_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_HPA_OPERATOR", rt.make_int(wxSTC_HPA_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_HPA_DEFNAME", rt.make_int(wxSTC_HPA_DEFNAME) },
    { WXE_ATOM_define, "wxSTC_HPA_CLASSNAME", rt.make_int(wxSTC_HPA_CLASSNAME) },
    { WXE_ATOM_define, "wxSTC_HPA_TRIPLEDOUBLE", rt.make_int(wxSTC_HPA_TRIPLEDOUBLE) },
    { WXE_ATOM_define, "wxSTC_HPA_TRIPLE", rt.make_int(wxSTC_HPA_TRIPLE) },
    { WXE_ATOM_define, "wxSTC_HPA_WORD", rt.make_int(wxSTC_HPA_WORD) },
    { WXE_ATOM_define, "wxSTC_HPA_CHARACTER", rt.make_int(wxSTC_HPA_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_HPA_STRING", rt.make_int(wxSTC_HPA_STRING) },
    { WXE_ATOM_define, "wxSTC_HPA_NUMBER", rt.make_int(wxSTC_HPA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HPA_COMMENTLINE", rt.make_int(wxSTC_HPA_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HPA_DEFAULT", rt.make_int(wxSTC_HPA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HPA_START", rt.make_int(wxSTC_HPA_START) },
    { WXE_ATOM_define, "wxSTC_HPHP_COMPLEX_VARIABLE", rt.make_int(wxSTC_HPHP_COMPLEX_VARIABLE) },
    { WXE_ATOM_define, "wxSTC_HP_IDENTIFIER", rt.make_int(wxSTC_HP_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_HP_OPERATOR", rt.make_int(wxSTC_HP_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_HP_DEFNAME", rt.make_int(wxSTC_HP_DEFNAME) },
    { WXE_ATOM_define, "wxSTC_HP_CLASSNAME", rt.make_int(wxSTC_HP_CLASSNAME) },
    { WXE_ATOM_define, "wxSTC_HP_TRIPLEDOUBLE", rt.make_int(wxSTC_HP_TRIPLEDOUBLE) },
    { WXE_ATOM_define, "wxSTC_HP_TRIPLE", rt.make_int(wxSTC_HP_TRIPLE) },
    { WXE_ATOM_define, "wxSTC_HP_WORD", rt.make_int(wxSTC_HP_WORD) },
    { WXE_ATOM_define, "wxSTC_HP_CHARACTER", rt.make_int(wxSTC_HP_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_HP_STRING", rt.make_int(wxSTC_HP_STRING) },
    { WXE_ATOM_define, "wxSTC_HP_NUMBER", rt.make_int(wxSTC_HP_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HP_COMMENTLINE", rt.make_int(wxSTC_HP_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HP_DEFAULT", rt.make_int(wxSTC_HP_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HP_START", rt.make_int(wxSTC_HP_START) },
    { WXE_ATOM_define, "wxSTC_HBA_STRINGEOL", rt.make_int(wxSTC_HBA_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_HBA_IDENTIFIER", rt.make_int(wxSTC_HBA_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_HBA_STRING", rt.make_int(wxSTC_HBA_STRING) },
    { WXE_ATOM_define, "wxSTC_HBA_WORD", rt.make_int(wxSTC_HBA_WORD) },
    { WXE_ATOM_define, "wxSTC_HBA_NUMBER", rt.make_int(wxSTC_HBA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HBA_COMMENTLINE", rt.make_int(wxSTC_HBA_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HBA_DEFAULT", rt.make_int(wxSTC_HBA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HBA_START", rt.make_int(wxSTC_HBA_START) },
    { WXE_ATOM_define, "wxSTC_HB_STRINGEOL", rt.make_int(wxSTC_HB_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_HB_IDENTIFIER", rt.make_int(wxSTC_HB_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_HB_STRING", rt.make_int(wxSTC_HB_STRING) },
    { WXE_ATOM_define, "wxSTC_HB_WORD", rt.make_int(wxSTC_HB_WORD) },
    { WXE_ATOM_define, "wxSTC_HB_NUMBER", rt.make_int(wxSTC_HB_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HB_COMMENTLINE", rt.make_int(wxSTC_HB_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HB_DEFAULT", rt.make_int(wxSTC_HB_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HB_START", rt.make_int(wxSTC_HB_START) },
    { WXE_ATOM_define, "wxSTC_HJA_REGEX", rt.make_int(wxSTC_HJA_REGEX) },
    { WXE_ATOM_define, "wxSTC_HJA_STRINGEOL", rt.make_int(wxSTC_HJA_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_HJA_SYMBOLS", rt.make_int(wxSTC_HJA_SYMBOLS) },
    { WXE_ATOM_define, "wxSTC_HJA_SINGLESTRING", rt.make_int(wxSTC_HJA_SINGLESTRING) },
    { WXE_ATOM_define, "wxSTC_HJA_DOUBLESTRING", rt.make_int(wxSTC_HJA_DOUBLESTRING) },
    { WXE_ATOM_define, "wxSTC_HJA_KEYWORD", rt.make_int(wxSTC_HJA_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_HJA_WORD", rt.make_int(wxSTC_HJA_WORD) },
    { WXE_ATOM_define, "wxSTC_HJA_NUMBER", rt.make_int(wxSTC_HJA_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HJA_COMMENTDOC", rt.make_int(wxSTC_HJA_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_HJA_COMMENTLINE", rt.make_int(wxSTC_HJA_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HJA_COMMENT", rt.make_int(wxSTC_HJA_COMMENT) },
    { WXE_ATOM_define, "wxSTC_HJA_DEFAULT", rt.make_int(wxSTC_HJA_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HJA_START", rt.make_int(wxSTC_HJA_START) },
    { WXE_ATOM_define, "wxSTC_HJ_REGEX", rt.make_int(wxSTC_HJ_REGEX) },
    { WXE_ATOM_define, "wxSTC_HJ_STRINGEOL", rt.make_int(wxSTC_HJ_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_HJ_SYMBOLS", rt.make_int(wxSTC_HJ_SYMBOLS) },
    { WXE_ATOM_define, "wxSTC_HJ_SINGLESTRING", rt.make_int(wxSTC_HJ_SINGLESTRING) },
    { WXE_ATOM_define, "wxSTC_HJ_DOUBLESTRING", rt.make_int(wxSTC_HJ_DOUBLESTRING) },
    { WXE_ATOM_define, "wxSTC_HJ_KEYWORD", rt.make_int(wxSTC_HJ_KEYWORD) },
    { WXE_ATOM_define, "wxSTC_HJ_WORD", rt.make_int(wxSTC_HJ_WORD) },
    { WXE_ATOM_define, "wxSTC_HJ_NUMBER", rt.make_int(wxSTC_HJ_NUMBER) },
    { WXE_ATOM_define, "wxSTC_HJ_COMMENTDOC", rt.make_int(wxSTC_HJ_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_HJ_COMMENTLINE", rt.make_int(wxSTC_HJ_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_HJ_COMMENT", rt.make_int(wxSTC_HJ_COMMENT) },
    { WXE_ATOM_define, "wxSTC_HJ_DEFAULT", rt.make_int(wxSTC_HJ_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_HJ_START", rt.make_int(wxSTC_HJ_START) },
    { WXE_ATOM_define, "wxSTC_H_SGML_BLOCK_DEFAULT", rt.make_int(wxSTC_H_SGML_BLOCK_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_H_SGML_1ST_PARAM_COMMENT", rt.make_int(wxSTC_H_SGML_1ST_PARAM_COMMENT) },
    { WXE_ATOM_define, "wxSTC_H_SGML_COMMENT", rt.make_int(wxSTC_H_SGML_COMMENT) },
    { WXE_ATOM_define, "wxSTC_H_SGML_ENTITY", rt.make_int(wxSTC_H_SGML_ENTITY) },
    { WXE_ATOM_define, "wxSTC_H_SGML_SPECIAL", rt.make_int(wxSTC_H_SGML_SPECIAL) },
    { WXE_ATOM_define, "wxSTC_H_SGML_ERROR", rt.make_int(wxSTC_H_SGML_ERROR) },
    { WXE_ATOM_define, "wxSTC_H_SGML_SIMPLESTRING", rt.make_int(wxSTC_H_SGML_SIMPLESTRING) },
    { WXE_ATOM_define, "wxSTC_H_SGML_DOUBLESTRING", rt.make_int(wxSTC_H_SGML_DOUBLESTRING) },
    { WXE_ATOM_define, "wxSTC_H_SGML_1ST_PARAM", rt.make_int(wxSTC_H_SGML_1ST_PARAM) },
    { WXE_ATOM_define, "wxSTC_H_SGML_COMMAND", rt.make_int(wxSTC_H_SGML_COMMAND) },
    { WXE_ATOM_define, "wxSTC_H_SGML_DEFAULT", rt.make_int(wxSTC_H_SGML_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_H_XCCOMMENT", rt.make_int(wxSTC_H_XCCOMMENT) },
    { WXE_ATOM_define, "wxSTC_H_VALUE", rt.make_int(wxSTC_H_VALUE) },
    { WXE_ATOM_define, "wxSTC_H_QUESTION", rt.make_int(wxSTC_H_QUESTION) },
    { WXE_ATOM_define, "wxSTC_H_CDATA", rt.make_int(wxSTC_H_CDATA) },
    { WXE_ATOM_define, "wxSTC_H_ASPAT", rt.make_int(wxSTC_H_ASPAT) },
    { WXE_ATOM_define, "wxSTC_H_ASP", rt.make_int(wxSTC_H_ASP) },
    { WXE_ATOM_define, "wxSTC_H_SCRIPT", rt.make_int(wxSTC_H_SCRIPT) },
    { WXE_ATOM_define, "wxSTC_H_XMLEND", rt.make_int(wxSTC_H_XMLEND) },
    { WXE_ATOM_define, "wxSTC_H_XMLSTART", rt.make_int(wxSTC_H_XMLSTART) },
    { WXE_ATOM_define, "wxSTC_H_TAGEND", rt.make_int(wxSTC_H_TAGEND) },
    { WXE_ATOM_define, "wxSTC_H_ENTITY", rt.make_int(wxSTC_H_ENTITY) },
    { WXE_ATOM_define, "wxSTC_H_COMMENT", rt.make_int(wxSTC_H_COMMENT) },
    { WXE_ATOM_define, "wxSTC_H_OTHER", rt.make_int(wxSTC_H_OTHER) },
    { WXE_ATOM_define, "wxSTC_H_SINGLESTRING", rt.make_int(wxSTC_H_SINGLESTRING) },
    { WXE_ATOM_define, "wxSTC_H_DOUBLESTRING", rt.make_int(wxSTC_H_DOUBLESTRING) },
    { WXE_ATOM_define, "wxSTC_H_NUMBER", rt.make_int(wxSTC_H_NUMBER) },
    { WXE_ATOM_define, "wxSTC_H_ATTRIBUTEUNKNOWN", rt.make_int(wxSTC_H_ATTRIBUTEUNKNOWN) },
    { WXE_ATOM_define, "wxSTC_H_ATTRIBUTE", rt.make_int(wxSTC_H_ATTRIBUTE) },
    { WXE_ATOM_define, "wxSTC_H_TAGUNKNOWN", rt.make_int(wxSTC_H_TAGUNKNOWN) },
    { WXE_ATOM_define, "wxSTC_H_TAG", rt.make_int(wxSTC_H_TAG) },
    { WXE_ATOM_define, "wxSTC_H_DEFAULT", rt.make_int(wxSTC_H_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_TCL_BLOCK_COMMENT", rt.make_int(wxSTC_TCL_BLOCK_COMMENT) },
    { WXE_ATOM_define, "wxSTC_TCL_COMMENT_BOX", rt.make_int(wxSTC_TCL_COMMENT_BOX) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD8", rt.make_int(wxSTC_TCL_WORD8) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD7", rt.make_int(wxSTC_TCL_WORD7) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD6", rt.make_int(wxSTC_TCL_WORD6) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD5", rt.make_int(wxSTC_TCL_WORD5) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD4", rt.make_int(wxSTC_TCL_WORD4) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD3", rt.make_int(wxSTC_TCL_WORD3) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD2", rt.make_int(wxSTC_TCL_WORD2) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD", rt.make_int(wxSTC_TCL_WORD) },
    { WXE_ATOM_define, "wxSTC_TCL_EXPAND", rt.make_int(wxSTC_TCL_EXPAND) },
    { WXE_ATOM_define, "wxSTC_TCL_MODIFIER", rt.make_int(wxSTC_TCL_MODIFIER) },
    { WXE_ATOM_define, "wxSTC_TCL_SUB_BRACE", rt.make_int(wxSTC_TCL_SUB_BRACE) },
    { WXE_ATOM_define, "wxSTC_TCL_SUBSTITUTION", rt.make_int(wxSTC_TCL_SUBSTITUTION) },
    { WXE_ATOM_define, "wxSTC_TCL_IDENTIFIER", rt.make_int(wxSTC_TCL_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_TCL_OPERATOR", rt.make_int(wxSTC_TCL_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_TCL_IN_QUOTE", rt.make_int(wxSTC_TCL_IN_QUOTE) },
    { WXE_ATOM_define, "wxSTC_TCL_WORD_IN_QUOTE", rt.make_int(wxSTC_TCL_WORD_IN_QUOTE) },
    { WXE_ATOM_define, "wxSTC_TCL_NUMBER", rt.make_int(wxSTC_TCL_NUMBER) },
    { WXE_ATOM_define, "wxSTC_TCL_COMMENTLINE", rt.make_int(wxSTC_TCL_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_TCL_COMMENT", rt.make_int(wxSTC_TCL_COMMENT) },
    { WXE_ATOM_define, "wxSTC_TCL_DEFAULT", rt.make_int(wxSTC_TCL_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_D_WORD7", rt.make_int(wxSTC_D_WORD7) },
    { WXE_ATOM_define, "wxSTC_D_WORD6", rt.make_int(wxSTC_D_WORD6) },
    { WXE_ATOM_define, "wxSTC_D_WORD5", rt.make_int(wxSTC_D_WORD5) },
    { WXE_ATOM_define, "wxSTC_D_STRINGR", rt.make_int(wxSTC_D_STRINGR) },
    { WXE_ATOM_define, "wxSTC_D_STRINGB", rt.make_int(wxSTC_D_STRINGB) },
    { WXE_ATOM_define, "wxSTC_D_COMMENTDOCKEYWORDERROR", rt.make_int(wxSTC_D_COMMENTDOCKEYWORDERROR) },
    { WXE_ATOM_define, "wxSTC_D_COMMENTDOCKEYWORD", rt.make_int(wxSTC_D_COMMENTDOCKEYWORD) },
    { WXE_ATOM_define, "wxSTC_D_COMMENTLINEDOC", rt.make_int(wxSTC_D_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_D_IDENTIFIER", rt.make_int(wxSTC_D_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_D_OPERATOR", rt.make_int(wxSTC_D_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_D_CHARACTER", rt.make_int(wxSTC_D_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_D_STRINGEOL", rt.make_int(wxSTC_D_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_D_STRING", rt.make_int(wxSTC_D_STRING) },
    { WXE_ATOM_define, "wxSTC_D_TYPEDEF", rt.make_int(wxSTC_D_TYPEDEF) },
    { WXE_ATOM_define, "wxSTC_D_WORD3", rt.make_int(wxSTC_D_WORD3) },
    { WXE_ATOM_define, "wxSTC_D_WORD2", rt.make_int(wxSTC_D_WORD2) },
    { WXE_ATOM_define, "wxSTC_D_WORD", rt.make_int(wxSTC_D_WORD) },
    { WXE_ATOM_define, "wxSTC_D_NUMBER", rt.make_int(wxSTC_D_NUMBER) },
    { WXE_ATOM_define, "wxSTC_D_COMMENTNESTED", rt.make_int(wxSTC_D_COMMENTNESTED) },
    { WXE_ATOM_define, "wxSTC_D_COMMENTDOC", rt.make_int(wxSTC_D_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_D_COMMENTLINE", rt.make_int(wxSTC_D_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_D_COMMENT", rt.make_int(wxSTC_D_COMMENT) },
    { WXE_ATOM_define, "wxSTC_D_DEFAULT", rt.make_int(wxSTC_D_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_C_ESCAPESEQUENCE", rt.make_int(wxSTC_C_ESCAPESEQUENCE) },
#else
    { WXE_ATOM_define, "wxSTC_C_ESCAPESEQUENCE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_C_TASKMARKER", rt.make_int(wxSTC_C_TASKMARKER) },
#else
    { WXE_ATOM_define, "wxSTC_C_TASKMARKER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_C_USERLITERAL", rt.make_int(wxSTC_C_USERLITERAL) },
#else
    { WXE_ATOM_define, "wxSTC_C_USERLITERAL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_C_PREPROCESSORCOMMENTDOC", rt.make_int(wxSTC_C_PREPROCESSORCOMMENTDOC) },
#else
    { WXE_ATOM_define, "wxSTC_C_PREPROCESSORCOMMENTDOC", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_C_PREPROCESSORCOMMENT", rt.make_int(wxSTC_C_PREPROCESSORCOMMENT) },
    { WXE_ATOM_define, "wxSTC_C_HASHQUOTEDSTRING", rt.make_int(wxSTC_C_HASHQUOTEDSTRING) },
    { WXE_ATOM_define, "wxSTC_C_TRIPLEVERBATIM", rt.make_int(wxSTC_C_TRIPLEVERBATIM) },
    { WXE_ATOM_define, "wxSTC_C_STRINGRAW", rt.make_int(wxSTC_C_STRINGRAW) },
    { WXE_ATOM_define, "wxSTC_C_GLOBALCLASS", rt.make_int(wxSTC_C_GLOBALCLASS) },
    { WXE_ATOM_define, "wxSTC_C_COMMENTDOCKEYWORDERROR", rt.make_int(wxSTC_C_COMMENTDOCKEYWORDERROR) },
    { WXE_ATOM_define, "wxSTC_C_COMMENTDOCKEYWORD", rt.make_int(wxSTC_C_COMMENTDOCKEYWORD) },
    { WXE_ATOM_define, "wxSTC_C_WORD2", rt.make_int(wxSTC_C_WORD2) },
    { WXE_ATOM_define, "wxSTC_C_COMMENTLINEDOC", rt.make_int(wxSTC_C_COMMENTLINEDOC) },
    { WXE_ATOM_define, "wxSTC_C_REGEX", rt.make_int(wxSTC_C_REGEX) },
    { WXE_ATOM_define, "wxSTC_C_VERBATIM", rt.make_int(wxSTC_C_VERBATIM) },
    { WXE_ATOM_define, "wxSTC_C_STRINGEOL", rt.make_int(wxSTC_C_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_C_IDENTIFIER", rt.make_int(wxSTC_C_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_C_OPERATOR", rt.make_int(wxSTC_C_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_C_PREPROCESSOR", rt.make_int(wxSTC_C_PREPROCESSOR) },
    { WXE_ATOM_define, "wxSTC_C_UUID", rt.make_int(wxSTC_C_UUID) },
    { WXE_ATOM_define, "wxSTC_C_CHARACTER", rt.make_int(wxSTC_C_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_C_STRING", rt.make_int(wxSTC_C_STRING) },
    { WXE_ATOM_define, "wxSTC_C_WORD", rt.make_int(wxSTC_C_WORD) },
    { WXE_ATOM_define, "wxSTC_C_NUMBER", rt.make_int(wxSTC_C_NUMBER) },
    { WXE_ATOM_define, "wxSTC_C_COMMENTDOC", rt.make_int(wxSTC_C_COMMENTDOC) },
    { WXE_ATOM_define, "wxSTC_C_COMMENTLINE", rt.make_int(wxSTC_C_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_C_COMMENT", rt.make_int(wxSTC_C_COMMENT) },
    { WXE_ATOM_define, "wxSTC_C_DEFAULT", rt.make_int(wxSTC_C_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_P_DECORATOR", rt.make_int(wxSTC_P_DECORATOR) },
    { WXE_ATOM_define, "wxSTC_P_WORD2", rt.make_int(wxSTC_P_WORD2) },
    { WXE_ATOM_define, "wxSTC_P_STRINGEOL", rt.make_int(wxSTC_P_STRINGEOL) },
    { WXE_ATOM_define, "wxSTC_P_COMMENTBLOCK", rt.make_int(wxSTC_P_COMMENTBLOCK) },
    { WXE_ATOM_define, "wxSTC_P_IDENTIFIER", rt.make_int(wxSTC_P_IDENTIFIER) },
    { WXE_ATOM_define, "wxSTC_P_OPERATOR", rt.make_int(wxSTC_P_OPERATOR) },
    { WXE_ATOM_define, "wxSTC_P_DEFNAME", rt.make_int(wxSTC_P_DEFNAME) },
    { WXE_ATOM_define, "wxSTC_P_CLASSNAME", rt.make_int(wxSTC_P_CLASSNAME) },
    { WXE_ATOM_define, "wxSTC_P_TRIPLEDOUBLE", rt.make_int(wxSTC_P_TRIPLEDOUBLE) },
    { WXE_ATOM_define, "wxSTC_P_TRIPLE", rt.make_int(wxSTC_P_TRIPLE) },
    { WXE_ATOM_define, "wxSTC_P_WORD", rt.make_int(wxSTC_P_WORD) },
    { WXE_ATOM_define, "wxSTC_P_CHARACTER", rt.make_int(wxSTC_P_CHARACTER) },
    { WXE_ATOM_define, "wxSTC_P_STRING", rt.make_int(wxSTC_P_STRING) },
    { WXE_ATOM_define, "wxSTC_P_NUMBER", rt.make_int(wxSTC_P_NUMBER) },
    { WXE_ATOM_define, "wxSTC_P_COMMENTLINE", rt.make_int(wxSTC_P_COMMENTLINE) },
    { WXE_ATOM_define, "wxSTC_P_DEFAULT", rt.make_int(wxSTC_P_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_AUTOMATIC", rt.make_int(wxSTC_LEX_AUTOMATIC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_AUTOMATIC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_EDIFACT", rt.make_int(wxSTC_LEX_EDIFACT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_EDIFACT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_JSON", rt.make_int(wxSTC_LEX_JSON) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_JSON", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TEHEX", rt.make_int(wxSTC_LEX_TEHEX) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TEHEX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_IHEX", rt.make_int(wxSTC_LEX_IHEX) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_IHEX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SREC", rt.make_int(wxSTC_LEX_SREC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SREC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_BIBTEX", rt.make_int(wxSTC_LEX_BIBTEX) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_BIBTEX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_REGISTRY", rt.make_int(wxSTC_LEX_REGISTRY) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_REGISTRY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_DMIS", rt.make_int(wxSTC_LEX_DMIS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_DMIS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_AS", rt.make_int(wxSTC_LEX_AS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_AS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_DMAP", rt.make_int(wxSTC_LEX_DMAP) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_DMAP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_RUST", rt.make_int(wxSTC_LEX_RUST) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_RUST", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_KVIRC", rt.make_int(wxSTC_LEX_KVIRC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_KVIRC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_STTXT", rt.make_int(wxSTC_LEX_STTXT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_STTXT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_LITERATEHASKELL", rt.make_int(wxSTC_LEX_LITERATEHASKELL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_LITERATEHASKELL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_VISUALPROLOG", rt.make_int(wxSTC_LEX_VISUALPROLOG) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_VISUALPROLOG", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_OSCRIPT", rt.make_int(wxSTC_LEX_OSCRIPT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_OSCRIPT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ECL", rt.make_int(wxSTC_LEX_ECL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ECL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_AVS", rt.make_int(wxSTC_LEX_AVS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_AVS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TCMD", rt.make_int(wxSTC_LEX_TCMD) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TCMD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_COFFEESCRIPT", rt.make_int(wxSTC_LEX_COFFEESCRIPT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_COFFEESCRIPT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MODULA", rt.make_int(wxSTC_LEX_MODULA) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MODULA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_A68K", rt.make_int(wxSTC_LEX_A68K) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_A68K", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TXT2TAGS", rt.make_int(wxSTC_LEX_TXT2TAGS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TXT2TAGS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MARKDOWN", rt.make_int(wxSTC_LEX_MARKDOWN) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MARKDOWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SML", rt.make_int(wxSTC_LEX_SML) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SML", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_NIMROD", rt.make_int(wxSTC_LEX_NIMROD) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_NIMROD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_POWERPRO", rt.make_int(wxSTC_LEX_POWERPRO) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_POWERPRO", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SORCUS", rt.make_int(wxSTC_LEX_SORCUS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SORCUS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TACL", rt.make_int(wxSTC_LEX_TACL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TACL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_COBOL", rt.make_int(wxSTC_LEX_COBOL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_COBOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TAL", rt.make_int(wxSTC_LEX_TAL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TAL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PO", rt.make_int(wxSTC_LEX_PO) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PO", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MYSQL", rt.make_int(wxSTC_LEX_MYSQL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MYSQL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_POWERSHELL", rt.make_int(wxSTC_LEX_POWERSHELL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_POWERSHELL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MAGIK", rt.make_int(wxSTC_LEX_MAGIK) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MAGIK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_R", rt.make_int(wxSTC_LEX_R) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_R", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ASYMPTOTE", rt.make_int(wxSTC_LEX_ASYMPTOTE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ASYMPTOTE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ABAQUS", rt.make_int(wxSTC_LEX_ABAQUS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ABAQUS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PROGRESS", rt.make_int(wxSTC_LEX_PROGRESS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PROGRESS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PLM", rt.make_int(wxSTC_LEX_PLM) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PLM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_GAP", rt.make_int(wxSTC_LEX_GAP) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_GAP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CMAKE", rt.make_int(wxSTC_LEX_CMAKE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CMAKE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_D", rt.make_int(wxSTC_LEX_D) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_D", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SPICE", rt.make_int(wxSTC_LEX_SPICE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SPICE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_OPAL", rt.make_int(wxSTC_LEX_OPAL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_OPAL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_INNOSETUP", rt.make_int(wxSTC_LEX_INNOSETUP) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_INNOSETUP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_FREEBASIC", rt.make_int(wxSTC_LEX_FREEBASIC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_FREEBASIC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CSOUND", rt.make_int(wxSTC_LEX_CSOUND) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CSOUND", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_FLAGSHIP", rt.make_int(wxSTC_LEX_FLAGSHIP) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_FLAGSHIP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SMALLTALK", rt.make_int(wxSTC_LEX_SMALLTALK) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SMALLTALK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_REBOL", rt.make_int(wxSTC_LEX_REBOL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_REBOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TADS3", rt.make_int(wxSTC_LEX_TADS3) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TADS3", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PHPSCRIPT", rt.make_int(wxSTC_LEX_PHPSCRIPT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PHPSCRIPT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_HASKELL", rt.make_int(wxSTC_LEX_HASKELL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_HASKELL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PUREBASIC", rt.make_int(wxSTC_LEX_PUREBASIC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PUREBASIC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_BLITZBASIC", rt.make_int(wxSTC_LEX_BLITZBASIC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_BLITZBASIC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CAML", rt.make_int(wxSTC_LEX_CAML) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CAML", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_VHDL", rt.make_int(wxSTC_LEX_VHDL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_VHDL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ASN1", rt.make_int(wxSTC_LEX_ASN1) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ASN1", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_BASH", rt.make_int(wxSTC_LEX_BASH) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_BASH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_APDL", rt.make_int(wxSTC_LEX_APDL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_APDL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_AU3", rt.make_int(wxSTC_LEX_AU3) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_AU3", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SPECMAN", rt.make_int(wxSTC_LEX_SPECMAN) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SPECMAN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_GUI4CLI", rt.make_int(wxSTC_LEX_GUI4CLI) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_GUI4CLI", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_KIX", rt.make_int(wxSTC_LEX_KIX) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_KIX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_VERILOG", rt.make_int(wxSTC_LEX_VERILOG) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_VERILOG", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MSSQL", rt.make_int(wxSTC_LEX_MSSQL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MSSQL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_OCTAVE", rt.make_int(wxSTC_LEX_OCTAVE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_OCTAVE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ERLANG", rt.make_int(wxSTC_LEX_ERLANG) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ERLANG", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_FORTH", rt.make_int(wxSTC_LEX_FORTH) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_FORTH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_POWERBASIC", rt.make_int(wxSTC_LEX_POWERBASIC) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_POWERBASIC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_METAPOST", rt.make_int(wxSTC_LEX_METAPOST) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_METAPOST", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TEX", rt.make_int(wxSTC_LEX_TEX) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TEX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_YAML", rt.make_int(wxSTC_LEX_YAML) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_YAML", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_LOT", rt.make_int(wxSTC_LEX_LOT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_LOT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CLWNOCASE", rt.make_int(wxSTC_LEX_CLWNOCASE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CLWNOCASE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CLW", rt.make_int(wxSTC_LEX_CLW) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CLW", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MMIXAL", rt.make_int(wxSTC_LEX_MMIXAL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MMIXAL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_NSIS", rt.make_int(wxSTC_LEX_NSIS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_NSIS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PS", rt.make_int(wxSTC_LEX_PS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ESCRIPT", rt.make_int(wxSTC_LEX_ESCRIPT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ESCRIPT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_LOUT", rt.make_int(wxSTC_LEX_LOUT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_LOUT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_POV", rt.make_int(wxSTC_LEX_POV) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_POV", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CSS", rt.make_int(wxSTC_LEX_CSS) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CSS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_F77", rt.make_int(wxSTC_LEX_F77) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_F77", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_FORTRAN", rt.make_int(wxSTC_LEX_FORTRAN) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_FORTRAN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CPPNOCASE", rt.make_int(wxSTC_LEX_CPPNOCASE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CPPNOCASE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ASM", rt.make_int(wxSTC_LEX_ASM) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ASM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SCRIPTOL", rt.make_int(wxSTC_LEX_SCRIPTOL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SCRIPTOL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MATLAB", rt.make_int(wxSTC_LEX_MATLAB) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MATLAB", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_BAAN", rt.make_int(wxSTC_LEX_BAAN) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_BAAN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_VBSCRIPT", rt.make_int(wxSTC_LEX_VBSCRIPT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_VBSCRIPT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_BULLANT", rt.make_int(wxSTC_LEX_BULLANT) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_BULLANT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_NNCRONTAB", rt.make_int(wxSTC_LEX_NNCRONTAB) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_NNCRONTAB", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_TCL", rt.make_int(wxSTC_LEX_TCL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_TCL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_EIFFELKW", rt.make_int(wxSTC_LEX_EIFFELKW) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_EIFFELKW", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_EIFFEL", rt.make_int(wxSTC_LEX_EIFFEL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_EIFFEL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_RUBY", rt.make_int(wxSTC_LEX_RUBY) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_RUBY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_LISP", rt.make_int(wxSTC_LEX_LISP) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_LISP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ADA", rt.make_int(wxSTC_LEX_ADA) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ADA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_AVE", rt.make_int(wxSTC_LEX_AVE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_AVE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PASCAL", rt.make_int(wxSTC_LEX_PASCAL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PASCAL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CONF", rt.make_int(wxSTC_LEX_CONF) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CONF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_DIFF", rt.make_int(wxSTC_LEX_DIFF) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_DIFF", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_LUA", rt.make_int(wxSTC_LEX_LUA) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_LUA", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_LATEX", rt.make_int(wxSTC_LEX_LATEX) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_LATEX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_XCODE", rt.make_int(wxSTC_LEX_XCODE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_XCODE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_BATCH", rt.make_int(wxSTC_LEX_BATCH) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_BATCH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_MAKEFILE", rt.make_int(wxSTC_LEX_MAKEFILE) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_MAKEFILE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_ERRORLIST", rt.make_int(wxSTC_LEX_ERRORLIST) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_ERRORLIST", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PROPERTIES", rt.make_int(wxSTC_LEX_PROPERTIES) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PROPERTIES", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_VB", rt.make_int(wxSTC_LEX_VB) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_VB", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_SQL", rt.make_int(wxSTC_LEX_SQL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_SQL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PERL", rt.make_int(wxSTC_LEX_PERL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PERL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_XML", rt.make_int(wxSTC_LEX_XML) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_XML", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_HTML", rt.make_int(wxSTC_LEX_HTML) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_HTML", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CPP", rt.make_int(wxSTC_LEX_CPP) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CPP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_PYTHON", rt.make_int(wxSTC_LEX_PYTHON) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_PYTHON", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_NULL", rt.make_int(wxSTC_LEX_NULL) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_NULL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LEX_CONTAINER", rt.make_int(wxSTC_LEX_CONTAINER) },
#else
    { WXE_ATOM_define, "wxSTC_LEX_CONTAINER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AC_COMMAND", rt.make_int(wxSTC_AC_COMMAND) },
#else
    { WXE_ATOM_define, "wxSTC_AC_COMMAND", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AC_NEWLINE", rt.make_int(wxSTC_AC_NEWLINE) },
#else
    { WXE_ATOM_define, "wxSTC_AC_NEWLINE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AC_TAB", rt.make_int(wxSTC_AC_TAB) },
#else
    { WXE_ATOM_define, "wxSTC_AC_TAB", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AC_DOUBLECLICK", rt.make_int(wxSTC_AC_DOUBLECLICK) },
#else
    { WXE_ATOM_define, "wxSTC_AC_DOUBLECLICK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AC_FILLUP", rt.make_int(wxSTC_AC_FILLUP) },
#else
    { WXE_ATOM_define, "wxSTC_AC_FILLUP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KEYMOD_META", rt.make_int(wxSTC_KEYMOD_META) },
#else
    { WXE_ATOM_define, "wxSTC_KEYMOD_META", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KEYMOD_SUPER", rt.make_int(wxSTC_KEYMOD_SUPER) },
#else
    { WXE_ATOM_define, "wxSTC_KEYMOD_SUPER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KEYMOD_ALT", rt.make_int(wxSTC_KEYMOD_ALT) },
#else
    { WXE_ATOM_define, "wxSTC_KEYMOD_ALT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KEYMOD_CTRL", rt.make_int(wxSTC_KEYMOD_CTRL) },
#else
    { WXE_ATOM_define, "wxSTC_KEYMOD_CTRL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KEYMOD_SHIFT", rt.make_int(wxSTC_KEYMOD_SHIFT) },
#else
    { WXE_ATOM_define, "wxSTC_KEYMOD_SHIFT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_KEYMOD_NORM", rt.make_int(wxSTC_KEYMOD_NORM) },
#else
    { WXE_ATOM_define, "wxSTC_KEYMOD_NORM", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_KEY_MENU", rt.make_int(wxSTC_KEY_MENU) },
    { WXE_ATOM_define, "wxSTC_KEY_RWIN", rt.make_int(wxSTC_KEY_RWIN) },
    { WXE_ATOM_define, "wxSTC_KEY_WIN", rt.make_int(wxSTC_KEY_WIN) },
    { WXE_ATOM_define, "wxSTC_KEY_DIVIDE", rt.make_int(wxSTC_KEY_DIVIDE) },
    { WXE_ATOM_define, "wxSTC_KEY_SUBTRACT", rt.make_int(wxSTC_KEY_SUBTRACT) },
    { WXE_ATOM_define, "wxSTC_KEY_ADD", rt.make_int(wxSTC_KEY_ADD) },
    { WXE_ATOM_define, "wxSTC_KEY_RETURN", rt.make_int(wxSTC_KEY_RETURN) },
    { WXE_ATOM_define, "wxSTC_KEY_TAB", rt.make_int(wxSTC_KEY_TAB) },
    { WXE_ATOM_define, "wxSTC_KEY_BACK", rt.make_int(wxSTC_KEY_BACK) },
    { WXE_ATOM_define, "wxSTC_KEY_ESCAPE", rt.make_int(wxSTC_KEY_ESCAPE) },
    { WXE_ATOM_define, "wxSTC_KEY_INSERT", rt.make_int(wxSTC_KEY_INSERT) },
    { WXE_ATOM_define, "wxSTC_KEY_DELETE", rt.make_int(wxSTC_KEY_DELETE) },
    { WXE_ATOM_define, "wxSTC_KEY_NEXT", rt.make_int(wxSTC_KEY_NEXT) },
    { WXE_ATOM_define, "wxSTC_KEY_PRIOR", rt.make_int(wxSTC_KEY_PRIOR) },
    { WXE_ATOM_define, "wxSTC_KEY_END", rt.make_int(wxSTC_KEY_END) },
    { WXE_ATOM_define, "wxSTC_KEY_HOME", rt.make_int(wxSTC_KEY_HOME) },
    { WXE_ATOM_define, "wxSTC_KEY_RIGHT", rt.make_int(wxSTC_KEY_RIGHT) },
    { WXE_ATOM_define, "wxSTC_KEY_LEFT", rt.make_int(wxSTC_KEY_LEFT) },
    { WXE_ATOM_define, "wxSTC_KEY_UP", rt.make_int(wxSTC_KEY_UP) },
    { WXE_ATOM_define, "wxSTC_KEY_DOWN", rt.make_int(wxSTC_KEY_DOWN) },
    { WXE_ATOM_define, "wxSTC_UPDATE_H_SCROLL", rt.make_int(wxSTC_UPDATE_H_SCROLL) },
    { WXE_ATOM_define, "wxSTC_UPDATE_V_SCROLL", rt.make_int(wxSTC_UPDATE_V_SCROLL) },
    { WXE_ATOM_define, "wxSTC_UPDATE_SELECTION", rt.make_int(wxSTC_UPDATE_SELECTION) },
    { WXE_ATOM_define, "wxSTC_UPDATE_CONTENT", rt.make_int(wxSTC_UPDATE_CONTENT) },
    { WXE_ATOM_define, "wxSTC_MODEVENTMASKALL", rt.make_int(wxSTC_MODEVENTMASKALL) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MOD_CHANGETABSTOPS", rt.make_int(wxSTC_MOD_CHANGETABSTOPS) },
#else
    { WXE_ATOM_define, "wxSTC_MOD_CHANGETABSTOPS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MOD_INSERTCHECK", rt.make_int(wxSTC_MOD_INSERTCHECK) },
#else
    { WXE_ATOM_define, "wxSTC_MOD_INSERTCHECK", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_MOD_LEXERSTATE", rt.make_int(wxSTC_MOD_LEXERSTATE) },
    { WXE_ATOM_define, "wxSTC_MOD_CONTAINER", rt.make_int(wxSTC_MOD_CONTAINER) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGEANNOTATION", rt.make_int(wxSTC_MOD_CHANGEANNOTATION) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGEMARGIN", rt.make_int(wxSTC_MOD_CHANGEMARGIN) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGELINESTATE", rt.make_int(wxSTC_MOD_CHANGELINESTATE) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGEINDICATOR", rt.make_int(wxSTC_MOD_CHANGEINDICATOR) },
    { WXE_ATOM_define, "wxSTC_STARTACTION", rt.make_int(wxSTC_STARTACTION) },
    { WXE_ATOM_define, "wxSTC_MULTILINEUNDOREDO", rt.make_int(wxSTC_MULTILINEUNDOREDO) },
    { WXE_ATOM_define, "wxSTC_MOD_BEFOREDELETE", rt.make_int(wxSTC_MOD_BEFOREDELETE) },
    { WXE_ATOM_define, "wxSTC_MOD_BEFOREINSERT", rt.make_int(wxSTC_MOD_BEFOREINSERT) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGEMARKER", rt.make_int(wxSTC_MOD_CHANGEMARKER) },
    { WXE_ATOM_define, "wxSTC_LASTSTEPINUNDOREDO", rt.make_int(wxSTC_LASTSTEPINUNDOREDO) },
    { WXE_ATOM_define, "wxSTC_MULTISTEPUNDOREDO", rt.make_int(wxSTC_MULTISTEPUNDOREDO) },
    { WXE_ATOM_define, "wxSTC_PERFORMED_REDO", rt.make_int(wxSTC_PERFORMED_REDO) },
    { WXE_ATOM_define, "wxSTC_PERFORMED_UNDO", rt.make_int(wxSTC_PERFORMED_UNDO) },
    { WXE_ATOM_define, "wxSTC_PERFORMED_USER", rt.make_int(wxSTC_PERFORMED_USER) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGEFOLD", rt.make_int(wxSTC_MOD_CHANGEFOLD) },
    { WXE_ATOM_define, "wxSTC_MOD_CHANGESTYLE", rt.make_int(wxSTC_MOD_CHANGESTYLE) },
    { WXE_ATOM_define, "wxSTC_MOD_DELETETEXT", rt.make_int(wxSTC_MOD_DELETETEXT) },
    { WXE_ATOM_define, "wxSTC_MOD_INSERTTEXT", rt.make_int(wxSTC_MOD_INSERTTEXT) },
    { WXE_ATOM_define, "wxSTC_TYPE_STRING", rt.make_int(wxSTC_TYPE_STRING) },
    { WXE_ATOM_define, "wxSTC_TYPE_INTEGER", rt.make_int(wxSTC_TYPE_INTEGER) },
    { WXE_ATOM_define, "wxSTC_TYPE_BOOLEAN", rt.make_int(wxSTC_TYPE_BOOLEAN) },
    { WXE_ATOM_define, "wxSTC_KEYWORDSET_MAX", rt.make_int(wxSTC_KEYWORDSET_MAX) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LINE_END_TYPE_UNICODE", rt.make_int(wxSTC_LINE_END_TYPE_UNICODE) },
#else
    { WXE_ATOM_define, "wxSTC_LINE_END_TYPE_UNICODE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_LINE_END_TYPE_DEFAULT", rt.make_int(wxSTC_LINE_END_TYPE_DEFAULT) },
#else
    { WXE_ATOM_define, "wxSTC_LINE_END_TYPE_DEFAULT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_TECHNOLOGY_DIRECTWRITE", rt.make_int(wxSTC_TECHNOLOGY_DIRECTWRITE) },
    { WXE_ATOM_define, "wxSTC_TECHNOLOGY_DEFAULT", rt.make_int(wxSTC_TECHNOLOGY_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_VS_NOWRAPLINESTART", rt.make_int(wxSTC_VS_NOWRAPLINESTART) },
#else
    { WXE_ATOM_define, "wxSTC_VS_NOWRAPLINESTART", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_VS_USERACCESSIBLE", rt.make_int(wxSTC_VS_USERACCESSIBLE) },
#else
    { WXE_ATOM_define, "wxSTC_VS_USERACCESSIBLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_VS_RECTANGULARSELECTION", rt.make_int(wxSTC_VS_RECTANGULARSELECTION) },
#else
    { WXE_ATOM_define, "wxSTC_VS_RECTANGULARSELECTION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_VS_NONE", rt.make_int(wxSTC_VS_NONE) },
#else
    { WXE_ATOM_define, "wxSTC_VS_NONE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_UNDO_MAY_COALESCE", rt.make_int(wxSTC_UNDO_MAY_COALESCE) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ANNOTATION_INDENTED", rt.make_int(wxSTC_ANNOTATION_INDENTED) },
#else
    { WXE_ATOM_define, "wxSTC_ANNOTATION_INDENTED", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_ANNOTATION_BOXED", rt.make_int(wxSTC_ANNOTATION_BOXED) },
    { WXE_ATOM_define, "wxSTC_ANNOTATION_STANDARD", rt.make_int(wxSTC_ANNOTATION_STANDARD) },
    { WXE_ATOM_define, "wxSTC_ANNOTATION_HIDDEN", rt.make_int(wxSTC_ANNOTATION_HIDDEN) },
    { WXE_ATOM_define, "wxSTC_MARGINOPTION_SUBLINESELECT", rt.make_int(wxSTC_MARGINOPTION_SUBLINESELECT) },
    { WXE_ATOM_define, "wxSTC_MARGINOPTION_NONE", rt.make_int(wxSTC_MARGINOPTION_NONE) },
    { WXE_ATOM_define, "wxSTC_CARETSTYLE_BLOCK", rt.make_int(wxSTC_CARETSTYLE_BLOCK) },
    { WXE_ATOM_define, "wxSTC_CARETSTYLE_LINE", rt.make_int(wxSTC_CARETSTYLE_LINE) },
    { WXE_ATOM_define, "wxSTC_CARETSTYLE_INVISIBLE", rt.make_int(wxSTC_CARETSTYLE_INVISIBLE) },
    { WXE_ATOM_define, "wxSTC_ALPHA_NOALPHA", rt.make_int(wxSTC_ALPHA_NOALPHA) },
    { WXE_ATOM_define, "wxSTC_ALPHA_OPAQUE", rt.make_int(wxSTC_ALPHA_OPAQUE) },
    { WXE_ATOM_define, "wxSTC_ALPHA_TRANSPARENT", rt.make_int(wxSTC_ALPHA_TRANSPARENT) },
    { WXE_ATOM_define, "wxSTC_CARETSTICKY_WHITESPACE", rt.make_int(wxSTC_CARETSTICKY_WHITESPACE) },
    { WXE_ATOM_define, "wxSTC_CARETSTICKY_ON", rt.make_int(wxSTC_CARETSTICKY_ON) },
    { WXE_ATOM_define, "wxSTC_CARETSTICKY_OFF", rt.make_int(wxSTC_CARETSTICKY_OFF) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ORDER_CUSTOM", rt.make_int(wxSTC_ORDER_CUSTOM) },
#else
    { WXE_ATOM_define, "wxSTC_ORDER_CUSTOM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ORDER_PERFORMSORT", rt.make_int(wxSTC_ORDER_PERFORMSORT) },
#else
    { WXE_ATOM_define, "wxSTC_ORDER_PERFORMSORT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_ORDER_PRESORTED", rt.make_int(wxSTC_ORDER_PRESORTED) },
#else
    { WXE_ATOM_define, "wxSTC_ORDER_PRESORTED", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MULTIAUTOC_EACH", rt.make_int(wxSTC_MULTIAUTOC_EACH) },
#else
    { WXE_ATOM_define, "wxSTC_MULTIAUTOC_EACH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MULTIAUTOC_ONCE", rt.make_int(wxSTC_MULTIAUTOC_ONCE) },
#else
    { WXE_ATOM_define, "wxSTC_MULTIAUTOC_ONCE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_CASEINSENSITIVEBEHAVIOUR_IGNORECASE", rt.make_int(wxSTC_CASEINSENSITIVEBEHAVIOUR_IGNORECASE) },
    { WXE_ATOM_define, "wxSTC_CASEINSENSITIVEBEHAVIOUR_RESPECTCASE", rt.make_int(wxSTC_CASEINSENSITIVEBEHAVIOUR_RESPECTCASE) },
    { WXE_ATOM_define, "wxSTC_SEL_THIN", rt.make_int(wxSTC_SEL_THIN) },
    { WXE_ATOM_define, "wxSTC_SEL_LINES", rt.make_int(wxSTC_SEL_LINES) },
    { WXE_ATOM_define, "wxSTC_SEL_RECTANGLE", rt.make_int(wxSTC_SEL_RECTANGLE) },
    { WXE_ATOM_define, "wxSTC_SEL_STREAM", rt.make_int(wxSTC_SEL_STREAM) },
    { WXE_ATOM_define, "wxSTC_CARET_EVEN", rt.make_int(wxSTC_CARET_EVEN) },
    { WXE_ATOM_define, "wxSTC_CARET_JUMPS", rt.make_int(wxSTC_CARET_JUMPS) },
    { WXE_ATOM_define, "wxSTC_CARET_STRICT", rt.make_int(wxSTC_CARET_STRICT) },
    { WXE_ATOM_define, "wxSTC_CARET_SLOP", rt.make_int(wxSTC_CARET_SLOP) },
    { WXE_ATOM_define, "wxSTC_VISIBLE_STRICT", rt.make_int(wxSTC_VISIBLE_STRICT) },
    { WXE_ATOM_define, "wxSTC_VISIBLE_SLOP", rt.make_int(wxSTC_VISIBLE_SLOP) },
    { WXE_ATOM_define, "wxSTC_CURSORREVERSEARROW", rt.make_int(wxSTC_CURSORREVERSEARROW) },
    { WXE_ATOM_define, "wxSTC_CURSORWAIT", rt.make_int(wxSTC_CURSORWAIT) },
    { WXE_ATOM_define, "wxSTC_CURSORARROW", rt.make_int(wxSTC_CURSORARROW) },
    { WXE_ATOM_define, "wxSTC_CURSORNORMAL", rt.make_int(wxSTC_CURSORNORMAL) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STATUS_WARN_REGEX", rt.make_int(wxSTC_STATUS_WARN_REGEX) },
#else
    { WXE_ATOM_define, "wxSTC_STATUS_WARN_REGEX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STATUS_WARN_START", rt.make_int(wxSTC_STATUS_WARN_START) },
#else
    { WXE_ATOM_define, "wxSTC_STATUS_WARN_START", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_STATUS_BADALLOC", rt.make_int(wxSTC_STATUS_BADALLOC) },
    { WXE_ATOM_define, "wxSTC_STATUS_FAILURE", rt.make_int(wxSTC_STATUS_FAILURE) },
    { WXE_ATOM_define, "wxSTC_STATUS_OK", rt.make_int(wxSTC_STATUS_OK) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_POPUP_TEXT", rt.make_int(wxSTC_POPUP_TEXT) },
#else
    { WXE_ATOM_define, "wxSTC_POPUP_TEXT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_POPUP_ALL", rt.make_int(wxSTC_POPUP_ALL) },
#else
    { WXE_ATOM_define, "wxSTC_POPUP_ALL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_POPUP_NEVER", rt.make_int(wxSTC_POPUP_NEVER) },
#else
    { WXE_ATOM_define, "wxSTC_POPUP_NEVER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_EDGE_MULTILINE", rt.make_int(wxSTC_EDGE_MULTILINE) },
#else
    { WXE_ATOM_define, "wxSTC_EDGE_MULTILINE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_EDGE_BACKGROUND", rt.make_int(wxSTC_EDGE_BACKGROUND) },
    { WXE_ATOM_define, "wxSTC_EDGE_LINE", rt.make_int(wxSTC_EDGE_LINE) },
    { WXE_ATOM_define, "wxSTC_EDGE_NONE", rt.make_int(wxSTC_EDGE_NONE) },
    { WXE_ATOM_define, "wxSTC_MULTIPASTE_EACH", rt.make_int(wxSTC_MULTIPASTE_EACH) },
    { WXE_ATOM_define, "wxSTC_MULTIPASTE_ONCE", rt.make_int(wxSTC_MULTIPASTE_ONCE) },
    { WXE_ATOM_define, "wxSTC_EFF_QUALITY_LCD_OPTIMIZED", rt.make_int(wxSTC_EFF_QUALITY_LCD_OPTIMIZED) },
    { WXE_ATOM_define, "wxSTC_EFF_QUALITY_ANTIALIASED", rt.make_int(wxSTC_EFF_QUALITY_ANTIALIASED) },
    { WXE_ATOM_define, "wxSTC_EFF_QUALITY_NON_ANTIALIASED", rt.make_int(wxSTC_EFF_QUALITY_NON_ANTIALIASED) },
    { WXE_ATOM_define, "wxSTC_EFF_QUALITY_DEFAULT", rt.make_int(wxSTC_EFF_QUALITY_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_EFF_QUALITY_MASK", rt.make_int(wxSTC_EFF_QUALITY_MASK) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PHASES_MULTIPLE", rt.make_int(wxSTC_PHASES_MULTIPLE) },
#else
    { WXE_ATOM_define, "wxSTC_PHASES_MULTIPLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PHASES_TWO", rt.make_int(wxSTC_PHASES_TWO) },
#else
    { WXE_ATOM_define, "wxSTC_PHASES_TWO", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_PHASES_ONE", rt.make_int(wxSTC_PHASES_ONE) },
#else
    { WXE_ATOM_define, "wxSTC_PHASES_ONE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_CACHE_DOCUMENT", rt.make_int(wxSTC_CACHE_DOCUMENT) },
    { WXE_ATOM_define, "wxSTC_CACHE_PAGE", rt.make_int(wxSTC_CACHE_PAGE) },
    { WXE_ATOM_define, "wxSTC_CACHE_CARET", rt.make_int(wxSTC_CACHE_CARET) },
    { WXE_ATOM_define, "wxSTC_CACHE_NONE", rt.make_int(wxSTC_CACHE_NONE) },
    { WXE_ATOM_define, "wxSTC_WRAPINDENT_INDENT", rt.make_int(wxSTC_WRAPINDENT_INDENT) },
    { WXE_ATOM_define, "wxSTC_WRAPINDENT_SAME", rt.make_int(wxSTC_WRAPINDENT_SAME) },
    { WXE_ATOM_define, "wxSTC_WRAPINDENT_FIXED", rt.make_int(wxSTC_WRAPINDENT_FIXED) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAGLOC_START_BY_TEXT", rt.make_int(wxSTC_WRAPVISUALFLAGLOC_START_BY_TEXT) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAGLOC_END_BY_TEXT", rt.make_int(wxSTC_WRAPVISUALFLAGLOC_END_BY_TEXT) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAGLOC_DEFAULT", rt.make_int(wxSTC_WRAPVISUALFLAGLOC_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAG_MARGIN", rt.make_int(wxSTC_WRAPVISUALFLAG_MARGIN) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAG_START", rt.make_int(wxSTC_WRAPVISUALFLAG_START) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAG_END", rt.make_int(wxSTC_WRAPVISUALFLAG_END) },
    { WXE_ATOM_define, "wxSTC_WRAPVISUALFLAG_NONE", rt.make_int(wxSTC_WRAPVISUALFLAG_NONE) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_WRAP_WHITESPACE", rt.make_int(wxSTC_WRAP_WHITESPACE) },
#else
    { WXE_ATOM_define, "wxSTC_WRAP_WHITESPACE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_WRAP_CHAR", rt.make_int(wxSTC_WRAP_CHAR) },
    { WXE_ATOM_define, "wxSTC_WRAP_WORD", rt.make_int(wxSTC_WRAP_WORD) },
    { WXE_ATOM_define, "wxSTC_WRAP_NONE", rt.make_int(wxSTC_WRAP_NONE) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_ALL", rt.make_int(wxSTC_IDLESTYLING_ALL) },
#else
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_ALL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_AFTERVISIBLE", rt.make_int(wxSTC_IDLESTYLING_AFTERVISIBLE) },
#else
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_AFTERVISIBLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_TOVISIBLE", rt.make_int(wxSTC_IDLESTYLING_TOVISIBLE) },
#else
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_TOVISIBLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_NONE", rt.make_int(wxSTC_IDLESTYLING_NONE) },
#else
    { WXE_ATOM_define, "wxSTC_IDLESTYLING_NONE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_TIME_FOREVER", rt.make_int(wxSTC_TIME_FOREVER) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LINESTATE", rt.make_int(wxSTC_FOLDFLAG_LINESTATE) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LINESTATE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LEVELNUMBERS", rt.make_int(wxSTC_FOLDFLAG_LEVELNUMBERS) },
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LINEAFTER_CONTRACTED", rt.make_int(wxSTC_FOLDFLAG_LINEAFTER_CONTRACTED) },
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LINEAFTER_EXPANDED", rt.make_int(wxSTC_FOLDFLAG_LINEAFTER_EXPANDED) },
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LINEBEFORE_CONTRACTED", rt.make_int(wxSTC_FOLDFLAG_LINEBEFORE_CONTRACTED) },
    { WXE_ATOM_define, "wxSTC_FOLDFLAG_LINEBEFORE_EXPANDED", rt.make_int(wxSTC_FOLDFLAG_LINEBEFORE_EXPANDED) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AUTOMATICFOLD_CHANGE", rt.make_int(wxSTC_AUTOMATICFOLD_CHANGE) },
#else
    { WXE_ATOM_define, "wxSTC_AUTOMATICFOLD_CHANGE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AUTOMATICFOLD_CLICK", rt.make_int(wxSTC_AUTOMATICFOLD_CLICK) },
#else
    { WXE_ATOM_define, "wxSTC_AUTOMATICFOLD_CLICK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_AUTOMATICFOLD_SHOW", rt.make_int(wxSTC_AUTOMATICFOLD_SHOW) },
#else
    { WXE_ATOM_define, "wxSTC_AUTOMATICFOLD_SHOW", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDACTION_TOGGLE", rt.make_int(wxSTC_FOLDACTION_TOGGLE) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDACTION_TOGGLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDACTION_EXPAND", rt.make_int(wxSTC_FOLDACTION_EXPAND) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDACTION_EXPAND", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDACTION_CONTRACT", rt.make_int(wxSTC_FOLDACTION_CONTRACT) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDACTION_CONTRACT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDDISPLAYTEXT_BOXED", rt.make_int(wxSTC_FOLDDISPLAYTEXT_BOXED) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDDISPLAYTEXT_BOXED", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDDISPLAYTEXT_STANDARD", rt.make_int(wxSTC_FOLDDISPLAYTEXT_STANDARD) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDDISPLAYTEXT_STANDARD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_FOLDDISPLAYTEXT_HIDDEN", rt.make_int(wxSTC_FOLDDISPLAYTEXT_HIDDEN) },
#else
    { WXE_ATOM_define, "wxSTC_FOLDDISPLAYTEXT_HIDDEN", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_FOLDLEVELNUMBERMASK", rt.make_int(wxSTC_FOLDLEVELNUMBERMASK) },
    { WXE_ATOM_define, "wxSTC_FOLDLEVELHEADERFLAG", rt.make_int(wxSTC_FOLDLEVELHEADERFLAG) },
    { WXE_ATOM_define, "wxSTC_FOLDLEVELWHITEFLAG", rt.make_int(wxSTC_FOLDLEVELWHITEFLAG) },
    { WXE_ATOM_define, "wxSTC_FOLDLEVELBASE", rt.make_int(wxSTC_FOLDLEVELBASE) },
    { WXE_ATOM_define, "wxSTC_FIND_POSIX", rt.make_int(wxSTC_FIND_POSIX) },
    { WXE_ATOM_define, "wxSTC_FIND_REGEXP", rt.make_int(wxSTC_FIND_REGEXP) },
    { WXE_ATOM_define, "wxSTC_FIND_WORDSTART", rt.make_int(wxSTC_FIND_WORDSTART) },
    { WXE_ATOM_define, "wxSTC_FIND_MATCHCASE", rt.make_int(wxSTC_FIND_MATCHCASE) },
    { WXE_ATOM_define, "wxSTC_FIND_WHOLEWORD", rt.make_int(wxSTC_FIND_WHOLEWORD) },
    { WXE_ATOM_define, "wxSTC_PRINT_COLOURONWHITEDEFAULTBG", rt.make_int(wxSTC_PRINT_COLOURONWHITEDEFAULTBG) },
    { WXE_ATOM_define, "wxSTC_PRINT_COLOURONWHITE", rt.make_int(wxSTC_PRINT_COLOURONWHITE) },
    { WXE_ATOM_define, "wxSTC_PRINT_BLACKONWHITE", rt.make_int(wxSTC_PRINT_BLACKONWHITE) },
    { WXE_ATOM_define, "wxSTC_PRINT_INVERTLIGHT", rt.make_int(wxSTC_PRINT_INVERTLIGHT) },
    { WXE_ATOM_define, "wxSTC_PRINT_NORMAL", rt.make_int(wxSTC_PRINT_NORMAL) },
    { WXE_ATOM_define, "wxSTC_IV_LOOKBOTH", rt.make_int(wxSTC_IV_LOOKBOTH) },
    { WXE_ATOM_define, "wxSTC_IV_LOOKFORWARD", rt.make_int(wxSTC_IV_LOOKFORWARD) },
    { WXE_ATOM_define, "wxSTC_IV_REAL", rt.make_int(wxSTC_IV_REAL) },
    { WXE_ATOM_define, "wxSTC_IV_NONE", rt.make_int(wxSTC_IV_NONE) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDICFLAG_VALUEFORE", rt.make_int(wxSTC_INDICFLAG_VALUEFORE) },
#else
    { WXE_ATOM_define, "wxSTC_INDICFLAG_VALUEFORE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDICVALUEMASK", rt.make_int(wxSTC_INDICVALUEMASK) },
#else
    { WXE_ATOM_define, "wxSTC_INDICVALUEMASK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDICVALUEBIT", rt.make_int(wxSTC_INDICVALUEBIT) },
#else
    { WXE_ATOM_define, "wxSTC_INDICVALUEBIT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_INDIC_CONTAINER", rt.make_int(wxSTC_INDIC_CONTAINER) },
    { WXE_ATOM_define, "wxSTC_INDIC_MAX", rt.make_int(wxSTC_INDIC_MAX) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_IME_MAX", rt.make_int(wxSTC_INDIC_IME_MAX) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_IME_MAX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_IME", rt.make_int(wxSTC_INDIC_IME) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_IME", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_POINTCHARACTER", rt.make_int(wxSTC_INDIC_POINTCHARACTER) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_POINTCHARACTER", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_POINT", rt.make_int(wxSTC_INDIC_POINT) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_POINT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_TEXTFORE", rt.make_int(wxSTC_INDIC_TEXTFORE) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_TEXTFORE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_FULLBOX", rt.make_int(wxSTC_INDIC_FULLBOX) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_FULLBOX", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_COMPOSITIONTHIN", rt.make_int(wxSTC_INDIC_COMPOSITIONTHIN) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_COMPOSITIONTHIN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_COMPOSITIONTHICK", rt.make_int(wxSTC_INDIC_COMPOSITIONTHICK) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_COMPOSITIONTHICK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_SQUIGGLEPIXMAP", rt.make_int(wxSTC_INDIC_SQUIGGLEPIXMAP) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_SQUIGGLEPIXMAP", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_INDIC_DOTBOX", rt.make_int(wxSTC_INDIC_DOTBOX) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_SQUIGGLELOW", rt.make_int(wxSTC_INDIC_SQUIGGLELOW) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_SQUIGGLELOW", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_INDIC_DOTS", rt.make_int(wxSTC_INDIC_DOTS) },
    { WXE_ATOM_define, "wxSTC_INDIC_DASH", rt.make_int(wxSTC_INDIC_DASH) },
    { WXE_ATOM_define, "wxSTC_INDIC_STRAIGHTBOX", rt.make_int(wxSTC_INDIC_STRAIGHTBOX) },
    { WXE_ATOM_define, "wxSTC_INDIC_ROUNDBOX", rt.make_int(wxSTC_INDIC_ROUNDBOX) },
    { WXE_ATOM_define, "wxSTC_INDIC_BOX", rt.make_int(wxSTC_INDIC_BOX) },
    { WXE_ATOM_define, "wxSTC_INDIC_HIDDEN", rt.make_int(wxSTC_INDIC_HIDDEN) },
    { WXE_ATOM_define, "wxSTC_INDIC_STRIKE", rt.make_int(wxSTC_INDIC_STRIKE) },
    { WXE_ATOM_define, "wxSTC_INDIC_DIAGONAL", rt.make_int(wxSTC_INDIC_DIAGONAL) },
    { WXE_ATOM_define, "wxSTC_INDIC_TT", rt.make_int(wxSTC_INDIC_TT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_INDIC_SQUIGGLE", rt.make_int(wxSTC_INDIC_SQUIGGLE) },
#else
    { WXE_ATOM_define, "wxSTC_INDIC_SQUIGGLE", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_INDIC_PLAIN", rt.make_int(wxSTC_INDIC_PLAIN) },
    { WXE_ATOM_define, "wxSTC_WEIGHT_BOLD", rt.make_int(wxSTC_WEIGHT_BOLD) },
    { WXE_ATOM_define, "wxSTC_WEIGHT_SEMIBOLD", rt.make_int(wxSTC_WEIGHT_SEMIBOLD) },
    { WXE_ATOM_define, "wxSTC_WEIGHT_NORMAL", rt.make_int(wxSTC_WEIGHT_NORMAL) },
    { WXE_ATOM_define, "wxSTC_FONT_SIZE_MULTIPLIER", rt.make_int(wxSTC_FONT_SIZE_MULTIPLIER) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_CASE_CAMEL", rt.make_int(wxSTC_CASE_CAMEL) },
#else
    { WXE_ATOM_define, "wxSTC_CASE_CAMEL", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_CASE_LOWER", rt.make_int(wxSTC_CASE_LOWER) },
    { WXE_ATOM_define, "wxSTC_CASE_UPPER", rt.make_int(wxSTC_CASE_UPPER) },
    { WXE_ATOM_define, "wxSTC_CASE_MIXED", rt.make_int(wxSTC_CASE_MIXED) },
    { WXE_ATOM_define, "wxSTC_CHARSET_8859_15", rt.make_int(wxSTC_CHARSET_8859_15) },
    { WXE_ATOM_define, "wxSTC_CHARSET_THAI", rt.make_int(wxSTC_CHARSET_THAI) },
    { WXE_ATOM_define, "wxSTC_CHARSET_VIETNAMESE", rt.make_int(wxSTC_CHARSET_VIETNAMESE) },
    { WXE_ATOM_define, "wxSTC_CHARSET_ARABIC", rt.make_int(wxSTC_CHARSET_ARABIC) },
    { WXE_ATOM_define, "wxSTC_CHARSET_HEBREW", rt.make_int(wxSTC_CHARSET_HEBREW) },
    { WXE_ATOM_define, "wxSTC_CHARSET_JOHAB", rt.make_int(wxSTC_CHARSET_JOHAB) },
    { WXE_ATOM_define, "wxSTC_CHARSET_TURKISH", rt.make_int(wxSTC_CHARSET_TURKISH) },
    { WXE_ATOM_define, "wxSTC_CHARSET_SYMBOL", rt.make_int(wxSTC_CHARSET_SYMBOL) },
    { WXE_ATOM_define, "wxSTC_CHARSET_SHIFTJIS", rt.make_int(wxSTC_CHARSET_SHIFTJIS) },
    { WXE_ATOM_define, "wxSTC_CHARSET_CYRILLIC", rt.make_int(wxSTC_CHARSET_CYRILLIC) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_CHARSET_OEM866", rt.make_int(wxSTC_CHARSET_OEM866) },
#else
    { WXE_ATOM_define, "wxSTC_CHARSET_OEM866", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_CHARSET_RUSSIAN", rt.make_int(wxSTC_CHARSET_RUSSIAN) },
    { WXE_ATOM_define, "wxSTC_CHARSET_OEM", rt.make_int(wxSTC_CHARSET_OEM) },
    { WXE_ATOM_define, "wxSTC_CHARSET_MAC", rt.make_int(wxSTC_CHARSET_MAC) },
    { WXE_ATOM_define, "wxSTC_CHARSET_HANGUL", rt.make_int(wxSTC_CHARSET_HANGUL) },
    { WXE_ATOM_define, "wxSTC_CHARSET_GREEK", rt.make_int(wxSTC_CHARSET_GREEK) },
    { WXE_ATOM_define, "wxSTC_CHARSET_GB2312", rt.make_int(wxSTC_CHARSET_GB2312) },
    { WXE_ATOM_define, "wxSTC_CHARSET_EASTEUROPE", rt.make_int(wxSTC_CHARSET_EASTEUROPE) },
    { WXE_ATOM_define, "wxSTC_CHARSET_CHINESEBIG5", rt.make_int(wxSTC_CHARSET_CHINESEBIG5) },
    { WXE_ATOM_define, "wxSTC_CHARSET_BALTIC", rt.make_int(wxSTC_CHARSET_BALTIC) },
    { WXE_ATOM_define, "wxSTC_CHARSET_DEFAULT", rt.make_int(wxSTC_CHARSET_DEFAULT) },
    { WXE_ATOM_define, "wxSTC_CHARSET_ANSI", rt.make_int(wxSTC_CHARSET_ANSI) },
    { WXE_ATOM_define, "wxSTC_STYLE_MAX", rt.make_int(wxSTC_STYLE_MAX) },
    { WXE_ATOM_define, "wxSTC_STYLE_LASTPREDEFINED", rt.make_int(wxSTC_STYLE_LASTPREDEFINED) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_STYLE_FOLDDISPLAYTEXT", rt.make_int(wxSTC_STYLE_FOLDDISPLAYTEXT) },
#else
    { WXE_ATOM_define, "wxSTC_STYLE_FOLDDISPLAYTEXT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_STYLE_CALLTIP", rt.make_int(wxSTC_STYLE_CALLTIP) },
    { WXE_ATOM_define, "wxSTC_STYLE_INDENTGUIDE", rt.make_int(wxSTC_STYLE_INDENTGUIDE) },
    { WXE_ATOM_define, "wxSTC_STYLE_CONTROLCHAR", rt.make_int(wxSTC_STYLE_CONTROLCHAR) },
    { WXE_ATOM_define, "wxSTC_STYLE_BRACEBAD", rt.make_int(wxSTC_STYLE_BRACEBAD) },
    { WXE_ATOM_define, "wxSTC_STYLE_BRACELIGHT", rt.make_int(wxSTC_STYLE_BRACELIGHT) },
    { WXE_ATOM_define, "wxSTC_STYLE_LINENUMBER", rt.make_int(wxSTC_STYLE_LINENUMBER) },
    { WXE_ATOM_define, "wxSTC_STYLE_DEFAULT", rt.make_int(wxSTC_STYLE_DEFAULT) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MARGIN_COLOUR", rt.make_int(wxSTC_MARGIN_COLOUR) },
#else
    { WXE_ATOM_define, "wxSTC_MARGIN_COLOUR", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_MARGIN_RTEXT", rt.make_int(wxSTC_MARGIN_RTEXT) },
    { WXE_ATOM_define, "wxSTC_MARGIN_TEXT", rt.make_int(wxSTC_MARGIN_TEXT) },
    { WXE_ATOM_define, "wxSTC_MARGIN_FORE", rt.make_int(wxSTC_MARGIN_FORE) },
    { WXE_ATOM_define, "wxSTC_MARGIN_BACK", rt.make_int(wxSTC_MARGIN_BACK) },
    { WXE_ATOM_define, "wxSTC_MARGIN_NUMBER", rt.make_int(wxSTC_MARGIN_NUMBER) },
    { WXE_ATOM_define, "wxSTC_MARGIN_SYMBOL", rt.make_int(wxSTC_MARGIN_SYMBOL) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MAX_MARGIN", rt.make_int(wxSTC_MAX_MARGIN) },
#else
    { WXE_ATOM_define, "wxSTC_MAX_MARGIN", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_MASK_FOLDERS", rt.make_int(wxSTC_MASK_FOLDERS) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDEROPEN", rt.make_int(wxSTC_MARKNUM_FOLDEROPEN) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDER", rt.make_int(wxSTC_MARKNUM_FOLDER) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDERSUB", rt.make_int(wxSTC_MARKNUM_FOLDERSUB) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDERTAIL", rt.make_int(wxSTC_MARKNUM_FOLDERTAIL) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDERMIDTAIL", rt.make_int(wxSTC_MARKNUM_FOLDERMIDTAIL) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDEROPENMID", rt.make_int(wxSTC_MARKNUM_FOLDEROPENMID) },
    { WXE_ATOM_define, "wxSTC_MARKNUM_FOLDEREND", rt.make_int(wxSTC_MARKNUM_FOLDEREND) },
    { WXE_ATOM_define, "wxSTC_MARK_CHARACTER", rt.make_int(wxSTC_MARK_CHARACTER) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_MARK_BOOKMARK", rt.make_int(wxSTC_MARK_BOOKMARK) },
#else
    { WXE_ATOM_define, "wxSTC_MARK_BOOKMARK", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_MARK_RGBAIMAGE", rt.make_int(wxSTC_MARK_RGBAIMAGE) },
    { WXE_ATOM_define, "wxSTC_MARK_UNDERLINE", rt.make_int(wxSTC_MARK_UNDERLINE) },
    { WXE_ATOM_define, "wxSTC_MARK_AVAILABLE", rt.make_int(wxSTC_MARK_AVAILABLE) },
    { WXE_ATOM_define, "wxSTC_MARK_LEFTRECT", rt.make_int(wxSTC_MARK_LEFTRECT) },
    { WXE_ATOM_define, "wxSTC_MARK_FULLRECT", rt.make_int(wxSTC_MARK_FULLRECT) },
    { WXE_ATOM_define, "wxSTC_MARK_PIXMAP", rt.make_int(wxSTC_MARK_PIXMAP) },
    { WXE_ATOM_define, "wxSTC_MARK_ARROWS", rt.make_int(wxSTC_MARK_ARROWS) },
    { WXE_ATOM_define, "wxSTC_MARK_DOTDOTDOT", rt.make_int(wxSTC_MARK_DOTDOTDOT) },
    { WXE_ATOM_define, "wxSTC_MARK_BACKGROUND", rt.make_int(wxSTC_MARK_BACKGROUND) },
    { WXE_ATOM_define, "wxSTC_MARK_CIRCLEMINUSCONNECTED", rt.make_int(wxSTC_MARK_CIRCLEMINUSCONNECTED) },
    { WXE_ATOM_define, "wxSTC_MARK_CIRCLEMINUS", rt.make_int(wxSTC_MARK_CIRCLEMINUS) },
    { WXE_ATOM_define, "wxSTC_MARK_CIRCLEPLUSCONNECTED", rt.make_int(wxSTC_MARK_CIRCLEPLUSCONNECTED) },
    { WXE_ATOM_define, "wxSTC_MARK_CIRCLEPLUS", rt.make_int(wxSTC_MARK_CIRCLEPLUS) },
    { WXE_ATOM_define, "wxSTC_MARK_TCORNERCURVE", rt.make_int(wxSTC_MARK_TCORNERCURVE) },
    { WXE_ATOM_define, "wxSTC_MARK_LCORNERCURVE", rt.make_int(wxSTC_MARK_LCORNERCURVE) },
    { WXE_ATOM_define, "wxSTC_MARK_BOXMINUSCONNECTED", rt.make_int(wxSTC_MARK_BOXMINUSCONNECTED) },
    { WXE_ATOM_define, "wxSTC_MARK_BOXMINUS", rt.make_int(wxSTC_MARK_BOXMINUS) },
    { WXE_ATOM_define, "wxSTC_MARK_BOXPLUSCONNECTED", rt.make_int(wxSTC_MARK_BOXPLUSCONNECTED) },
    { WXE_ATOM_define, "wxSTC_MARK_BOXPLUS", rt.make_int(wxSTC_MARK_BOXPLUS) },
    { WXE_ATOM_define, "wxSTC_MARK_TCORNER", rt.make_int(wxSTC_MARK_TCORNER) },
    { WXE_ATOM_define, "wxSTC_MARK_LCORNER", rt.make_int(wxSTC_MARK_LCORNER) },
    { WXE_ATOM_define, "wxSTC_MARK_VLINE", rt.make_int(wxSTC_MARK_VLINE) },
    { WXE_ATOM_define, "wxSTC_MARK_PLUS", rt.make_int(wxSTC_MARK_PLUS) },
    { WXE_ATOM_define, "wxSTC_MARK_MINUS", rt.make_int(wxSTC_MARK_MINUS) },
    { WXE_ATOM_define, "wxSTC_MARK_ARROWDOWN", rt.make_int(wxSTC_MARK_ARROWDOWN) },
    { WXE_ATOM_define, "wxSTC_MARK_EMPTY", rt.make_int(wxSTC_MARK_EMPTY) },
    { WXE_ATOM_define, "wxSTC_MARK_SHORTARROW", rt.make_int(wxSTC_MARK_SHORTARROW) },
    { WXE_ATOM_define, "wxSTC_MARK_SMALLRECT", rt.make_int(wxSTC_MARK_SMALLRECT) },
    { WXE_ATOM_define, "wxSTC_MARK_ARROW", rt.make_int(wxSTC_MARK_ARROW) },
    { WXE_ATOM_define, "wxSTC_MARK_ROUNDRECT", rt.make_int(wxSTC_MARK_ROUNDRECT) },
    { WXE_ATOM_define, "wxSTC_MARK_CIRCLE", rt.make_int(wxSTC_MARK_CIRCLE) },
    { WXE_ATOM_define, "wxSTC_MARKER_MAX", rt.make_int(wxSTC_MARKER_MAX) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_IME_INLINE", rt.make_int(wxSTC_IME_INLINE) },
#else
    { WXE_ATOM_define, "wxSTC_IME_INLINE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_IME_WINDOWED", rt.make_int(wxSTC_IME_WINDOWED) },
#else
    { WXE_ATOM_define, "wxSTC_IME_WINDOWED", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_CP_UTF8", rt.make_int(wxSTC_CP_UTF8) },
    { WXE_ATOM_define, "wxSTC_EOL_LF", rt.make_int(wxSTC_EOL_LF) },
    { WXE_ATOM_define, "wxSTC_EOL_CR", rt.make_int(wxSTC_EOL_CR) },
    { WXE_ATOM_define, "wxSTC_EOL_CRLF", rt.make_int(wxSTC_EOL_CRLF) },
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_TD_STRIKEOUT", rt.make_int(wxSTC_TD_STRIKEOUT) },
#else
    { WXE_ATOM_define, "wxSTC_TD_STRIKEOUT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_TD_LONGARROW", rt.make_int(wxSTC_TD_LONGARROW) },
#else
    { WXE_ATOM_define, "wxSTC_TD_LONGARROW", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { WXE_ATOM_define, "wxSTC_WS_VISIBLEONLYININDENT", rt.make_int(wxSTC_WS_VISIBLEONLYININDENT) },
#else
    { WXE_ATOM_define, "wxSTC_WS_VISIBLEONLYININDENT", WXE_ATOM_undefined },
#endif
    { WXE_ATOM_define, "wxSTC_WS_VISIBLEAFTERINDENT", rt.make_int(wxSTC_WS_VISIBLEAFTERINDENT) },
    { WXE_ATOM_define, "wxSTC_WS_VISIBLEALWAYS", rt.make_int(wxSTC_WS_VISIBLEALWAYS) },
    { WXE_ATOM_define, "wxSTC_WS_INVISIBLE", rt.make_int(wxSTC_WS_INVISIBLE) },
    { WXE_ATOM_define, "wxSTC_LEXER_START", rt.make_int(wxSTC_LEXER_START) },
    { WXE_ATOM_define, "wxSTC_OPTIONAL_START", rt.make_int(wxSTC_OPTIONAL_START) },
    { WXE_ATOM_define, "wxSTC_START", rt.make_int(wxSTC_START) },
    { WXE_ATOM_define, "wxSTC_INVALID_POSITION", rt.make_int(wxSTC_INVALID_POSITION) },
//  From "textctrl.h"
    { WXE_ATOM_define, "wxTEXT_TYPE_ANY", rt.make_int(wxTEXT_TYPE_ANY) },
    { WXE_ATOM_define, "wxTE_RICH2", rt.make_int(wxTE_RICH2) },
    { WXE_ATOM_define, "wxTE_BESTWRAP", rt.make_int(wxTE_BESTWRAP) },
    { WXE_ATOM_define, "wxTE_WORDWRAP", rt.make_int(wxTE_WORDWRAP) },
    { WXE_ATOM_define, "wxTE_CHARWRAP", rt.make_int(wxTE_CHARWRAP) },
    { WXE_ATOM_define, "wxTE_DONTWRAP", rt.make_int(wxTE_DONTWRAP) },
    { WXE_ATOM_define, "wxTE_NOHIDESEL", rt.make_int(wxTE_NOHIDESEL) },
    { WXE_ATOM_define, "wxTE_AUTO_URL", rt.make_int(wxTE_AUTO_URL) },
    { WXE_ATOM_define, "wxTE_PASSWORD", rt.make_int(wxTE_PASSWORD) },
    { WXE_ATOM_define, "wxTE_PROCESS_ENTER", rt.make_int(wxTE_PROCESS_ENTER) },
    { WXE_ATOM_define, "wxTE_RICH", rt.make_int(wxTE_RICH) },
    { WXE_ATOM_define, "wxTE_CENTRE", rt.make_int(wxTE_CENTRE) },
    { WXE_ATOM_define, "wxTE_RIGHT", rt.make_int(wxTE_RIGHT) },
    { WXE_ATOM_define, "wxTE_CENTER", rt.make_int(wxTE_CENTER) },
    { WXE_ATOM_define, "wxTE_LEFT", rt.make_int(wxTE_LEFT) },
    { WXE_ATOM_define, "wxTE_PROCESS_TAB", rt.make_int(wxTE_PROCESS_TAB) },
    { WXE_ATOM_define, "wxTE_MULTILINE", rt.make_int(wxTE_MULTILINE) },
    { WXE_ATOM_define, "wxTE_READONLY", rt.make_int(wxTE_READONLY) },
    { WXE_ATOM_define, "wxTE_NO_VSCROLL", rt.make_int(wxTE_NO_VSCROLL) },
//  From "textdlg.h"
    { WXE_ATOM_define, "wxTextEntryDialogStyle", rt.make_int(wxTextEntryDialogStyle) },
//  From "toolbook.h"
    { WXE_ATOM_define, "wxTBK_HORZ_LAYOUT", rt.make_int(wxTBK_HORZ_LAYOUT) },
    { WXE_ATOM_define, "wxTBK_BUTTONBAR", rt.make_int(wxTBK_BUTTONBAR) },
//  From "toplevel.h"
    { WXE_ATOM_define, "wxDEFAULT_FRAME_STYLE", rt.make_int(wxDEFAULT_FRAME_STYLE) },
//  From "treebase.h"
    { WXE_ATOM_define, "wxTR_DEFAULT_STYLE", rt.make_int(wxTR_DEFAULT_STYLE) },
    { WXE_ATOM_define, "wxTR_FULL_ROW_HIGHLIGHT", rt.make_int(wxTR_FULL_ROW_HIGHLIGHT) },
    { WXE_ATOM_define, "wxTR_HIDE_ROOT", rt.make_int(wxTR_HIDE_ROOT) },
    { WXE_ATOM_define, "wxTR_ROW_LINES", rt.make_int(wxTR_ROW_LINES) },
    { WXE_ATOM_define, "wxTR_EDIT_LABELS", rt.make_int(wxTR_EDIT_LABELS) },
    { WXE_ATOM_define, "wxTR_HAS_VARIABLE_ROW_HEIGHT", rt.make_int(wxTR_HAS_VARIABLE_ROW_HEIGHT) },
    { WXE_ATOM_define, "wxTR_MULTIPLE", rt.make_int(wxTR_MULTIPLE) },
    { WXE_ATOM_define, "wxTR_SINGLE", rt.make_int(wxTR_SINGLE) },
    { WXE_ATOM_define, "wxTR_TWIST_BUTTONS", rt.make_int(wxTR_TWIST_BUTTONS) },
    { WXE_ATOM_define, "wxTR_LINES_AT_ROOT", rt.make_int(wxTR_LINES_AT_ROOT) },
    { WXE_ATOM_define, "wxTR_NO_LINES", rt.make_int(wxTR_NO_LINES) },
    { WXE_ATOM_define, "wxTR_HAS_BUTTONS", rt.make_int(wxTR_HAS_BUTTONS) },
    { WXE_ATOM_define, "wxTR_NO_BUTTONS", rt.make_int(wxTR_NO_BUTTONS) },
//  From class wxActivateEvent::Reason
    { enif_make_atom(rt.env,"Reason"), "wxActivateEvent_Reason_Mouse", rt.make_int(wxActivateEvent::Reason_Mouse) },
    { enif_make_atom(rt.env,"Reason"), "wxActivateEvent_Reason_Unknown", rt.make_int(wxActivateEvent::Reason_Unknown) },
//  From class wxAuiNotebook
    { enif_make_atom(rt.env,"wxAuiNotebook"), "wxAuiNotebook_NO_IMAGE", rt.make_int(wxAuiNotebook::NO_IMAGE) },
//  From class wxBookCtrlBase
    { enif_make_atom(rt.env,"wxBookCtrlBase"), "wxBookCtrlBase_NO_IMAGE", rt.make_int(wxBookCtrlBase::NO_IMAGE) },
//  From class wxChoicebook
    { enif_make_atom(rt.env,"wxChoicebook"), "wxChoicebook_NO_IMAGE", rt.make_int(wxChoicebook::NO_IMAGE) },
//  From class wxChoicebook
    { enif_make_atom(rt.env,"wxChoicebook"), "wxChoicebook_NO_IMAGE", rt.make_int(wxChoicebook::NO_IMAGE) },
//  From class wxColourData
    { enif_make_atom(rt.env,"wxColourData"), "wxColourData_NUM_CUSTOM", rt.make_int(wxColourData::NUM_CUSTOM) },
//  From class wxDataObject::Direction
    { enif_make_atom(rt.env,"Direction"), "wxDataObject_Get", rt.make_int(wxDataObject::Get) },
    { enif_make_atom(rt.env,"Direction"), "wxDataObject_Set", rt.make_int(wxDataObject::Set) },
    { enif_make_atom(rt.env,"Direction"), "wxDataObject_Both", rt.make_int(wxDataObject::Both) },
//  From class wxDateTime::Calendar
    { enif_make_atom(rt.env,"Calendar"), "wxDateTime_Gregorian", rt.make_int(wxDateTime::Gregorian) },
    { enif_make_atom(rt.env,"Calendar"), "wxDateTime_Julian", rt.make_int(wxDateTime::Julian) },
//  From class wxDateTime::Country
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Country_Unknown", rt.make_int(wxDateTime::Country_Unknown) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Country_Default", rt.make_int(wxDateTime::Country_Default) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Country_WesternEurope_Start", rt.make_int(wxDateTime::Country_WesternEurope_Start) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Country_EEC", rt.make_int(wxDateTime::Country_EEC) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_France", rt.make_int(wxDateTime::France) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Germany", rt.make_int(wxDateTime::Germany) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_UK", rt.make_int(wxDateTime::UK) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Country_WesternEurope_End", rt.make_int(wxDateTime::Country_WesternEurope_End) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_Russia", rt.make_int(wxDateTime::Russia) },
    { enif_make_atom(rt.env,"Country"), "wxDateTime_USA", rt.make_int(wxDateTime::USA) },
//  From class wxDateTime::Month
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Jan", rt.make_int(wxDateTime::Jan) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Feb", rt.make_int(wxDateTime::Feb) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Mar", rt.make_int(wxDateTime::Mar) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Apr", rt.make_int(wxDateTime::Apr) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_May", rt.make_int(wxDateTime::May) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Jun", rt.make_int(wxDateTime::Jun) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Jul", rt.make_int(wxDateTime::Jul) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Aug", rt.make_int(wxDateTime::Aug) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Sep", rt.make_int(wxDateTime::Sep) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Oct", rt.make_int(wxDateTime::Oct) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Nov", rt.make_int(wxDateTime::Nov) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Dec", rt.make_int(wxDateTime::Dec) },
    { enif_make_atom(rt.env,"Month"), "wxDateTime_Inv_Month", rt.make_int(wxDateTime::Inv_Month) },
//  From class wxDateTime::NameFlags
    { enif_make_atom(rt.env,"NameFlags"), "wxDateTime_Name_Full", rt.make_int(wxDateTime::Name_Full) },
    { enif_make_atom(rt.env,"NameFlags"), "wxDateTime_Name_Abbr", rt.make_int(wxDateTime::Name_Abbr) },
//  From class wxDateTime::TZ
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_Local", rt.make_int(wxDateTime::Local) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_12", rt.make_int(wxDateTime::GMT_12) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_11", rt.make_int(wxDateTime::GMT_11) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_10", rt.make_int(wxDateTime::GMT_10) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_9", rt.make_int(wxDateTime::GMT_9) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_8", rt.make_int(wxDateTime::GMT_8) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_7", rt.make_int(wxDateTime::GMT_7) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_6", rt.make_int(wxDateTime::GMT_6) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_5", rt.make_int(wxDateTime::GMT_5) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_4", rt.make_int(wxDateTime::GMT_4) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_3", rt.make_int(wxDateTime::GMT_3) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_2", rt.make_int(wxDateTime::GMT_2) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT_1", rt.make_int(wxDateTime::GMT_1) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT0", rt.make_int(wxDateTime::GMT0) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT1", rt.make_int(wxDateTime::GMT1) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT2", rt.make_int(wxDateTime::GMT2) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT3", rt.make_int(wxDateTime::GMT3) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT4", rt.make_int(wxDateTime::GMT4) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT5", rt.make_int(wxDateTime::GMT5) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT6", rt.make_int(wxDateTime::GMT6) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT7", rt.make_int(wxDateTime::GMT7) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT8", rt.make_int(wxDateTime::GMT8) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT9", rt.make_int(wxDateTime::GMT9) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT10", rt.make_int(wxDateTime::GMT10) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT11", rt.make_int(wxDateTime::GMT11) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT12", rt.make_int(wxDateTime::GMT12) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_GMT13", rt.make_int(wxDateTime::GMT13) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_WET", rt.make_int(wxDateTime::WET) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_WEST", rt.make_int(wxDateTime::WEST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_CET", rt.make_int(wxDateTime::CET) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_CEST", rt.make_int(wxDateTime::CEST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_EET", rt.make_int(wxDateTime::EET) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_EEST", rt.make_int(wxDateTime::EEST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_MSK", rt.make_int(wxDateTime::MSK) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_MSD", rt.make_int(wxDateTime::MSD) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_AST", rt.make_int(wxDateTime::AST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_ADT", rt.make_int(wxDateTime::ADT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_EST", rt.make_int(wxDateTime::EST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_EDT", rt.make_int(wxDateTime::EDT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_CST", rt.make_int(wxDateTime::CST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_CDT", rt.make_int(wxDateTime::CDT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_MST", rt.make_int(wxDateTime::MST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_MDT", rt.make_int(wxDateTime::MDT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_PST", rt.make_int(wxDateTime::PST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_PDT", rt.make_int(wxDateTime::PDT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_HST", rt.make_int(wxDateTime::HST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_AKST", rt.make_int(wxDateTime::AKST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_AKDT", rt.make_int(wxDateTime::AKDT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_A_WST", rt.make_int(wxDateTime::A_WST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_A_CST", rt.make_int(wxDateTime::A_CST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_A_EST", rt.make_int(wxDateTime::A_EST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_A_ESST", rt.make_int(wxDateTime::A_ESST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_NZST", rt.make_int(wxDateTime::NZST) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_NZDT", rt.make_int(wxDateTime::NZDT) },
    { enif_make_atom(rt.env,"TZ"), "wxDateTime_UTC", rt.make_int(wxDateTime::UTC) },
//  From class wxDateTime::WeekDay
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Sun", rt.make_int(wxDateTime::Sun) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Mon", rt.make_int(wxDateTime::Mon) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Tue", rt.make_int(wxDateTime::Tue) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Wed", rt.make_int(wxDateTime::Wed) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Thu", rt.make_int(wxDateTime::Thu) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Fri", rt.make_int(wxDateTime::Fri) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Sat", rt.make_int(wxDateTime::Sat) },
    { enif_make_atom(rt.env,"WeekDay"), "wxDateTime_Inv_WeekDay", rt.make_int(wxDateTime::Inv_WeekDay) },
//  From class wxDateTime::WeekFlags
    { enif_make_atom(rt.env,"WeekFlags"), "wxDateTime_Default_First", rt.make_int(wxDateTime::Default_First) },
    { enif_make_atom(rt.env,"WeekFlags"), "wxDateTime_Monday_First", rt.make_int(wxDateTime::Monday_First) },
    { enif_make_atom(rt.env,"WeekFlags"), "wxDateTime_Sunday_First", rt.make_int(wxDateTime::Sunday_First) },
//  From class wxDateTime::Year
    { enif_make_atom(rt.env,"Year"), "wxDateTime_Inv_Year", rt.make_int(wxDateTime::Inv_Year) },
//  From class wxGrid::CellSpan
    { enif_make_atom(rt.env,"CellSpan"), "wxGrid_CellSpan_Inside", rt.make_int(wxGrid::CellSpan_Inside) },
    { enif_make_atom(rt.env,"CellSpan"), "wxGrid_CellSpan_None", rt.make_int(wxGrid::CellSpan_None) },
    { enif_make_atom(rt.env,"CellSpan"), "wxGrid_CellSpan_Main", rt.make_int(wxGrid::CellSpan_Main) },
//  From class wxGrid::TabBehaviour
    { enif_make_atom(rt.env,"TabBehaviour"), "wxGrid_Tab_Stop", rt.make_int(wxGrid::Tab_Stop) },
    { enif_make_atom(rt.env,"TabBehaviour"), "wxGrid_Tab_Wrap", rt.make_int(wxGrid::Tab_Wrap) },
    { enif_make_atom(rt.env,"TabBehaviour"), "wxGrid_Tab_Leave", rt.make_int(wxGrid::Tab_Leave) },
//  From class wxGrid::wxGridSelectionModes
    { enif_make_atom(rt.env,"wxGridSelectionModes"), "wxGrid_wxGridSelectCells", rt.make_int(wxGrid::wxGridSelectCells) },
    { enif_make_atom(rt.env,"wxGridSelectionModes"), "wxGrid_wxGridSelectRows", rt.make_int(wxGrid::wxGridSelectRows) },
    { enif_make_atom(rt.env,"wxGridSelectionModes"), "wxGrid_wxGridSelectColumns", rt.make_int(wxGrid::wxGridSelectColumns) },
    { enif_make_atom(rt.env,"wxGridSelectionModes"), "wxGrid_wxGridSelectRowsOrColumns", rt.make_int(wxGrid::wxGridSelectRowsOrColumns) },
//  From class wxGridCellAttr::wxAttrKind
    { enif_make_atom(rt.env,"wxAttrKind"), "wxGridCellAttr_Any", rt.make_int(wxGridCellAttr::Any) },
    { enif_make_atom(rt.env,"wxAttrKind"), "wxGridCellAttr_Default", rt.make_int(wxGridCellAttr::Default) },
    { enif_make_atom(rt.env,"wxAttrKind"), "wxGridCellAttr_Cell", rt.make_int(wxGridCellAttr::Cell) },
    { enif_make_atom(rt.env,"wxAttrKind"), "wxGridCellAttr_Row", rt.make_int(wxGridCellAttr::Row) },
    { enif_make_atom(rt.env,"wxAttrKind"), "wxGridCellAttr_Col", rt.make_int(wxGridCellAttr::Col) },
    { enif_make_atom(rt.env,"wxAttrKind"), "wxGridCellAttr_Merged", rt.make_int(wxGridCellAttr::Merged) },
//  From class wxHelpEvent::Origin
    { enif_make_atom(rt.env,"Origin"), "wxHelpEvent_Origin_Unknown", rt.make_int(wxHelpEvent::Origin_Unknown) },
    { enif_make_atom(rt.env,"Origin"), "wxHelpEvent_Origin_Keyboard", rt.make_int(wxHelpEvent::Origin_Keyboard) },
    { enif_make_atom(rt.env,"Origin"), "wxHelpEvent_Origin_HelpButton", rt.make_int(wxHelpEvent::Origin_HelpButton) },
//  From class wxHtmlEasyPrinting::PromptMode
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"PromptMode"), "wxHtmlEasyPrinting_Prompt_Never", rt.make_int(wxHtmlEasyPrinting::Prompt_Never) },
#else
    { enif_make_atom(rt.env,"PromptMode"), "wxHtmlEasyPrinting_Prompt_Never", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"PromptMode"), "wxHtmlEasyPrinting_Prompt_Once", rt.make_int(wxHtmlEasyPrinting::Prompt_Once) },
#else
    { enif_make_atom(rt.env,"PromptMode"), "wxHtmlEasyPrinting_Prompt_Once", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"PromptMode"), "wxHtmlEasyPrinting_Prompt_Always", rt.make_int(wxHtmlEasyPrinting::Prompt_Always) },
#else
    { enif_make_atom(rt.env,"PromptMode"), "wxHtmlEasyPrinting_Prompt_Always", WXE_ATOM_undefined },
#endif
//  From class wxIconBundle
    { enif_make_atom(rt.env,"wxIconBundle"), "wxIconBundle_FALLBACK_NONE", rt.make_int(wxIconBundle::FALLBACK_NONE) },
    { enif_make_atom(rt.env,"wxIconBundle"), "wxIconBundle_FALLBACK_SYSTEM", rt.make_int(wxIconBundle::FALLBACK_SYSTEM) },
    { enif_make_atom(rt.env,"wxIconBundle"), "wxIconBundle_FALLBACK_NEAREST_LARGER", rt.make_int(wxIconBundle::FALLBACK_NEAREST_LARGER) },
//  From class wxListbook
    { enif_make_atom(rt.env,"wxListbook"), "wxListbook_NO_IMAGE", rt.make_int(wxListbook::NO_IMAGE) },
//  From class wxListbook
    { enif_make_atom(rt.env,"wxListbook"), "wxListbook_NO_IMAGE", rt.make_int(wxListbook::NO_IMAGE) },
//  From class wxNavigationKeyEvent::wxNavigationKeyEventFlags
    { enif_make_atom(rt.env,"wxNavigationKeyEventFlags"), "wxNavigationKeyEvent_IsBackward", rt.make_int(wxNavigationKeyEvent::IsBackward) },
    { enif_make_atom(rt.env,"wxNavigationKeyEventFlags"), "wxNavigationKeyEvent_IsForward", rt.make_int(wxNavigationKeyEvent::IsForward) },
    { enif_make_atom(rt.env,"wxNavigationKeyEventFlags"), "wxNavigationKeyEvent_WinChange", rt.make_int(wxNavigationKeyEvent::WinChange) },
    { enif_make_atom(rt.env,"wxNavigationKeyEventFlags"), "wxNavigationKeyEvent_FromTab", rt.make_int(wxNavigationKeyEvent::FromTab) },
//  From class wxNotebook
    { enif_make_atom(rt.env,"wxNotebook"), "wxNotebook_NO_IMAGE", rt.make_int(wxNotebook::NO_IMAGE) },
//  From class wxNotebook
    { enif_make_atom(rt.env,"wxNotebook"), "wxNotebook_NO_IMAGE", rt.make_int(wxNotebook::NO_IMAGE) },
//  From class wxNotificationMessage
    { enif_make_atom(rt.env,"wxNotificationMessage"), "wxNotificationMessage_Timeout_Auto", rt.make_int(wxNotificationMessage::Timeout_Auto) },
    { enif_make_atom(rt.env,"wxNotificationMessage"), "wxNotificationMessage_Timeout_Never", rt.make_int(wxNotificationMessage::Timeout_Never) },
//  From class wxStaticBitmap::ScaleMode
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_None", rt.make_int(wxStaticBitmap::Scale_None) },
#else
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_None", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_Fill", rt.make_int(wxStaticBitmap::Scale_Fill) },
#else
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_Fill", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_AspectFit", rt.make_int(wxStaticBitmap::Scale_AspectFit) },
#else
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_AspectFit", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_AspectFill", rt.make_int(wxStaticBitmap::Scale_AspectFill) },
#else
    { enif_make_atom(rt.env,"ScaleMode"), "wxStaticBitmap_Scale_AspectFill", WXE_ATOM_undefined },
#endif
//  From class wxToolbook
    { enif_make_atom(rt.env,"wxToolbook"), "wxToolbook_NO_IMAGE", rt.make_int(wxToolbook::NO_IMAGE) },
//  From class wxToolbook
    { enif_make_atom(rt.env,"wxToolbook"), "wxToolbook_NO_IMAGE", rt.make_int(wxToolbook::NO_IMAGE) },
//  From class wxTreebook
    { enif_make_atom(rt.env,"wxTreebook"), "wxTreebook_NO_IMAGE", rt.make_int(wxTreebook::NO_IMAGE) },
//  From class wxTreebook
    { enif_make_atom(rt.env,"wxTreebook"), "wxTreebook_NO_IMAGE", rt.make_int(wxTreebook::NO_IMAGE) },
//  From "datectrl.h"
    { enif_make_atom(rt.env,"wxDP"), "wxDP_DEFAULT", rt.make_int(wxDP_DEFAULT) },
    { enif_make_atom(rt.env,"wxDP"), "wxDP_SPIN", rt.make_int(wxDP_SPIN) },
    { enif_make_atom(rt.env,"wxDP"), "wxDP_DROPDOWN", rt.make_int(wxDP_DROPDOWN) },
    { enif_make_atom(rt.env,"wxDP"), "wxDP_SHOWCENTURY", rt.make_int(wxDP_SHOWCENTURY) },
    { enif_make_atom(rt.env,"wxDP"), "wxDP_ALLOWNONE", rt.make_int(wxDP_ALLOWNONE) },
//  From "dirctrl.h"
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_DIR_ONLY", rt.make_int(wxDIRCTRL_DIR_ONLY) },
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_SELECT_FIRST", rt.make_int(wxDIRCTRL_SELECT_FIRST) },
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_SHOW_FILTERS", rt.make_int(wxDIRCTRL_SHOW_FILTERS) },
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_3D_INTERNAL", rt.make_int(wxDIRCTRL_3D_INTERNAL) },
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_EDIT_LABELS", rt.make_int(wxDIRCTRL_EDIT_LABELS) },
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_MULTIPLE", rt.make_int(wxDIRCTRL_MULTIPLE) },
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_DEFAULT_STYLE", rt.make_int(wxDIRCTRL_DEFAULT_STYLE) },
#else
    { enif_make_atom(rt.env,"wxDIRCTRL"), "wxDIRCTRL_DEFAULT_STYLE", WXE_ATOM_undefined },
#endif
//  From "dnd.h"
    { enif_make_atom(rt.env,"wxDrag"), "wxDrag_CopyOnly", rt.make_int(wxDrag_CopyOnly) },
    { enif_make_atom(rt.env,"wxDrag"), "wxDrag_AllowMove", rt.make_int(wxDrag_AllowMove) },
    { enif_make_atom(rt.env,"wxDrag"), "wxDrag_DefaultMove", rt.make_int(wxDrag_DefaultMove) },
//  From "event.h"
    { enif_make_atom(rt.env,"wxJOYSTICK"), "wxJOYSTICK1", rt.make_int(wxJOYSTICK1) },
    { enif_make_atom(rt.env,"wxJOYSTICK"), "wxJOYSTICK2", rt.make_int(wxJOYSTICK2) },
//  From "event.h"
    { enif_make_atom(rt.env,"wxJOY"), "wxJOY_BUTTON_ANY", rt.make_int(wxJOY_BUTTON_ANY) },
    { enif_make_atom(rt.env,"wxJOY"), "wxJOY_BUTTON1", rt.make_int(wxJOY_BUTTON1) },
    { enif_make_atom(rt.env,"wxJOY"), "wxJOY_BUTTON2", rt.make_int(wxJOY_BUTTON2) },
    { enif_make_atom(rt.env,"wxJOY"), "wxJOY_BUTTON3", rt.make_int(wxJOY_BUTTON3) },
    { enif_make_atom(rt.env,"wxJOY"), "wxJOY_BUTTON4", rt.make_int(wxJOY_BUTTON4) },
//  From "filedlg.h"
    { enif_make_atom(rt.env,"wxFD"), "wxFD_OPEN", rt.make_int(wxFD_OPEN) },
    { enif_make_atom(rt.env,"wxFD"), "wxFD_SAVE", rt.make_int(wxFD_SAVE) },
    { enif_make_atom(rt.env,"wxFD"), "wxFD_OVERWRITE_PROMPT", rt.make_int(wxFD_OVERWRITE_PROMPT) },
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxFD"), "wxFD_NO_FOLLOW", rt.make_int(wxFD_NO_FOLLOW) },
#else
    { enif_make_atom(rt.env,"wxFD"), "wxFD_NO_FOLLOW", WXE_ATOM_undefined },
#endif
    { enif_make_atom(rt.env,"wxFD"), "wxFD_FILE_MUST_EXIST", rt.make_int(wxFD_FILE_MUST_EXIST) },
    { enif_make_atom(rt.env,"wxFD"), "wxFD_CHANGE_DIR", rt.make_int(wxFD_CHANGE_DIR) },
    { enif_make_atom(rt.env,"wxFD"), "wxFD_PREVIEW", rt.make_int(wxFD_PREVIEW) },
    { enif_make_atom(rt.env,"wxFD"), "wxFD_MULTIPLE", rt.make_int(wxFD_MULTIPLE) },
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxFD"), "wxFD_SHOW_HIDDEN", rt.make_int(wxFD_SHOW_HIDDEN) },
#else
    { enif_make_atom(rt.env,"wxFD"), "wxFD_SHOW_HIDDEN", WXE_ATOM_undefined },
#endif
//  From "htmprint.h"
    { enif_make_atom(rt.env,"wxPAGE"), "wxPAGE_ODD", rt.make_int(wxPAGE_ODD) },
    { enif_make_atom(rt.env,"wxPAGE"), "wxPAGE_EVEN", rt.make_int(wxPAGE_EVEN) },
    { enif_make_atom(rt.env,"wxPAGE"), "wxPAGE_ALL", rt.make_int(wxPAGE_ALL) },
//  From "bookctrl.h"
    { enif_make_atom(rt.env,"wxBK"), "wxBK_HITTEST_NOWHERE", rt.make_int(wxBK_HITTEST_NOWHERE) },
    { enif_make_atom(rt.env,"wxBK"), "wxBK_HITTEST_ONICON", rt.make_int(wxBK_HITTEST_ONICON) },
    { enif_make_atom(rt.env,"wxBK"), "wxBK_HITTEST_ONLABEL", rt.make_int(wxBK_HITTEST_ONLABEL) },
    { enif_make_atom(rt.env,"wxBK"), "wxBK_HITTEST_ONITEM", rt.make_int(wxBK_HITTEST_ONITEM) },
    { enif_make_atom(rt.env,"wxBK"), "wxBK_HITTEST_ONPAGE", rt.make_int(wxBK_HITTEST_ONPAGE) },
//  From "image.h"
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_24BPP", rt.make_int(wxBMP_24BPP) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_8BPP", rt.make_int(wxBMP_8BPP) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_8BPP_GREY", rt.make_int(wxBMP_8BPP_GREY) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_8BPP_GRAY", rt.make_int(wxBMP_8BPP_GRAY) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_8BPP_RED", rt.make_int(wxBMP_8BPP_RED) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_8BPP_PALETTE", rt.make_int(wxBMP_8BPP_PALETTE) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_4BPP", rt.make_int(wxBMP_4BPP) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_1BPP", rt.make_int(wxBMP_1BPP) },
    { enif_make_atom(rt.env,"wxBMP"), "wxBMP_1BPP_BW", rt.make_int(wxBMP_1BPP_BW) },
//  From "notebook.h"
    { enif_make_atom(rt.env,"wxNB"), "wxNB_HITTEST_NOWHERE", rt.make_int(wxNB_HITTEST_NOWHERE) },
    { enif_make_atom(rt.env,"wxNB"), "wxNB_HITTEST_ONICON", rt.make_int(wxNB_HITTEST_ONICON) },
    { enif_make_atom(rt.env,"wxNB"), "wxNB_HITTEST_ONLABEL", rt.make_int(wxNB_HITTEST_ONLABEL) },
    { enif_make_atom(rt.env,"wxNB"), "wxNB_HITTEST_ONITEM", rt.make_int(wxNB_HITTEST_ONITEM) },
    { enif_make_atom(rt.env,"wxNB"), "wxNB_HITTEST_ONPAGE", rt.make_int(wxNB_HITTEST_ONPAGE) },
//  From "splitter.h"
    { enif_make_atom(rt.env,"wxSPLIT"), "wxSPLIT_DRAG_NONE", rt.make_int(wxSPLIT_DRAG_NONE) },
    { enif_make_atom(rt.env,"wxSPLIT"), "wxSPLIT_DRAG_DRAGGING", rt.make_int(wxSPLIT_DRAG_DRAGGING) },
    { enif_make_atom(rt.env,"wxSPLIT"), "wxSPLIT_DRAG_LEFT_DOWN", rt.make_int(wxSPLIT_DRAG_LEFT_DOWN) },
//  From "toolbar.h"
    { enif_make_atom(rt.env,"wxTB"), "wxTB_HORIZONTAL", rt.make_int(wxTB_HORIZONTAL) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_TOP", rt.make_int(wxTB_TOP) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_VERTICAL", rt.make_int(wxTB_VERTICAL) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_LEFT", rt.make_int(wxTB_LEFT) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_FLAT", rt.make_int(wxTB_FLAT) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_DOCKABLE", rt.make_int(wxTB_DOCKABLE) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_NOICONS", rt.make_int(wxTB_NOICONS) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_TEXT", rt.make_int(wxTB_TEXT) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_NODIVIDER", rt.make_int(wxTB_NODIVIDER) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_NOALIGN", rt.make_int(wxTB_NOALIGN) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_HORZ_LAYOUT", rt.make_int(wxTB_HORZ_LAYOUT) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_HORZ_TEXT", rt.make_int(wxTB_HORZ_TEXT) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_NO_TOOLTIPS", rt.make_int(wxTB_NO_TOOLTIPS) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_BOTTOM", rt.make_int(wxTB_BOTTOM) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_RIGHT", rt.make_int(wxTB_RIGHT) },
    { enif_make_atom(rt.env,"wxTB"), "wxTB_DEFAULT_STYLE", rt.make_int(wxTB_DEFAULT_STYLE) },
//  From "toplevel.h"
    { enif_make_atom(rt.env,"wxUSER"), "wxUSER_ATTENTION_INFO", rt.make_int(wxUSER_ATTENTION_INFO) },
    { enif_make_atom(rt.env,"wxUSER"), "wxUSER_ATTENTION_ERROR", rt.make_int(wxUSER_ATTENTION_ERROR) },
//  From "toplevel.h"
    { enif_make_atom(rt.env,"wxFULLSCREEN"), "wxFULLSCREEN_NOMENUBAR", rt.make_int(wxFULLSCREEN_NOMENUBAR) },
    { enif_make_atom(rt.env,"wxFULLSCREEN"), "wxFULLSCREEN_NOTOOLBAR", rt.make_int(wxFULLSCREEN_NOTOOLBAR) },
    { enif_make_atom(rt.env,"wxFULLSCREEN"), "wxFULLSCREEN_NOSTATUSBAR", rt.make_int(wxFULLSCREEN_NOSTATUSBAR) },
    { enif_make_atom(rt.env,"wxFULLSCREEN"), "wxFULLSCREEN_NOBORDER", rt.make_int(wxFULLSCREEN_NOBORDER) },
    { enif_make_atom(rt.env,"wxFULLSCREEN"), "wxFULLSCREEN_NOCAPTION", rt.make_int(wxFULLSCREEN_NOCAPTION) },
    { enif_make_atom(rt.env,"wxFULLSCREEN"), "wxFULLSCREEN_ALL", rt.make_int(wxFULLSCREEN_ALL) },
//  From "utils.h"
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_Mnemonics", rt.make_int(wxStrip_Mnemonics) },
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_Accel", rt.make_int(wxStrip_Accel) },
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_CJKMnemonics", rt.make_int(wxStrip_CJKMnemonics) },
#else
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_CJKMnemonics", WXE_ATOM_undefined },
#endif
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_All", rt.make_int(wxStrip_All) },
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_Menu", rt.make_int(wxStrip_Menu) },
#else
    { enif_make_atom(rt.env,"wxStrip"), "wxStrip_Menu", WXE_ATOM_undefined },
#endif
//  From "utils.h"
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_ASYNC", rt.make_int(wxEXEC_ASYNC) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_SYNC", rt.make_int(wxEXEC_SYNC) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_SHOW_CONSOLE", rt.make_int(wxEXEC_SHOW_CONSOLE) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_MAKE_GROUP_LEADER", rt.make_int(wxEXEC_MAKE_GROUP_LEADER) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_NODISABLE", rt.make_int(wxEXEC_NODISABLE) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_NOEVENTS", rt.make_int(wxEXEC_NOEVENTS) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_HIDE_CONSOLE", rt.make_int(wxEXEC_HIDE_CONSOLE) },
    { enif_make_atom(rt.env,"wxEXEC"), "wxEXEC_BLOCK", rt.make_int(wxEXEC_BLOCK) },
//  From "calctrl.h"
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_SUNDAY_FIRST", rt.make_int(wxCAL_SUNDAY_FIRST) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_MONDAY_FIRST", rt.make_int(wxCAL_MONDAY_FIRST) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_SHOW_HOLIDAYS", rt.make_int(wxCAL_SHOW_HOLIDAYS) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_NO_YEAR_CHANGE", rt.make_int(wxCAL_NO_YEAR_CHANGE) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_NO_MONTH_CHANGE", rt.make_int(wxCAL_NO_MONTH_CHANGE) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_SEQUENTIAL_MONTH_SELECTION", rt.make_int(wxCAL_SEQUENTIAL_MONTH_SELECTION) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_SHOW_SURROUNDING_WEEKS", rt.make_int(wxCAL_SHOW_SURROUNDING_WEEKS) },
    { enif_make_atom(rt.env,"wxCAL"), "wxCAL_SHOW_WEEK_NUMBERS", rt.make_int(wxCAL_SHOW_WEEK_NUMBERS) },
//  From "window.h"
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_NONE", rt.make_int(wxTOUCH_NONE) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_NONE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_VERTICAL_PAN_GESTURE", rt.make_int(wxTOUCH_VERTICAL_PAN_GESTURE) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_VERTICAL_PAN_GESTURE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_HORIZONTAL_PAN_GESTURE", rt.make_int(wxTOUCH_HORIZONTAL_PAN_GESTURE) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_HORIZONTAL_PAN_GESTURE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_PAN_GESTURES", rt.make_int(wxTOUCH_PAN_GESTURES) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_PAN_GESTURES", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_ZOOM_GESTURE", rt.make_int(wxTOUCH_ZOOM_GESTURE) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_ZOOM_GESTURE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_ROTATE_GESTURE", rt.make_int(wxTOUCH_ROTATE_GESTURE) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_ROTATE_GESTURE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_PRESS_GESTURES", rt.make_int(wxTOUCH_PRESS_GESTURES) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_PRESS_GESTURES", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_ALL_GESTURES", rt.make_int(wxTOUCH_ALL_GESTURES) },
#else
    { enif_make_atom(rt.env,"wxTOUCH"), "wxTOUCH_ALL_GESTURES", WXE_ATOM_undefined },
#endif
//  From "window.h"
    { enif_make_atom(rt.env,"wxSEND"), "wxSEND_EVENT_POST", rt.make_int(wxSEND_EVENT_POST) },
//  From "accel.h": wxAcceleratorEntryFlags
    { enif_make_atom(rt.env,"wxAcceleratorEntryFlags"), "wxACCEL_NORMAL", rt.make_int(wxACCEL_NORMAL) },
    { enif_make_atom(rt.env,"wxAcceleratorEntryFlags"), "wxACCEL_ALT", rt.make_int(wxACCEL_ALT) },
    { enif_make_atom(rt.env,"wxAcceleratorEntryFlags"), "wxACCEL_CTRL", rt.make_int(wxACCEL_CTRL) },
    { enif_make_atom(rt.env,"wxAcceleratorEntryFlags"), "wxACCEL_SHIFT", rt.make_int(wxACCEL_SHIFT) },
    { enif_make_atom(rt.env,"wxAcceleratorEntryFlags"), "wxACCEL_RAW_CTRL", rt.make_int(wxACCEL_RAW_CTRL) },
    { enif_make_atom(rt.env,"wxAcceleratorEntryFlags"), "wxACCEL_CMD", rt.make_int(wxACCEL_CMD) },
//  From "defs.h": wxAlignment
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_INVALID", rt.make_int(wxALIGN_INVALID) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_NOT", rt.make_int(wxALIGN_NOT) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_CENTER_HORIZONTAL", rt.make_int(wxALIGN_CENTER_HORIZONTAL) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_CENTRE_HORIZONTAL", rt.make_int(wxALIGN_CENTRE_HORIZONTAL) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_LEFT", rt.make_int(wxALIGN_LEFT) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_TOP", rt.make_int(wxALIGN_TOP) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_RIGHT", rt.make_int(wxALIGN_RIGHT) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_BOTTOM", rt.make_int(wxALIGN_BOTTOM) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_CENTER_VERTICAL", rt.make_int(wxALIGN_CENTER_VERTICAL) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_CENTRE_VERTICAL", rt.make_int(wxALIGN_CENTRE_VERTICAL) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_CENTER", rt.make_int(wxALIGN_CENTER) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_CENTRE", rt.make_int(wxALIGN_CENTRE) },
    { enif_make_atom(rt.env,"wxAlignment"), "wxALIGN_MASK", rt.make_int(wxALIGN_MASK) },
//  From "graphics.h": wxAntialiasMode
    { enif_make_atom(rt.env,"wxAntialiasMode"), "wxANTIALIAS_NONE", rt.make_int(wxANTIALIAS_NONE) },
    { enif_make_atom(rt.env,"wxAntialiasMode"), "wxANTIALIAS_DEFAULT", rt.make_int(wxANTIALIAS_DEFAULT) },
//  From "dockart.h": wxAuiButtonId
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_CLOSE", rt.make_int(wxAUI_BUTTON_CLOSE) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_MAXIMIZE_RESTORE", rt.make_int(wxAUI_BUTTON_MAXIMIZE_RESTORE) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_MINIMIZE", rt.make_int(wxAUI_BUTTON_MINIMIZE) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_PIN", rt.make_int(wxAUI_BUTTON_PIN) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_OPTIONS", rt.make_int(wxAUI_BUTTON_OPTIONS) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_WINDOWLIST", rt.make_int(wxAUI_BUTTON_WINDOWLIST) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_LEFT", rt.make_int(wxAUI_BUTTON_LEFT) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_RIGHT", rt.make_int(wxAUI_BUTTON_RIGHT) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_UP", rt.make_int(wxAUI_BUTTON_UP) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_DOWN", rt.make_int(wxAUI_BUTTON_DOWN) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_CUSTOM1", rt.make_int(wxAUI_BUTTON_CUSTOM1) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_CUSTOM2", rt.make_int(wxAUI_BUTTON_CUSTOM2) },
    { enif_make_atom(rt.env,"wxAuiButtonId"), "wxAUI_BUTTON_CUSTOM3", rt.make_int(wxAUI_BUTTON_CUSTOM3) },
//  From "framemanager.h": wxAuiInsertLevel
    { enif_make_atom(rt.env,"wxAuiInsertLevel"), "wxAUI_INSERT_PANE", rt.make_int(wxAUI_INSERT_PANE) },
    { enif_make_atom(rt.env,"wxAuiInsertLevel"), "wxAUI_INSERT_ROW", rt.make_int(wxAUI_INSERT_ROW) },
    { enif_make_atom(rt.env,"wxAuiInsertLevel"), "wxAUI_INSERT_DOCK", rt.make_int(wxAUI_INSERT_DOCK) },
//  From "framemanager.h": wxAuiManagerDock
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_NONE", rt.make_int(wxAUI_DOCK_NONE) },
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_TOP", rt.make_int(wxAUI_DOCK_TOP) },
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_RIGHT", rt.make_int(wxAUI_DOCK_RIGHT) },
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_BOTTOM", rt.make_int(wxAUI_DOCK_BOTTOM) },
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_LEFT", rt.make_int(wxAUI_DOCK_LEFT) },
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_CENTER", rt.make_int(wxAUI_DOCK_CENTER) },
    { enif_make_atom(rt.env,"wxAuiManagerDock"), "wxAUI_DOCK_CENTRE", rt.make_int(wxAUI_DOCK_CENTRE) },
//  From "framemanager.h": wxAuiManagerOption
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_ALLOW_FLOATING", rt.make_int(wxAUI_MGR_ALLOW_FLOATING) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_ALLOW_ACTIVE_PANE", rt.make_int(wxAUI_MGR_ALLOW_ACTIVE_PANE) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_TRANSPARENT_DRAG", rt.make_int(wxAUI_MGR_TRANSPARENT_DRAG) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_TRANSPARENT_HINT", rt.make_int(wxAUI_MGR_TRANSPARENT_HINT) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_VENETIAN_BLINDS_HINT", rt.make_int(wxAUI_MGR_VENETIAN_BLINDS_HINT) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_RECTANGLE_HINT", rt.make_int(wxAUI_MGR_RECTANGLE_HINT) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_HINT_FADE", rt.make_int(wxAUI_MGR_HINT_FADE) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_NO_VENETIAN_BLINDS_FADE", rt.make_int(wxAUI_MGR_NO_VENETIAN_BLINDS_FADE) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_LIVE_RESIZE", rt.make_int(wxAUI_MGR_LIVE_RESIZE) },
    { enif_make_atom(rt.env,"wxAuiManagerOption"), "wxAUI_MGR_DEFAULT", rt.make_int(wxAUI_MGR_DEFAULT) },
//  From "auibook.h": wxAuiNotebookOption
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_TOP", rt.make_int(wxAUI_NB_TOP) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_LEFT", rt.make_int(wxAUI_NB_LEFT) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_RIGHT", rt.make_int(wxAUI_NB_RIGHT) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_BOTTOM", rt.make_int(wxAUI_NB_BOTTOM) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_TAB_SPLIT", rt.make_int(wxAUI_NB_TAB_SPLIT) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_TAB_MOVE", rt.make_int(wxAUI_NB_TAB_MOVE) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_TAB_EXTERNAL_MOVE", rt.make_int(wxAUI_NB_TAB_EXTERNAL_MOVE) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_TAB_FIXED_WIDTH", rt.make_int(wxAUI_NB_TAB_FIXED_WIDTH) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_SCROLL_BUTTONS", rt.make_int(wxAUI_NB_SCROLL_BUTTONS) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_WINDOWLIST_BUTTON", rt.make_int(wxAUI_NB_WINDOWLIST_BUTTON) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_CLOSE_BUTTON", rt.make_int(wxAUI_NB_CLOSE_BUTTON) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_CLOSE_ON_ACTIVE_TAB", rt.make_int(wxAUI_NB_CLOSE_ON_ACTIVE_TAB) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_CLOSE_ON_ALL_TABS", rt.make_int(wxAUI_NB_CLOSE_ON_ALL_TABS) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_MIDDLE_CLICK_CLOSE", rt.make_int(wxAUI_NB_MIDDLE_CLICK_CLOSE) },
    { enif_make_atom(rt.env,"wxAuiNotebookOption"), "wxAUI_NB_DEFAULT_STYLE", rt.make_int(wxAUI_NB_DEFAULT_STYLE) },
//  From "dockart.h": wxAuiPaneButtonState
    { enif_make_atom(rt.env,"wxAuiPaneButtonState"), "wxAUI_BUTTON_STATE_NORMAL", rt.make_int(wxAUI_BUTTON_STATE_NORMAL) },
    { enif_make_atom(rt.env,"wxAuiPaneButtonState"), "wxAUI_BUTTON_STATE_HOVER", rt.make_int(wxAUI_BUTTON_STATE_HOVER) },
    { enif_make_atom(rt.env,"wxAuiPaneButtonState"), "wxAUI_BUTTON_STATE_PRESSED", rt.make_int(wxAUI_BUTTON_STATE_PRESSED) },
    { enif_make_atom(rt.env,"wxAuiPaneButtonState"), "wxAUI_BUTTON_STATE_DISABLED", rt.make_int(wxAUI_BUTTON_STATE_DISABLED) },
    { enif_make_atom(rt.env,"wxAuiPaneButtonState"), "wxAUI_BUTTON_STATE_HIDDEN", rt.make_int(wxAUI_BUTTON_STATE_HIDDEN) },
    { enif_make_atom(rt.env,"wxAuiPaneButtonState"), "wxAUI_BUTTON_STATE_CHECKED", rt.make_int(wxAUI_BUTTON_STATE_CHECKED) },
//  From "dockart.h": wxAuiPaneDockArtGradients
    { enif_make_atom(rt.env,"wxAuiPaneDockArtGradients"), "wxAUI_GRADIENT_NONE", rt.make_int(wxAUI_GRADIENT_NONE) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtGradients"), "wxAUI_GRADIENT_VERTICAL", rt.make_int(wxAUI_GRADIENT_VERTICAL) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtGradients"), "wxAUI_GRADIENT_HORIZONTAL", rt.make_int(wxAUI_GRADIENT_HORIZONTAL) },
//  From "dockart.h": wxAuiPaneDockArtSetting
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_SASH_SIZE", rt.make_int(wxAUI_DOCKART_SASH_SIZE) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_CAPTION_SIZE", rt.make_int(wxAUI_DOCKART_CAPTION_SIZE) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_GRIPPER_SIZE", rt.make_int(wxAUI_DOCKART_GRIPPER_SIZE) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_PANE_BORDER_SIZE", rt.make_int(wxAUI_DOCKART_PANE_BORDER_SIZE) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_PANE_BUTTON_SIZE", rt.make_int(wxAUI_DOCKART_PANE_BUTTON_SIZE) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_BACKGROUND_COLOUR", rt.make_int(wxAUI_DOCKART_BACKGROUND_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_SASH_COLOUR", rt.make_int(wxAUI_DOCKART_SASH_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_ACTIVE_CAPTION_COLOUR", rt.make_int(wxAUI_DOCKART_ACTIVE_CAPTION_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_ACTIVE_CAPTION_GRADIENT_COLOUR", rt.make_int(wxAUI_DOCKART_ACTIVE_CAPTION_GRADIENT_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_INACTIVE_CAPTION_COLOUR", rt.make_int(wxAUI_DOCKART_INACTIVE_CAPTION_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_INACTIVE_CAPTION_GRADIENT_COLOUR", rt.make_int(wxAUI_DOCKART_INACTIVE_CAPTION_GRADIENT_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_ACTIVE_CAPTION_TEXT_COLOUR", rt.make_int(wxAUI_DOCKART_ACTIVE_CAPTION_TEXT_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_INACTIVE_CAPTION_TEXT_COLOUR", rt.make_int(wxAUI_DOCKART_INACTIVE_CAPTION_TEXT_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_BORDER_COLOUR", rt.make_int(wxAUI_DOCKART_BORDER_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_GRIPPER_COLOUR", rt.make_int(wxAUI_DOCKART_GRIPPER_COLOUR) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_CAPTION_FONT", rt.make_int(wxAUI_DOCKART_CAPTION_FONT) },
    { enif_make_atom(rt.env,"wxAuiPaneDockArtSetting"), "wxAUI_DOCKART_GRADIENT_TYPE", rt.make_int(wxAUI_DOCKART_GRADIENT_TYPE) },
//  From "defs.h": wxBackgroundStyle
    { enif_make_atom(rt.env,"wxBackgroundStyle"), "wxBG_STYLE_ERASE", rt.make_int(wxBG_STYLE_ERASE) },
    { enif_make_atom(rt.env,"wxBackgroundStyle"), "wxBG_STYLE_SYSTEM", rt.make_int(wxBG_STYLE_SYSTEM) },
    { enif_make_atom(rt.env,"wxBackgroundStyle"), "wxBG_STYLE_PAINT", rt.make_int(wxBG_STYLE_PAINT) },
    { enif_make_atom(rt.env,"wxBackgroundStyle"), "wxBG_STYLE_COLOUR", rt.make_int(wxBG_STYLE_COLOUR) },
    { enif_make_atom(rt.env,"wxBackgroundStyle"), "wxBG_STYLE_TRANSPARENT", rt.make_int(wxBG_STYLE_TRANSPARENT) },
//  From "gdicmn.h": wxBitmapType
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_INVALID", rt.make_int(wxBITMAP_TYPE_INVALID) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_BMP", rt.make_int(wxBITMAP_TYPE_BMP) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_BMP_RESOURCE", rt.make_int(wxBITMAP_TYPE_BMP_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_RESOURCE", rt.make_int(wxBITMAP_TYPE_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_ICO", rt.make_int(wxBITMAP_TYPE_ICO) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_ICO_RESOURCE", rt.make_int(wxBITMAP_TYPE_ICO_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_CUR", rt.make_int(wxBITMAP_TYPE_CUR) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_CUR_RESOURCE", rt.make_int(wxBITMAP_TYPE_CUR_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_XBM", rt.make_int(wxBITMAP_TYPE_XBM) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_XBM_DATA", rt.make_int(wxBITMAP_TYPE_XBM_DATA) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_XPM", rt.make_int(wxBITMAP_TYPE_XPM) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_XPM_DATA", rt.make_int(wxBITMAP_TYPE_XPM_DATA) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_TIFF", rt.make_int(wxBITMAP_TYPE_TIFF) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_TIF", rt.make_int(wxBITMAP_TYPE_TIF) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_TIFF_RESOURCE", rt.make_int(wxBITMAP_TYPE_TIFF_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_TIF_RESOURCE", rt.make_int(wxBITMAP_TYPE_TIF_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_GIF", rt.make_int(wxBITMAP_TYPE_GIF) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_GIF_RESOURCE", rt.make_int(wxBITMAP_TYPE_GIF_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PNG", rt.make_int(wxBITMAP_TYPE_PNG) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PNG_RESOURCE", rt.make_int(wxBITMAP_TYPE_PNG_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_JPEG", rt.make_int(wxBITMAP_TYPE_JPEG) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_JPEG_RESOURCE", rt.make_int(wxBITMAP_TYPE_JPEG_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PNM", rt.make_int(wxBITMAP_TYPE_PNM) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PNM_RESOURCE", rt.make_int(wxBITMAP_TYPE_PNM_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PCX", rt.make_int(wxBITMAP_TYPE_PCX) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PCX_RESOURCE", rt.make_int(wxBITMAP_TYPE_PCX_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PICT", rt.make_int(wxBITMAP_TYPE_PICT) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_PICT_RESOURCE", rt.make_int(wxBITMAP_TYPE_PICT_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_ICON", rt.make_int(wxBITMAP_TYPE_ICON) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_ICON_RESOURCE", rt.make_int(wxBITMAP_TYPE_ICON_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_ANI", rt.make_int(wxBITMAP_TYPE_ANI) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_IFF", rt.make_int(wxBITMAP_TYPE_IFF) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_TGA", rt.make_int(wxBITMAP_TYPE_TGA) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_MACCURSOR", rt.make_int(wxBITMAP_TYPE_MACCURSOR) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_MACCURSOR_RESOURCE", rt.make_int(wxBITMAP_TYPE_MACCURSOR_RESOURCE) },
    { enif_make_atom(rt.env,"wxBitmapType"), "wxBITMAP_TYPE_ANY", rt.make_int(wxBITMAP_TYPE_ANY) },
//  From "defs.h": wxBorder
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_DEFAULT", rt.make_int(wxBORDER_DEFAULT) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_NONE", rt.make_int(wxBORDER_NONE) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_STATIC", rt.make_int(wxBORDER_STATIC) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_SIMPLE", rt.make_int(wxBORDER_SIMPLE) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_RAISED", rt.make_int(wxBORDER_RAISED) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_SUNKEN", rt.make_int(wxBORDER_SUNKEN) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_DOUBLE", rt.make_int(wxBORDER_DOUBLE) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_THEME", rt.make_int(wxBORDER_THEME) },
    { enif_make_atom(rt.env,"wxBorder"), "wxBORDER_MASK", rt.make_int(wxBORDER_MASK) },
//  From "brush.h": wxBrushStyle
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_INVALID", rt.make_int(wxBRUSHSTYLE_INVALID) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_SOLID", rt.make_int(wxBRUSHSTYLE_SOLID) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_TRANSPARENT", rt.make_int(wxBRUSHSTYLE_TRANSPARENT) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE", rt.make_int(wxBRUSHSTYLE_STIPPLE_MASK_OPAQUE) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_STIPPLE_MASK", rt.make_int(wxBRUSHSTYLE_STIPPLE_MASK) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_STIPPLE", rt.make_int(wxBRUSHSTYLE_STIPPLE) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_BDIAGONAL_HATCH", rt.make_int(wxBRUSHSTYLE_BDIAGONAL_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_CROSSDIAG_HATCH", rt.make_int(wxBRUSHSTYLE_CROSSDIAG_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_FDIAGONAL_HATCH", rt.make_int(wxBRUSHSTYLE_FDIAGONAL_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_CROSS_HATCH", rt.make_int(wxBRUSHSTYLE_CROSS_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_HORIZONTAL_HATCH", rt.make_int(wxBRUSHSTYLE_HORIZONTAL_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_VERTICAL_HATCH", rt.make_int(wxBRUSHSTYLE_VERTICAL_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_FIRST_HATCH", rt.make_int(wxBRUSHSTYLE_FIRST_HATCH) },
    { enif_make_atom(rt.env,"wxBrushStyle"), "wxBRUSHSTYLE_LAST_HATCH", rt.make_int(wxBRUSHSTYLE_LAST_HATCH) },
//  From "calctrl.h": wxCalendarDateBorder
    { enif_make_atom(rt.env,"wxCalendarDateBorder"), "wxCAL_BORDER_NONE", rt.make_int(wxCAL_BORDER_NONE) },
    { enif_make_atom(rt.env,"wxCalendarDateBorder"), "wxCAL_BORDER_SQUARE", rt.make_int(wxCAL_BORDER_SQUARE) },
    { enif_make_atom(rt.env,"wxCalendarDateBorder"), "wxCAL_BORDER_ROUND", rt.make_int(wxCAL_BORDER_ROUND) },
//  From "calctrl.h": wxCalendarHitTestResult
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_NOWHERE", rt.make_int(wxCAL_HITTEST_NOWHERE) },
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_HEADER", rt.make_int(wxCAL_HITTEST_HEADER) },
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_DAY", rt.make_int(wxCAL_HITTEST_DAY) },
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_INCMONTH", rt.make_int(wxCAL_HITTEST_INCMONTH) },
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_DECMONTH", rt.make_int(wxCAL_HITTEST_DECMONTH) },
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_SURROUNDING_WEEK", rt.make_int(wxCAL_HITTEST_SURROUNDING_WEEK) },
    { enif_make_atom(rt.env,"wxCalendarHitTestResult"), "wxCAL_HITTEST_WEEK", rt.make_int(wxCAL_HITTEST_WEEK) },
//  From "checkbox.h": wxCheckBoxState
    { enif_make_atom(rt.env,"wxCheckBoxState"), "wxCHK_UNCHECKED", rt.make_int(wxCHK_UNCHECKED) },
    { enif_make_atom(rt.env,"wxCheckBoxState"), "wxCHK_CHECKED", rt.make_int(wxCHK_CHECKED) },
    { enif_make_atom(rt.env,"wxCheckBoxState"), "wxCHK_UNDETERMINED", rt.make_int(wxCHK_UNDETERMINED) },
//  From "graphics.h": wxCompositionMode
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_INVALID", rt.make_int(wxCOMPOSITION_INVALID) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_CLEAR", rt.make_int(wxCOMPOSITION_CLEAR) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_SOURCE", rt.make_int(wxCOMPOSITION_SOURCE) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_OVER", rt.make_int(wxCOMPOSITION_OVER) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_IN", rt.make_int(wxCOMPOSITION_IN) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_OUT", rt.make_int(wxCOMPOSITION_OUT) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_ATOP", rt.make_int(wxCOMPOSITION_ATOP) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_DEST", rt.make_int(wxCOMPOSITION_DEST) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_DEST_OVER", rt.make_int(wxCOMPOSITION_DEST_OVER) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_DEST_IN", rt.make_int(wxCOMPOSITION_DEST_IN) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_DEST_OUT", rt.make_int(wxCOMPOSITION_DEST_OUT) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_DEST_ATOP", rt.make_int(wxCOMPOSITION_DEST_ATOP) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_XOR", rt.make_int(wxCOMPOSITION_XOR) },
    { enif_make_atom(rt.env,"wxCompositionMode"), "wxCOMPOSITION_ADD", rt.make_int(wxCOMPOSITION_ADD) },
//  From "defs.h": wxDataFormatId
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_INVALID", rt.make_int(wxDF_INVALID) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_TEXT", rt.make_int(wxDF_TEXT) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_BITMAP", rt.make_int(wxDF_BITMAP) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_METAFILE", rt.make_int(wxDF_METAFILE) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_SYLK", rt.make_int(wxDF_SYLK) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_DIF", rt.make_int(wxDF_DIF) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_TIFF", rt.make_int(wxDF_TIFF) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_OEMTEXT", rt.make_int(wxDF_OEMTEXT) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_DIB", rt.make_int(wxDF_DIB) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_PALETTE", rt.make_int(wxDF_PALETTE) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_PENDATA", rt.make_int(wxDF_PENDATA) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_RIFF", rt.make_int(wxDF_RIFF) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_WAVE", rt.make_int(wxDF_WAVE) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_UNICODETEXT", rt.make_int(wxDF_UNICODETEXT) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_ENHMETAFILE", rt.make_int(wxDF_ENHMETAFILE) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_FILENAME", rt.make_int(wxDF_FILENAME) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_LOCALE", rt.make_int(wxDF_LOCALE) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_PRIVATE", rt.make_int(wxDF_PRIVATE) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_HTML", rt.make_int(wxDF_HTML) },
    { enif_make_atom(rt.env,"wxDataFormatId"), "wxDF_MAX", rt.make_int(wxDF_MAX) },
//  From "defs.h": wxDeprecatedGUIConstants
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxDEFAULT", rt.make_int(wxDEFAULT) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxDECORATIVE", rt.make_int(wxDECORATIVE) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxROMAN", rt.make_int(wxROMAN) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSCRIPT", rt.make_int(wxSCRIPT) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSWISS", rt.make_int(wxSWISS) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxMODERN", rt.make_int(wxMODERN) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxTELETYPE", rt.make_int(wxTELETYPE) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxVARIABLE", rt.make_int(wxVARIABLE) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxFIXED", rt.make_int(wxFIXED) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxNORMAL", rt.make_int(wxNORMAL) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxLIGHT", rt.make_int(wxLIGHT) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxBOLD", rt.make_int(wxBOLD) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxITALIC", rt.make_int(wxITALIC) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSLANT", rt.make_int(wxSLANT) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSOLID", rt.make_int(wxSOLID) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxDOT", rt.make_int(wxDOT) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxLONG_DASH", rt.make_int(wxLONG_DASH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSHORT_DASH", rt.make_int(wxSHORT_DASH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxDOT_DASH", rt.make_int(wxDOT_DASH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxUSER_DASH", rt.make_int(wxUSER_DASH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxTRANSPARENT", rt.make_int(wxTRANSPARENT) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSTIPPLE_MASK_OPAQUE", rt.make_int(wxSTIPPLE_MASK_OPAQUE) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSTIPPLE_MASK", rt.make_int(wxSTIPPLE_MASK) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxSTIPPLE", rt.make_int(wxSTIPPLE) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxBDIAGONAL_HATCH", rt.make_int(wxBDIAGONAL_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxCROSSDIAG_HATCH", rt.make_int(wxCROSSDIAG_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxFDIAGONAL_HATCH", rt.make_int(wxFDIAGONAL_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxCROSS_HATCH", rt.make_int(wxCROSS_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxHORIZONTAL_HATCH", rt.make_int(wxHORIZONTAL_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxVERTICAL_HATCH", rt.make_int(wxVERTICAL_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxFIRST_HATCH", rt.make_int(wxFIRST_HATCH) },
    { enif_make_atom(rt.env,"wxDeprecatedGUIConstants"), "wxLAST_HATCH", rt.make_int(wxLAST_HATCH) },
//  From "dialog.h": wxDialogLayoutAdaptationMode
    { enif_make_atom(rt.env,"wxDialogLayoutAdaptationMode"), "wxDIALOG_ADAPTATION_MODE_DEFAULT", rt.make_int(wxDIALOG_ADAPTATION_MODE_DEFAULT) },
    { enif_make_atom(rt.env,"wxDialogLayoutAdaptationMode"), "wxDIALOG_ADAPTATION_MODE_ENABLED", rt.make_int(wxDIALOG_ADAPTATION_MODE_ENABLED) },
    { enif_make_atom(rt.env,"wxDialogLayoutAdaptationMode"), "wxDIALOG_ADAPTATION_MODE_DISABLED", rt.make_int(wxDIALOG_ADAPTATION_MODE_DISABLED) },
//  From "defs.h": wxDirection
    { enif_make_atom(rt.env,"wxDirection"), "wxLEFT", rt.make_int(wxLEFT) },
    { enif_make_atom(rt.env,"wxDirection"), "wxRIGHT", rt.make_int(wxRIGHT) },
    { enif_make_atom(rt.env,"wxDirection"), "wxUP", rt.make_int(wxUP) },
    { enif_make_atom(rt.env,"wxDirection"), "wxDOWN", rt.make_int(wxDOWN) },
    { enif_make_atom(rt.env,"wxDirection"), "wxTOP", rt.make_int(wxTOP) },
    { enif_make_atom(rt.env,"wxDirection"), "wxBOTTOM", rt.make_int(wxBOTTOM) },
    { enif_make_atom(rt.env,"wxDirection"), "wxNORTH", rt.make_int(wxNORTH) },
    { enif_make_atom(rt.env,"wxDirection"), "wxSOUTH", rt.make_int(wxSOUTH) },
    { enif_make_atom(rt.env,"wxDirection"), "wxWEST", rt.make_int(wxWEST) },
    { enif_make_atom(rt.env,"wxDirection"), "wxEAST", rt.make_int(wxEAST) },
    { enif_make_atom(rt.env,"wxDirection"), "wxALL", rt.make_int(wxALL) },
    { enif_make_atom(rt.env,"wxDirection"), "wxDIRECTION_MASK", rt.make_int(wxDIRECTION_MASK) },
//  From "dnd.h": wxDragResult
    { enif_make_atom(rt.env,"wxDragResult"), "wxDragError", rt.make_int(wxDragError) },
    { enif_make_atom(rt.env,"wxDragResult"), "wxDragNone", rt.make_int(wxDragNone) },
    { enif_make_atom(rt.env,"wxDragResult"), "wxDragCopy", rt.make_int(wxDragCopy) },
    { enif_make_atom(rt.env,"wxDragResult"), "wxDragMove", rt.make_int(wxDragMove) },
    { enif_make_atom(rt.env,"wxDragResult"), "wxDragLink", rt.make_int(wxDragLink) },
    { enif_make_atom(rt.env,"wxDragResult"), "wxDragCancel", rt.make_int(wxDragCancel) },
//  From "defs.h": wxDuplexMode
    { enif_make_atom(rt.env,"wxDuplexMode"), "wxDUPLEX_SIMPLEX", rt.make_int(wxDUPLEX_SIMPLEX) },
    { enif_make_atom(rt.env,"wxDuplexMode"), "wxDUPLEX_HORIZONTAL", rt.make_int(wxDUPLEX_HORIZONTAL) },
    { enif_make_atom(rt.env,"wxDuplexMode"), "wxDUPLEX_VERTICAL", rt.make_int(wxDUPLEX_VERTICAL) },
//  From "layout.h": wxEdge
    { enif_make_atom(rt.env,"wxEdge"), "wxLeft", rt.make_int(wxLeft) },
    { enif_make_atom(rt.env,"wxEdge"), "wxTop", rt.make_int(wxTop) },
    { enif_make_atom(rt.env,"wxEdge"), "wxRight", rt.make_int(wxRight) },
    { enif_make_atom(rt.env,"wxEdge"), "wxBottom", rt.make_int(wxBottom) },
    { enif_make_atom(rt.env,"wxEdge"), "wxWidth", rt.make_int(wxWidth) },
    { enif_make_atom(rt.env,"wxEdge"), "wxHeight", rt.make_int(wxHeight) },
    { enif_make_atom(rt.env,"wxEdge"), "wxCentre", rt.make_int(wxCentre) },
    { enif_make_atom(rt.env,"wxEdge"), "wxCenter", rt.make_int(wxCenter) },
    { enif_make_atom(rt.env,"wxEdge"), "wxCentreX", rt.make_int(wxCentreX) },
    { enif_make_atom(rt.env,"wxEdge"), "wxCentreY", rt.make_int(wxCentreY) },
//  From "gdicmn.h": wxEllipsizeFlags
    { enif_make_atom(rt.env,"wxEllipsizeFlags"), "wxELLIPSIZE_FLAGS_NONE", rt.make_int(wxELLIPSIZE_FLAGS_NONE) },
    { enif_make_atom(rt.env,"wxEllipsizeFlags"), "wxELLIPSIZE_FLAGS_PROCESS_MNEMONICS", rt.make_int(wxELLIPSIZE_FLAGS_PROCESS_MNEMONICS) },
    { enif_make_atom(rt.env,"wxEllipsizeFlags"), "wxELLIPSIZE_FLAGS_EXPAND_TABS", rt.make_int(wxELLIPSIZE_FLAGS_EXPAND_TABS) },
    { enif_make_atom(rt.env,"wxEllipsizeFlags"), "wxELLIPSIZE_FLAGS_DEFAULT", rt.make_int(wxELLIPSIZE_FLAGS_DEFAULT) },
//  From "gdicmn.h": wxEllipsizeMode
    { enif_make_atom(rt.env,"wxEllipsizeMode"), "wxELLIPSIZE_NONE", rt.make_int(wxELLIPSIZE_NONE) },
    { enif_make_atom(rt.env,"wxEllipsizeMode"), "wxELLIPSIZE_START", rt.make_int(wxELLIPSIZE_START) },
    { enif_make_atom(rt.env,"wxEllipsizeMode"), "wxELLIPSIZE_MIDDLE", rt.make_int(wxELLIPSIZE_MIDDLE) },
    { enif_make_atom(rt.env,"wxEllipsizeMode"), "wxELLIPSIZE_END", rt.make_int(wxELLIPSIZE_END) },
//  From "event.h": wxEventCategory
    { enif_make_atom(rt.env,"wxEventCategory"), "wxEVT_CATEGORY_UI", rt.make_int(wxEVT_CATEGORY_UI) },
    { enif_make_atom(rt.env,"wxEventCategory"), "wxEVT_CATEGORY_USER_INPUT", rt.make_int(wxEVT_CATEGORY_USER_INPUT) },
    { enif_make_atom(rt.env,"wxEventCategory"), "wxEVT_CATEGORY_SOCKET", rt.make_int(wxEVT_CATEGORY_SOCKET) },
    { enif_make_atom(rt.env,"wxEventCategory"), "wxEVT_CATEGORY_TIMER", rt.make_int(wxEVT_CATEGORY_TIMER) },
    { enif_make_atom(rt.env,"wxEventCategory"), "wxEVT_CATEGORY_THREAD", rt.make_int(wxEVT_CATEGORY_THREAD) },
    { enif_make_atom(rt.env,"wxEventCategory"), "wxEVT_CATEGORY_ALL", rt.make_int(wxEVT_CATEGORY_ALL) },
//  From "event.h": wxEventPropagation
    { enif_make_atom(rt.env,"wxEventPropagation"), "wxEVENT_PROPAGATE_NONE", rt.make_int(wxEVENT_PROPAGATE_NONE) },
    { enif_make_atom(rt.env,"wxEventPropagation"), "wxEVENT_PROPAGATE_MAX", rt.make_int(wxEVENT_PROPAGATE_MAX) },
//  From "fdrepdlg.h": wxFindReplaceDialogStyles
    { enif_make_atom(rt.env,"wxFindReplaceDialogStyles"), "wxFR_REPLACEDIALOG", rt.make_int(wxFR_REPLACEDIALOG) },
    { enif_make_atom(rt.env,"wxFindReplaceDialogStyles"), "wxFR_NOUPDOWN", rt.make_int(wxFR_NOUPDOWN) },
    { enif_make_atom(rt.env,"wxFindReplaceDialogStyles"), "wxFR_NOMATCHCASE", rt.make_int(wxFR_NOMATCHCASE) },
    { enif_make_atom(rt.env,"wxFindReplaceDialogStyles"), "wxFR_NOWHOLEWORD", rt.make_int(wxFR_NOWHOLEWORD) },
//  From "fdrepdlg.h": wxFindReplaceFlags
    { enif_make_atom(rt.env,"wxFindReplaceFlags"), "wxFR_DOWN", rt.make_int(wxFR_DOWN) },
    { enif_make_atom(rt.env,"wxFindReplaceFlags"), "wxFR_WHOLEWORD", rt.make_int(wxFR_WHOLEWORD) },
    { enif_make_atom(rt.env,"wxFindReplaceFlags"), "wxFR_MATCHCASE", rt.make_int(wxFR_MATCHCASE) },
//  From "sizer.h": wxFlexSizerGrowMode
    { enif_make_atom(rt.env,"wxFlexSizerGrowMode"), "wxFLEX_GROWMODE_NONE", rt.make_int(wxFLEX_GROWMODE_NONE) },
    { enif_make_atom(rt.env,"wxFlexSizerGrowMode"), "wxFLEX_GROWMODE_SPECIFIED", rt.make_int(wxFLEX_GROWMODE_SPECIFIED) },
    { enif_make_atom(rt.env,"wxFlexSizerGrowMode"), "wxFLEX_GROWMODE_ALL", rt.make_int(wxFLEX_GROWMODE_ALL) },
//  From "dc.h": wxFloodFillStyle
    { enif_make_atom(rt.env,"wxFloodFillStyle"), "wxFLOOD_SURFACE", rt.make_int(wxFLOOD_SURFACE) },
    { enif_make_atom(rt.env,"wxFloodFillStyle"), "wxFLOOD_BORDER", rt.make_int(wxFLOOD_BORDER) },
//  From "font.h": wxFontEncoding
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_SYSTEM", rt.make_int(wxFONTENCODING_SYSTEM) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_DEFAULT", rt.make_int(wxFONTENCODING_DEFAULT) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_1", rt.make_int(wxFONTENCODING_ISO8859_1) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_2", rt.make_int(wxFONTENCODING_ISO8859_2) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_3", rt.make_int(wxFONTENCODING_ISO8859_3) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_4", rt.make_int(wxFONTENCODING_ISO8859_4) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_5", rt.make_int(wxFONTENCODING_ISO8859_5) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_6", rt.make_int(wxFONTENCODING_ISO8859_6) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_7", rt.make_int(wxFONTENCODING_ISO8859_7) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_8", rt.make_int(wxFONTENCODING_ISO8859_8) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_9", rt.make_int(wxFONTENCODING_ISO8859_9) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_10", rt.make_int(wxFONTENCODING_ISO8859_10) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_11", rt.make_int(wxFONTENCODING_ISO8859_11) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_12", rt.make_int(wxFONTENCODING_ISO8859_12) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_13", rt.make_int(wxFONTENCODING_ISO8859_13) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_14", rt.make_int(wxFONTENCODING_ISO8859_14) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_15", rt.make_int(wxFONTENCODING_ISO8859_15) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO8859_MAX", rt.make_int(wxFONTENCODING_ISO8859_MAX) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_KOI8", rt.make_int(wxFONTENCODING_KOI8) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_KOI8_U", rt.make_int(wxFONTENCODING_KOI8_U) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ALTERNATIVE", rt.make_int(wxFONTENCODING_ALTERNATIVE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_BULGARIAN", rt.make_int(wxFONTENCODING_BULGARIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP437", rt.make_int(wxFONTENCODING_CP437) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP850", rt.make_int(wxFONTENCODING_CP850) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP852", rt.make_int(wxFONTENCODING_CP852) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP855", rt.make_int(wxFONTENCODING_CP855) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP866", rt.make_int(wxFONTENCODING_CP866) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP874", rt.make_int(wxFONTENCODING_CP874) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP932", rt.make_int(wxFONTENCODING_CP932) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP936", rt.make_int(wxFONTENCODING_CP936) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP949", rt.make_int(wxFONTENCODING_CP949) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP950", rt.make_int(wxFONTENCODING_CP950) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1250", rt.make_int(wxFONTENCODING_CP1250) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1251", rt.make_int(wxFONTENCODING_CP1251) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1252", rt.make_int(wxFONTENCODING_CP1252) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1253", rt.make_int(wxFONTENCODING_CP1253) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1254", rt.make_int(wxFONTENCODING_CP1254) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1255", rt.make_int(wxFONTENCODING_CP1255) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1256", rt.make_int(wxFONTENCODING_CP1256) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1257", rt.make_int(wxFONTENCODING_CP1257) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1258", rt.make_int(wxFONTENCODING_CP1258) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP1361", rt.make_int(wxFONTENCODING_CP1361) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_CP12_MAX", rt.make_int(wxFONTENCODING_CP12_MAX) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF7", rt.make_int(wxFONTENCODING_UTF7) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF8", rt.make_int(wxFONTENCODING_UTF8) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_EUC_JP", rt.make_int(wxFONTENCODING_EUC_JP) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF16BE", rt.make_int(wxFONTENCODING_UTF16BE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF16LE", rt.make_int(wxFONTENCODING_UTF16LE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF32BE", rt.make_int(wxFONTENCODING_UTF32BE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF32LE", rt.make_int(wxFONTENCODING_UTF32LE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACROMAN", rt.make_int(wxFONTENCODING_MACROMAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACJAPANESE", rt.make_int(wxFONTENCODING_MACJAPANESE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACCHINESETRAD", rt.make_int(wxFONTENCODING_MACCHINESETRAD) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACKOREAN", rt.make_int(wxFONTENCODING_MACKOREAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACARABIC", rt.make_int(wxFONTENCODING_MACARABIC) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACHEBREW", rt.make_int(wxFONTENCODING_MACHEBREW) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACGREEK", rt.make_int(wxFONTENCODING_MACGREEK) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACCYRILLIC", rt.make_int(wxFONTENCODING_MACCYRILLIC) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACDEVANAGARI", rt.make_int(wxFONTENCODING_MACDEVANAGARI) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACGURMUKHI", rt.make_int(wxFONTENCODING_MACGURMUKHI) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACGUJARATI", rt.make_int(wxFONTENCODING_MACGUJARATI) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACORIYA", rt.make_int(wxFONTENCODING_MACORIYA) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACBENGALI", rt.make_int(wxFONTENCODING_MACBENGALI) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACTAMIL", rt.make_int(wxFONTENCODING_MACTAMIL) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACTELUGU", rt.make_int(wxFONTENCODING_MACTELUGU) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACKANNADA", rt.make_int(wxFONTENCODING_MACKANNADA) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACMALAJALAM", rt.make_int(wxFONTENCODING_MACMALAJALAM) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACSINHALESE", rt.make_int(wxFONTENCODING_MACSINHALESE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACBURMESE", rt.make_int(wxFONTENCODING_MACBURMESE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACKHMER", rt.make_int(wxFONTENCODING_MACKHMER) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACTHAI", rt.make_int(wxFONTENCODING_MACTHAI) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACLAOTIAN", rt.make_int(wxFONTENCODING_MACLAOTIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACGEORGIAN", rt.make_int(wxFONTENCODING_MACGEORGIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACARMENIAN", rt.make_int(wxFONTENCODING_MACARMENIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACCHINESESIMP", rt.make_int(wxFONTENCODING_MACCHINESESIMP) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACTIBETAN", rt.make_int(wxFONTENCODING_MACTIBETAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACMONGOLIAN", rt.make_int(wxFONTENCODING_MACMONGOLIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACETHIOPIC", rt.make_int(wxFONTENCODING_MACETHIOPIC) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACCENTRALEUR", rt.make_int(wxFONTENCODING_MACCENTRALEUR) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACVIATNAMESE", rt.make_int(wxFONTENCODING_MACVIATNAMESE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACARABICEXT", rt.make_int(wxFONTENCODING_MACARABICEXT) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACSYMBOL", rt.make_int(wxFONTENCODING_MACSYMBOL) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACDINGBATS", rt.make_int(wxFONTENCODING_MACDINGBATS) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACTURKISH", rt.make_int(wxFONTENCODING_MACTURKISH) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACCROATIAN", rt.make_int(wxFONTENCODING_MACCROATIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACICELANDIC", rt.make_int(wxFONTENCODING_MACICELANDIC) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACROMANIAN", rt.make_int(wxFONTENCODING_MACROMANIAN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACCELTIC", rt.make_int(wxFONTENCODING_MACCELTIC) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACGAELIC", rt.make_int(wxFONTENCODING_MACGAELIC) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACKEYBOARD", rt.make_int(wxFONTENCODING_MACKEYBOARD) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_ISO2022_JP", rt.make_int(wxFONTENCODING_ISO2022_JP) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MAX", rt.make_int(wxFONTENCODING_MAX) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACMIN", rt.make_int(wxFONTENCODING_MACMIN) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_MACMAX", rt.make_int(wxFONTENCODING_MACMAX) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF16", rt.make_int(wxFONTENCODING_UTF16) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UTF32", rt.make_int(wxFONTENCODING_UTF32) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_UNICODE", rt.make_int(wxFONTENCODING_UNICODE) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_GB2312", rt.make_int(wxFONTENCODING_GB2312) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_BIG5", rt.make_int(wxFONTENCODING_BIG5) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_SHIFT_JIS", rt.make_int(wxFONTENCODING_SHIFT_JIS) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_EUC_KR", rt.make_int(wxFONTENCODING_EUC_KR) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_JOHAB", rt.make_int(wxFONTENCODING_JOHAB) },
    { enif_make_atom(rt.env,"wxFontEncoding"), "wxFONTENCODING_VIETNAMESE", rt.make_int(wxFONTENCODING_VIETNAMESE) },
//  From "font.h": wxFontFamily
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_DEFAULT", rt.make_int(wxFONTFAMILY_DEFAULT) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_DECORATIVE", rt.make_int(wxFONTFAMILY_DECORATIVE) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_ROMAN", rt.make_int(wxFONTFAMILY_ROMAN) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_SCRIPT", rt.make_int(wxFONTFAMILY_SCRIPT) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_SWISS", rt.make_int(wxFONTFAMILY_SWISS) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_MODERN", rt.make_int(wxFONTFAMILY_MODERN) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_TELETYPE", rt.make_int(wxFONTFAMILY_TELETYPE) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_MAX", rt.make_int(wxFONTFAMILY_MAX) },
    { enif_make_atom(rt.env,"wxFontFamily"), "wxFONTFAMILY_UNKNOWN", rt.make_int(wxFONTFAMILY_UNKNOWN) },
//  From "font.h": wxFontFlag
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_DEFAULT", rt.make_int(wxFONTFLAG_DEFAULT) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_ITALIC", rt.make_int(wxFONTFLAG_ITALIC) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_SLANT", rt.make_int(wxFONTFLAG_SLANT) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_LIGHT", rt.make_int(wxFONTFLAG_LIGHT) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_BOLD", rt.make_int(wxFONTFLAG_BOLD) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_ANTIALIASED", rt.make_int(wxFONTFLAG_ANTIALIASED) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_NOT_ANTIALIASED", rt.make_int(wxFONTFLAG_NOT_ANTIALIASED) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_UNDERLINED", rt.make_int(wxFONTFLAG_UNDERLINED) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_STRIKETHROUGH", rt.make_int(wxFONTFLAG_STRIKETHROUGH) },
    { enif_make_atom(rt.env,"wxFontFlag"), "wxFONTFLAG_MASK", rt.make_int(wxFONTFLAG_MASK) },
//  From "font.h": wxFontStyle
    { enif_make_atom(rt.env,"wxFontStyle"), "wxFONTSTYLE_NORMAL", rt.make_int(wxFONTSTYLE_NORMAL) },
    { enif_make_atom(rt.env,"wxFontStyle"), "wxFONTSTYLE_ITALIC", rt.make_int(wxFONTSTYLE_ITALIC) },
    { enif_make_atom(rt.env,"wxFontStyle"), "wxFONTSTYLE_SLANT", rt.make_int(wxFONTSTYLE_SLANT) },
    { enif_make_atom(rt.env,"wxFontStyle"), "wxFONTSTYLE_MAX", rt.make_int(wxFONTSTYLE_MAX) },
//  From "font.h": wxFontSymbolicSize
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_XX_SMALL", rt.make_int(wxFONTSIZE_XX_SMALL) },
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_X_SMALL", rt.make_int(wxFONTSIZE_X_SMALL) },
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_SMALL", rt.make_int(wxFONTSIZE_SMALL) },
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_MEDIUM", rt.make_int(wxFONTSIZE_MEDIUM) },
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_LARGE", rt.make_int(wxFONTSIZE_LARGE) },
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_X_LARGE", rt.make_int(wxFONTSIZE_X_LARGE) },
    { enif_make_atom(rt.env,"wxFontSymbolicSize"), "wxFONTSIZE_XX_LARGE", rt.make_int(wxFONTSIZE_XX_LARGE) },
//  From "font.h": wxFontWeight
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_INVALID", rt.make_int(wxFONTWEIGHT_INVALID) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_INVALID", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_THIN", rt.make_int(wxFONTWEIGHT_THIN) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_THIN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_EXTRALIGHT", rt.make_int(wxFONTWEIGHT_EXTRALIGHT) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_EXTRALIGHT", WXE_ATOM_undefined },
#endif
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_LIGHT", rt.make_int(wxFONTWEIGHT_LIGHT) },
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_NORMAL", rt.make_int(wxFONTWEIGHT_NORMAL) },
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_MEDIUM", rt.make_int(wxFONTWEIGHT_MEDIUM) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_MEDIUM", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_SEMIBOLD", rt.make_int(wxFONTWEIGHT_SEMIBOLD) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_SEMIBOLD", WXE_ATOM_undefined },
#endif
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_BOLD", rt.make_int(wxFONTWEIGHT_BOLD) },
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_EXTRABOLD", rt.make_int(wxFONTWEIGHT_EXTRABOLD) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_EXTRABOLD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_HEAVY", rt.make_int(wxFONTWEIGHT_HEAVY) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_HEAVY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_EXTRAHEAVY", rt.make_int(wxFONTWEIGHT_EXTRAHEAVY) },
#else
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_EXTRAHEAVY", WXE_ATOM_undefined },
#endif
    { enif_make_atom(rt.env,"wxFontWeight"), "wxFONTWEIGHT_MAX", rt.make_int(wxFONTWEIGHT_MAX) },
//  From "glcanvas.h": wxGL_FLAGS
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RGBA", rt.make_int(WX_GL_RGBA) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_BUFFER_SIZE", rt.make_int(WX_GL_BUFFER_SIZE) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_LEVEL", rt.make_int(WX_GL_LEVEL) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_DOUBLEBUFFER", rt.make_int(WX_GL_DOUBLEBUFFER) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_STEREO", rt.make_int(WX_GL_STEREO) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_AUX_BUFFERS", rt.make_int(WX_GL_AUX_BUFFERS) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_RED", rt.make_int(WX_GL_MIN_RED) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_GREEN", rt.make_int(WX_GL_MIN_GREEN) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_BLUE", rt.make_int(WX_GL_MIN_BLUE) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_ALPHA", rt.make_int(WX_GL_MIN_ALPHA) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_DEPTH_SIZE", rt.make_int(WX_GL_DEPTH_SIZE) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_STENCIL_SIZE", rt.make_int(WX_GL_STENCIL_SIZE) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_ACCUM_RED", rt.make_int(WX_GL_MIN_ACCUM_RED) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_ACCUM_GREEN", rt.make_int(WX_GL_MIN_ACCUM_GREEN) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_ACCUM_BLUE", rt.make_int(WX_GL_MIN_ACCUM_BLUE) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MIN_ACCUM_ALPHA", rt.make_int(WX_GL_MIN_ACCUM_ALPHA) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_SAMPLE_BUFFERS", rt.make_int(WX_GL_SAMPLE_BUFFERS) },
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_SAMPLES", rt.make_int(WX_GL_SAMPLES) },
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_FRAMEBUFFER_SRGB", rt.make_int(WX_GL_FRAMEBUFFER_SRGB) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_FRAMEBUFFER_SRGB", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,0,3)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MAJOR_VERSION", rt.make_int(WX_GL_MAJOR_VERSION) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MAJOR_VERSION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,0,3)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MINOR_VERSION", rt.make_int(WX_GL_MINOR_VERSION) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_MINOR_VERSION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,0,3)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_CORE_PROFILE", rt.make_int(WX_GL_CORE_PROFILE) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_CORE_PROFILE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_COMPAT_PROFILE", rt.make_int(wx_GL_COMPAT_PROFILE) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_COMPAT_PROFILE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_FORWARD_COMPAT", rt.make_int(WX_GL_FORWARD_COMPAT) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_FORWARD_COMPAT", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_ES2", rt.make_int(WX_GL_ES2) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_ES2", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_DEBUG", rt.make_int(WX_GL_DEBUG) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_DEBUG", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_ROBUST_ACCESS", rt.make_int(WX_GL_ROBUST_ACCESS) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_ROBUST_ACCESS", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_NO_RESET_NOTIFY", rt.make_int(WX_GL_NO_RESET_NOTIFY) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_NO_RESET_NOTIFY", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_LOSE_ON_RESET", rt.make_int(WX_GL_LOSE_ON_RESET) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_LOSE_ON_RESET", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RESET_ISOLATION", rt.make_int(WX_GL_RESET_ISOLATION) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RESET_ISOLATION", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RELEASE_FLUSH", rt.make_int(WX_GL_RELEASE_FLUSH) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RELEASE_FLUSH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RELEASE_NONE", rt.make_int(WX_GL_RELEASE_NONE) },
#else
    { enif_make_atom(rt.env,"wxGL_FLAGS"), "wx_GL_RELEASE_NONE", WXE_ATOM_undefined },
#endif
//  From "defs.h": wxGeometryCentre
    { enif_make_atom(rt.env,"wxGeometryCentre"), "wxCENTRE", rt.make_int(wxCENTRE) },
    { enif_make_atom(rt.env,"wxGeometryCentre"), "wxCENTER", rt.make_int(wxCENTER) },
//  From "graphics.h": wxGradientType
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxGradientType"), "wxGRADIENT_NONE", rt.make_int(wxGRADIENT_NONE) },
#else
    { enif_make_atom(rt.env,"wxGradientType"), "wxGRADIENT_NONE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxGradientType"), "wxGRADIENT_LINEAR", rt.make_int(wxGRADIENT_LINEAR) },
#else
    { enif_make_atom(rt.env,"wxGradientType"), "wxGRADIENT_LINEAR", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxGradientType"), "wxGRADIENT_RADIAL", rt.make_int(wxGRADIENT_RADIAL) },
#else
    { enif_make_atom(rt.env,"wxGradientType"), "wxGRADIENT_RADIAL", WXE_ATOM_undefined },
#endif
//  From "grid.h": wxGridCellFloatFormat
    { enif_make_atom(rt.env,"wxGridCellFloatFormat"), "wxGRID_FLOAT_FORMAT_FIXED", rt.make_int(wxGRID_FLOAT_FORMAT_FIXED) },
    { enif_make_atom(rt.env,"wxGridCellFloatFormat"), "wxGRID_FLOAT_FORMAT_SCIENTIFIC", rt.make_int(wxGRID_FLOAT_FORMAT_SCIENTIFIC) },
    { enif_make_atom(rt.env,"wxGridCellFloatFormat"), "wxGRID_FLOAT_FORMAT_COMPACT", rt.make_int(wxGRID_FLOAT_FORMAT_COMPACT) },
    { enif_make_atom(rt.env,"wxGridCellFloatFormat"), "wxGRID_FLOAT_FORMAT_UPPER", rt.make_int(wxGRID_FLOAT_FORMAT_UPPER) },
    { enif_make_atom(rt.env,"wxGridCellFloatFormat"), "wxGRID_FLOAT_FORMAT_DEFAULT", rt.make_int(wxGRID_FLOAT_FORMAT_DEFAULT) },
//  From "grid.h": wxGridRenderStyle
    { enif_make_atom(rt.env,"wxGridRenderStyle"), "wxGRID_DRAW_ROWS_HEADER", rt.make_int(wxGRID_DRAW_ROWS_HEADER) },
    { enif_make_atom(rt.env,"wxGridRenderStyle"), "wxGRID_DRAW_COLS_HEADER", rt.make_int(wxGRID_DRAW_COLS_HEADER) },
    { enif_make_atom(rt.env,"wxGridRenderStyle"), "wxGRID_DRAW_CELL_LINES", rt.make_int(wxGRID_DRAW_CELL_LINES) },
    { enif_make_atom(rt.env,"wxGridRenderStyle"), "wxGRID_DRAW_BOX_RECT", rt.make_int(wxGRID_DRAW_BOX_RECT) },
    { enif_make_atom(rt.env,"wxGridRenderStyle"), "wxGRID_DRAW_SELECTION", rt.make_int(wxGRID_DRAW_SELECTION) },
    { enif_make_atom(rt.env,"wxGridRenderStyle"), "wxGRID_DRAW_DEFAULT", rt.make_int(wxGRID_DRAW_DEFAULT) },
//  From "grid.h": wxGridTableRequest
    { enif_make_atom(rt.env,"wxGridTableRequest"), "wxGRIDTABLE_NOTIFY_ROWS_INSERTED", rt.make_int(wxGRIDTABLE_NOTIFY_ROWS_INSERTED) },
    { enif_make_atom(rt.env,"wxGridTableRequest"), "wxGRIDTABLE_NOTIFY_ROWS_APPENDED", rt.make_int(wxGRIDTABLE_NOTIFY_ROWS_APPENDED) },
    { enif_make_atom(rt.env,"wxGridTableRequest"), "wxGRIDTABLE_NOTIFY_ROWS_DELETED", rt.make_int(wxGRIDTABLE_NOTIFY_ROWS_DELETED) },
    { enif_make_atom(rt.env,"wxGridTableRequest"), "wxGRIDTABLE_NOTIFY_COLS_INSERTED", rt.make_int(wxGRIDTABLE_NOTIFY_COLS_INSERTED) },
    { enif_make_atom(rt.env,"wxGridTableRequest"), "wxGRIDTABLE_NOTIFY_COLS_APPENDED", rt.make_int(wxGRIDTABLE_NOTIFY_COLS_APPENDED) },
    { enif_make_atom(rt.env,"wxGridTableRequest"), "wxGRIDTABLE_NOTIFY_COLS_DELETED", rt.make_int(wxGRIDTABLE_NOTIFY_COLS_DELETED) },
//  From "defs.h": wxHitTest
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_NOWHERE", rt.make_int(wxHT_NOWHERE) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_FIRST", rt.make_int(wxHT_SCROLLBAR_FIRST) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_ARROW_LINE_1", rt.make_int(wxHT_SCROLLBAR_ARROW_LINE_1) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_ARROW_LINE_2", rt.make_int(wxHT_SCROLLBAR_ARROW_LINE_2) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_ARROW_PAGE_1", rt.make_int(wxHT_SCROLLBAR_ARROW_PAGE_1) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_ARROW_PAGE_2", rt.make_int(wxHT_SCROLLBAR_ARROW_PAGE_2) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_THUMB", rt.make_int(wxHT_SCROLLBAR_THUMB) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_BAR_1", rt.make_int(wxHT_SCROLLBAR_BAR_1) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_BAR_2", rt.make_int(wxHT_SCROLLBAR_BAR_2) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_SCROLLBAR_LAST", rt.make_int(wxHT_SCROLLBAR_LAST) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_WINDOW_OUTSIDE", rt.make_int(wxHT_WINDOW_OUTSIDE) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_WINDOW_INSIDE", rt.make_int(wxHT_WINDOW_INSIDE) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_WINDOW_VERT_SCROLLBAR", rt.make_int(wxHT_WINDOW_VERT_SCROLLBAR) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_WINDOW_HORZ_SCROLLBAR", rt.make_int(wxHT_WINDOW_HORZ_SCROLLBAR) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_WINDOW_CORNER", rt.make_int(wxHT_WINDOW_CORNER) },
    { enif_make_atom(rt.env,"wxHitTest"), "wxHT_MAX", rt.make_int(wxHT_MAX) },
//  From "htmlwin.h": wxHtmlOpeningStatus
    { enif_make_atom(rt.env,"wxHtmlOpeningStatus"), "wxHTML_OPEN", rt.make_int(wxHTML_OPEN) },
    { enif_make_atom(rt.env,"wxHtmlOpeningStatus"), "wxHTML_BLOCK", rt.make_int(wxHTML_BLOCK) },
    { enif_make_atom(rt.env,"wxHtmlOpeningStatus"), "wxHTML_REDIRECT", rt.make_int(wxHTML_REDIRECT) },
//  From "event.h": wxIdleMode
    { enif_make_atom(rt.env,"wxIdleMode"), "wxIDLE_PROCESS_ALL", rt.make_int(wxIDLE_PROCESS_ALL) },
    { enif_make_atom(rt.env,"wxIdleMode"), "wxIDLE_PROCESS_SPECIFIED", rt.make_int(wxIDLE_PROCESS_SPECIFIED) },
//  From "imaglist.h": wxImageListFlags
    { enif_make_atom(rt.env,"wxImageListFlags"), "wxIMAGE_LIST_NORMAL", rt.make_int(wxIMAGE_LIST_NORMAL) },
    { enif_make_atom(rt.env,"wxImageListFlags"), "wxIMAGE_LIST_SMALL", rt.make_int(wxIMAGE_LIST_SMALL) },
    { enif_make_atom(rt.env,"wxImageListFlags"), "wxIMAGE_LIST_STATE", rt.make_int(wxIMAGE_LIST_STATE) },
//  From "image.h": wxImagePNGType
    { enif_make_atom(rt.env,"wxImagePNGType"), "wxPNG_TYPE_COLOUR", rt.make_int(wxPNG_TYPE_COLOUR) },
    { enif_make_atom(rt.env,"wxImagePNGType"), "wxPNG_TYPE_GREY", rt.make_int(wxPNG_TYPE_GREY) },
    { enif_make_atom(rt.env,"wxImagePNGType"), "wxPNG_TYPE_GREY_RED", rt.make_int(wxPNG_TYPE_GREY_RED) },
    { enif_make_atom(rt.env,"wxImagePNGType"), "wxPNG_TYPE_PALETTE", rt.make_int(wxPNG_TYPE_PALETTE) },
//  From "image.h": wxImageResizeQuality
    { enif_make_atom(rt.env,"wxImageResizeQuality"), "wxIMAGE_QUALITY_NEAREST", rt.make_int(wxIMAGE_QUALITY_NEAREST) },
    { enif_make_atom(rt.env,"wxImageResizeQuality"), "wxIMAGE_QUALITY_BILINEAR", rt.make_int(wxIMAGE_QUALITY_BILINEAR) },
    { enif_make_atom(rt.env,"wxImageResizeQuality"), "wxIMAGE_QUALITY_BICUBIC", rt.make_int(wxIMAGE_QUALITY_BICUBIC) },
    { enif_make_atom(rt.env,"wxImageResizeQuality"), "wxIMAGE_QUALITY_BOX_AVERAGE", rt.make_int(wxIMAGE_QUALITY_BOX_AVERAGE) },
    { enif_make_atom(rt.env,"wxImageResizeQuality"), "wxIMAGE_QUALITY_NORMAL", rt.make_int(wxIMAGE_QUALITY_NORMAL) },
    { enif_make_atom(rt.env,"wxImageResizeQuality"), "wxIMAGE_QUALITY_HIGH", rt.make_int(wxIMAGE_QUALITY_HIGH) },
//  From "image.h": wxImageResolution
    { enif_make_atom(rt.env,"wxImageResolution"), "wxIMAGE_RESOLUTION_NONE", rt.make_int(wxIMAGE_RESOLUTION_NONE) },
    { enif_make_atom(rt.env,"wxImageResolution"), "wxIMAGE_RESOLUTION_INCHES", rt.make_int(wxIMAGE_RESOLUTION_INCHES) },
    { enif_make_atom(rt.env,"wxImageResolution"), "wxIMAGE_RESOLUTION_CM", rt.make_int(wxIMAGE_RESOLUTION_CM) },
//  From "graphics.h": wxInterpolationQuality
    { enif_make_atom(rt.env,"wxInterpolationQuality"), "wxINTERPOLATION_DEFAULT", rt.make_int(wxINTERPOLATION_DEFAULT) },
    { enif_make_atom(rt.env,"wxInterpolationQuality"), "wxINTERPOLATION_NONE", rt.make_int(wxINTERPOLATION_NONE) },
    { enif_make_atom(rt.env,"wxInterpolationQuality"), "wxINTERPOLATION_FAST", rt.make_int(wxINTERPOLATION_FAST) },
    { enif_make_atom(rt.env,"wxInterpolationQuality"), "wxINTERPOLATION_GOOD", rt.make_int(wxINTERPOLATION_GOOD) },
    { enif_make_atom(rt.env,"wxInterpolationQuality"), "wxINTERPOLATION_BEST", rt.make_int(wxINTERPOLATION_BEST) },
//  From "defs.h": wxItemKind
    { enif_make_atom(rt.env,"wxItemKind"), "wxITEM_SEPARATOR", rt.make_int(wxITEM_SEPARATOR) },
    { enif_make_atom(rt.env,"wxItemKind"), "wxITEM_NORMAL", rt.make_int(wxITEM_NORMAL) },
    { enif_make_atom(rt.env,"wxItemKind"), "wxITEM_CHECK", rt.make_int(wxITEM_CHECK) },
    { enif_make_atom(rt.env,"wxItemKind"), "wxITEM_RADIO", rt.make_int(wxITEM_RADIO) },
    { enif_make_atom(rt.env,"wxItemKind"), "wxITEM_DROPDOWN", rt.make_int(wxITEM_DROPDOWN) },
    { enif_make_atom(rt.env,"wxItemKind"), "wxITEM_MAX", rt.make_int(wxITEM_MAX) },
//  From "event.h": wxKeyCategoryFlags
    { enif_make_atom(rt.env,"wxKeyCategoryFlags"), "wxk_CATEGORY_ARROW", rt.make_int(WXK_CATEGORY_ARROW) },
    { enif_make_atom(rt.env,"wxKeyCategoryFlags"), "wxk_CATEGORY_PAGING", rt.make_int(WXK_CATEGORY_PAGING) },
    { enif_make_atom(rt.env,"wxKeyCategoryFlags"), "wxk_CATEGORY_JUMP", rt.make_int(WXK_CATEGORY_JUMP) },
    { enif_make_atom(rt.env,"wxKeyCategoryFlags"), "wxk_CATEGORY_TAB", rt.make_int(WXK_CATEGORY_TAB) },
    { enif_make_atom(rt.env,"wxKeyCategoryFlags"), "wxk_CATEGORY_CUT", rt.make_int(WXK_CATEGORY_CUT) },
    { enif_make_atom(rt.env,"wxKeyCategoryFlags"), "wxk_CATEGORY_NAVIGATION", rt.make_int(WXK_CATEGORY_NAVIGATION) },
//  From "defs.h": wxKeyCode
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NONE", rt.make_int(WXK_NONE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_A", rt.make_int(WXK_CONTROL_A) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_B", rt.make_int(WXK_CONTROL_B) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_C", rt.make_int(WXK_CONTROL_C) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_D", rt.make_int(WXK_CONTROL_D) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_E", rt.make_int(WXK_CONTROL_E) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_F", rt.make_int(WXK_CONTROL_F) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_G", rt.make_int(WXK_CONTROL_G) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_H", rt.make_int(WXK_CONTROL_H) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_I", rt.make_int(WXK_CONTROL_I) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_J", rt.make_int(WXK_CONTROL_J) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_K", rt.make_int(WXK_CONTROL_K) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_L", rt.make_int(WXK_CONTROL_L) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_M", rt.make_int(WXK_CONTROL_M) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_N", rt.make_int(WXK_CONTROL_N) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_O", rt.make_int(WXK_CONTROL_O) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_P", rt.make_int(WXK_CONTROL_P) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_Q", rt.make_int(WXK_CONTROL_Q) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_R", rt.make_int(WXK_CONTROL_R) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_S", rt.make_int(WXK_CONTROL_S) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_T", rt.make_int(WXK_CONTROL_T) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_U", rt.make_int(WXK_CONTROL_U) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_V", rt.make_int(WXK_CONTROL_V) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_W", rt.make_int(WXK_CONTROL_W) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_X", rt.make_int(WXK_CONTROL_X) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_Y", rt.make_int(WXK_CONTROL_Y) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL_Z", rt.make_int(WXK_CONTROL_Z) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BACK", rt.make_int(WXK_BACK) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_TAB", rt.make_int(WXK_TAB) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_RETURN", rt.make_int(WXK_RETURN) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_ESCAPE", rt.make_int(WXK_ESCAPE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPACE", rt.make_int(WXK_SPACE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_DELETE", rt.make_int(WXK_DELETE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_START", rt.make_int(WXK_START) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LBUTTON", rt.make_int(WXK_LBUTTON) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_RBUTTON", rt.make_int(WXK_RBUTTON) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CANCEL", rt.make_int(WXK_CANCEL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MBUTTON", rt.make_int(WXK_MBUTTON) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CLEAR", rt.make_int(WXK_CLEAR) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SHIFT", rt.make_int(WXK_SHIFT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_ALT", rt.make_int(WXK_ALT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CONTROL", rt.make_int(WXK_CONTROL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MENU", rt.make_int(WXK_MENU) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_PAUSE", rt.make_int(WXK_PAUSE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_CAPITAL", rt.make_int(WXK_CAPITAL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_END", rt.make_int(WXK_END) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_HOME", rt.make_int(WXK_HOME) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LEFT", rt.make_int(WXK_LEFT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_UP", rt.make_int(WXK_UP) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_RIGHT", rt.make_int(WXK_RIGHT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_DOWN", rt.make_int(WXK_DOWN) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SELECT", rt.make_int(WXK_SELECT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_PRINT", rt.make_int(WXK_PRINT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_EXECUTE", rt.make_int(WXK_EXECUTE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SNAPSHOT", rt.make_int(WXK_SNAPSHOT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_INSERT", rt.make_int(WXK_INSERT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_HELP", rt.make_int(WXK_HELP) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD0", rt.make_int(WXK_NUMPAD0) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD1", rt.make_int(WXK_NUMPAD1) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD2", rt.make_int(WXK_NUMPAD2) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD3", rt.make_int(WXK_NUMPAD3) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD4", rt.make_int(WXK_NUMPAD4) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD5", rt.make_int(WXK_NUMPAD5) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD6", rt.make_int(WXK_NUMPAD6) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD7", rt.make_int(WXK_NUMPAD7) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD8", rt.make_int(WXK_NUMPAD8) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD9", rt.make_int(WXK_NUMPAD9) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MULTIPLY", rt.make_int(WXK_MULTIPLY) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_ADD", rt.make_int(WXK_ADD) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SEPARATOR", rt.make_int(WXK_SEPARATOR) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SUBTRACT", rt.make_int(WXK_SUBTRACT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_DECIMAL", rt.make_int(WXK_DECIMAL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_DIVIDE", rt.make_int(WXK_DIVIDE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F1", rt.make_int(WXK_F1) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F2", rt.make_int(WXK_F2) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F3", rt.make_int(WXK_F3) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F4", rt.make_int(WXK_F4) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F5", rt.make_int(WXK_F5) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F6", rt.make_int(WXK_F6) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F7", rt.make_int(WXK_F7) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F8", rt.make_int(WXK_F8) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F9", rt.make_int(WXK_F9) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F10", rt.make_int(WXK_F10) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F11", rt.make_int(WXK_F11) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F12", rt.make_int(WXK_F12) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F13", rt.make_int(WXK_F13) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F14", rt.make_int(WXK_F14) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F15", rt.make_int(WXK_F15) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F16", rt.make_int(WXK_F16) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F17", rt.make_int(WXK_F17) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F18", rt.make_int(WXK_F18) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F19", rt.make_int(WXK_F19) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F20", rt.make_int(WXK_F20) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F21", rt.make_int(WXK_F21) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F22", rt.make_int(WXK_F22) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F23", rt.make_int(WXK_F23) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_F24", rt.make_int(WXK_F24) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMLOCK", rt.make_int(WXK_NUMLOCK) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SCROLL", rt.make_int(WXK_SCROLL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_PAGEUP", rt.make_int(WXK_PAGEUP) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_PAGEDOWN", rt.make_int(WXK_PAGEDOWN) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_SPACE", rt.make_int(WXK_NUMPAD_SPACE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_TAB", rt.make_int(WXK_NUMPAD_TAB) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_ENTER", rt.make_int(WXK_NUMPAD_ENTER) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_F1", rt.make_int(WXK_NUMPAD_F1) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_F2", rt.make_int(WXK_NUMPAD_F2) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_F3", rt.make_int(WXK_NUMPAD_F3) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_F4", rt.make_int(WXK_NUMPAD_F4) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_HOME", rt.make_int(WXK_NUMPAD_HOME) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_LEFT", rt.make_int(WXK_NUMPAD_LEFT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_UP", rt.make_int(WXK_NUMPAD_UP) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_RIGHT", rt.make_int(WXK_NUMPAD_RIGHT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_DOWN", rt.make_int(WXK_NUMPAD_DOWN) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_PAGEUP", rt.make_int(WXK_NUMPAD_PAGEUP) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_PAGEDOWN", rt.make_int(WXK_NUMPAD_PAGEDOWN) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_END", rt.make_int(WXK_NUMPAD_END) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_BEGIN", rt.make_int(WXK_NUMPAD_BEGIN) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_INSERT", rt.make_int(WXK_NUMPAD_INSERT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_DELETE", rt.make_int(WXK_NUMPAD_DELETE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_EQUAL", rt.make_int(WXK_NUMPAD_EQUAL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_MULTIPLY", rt.make_int(WXK_NUMPAD_MULTIPLY) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_ADD", rt.make_int(WXK_NUMPAD_ADD) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_SEPARATOR", rt.make_int(WXK_NUMPAD_SEPARATOR) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_SUBTRACT", rt.make_int(WXK_NUMPAD_SUBTRACT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_DECIMAL", rt.make_int(WXK_NUMPAD_DECIMAL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_NUMPAD_DIVIDE", rt.make_int(WXK_NUMPAD_DIVIDE) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_WINDOWS_LEFT", rt.make_int(WXK_WINDOWS_LEFT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_WINDOWS_RIGHT", rt.make_int(WXK_WINDOWS_RIGHT) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_WINDOWS_MENU", rt.make_int(WXK_WINDOWS_MENU) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_RAW_CONTROL", rt.make_int(WXK_RAW_CONTROL) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_COMMAND", rt.make_int(WXK_COMMAND) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL1", rt.make_int(WXK_SPECIAL1) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL2", rt.make_int(WXK_SPECIAL2) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL3", rt.make_int(WXK_SPECIAL3) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL4", rt.make_int(WXK_SPECIAL4) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL5", rt.make_int(WXK_SPECIAL5) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL6", rt.make_int(WXK_SPECIAL6) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL7", rt.make_int(WXK_SPECIAL7) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL8", rt.make_int(WXK_SPECIAL8) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL9", rt.make_int(WXK_SPECIAL9) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL10", rt.make_int(WXK_SPECIAL10) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL11", rt.make_int(WXK_SPECIAL11) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL12", rt.make_int(WXK_SPECIAL12) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL13", rt.make_int(WXK_SPECIAL13) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL14", rt.make_int(WXK_SPECIAL14) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL15", rt.make_int(WXK_SPECIAL15) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL16", rt.make_int(WXK_SPECIAL16) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL17", rt.make_int(WXK_SPECIAL17) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL18", rt.make_int(WXK_SPECIAL18) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL19", rt.make_int(WXK_SPECIAL19) },
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_SPECIAL20", rt.make_int(WXK_SPECIAL20) },
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_BACK", rt.make_int(WXK_BROWSER_BACK) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_BACK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_FORWARD", rt.make_int(WXK_BROWSER_FORWARD) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_FORWARD", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_REFRESH", rt.make_int(WXK_BROWSER_REFRESH) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_REFRESH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_STOP", rt.make_int(WXK_BROWSER_STOP) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_STOP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_SEARCH", rt.make_int(WXK_BROWSER_SEARCH) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_SEARCH", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_FAVORITES", rt.make_int(WXK_BROWSER_FAVORITES) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_FAVORITES", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_HOME", rt.make_int(WXK_BROWSER_HOME) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_BROWSER_HOME", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_VOLUME_MUTE", rt.make_int(WXK_VOLUME_MUTE) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_VOLUME_MUTE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_VOLUME_DOWN", rt.make_int(WXK_VOLUME_DOWN) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_VOLUME_DOWN", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_VOLUME_UP", rt.make_int(WXK_VOLUME_UP) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_VOLUME_UP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_NEXT_TRACK", rt.make_int(WXK_MEDIA_NEXT_TRACK) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_NEXT_TRACK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_PREV_TRACK", rt.make_int(WXK_MEDIA_PREV_TRACK) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_PREV_TRACK", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_STOP", rt.make_int(WXK_MEDIA_STOP) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_STOP", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_PLAY_PAUSE", rt.make_int(WXK_MEDIA_PLAY_PAUSE) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_MEDIA_PLAY_PAUSE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,2,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LAUNCH_MAIL", rt.make_int(WXK_LAUNCH_MAIL) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LAUNCH_MAIL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,2,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LAUNCH_APP1", rt.make_int(WXK_LAUNCH_APP1) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LAUNCH_APP1", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,2,0)
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LAUNCH_APP2", rt.make_int(WXK_LAUNCH_APP2) },
#else
    { enif_make_atom(rt.env,"wxKeyCode"), "wxk_LAUNCH_APP2", WXE_ATOM_undefined },
#endif
//  From "defs.h": wxKeyModifier
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_NONE", rt.make_int(wxMOD_NONE) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_ALT", rt.make_int(wxMOD_ALT) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_CONTROL", rt.make_int(wxMOD_CONTROL) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_ALTGR", rt.make_int(wxMOD_ALTGR) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_SHIFT", rt.make_int(wxMOD_SHIFT) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_META", rt.make_int(wxMOD_META) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_WIN", rt.make_int(wxMOD_WIN) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_RAW_CONTROL", rt.make_int(wxMOD_RAW_CONTROL) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_CMD", rt.make_int(wxMOD_CMD) },
    { enif_make_atom(rt.env,"wxKeyModifier"), "wxMOD_ALL", rt.make_int(wxMOD_ALL) },
//  From "utils.h": wxKillError
    { enif_make_atom(rt.env,"wxKillError"), "wxKILL_OK", rt.make_int(wxKILL_OK) },
    { enif_make_atom(rt.env,"wxKillError"), "wxKILL_BAD_SIGNAL", rt.make_int(wxKILL_BAD_SIGNAL) },
    { enif_make_atom(rt.env,"wxKillError"), "wxKILL_ACCESS_DENIED", rt.make_int(wxKILL_ACCESS_DENIED) },
    { enif_make_atom(rt.env,"wxKillError"), "wxKILL_NO_PROCESS", rt.make_int(wxKILL_NO_PROCESS) },
    { enif_make_atom(rt.env,"wxKillError"), "wxKILL_ERROR", rt.make_int(wxKILL_ERROR) },
//  From "utils.h": wxKillFlags
    { enif_make_atom(rt.env,"wxKillFlags"), "wxKILL_NOCHILDREN", rt.make_int(wxKILL_NOCHILDREN) },
    { enif_make_atom(rt.env,"wxKillFlags"), "wxKILL_CHILDREN", rt.make_int(wxKILL_CHILDREN) },
//  From "language.h": wxLanguage
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_DEFAULT", rt.make_int(wxLANGUAGE_DEFAULT) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_UNKNOWN", rt.make_int(wxLANGUAGE_UNKNOWN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ABKHAZIAN", rt.make_int(wxLANGUAGE_ABKHAZIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AFAR", rt.make_int(wxLANGUAGE_AFAR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AFRIKAANS", rt.make_int(wxLANGUAGE_AFRIKAANS) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ALBANIAN", rt.make_int(wxLANGUAGE_ALBANIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AMHARIC", rt.make_int(wxLANGUAGE_AMHARIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC", rt.make_int(wxLANGUAGE_ARABIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_ALGERIA", rt.make_int(wxLANGUAGE_ARABIC_ALGERIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_BAHRAIN", rt.make_int(wxLANGUAGE_ARABIC_BAHRAIN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_EGYPT", rt.make_int(wxLANGUAGE_ARABIC_EGYPT) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_IRAQ", rt.make_int(wxLANGUAGE_ARABIC_IRAQ) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_JORDAN", rt.make_int(wxLANGUAGE_ARABIC_JORDAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_KUWAIT", rt.make_int(wxLANGUAGE_ARABIC_KUWAIT) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_LEBANON", rt.make_int(wxLANGUAGE_ARABIC_LEBANON) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_LIBYA", rt.make_int(wxLANGUAGE_ARABIC_LIBYA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_MOROCCO", rt.make_int(wxLANGUAGE_ARABIC_MOROCCO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_OMAN", rt.make_int(wxLANGUAGE_ARABIC_OMAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_QATAR", rt.make_int(wxLANGUAGE_ARABIC_QATAR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_SAUDI_ARABIA", rt.make_int(wxLANGUAGE_ARABIC_SAUDI_ARABIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_SUDAN", rt.make_int(wxLANGUAGE_ARABIC_SUDAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_SYRIA", rt.make_int(wxLANGUAGE_ARABIC_SYRIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_TUNISIA", rt.make_int(wxLANGUAGE_ARABIC_TUNISIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_UAE", rt.make_int(wxLANGUAGE_ARABIC_UAE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARABIC_YEMEN", rt.make_int(wxLANGUAGE_ARABIC_YEMEN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ARMENIAN", rt.make_int(wxLANGUAGE_ARMENIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ASSAMESE", rt.make_int(wxLANGUAGE_ASSAMESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ASTURIAN", rt.make_int(wxLANGUAGE_ASTURIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AYMARA", rt.make_int(wxLANGUAGE_AYMARA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AZERI", rt.make_int(wxLANGUAGE_AZERI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AZERI_CYRILLIC", rt.make_int(wxLANGUAGE_AZERI_CYRILLIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_AZERI_LATIN", rt.make_int(wxLANGUAGE_AZERI_LATIN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BASHKIR", rt.make_int(wxLANGUAGE_BASHKIR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BASQUE", rt.make_int(wxLANGUAGE_BASQUE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BELARUSIAN", rt.make_int(wxLANGUAGE_BELARUSIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BENGALI", rt.make_int(wxLANGUAGE_BENGALI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BHUTANI", rt.make_int(wxLANGUAGE_BHUTANI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BIHARI", rt.make_int(wxLANGUAGE_BIHARI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BISLAMA", rt.make_int(wxLANGUAGE_BISLAMA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BOSNIAN", rt.make_int(wxLANGUAGE_BOSNIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BRETON", rt.make_int(wxLANGUAGE_BRETON) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BULGARIAN", rt.make_int(wxLANGUAGE_BULGARIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_BURMESE", rt.make_int(wxLANGUAGE_BURMESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CATALAN", rt.make_int(wxLANGUAGE_CATALAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE", rt.make_int(wxLANGUAGE_CHINESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE_SIMPLIFIED", rt.make_int(wxLANGUAGE_CHINESE_SIMPLIFIED) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE_TRADITIONAL", rt.make_int(wxLANGUAGE_CHINESE_TRADITIONAL) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE_HONGKONG", rt.make_int(wxLANGUAGE_CHINESE_HONGKONG) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE_MACAU", rt.make_int(wxLANGUAGE_CHINESE_MACAU) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE_SINGAPORE", rt.make_int(wxLANGUAGE_CHINESE_SINGAPORE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CHINESE_TAIWAN", rt.make_int(wxLANGUAGE_CHINESE_TAIWAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CORSICAN", rt.make_int(wxLANGUAGE_CORSICAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CROATIAN", rt.make_int(wxLANGUAGE_CROATIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CZECH", rt.make_int(wxLANGUAGE_CZECH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_DANISH", rt.make_int(wxLANGUAGE_DANISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_DUTCH", rt.make_int(wxLANGUAGE_DUTCH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_DUTCH_BELGIAN", rt.make_int(wxLANGUAGE_DUTCH_BELGIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH", rt.make_int(wxLANGUAGE_ENGLISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_UK", rt.make_int(wxLANGUAGE_ENGLISH_UK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_US", rt.make_int(wxLANGUAGE_ENGLISH_US) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_AUSTRALIA", rt.make_int(wxLANGUAGE_ENGLISH_AUSTRALIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_BELIZE", rt.make_int(wxLANGUAGE_ENGLISH_BELIZE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_BOTSWANA", rt.make_int(wxLANGUAGE_ENGLISH_BOTSWANA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_CANADA", rt.make_int(wxLANGUAGE_ENGLISH_CANADA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_CARIBBEAN", rt.make_int(wxLANGUAGE_ENGLISH_CARIBBEAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_DENMARK", rt.make_int(wxLANGUAGE_ENGLISH_DENMARK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_EIRE", rt.make_int(wxLANGUAGE_ENGLISH_EIRE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_JAMAICA", rt.make_int(wxLANGUAGE_ENGLISH_JAMAICA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_NEW_ZEALAND", rt.make_int(wxLANGUAGE_ENGLISH_NEW_ZEALAND) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_PHILIPPINES", rt.make_int(wxLANGUAGE_ENGLISH_PHILIPPINES) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_SOUTH_AFRICA", rt.make_int(wxLANGUAGE_ENGLISH_SOUTH_AFRICA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_TRINIDAD", rt.make_int(wxLANGUAGE_ENGLISH_TRINIDAD) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ENGLISH_ZIMBABWE", rt.make_int(wxLANGUAGE_ENGLISH_ZIMBABWE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ESPERANTO", rt.make_int(wxLANGUAGE_ESPERANTO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ESTONIAN", rt.make_int(wxLANGUAGE_ESTONIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FAEROESE", rt.make_int(wxLANGUAGE_FAEROESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FARSI", rt.make_int(wxLANGUAGE_FARSI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FIJI", rt.make_int(wxLANGUAGE_FIJI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FINNISH", rt.make_int(wxLANGUAGE_FINNISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRENCH", rt.make_int(wxLANGUAGE_FRENCH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRENCH_BELGIAN", rt.make_int(wxLANGUAGE_FRENCH_BELGIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRENCH_CANADIAN", rt.make_int(wxLANGUAGE_FRENCH_CANADIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRENCH_LUXEMBOURG", rt.make_int(wxLANGUAGE_FRENCH_LUXEMBOURG) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRENCH_MONACO", rt.make_int(wxLANGUAGE_FRENCH_MONACO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRENCH_SWISS", rt.make_int(wxLANGUAGE_FRENCH_SWISS) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_FRISIAN", rt.make_int(wxLANGUAGE_FRISIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GALICIAN", rt.make_int(wxLANGUAGE_GALICIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GEORGIAN", rt.make_int(wxLANGUAGE_GEORGIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GERMAN", rt.make_int(wxLANGUAGE_GERMAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GERMAN_AUSTRIAN", rt.make_int(wxLANGUAGE_GERMAN_AUSTRIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GERMAN_BELGIUM", rt.make_int(wxLANGUAGE_GERMAN_BELGIUM) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GERMAN_LIECHTENSTEIN", rt.make_int(wxLANGUAGE_GERMAN_LIECHTENSTEIN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GERMAN_LUXEMBOURG", rt.make_int(wxLANGUAGE_GERMAN_LUXEMBOURG) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GERMAN_SWISS", rt.make_int(wxLANGUAGE_GERMAN_SWISS) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GREEK", rt.make_int(wxLANGUAGE_GREEK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GREENLANDIC", rt.make_int(wxLANGUAGE_GREENLANDIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GUARANI", rt.make_int(wxLANGUAGE_GUARANI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_GUJARATI", rt.make_int(wxLANGUAGE_GUJARATI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_HAUSA", rt.make_int(wxLANGUAGE_HAUSA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_HEBREW", rt.make_int(wxLANGUAGE_HEBREW) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_HINDI", rt.make_int(wxLANGUAGE_HINDI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_HUNGARIAN", rt.make_int(wxLANGUAGE_HUNGARIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ICELANDIC", rt.make_int(wxLANGUAGE_ICELANDIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_INDONESIAN", rt.make_int(wxLANGUAGE_INDONESIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_INTERLINGUA", rt.make_int(wxLANGUAGE_INTERLINGUA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_INTERLINGUE", rt.make_int(wxLANGUAGE_INTERLINGUE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_INUKTITUT", rt.make_int(wxLANGUAGE_INUKTITUT) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_INUPIAK", rt.make_int(wxLANGUAGE_INUPIAK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_IRISH", rt.make_int(wxLANGUAGE_IRISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ITALIAN", rt.make_int(wxLANGUAGE_ITALIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ITALIAN_SWISS", rt.make_int(wxLANGUAGE_ITALIAN_SWISS) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_JAPANESE", rt.make_int(wxLANGUAGE_JAPANESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_JAVANESE", rt.make_int(wxLANGUAGE_JAVANESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KABYLE", rt.make_int(wxLANGUAGE_KABYLE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KANNADA", rt.make_int(wxLANGUAGE_KANNADA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KASHMIRI", rt.make_int(wxLANGUAGE_KASHMIRI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KASHMIRI_INDIA", rt.make_int(wxLANGUAGE_KASHMIRI_INDIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KAZAKH", rt.make_int(wxLANGUAGE_KAZAKH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KERNEWEK", rt.make_int(wxLANGUAGE_KERNEWEK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KINYARWANDA", rt.make_int(wxLANGUAGE_KINYARWANDA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KIRGHIZ", rt.make_int(wxLANGUAGE_KIRGHIZ) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KIRUNDI", rt.make_int(wxLANGUAGE_KIRUNDI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KONKANI", rt.make_int(wxLANGUAGE_KONKANI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KOREAN", rt.make_int(wxLANGUAGE_KOREAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_KURDISH", rt.make_int(wxLANGUAGE_KURDISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_LAOTHIAN", rt.make_int(wxLANGUAGE_LAOTHIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_LATIN", rt.make_int(wxLANGUAGE_LATIN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_LATVIAN", rt.make_int(wxLANGUAGE_LATVIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_LINGALA", rt.make_int(wxLANGUAGE_LINGALA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_LITHUANIAN", rt.make_int(wxLANGUAGE_LITHUANIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MACEDONIAN", rt.make_int(wxLANGUAGE_MACEDONIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MALAGASY", rt.make_int(wxLANGUAGE_MALAGASY) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MALAY", rt.make_int(wxLANGUAGE_MALAY) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MALAYALAM", rt.make_int(wxLANGUAGE_MALAYALAM) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM", rt.make_int(wxLANGUAGE_MALAY_BRUNEI_DARUSSALAM) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MALAY_MALAYSIA", rt.make_int(wxLANGUAGE_MALAY_MALAYSIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MALTESE", rt.make_int(wxLANGUAGE_MALTESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MANIPURI", rt.make_int(wxLANGUAGE_MANIPURI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MAORI", rt.make_int(wxLANGUAGE_MAORI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MARATHI", rt.make_int(wxLANGUAGE_MARATHI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MOLDAVIAN", rt.make_int(wxLANGUAGE_MOLDAVIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_MONGOLIAN", rt.make_int(wxLANGUAGE_MONGOLIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_NAURU", rt.make_int(wxLANGUAGE_NAURU) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_NEPALI", rt.make_int(wxLANGUAGE_NEPALI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_NEPALI_INDIA", rt.make_int(wxLANGUAGE_NEPALI_INDIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_NORWEGIAN_BOKMAL", rt.make_int(wxLANGUAGE_NORWEGIAN_BOKMAL) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_NORWEGIAN_NYNORSK", rt.make_int(wxLANGUAGE_NORWEGIAN_NYNORSK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_OCCITAN", rt.make_int(wxLANGUAGE_OCCITAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ORIYA", rt.make_int(wxLANGUAGE_ORIYA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_OROMO", rt.make_int(wxLANGUAGE_OROMO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_PASHTO", rt.make_int(wxLANGUAGE_PASHTO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_POLISH", rt.make_int(wxLANGUAGE_POLISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_PORTUGUESE", rt.make_int(wxLANGUAGE_PORTUGUESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_PORTUGUESE_BRAZILIAN", rt.make_int(wxLANGUAGE_PORTUGUESE_BRAZILIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_PUNJABI", rt.make_int(wxLANGUAGE_PUNJABI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_QUECHUA", rt.make_int(wxLANGUAGE_QUECHUA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_RHAETO_ROMANCE", rt.make_int(wxLANGUAGE_RHAETO_ROMANCE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ROMANIAN", rt.make_int(wxLANGUAGE_ROMANIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_RUSSIAN", rt.make_int(wxLANGUAGE_RUSSIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_RUSSIAN_UKRAINE", rt.make_int(wxLANGUAGE_RUSSIAN_UKRAINE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SAMI", rt.make_int(wxLANGUAGE_SAMI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SAMOAN", rt.make_int(wxLANGUAGE_SAMOAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SANGHO", rt.make_int(wxLANGUAGE_SANGHO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SANSKRIT", rt.make_int(wxLANGUAGE_SANSKRIT) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SCOTS_GAELIC", rt.make_int(wxLANGUAGE_SCOTS_GAELIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SERBIAN", rt.make_int(wxLANGUAGE_SERBIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SERBIAN_CYRILLIC", rt.make_int(wxLANGUAGE_SERBIAN_CYRILLIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SERBIAN_LATIN", rt.make_int(wxLANGUAGE_SERBIAN_LATIN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SERBO_CROATIAN", rt.make_int(wxLANGUAGE_SERBO_CROATIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SESOTHO", rt.make_int(wxLANGUAGE_SESOTHO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SETSWANA", rt.make_int(wxLANGUAGE_SETSWANA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SHONA", rt.make_int(wxLANGUAGE_SHONA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SINDHI", rt.make_int(wxLANGUAGE_SINDHI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SINHALESE", rt.make_int(wxLANGUAGE_SINHALESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SISWATI", rt.make_int(wxLANGUAGE_SISWATI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SLOVAK", rt.make_int(wxLANGUAGE_SLOVAK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SLOVENIAN", rt.make_int(wxLANGUAGE_SLOVENIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SOMALI", rt.make_int(wxLANGUAGE_SOMALI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH", rt.make_int(wxLANGUAGE_SPANISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_ARGENTINA", rt.make_int(wxLANGUAGE_SPANISH_ARGENTINA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_BOLIVIA", rt.make_int(wxLANGUAGE_SPANISH_BOLIVIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_CHILE", rt.make_int(wxLANGUAGE_SPANISH_CHILE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_COLOMBIA", rt.make_int(wxLANGUAGE_SPANISH_COLOMBIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_COSTA_RICA", rt.make_int(wxLANGUAGE_SPANISH_COSTA_RICA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC", rt.make_int(wxLANGUAGE_SPANISH_DOMINICAN_REPUBLIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_ECUADOR", rt.make_int(wxLANGUAGE_SPANISH_ECUADOR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_EL_SALVADOR", rt.make_int(wxLANGUAGE_SPANISH_EL_SALVADOR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_GUATEMALA", rt.make_int(wxLANGUAGE_SPANISH_GUATEMALA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_HONDURAS", rt.make_int(wxLANGUAGE_SPANISH_HONDURAS) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_MEXICAN", rt.make_int(wxLANGUAGE_SPANISH_MEXICAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_MODERN", rt.make_int(wxLANGUAGE_SPANISH_MODERN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_NICARAGUA", rt.make_int(wxLANGUAGE_SPANISH_NICARAGUA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_PANAMA", rt.make_int(wxLANGUAGE_SPANISH_PANAMA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_PARAGUAY", rt.make_int(wxLANGUAGE_SPANISH_PARAGUAY) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_PERU", rt.make_int(wxLANGUAGE_SPANISH_PERU) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_PUERTO_RICO", rt.make_int(wxLANGUAGE_SPANISH_PUERTO_RICO) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_URUGUAY", rt.make_int(wxLANGUAGE_SPANISH_URUGUAY) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_US", rt.make_int(wxLANGUAGE_SPANISH_US) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SPANISH_VENEZUELA", rt.make_int(wxLANGUAGE_SPANISH_VENEZUELA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SUNDANESE", rt.make_int(wxLANGUAGE_SUNDANESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SWAHILI", rt.make_int(wxLANGUAGE_SWAHILI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SWEDISH", rt.make_int(wxLANGUAGE_SWEDISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_SWEDISH_FINLAND", rt.make_int(wxLANGUAGE_SWEDISH_FINLAND) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TAGALOG", rt.make_int(wxLANGUAGE_TAGALOG) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TAJIK", rt.make_int(wxLANGUAGE_TAJIK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TAMIL", rt.make_int(wxLANGUAGE_TAMIL) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TATAR", rt.make_int(wxLANGUAGE_TATAR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TELUGU", rt.make_int(wxLANGUAGE_TELUGU) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_THAI", rt.make_int(wxLANGUAGE_THAI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TIBETAN", rt.make_int(wxLANGUAGE_TIBETAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TIGRINYA", rt.make_int(wxLANGUAGE_TIGRINYA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TONGA", rt.make_int(wxLANGUAGE_TONGA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TSONGA", rt.make_int(wxLANGUAGE_TSONGA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TURKISH", rt.make_int(wxLANGUAGE_TURKISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TURKMEN", rt.make_int(wxLANGUAGE_TURKMEN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_TWI", rt.make_int(wxLANGUAGE_TWI) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_UIGHUR", rt.make_int(wxLANGUAGE_UIGHUR) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_UKRAINIAN", rt.make_int(wxLANGUAGE_UKRAINIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_URDU", rt.make_int(wxLANGUAGE_URDU) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_URDU_INDIA", rt.make_int(wxLANGUAGE_URDU_INDIA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_URDU_PAKISTAN", rt.make_int(wxLANGUAGE_URDU_PAKISTAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_UZBEK", rt.make_int(wxLANGUAGE_UZBEK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_UZBEK_CYRILLIC", rt.make_int(wxLANGUAGE_UZBEK_CYRILLIC) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_UZBEK_LATIN", rt.make_int(wxLANGUAGE_UZBEK_LATIN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_VALENCIAN", rt.make_int(wxLANGUAGE_VALENCIAN) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_VIETNAMESE", rt.make_int(wxLANGUAGE_VIETNAMESE) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_VOLAPUK", rt.make_int(wxLANGUAGE_VOLAPUK) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_WELSH", rt.make_int(wxLANGUAGE_WELSH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_WOLOF", rt.make_int(wxLANGUAGE_WOLOF) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_XHOSA", rt.make_int(wxLANGUAGE_XHOSA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_YIDDISH", rt.make_int(wxLANGUAGE_YIDDISH) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_YORUBA", rt.make_int(wxLANGUAGE_YORUBA) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ZHUANG", rt.make_int(wxLANGUAGE_ZHUANG) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_ZULU", rt.make_int(wxLANGUAGE_ZULU) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_USER_DEFINED", rt.make_int(wxLANGUAGE_USER_DEFINED) },
    { enif_make_atom(rt.env,"wxLanguage"), "wxLANGUAGE_CAMBODIAN", rt.make_int(wxLANGUAGE_CAMBODIAN) },
//  From "laywin.h": wxLayoutAlignment
    { enif_make_atom(rt.env,"wxLayoutAlignment"), "wxLAYOUT_NONE", rt.make_int(wxLAYOUT_NONE) },
    { enif_make_atom(rt.env,"wxLayoutAlignment"), "wxLAYOUT_TOP", rt.make_int(wxLAYOUT_TOP) },
    { enif_make_atom(rt.env,"wxLayoutAlignment"), "wxLAYOUT_LEFT", rt.make_int(wxLAYOUT_LEFT) },
    { enif_make_atom(rt.env,"wxLayoutAlignment"), "wxLAYOUT_RIGHT", rt.make_int(wxLAYOUT_RIGHT) },
    { enif_make_atom(rt.env,"wxLayoutAlignment"), "wxLAYOUT_BOTTOM", rt.make_int(wxLAYOUT_BOTTOM) },
//  From "intl.h": wxLayoutDirection
    { enif_make_atom(rt.env,"wxLayoutDirection"), "wxLayout_Default", rt.make_int(wxLayout_Default) },
    { enif_make_atom(rt.env,"wxLayoutDirection"), "wxLayout_LeftToRight", rt.make_int(wxLayout_LeftToRight) },
    { enif_make_atom(rt.env,"wxLayoutDirection"), "wxLayout_RightToLeft", rt.make_int(wxLayout_RightToLeft) },
//  From "laywin.h": wxLayoutOrientation
    { enif_make_atom(rt.env,"wxLayoutOrientation"), "wxLAYOUT_HORIZONTAL", rt.make_int(wxLAYOUT_HORIZONTAL) },
    { enif_make_atom(rt.env,"wxLayoutOrientation"), "wxLAYOUT_VERTICAL", rt.make_int(wxLAYOUT_VERTICAL) },
//  From "listctrl.h": wxListAlignFlags
    { enif_make_atom(rt.env,"wxListAlignFlags"), "wxLIST_ALIGN_DEFAULT", rt.make_int(wxLIST_ALIGN_DEFAULT) },
    { enif_make_atom(rt.env,"wxListAlignFlags"), "wxLIST_ALIGN_LEFT", rt.make_int(wxLIST_ALIGN_LEFT) },
    { enif_make_atom(rt.env,"wxListAlignFlags"), "wxLIST_ALIGN_TOP", rt.make_int(wxLIST_ALIGN_TOP) },
    { enif_make_atom(rt.env,"wxListAlignFlags"), "wxLIST_ALIGN_SNAP_TO_GRID", rt.make_int(wxLIST_ALIGN_SNAP_TO_GRID) },
//  From "listctrl.h": wxListAutoSize
    { enif_make_atom(rt.env,"wxListAutoSize"), "wxLIST_AUTOSIZE", rt.make_int(wxLIST_AUTOSIZE) },
    { enif_make_atom(rt.env,"wxListAutoSize"), "wxLIST_AUTOSIZE_USEHEADER", rt.make_int(wxLIST_AUTOSIZE_USEHEADER) },
//  From "listctrl.h": wxListColumnFormat
    { enif_make_atom(rt.env,"wxListColumnFormat"), "wxLIST_FORMAT_LEFT", rt.make_int(wxLIST_FORMAT_LEFT) },
    { enif_make_atom(rt.env,"wxListColumnFormat"), "wxLIST_FORMAT_RIGHT", rt.make_int(wxLIST_FORMAT_RIGHT) },
    { enif_make_atom(rt.env,"wxListColumnFormat"), "wxLIST_FORMAT_CENTRE", rt.make_int(wxLIST_FORMAT_CENTRE) },
    { enif_make_atom(rt.env,"wxListColumnFormat"), "wxLIST_FORMAT_CENTER", rt.make_int(wxLIST_FORMAT_CENTER) },
//  From "listctrl.h": wxListFindFlags
    { enif_make_atom(rt.env,"wxListFindFlags"), "wxLIST_FIND_UP", rt.make_int(wxLIST_FIND_UP) },
    { enif_make_atom(rt.env,"wxListFindFlags"), "wxLIST_FIND_DOWN", rt.make_int(wxLIST_FIND_DOWN) },
    { enif_make_atom(rt.env,"wxListFindFlags"), "wxLIST_FIND_LEFT", rt.make_int(wxLIST_FIND_LEFT) },
    { enif_make_atom(rt.env,"wxListFindFlags"), "wxLIST_FIND_RIGHT", rt.make_int(wxLIST_FIND_RIGHT) },
//  From "listctrl.h": wxListNextFlags
    { enif_make_atom(rt.env,"wxListNextFlags"), "wxLIST_NEXT_ABOVE", rt.make_int(wxLIST_NEXT_ABOVE) },
    { enif_make_atom(rt.env,"wxListNextFlags"), "wxLIST_NEXT_ALL", rt.make_int(wxLIST_NEXT_ALL) },
    { enif_make_atom(rt.env,"wxListNextFlags"), "wxLIST_NEXT_BELOW", rt.make_int(wxLIST_NEXT_BELOW) },
    { enif_make_atom(rt.env,"wxListNextFlags"), "wxLIST_NEXT_LEFT", rt.make_int(wxLIST_NEXT_LEFT) },
    { enif_make_atom(rt.env,"wxListNextFlags"), "wxLIST_NEXT_RIGHT", rt.make_int(wxLIST_NEXT_RIGHT) },
//  From "listctrl.h": wxListRectFlags
    { enif_make_atom(rt.env,"wxListRectFlags"), "wxLIST_RECT_BOUNDS", rt.make_int(wxLIST_RECT_BOUNDS) },
    { enif_make_atom(rt.env,"wxListRectFlags"), "wxLIST_RECT_ICON", rt.make_int(wxLIST_RECT_ICON) },
    { enif_make_atom(rt.env,"wxListRectFlags"), "wxLIST_RECT_LABEL", rt.make_int(wxLIST_RECT_LABEL) },
//  From "intl.h": wxLocaleCategory
    { enif_make_atom(rt.env,"wxLocaleCategory"), "wxLOCALE_CAT_NUMBER", rt.make_int(wxLOCALE_CAT_NUMBER) },
    { enif_make_atom(rt.env,"wxLocaleCategory"), "wxLOCALE_CAT_DATE", rt.make_int(wxLOCALE_CAT_DATE) },
    { enif_make_atom(rt.env,"wxLocaleCategory"), "wxLOCALE_CAT_MONEY", rt.make_int(wxLOCALE_CAT_MONEY) },
    { enif_make_atom(rt.env,"wxLocaleCategory"), "wxLOCALE_CAT_DEFAULT", rt.make_int(wxLOCALE_CAT_DEFAULT) },
//  From "intl.h": wxLocaleInfo
    { enif_make_atom(rt.env,"wxLocaleInfo"), "wxLOCALE_THOUSANDS_SEP", rt.make_int(wxLOCALE_THOUSANDS_SEP) },
    { enif_make_atom(rt.env,"wxLocaleInfo"), "wxLOCALE_DECIMAL_POINT", rt.make_int(wxLOCALE_DECIMAL_POINT) },
    { enif_make_atom(rt.env,"wxLocaleInfo"), "wxLOCALE_SHORT_DATE_FMT", rt.make_int(wxLOCALE_SHORT_DATE_FMT) },
    { enif_make_atom(rt.env,"wxLocaleInfo"), "wxLOCALE_LONG_DATE_FMT", rt.make_int(wxLOCALE_LONG_DATE_FMT) },
    { enif_make_atom(rt.env,"wxLocaleInfo"), "wxLOCALE_DATE_TIME_FMT", rt.make_int(wxLOCALE_DATE_TIME_FMT) },
    { enif_make_atom(rt.env,"wxLocaleInfo"), "wxLOCALE_TIME_FMT", rt.make_int(wxLOCALE_TIME_FMT) },
//  From "intl.h": wxLocaleInitFlags
    { enif_make_atom(rt.env,"wxLocaleInitFlags"), "wxLOCALE_DONT_LOAD_DEFAULT", rt.make_int(wxLOCALE_DONT_LOAD_DEFAULT) },
    { enif_make_atom(rt.env,"wxLocaleInitFlags"), "wxLOCALE_LOAD_DEFAULT", rt.make_int(wxLOCALE_LOAD_DEFAULT) },
//  From "log.h": wxLogLevelValues
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_FatalError", rt.make_int(wxLOG_FatalError) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Error", rt.make_int(wxLOG_Error) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Warning", rt.make_int(wxLOG_Warning) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Message", rt.make_int(wxLOG_Message) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Status", rt.make_int(wxLOG_Status) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Info", rt.make_int(wxLOG_Info) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Debug", rt.make_int(wxLOG_Debug) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Trace", rt.make_int(wxLOG_Trace) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Progress", rt.make_int(wxLOG_Progress) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_User", rt.make_int(wxLOG_User) },
    { enif_make_atom(rt.env,"wxLogLevelValues"), "wxLOG_Max", rt.make_int(wxLOG_Max) },
//  From "dc.h": wxMappingMode
    { enif_make_atom(rt.env,"wxMappingMode"), "wxMM_TEXT", rt.make_int(wxMM_TEXT) },
    { enif_make_atom(rt.env,"wxMappingMode"), "wxMM_METRIC", rt.make_int(wxMM_METRIC) },
    { enif_make_atom(rt.env,"wxMappingMode"), "wxMM_LOMETRIC", rt.make_int(wxMM_LOMETRIC) },
    { enif_make_atom(rt.env,"wxMappingMode"), "wxMM_TWIPS", rt.make_int(wxMM_TWIPS) },
    { enif_make_atom(rt.env,"wxMappingMode"), "wxMM_POINTS", rt.make_int(wxMM_POINTS) },
//  From "mousestate.h": wxMouseButton
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_ANY", rt.make_int(wxMOUSE_BTN_ANY) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_NONE", rt.make_int(wxMOUSE_BTN_NONE) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_LEFT", rt.make_int(wxMOUSE_BTN_LEFT) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_MIDDLE", rt.make_int(wxMOUSE_BTN_MIDDLE) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_RIGHT", rt.make_int(wxMOUSE_BTN_RIGHT) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_AUX1", rt.make_int(wxMOUSE_BTN_AUX1) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_AUX2", rt.make_int(wxMOUSE_BTN_AUX2) },
    { enif_make_atom(rt.env,"wxMouseButton"), "wxMOUSE_BTN_MAX", rt.make_int(wxMOUSE_BTN_MAX) },
//  From "event.h": wxMouseWheelAxis
    { enif_make_atom(rt.env,"wxMouseWheelAxis"), "wxMOUSE_WHEEL_VERTICAL", rt.make_int(wxMOUSE_WHEEL_VERTICAL) },
    { enif_make_atom(rt.env,"wxMouseWheelAxis"), "wxMOUSE_WHEEL_HORIZONTAL", rt.make_int(wxMOUSE_WHEEL_HORIZONTAL) },
//  From "defs.h": wxOrientation
    { enif_make_atom(rt.env,"wxOrientation"), "wxHORIZONTAL", rt.make_int(wxHORIZONTAL) },
    { enif_make_atom(rt.env,"wxOrientation"), "wxVERTICAL", rt.make_int(wxVERTICAL) },
    { enif_make_atom(rt.env,"wxOrientation"), "wxBOTH", rt.make_int(wxBOTH) },
    { enif_make_atom(rt.env,"wxOrientation"), "wxORIENTATION_MASK", rt.make_int(wxORIENTATION_MASK) },
//  From "defs.h": wxPaperSize
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_NONE", rt.make_int(wxPAPER_NONE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTER", rt.make_int(wxPAPER_LETTER) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LEGAL", rt.make_int(wxPAPER_LEGAL) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A4", rt.make_int(wxPAPER_A4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_CSHEET", rt.make_int(wxPAPER_CSHEET) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_DSHEET", rt.make_int(wxPAPER_DSHEET) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ESHEET", rt.make_int(wxPAPER_ESHEET) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTERSMALL", rt.make_int(wxPAPER_LETTERSMALL) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_TABLOID", rt.make_int(wxPAPER_TABLOID) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LEDGER", rt.make_int(wxPAPER_LEDGER) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_STATEMENT", rt.make_int(wxPAPER_STATEMENT) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_EXECUTIVE", rt.make_int(wxPAPER_EXECUTIVE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A3", rt.make_int(wxPAPER_A3) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A4SMALL", rt.make_int(wxPAPER_A4SMALL) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A5", rt.make_int(wxPAPER_A5) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B4", rt.make_int(wxPAPER_B4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B5", rt.make_int(wxPAPER_B5) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_FOLIO", rt.make_int(wxPAPER_FOLIO) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_QUARTO", rt.make_int(wxPAPER_QUARTO) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_10X14", rt.make_int(wxPAPER_10X14) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_11X17", rt.make_int(wxPAPER_11X17) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_NOTE", rt.make_int(wxPAPER_NOTE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_9", rt.make_int(wxPAPER_ENV_9) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_10", rt.make_int(wxPAPER_ENV_10) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_11", rt.make_int(wxPAPER_ENV_11) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_12", rt.make_int(wxPAPER_ENV_12) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_14", rt.make_int(wxPAPER_ENV_14) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_DL", rt.make_int(wxPAPER_ENV_DL) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_C5", rt.make_int(wxPAPER_ENV_C5) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_C3", rt.make_int(wxPAPER_ENV_C3) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_C4", rt.make_int(wxPAPER_ENV_C4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_C6", rt.make_int(wxPAPER_ENV_C6) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_C65", rt.make_int(wxPAPER_ENV_C65) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_B4", rt.make_int(wxPAPER_ENV_B4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_B5", rt.make_int(wxPAPER_ENV_B5) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_B6", rt.make_int(wxPAPER_ENV_B6) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_ITALY", rt.make_int(wxPAPER_ENV_ITALY) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_MONARCH", rt.make_int(wxPAPER_ENV_MONARCH) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_PERSONAL", rt.make_int(wxPAPER_ENV_PERSONAL) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_FANFOLD_US", rt.make_int(wxPAPER_FANFOLD_US) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_FANFOLD_STD_GERMAN", rt.make_int(wxPAPER_FANFOLD_STD_GERMAN) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_FANFOLD_LGL_GERMAN", rt.make_int(wxPAPER_FANFOLD_LGL_GERMAN) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ISO_B4", rt.make_int(wxPAPER_ISO_B4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JAPANESE_POSTCARD", rt.make_int(wxPAPER_JAPANESE_POSTCARD) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_9X11", rt.make_int(wxPAPER_9X11) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_10X11", rt.make_int(wxPAPER_10X11) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_15X11", rt.make_int(wxPAPER_15X11) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_ENV_INVITE", rt.make_int(wxPAPER_ENV_INVITE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTER_EXTRA", rt.make_int(wxPAPER_LETTER_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LEGAL_EXTRA", rt.make_int(wxPAPER_LEGAL_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_TABLOID_EXTRA", rt.make_int(wxPAPER_TABLOID_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A4_EXTRA", rt.make_int(wxPAPER_A4_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTER_TRANSVERSE", rt.make_int(wxPAPER_LETTER_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A4_TRANSVERSE", rt.make_int(wxPAPER_A4_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTER_EXTRA_TRANSVERSE", rt.make_int(wxPAPER_LETTER_EXTRA_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A_PLUS", rt.make_int(wxPAPER_A_PLUS) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B_PLUS", rt.make_int(wxPAPER_B_PLUS) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTER_PLUS", rt.make_int(wxPAPER_LETTER_PLUS) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A4_PLUS", rt.make_int(wxPAPER_A4_PLUS) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A5_TRANSVERSE", rt.make_int(wxPAPER_A5_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B5_TRANSVERSE", rt.make_int(wxPAPER_B5_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A3_EXTRA", rt.make_int(wxPAPER_A3_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A5_EXTRA", rt.make_int(wxPAPER_A5_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B5_EXTRA", rt.make_int(wxPAPER_B5_EXTRA) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A2", rt.make_int(wxPAPER_A2) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A3_TRANSVERSE", rt.make_int(wxPAPER_A3_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A3_EXTRA_TRANSVERSE", rt.make_int(wxPAPER_A3_EXTRA_TRANSVERSE) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_DBL_JAPANESE_POSTCARD", rt.make_int(wxPAPER_DBL_JAPANESE_POSTCARD) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A6", rt.make_int(wxPAPER_A6) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_KAKU2", rt.make_int(wxPAPER_JENV_KAKU2) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_KAKU3", rt.make_int(wxPAPER_JENV_KAKU3) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_CHOU3", rt.make_int(wxPAPER_JENV_CHOU3) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_CHOU4", rt.make_int(wxPAPER_JENV_CHOU4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_LETTER_ROTATED", rt.make_int(wxPAPER_LETTER_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A3_ROTATED", rt.make_int(wxPAPER_A3_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A4_ROTATED", rt.make_int(wxPAPER_A4_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A5_ROTATED", rt.make_int(wxPAPER_A5_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B4_JIS_ROTATED", rt.make_int(wxPAPER_B4_JIS_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B5_JIS_ROTATED", rt.make_int(wxPAPER_B5_JIS_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JAPANESE_POSTCARD_ROTATED", rt.make_int(wxPAPER_JAPANESE_POSTCARD_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED", rt.make_int(wxPAPER_DBL_JAPANESE_POSTCARD_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A6_ROTATED", rt.make_int(wxPAPER_A6_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_KAKU2_ROTATED", rt.make_int(wxPAPER_JENV_KAKU2_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_KAKU3_ROTATED", rt.make_int(wxPAPER_JENV_KAKU3_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_CHOU3_ROTATED", rt.make_int(wxPAPER_JENV_CHOU3_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_CHOU4_ROTATED", rt.make_int(wxPAPER_JENV_CHOU4_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B6_JIS", rt.make_int(wxPAPER_B6_JIS) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_B6_JIS_ROTATED", rt.make_int(wxPAPER_B6_JIS_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_12X11", rt.make_int(wxPAPER_12X11) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_YOU4", rt.make_int(wxPAPER_JENV_YOU4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_JENV_YOU4_ROTATED", rt.make_int(wxPAPER_JENV_YOU4_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_P16K", rt.make_int(wxPAPER_P16K) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_P32K", rt.make_int(wxPAPER_P32K) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_P32KBIG", rt.make_int(wxPAPER_P32KBIG) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_1", rt.make_int(wxPAPER_PENV_1) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_2", rt.make_int(wxPAPER_PENV_2) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_3", rt.make_int(wxPAPER_PENV_3) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_4", rt.make_int(wxPAPER_PENV_4) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_5", rt.make_int(wxPAPER_PENV_5) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_6", rt.make_int(wxPAPER_PENV_6) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_7", rt.make_int(wxPAPER_PENV_7) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_8", rt.make_int(wxPAPER_PENV_8) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_9", rt.make_int(wxPAPER_PENV_9) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_10", rt.make_int(wxPAPER_PENV_10) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_P16K_ROTATED", rt.make_int(wxPAPER_P16K_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_P32K_ROTATED", rt.make_int(wxPAPER_P32K_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_P32KBIG_ROTATED", rt.make_int(wxPAPER_P32KBIG_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_1_ROTATED", rt.make_int(wxPAPER_PENV_1_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_2_ROTATED", rt.make_int(wxPAPER_PENV_2_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_3_ROTATED", rt.make_int(wxPAPER_PENV_3_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_4_ROTATED", rt.make_int(wxPAPER_PENV_4_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_5_ROTATED", rt.make_int(wxPAPER_PENV_5_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_6_ROTATED", rt.make_int(wxPAPER_PENV_6_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_7_ROTATED", rt.make_int(wxPAPER_PENV_7_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_8_ROTATED", rt.make_int(wxPAPER_PENV_8_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_9_ROTATED", rt.make_int(wxPAPER_PENV_9_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_PENV_10_ROTATED", rt.make_int(wxPAPER_PENV_10_ROTATED) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A0", rt.make_int(wxPAPER_A0) },
    { enif_make_atom(rt.env,"wxPaperSize"), "wxPAPER_A1", rt.make_int(wxPAPER_A1) },
//  From "pen.h": wxPenCap
    { enif_make_atom(rt.env,"wxPenCap"), "wxCAP_INVALID", rt.make_int(wxCAP_INVALID) },
    { enif_make_atom(rt.env,"wxPenCap"), "wxCAP_ROUND", rt.make_int(wxCAP_ROUND) },
    { enif_make_atom(rt.env,"wxPenCap"), "wxCAP_PROJECTING", rt.make_int(wxCAP_PROJECTING) },
    { enif_make_atom(rt.env,"wxPenCap"), "wxCAP_BUTT", rt.make_int(wxCAP_BUTT) },
//  From "pen.h": wxPenJoin
    { enif_make_atom(rt.env,"wxPenJoin"), "wxJOIN_INVALID", rt.make_int(wxJOIN_INVALID) },
    { enif_make_atom(rt.env,"wxPenJoin"), "wxJOIN_BEVEL", rt.make_int(wxJOIN_BEVEL) },
    { enif_make_atom(rt.env,"wxPenJoin"), "wxJOIN_MITER", rt.make_int(wxJOIN_MITER) },
    { enif_make_atom(rt.env,"wxPenJoin"), "wxJOIN_ROUND", rt.make_int(wxJOIN_ROUND) },
//  From "pen.h": wxPenStyle
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_INVALID", rt.make_int(wxPENSTYLE_INVALID) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_SOLID", rt.make_int(wxPENSTYLE_SOLID) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_DOT", rt.make_int(wxPENSTYLE_DOT) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_LONG_DASH", rt.make_int(wxPENSTYLE_LONG_DASH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_SHORT_DASH", rt.make_int(wxPENSTYLE_SHORT_DASH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_DOT_DASH", rt.make_int(wxPENSTYLE_DOT_DASH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_USER_DASH", rt.make_int(wxPENSTYLE_USER_DASH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_TRANSPARENT", rt.make_int(wxPENSTYLE_TRANSPARENT) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_STIPPLE_MASK_OPAQUE", rt.make_int(wxPENSTYLE_STIPPLE_MASK_OPAQUE) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_STIPPLE_MASK", rt.make_int(wxPENSTYLE_STIPPLE_MASK) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_STIPPLE", rt.make_int(wxPENSTYLE_STIPPLE) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_BDIAGONAL_HATCH", rt.make_int(wxPENSTYLE_BDIAGONAL_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_CROSSDIAG_HATCH", rt.make_int(wxPENSTYLE_CROSSDIAG_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_FDIAGONAL_HATCH", rt.make_int(wxPENSTYLE_FDIAGONAL_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_CROSS_HATCH", rt.make_int(wxPENSTYLE_CROSS_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_HORIZONTAL_HATCH", rt.make_int(wxPENSTYLE_HORIZONTAL_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_VERTICAL_HATCH", rt.make_int(wxPENSTYLE_VERTICAL_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_FIRST_HATCH", rt.make_int(wxPENSTYLE_FIRST_HATCH) },
    { enif_make_atom(rt.env,"wxPenStyle"), "wxPENSTYLE_LAST_HATCH", rt.make_int(wxPENSTYLE_LAST_HATCH) },
//  From "gdicmn.h": wxPolygonFillMode
    { enif_make_atom(rt.env,"wxPolygonFillMode"), "wxODDEVEN_RULE", rt.make_int(wxODDEVEN_RULE) },
    { enif_make_atom(rt.env,"wxPolygonFillMode"), "wxWINDING_RULE", rt.make_int(wxWINDING_RULE) },
//  From "print.h": wxPreviewFrameModalityKind
    { enif_make_atom(rt.env,"wxPreviewFrameModalityKind"), "wxPreviewFrame_AppModal", rt.make_int(wxPreviewFrame_AppModal) },
    { enif_make_atom(rt.env,"wxPreviewFrameModalityKind"), "wxPreviewFrame_WindowModal", rt.make_int(wxPreviewFrame_WindowModal) },
    { enif_make_atom(rt.env,"wxPreviewFrameModalityKind"), "wxPreviewFrame_NonModal", rt.make_int(wxPreviewFrame_NonModal) },
//  From "cmndata.h": wxPrintBin
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_DEFAULT", rt.make_int(wxPRINTBIN_DEFAULT) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_ONLYONE", rt.make_int(wxPRINTBIN_ONLYONE) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_LOWER", rt.make_int(wxPRINTBIN_LOWER) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_MIDDLE", rt.make_int(wxPRINTBIN_MIDDLE) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_MANUAL", rt.make_int(wxPRINTBIN_MANUAL) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_ENVELOPE", rt.make_int(wxPRINTBIN_ENVELOPE) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_ENVMANUAL", rt.make_int(wxPRINTBIN_ENVMANUAL) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_AUTO", rt.make_int(wxPRINTBIN_AUTO) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_TRACTOR", rt.make_int(wxPRINTBIN_TRACTOR) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_SMALLFMT", rt.make_int(wxPRINTBIN_SMALLFMT) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_LARGEFMT", rt.make_int(wxPRINTBIN_LARGEFMT) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_LARGECAPACITY", rt.make_int(wxPRINTBIN_LARGECAPACITY) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_CASSETTE", rt.make_int(wxPRINTBIN_CASSETTE) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_FORMSOURCE", rt.make_int(wxPRINTBIN_FORMSOURCE) },
    { enif_make_atom(rt.env,"wxPrintBin"), "wxPRINTBIN_USER", rt.make_int(wxPRINTBIN_USER) },
//  From "defs.h": wxPrintMode
    { enif_make_atom(rt.env,"wxPrintMode"), "wxPRINT_MODE_NONE", rt.make_int(wxPRINT_MODE_NONE) },
    { enif_make_atom(rt.env,"wxPrintMode"), "wxPRINT_MODE_PREVIEW", rt.make_int(wxPRINT_MODE_PREVIEW) },
    { enif_make_atom(rt.env,"wxPrintMode"), "wxPRINT_MODE_FILE", rt.make_int(wxPRINT_MODE_FILE) },
    { enif_make_atom(rt.env,"wxPrintMode"), "wxPRINT_MODE_PRINTER", rt.make_int(wxPRINT_MODE_PRINTER) },
    { enif_make_atom(rt.env,"wxPrintMode"), "wxPRINT_MODE_STREAM", rt.make_int(wxPRINT_MODE_STREAM) },
//  From "defs.h": wxPrintOrientation
    { enif_make_atom(rt.env,"wxPrintOrientation"), "wxPORTRAIT", rt.make_int(wxPORTRAIT) },
    { enif_make_atom(rt.env,"wxPrintOrientation"), "wxLANDSCAPE", rt.make_int(wxLANDSCAPE) },
//  From "print.h": wxPrinterError
    { enif_make_atom(rt.env,"wxPrinterError"), "wxPRINTER_NO_ERROR", rt.make_int(wxPRINTER_NO_ERROR) },
    { enif_make_atom(rt.env,"wxPrinterError"), "wxPRINTER_CANCELLED", rt.make_int(wxPRINTER_CANCELLED) },
    { enif_make_atom(rt.env,"wxPrinterError"), "wxPRINTER_ERROR", rt.make_int(wxPRINTER_ERROR) },
//  From "dc.h": wxRasterOperationMode
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxCLEAR", rt.make_int(wxCLEAR) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxXOR", rt.make_int(wxXOR) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxINVERT", rt.make_int(wxINVERT) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxOR_REVERSE", rt.make_int(wxOR_REVERSE) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxAND_REVERSE", rt.make_int(wxAND_REVERSE) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxCOPY", rt.make_int(wxCOPY) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxAND", rt.make_int(wxAND) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxAND_INVERT", rt.make_int(wxAND_INVERT) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxNO_OP", rt.make_int(wxNO_OP) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxNOR", rt.make_int(wxNOR) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxEQUIV", rt.make_int(wxEQUIV) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxSRC_INVERT", rt.make_int(wxSRC_INVERT) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxOR_INVERT", rt.make_int(wxOR_INVERT) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxNAND", rt.make_int(wxNAND) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxOR", rt.make_int(wxOR) },
    { enif_make_atom(rt.env,"wxRasterOperationMode"), "wxSET", rt.make_int(wxSET) },
//  From "region.h": wxRegionContain
    { enif_make_atom(rt.env,"wxRegionContain"), "wxOutRegion", rt.make_int(wxOutRegion) },
    { enif_make_atom(rt.env,"wxRegionContain"), "wxPartRegion", rt.make_int(wxPartRegion) },
    { enif_make_atom(rt.env,"wxRegionContain"), "wxInRegion", rt.make_int(wxInRegion) },
//  From "layout.h": wxRelationship
    { enif_make_atom(rt.env,"wxRelationship"), "wxUnconstrained", rt.make_int(wxUnconstrained) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxAsIs", rt.make_int(wxAsIs) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxPercentOf", rt.make_int(wxPercentOf) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxAbove", rt.make_int(wxAbove) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxBelow", rt.make_int(wxBelow) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxLeftOf", rt.make_int(wxLeftOf) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxRightOf", rt.make_int(wxRightOf) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxSameAs", rt.make_int(wxSameAs) },
    { enif_make_atom(rt.env,"wxRelationship"), "wxAbsolute", rt.make_int(wxAbsolute) },
//  From "sashwin.h": wxSashDragStatus
    { enif_make_atom(rt.env,"wxSashDragStatus"), "wxSASH_STATUS_OK", rt.make_int(wxSASH_STATUS_OK) },
    { enif_make_atom(rt.env,"wxSashDragStatus"), "wxSASH_STATUS_OUT_OF_RANGE", rt.make_int(wxSASH_STATUS_OUT_OF_RANGE) },
//  From "sashwin.h": wxSashEdgePosition
    { enif_make_atom(rt.env,"wxSashEdgePosition"), "wxSASH_TOP", rt.make_int(wxSASH_TOP) },
    { enif_make_atom(rt.env,"wxSashEdgePosition"), "wxSASH_RIGHT", rt.make_int(wxSASH_RIGHT) },
    { enif_make_atom(rt.env,"wxSashEdgePosition"), "wxSASH_BOTTOM", rt.make_int(wxSASH_BOTTOM) },
    { enif_make_atom(rt.env,"wxSashEdgePosition"), "wxSASH_LEFT", rt.make_int(wxSASH_LEFT) },
    { enif_make_atom(rt.env,"wxSashEdgePosition"), "wxSASH_NONE", rt.make_int(wxSASH_NONE) },
//  From "scrolwin.h": wxScrollbarVisibility
    { enif_make_atom(rt.env,"wxScrollbarVisibility"), "wxSHOW_SB_NEVER", rt.make_int(wxSHOW_SB_NEVER) },
    { enif_make_atom(rt.env,"wxScrollbarVisibility"), "wxSHOW_SB_DEFAULT", rt.make_int(wxSHOW_SB_DEFAULT) },
    { enif_make_atom(rt.env,"wxScrollbarVisibility"), "wxSHOW_SB_ALWAYS", rt.make_int(wxSHOW_SB_ALWAYS) },
//  From "window.h": wxShowEffect
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_NONE", rt.make_int(wxSHOW_EFFECT_NONE) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_ROLL_TO_LEFT", rt.make_int(wxSHOW_EFFECT_ROLL_TO_LEFT) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_ROLL_TO_RIGHT", rt.make_int(wxSHOW_EFFECT_ROLL_TO_RIGHT) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_ROLL_TO_TOP", rt.make_int(wxSHOW_EFFECT_ROLL_TO_TOP) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_ROLL_TO_BOTTOM", rt.make_int(wxSHOW_EFFECT_ROLL_TO_BOTTOM) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_SLIDE_TO_LEFT", rt.make_int(wxSHOW_EFFECT_SLIDE_TO_LEFT) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_SLIDE_TO_RIGHT", rt.make_int(wxSHOW_EFFECT_SLIDE_TO_RIGHT) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_SLIDE_TO_TOP", rt.make_int(wxSHOW_EFFECT_SLIDE_TO_TOP) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_SLIDE_TO_BOTTOM", rt.make_int(wxSHOW_EFFECT_SLIDE_TO_BOTTOM) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_BLEND", rt.make_int(wxSHOW_EFFECT_BLEND) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_EXPAND", rt.make_int(wxSHOW_EFFECT_EXPAND) },
    { enif_make_atom(rt.env,"wxShowEffect"), "wxSHOW_EFFECT_MAX", rt.make_int(wxSHOW_EFFECT_MAX) },
//  From "utils.h": wxShutdownFlags
    { enif_make_atom(rt.env,"wxShutdownFlags"), "wxSHUTDOWN_FORCE", rt.make_int(wxSHUTDOWN_FORCE) },
    { enif_make_atom(rt.env,"wxShutdownFlags"), "wxSHUTDOWN_POWEROFF", rt.make_int(wxSHUTDOWN_POWEROFF) },
    { enif_make_atom(rt.env,"wxShutdownFlags"), "wxSHUTDOWN_REBOOT", rt.make_int(wxSHUTDOWN_REBOOT) },
    { enif_make_atom(rt.env,"wxShutdownFlags"), "wxSHUTDOWN_LOGOFF", rt.make_int(wxSHUTDOWN_LOGOFF) },
//  From "utils.h": wxSignal
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGNONE", rt.make_int(wxSIGNONE) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGHUP", rt.make_int(wxSIGHUP) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGINT", rt.make_int(wxSIGINT) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGQUIT", rt.make_int(wxSIGQUIT) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGILL", rt.make_int(wxSIGILL) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGTRAP", rt.make_int(wxSIGTRAP) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGABRT", rt.make_int(wxSIGABRT) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGEMT", rt.make_int(wxSIGEMT) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGFPE", rt.make_int(wxSIGFPE) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGKILL", rt.make_int(wxSIGKILL) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGBUS", rt.make_int(wxSIGBUS) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGSEGV", rt.make_int(wxSIGSEGV) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGSYS", rt.make_int(wxSIGSYS) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGPIPE", rt.make_int(wxSIGPIPE) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGALRM", rt.make_int(wxSIGALRM) },
    { enif_make_atom(rt.env,"wxSignal"), "wxSIGTERM", rt.make_int(wxSIGTERM) },
//  From "defs.h": wxSizerFlagBits
    { enif_make_atom(rt.env,"wxSizerFlagBits"), "wxFIXED_MINSIZE", rt.make_int(wxFIXED_MINSIZE) },
    { enif_make_atom(rt.env,"wxSizerFlagBits"), "wxRESERVE_SPACE_EVEN_IF_HIDDEN", rt.make_int(wxRESERVE_SPACE_EVEN_IF_HIDDEN) },
    { enif_make_atom(rt.env,"wxSizerFlagBits"), "wxSIZER_FLAG_BITS_MASK", rt.make_int(wxSIZER_FLAG_BITS_MASK) },
//  From "splitter.h": wxSplitMode
    { enif_make_atom(rt.env,"wxSplitMode"), "wxSPLIT_HORIZONTAL", rt.make_int(wxSPLIT_HORIZONTAL) },
    { enif_make_atom(rt.env,"wxSplitMode"), "wxSPLIT_VERTICAL", rt.make_int(wxSPLIT_VERTICAL) },
//  From "defs.h": wxStandardID
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_AUTO_LOWEST", rt.make_int(wxID_AUTO_LOWEST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_AUTO_HIGHEST", rt.make_int(wxID_AUTO_HIGHEST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_NONE", rt.make_int(wxID_NONE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SEPARATOR", rt.make_int(wxID_SEPARATOR) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ANY", rt.make_int(wxID_ANY) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_LOWEST", rt.make_int(wxID_LOWEST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OPEN", rt.make_int(wxID_OPEN) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CLOSE", rt.make_int(wxID_CLOSE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_NEW", rt.make_int(wxID_NEW) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SAVE", rt.make_int(wxID_SAVE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SAVEAS", rt.make_int(wxID_SAVEAS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REVERT", rt.make_int(wxID_REVERT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_EXIT", rt.make_int(wxID_EXIT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_UNDO", rt.make_int(wxID_UNDO) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REDO", rt.make_int(wxID_REDO) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP", rt.make_int(wxID_HELP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PRINT", rt.make_int(wxID_PRINT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PRINT_SETUP", rt.make_int(wxID_PRINT_SETUP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PAGE_SETUP", rt.make_int(wxID_PAGE_SETUP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PREVIEW", rt.make_int(wxID_PREVIEW) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ABOUT", rt.make_int(wxID_ABOUT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP_CONTENTS", rt.make_int(wxID_HELP_CONTENTS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP_INDEX", rt.make_int(wxID_HELP_INDEX) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP_SEARCH", rt.make_int(wxID_HELP_SEARCH) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP_COMMANDS", rt.make_int(wxID_HELP_COMMANDS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP_PROCEDURES", rt.make_int(wxID_HELP_PROCEDURES) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HELP_CONTEXT", rt.make_int(wxID_HELP_CONTEXT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CLOSE_ALL", rt.make_int(wxID_CLOSE_ALL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PREFERENCES", rt.make_int(wxID_PREFERENCES) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_EDIT", rt.make_int(wxID_EDIT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CUT", rt.make_int(wxID_CUT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_COPY", rt.make_int(wxID_COPY) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PASTE", rt.make_int(wxID_PASTE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CLEAR", rt.make_int(wxID_CLEAR) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FIND", rt.make_int(wxID_FIND) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_DUPLICATE", rt.make_int(wxID_DUPLICATE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SELECTALL", rt.make_int(wxID_SELECTALL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_DELETE", rt.make_int(wxID_DELETE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REPLACE", rt.make_int(wxID_REPLACE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REPLACE_ALL", rt.make_int(wxID_REPLACE_ALL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_PROPERTIES", rt.make_int(wxID_PROPERTIES) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_DETAILS", rt.make_int(wxID_VIEW_DETAILS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_LARGEICONS", rt.make_int(wxID_VIEW_LARGEICONS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_SMALLICONS", rt.make_int(wxID_VIEW_SMALLICONS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_LIST", rt.make_int(wxID_VIEW_LIST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_SORTDATE", rt.make_int(wxID_VIEW_SORTDATE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_SORTNAME", rt.make_int(wxID_VIEW_SORTNAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_SORTSIZE", rt.make_int(wxID_VIEW_SORTSIZE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_VIEW_SORTTYPE", rt.make_int(wxID_VIEW_SORTTYPE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE", rt.make_int(wxID_FILE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE1", rt.make_int(wxID_FILE1) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE2", rt.make_int(wxID_FILE2) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE3", rt.make_int(wxID_FILE3) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE4", rt.make_int(wxID_FILE4) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE5", rt.make_int(wxID_FILE5) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE6", rt.make_int(wxID_FILE6) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE7", rt.make_int(wxID_FILE7) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE8", rt.make_int(wxID_FILE8) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILE9", rt.make_int(wxID_FILE9) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OK", rt.make_int(wxID_OK) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CANCEL", rt.make_int(wxID_CANCEL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_APPLY", rt.make_int(wxID_APPLY) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_YES", rt.make_int(wxID_YES) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_NO", rt.make_int(wxID_NO) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_STATIC", rt.make_int(wxID_STATIC) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FORWARD", rt.make_int(wxID_FORWARD) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_BACKWARD", rt.make_int(wxID_BACKWARD) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_DEFAULT", rt.make_int(wxID_DEFAULT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MORE", rt.make_int(wxID_MORE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SETUP", rt.make_int(wxID_SETUP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_RESET", rt.make_int(wxID_RESET) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CONTEXT_HELP", rt.make_int(wxID_CONTEXT_HELP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_YESTOALL", rt.make_int(wxID_YESTOALL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_NOTOALL", rt.make_int(wxID_NOTOALL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ABORT", rt.make_int(wxID_ABORT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_RETRY", rt.make_int(wxID_RETRY) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_IGNORE", rt.make_int(wxID_IGNORE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ADD", rt.make_int(wxID_ADD) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REMOVE", rt.make_int(wxID_REMOVE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_UP", rt.make_int(wxID_UP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_DOWN", rt.make_int(wxID_DOWN) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HOME", rt.make_int(wxID_HOME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REFRESH", rt.make_int(wxID_REFRESH) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_STOP", rt.make_int(wxID_STOP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_INDEX", rt.make_int(wxID_INDEX) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_BOLD", rt.make_int(wxID_BOLD) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ITALIC", rt.make_int(wxID_ITALIC) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_JUSTIFY_CENTER", rt.make_int(wxID_JUSTIFY_CENTER) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_JUSTIFY_FILL", rt.make_int(wxID_JUSTIFY_FILL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_JUSTIFY_RIGHT", rt.make_int(wxID_JUSTIFY_RIGHT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_JUSTIFY_LEFT", rt.make_int(wxID_JUSTIFY_LEFT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_UNDERLINE", rt.make_int(wxID_UNDERLINE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_INDENT", rt.make_int(wxID_INDENT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_UNINDENT", rt.make_int(wxID_UNINDENT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ZOOM_100", rt.make_int(wxID_ZOOM_100) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ZOOM_FIT", rt.make_int(wxID_ZOOM_FIT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ZOOM_IN", rt.make_int(wxID_ZOOM_IN) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ZOOM_OUT", rt.make_int(wxID_ZOOM_OUT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_UNDELETE", rt.make_int(wxID_UNDELETE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_REVERT_TO_SAVED", rt.make_int(wxID_REVERT_TO_SAVED) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CDROM", rt.make_int(wxID_CDROM) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CONVERT", rt.make_int(wxID_CONVERT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_EXECUTE", rt.make_int(wxID_EXECUTE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FLOPPY", rt.make_int(wxID_FLOPPY) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HARDDISK", rt.make_int(wxID_HARDDISK) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_BOTTOM", rt.make_int(wxID_BOTTOM) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FIRST", rt.make_int(wxID_FIRST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_LAST", rt.make_int(wxID_LAST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_TOP", rt.make_int(wxID_TOP) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_INFO", rt.make_int(wxID_INFO) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_JUMP_TO", rt.make_int(wxID_JUMP_TO) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_NETWORK", rt.make_int(wxID_NETWORK) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SELECT_COLOR", rt.make_int(wxID_SELECT_COLOR) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SELECT_FONT", rt.make_int(wxID_SELECT_FONT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SORT_ASCENDING", rt.make_int(wxID_SORT_ASCENDING) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SORT_DESCENDING", rt.make_int(wxID_SORT_DESCENDING) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SPELL_CHECK", rt.make_int(wxID_SPELL_CHECK) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_STRIKETHROUGH", rt.make_int(wxID_STRIKETHROUGH) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_SYSTEM_MENU", rt.make_int(wxID_SYSTEM_MENU) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_CLOSE_FRAME", rt.make_int(wxID_CLOSE_FRAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MOVE_FRAME", rt.make_int(wxID_MOVE_FRAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_RESIZE_FRAME", rt.make_int(wxID_RESIZE_FRAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MAXIMIZE_FRAME", rt.make_int(wxID_MAXIMIZE_FRAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_ICONIZE_FRAME", rt.make_int(wxID_ICONIZE_FRAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_RESTORE_FRAME", rt.make_int(wxID_RESTORE_FRAME) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_FIRST", rt.make_int(wxID_MDI_WINDOW_FIRST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_CASCADE", rt.make_int(wxID_MDI_WINDOW_CASCADE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_TILE_HORZ", rt.make_int(wxID_MDI_WINDOW_TILE_HORZ) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_TILE_VERT", rt.make_int(wxID_MDI_WINDOW_TILE_VERT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_ARRANGE_ICONS", rt.make_int(wxID_MDI_WINDOW_ARRANGE_ICONS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_PREV", rt.make_int(wxID_MDI_WINDOW_PREV) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_NEXT", rt.make_int(wxID_MDI_WINDOW_NEXT) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_MDI_WINDOW_LAST", rt.make_int(wxID_MDI_WINDOW_LAST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OSX_MENU_FIRST", rt.make_int(wxID_OSX_MENU_FIRST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OSX_HIDE", rt.make_int(wxID_OSX_HIDE) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OSX_HIDEOTHERS", rt.make_int(wxID_OSX_HIDEOTHERS) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OSX_SHOWALL", rt.make_int(wxID_OSX_SHOWALL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OSX_SERVICES", rt.make_int(wxID_OSX_SERVICES) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_OSX_MENU_LAST", rt.make_int(wxID_OSX_MENU_LAST) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILEDLGG", rt.make_int(wxID_FILEDLGG) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_FILECTRL", rt.make_int(wxID_FILECTRL) },
    { enif_make_atom(rt.env,"wxStandardID"), "wxID_HIGHEST", rt.make_int(wxID_HIGHEST) },
//  From "gdicmn.h": wxStockCursor
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_NONE", rt.make_int(wxCURSOR_NONE) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_ARROW", rt.make_int(wxCURSOR_ARROW) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_RIGHT_ARROW", rt.make_int(wxCURSOR_RIGHT_ARROW) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_BULLSEYE", rt.make_int(wxCURSOR_BULLSEYE) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_CHAR", rt.make_int(wxCURSOR_CHAR) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_CROSS", rt.make_int(wxCURSOR_CROSS) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_HAND", rt.make_int(wxCURSOR_HAND) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_IBEAM", rt.make_int(wxCURSOR_IBEAM) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_LEFT_BUTTON", rt.make_int(wxCURSOR_LEFT_BUTTON) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_MAGNIFIER", rt.make_int(wxCURSOR_MAGNIFIER) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_MIDDLE_BUTTON", rt.make_int(wxCURSOR_MIDDLE_BUTTON) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_NO_ENTRY", rt.make_int(wxCURSOR_NO_ENTRY) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_PAINT_BRUSH", rt.make_int(wxCURSOR_PAINT_BRUSH) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_PENCIL", rt.make_int(wxCURSOR_PENCIL) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_POINT_LEFT", rt.make_int(wxCURSOR_POINT_LEFT) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_POINT_RIGHT", rt.make_int(wxCURSOR_POINT_RIGHT) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_QUESTION_ARROW", rt.make_int(wxCURSOR_QUESTION_ARROW) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_RIGHT_BUTTON", rt.make_int(wxCURSOR_RIGHT_BUTTON) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_SIZENESW", rt.make_int(wxCURSOR_SIZENESW) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_SIZENS", rt.make_int(wxCURSOR_SIZENS) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_SIZENWSE", rt.make_int(wxCURSOR_SIZENWSE) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_SIZEWE", rt.make_int(wxCURSOR_SIZEWE) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_SIZING", rt.make_int(wxCURSOR_SIZING) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_SPRAYCAN", rt.make_int(wxCURSOR_SPRAYCAN) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_WAIT", rt.make_int(wxCURSOR_WAIT) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_WATCH", rt.make_int(wxCURSOR_WATCH) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_BLANK", rt.make_int(wxCURSOR_BLANK) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_DEFAULT", rt.make_int(wxCURSOR_DEFAULT) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_ARROWWAIT", rt.make_int(wxCURSOR_ARROWWAIT) },
    { enif_make_atom(rt.env,"wxStockCursor"), "wxCURSOR_MAX", rt.make_int(wxCURSOR_MAX) },
//  From "defs.h": wxStretch
    { enif_make_atom(rt.env,"wxStretch"), "wxSTRETCH_NOT", rt.make_int(wxSTRETCH_NOT) },
    { enif_make_atom(rt.env,"wxStretch"), "wxSHRINK", rt.make_int(wxSHRINK) },
    { enif_make_atom(rt.env,"wxStretch"), "wxGROW", rt.make_int(wxGROW) },
    { enif_make_atom(rt.env,"wxStretch"), "wxEXPAND", rt.make_int(wxEXPAND) },
    { enif_make_atom(rt.env,"wxStretch"), "wxSHAPED", rt.make_int(wxSHAPED) },
    { enif_make_atom(rt.env,"wxStretch"), "wxTILE", rt.make_int(wxTILE) },
    { enif_make_atom(rt.env,"wxStretch"), "wxSTRETCH_MASK", rt.make_int(wxSTRETCH_MASK) },
//  From "settings.h": wxSystemColour
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_SCROLLBAR", rt.make_int(wxSYS_COLOUR_SCROLLBAR) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_DESKTOP", rt.make_int(wxSYS_COLOUR_DESKTOP) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_ACTIVECAPTION", rt.make_int(wxSYS_COLOUR_ACTIVECAPTION) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_INACTIVECAPTION", rt.make_int(wxSYS_COLOUR_INACTIVECAPTION) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_MENU", rt.make_int(wxSYS_COLOUR_MENU) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_WINDOW", rt.make_int(wxSYS_COLOUR_WINDOW) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_WINDOWFRAME", rt.make_int(wxSYS_COLOUR_WINDOWFRAME) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_MENUTEXT", rt.make_int(wxSYS_COLOUR_MENUTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_WINDOWTEXT", rt.make_int(wxSYS_COLOUR_WINDOWTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_CAPTIONTEXT", rt.make_int(wxSYS_COLOUR_CAPTIONTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_ACTIVEBORDER", rt.make_int(wxSYS_COLOUR_ACTIVEBORDER) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_INACTIVEBORDER", rt.make_int(wxSYS_COLOUR_INACTIVEBORDER) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_APPWORKSPACE", rt.make_int(wxSYS_COLOUR_APPWORKSPACE) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_HIGHLIGHT", rt.make_int(wxSYS_COLOUR_HIGHLIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_HIGHLIGHTTEXT", rt.make_int(wxSYS_COLOUR_HIGHLIGHTTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_BTNFACE", rt.make_int(wxSYS_COLOUR_BTNFACE) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_BTNSHADOW", rt.make_int(wxSYS_COLOUR_BTNSHADOW) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_GRAYTEXT", rt.make_int(wxSYS_COLOUR_GRAYTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_BTNTEXT", rt.make_int(wxSYS_COLOUR_BTNTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_INACTIVECAPTIONTEXT", rt.make_int(wxSYS_COLOUR_INACTIVECAPTIONTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_BTNHIGHLIGHT", rt.make_int(wxSYS_COLOUR_BTNHIGHLIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_3DDKSHADOW", rt.make_int(wxSYS_COLOUR_3DDKSHADOW) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_3DLIGHT", rt.make_int(wxSYS_COLOUR_3DLIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_INFOTEXT", rt.make_int(wxSYS_COLOUR_INFOTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_INFOBK", rt.make_int(wxSYS_COLOUR_INFOBK) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_LISTBOX", rt.make_int(wxSYS_COLOUR_LISTBOX) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_HOTLIGHT", rt.make_int(wxSYS_COLOUR_HOTLIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_GRADIENTACTIVECAPTION", rt.make_int(wxSYS_COLOUR_GRADIENTACTIVECAPTION) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_GRADIENTINACTIVECAPTION", rt.make_int(wxSYS_COLOUR_GRADIENTINACTIVECAPTION) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_MENUHILIGHT", rt.make_int(wxSYS_COLOUR_MENUHILIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_MENUBAR", rt.make_int(wxSYS_COLOUR_MENUBAR) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_LISTBOXTEXT", rt.make_int(wxSYS_COLOUR_LISTBOXTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT", rt.make_int(wxSYS_COLOUR_LISTBOXHIGHLIGHTTEXT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_BACKGROUND", rt.make_int(wxSYS_COLOUR_BACKGROUND) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_3DFACE", rt.make_int(wxSYS_COLOUR_3DFACE) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_3DSHADOW", rt.make_int(wxSYS_COLOUR_3DSHADOW) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_BTNHILIGHT", rt.make_int(wxSYS_COLOUR_BTNHILIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_3DHIGHLIGHT", rt.make_int(wxSYS_COLOUR_3DHIGHLIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_3DHILIGHT", rt.make_int(wxSYS_COLOUR_3DHILIGHT) },
    { enif_make_atom(rt.env,"wxSystemColour"), "wxSYS_COLOUR_FRAMEBK", rt.make_int(wxSYS_COLOUR_FRAMEBK) },
//  From "settings.h": wxSystemFeature
    { enif_make_atom(rt.env,"wxSystemFeature"), "wxSYS_CAN_DRAW_FRAME_DECORATIONS", rt.make_int(wxSYS_CAN_DRAW_FRAME_DECORATIONS) },
    { enif_make_atom(rt.env,"wxSystemFeature"), "wxSYS_CAN_ICONIZE_FRAME", rt.make_int(wxSYS_CAN_ICONIZE_FRAME) },
    { enif_make_atom(rt.env,"wxSystemFeature"), "wxSYS_TABLET_PRESENT", rt.make_int(wxSYS_TABLET_PRESENT) },
//  From "settings.h": wxSystemFont
    { enif_make_atom(rt.env,"wxSystemFont"), "wxSYS_OEM_FIXED_FONT", rt.make_int(wxSYS_OEM_FIXED_FONT) },
    { enif_make_atom(rt.env,"wxSystemFont"), "wxSYS_ANSI_FIXED_FONT", rt.make_int(wxSYS_ANSI_FIXED_FONT) },
    { enif_make_atom(rt.env,"wxSystemFont"), "wxSYS_ANSI_VAR_FONT", rt.make_int(wxSYS_ANSI_VAR_FONT) },
    { enif_make_atom(rt.env,"wxSystemFont"), "wxSYS_SYSTEM_FONT", rt.make_int(wxSYS_SYSTEM_FONT) },
    { enif_make_atom(rt.env,"wxSystemFont"), "wxSYS_DEVICE_DEFAULT_FONT", rt.make_int(wxSYS_DEVICE_DEFAULT_FONT) },
    { enif_make_atom(rt.env,"wxSystemFont"), "wxSYS_DEFAULT_GUI_FONT", rt.make_int(wxSYS_DEFAULT_GUI_FONT) },
//  From "settings.h": wxSystemMetric
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_MOUSE_BUTTONS", rt.make_int(wxSYS_MOUSE_BUTTONS) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_BORDER_X", rt.make_int(wxSYS_BORDER_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_BORDER_Y", rt.make_int(wxSYS_BORDER_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CURSOR_X", rt.make_int(wxSYS_CURSOR_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CURSOR_Y", rt.make_int(wxSYS_CURSOR_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_DCLICK_X", rt.make_int(wxSYS_DCLICK_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_DCLICK_Y", rt.make_int(wxSYS_DCLICK_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_DRAG_X", rt.make_int(wxSYS_DRAG_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_DRAG_Y", rt.make_int(wxSYS_DRAG_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_EDGE_X", rt.make_int(wxSYS_EDGE_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_EDGE_Y", rt.make_int(wxSYS_EDGE_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_HSCROLL_ARROW_X", rt.make_int(wxSYS_HSCROLL_ARROW_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_HSCROLL_ARROW_Y", rt.make_int(wxSYS_HSCROLL_ARROW_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_HTHUMB_X", rt.make_int(wxSYS_HTHUMB_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_ICON_X", rt.make_int(wxSYS_ICON_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_ICON_Y", rt.make_int(wxSYS_ICON_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_ICONSPACING_X", rt.make_int(wxSYS_ICONSPACING_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_ICONSPACING_Y", rt.make_int(wxSYS_ICONSPACING_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_WINDOWMIN_X", rt.make_int(wxSYS_WINDOWMIN_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_WINDOWMIN_Y", rt.make_int(wxSYS_WINDOWMIN_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_SCREEN_X", rt.make_int(wxSYS_SCREEN_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_SCREEN_Y", rt.make_int(wxSYS_SCREEN_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_FRAMESIZE_X", rt.make_int(wxSYS_FRAMESIZE_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_FRAMESIZE_Y", rt.make_int(wxSYS_FRAMESIZE_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_SMALLICON_X", rt.make_int(wxSYS_SMALLICON_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_SMALLICON_Y", rt.make_int(wxSYS_SMALLICON_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_HSCROLL_Y", rt.make_int(wxSYS_HSCROLL_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_VSCROLL_X", rt.make_int(wxSYS_VSCROLL_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_VSCROLL_ARROW_X", rt.make_int(wxSYS_VSCROLL_ARROW_X) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_VSCROLL_ARROW_Y", rt.make_int(wxSYS_VSCROLL_ARROW_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_VTHUMB_Y", rt.make_int(wxSYS_VTHUMB_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CAPTION_Y", rt.make_int(wxSYS_CAPTION_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_MENU_Y", rt.make_int(wxSYS_MENU_Y) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_NETWORK_PRESENT", rt.make_int(wxSYS_NETWORK_PRESENT) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_PENWINDOWS_PRESENT", rt.make_int(wxSYS_PENWINDOWS_PRESENT) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_SHOW_SOUNDS", rt.make_int(wxSYS_SHOW_SOUNDS) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_SWAP_BUTTONS", rt.make_int(wxSYS_SWAP_BUTTONS) },
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_DCLICK_MSEC", rt.make_int(wxSYS_DCLICK_MSEC) },
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CARET_ON_MSEC", rt.make_int(wxSYS_CARET_ON_MSEC) },
#else
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CARET_ON_MSEC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CARET_OFF_MSEC", rt.make_int(wxSYS_CARET_OFF_MSEC) },
#else
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CARET_OFF_MSEC", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,1)
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CARET_TIMEOUT_MSEC", rt.make_int(wxSYS_CARET_TIMEOUT_MSEC) },
#else
    { enif_make_atom(rt.env,"wxSystemMetric"), "wxSYS_CARET_TIMEOUT_MSEC", WXE_ATOM_undefined },
#endif
//  From "settings.h": wxSystemScreenType
    { enif_make_atom(rt.env,"wxSystemScreenType"), "wxSYS_SCREEN_NONE", rt.make_int(wxSYS_SCREEN_NONE) },
    { enif_make_atom(rt.env,"wxSystemScreenType"), "wxSYS_SCREEN_TINY", rt.make_int(wxSYS_SCREEN_TINY) },
    { enif_make_atom(rt.env,"wxSystemScreenType"), "wxSYS_SCREEN_PDA", rt.make_int(wxSYS_SCREEN_PDA) },
    { enif_make_atom(rt.env,"wxSystemScreenType"), "wxSYS_SCREEN_SMALL", rt.make_int(wxSYS_SCREEN_SMALL) },
    { enif_make_atom(rt.env,"wxSystemScreenType"), "wxSYS_SCREEN_DESKTOP", rt.make_int(wxSYS_SCREEN_DESKTOP) },
//  From "taskbar.h": wxTaskBarIconType
    { enif_make_atom(rt.env,"wxTaskBarIconType"), "wxTBI_DOCK", rt.make_int(wxTBI_DOCK) },
    { enif_make_atom(rt.env,"wxTaskBarIconType"), "wxTBI_CUSTOM_STATUSITEM", rt.make_int(wxTBI_CUSTOM_STATUSITEM) },
    { enif_make_atom(rt.env,"wxTaskBarIconType"), "wxTBI_DEFAULT_TYPE", rt.make_int(wxTBI_DEFAULT_TYPE) },
//  From "textctrl.h": wxTextAttrAlignment
    { enif_make_atom(rt.env,"wxTextAttrAlignment"), "wxTEXT_ALIGNMENT_DEFAULT", rt.make_int(wxTEXT_ALIGNMENT_DEFAULT) },
    { enif_make_atom(rt.env,"wxTextAttrAlignment"), "wxTEXT_ALIGNMENT_LEFT", rt.make_int(wxTEXT_ALIGNMENT_LEFT) },
    { enif_make_atom(rt.env,"wxTextAttrAlignment"), "wxTEXT_ALIGNMENT_CENTRE", rt.make_int(wxTEXT_ALIGNMENT_CENTRE) },
    { enif_make_atom(rt.env,"wxTextAttrAlignment"), "wxTEXT_ALIGNMENT_CENTER", rt.make_int(wxTEXT_ALIGNMENT_CENTER) },
    { enif_make_atom(rt.env,"wxTextAttrAlignment"), "wxTEXT_ALIGNMENT_RIGHT", rt.make_int(wxTEXT_ALIGNMENT_RIGHT) },
    { enif_make_atom(rt.env,"wxTextAttrAlignment"), "wxTEXT_ALIGNMENT_JUSTIFIED", rt.make_int(wxTEXT_ALIGNMENT_JUSTIFIED) },
//  From "textctrl.h": wxTextAttrBulletStyle
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_NONE", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_NONE) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_ARABIC", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_ARABIC) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_LETTERS_UPPER", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_LETTERS_UPPER) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_LETTERS_LOWER", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_LETTERS_LOWER) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_ROMAN_UPPER", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_ROMAN_UPPER) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_ROMAN_LOWER", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_ROMAN_LOWER) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_SYMBOL", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_SYMBOL) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_BITMAP", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_BITMAP) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_PARENTHESES", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_PARENTHESES) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_PERIOD", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_PERIOD) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_STANDARD", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_STANDARD) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_RIGHT_PARENTHESIS", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_RIGHT_PARENTHESIS) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_OUTLINE", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_OUTLINE) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_ALIGN_LEFT", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_ALIGN_LEFT) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_ALIGN_RIGHT", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_ALIGN_RIGHT) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_ALIGN_CENTRE", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_ALIGN_CENTRE) },
    { enif_make_atom(rt.env,"wxTextAttrBulletStyle"), "wxTEXT_ATTR_BULLET_STYLE_CONTINUATION", rt.make_int(wxTEXT_ATTR_BULLET_STYLE_CONTINUATION) },
//  From "textctrl.h": wxTextAttrEffects
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_NONE", rt.make_int(wxTEXT_ATTR_EFFECT_NONE) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_CAPITALS", rt.make_int(wxTEXT_ATTR_EFFECT_CAPITALS) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_SMALL_CAPITALS", rt.make_int(wxTEXT_ATTR_EFFECT_SMALL_CAPITALS) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_STRIKETHROUGH", rt.make_int(wxTEXT_ATTR_EFFECT_STRIKETHROUGH) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_DOUBLE_STRIKETHROUGH", rt.make_int(wxTEXT_ATTR_EFFECT_DOUBLE_STRIKETHROUGH) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_SHADOW", rt.make_int(wxTEXT_ATTR_EFFECT_SHADOW) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_EMBOSS", rt.make_int(wxTEXT_ATTR_EFFECT_EMBOSS) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_OUTLINE", rt.make_int(wxTEXT_ATTR_EFFECT_OUTLINE) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_ENGRAVE", rt.make_int(wxTEXT_ATTR_EFFECT_ENGRAVE) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_SUPERSCRIPT", rt.make_int(wxTEXT_ATTR_EFFECT_SUPERSCRIPT) },
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_SUBSCRIPT", rt.make_int(wxTEXT_ATTR_EFFECT_SUBSCRIPT) },
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_RTL", rt.make_int(wxTEXT_ATTR_EFFECT_RTL) },
#else
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_RTL", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_SUPPRESS_HYPHENATION", rt.make_int(wxTEXT_ATTR_EFFECT_SUPPRESS_HYPHENATION) },
#else
    { enif_make_atom(rt.env,"wxTextAttrEffects"), "wxTEXT_ATTR_EFFECT_SUPPRESS_HYPHENATION", WXE_ATOM_undefined },
#endif
//  From "textctrl.h": wxTextAttrFlags
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_TEXT_COLOUR", rt.make_int(wxTEXT_ATTR_TEXT_COLOUR) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_BACKGROUND_COLOUR", rt.make_int(wxTEXT_ATTR_BACKGROUND_COLOUR) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_FACE", rt.make_int(wxTEXT_ATTR_FONT_FACE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_POINT_SIZE", rt.make_int(wxTEXT_ATTR_FONT_POINT_SIZE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_PIXEL_SIZE", rt.make_int(wxTEXT_ATTR_FONT_PIXEL_SIZE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_WEIGHT", rt.make_int(wxTEXT_ATTR_FONT_WEIGHT) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_ITALIC", rt.make_int(wxTEXT_ATTR_FONT_ITALIC) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_UNDERLINE", rt.make_int(wxTEXT_ATTR_FONT_UNDERLINE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_STRIKETHROUGH", rt.make_int(wxTEXT_ATTR_FONT_STRIKETHROUGH) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_ENCODING", rt.make_int(wxTEXT_ATTR_FONT_ENCODING) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_FAMILY", rt.make_int(wxTEXT_ATTR_FONT_FAMILY) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT_SIZE", rt.make_int(wxTEXT_ATTR_FONT_SIZE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_FONT", rt.make_int(wxTEXT_ATTR_FONT) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_ALIGNMENT", rt.make_int(wxTEXT_ATTR_ALIGNMENT) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_LEFT_INDENT", rt.make_int(wxTEXT_ATTR_LEFT_INDENT) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_RIGHT_INDENT", rt.make_int(wxTEXT_ATTR_RIGHT_INDENT) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_TABS", rt.make_int(wxTEXT_ATTR_TABS) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_PARA_SPACING_AFTER", rt.make_int(wxTEXT_ATTR_PARA_SPACING_AFTER) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_PARA_SPACING_BEFORE", rt.make_int(wxTEXT_ATTR_PARA_SPACING_BEFORE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_LINE_SPACING", rt.make_int(wxTEXT_ATTR_LINE_SPACING) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_CHARACTER_STYLE_NAME", rt.make_int(wxTEXT_ATTR_CHARACTER_STYLE_NAME) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_PARAGRAPH_STYLE_NAME", rt.make_int(wxTEXT_ATTR_PARAGRAPH_STYLE_NAME) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_LIST_STYLE_NAME", rt.make_int(wxTEXT_ATTR_LIST_STYLE_NAME) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_BULLET_STYLE", rt.make_int(wxTEXT_ATTR_BULLET_STYLE) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_BULLET_NUMBER", rt.make_int(wxTEXT_ATTR_BULLET_NUMBER) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_BULLET_TEXT", rt.make_int(wxTEXT_ATTR_BULLET_TEXT) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_BULLET_NAME", rt.make_int(wxTEXT_ATTR_BULLET_NAME) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_BULLET", rt.make_int(wxTEXT_ATTR_BULLET) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_URL", rt.make_int(wxTEXT_ATTR_URL) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_PAGE_BREAK", rt.make_int(wxTEXT_ATTR_PAGE_BREAK) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_EFFECTS", rt.make_int(wxTEXT_ATTR_EFFECTS) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_OUTLINE_LEVEL", rt.make_int(wxTEXT_ATTR_OUTLINE_LEVEL) },
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_AVOID_PAGE_BREAK_BEFORE", rt.make_int(wxTEXT_ATTR_AVOID_PAGE_BREAK_BEFORE) },
#else
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_AVOID_PAGE_BREAK_BEFORE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_AVOID_PAGE_BREAK_AFTER", rt.make_int(wxTEXT_ATTR_AVOID_PAGE_BREAK_AFTER) },
#else
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_AVOID_PAGE_BREAK_AFTER", WXE_ATOM_undefined },
#endif
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_CHARACTER", rt.make_int(wxTEXT_ATTR_CHARACTER) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_PARAGRAPH", rt.make_int(wxTEXT_ATTR_PARAGRAPH) },
    { enif_make_atom(rt.env,"wxTextAttrFlags"), "wxTEXT_ATTR_ALL", rt.make_int(wxTEXT_ATTR_ALL) },
//  From "textctrl.h": wxTextAttrLineSpacing
    { enif_make_atom(rt.env,"wxTextAttrLineSpacing"), "wxTEXT_ATTR_LINE_SPACING_NORMAL", rt.make_int(wxTEXT_ATTR_LINE_SPACING_NORMAL) },
    { enif_make_atom(rt.env,"wxTextAttrLineSpacing"), "wxTEXT_ATTR_LINE_SPACING_HALF", rt.make_int(wxTEXT_ATTR_LINE_SPACING_HALF) },
    { enif_make_atom(rt.env,"wxTextAttrLineSpacing"), "wxTEXT_ATTR_LINE_SPACING_TWICE", rt.make_int(wxTEXT_ATTR_LINE_SPACING_TWICE) },
//  From "textctrl.h": wxTextAttrUnderlineType
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_NONE", rt.make_int(wxTEXT_ATTR_UNDERLINE_NONE) },
#else
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_NONE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_SOLID", rt.make_int(wxTEXT_ATTR_UNDERLINE_SOLID) },
#else
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_SOLID", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_DOUBLE", rt.make_int(wxTEXT_ATTR_UNDERLINE_DOUBLE) },
#else
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_DOUBLE", WXE_ATOM_undefined },
#endif
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_SPECIAL", rt.make_int(wxTEXT_ATTR_UNDERLINE_SPECIAL) },
#else
    { enif_make_atom(rt.env,"wxTextAttrUnderlineType"), "wxTEXT_ATTR_UNDERLINE_SPECIAL", WXE_ATOM_undefined },
#endif
//  From "textctrl.h": wxTextCtrlHitTestResult
    { enif_make_atom(rt.env,"wxTextCtrlHitTestResult"), "wxTE_HT_UNKNOWN", rt.make_int(wxTE_HT_UNKNOWN) },
    { enif_make_atom(rt.env,"wxTextCtrlHitTestResult"), "wxTE_HT_BEFORE", rt.make_int(wxTE_HT_BEFORE) },
    { enif_make_atom(rt.env,"wxTextCtrlHitTestResult"), "wxTE_HT_ON_TEXT", rt.make_int(wxTE_HT_ON_TEXT) },
    { enif_make_atom(rt.env,"wxTextCtrlHitTestResult"), "wxTE_HT_BELOW", rt.make_int(wxTE_HT_BELOW) },
    { enif_make_atom(rt.env,"wxTextCtrlHitTestResult"), "wxTE_HT_BEYOND", rt.make_int(wxTE_HT_BEYOND) },
//  From "toolbar.h": wxToolBarToolStyle
    { enif_make_atom(rt.env,"wxToolBarToolStyle"), "wxTOOL_STYLE_BUTTON", rt.make_int(wxTOOL_STYLE_BUTTON) },
    { enif_make_atom(rt.env,"wxToolBarToolStyle"), "wxTOOL_STYLE_SEPARATOR", rt.make_int(wxTOOL_STYLE_SEPARATOR) },
    { enif_make_atom(rt.env,"wxToolBarToolStyle"), "wxTOOL_STYLE_CONTROL", rt.make_int(wxTOOL_STYLE_CONTROL) },
//  From "treebase.h": wxTreeItemIcon
    { enif_make_atom(rt.env,"wxTreeItemIcon"), "wxTreeItemIcon_Normal", rt.make_int(wxTreeItemIcon_Normal) },
    { enif_make_atom(rt.env,"wxTreeItemIcon"), "wxTreeItemIcon_Selected", rt.make_int(wxTreeItemIcon_Selected) },
    { enif_make_atom(rt.env,"wxTreeItemIcon"), "wxTreeItemIcon_Expanded", rt.make_int(wxTreeItemIcon_Expanded) },
    { enif_make_atom(rt.env,"wxTreeItemIcon"), "wxTreeItemIcon_SelectedExpanded", rt.make_int(wxTreeItemIcon_SelectedExpanded) },
    { enif_make_atom(rt.env,"wxTreeItemIcon"), "wxTreeItemIcon_Max", rt.make_int(wxTreeItemIcon_Max) },
//  From "defs.h": wxUpdateUI
    { enif_make_atom(rt.env,"wxUpdateUI"), "wxUPDATE_UI_NONE", rt.make_int(wxUPDATE_UI_NONE) },
    { enif_make_atom(rt.env,"wxUpdateUI"), "wxUPDATE_UI_RECURSE", rt.make_int(wxUPDATE_UI_RECURSE) },
    { enif_make_atom(rt.env,"wxUpdateUI"), "wxUPDATE_UI_FROMIDLE", rt.make_int(wxUPDATE_UI_FROMIDLE) },
//  From "event.h": wxUpdateUIMode
    { enif_make_atom(rt.env,"wxUpdateUIMode"), "wxUPDATE_UI_PROCESS_ALL", rt.make_int(wxUPDATE_UI_PROCESS_ALL) },
    { enif_make_atom(rt.env,"wxUpdateUIMode"), "wxUPDATE_UI_PROCESS_SPECIFIED", rt.make_int(wxUPDATE_UI_PROCESS_SPECIFIED) },
//  From "webview.h": wxWebViewFindFlags
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_WRAP", rt.make_int(wxWEBVIEW_FIND_WRAP) },
#else
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_WRAP", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_ENTIRE_WORD", rt.make_int(wxWEBVIEW_FIND_ENTIRE_WORD) },
#else
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_ENTIRE_WORD", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_MATCH_CASE", rt.make_int(wxWEBVIEW_FIND_MATCH_CASE) },
#else
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_MATCH_CASE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_HIGHLIGHT_RESULT", rt.make_int(wxWEBVIEW_FIND_HIGHLIGHT_RESULT) },
#else
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_HIGHLIGHT_RESULT", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_BACKWARDS", rt.make_int(wxWEBVIEW_FIND_BACKWARDS) },
#else
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_BACKWARDS", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_DEFAULT", rt.make_int(wxWEBVIEW_FIND_DEFAULT) },
#else
    { enif_make_atom(rt.env,"wxWebViewFindFlags"), "wxWEBVIEW_FIND_DEFAULT", WXE_ATOM_undefined },
#endif
//  From "webview.h": wxWebViewIE_EmulationLevel
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_DEFAULT", rt.make_int(wxWEBVIEWIE_EMU_DEFAULT) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_DEFAULT", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE7", rt.make_int(wxWEBVIEWIE_EMU_IE7) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE7", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE8", rt.make_int(wxWEBVIEWIE_EMU_IE8) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE8", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE8_FORCE", rt.make_int(wxWEBVIEWIE_EMU_IE8_FORCE) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE8_FORCE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE9", rt.make_int(wxWEBVIEWIE_EMU_IE9) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE9", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE9_FORCE", rt.make_int(wxWEBVIEWIE_EMU_IE9_FORCE) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE9_FORCE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE10", rt.make_int(wxWEBVIEWIE_EMU_IE10) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE10", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE10_FORCE", rt.make_int(wxWEBVIEWIE_EMU_IE10_FORCE) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE10_FORCE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE11", rt.make_int(wxWEBVIEWIE_EMU_IE11) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE11", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW_IE && wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE11_FORCE", rt.make_int(wxWEBVIEWIE_EMU_IE11_FORCE) },
#else
    { enif_make_atom(rt.env,"wxWebViewIE_EmulationLevel"), "wxWEBVIEWIE_EMU_IE11_FORCE", WXE_ATOM_undefined },
#endif
//  From "webview.h": wxWebViewNavigationActionFlags
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxWebViewNavigationActionFlags"), "wxWEBVIEW_NAV_ACTION_NONE", rt.make_int(wxWEBVIEW_NAV_ACTION_NONE) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationActionFlags"), "wxWEBVIEW_NAV_ACTION_NONE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxWebViewNavigationActionFlags"), "wxWEBVIEW_NAV_ACTION_USER", rt.make_int(wxWEBVIEW_NAV_ACTION_USER) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationActionFlags"), "wxWEBVIEW_NAV_ACTION_USER", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW && wxCHECK_VERSION(3,1,2)
    { enif_make_atom(rt.env,"wxWebViewNavigationActionFlags"), "wxWEBVIEW_NAV_ACTION_OTHER", rt.make_int(wxWEBVIEW_NAV_ACTION_OTHER) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationActionFlags"), "wxWEBVIEW_NAV_ACTION_OTHER", WXE_ATOM_undefined },
#endif
//  From "webview.h": wxWebViewNavigationError
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_CONNECTION", rt.make_int(wxWEBVIEW_NAV_ERR_CONNECTION) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_CONNECTION", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_CERTIFICATE", rt.make_int(wxWEBVIEW_NAV_ERR_CERTIFICATE) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_CERTIFICATE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_AUTH", rt.make_int(wxWEBVIEW_NAV_ERR_AUTH) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_AUTH", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_SECURITY", rt.make_int(wxWEBVIEW_NAV_ERR_SECURITY) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_SECURITY", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_NOT_FOUND", rt.make_int(wxWEBVIEW_NAV_ERR_NOT_FOUND) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_NOT_FOUND", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_REQUEST", rt.make_int(wxWEBVIEW_NAV_ERR_REQUEST) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_REQUEST", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_USER_CANCELLED", rt.make_int(wxWEBVIEW_NAV_ERR_USER_CANCELLED) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_USER_CANCELLED", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_OTHER", rt.make_int(wxWEBVIEW_NAV_ERR_OTHER) },
#else
    { enif_make_atom(rt.env,"wxWebViewNavigationError"), "wxWEBVIEW_NAV_ERR_OTHER", WXE_ATOM_undefined },
#endif
//  From "webview.h": wxWebViewReloadFlags
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewReloadFlags"), "wxWEBVIEW_RELOAD_DEFAULT", rt.make_int(wxWEBVIEW_RELOAD_DEFAULT) },
#else
    { enif_make_atom(rt.env,"wxWebViewReloadFlags"), "wxWEBVIEW_RELOAD_DEFAULT", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewReloadFlags"), "wxWEBVIEW_RELOAD_NO_CACHE", rt.make_int(wxWEBVIEW_RELOAD_NO_CACHE) },
#else
    { enif_make_atom(rt.env,"wxWebViewReloadFlags"), "wxWEBVIEW_RELOAD_NO_CACHE", WXE_ATOM_undefined },
#endif
//  From "webview.h": wxWebViewZoom
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_TINY", rt.make_int(wxWEBVIEW_ZOOM_TINY) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_TINY", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_SMALL", rt.make_int(wxWEBVIEW_ZOOM_SMALL) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_SMALL", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_MEDIUM", rt.make_int(wxWEBVIEW_ZOOM_MEDIUM) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_MEDIUM", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_LARGE", rt.make_int(wxWEBVIEW_ZOOM_LARGE) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_LARGE", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_LARGEST", rt.make_int(wxWEBVIEW_ZOOM_LARGEST) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoom"), "wxWEBVIEW_ZOOM_LARGEST", WXE_ATOM_undefined },
#endif
//  From "webview.h": wxWebViewZoomType
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoomType"), "wxWEBVIEW_ZOOM_TYPE_LAYOUT", rt.make_int(wxWEBVIEW_ZOOM_TYPE_LAYOUT) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoomType"), "wxWEBVIEW_ZOOM_TYPE_LAYOUT", WXE_ATOM_undefined },
#endif
#if WXE_WEBVIEW
    { enif_make_atom(rt.env,"wxWebViewZoomType"), "wxWEBVIEW_ZOOM_TYPE_TEXT", rt.make_int(wxWEBVIEW_ZOOM_TYPE_TEXT) },
#else
    { enif_make_atom(rt.env,"wxWebViewZoomType"), "wxWEBVIEW_ZOOM_TYPE_TEXT", WXE_ATOM_undefined },
#endif
//  From "window.h": wxWindowVariant
    { enif_make_atom(rt.env,"wxWindowVariant"), "wxWINDOW_VARIANT_NORMAL", rt.make_int(wxWINDOW_VARIANT_NORMAL) },
    { enif_make_atom(rt.env,"wxWindowVariant"), "wxWINDOW_VARIANT_SMALL", rt.make_int(wxWINDOW_VARIANT_SMALL) },
    { enif_make_atom(rt.env,"wxWindowVariant"), "wxWINDOW_VARIANT_MINI", rt.make_int(wxWINDOW_VARIANT_MINI) },
    { enif_make_atom(rt.env,"wxWindowVariant"), "wxWINDOW_VARIANT_LARGE", rt.make_int(wxWINDOW_VARIANT_LARGE) },
    { enif_make_atom(rt.env,"wxWindowVariant"), "wxWINDOW_VARIANT_MAX", rt.make_int(wxWINDOW_VARIANT_MAX) },
//  From "xmlres.h": wxXmlResourceFlags
    { enif_make_atom(rt.env,"wxXmlResourceFlags"), "wxXRC_USE_LOCALE", rt.make_int(wxXRC_USE_LOCALE) },
    { enif_make_atom(rt.env,"wxXmlResourceFlags"), "wxXRC_NO_SUBCLASSING", rt.make_int(wxXRC_NO_SUBCLASSING) },
    { enif_make_atom(rt.env,"wxXmlResourceFlags"), "wxXRC_NO_RELOADING", rt.make_int(wxXRC_NO_RELOADING) },
#if wxCHECK_VERSION(3,1,3)
    { enif_make_atom(rt.env,"wxXmlResourceFlags"), "wxXRC_USE_ENVVARS", rt.make_int(wxXRC_USE_ENVVARS) },
#else
    { enif_make_atom(rt.env,"wxXmlResourceFlags"), "wxXRC_USE_ENVVARS", WXE_ATOM_undefined },
#endif
  };
  int sz = sizeof(defs) / sizeof(defs[0]);
  std::vector <ERL_NIF_TERM> consts;
  /* INITIALIZING the CONSTS array in two steps make C++ compilation a lot faster */
  for( int i = 0; i <  sz; i++) {
    consts.push_back(enif_make_tuple3(rt.env, defs[i].type, enif_make_atom(rt.env, defs[i].key), defs[i].value));
  }
  rt.send(enif_make_list_from_array(rt.env, consts.data(), consts.size()));
}
