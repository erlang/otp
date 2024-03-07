/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2024. All Rights Reserved.
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
#include <wx/caret.h>
#include <wx/tooltip.h>
#include <wx/gbsizer.h>
#include <wx/splash.h>
#include <wx/grid.h>
#include <wx/image.h>
#include <wx/tglbtn.h>
#include <wx/calctrl.h>
#include <wx/dirctrl.h>
#include <wx/listctrl.h>
#include <wx/treectrl.h>
#include <wx/spinbutt.h>
#include <wx/spinctrl.h>
#include <wx/colordlg.h>
#include <wx/fdrepdlg.h>
#include <wx/fontdlg.h>
#include <wx/progdlg.h>
#include <wx/printdlg.h>
#include <wx/display.h>
#include <wx/dcbuffer.h>
#include <wx/dcmirror.h>
#include <wx/glcanvas.h>
#include <wx/dcps.h>
#include <wx/xrc/xmlres.h>
#include <wx/html/htmprint.h>
#include <wx/stc/stc.h>
#include <wx/minifram.h>
#include <wx/sashwin.h>
#include <wx/laywin.h>
#include <wx/graphics.h>
#include <wx/dcgraph.h>
#include <wx/aui/aui.h>
#include <wx/datectrl.h>
#include <wx/filepicker.h>
#include <wx/fontpicker.h>
#include <wx/clrpicker.h>
#include <wx/statline.h>
#include <wx/clipbrd.h>
#include <wx/splitter.h>
#include <wx/choicebk.h>
#include <wx/toolbook.h>
#include <wx/listbook.h>
#include <wx/treebook.h>
#include <wx/taskbar.h>
#include <wx/textctrl.h>
#include <wx/popupwin.h>
#include <wx/html/htmlwin.h>
#include <wx/html/htmlcell.h>
#include <wx/filename.h>
#include <wx/sysopt.h>
#include <wx/overlay.h>
#include <wx/notifmsg.h>
#include <wx/webview.h>
#if wxUSE_WEBVIEW && wxUSE_WEBVIEW_IE
#include <wx/msw/webview_ie.h>
#endif
#if wxUSE_GLCANVAS_EGL && !wxCHECK_VERSION(3,2,3)
#include <EGL/egl.h>
#endif


#ifndef wxICON_DEFAULT_BITMAP_TYPE
  #define wxICON_DEFAULT_BITMAP_TYPE wxBITMAP_TYPE_ICO_RESOURCE
#endif


#if defined(wxSTC_DISABLE_MACRO_DEPRECATIONS) && defined(wxSTC_DEPRECATED_MACRO_VALUE)
#undef wxSTC_DEPRECATED_MACRO_VALUE
#define wxSTC_DEPRECATED_MACRO_VALUE(value, msg) value
#endif
