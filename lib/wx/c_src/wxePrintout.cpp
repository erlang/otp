/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

#include <wx/wx.h>
#include "wxe_impl.h"
#include "wxe_return.h"
#include "gen/wxe_macros.h"
#include "gen/wxe_derived_dest.h"

/* *****************************************************************/
/* Special Class impls */

#define INVOKE_CALLBACK_INIT(port, callback, class_str)		        \
  {									\
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);	\
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);	\
    rt.addInt(callback);						\
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), class_str);

#define INVOKE_CALLBACK_END(port, args)					\
  rt.endList(1 + (args));						\
  rt.addAtom("_wx_invoke_cb_");						\
  rt.addTupleCount(3);							\
  rt.send();								\
  handle_event_callback(WXE_DRV_PORT_HANDLE, memenv->owner); 			\
 }

#define INVOKE_CALLBACK(port, callback, class_str)	\
  INVOKE_CALLBACK_INIT(port, callback, class_str);	\
  INVOKE_CALLBACK_END(port, 0)

/* *****************************************************************/
/* Printing special */

wxEPrintout::~wxEPrintout() {
  clear_cb(port, onPrintPage);
  clear_cb(port, onPreparePrinting);
  clear_cb(port, onBeginPrinting);
  clear_cb(port, onEndPrinting);
  clear_cb(port, onBeginDocument);
  clear_cb(port, onEndDocument);
  clear_cb(port, hasPage);
  clear_cb(port, getPageInfo);

  ((WxeApp *)wxTheApp)->clearPtr(this);
}

bool wxEPrintout::OnBeginDocument(int startPage, int endPage)
{
  if(onBeginDocument) {
    INVOKE_CALLBACK_INIT(port, onBeginDocument, "wxPrintout");
    rt.addInt(startPage);
    rt.addInt(endPage);
    INVOKE_CALLBACK_END(port, 2);
    if(((WxeApp *) wxTheApp)->cb_buff) {
      int res = * (int*) ((WxeApp *) wxTheApp)->cb_buff;
      driver_free(((WxeApp *) wxTheApp)->cb_buff);
      ((WxeApp *) wxTheApp)->cb_buff = NULL;
      return res;
    }
  }
  return wxPrintout::OnBeginDocument(startPage,endPage);
}

void wxEPrintout::OnEndDocument()
{
  if(onEndDocument) {
    INVOKE_CALLBACK(port, onEndDocument, "wxPrintout");
  } else {
    wxPrintout::OnEndDocument();
  }
}

void wxEPrintout::OnBeginPrinting()
{

  if(onBeginPrinting) {
    INVOKE_CALLBACK(port, onBeginPrinting, "wxPrintout");
  } else {
    wxPrintout::OnBeginPrinting();
  }
}

void wxEPrintout::OnEndPrinting()
{

  if(onEndPrinting) {
    INVOKE_CALLBACK(port, onEndPrinting, "wxPrintout");
  } else {
    wxPrintout::OnEndPrinting();
  }
}

void wxEPrintout::OnPreparePrinting()
{

  if(onPreparePrinting) {
    INVOKE_CALLBACK(port, onPreparePrinting, "wxPrintout");
  } else {
    wxPrintout::OnPreparePrinting();
  }
}

bool wxEPrintout::HasPage(int page)
{

  if(hasPage) {
    INVOKE_CALLBACK_INIT(port, hasPage, "wxPrintout");
    rt.addInt(page);
    INVOKE_CALLBACK_END(port, 1);
    if(((WxeApp *) wxTheApp)->cb_buff) {
      int res = * (int*) ((WxeApp *) wxTheApp)->cb_buff;
      driver_free(((WxeApp *) wxTheApp)->cb_buff);
      ((WxeApp *) wxTheApp)->cb_buff = NULL;
      return res;
    }
  }
  return wxPrintout::HasPage(page);
}

bool wxEPrintout::OnPrintPage(int page)
{
  INVOKE_CALLBACK_INIT(port, onPrintPage, "wxPrintout");
  rt.addInt(page);
  INVOKE_CALLBACK_END(port, 1);
  if(((WxeApp *) wxTheApp)->cb_buff) {
    int res = * (int*) ((WxeApp *) wxTheApp)->cb_buff;
    driver_free(((WxeApp *) wxTheApp)->cb_buff);
    ((WxeApp *) wxTheApp)->cb_buff = NULL;
    return res;
  }
  return FALSE;
}

void wxEPrintout::GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo)
{
  if(getPageInfo) {
    INVOKE_CALLBACK(port, getPageInfo, "wxPrintout");
    if(((WxeApp *) wxTheApp)->cb_buff) {
      char * bp = ((WxeApp *) wxTheApp)->cb_buff;
      *minPage  = *(int *) bp; bp += 4;
      *maxPage  = *(int *) bp; bp += 4;
      *pageFrom = *(int *) bp; bp += 4;
      *pageTo   = *(int *) bp; bp += 4;
      driver_free(((WxeApp *) wxTheApp)->cb_buff);
      ((WxeApp *) wxTheApp)->cb_buff = NULL;
    }
  }
  wxPrintout::GetPageInfo(minPage, maxPage, pageFrom, pageTo);
}

/* *****************************************************************/
// ListCtrl with callbacks for VIRTUAL_TABLES

wxString EwxListCtrl::OnGetItemText(long item, long col) const {
  if(onGetItemText) {
    INVOKE_CALLBACK_INIT(port, onGetItemText, "wxListCtrl");
    rt.addInt(item);
    rt.addInt(col);
    INVOKE_CALLBACK_END(port, 2);
    if(((WxeApp *) wxTheApp)->cb_buff) {
      char * bp = ((WxeApp *) wxTheApp)->cb_buff;
      wxString str = wxString(bp, wxConvUTF8);
      driver_free(((WxeApp *) wxTheApp)->cb_buff);
      ((WxeApp *) wxTheApp)->cb_buff = NULL;
      return str;
    }
  }
  return wxT("OnGetItemText not correctly defined");
}

wxListItemAttr* EwxListCtrl::OnGetItemAttr(long item) const {
  if(onGetItemAttr) {
    INVOKE_CALLBACK_INIT(port, onGetItemAttr, "wxListCtrl");
    rt.addInt(item);
    INVOKE_CALLBACK_END(port, 1);
    char * bp = ((WxeApp *) wxTheApp)->cb_buff;
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    if(bp) {
      wxListItemAttr * result = (wxListItemAttr *)((WxeApp *) wxTheApp)->getPtr(bp, memenv);
      driver_free(((WxeApp *) wxTheApp)->cb_buff);
      ((WxeApp *) wxTheApp)->cb_buff = NULL;
      return result;
    }
  }
  return NULL;
}

int EwxListCtrl::OnGetItemImage(long item) const {
  return OnGetItemColumnImage(item, 0);
}

int EwxListCtrl::OnGetItemColumnImage(long item, long col) const {
  if(onGetItemColumnImage) {
    INVOKE_CALLBACK_INIT(port, onGetItemColumnImage, "wxListCtrl");
    rt.addInt(item);
    rt.addInt(col);
    INVOKE_CALLBACK_END(port, 2);
    if(((WxeApp *) wxTheApp)->cb_buff) {
      int res = * (int*) ((WxeApp *) wxTheApp)->cb_buff;
      driver_free(((WxeApp *) wxTheApp)->cb_buff);
      ((WxeApp *) wxTheApp)->cb_buff = NULL;
      return res;
    }
  }
  return -1;
}

EwxListCtrl::~EwxListCtrl() {
  clear_cb(port, onGetItemText);
  clear_cb(port, onGetItemAttr);
  clear_cb(port, onGetItemColumnImage);
  ((WxeApp *)wxTheApp)->clearPtr(this);
}
// tools

void clear_cb(ErlDrvTermData port, int callback)
{
  if(callback > 0) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);
    rt.addAtom("wx_delete_cb");
    rt.addInt(callback);
    rt.addTupleCount(2);
    rt.send();
  }
}
