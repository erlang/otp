/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

/* *****************************************************************/
/* Special Class impls */


/* Printing special */

wxEPrintout::~wxEPrintout() {
  clear_cb(onPrintPage);
  clear_cb(onPreparePrinting);
  clear_cb(onBeginPrinting); 
  clear_cb(onEndPrinting);
  clear_cb(onBeginDocument);
  clear_cb(onEndDocument);
  clear_cb(hasPage); 
  clear_cb(getPageInfo);

  ((WxeApp *)wxTheApp)->clearPtr(this);
}

bool wxEPrintout::OnBeginDocument(int startPage, int endPage)
{
  if(onBeginDocument) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    char * bp = ((WxeApp *) wxTheApp)->cb_buff;
    
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);
    rt.addInt(onBeginDocument);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.addInt(startPage);
    rt.addInt(endPage);
    rt.endList(3);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    handle_callback_batch(port);
    return *(int*) bp; 
  } else {
    return wxPrintout::OnBeginDocument(startPage,endPage);
  } 
}

void wxEPrintout::OnEndDocument() 
{
  if(onEndDocument) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);
    rt.addInt(onEndDocument);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.endList(1);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    handle_callback_batch(port);
  } else {
    wxPrintout::OnEndDocument();
  } 
}

void wxEPrintout::OnBeginPrinting() 
{

  if(onBeginPrinting) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);    
    rt.addInt(onBeginPrinting);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.endList(1);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    handle_callback_batch(port);
  } else {
    wxPrintout::OnBeginPrinting();
  } 
}

void wxEPrintout::OnEndPrinting() 
{

  if(onEndPrinting) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);    
    rt.addInt(onEndPrinting);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.endList(1);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    handle_callback_batch(port);
  } else {
    wxPrintout::OnEndPrinting();
  }
}

void wxEPrintout::OnPreparePrinting()
{

  if(onPreparePrinting) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);    
    rt.addInt(onPreparePrinting);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.endList(1);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    handle_callback_batch(port);
  } else {
    wxPrintout::OnPreparePrinting();
  } 
}

bool wxEPrintout::HasPage(int page) 
{

  if(hasPage) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);    
    rt.addInt(hasPage);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.addInt(page);
    rt.endList(2);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    char * bp = ((WxeApp *) wxTheApp)->cb_buff;
    handle_callback_batch(port);    
    return *(int*) bp;
  } else {
    return wxPrintout::HasPage(page);
  } 
}

bool wxEPrintout::OnPrintPage(int page)
{
  wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
  wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);    
  rt.addInt(onPrintPage);
  rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
  rt.addInt(page);
  rt.endList(2);
  rt.addAtom("_wx_invoke_cb_");
  rt.addTupleCount(3);
  rt.send();
  handle_callback_batch(port);
  //fprintf(stderr,"%d ", __LINE__);handle_callback_batch(port);  fprintf(stderr,"%d\r\n", __LINE__);
  char * bp = ((WxeApp *) wxTheApp)->cb_buff;
  return *(int*) bp;
}
 
void wxEPrintout::GetPageInfo(int *minPage, int *maxPage, int *pageFrom, int *pageTo)
{
  if(getPageInfo) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);        
    rt.addInt(getPageInfo);
    rt.addRef(((WxeApp *) wxTheApp)->getRef((void *)this, memenv), "wxPrintout");
    rt.endList(1);
    rt.addAtom("_wx_invoke_cb_");
    rt.addTupleCount(3);
    rt.send();
    handle_callback_batch(port);
    //fprintf(stderr,"%d ", __LINE__);handle_callback_batch(port);  fprintf(stderr,"%d\r\n", __LINE__);    

    char * bp = ((WxeApp *) wxTheApp)->cb_buff;
    *minPage  = *(int *) bp; bp += 4;
    *maxPage  = *(int *) bp; bp += 4;
    *pageFrom = *(int *) bp; bp += 4;
    *pageTo   = *(int *) bp; bp += 4;
  } else {
    wxPrintout::GetPageInfo(minPage, maxPage, pageFrom, pageTo);
  }
}

void wxEPrintout::clear_cb(int callback)
{
  if(callback > 0) {
    wxeMemEnv * memenv =  ((WxeApp *) wxTheApp)->getMemEnv(port);
    wxeReturn rt = wxeReturn(WXE_DRV_PORT, memenv->owner, false);
    // NOTE: Remove this later when changing from funs to gen_server
    rt.addAtom("wx_delete_cb");
    rt.addInt(callback);
    rt.addTupleCount(2);
    rt.send();
  }
}

