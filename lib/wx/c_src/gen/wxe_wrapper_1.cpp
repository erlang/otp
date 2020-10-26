/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2020. All Rights Reserved.
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

/***** This file is generated do not edit ****/

#include <wx/wx.h>
#include "../wxe_impl.h"
#include "../wxe_events.h"
#include "../wxe_return.h"
#include "../wxe_gl.h"
#include "wxe_macros.h"
#include "wxe_derived_dest.h"


// ::destroy
void wxe_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
   ErlNifEnv *env = Ecmd.env;
   ERL_NIF_TERM * argv = Ecmd.args;
   void * This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
   wxeRefData *refd = app->getRefData(This);
   if(This && refd) {
       if(app->recurse_level > 1 && refd->type != 8) {
         Ecmd.op = 50;
         app->delayed_delete->Append(&Ecmd);
       } else {
         app->delete_object(This, refd);
         ((WxeApp *) wxTheApp)->clearPtr(This);}
  }
}

// wxe_util::registerPid()
void wxe_registerPid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
   ErlNifEnv *env = Ecmd.env;
   ERL_NIF_TERM * argv = Ecmd.args;
   int index;
   if(!enif_get_int(env, argv[0], &index)) Badarg("Ref");
   if(app->registerPid(index, Ecmd.caller, memenv)) {
      wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
      rt.send(WXE_ATOM_ok);
   } else Badarg("Ref");}


void wxEvtHandler_Connect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int winid;
  int lastid;
  int skip;
  wxeErlTerm * userData;
  int fun_cb;
  wxEvtHandler *This = (wxEvtHandler *) memenv->getPtr(env, argv[0], "This");
  if(!enif_get_int(env, argv[1], &winid)) Badarg("Winid");
  if(!enif_get_int(env, argv[2], &lastid)) Badarg("LastId");
  skip = enif_is_identical(argv[3], WXE_ATOM_true);
  userData = new wxeErlTerm(argv[4]);
  if(!enif_get_int(env, argv[5], &fun_cb)) Badarg("FunId");
  if(!enif_is_atom(env, argv[6])) Badarg("EvType");
  int eventType = wxeEventTypeFromAtom(argv[6]);
  if(!enif_is_atom(env, argv[7])) Badarg("ClassName");

  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  if(eventType > 0 ) {
    wxeEvtListener * Evt_cb = new wxeEvtListener(Ecmd.caller,app->getRef(This, memenv),
                                                 argv[7], fun_cb, skip, userData, memenv->me_ref);
    This->Connect(winid, lastid, eventType,
	          (wxObjectEventFunction)(wxEventFunction) &wxeEvtListener::forward,
	          Evt_cb, Evt_cb);
    rt.send(enif_make_tuple2(rt.env, WXE_ATOM_ok,
			     rt.make_ref(app->getRef((void *)Evt_cb,memenv),
					 "wxeEvtListener")));
  } else {
    rt.send(enif_make_tuple2(rt.env, WXE_ATOM_badarg, rt.make_atom("event_type")));
  }
}

void wxEvtHandler_Disconnect_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int winid;
  int lastid;

  wxeEvtListener *Listener = (wxeEvtListener *) memenv->getPtr(env, argv[0], "Listener");
  wxEvtHandler *This = (wxEvtHandler *) memenv->getPtr(env, argv[1],"This");
  if(!enif_get_int(env, argv[2], &winid)) Badarg("Winid");
  if(!enif_get_int(env, argv[3], &lastid)) Badarg("LastId");
  if(!enif_is_atom(env, argv[4])) Badarg("EvType");
  int eventType = wxeEventTypeFromAtom(argv[4]);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);

  if(eventType > 0) {
    if(app->recurse_level > 1) {
      Ecmd.op = 101;
      app->delayed_delete->Append(&Ecmd);
    } else {
      bool Result = This->Disconnect(winid,lastid,eventType,
				     (wxObjectEventFunction)(wxEventFunction)
				     &wxeEvtListener::forward,
				     NULL, Listener);
      rt.send(rt.make_bool(Result));
    }
  } else {
    rt.send(enif_make_tuple2(rt.env, WXE_ATOM_badarg, rt.make_atom("event_type")));
  }

}

// wxWindow::wxWindow
void wxWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * Result = new EwxWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::wxWindow
void wxWindow_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxWindow * Result = new EwxWindow(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::Create
void wxWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::CacheBestSize
void wxWindow_CacheBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->CacheBestSize(size);

}

// wxWindow::CaptureMouse
void wxWindow_CaptureMouse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CaptureMouse();

}

// wxWindow::Centre
void wxWindow_Centre(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int dir=wxBOTH;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  if(!enif_get_int(env, tpl[1], &dir)) Badarg("dir"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Centre(dir);

}

// wxWindow::CentreOnParent
void wxWindow_CentreOnParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int dir=wxBOTH;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  if(!enif_get_int(env, tpl[1], &dir)) Badarg("dir"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->CentreOnParent(dir);

}

// wxWindow::ClearBackground
void wxWindow_ClearBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearBackground();

}

// wxWindow::ClientToScreen
void wxWindow_ClientToScreen_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->ClientToScreen(&x,&y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(x),
  rt.make_int(y));
  rt.send(msg);

}

// wxWindow::ClientToScreen
void wxWindow_ClientToScreen_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->ClientToScreen(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::Close
void wxWindow_Close(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool force=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "force"))) {
  force = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Close(force);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::ConvertDialogToPixels
void wxWindow_ConvertDialogToPixels(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->ConvertDialogToPixels(sz);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::ConvertPixelsToDialog
void wxWindow_ConvertPixelsToDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->ConvertPixelsToDialog(sz);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::Destroy
void wxWindow_Destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Destroy();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::DestroyChildren
void wxWindow_DestroyChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DestroyChildren();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Disable
void wxWindow_Disable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Disable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::DragAcceptFiles
void wxWindow_DragAcceptFiles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  bool accept;
  accept = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->DragAcceptFiles(accept);

}

// wxWindow::Enable
void wxWindow_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Enable(enable);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::FindFocus
void wxWindow_FindFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * Result = (wxWindow*)wxWindow::FindFocus();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::FindWindow
void wxWindow_FindWindow_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  long id;
  if(!enif_get_long(env, argv[1], &id)) Badarg("id");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->FindWindow(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::FindWindow
void wxWindow_FindWindow_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->FindWindow(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::FindWindowById
void wxWindow_FindWindowById(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxWindow * parent=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  long id;
  if(!enif_get_long(env, argv[0], &id)) Badarg("id");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "parent"))) {
  parent = (wxWindow *) memenv->getPtr(env, tpl[1], "parent");
    } else        Badarg("Options");
  };
  wxWindow * Result = (wxWindow*)wxWindow::FindWindowById(id,parent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::FindWindowByName
void wxWindow_FindWindowByName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxWindow * parent=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "parent"))) {
  parent = (wxWindow *) memenv->getPtr(env, tpl[1], "parent");
    } else        Badarg("Options");
  };
  wxWindow * Result = (wxWindow*)wxWindow::FindWindowByName(name,parent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::FindWindowByLabel
void wxWindow_FindWindowByLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxWindow * parent=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[0], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "parent"))) {
  parent = (wxWindow *) memenv->getPtr(env, tpl[1], "parent");
    } else        Badarg("Options");
  };
  wxWindow * Result = (wxWindow*)wxWindow::FindWindowByLabel(label,parent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::Fit
void wxWindow_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Fit();

}

// wxWindow::FitInside
void wxWindow_FitInside(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->FitInside();

}

// wxWindow::Freeze
void wxWindow_Freeze(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Freeze();

}

// wxWindow::GetAcceleratorTable
void wxWindow_GetAcceleratorTable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAcceleratorTable * Result = (wxAcceleratorTable*)This->GetAcceleratorTable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAcceleratorTable"));

}

// wxWindow::GetBackgroundColour
void wxWindow_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetBackgroundStyle
void wxWindow_GetBackgroundStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBackgroundStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetBestSize
void wxWindow_GetBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetBestSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetCaret
void wxWindow_GetCaret(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCaret * Result = (wxCaret*)This->GetCaret();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCaret"));

}

// wxWindow::GetCapture
void wxWindow_GetCapture(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * Result = (wxWindow*)wxWindow::GetCapture();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::GetCharHeight
void wxWindow_GetCharHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCharHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetCharWidth
void wxWindow_GetCharWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCharWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetChildren
void wxWindow_GetChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxWindowList Result = This->GetChildren();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_list_objs(Result, app, "wxWindow"));

}

// wxWindow::GetClientSize
void wxWindow_GetClientSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetClientSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetContainingSizer
void wxWindow_GetContainingSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizer * Result = (wxSizer*)This->GetContainingSizer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizer"));

}

// wxWindow::GetCursor
void wxWindow_GetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxCursor * Result = &This->GetCursor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCursor"));

}

// wxWindow::GetDropTarget
void wxWindow_GetDropTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDropTarget * Result = (wxDropTarget*)This->GetDropTarget();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDropTarget"));

}

#if wxCHECK_VERSION(3,1,4)
// wxWindow::GetDPIScaleFactor
void wxWindow_GetDPIScaleFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  double Result = This->GetDPIScaleFactor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_double(Result));

}

#endif
// wxWindow::GetExtraStyle
void wxWindow_GetExtraStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetExtraStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetFont
void wxWindow_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxWindow::GetForegroundColour
void wxWindow_GetForegroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetForegroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetGrandParent
void wxWindow_GetGrandParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGrandParent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::GetHandle
void wxWindow_GetHandle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxUIntPtr * Result = (wxUIntPtr*)This->GetHandle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetHelpText
void wxWindow_GetHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetHelpText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetId
void wxWindow_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindowID Result = This->GetId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetLabel
void wxWindow_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetLabel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetMaxSize
void wxWindow_GetMaxSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetMaxSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetMinSize
void wxWindow_GetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetMinSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetName
void wxWindow_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetParent
void wxWindow_GetParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetParent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxWindow::GetPosition
void wxWindow_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetRect
void wxWindow_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetScreenPosition
void wxWindow_GetScreenPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetScreenPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetScreenRect
void wxWindow_GetScreenRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetScreenRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetScrollPos
void wxWindow_GetScrollPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int orientation;
  if(!enif_get_int(env, argv[1], &orientation)) Badarg("orientation"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollPos(orientation);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetScrollRange
void wxWindow_GetScrollRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int orientation;
  if(!enif_get_int(env, argv[1], &orientation)) Badarg("orientation"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollRange(orientation);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetScrollThumb
void wxWindow_GetScrollThumb(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int orientation;
  if(!enif_get_int(env, argv[1], &orientation)) Badarg("orientation"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollThumb(orientation);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetSize
void wxWindow_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetSizer
void wxWindow_GetSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizer * Result = (wxSizer*)This->GetSizer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizer"));

}

// wxWindow::GetTextExtent
void wxWindow_GetTextExtent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int w;
  int h;
  int descent;
  int externalLeading;
  const wxFont * theFont=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "theFont"))) {
  theFont = (wxFont *) memenv->getPtr(env, tpl[1], "theFont");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->GetTextExtent(string,&w,&h,&descent,&externalLeading,theFont);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_int(w),
  rt.make_int(h),
  rt.make_int(descent),
  rt.make_int(externalLeading));
  rt.send(msg);

}

// wxWindow::GetThemeEnabled
void wxWindow_GetThemeEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetThemeEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::GetToolTip
void wxWindow_GetToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxToolTip * Result = (wxToolTip*)This->GetToolTip();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolTip"));

}

// wxWindow::GetUpdateRegion
void wxWindow_GetUpdateRegion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxRegion * Result = &This->GetUpdateRegion();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRegion"));

}

// wxWindow::GetVirtualSize
void wxWindow_GetVirtualSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetVirtualSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::GetWindowStyleFlag
void wxWindow_GetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetWindowStyleFlag();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::GetWindowVariant
void wxWindow_GetWindowVariant(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWindowVariant();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxWindow::HasCapture
void wxWindow_HasCapture(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasCapture();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::HasScrollbar
void wxWindow_HasScrollbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int orient;
  if(!enif_get_int(env, argv[1], &orient)) Badarg("orient"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasScrollbar(orient);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::HasTransparentBackground
void wxWindow_HasTransparentBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTransparentBackground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Hide
void wxWindow_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Hide();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::InheritAttributes
void wxWindow_InheritAttributes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InheritAttributes();

}

// wxWindow::InitDialog
void wxWindow_InitDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InitDialog();

}

// wxWindow::InvalidateBestSize
void wxWindow_InvalidateBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InvalidateBestSize();

}

// wxWindow::IsFrozen
void wxWindow_IsFrozen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFrozen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsEnabled
void wxWindow_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsExposed
void wxWindow_IsExposed_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsExposed(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsExposed
void wxWindow_IsExposed_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsExposed(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsExposed
void wxWindow_IsExposed_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  int w;
  if(!enif_get_int(env, argv[3], &w)) Badarg("w"); // int
  int h;
  if(!enif_get_int(env, argv[4], &h)) Badarg("h"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsExposed(x,y,w,h);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsExposed
void wxWindow_IsExposed_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsExposed(rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsRetained
void wxWindow_IsRetained(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsRetained();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsShown
void wxWindow_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsShown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsTopLevel
void wxWindow_IsTopLevel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsTopLevel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsShownOnScreen
void wxWindow_IsShownOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsShownOnScreen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Layout
void wxWindow_Layout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Layout();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::LineDown
void wxWindow_LineDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->LineDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::LineUp
void wxWindow_LineUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->LineUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Lower
void wxWindow_Lower(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Lower();

}

// wxWindow::Move
void wxWindow_Move_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxSIZE_USE_EXISTING;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Move(x,y,flags);

}

// wxWindow::Move
void wxWindow_Move_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxSIZE_USE_EXISTING;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Move(pt,flags);

}

// wxWindow::MoveAfterInTabOrder
void wxWindow_MoveAfterInTabOrder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *win;
  win = (wxWindow *) memenv->getPtr(env, argv[1], "win");
  if(!This) throw wxe_badarg("This");
  This->MoveAfterInTabOrder(win);

}

// wxWindow::MoveBeforeInTabOrder
void wxWindow_MoveBeforeInTabOrder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *win;
  win = (wxWindow *) memenv->getPtr(env, argv[1], "win");
  if(!This) throw wxe_badarg("This");
  This->MoveBeforeInTabOrder(win);

}

// wxWindow::Navigate
void wxWindow_Navigate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxNavigationKeyEvent::IsForward;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Navigate(flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::PageDown
void wxWindow_PageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->PageDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::PageUp
void wxWindow_PageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->PageUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::PopupMenu
void wxWindow_PopupMenu_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[1], "menu");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->PopupMenu(menu,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::PopupMenu
void wxWindow_PopupMenu_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[1], "menu");
  int x;
  if(!enif_get_int(env, argv[2], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[3], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->PopupMenu(menu,x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Raise
void wxWindow_Raise(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Raise();

}

// wxWindow::Refresh
void wxWindow_Refresh(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool eraseBackground=true;
  const wxRect *rect=NULL; wxRect rectTmp;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "eraseBackground"))) {
  eraseBackground = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "rect"))) {
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, tpl[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  rectTmp = wxRect(rectX,rectY,rectW,rectH); rect = & rectTmp;
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Refresh(eraseBackground,rect);

}

// wxWindow::RefreshRect
void wxWindow_RefreshRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool eraseBackground=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "eraseBackground"))) {
  eraseBackground = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->RefreshRect(rect,eraseBackground);

}

// wxWindow::ReleaseMouse
void wxWindow_ReleaseMouse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ReleaseMouse();

}

// wxWindow::RemoveChild
void wxWindow_RemoveChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *child;
  child = (wxWindow *) memenv->getPtr(env, argv[1], "child");
  if(!This) throw wxe_badarg("This");
  This->RemoveChild(child);

}

// wxWindow::Reparent
void wxWindow_Reparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *newParent;
  newParent = (wxWindow *) memenv->getPtr(env, argv[1], "newParent");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Reparent(newParent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::ScreenToClient
void wxWindow_ScreenToClient_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int x;
  int y;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ScreenToClient(&x,&y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(x),
  rt.make_int(y));
  rt.send(msg);

}

// wxWindow::ScreenToClient
void wxWindow_ScreenToClient_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->ScreenToClient(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxWindow::ScrollLines
void wxWindow_ScrollLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int lines;
  if(!enif_get_int(env, argv[1], &lines)) Badarg("lines"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->ScrollLines(lines);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::ScrollPages
void wxWindow_ScrollPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int pages;
  if(!enif_get_int(env, argv[1], &pages)) Badarg("pages"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->ScrollPages(pages);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::ScrollWindow
void wxWindow_ScrollWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxRect *rect=NULL; wxRect rectTmp;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int dx;
  if(!enif_get_int(env, argv[1], &dx)) Badarg("dx"); // int
  int dy;
  if(!enif_get_int(env, argv[2], &dy)) Badarg("dy"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "rect"))) {
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, tpl[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  rectTmp = wxRect(rectX,rectY,rectW,rectH); rect = & rectTmp;
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->ScrollWindow(dx,dy,rect);

}

// wxWindow::SetAcceleratorTable
void wxWindow_SetAcceleratorTable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxAcceleratorTable *accel;
  accel = (wxAcceleratorTable *) memenv->getPtr(env, argv[1], "accel");
  if(!This) throw wxe_badarg("This");
  This->SetAcceleratorTable(*accel);

}

// wxWindow::SetAutoLayout
void wxWindow_SetAutoLayout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  bool autoLayout;
  autoLayout = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetAutoLayout(autoLayout);

}

// wxWindow::SetBackgroundColour
void wxWindow_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetBackgroundColour(colour);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::SetBackgroundStyle
void wxWindow_SetBackgroundStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxBackgroundStyle style;
  if(!enif_get_int(env, argv[1], (int *) &style)) Badarg("style"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetBackgroundStyle(style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::SetCaret
void wxWindow_SetCaret(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxCaret *caret;
  caret = (wxCaret *) memenv->getPtr(env, argv[1], "caret");
  if(!This) throw wxe_badarg("This");
  This->SetCaret(caret);

}

// wxWindow::SetClientSize
void wxWindow_SetClientSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetClientSize(width,height);

}

// wxWindow::SetClientSize
void wxWindow_SetClientSize_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetClientSize(size);

}

// wxWindow::SetClientSize
void wxWindow_SetClientSize_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->SetClientSize(rect);

}

// wxWindow::SetContainingSizer
void wxWindow_SetContainingSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  if(!This) throw wxe_badarg("This");
  This->SetContainingSizer(sizer);

}

// wxWindow::SetCursor
void wxWindow_SetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxCursor *cursor;
  cursor = (wxCursor *) memenv->getPtr(env, argv[1], "cursor");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetCursor(*cursor);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::SetMaxSize
void wxWindow_SetMaxSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetMaxSize(size);

}

// wxWindow::SetMinSize
void wxWindow_SetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetMinSize(size);

}

// wxWindow::SetOwnBackgroundColour
void wxWindow_SetOwnBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetOwnBackgroundColour(colour);

}

// wxWindow::SetOwnFont
void wxWindow_SetOwnFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetOwnFont(*font);

}

// wxWindow::SetOwnForegroundColour
void wxWindow_SetOwnForegroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetOwnForegroundColour(colour);

}

// wxWindow::SetDropTarget
void wxWindow_SetDropTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxDropTarget *target;
  target = (wxDropTarget *) memenv->getPtr(env, argv[1], "target");
  if(!This) throw wxe_badarg("This");
  This->SetDropTarget(target);

}

// wxWindow::SetExtraStyle
void wxWindow_SetExtraStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  long exStyle;
  if(!enif_get_long(env, argv[1], &exStyle)) Badarg("exStyle");
  if(!This) throw wxe_badarg("This");
  This->SetExtraStyle(exStyle);

}

// wxWindow::SetFocus
void wxWindow_SetFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetFocus();

}

// wxWindow::SetFocusFromKbd
void wxWindow_SetFocusFromKbd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetFocusFromKbd();

}

// wxWindow::SetFont
void wxWindow_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetFont(*font);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::SetForegroundColour
void wxWindow_SetForegroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetForegroundColour(colour);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::SetHelpText
void wxWindow_SetHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary helpText_bin;
  wxString helpText;
  if(!enif_inspect_binary(env, argv[1], &helpText_bin)) Badarg("helpText");
  helpText = wxString(helpText_bin.data, wxConvUTF8, helpText_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetHelpText(helpText);

}

// wxWindow::SetId
void wxWindow_SetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int winid;
  if(!enif_get_int(env, argv[1], &winid)) Badarg("winid"); // wxWindowID
  if(!This) throw wxe_badarg("This");
  This->SetId(winid);

}

// wxWindow::SetLabel
void wxWindow_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetLabel(label);

}

// wxWindow::SetName
void wxWindow_SetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetName(name);

}

// wxWindow::SetPalette
void wxWindow_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxPalette *pal;
  pal = (wxPalette *) memenv->getPtr(env, argv[1], "pal");
  if(!This) throw wxe_badarg("This");
  This->SetPalette(*pal);

}

// wxWindow::SetScrollbar
void wxWindow_SetScrollbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool refresh=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int orientation;
  if(!enif_get_int(env, argv[1], &orientation)) Badarg("orientation"); // int
  int position;
  if(!enif_get_int(env, argv[2], &position)) Badarg("position"); // int
  int thumbSize;
  if(!enif_get_int(env, argv[3], &thumbSize)) Badarg("thumbSize"); // int
  int range;
  if(!enif_get_int(env, argv[4], &range)) Badarg("range"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "refresh"))) {
  refresh = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetScrollbar(orientation,position,thumbSize,range,refresh);

}

// wxWindow::SetScrollPos
void wxWindow_SetScrollPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool refresh=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int orientation;
  if(!enif_get_int(env, argv[1], &orientation)) Badarg("orientation"); // int
  int pos;
  if(!enif_get_int(env, argv[2], &pos)) Badarg("pos"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "refresh"))) {
  refresh = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetScrollPos(orientation,pos,refresh);

}

// wxWindow::SetSize
void wxWindow_SetSize_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int sizeFlags=wxSIZE_AUTO;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sizeFlags"))) {
  if(!enif_get_int(env, tpl[1], &sizeFlags)) Badarg("sizeFlags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSize(x,y,width,height,sizeFlags);

}

// wxWindow::SetSize
void wxWindow_SetSize_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int sizeFlags=wxSIZE_AUTO;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sizeFlags"))) {
  if(!enif_get_int(env, tpl[1], &sizeFlags)) Badarg("sizeFlags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSize(rect,sizeFlags);

}

// wxWindow::SetSize
void wxWindow_SetSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetSize(size);

}

// wxWindow::SetSize
void wxWindow_SetSize_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSize(width,height);

}

// wxWindow::SetSizeHints
void wxWindow_SetSizeHints_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSize maxSize= wxDefaultSize;
  wxSize incSize= wxDefaultSize;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *minSize_t;
  int minSize_sz;
  if(!enif_get_tuple(env, argv[1], &minSize_sz, &minSize_t)) Badarg("minSize");
  int minSizeW;
  if(!enif_get_int(env, minSize_t[0], &minSizeW)) Badarg("minSize");
  int minSizeH;
  if(!enif_get_int(env, minSize_t[1], &minSizeH)) Badarg("minSize");
  wxSize minSize = wxSize(minSizeW,minSizeH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "maxSize"))) {
  const ERL_NIF_TERM *maxSize_t;
  int maxSize_sz;
  if(!enif_get_tuple(env, tpl[1], &maxSize_sz, &maxSize_t)) Badarg("maxSize");
  int maxSizeW;
  if(!enif_get_int(env, maxSize_t[0], &maxSizeW)) Badarg("maxSize");
  int maxSizeH;
  if(!enif_get_int(env, maxSize_t[1], &maxSizeH)) Badarg("maxSize");
  maxSize = wxSize(maxSizeW,maxSizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "incSize"))) {
  const ERL_NIF_TERM *incSize_t;
  int incSize_sz;
  if(!enif_get_tuple(env, tpl[1], &incSize_sz, &incSize_t)) Badarg("incSize");
  int incSizeW;
  if(!enif_get_int(env, incSize_t[0], &incSizeW)) Badarg("incSize");
  int incSizeH;
  if(!enif_get_int(env, incSize_t[1], &incSizeH)) Badarg("incSize");
  incSize = wxSize(incSizeW,incSizeH);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSizeHints(minSize,maxSize,incSize);

}

// wxWindow::SetSizeHints
void wxWindow_SetSizeHints_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int maxW=-1;
  int maxH=-1;
  int incW=-1;
  int incH=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int minW;
  if(!enif_get_int(env, argv[1], &minW)) Badarg("minW"); // int
  int minH;
  if(!enif_get_int(env, argv[2], &minH)) Badarg("minH"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "maxW"))) {
  if(!enif_get_int(env, tpl[1], &maxW)) Badarg("maxW"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "maxH"))) {
  if(!enif_get_int(env, tpl[1], &maxH)) Badarg("maxH"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "incW"))) {
  if(!enif_get_int(env, tpl[1], &incW)) Badarg("incW"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "incH"))) {
  if(!enif_get_int(env, tpl[1], &incH)) Badarg("incH"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSizeHints(minW,minH,maxW,maxH,incW,incH);

}

// wxWindow::SetSizer
void wxWindow_SetSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool deleteOld=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "deleteOld"))) {
  deleteOld = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSizer(sizer,deleteOld);

}

// wxWindow::SetSizerAndFit
void wxWindow_SetSizerAndFit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool deleteOld=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "deleteOld"))) {
  deleteOld = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSizerAndFit(sizer,deleteOld);

}

// wxWindow::SetThemeEnabled
void wxWindow_SetThemeEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  bool enable;
  enable = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetThemeEnabled(enable);

}

// wxWindow::SetToolTip
void wxWindow_SetToolTip_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary tipString_bin;
  wxString tipString;
  if(!enif_inspect_binary(env, argv[1], &tipString_bin)) Badarg("tipString");
  tipString = wxString(tipString_bin.data, wxConvUTF8, tipString_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetToolTip(tipString);

}

// wxWindow::SetToolTip
void wxWindow_SetToolTip_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxToolTip *tip;
  tip = (wxToolTip *) memenv->getPtr(env, argv[1], "tip");
  if(!This) throw wxe_badarg("This");
  This->SetToolTip(tip);

}

// wxWindow::SetVirtualSize
void wxWindow_SetVirtualSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetVirtualSize(width,height);

}

// wxWindow::SetVirtualSize
void wxWindow_SetVirtualSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetVirtualSize(size);

}

// wxWindow::SetWindowStyle
void wxWindow_SetWindowStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  long style;
  if(!enif_get_long(env, argv[1], &style)) Badarg("style");
  if(!This) throw wxe_badarg("This");
  This->SetWindowStyle(style);

}

// wxWindow::SetWindowStyleFlag
void wxWindow_SetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  long style;
  if(!enif_get_long(env, argv[1], &style)) Badarg("style");
  if(!This) throw wxe_badarg("This");
  This->SetWindowStyleFlag(style);

}

// wxWindow::SetWindowVariant
void wxWindow_SetWindowVariant(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindowVariant variant;
  if(!enif_get_int(env, argv[1], (int *) &variant)) Badarg("variant"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetWindowVariant(variant);

}

// wxWindow::ShouldInheritColours
void wxWindow_ShouldInheritColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShouldInheritColours();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Show
void wxWindow_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "show"))) {
  show = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Show(show);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Thaw
void wxWindow_Thaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Thaw();

}

// wxWindow::TransferDataFromWindow
void wxWindow_TransferDataFromWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->TransferDataFromWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::TransferDataToWindow
void wxWindow_TransferDataToWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->TransferDataToWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::Update
void wxWindow_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Update();

}

// wxWindow::UpdateWindowUI
void wxWindow_UpdateWindowUI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags=wxUPDATE_UI_NONE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_long(env, tpl[1], &flags)) Badarg("flags");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->UpdateWindowUI(flags);

}

// wxWindow::Validate
void wxWindow_Validate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Validate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::WarpPointer
void wxWindow_WarpPointer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->WarpPointer(x,y);

}

// wxWindow::SetTransparent
void wxWindow_SetTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int alpha;
  if(!enif_get_int(env, argv[1], &alpha)) Badarg("alpha"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetTransparent(alpha);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::CanSetTransparent
void wxWindow_CanSetTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanSetTransparent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxWindow::IsDoubleBuffered
void wxWindow_IsDoubleBuffered(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsDoubleBuffered();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxCHECK_VERSION(3,1,0) || (!defined(__WXMAC__) && wxCHECK_VERSION(3,0,0))
// wxWindow::SetDoubleBuffered
void wxWindow_SetDoubleBuffered(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  bool on;
  on = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetDoubleBuffered(on);

}

#endif
// wxWindow::GetContentScaleFactor
void wxWindow_GetContentScaleFactor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  double Result = This->GetContentScaleFactor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_double(Result));

}

#if wxCHECK_VERSION(3,1,3)
// wxWindow::GetDPI
void wxWindow_GetDPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetDPI();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::FromDIP
void wxWindow_FromDIP_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->FromDIP(sz);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::FromDIP
void wxWindow_FromDIP_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int d;
  if(!enif_get_int(env, argv[1], &d)) Badarg("d"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->FromDIP(d);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::FromDIP
void wxWindow_FromDIP_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[0], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  wxWindow *w;
  w = (wxWindow *) memenv->getPtr(env, argv[1], "w");
  wxSize Result = wxWindow::FromDIP(sz,w);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::FromDIP
void wxWindow_FromDIP_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int d;
  if(!enif_get_int(env, argv[0], &d)) Badarg("d"); // int
  wxWindow *w;
  w = (wxWindow *) memenv->getPtr(env, argv[1], "w");
  int Result = wxWindow::FromDIP(d,w);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::ToDIP
void wxWindow_ToDIP_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->ToDIP(sz);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::ToDIP
void wxWindow_ToDIP_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *This;
  This = (wxWindow *) memenv->getPtr(env, argv[0], "This");
  int d;
  if(!enif_get_int(env, argv[1], &d)) Badarg("d"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->ToDIP(d);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::ToDIP
void wxWindow_ToDIP_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[0], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  wxWindow *w;
  w = (wxWindow *) memenv->getPtr(env, argv[1], "w");
  wxSize Result = wxWindow::ToDIP(sz,w);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
#if wxCHECK_VERSION(3,1,0)
// wxWindow::ToDIP
void wxWindow_ToDIP_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int d;
  if(!enif_get_int(env, argv[0], &d)) Badarg("d"); // int
  wxWindow *w;
  w = (wxWindow *) memenv->getPtr(env, argv[1], "w");
  int Result = wxWindow::ToDIP(d,w);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

#endif
// wxTopLevelWindow::GetIcon
void wxTopLevelWindow_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxIcon * Result = new wxIcon(This->GetIcon()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxTopLevelWindow::GetIcons
void wxTopLevelWindow_GetIcons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxIconBundle * Result = &This->GetIcons();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxTopLevelWindow::GetTitle
void wxTopLevelWindow_GetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetTitle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTopLevelWindow::IsActive
void wxTopLevelWindow_IsActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsActive();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTopLevelWindow::Iconize
void wxTopLevelWindow_Iconize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool iconize=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "iconize"))) {
  iconize = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Iconize(iconize);

}

// wxTopLevelWindow::IsFullScreen
void wxTopLevelWindow_IsFullScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFullScreen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTopLevelWindow::IsIconized
void wxTopLevelWindow_IsIconized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsIconized();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTopLevelWindow::IsMaximized
void wxTopLevelWindow_IsMaximized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsMaximized();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTopLevelWindow::Maximize
void wxTopLevelWindow_Maximize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool maximize=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "maximize"))) {
  maximize = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Maximize(maximize);

}

// wxTopLevelWindow::RequestUserAttention
void wxTopLevelWindow_RequestUserAttention(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxUSER_ATTENTION_INFO;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->RequestUserAttention(flags);

}

// wxTopLevelWindow::SetIcon
void wxTopLevelWindow_SetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  if(!This) throw wxe_badarg("This");
  This->SetIcon(*icon);

}

// wxTopLevelWindow::SetIcons
void wxTopLevelWindow_SetIcons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  wxIconBundle *icons;
  icons = (wxIconBundle *) memenv->getPtr(env, argv[1], "icons");
  if(!This) throw wxe_badarg("This");
  This->SetIcons(*icons);

}

// wxTopLevelWindow::CentreOnScreen
void wxTopLevelWindow_CentreOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int dir=wxBOTH;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  if(!enif_get_int(env, tpl[1], &dir)) Badarg("dir"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->CentreOnScreen(dir);

}

// wxTopLevelWindow::SetShape
void wxTopLevelWindow_SetShape(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM region_type;
  void * region = memenv->getPtr(env, argv[1], "region", &region_type);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(region_type, WXE_ATOM_wxRegion))
   Result =  This->SetShape(* static_cast<wxRegion*> (region));
  else if(enif_is_identical(region_type, WXE_ATOM_wxGraphicsPath))
   Result =  This->SetShape(* static_cast<wxGraphicsPath*> (region));
  else throw wxe_badarg("region");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTopLevelWindow::SetTitle
void wxTopLevelWindow_SetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[1], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetTitle(title);

}

// wxTopLevelWindow::ShowFullScreen
void wxTopLevelWindow_ShowFullScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=wxFULLSCREEN_ALL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTopLevelWindow *This;
  This = (wxTopLevelWindow *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShowFullScreen(show,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFrame::wxFrame
void wxFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFrame * Result = new EwxFrame();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxFrame::wxFrame
void wxFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[2], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxFrame * Result = new EwxFrame(parent,id,title,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxFrame::Create
void wxFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[3], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,title,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFrame::CreateStatusBar
void wxFrame_CreateStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=1;
  long style=wxSTB_DEFAULT_STYLE;
  wxWindowID id=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "number"))) {
  if(!enif_get_int(env, tpl[1], &number)) Badarg("number"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxStatusBar * Result = (wxStatusBar*)This->CreateStatusBar(number,style,id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStatusBar"));

}

// wxFrame::CreateToolBar
void wxFrame_CreateToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=wxTB_DEFAULT_STYLE;
  wxWindowID id=wxID_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBar * Result = (wxToolBar*)This->CreateToolBar(style,id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolBar"));

}

// wxFrame::GetClientAreaOrigin
void wxFrame_GetClientAreaOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetClientAreaOrigin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFrame::GetMenuBar
void wxFrame_GetMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenuBar * Result = (wxMenuBar*)This->GetMenuBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

// wxFrame::GetStatusBar
void wxFrame_GetStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxStatusBar * Result = (wxStatusBar*)This->GetStatusBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStatusBar"));

}

// wxFrame::GetStatusBarPane
void wxFrame_GetStatusBarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStatusBarPane();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFrame::GetToolBar
void wxFrame_GetToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxToolBar * Result = (wxToolBar*)This->GetToolBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolBar"));

}

// wxFrame::ProcessCommand
void wxFrame_ProcessCommand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->ProcessCommand(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFrame::SendSizeEvent
void wxFrame_SendSizeEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SendSizeEvent(flags);

}

// wxFrame::SetMenuBar
void wxFrame_SetMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxMenuBar *menuBar;
  menuBar = (wxMenuBar *) memenv->getPtr(env, argv[1], "menuBar");
  if(!This) throw wxe_badarg("This");
  This->SetMenuBar(menuBar);

}

// wxFrame::SetStatusBar
void wxFrame_SetStatusBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxStatusBar *statusBar;
  statusBar = (wxStatusBar *) memenv->getPtr(env, argv[1], "statusBar");
  if(!This) throw wxe_badarg("This");
  This->SetStatusBar(statusBar);

}

// wxFrame::SetStatusBarPane
void wxFrame_SetStatusBarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetStatusBarPane(n);

}

// wxFrame::SetStatusText
void wxFrame_SetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "number"))) {
  if(!enif_get_int(env, tpl[1], &number)) Badarg("number"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetStatusText(text,number);

}

// wxFrame::SetStatusWidths
void wxFrame_SetStatusWidths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  int widths_field_tmp;
  unsigned int widths_fieldLen;
  ERL_NIF_TERM widths_fieldHead, widths_fieldTail;
  if(!enif_get_list_length(env, argv[1], &widths_fieldLen)) Badarg("widths_field");
  std::vector <int> widths_field;
  widths_fieldTail = argv[1];
  while(!enif_is_empty_list(env, widths_fieldTail)) {
    if(!enif_get_list_cell(env, widths_fieldTail, &widths_fieldHead, &widths_fieldTail)) Badarg("widths_field");
    if(!enif_get_int(env, widths_fieldHead, &widths_field_tmp)) Badarg("widths_field");
    widths_field.push_back( (int) widths_field_tmp);
  };
  if(!This) throw wxe_badarg("This");
  This->SetStatusWidths(widths_fieldLen,widths_field.data());

}

// wxFrame::SetToolBar
void wxFrame_SetToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *This;
  This = (wxFrame *) memenv->getPtr(env, argv[0], "This");
  wxToolBar *toolBar;
  toolBar = (wxToolBar *) memenv->getPtr(env, argv[1], "toolBar");
  if(!This) throw wxe_badarg("This");
  This->SetToolBar(toolBar);

}

// wxMiniFrame::wxMiniFrame
void wxMiniFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMiniFrame * Result = new EwxMiniFrame();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMiniFrame"));

}

// wxMiniFrame::wxMiniFrame
void wxMiniFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCAPTION|wxRESIZE_BORDER;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[2], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxMiniFrame * Result = new EwxMiniFrame(parent,id,title,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMiniFrame"));

}

// wxMiniFrame::Create
void wxMiniFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCAPTION|wxRESIZE_BORDER;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMiniFrame *This;
  This = (wxMiniFrame *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[3], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,title,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplashScreen::wxSplashScreen
void wxSplashScreen_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxBORDER_SIMPLE|wxFRAME_NO_TASKBAR|wxSTAY_ON_TOP;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[0], "bitmap");
  long splashStyle;
  if(!enif_get_long(env, argv[1], &splashStyle)) Badarg("splashStyle");
  int milliseconds;
  if(!enif_get_int(env, argv[2], &milliseconds)) Badarg("milliseconds"); // int
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[3], "parent");
  int id;
  if(!enif_get_int(env, argv[4], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxSplashScreen * Result = new EwxSplashScreen(*bitmap,splashStyle,milliseconds,parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSplashScreen"));

}

// wxSplashScreen::GetSplashStyle
void wxSplashScreen_GetSplashStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplashScreen *This;
  This = (wxSplashScreen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetSplashStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplashScreen::GetTimeout
void wxSplashScreen_GetTimeout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplashScreen *This;
  This = (wxSplashScreen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetTimeout();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPanel::wxPanel
void wxPanel_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPanel * Result = new EwxPanel();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPanel"));

}

// wxPanel::wxPanel
void wxPanel_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID winid=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxTAB_TRAVERSAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "winid"))) {
  if(!enif_get_int(env, tpl[1], &winid)) Badarg("winid"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxPanel * Result = new EwxPanel(parent,winid,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPanel"));

}

// wxPanel::InitDialog
void wxPanel_InitDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPanel *This;
  This = (wxPanel *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InitDialog();

}

// wxPanel::SetFocusIgnoringChildren
void wxPanel_SetFocusIgnoringChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPanel *This;
  This = (wxPanel *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetFocusIgnoringChildren();

}

// wxScrolledWindow::wxScrolledWindow
void wxScrolledWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxScrolledWindow * Result = new EwxScrolledWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxScrolledWindow"));

}

// wxScrolledWindow::wxScrolledWindow
void wxScrolledWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID winid=-1;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxHSCROLL|wxVSCROLL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "winid"))) {
  if(!enif_get_int(env, tpl[1], &winid)) Badarg("winid"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxScrolledWindow * Result = new EwxScrolledWindow(parent,winid,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxScrolledWindow"));

}

// wxScrolledWindow::CalcScrolledPosition
void wxScrolledWindow_CalcScrolledPosition_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int xx;
  int yy;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->CalcScrolledPosition(x,y,&xx,&yy);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(xx),
  rt.make_int(yy));
  rt.send(msg);

}

// wxScrolledWindow::CalcScrolledPosition
void wxScrolledWindow_CalcScrolledPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->CalcScrolledPosition(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxScrolledWindow::CalcUnscrolledPosition
void wxScrolledWindow_CalcUnscrolledPosition_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int xx;
  int yy;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->CalcUnscrolledPosition(x,y,&xx,&yy);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(xx),
  rt.make_int(yy));
  rt.send(msg);

}

// wxScrolledWindow::CalcUnscrolledPosition
void wxScrolledWindow_CalcUnscrolledPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->CalcUnscrolledPosition(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxScrolledWindow::EnableScrolling
void wxScrolledWindow_EnableScrolling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  bool xScrolling;
  xScrolling = enif_is_identical(argv[1], WXE_ATOM_true);
  bool yScrolling;
  yScrolling = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableScrolling(xScrolling,yScrolling);

}

// wxScrolledWindow::GetScrollPixelsPerUnit
void wxScrolledWindow_GetScrollPixelsPerUnit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int xUnit;
  int yUnit;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetScrollPixelsPerUnit(&xUnit,&yUnit);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(xUnit),
  rt.make_int(yUnit));
  rt.send(msg);

}

// wxScrolledWindow::GetViewStart
void wxScrolledWindow_GetViewStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetViewStart();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxScrolledWindow::DoPrepareDC
void wxScrolledWindow_DoPrepareDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  if(!This) throw wxe_badarg("This");
  This->DoPrepareDC(*dc);

}

// wxScrolledWindow::PrepareDC
void wxScrolledWindow_PrepareDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  if(!This) throw wxe_badarg("This");
  This->PrepareDC(*dc);

}

// wxScrolledWindow::Scroll
void wxScrolledWindow_Scroll_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->Scroll(x,y);

}

// wxScrolledWindow::Scroll
void wxScrolledWindow_Scroll_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->Scroll(pt);

}

// wxScrolledWindow::SetScrollbars
void wxScrolledWindow_SetScrollbars(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int xPos=0;
  int yPos=0;
  bool noRefresh=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  int pixelsPerUnitX;
  if(!enif_get_int(env, argv[1], &pixelsPerUnitX)) Badarg("pixelsPerUnitX"); // int
  int pixelsPerUnitY;
  if(!enif_get_int(env, argv[2], &pixelsPerUnitY)) Badarg("pixelsPerUnitY"); // int
  int noUnitsX;
  if(!enif_get_int(env, argv[3], &noUnitsX)) Badarg("noUnitsX"); // int
  int noUnitsY;
  if(!enif_get_int(env, argv[4], &noUnitsY)) Badarg("noUnitsY"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "xPos"))) {
  if(!enif_get_int(env, tpl[1], &xPos)) Badarg("xPos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "yPos"))) {
  if(!enif_get_int(env, tpl[1], &yPos)) Badarg("yPos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "noRefresh"))) {
  noRefresh = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetScrollbars(pixelsPerUnitX,pixelsPerUnitY,noUnitsX,noUnitsY,xPos,yPos,noRefresh);

}

// wxScrolledWindow::SetScrollRate
void wxScrolledWindow_SetScrollRate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  int xstep;
  if(!enif_get_int(env, argv[1], &xstep)) Badarg("xstep"); // int
  int ystep;
  if(!enif_get_int(env, argv[2], &ystep)) Badarg("ystep"); // int
  if(!This) throw wxe_badarg("This");
  This->SetScrollRate(xstep,ystep);

}

// wxScrolledWindow::SetTargetWindow
void wxScrolledWindow_SetTargetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
  This->SetTargetWindow(window);

}

// wxSashWindow::wxSashWindow
void wxSashWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSashWindow * Result = new EwxSashWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSashWindow"));

}

// wxSashWindow::wxSashWindow
void wxSashWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCLIP_CHILDREN|wxSW_3D;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxSashWindow * Result = new EwxSashWindow(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSashWindow"));

}

// wxSashWindow::GetSashVisible
void wxSashWindow_GetSashVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  wxSashEdgePosition edge;
  if(!enif_get_int(env, argv[1], (int *) &edge)) Badarg("edge"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSashVisible(edge);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSashWindow::GetMaximumSizeX
void wxSashWindow_GetMaximumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaximumSizeX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashWindow::GetMaximumSizeY
void wxSashWindow_GetMaximumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaximumSizeY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashWindow::GetMinimumSizeX
void wxSashWindow_GetMinimumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMinimumSizeX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashWindow::GetMinimumSizeY
void wxSashWindow_GetMinimumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMinimumSizeY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashWindow::SetMaximumSizeX
void wxSashWindow_SetMaximumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMaximumSizeX(min);

}

// wxSashWindow::SetMaximumSizeY
void wxSashWindow_SetMaximumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMaximumSizeY(min);

}

// wxSashWindow::SetMinimumSizeX
void wxSashWindow_SetMinimumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMinimumSizeX(min);

}

// wxSashWindow::SetMinimumSizeY
void wxSashWindow_SetMinimumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMinimumSizeY(min);

}

// wxSashWindow::SetSashVisible
void wxSashWindow_SetSashVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  wxSashEdgePosition edge;
  if(!enif_get_int(env, argv[1], (int *) &edge)) Badarg("edge"); // enum
  bool visible;
  visible = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetSashVisible(edge,visible);

}

// wxSashLayoutWindow::wxSashLayoutWindow
void wxSashLayoutWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSashLayoutWindow * Result = new EwxSashLayoutWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSashLayoutWindow"));

}

// wxSashLayoutWindow::wxSashLayoutWindow
void wxSashLayoutWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCLIP_CHILDREN|wxSW_3D;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxSashLayoutWindow * Result = new EwxSashLayoutWindow(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSashLayoutWindow"));

}

// wxSashLayoutWindow::Create
void wxSashLayoutWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCLIP_CHILDREN|wxSW_3D;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSashLayoutWindow::GetAlignment
void wxSashLayoutWindow_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetAlignment();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashLayoutWindow::GetOrientation
void wxSashLayoutWindow_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOrientation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashLayoutWindow::SetAlignment
void wxSashLayoutWindow_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  wxLayoutAlignment alignment;
  if(!enif_get_int(env, argv[1], (int *) &alignment)) Badarg("alignment"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetAlignment(alignment);

}

// wxSashLayoutWindow::SetDefaultSize
void wxSashLayoutWindow_SetDefaultSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultSize(size);

}

// wxSashLayoutWindow::SetOrientation
void wxSashLayoutWindow_SetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  wxLayoutOrientation orientation;
  if(!enif_get_int(env, argv[1], (int *) &orientation)) Badarg("orientation"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetOrientation(orientation);

}

// wxGrid::wxGrid
void wxGrid_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGrid * Result = new EwxGrid();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGrid"));

}

// wxGrid::wxGrid
void wxGrid_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxWANTS_CHARS;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, tpl[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  pos = wxPoint(posX,posY);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxGrid * Result = new EwxGrid(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGrid"));

}

// wxGrid::AppendCols
void wxGrid_AppendCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int numCols=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "numCols"))) {
  if(!enif_get_int(env, tpl[1], &numCols)) Badarg("numCols"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AppendCols(numCols,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::AppendRows
void wxGrid_AppendRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int numRows=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "numRows"))) {
  if(!enif_get_int(env, tpl[1], &numRows)) Badarg("numRows"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AppendRows(numRows,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::AutoSize
void wxGrid_AutoSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->AutoSize();

}

// wxGrid::AutoSizeColumn
void wxGrid_AutoSizeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeColumn(col,setAsMin);

}

// wxGrid::AutoSizeColumns
void wxGrid_AutoSizeColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeColumns(setAsMin);

}

// wxGrid::AutoSizeRow
void wxGrid_AutoSizeRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeRow(row,setAsMin);

}

// wxGrid::AutoSizeRows
void wxGrid_AutoSizeRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool setAsMin=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "setAsMin"))) {
  setAsMin = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AutoSizeRows(setAsMin);

}

// wxGrid::BeginBatch
void wxGrid_BeginBatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->BeginBatch();

}

// wxGrid::BlockToDeviceRect
void wxGrid_BlockToDeviceRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *topLeft_t;
  int topLeft_sz;
  if(!enif_get_tuple(env, argv[1], &topLeft_sz, &topLeft_t)) Badarg("topLeft");
  int topLeftR;
  if(!enif_get_int(env, topLeft_t[0], &topLeftR)) Badarg("topLeft");
  int topLeftC;
  if(!enif_get_int(env, topLeft_t[1], &topLeftC)) Badarg("topLeft");
  wxGridCellCoords topLeft = wxGridCellCoords(topLeftR,topLeftC);
  const ERL_NIF_TERM *bottomRight_t;
  int bottomRight_sz;
  if(!enif_get_tuple(env, argv[2], &bottomRight_sz, &bottomRight_t)) Badarg("bottomRight");
  int bottomRightR;
  if(!enif_get_int(env, bottomRight_t[0], &bottomRightR)) Badarg("bottomRight");
  int bottomRightC;
  if(!enif_get_int(env, bottomRight_t[1], &bottomRightC)) Badarg("bottomRight");
  wxGridCellCoords bottomRight = wxGridCellCoords(bottomRightR,bottomRightC);
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->BlockToDeviceRect(topLeft,bottomRight);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::CanDragCell
void wxGrid_CanDragCell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragCell();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanDragColMove
void wxGrid_CanDragColMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragColMove();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxCHECK_VERSION(3,1,4)
// wxGrid::CanDragGridRowEdges
void wxGrid_CanDragGridRowEdges(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragGridRowEdges();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
// wxGrid::CanDragColSize
void wxGrid_CanDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragColSize(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanDragRowSize
void wxGrid_CanDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragRowSize(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanDragGridSize
void wxGrid_CanDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanDragGridSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CanEnableCellControl
void wxGrid_CanEnableCellControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanEnableCellControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::CellToRect
void wxGrid_CellToRect_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->CellToRect(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::CellToRect
void wxGrid_CellToRect_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->CellToRect(coords);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::ClearGrid
void wxGrid_ClearGrid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearGrid();

}

// wxGrid::ClearSelection
void wxGrid_ClearSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearSelection();

}

// wxGrid::CreateGrid
void wxGrid_CreateGrid(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxGrid::wxGridSelectionModes selmode=wxGrid::wxGridSelectCells;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int numRows;
  if(!enif_get_int(env, argv[1], &numRows)) Badarg("numRows"); // int
  int numCols;
  if(!enif_get_int(env, argv[2], &numCols)) Badarg("numCols"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "selmode"))) {
  if(!enif_get_int(env, tpl[1], (int *) &selmode)) Badarg("selmode"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->CreateGrid(numRows,numCols,selmode);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::DeleteCols
void wxGrid_DeleteCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numCols=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numCols"))) {
  if(!enif_get_int(env, tpl[1], &numCols)) Badarg("numCols"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteCols(pos,numCols,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::DeleteRows
void wxGrid_DeleteRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numRows=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numRows"))) {
  if(!enif_get_int(env, tpl[1], &numRows)) Badarg("numRows"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteRows(pos,numRows,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::DisableCellEditControl
void wxGrid_DisableCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableCellEditControl();

}

// wxGrid::DisableDragColSize
void wxGrid_DisableDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableDragColSize();

}

// wxGrid::DisableDragGridSize
void wxGrid_DisableDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableDragGridSize();

}

// wxGrid::DisableDragRowSize
void wxGrid_DisableDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DisableDragRowSize();

}

// wxGrid::EnableCellEditControl
void wxGrid_EnableCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableCellEditControl(enable);

}

// wxGrid::EnableDragColSize
void wxGrid_EnableDragColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableDragColSize(enable);

}

// wxGrid::EnableDragGridSize
void wxGrid_EnableDragGridSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableDragGridSize(enable);

}

// wxGrid::EnableDragRowSize
void wxGrid_EnableDragRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableDragRowSize(enable);

}

// wxGrid::EnableEditing
void wxGrid_EnableEditing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool edit;
  edit = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableEditing(edit);

}

// wxGrid::EnableGridLines
void wxGrid_EnableGridLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "enable"))) {
  enable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableGridLines(enable);

}

// wxGrid::EndBatch
void wxGrid_EndBatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EndBatch();

}

// wxGrid::Fit
void wxGrid_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Fit();

}

// wxGrid::ForceRefresh
void wxGrid_ForceRefresh(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ForceRefresh();

}

// wxGrid::GetBatchCount
void wxGrid_GetBatchCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBatchCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetCellAlignment
void wxGrid_GetCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->GetCellAlignment(row,col,&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetCellBackgroundColour
void wxGrid_GetCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCellBackgroundColour(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetCellEditor
void wxGrid_GetCellEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetCellEditor(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetCellFont
void wxGrid_GetCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetCellFont(row,col)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGrid::GetCellRenderer
void wxGrid_GetCellRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetCellRenderer(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetCellTextColour
void wxGrid_GetCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCellTextColour(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetCellValue
void wxGrid_GetCellValue_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCellValue(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetCellValue
void wxGrid_GetCellValue_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCellValue(coords);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetColLabelAlignment
void wxGrid_GetColLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetColLabelAlignment(&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetColLabelSize
void wxGrid_GetColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetColLabelValue
void wxGrid_GetColLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetColLabelValue(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetColMinimalAcceptableWidth
void wxGrid_GetColMinimalAcceptableWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColMinimalAcceptableWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultCellAlignment
void wxGrid_GetDefaultCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetDefaultCellAlignment(&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetDefaultCellBackgroundColour
void wxGrid_GetDefaultCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetDefaultCellBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetDefaultCellFont
void wxGrid_GetDefaultCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetDefaultCellFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGrid::GetDefaultCellTextColour
void wxGrid_GetDefaultCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetDefaultCellTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetDefaultColLabelSize
void wxGrid_GetDefaultColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultColLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultColSize
void wxGrid_GetDefaultColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultColSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultEditor
void wxGrid_GetDefaultEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultEditorForCell
void wxGrid_GetDefaultEditorForCell_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditorForCell(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultEditorForCell
void wxGrid_GetDefaultEditorForCell_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  int cR;
  if(!enif_get_int(env, c_t[0], &cR)) Badarg("c");
  int cC;
  if(!enif_get_int(env, c_t[1], &cC)) Badarg("c");
  wxGridCellCoords c = wxGridCellCoords(cR,cC);
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditorForCell(c);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultEditorForType
void wxGrid_GetDefaultEditorForType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[1], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetDefaultEditorForType(typeName);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGrid::GetDefaultRenderer
void wxGrid_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetDefaultRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetDefaultRendererForCell
void wxGrid_GetDefaultRendererForCell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetDefaultRendererForCell(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetDefaultRendererForType
void wxGrid_GetDefaultRendererForType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[1], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetDefaultRendererForType(typeName);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGrid::GetDefaultRowLabelSize
void wxGrid_GetDefaultRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultRowLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetDefaultRowSize
void wxGrid_GetDefaultRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDefaultRowSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetGridCursorCol
void wxGrid_GetGridCursorCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetGridCursorCol();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetGridCursorRow
void wxGrid_GetGridCursorRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetGridCursorRow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetGridLineColour
void wxGrid_GetGridLineColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetGridLineColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GridLinesEnabled
void wxGrid_GridLinesEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GridLinesEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::GetLabelBackgroundColour
void wxGrid_GetLabelBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetLabelBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetLabelFont
void wxGrid_GetLabelFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetLabelFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGrid::GetLabelTextColour
void wxGrid_GetLabelTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetLabelTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetNumberCols
void wxGrid_GetNumberCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNumberCols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetNumberRows
void wxGrid_GetNumberRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNumberRows();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetOrCreateCellAttr
void wxGrid_GetOrCreateCellAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellAttr * Result = (wxGridCellAttr*)This->GetOrCreateCellAttr(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellAttr"));

}

// wxGrid::GetRowMinimalAcceptableHeight
void wxGrid_GetRowMinimalAcceptableHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowMinimalAcceptableHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetRowLabelAlignment
void wxGrid_GetRowLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int horiz;
  int vert;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetRowLabelAlignment(&horiz,&vert);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(horiz),
  rt.make_int(vert));
  rt.send(msg);

}

// wxGrid::GetRowLabelSize
void wxGrid_GetRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowLabelSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetRowLabelValue
void wxGrid_GetRowLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetRowLabelValue(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetRowSize
void wxGrid_GetRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowSize(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetScrollLineX
void wxGrid_GetScrollLineX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollLineX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetScrollLineY
void wxGrid_GetScrollLineY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollLineY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::GetSelectedCells
void wxGrid_GetSelectedCells(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellCoordsArray Result = This->GetSelectedCells();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result));

}

// wxGrid::GetSelectedCols
void wxGrid_GetSelectedCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxArrayInt Result = This->GetSelectedCols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetSelectedRows
void wxGrid_GetSelectedRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxArrayInt Result = This->GetSelectedRows();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetSelectionBackground
void wxGrid_GetSelectionBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetSelectionBackground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetSelectionBlockTopLeft
void wxGrid_GetSelectionBlockTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellCoordsArray Result = This->GetSelectionBlockTopLeft();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result));

}

// wxGrid::GetSelectionBlockBottomRight
void wxGrid_GetSelectionBlockBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGridCellCoordsArray Result = This->GetSelectionBlockBottomRight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result));

}

// wxGrid::GetSelectionForeground
void wxGrid_GetSelectionForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetSelectionForeground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGrid::GetGridWindow
void wxGrid_GetGridWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::GetGridRowLabelWindow
void wxGrid_GetGridRowLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridRowLabelWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::GetGridColLabelWindow
void wxGrid_GetGridColLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridColLabelWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::GetGridCornerLabelWindow
void wxGrid_GetGridCornerLabelWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetGridCornerLabelWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxGrid::HideCellEditControl
void wxGrid_HideCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HideCellEditControl();

}

// wxGrid::InsertCols
void wxGrid_InsertCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numCols=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numCols"))) {
  if(!enif_get_int(env, tpl[1], &numCols)) Badarg("numCols"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertCols(pos,numCols,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::InsertRows
void wxGrid_InsertRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pos=0;
  int numRows=1;
  bool updateLabels=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
  if(!enif_get_int(env, tpl[1], &pos)) Badarg("pos"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "numRows"))) {
  if(!enif_get_int(env, tpl[1], &numRows)) Badarg("numRows"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "updateLabels"))) {
  updateLabels = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertRows(pos,numRows,updateLabels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsCellEditControlEnabled
void wxGrid_IsCellEditControlEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCellEditControlEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsCurrentCellReadOnly
void wxGrid_IsCurrentCellReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCurrentCellReadOnly();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsEditable
void wxGrid_IsEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEditable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsInSelection
void wxGrid_IsInSelection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsInSelection(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsInSelection
void wxGrid_IsInSelection_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsInSelection(coords);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsReadOnly
void wxGrid_IsReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsReadOnly(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsSelection
void wxGrid_IsSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsVisible
void wxGrid_IsVisible_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool wholeCellVisible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "wholeCellVisible"))) {
  wholeCellVisible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVisible(row,col,wholeCellVisible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::IsVisible
void wxGrid_IsVisible_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool wholeCellVisible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "wholeCellVisible"))) {
  wholeCellVisible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVisible(coords,wholeCellVisible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MakeCellVisible
void wxGrid_MakeCellVisible_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->MakeCellVisible(row,col);

}

// wxGrid::MakeCellVisible
void wxGrid_MakeCellVisible_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  This->MakeCellVisible(coords);

}

// wxGrid::MoveCursorDown
void wxGrid_MoveCursorDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorDown(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorLeft
void wxGrid_MoveCursorLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorLeft(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorRight
void wxGrid_MoveCursorRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorRight(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorUp
void wxGrid_MoveCursorUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorUp(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorDownBlock
void wxGrid_MoveCursorDownBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorDownBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorLeftBlock
void wxGrid_MoveCursorLeftBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorLeftBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorRightBlock
void wxGrid_MoveCursorRightBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorRightBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MoveCursorUpBlock
void wxGrid_MoveCursorUpBlock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  bool expandSelection;
  expandSelection = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->MoveCursorUpBlock(expandSelection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MovePageDown
void wxGrid_MovePageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MovePageDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::MovePageUp
void wxGrid_MovePageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MovePageUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGrid::RegisterDataType
void wxGrid_RegisterDataType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[1], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[2], "renderer");
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[3], "editor");
  if(!This) throw wxe_badarg("This");
  This->RegisterDataType(typeName,renderer,editor);

}

// wxGrid::SaveEditControlValue
void wxGrid_SaveEditControlValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SaveEditControlValue();

}

// wxGrid::SelectAll
void wxGrid_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SelectAll();

}

// wxGrid::SelectBlock
void wxGrid_SelectBlock_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int topRow;
  if(!enif_get_int(env, argv[1], &topRow)) Badarg("topRow"); // int
  int leftCol;
  if(!enif_get_int(env, argv[2], &leftCol)) Badarg("leftCol"); // int
  int bottomRow;
  if(!enif_get_int(env, argv[3], &bottomRow)) Badarg("bottomRow"); // int
  int rightCol;
  if(!enif_get_int(env, argv[4], &rightCol)) Badarg("rightCol"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectBlock(topRow,leftCol,bottomRow,rightCol,addToSelected);

}

// wxGrid::SelectBlock
void wxGrid_SelectBlock_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *topLeft_t;
  int topLeft_sz;
  if(!enif_get_tuple(env, argv[1], &topLeft_sz, &topLeft_t)) Badarg("topLeft");
  int topLeftR;
  if(!enif_get_int(env, topLeft_t[0], &topLeftR)) Badarg("topLeft");
  int topLeftC;
  if(!enif_get_int(env, topLeft_t[1], &topLeftC)) Badarg("topLeft");
  wxGridCellCoords topLeft = wxGridCellCoords(topLeftR,topLeftC);
  const ERL_NIF_TERM *bottomRight_t;
  int bottomRight_sz;
  if(!enif_get_tuple(env, argv[2], &bottomRight_sz, &bottomRight_t)) Badarg("bottomRight");
  int bottomRightR;
  if(!enif_get_int(env, bottomRight_t[0], &bottomRightR)) Badarg("bottomRight");
  int bottomRightC;
  if(!enif_get_int(env, bottomRight_t[1], &bottomRightC)) Badarg("bottomRight");
  wxGridCellCoords bottomRight = wxGridCellCoords(bottomRightR,bottomRightC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectBlock(topLeft,bottomRight,addToSelected);

}

// wxGrid::SelectCol
void wxGrid_SelectCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectCol(col,addToSelected);

}

// wxGrid::SelectRow
void wxGrid_SelectRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool addToSelected=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "addToSelected"))) {
  addToSelected = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectRow(row,addToSelected);

}

// wxGrid::SetCellAlignment
void wxGrid_SetCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  int horiz;
  if(!enif_get_int(env, argv[3], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[4], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetCellAlignment(row,col,horiz,vert);

}

// wxGrid::SetCellBackgroundColour
void wxGrid_SetCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[3], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetCellBackgroundColour(row,col,colour);

}

// wxGrid::SetCellEditor
void wxGrid_SetCellEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[3], "editor");
  if(!This) throw wxe_badarg("This");
  This->SetCellEditor(row,col,editor);

}

// wxGrid::SetCellFont
void wxGrid_SetCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[3], "font");
  if(!This) throw wxe_badarg("This");
  This->SetCellFont(row,col,*font);

}

// wxGrid::SetCellRenderer
void wxGrid_SetCellRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[3], "renderer");
  if(!This) throw wxe_badarg("This");
  This->SetCellRenderer(row,col,renderer);

}

// wxGrid::SetCellTextColour
void wxGrid_SetCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[3], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetCellTextColour(row,col,colour);

}

// wxGrid::SetCellValue
void wxGrid_SetCellValue_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  ErlNifBinary s_bin;
  wxString s;
  if(!enif_inspect_binary(env, argv[3], &s_bin)) Badarg("s");
  s = wxString(s_bin.data, wxConvUTF8, s_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetCellValue(row,col,s);

}

// wxGrid::SetCellValue
void wxGrid_SetCellValue_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  ErlNifBinary s_bin;
  wxString s;
  if(!enif_inspect_binary(env, argv[2], &s_bin)) Badarg("s");
  s = wxString(s_bin.data, wxConvUTF8, s_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetCellValue(coords,s);

}

// wxGrid::SetColAttr
void wxGrid_SetColAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  if(!This) throw wxe_badarg("This");
  This->SetColAttr(col,attr);

}

// wxGrid::SetColFormatBool
void wxGrid_SetColFormatBool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColFormatBool(col);

}

// wxGrid::SetColFormatNumber
void wxGrid_SetColFormatNumber(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColFormatNumber(col);

}

// wxGrid::SetColFormatFloat
void wxGrid_SetColFormatFloat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=-1;
  int precision=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "precision"))) {
  if(!enif_get_int(env, tpl[1], &precision)) Badarg("precision"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetColFormatFloat(col,width,precision);

}

// wxGrid::SetColFormatCustom
void wxGrid_SetColFormatCustom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ErlNifBinary typeName_bin;
  wxString typeName;
  if(!enif_inspect_binary(env, argv[2], &typeName_bin)) Badarg("typeName");
  typeName = wxString(typeName_bin.data, wxConvUTF8, typeName_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetColFormatCustom(col,typeName);

}

// wxGrid::SetColLabelAlignment
void wxGrid_SetColLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int horiz;
  if(!enif_get_int(env, argv[1], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[2], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColLabelAlignment(horiz,vert);

}

// wxGrid::SetColLabelSize
void wxGrid_SetColLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColLabelSize(height);

}

// wxGrid::SetColLabelValue
void wxGrid_SetColLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetColLabelValue(col,value);

}

// wxGrid::SetColMinimalWidth
void wxGrid_SetColMinimalWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColMinimalWidth(col,width);

}

// wxGrid::SetColMinimalAcceptableWidth
void wxGrid_SetColMinimalAcceptableWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColMinimalAcceptableWidth(width);

}

// wxGrid::SetColSize
void wxGrid_SetColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColSize(col,width);

}

// wxGrid::SetDefaultCellAlignment
void wxGrid_SetDefaultCellAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int horiz;
  if(!enif_get_int(env, argv[1], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[2], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellAlignment(horiz,vert);

}

// wxGrid::SetDefaultCellBackgroundColour
void wxGrid_SetDefaultCellBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellBackgroundColour(colour);

}

// wxGrid::SetDefaultCellFont
void wxGrid_SetDefaultCellFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellFont(*font);

}

// wxGrid::SetDefaultCellTextColour
void wxGrid_SetDefaultCellTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultCellTextColour(colour);

}

// wxGrid::SetDefaultEditor
void wxGrid_SetDefaultEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[1], "editor");
  if(!This) throw wxe_badarg("This");
  This->SetDefaultEditor(editor);

}

// wxGrid::SetDefaultRenderer
void wxGrid_SetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[1], "renderer");
  if(!This) throw wxe_badarg("This");
  This->SetDefaultRenderer(renderer);

}

// wxGrid::SetDefaultColSize
void wxGrid_SetDefaultColSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool resizeExistingCols=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "resizeExistingCols"))) {
  resizeExistingCols = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetDefaultColSize(width,resizeExistingCols);

}

// wxGrid::SetDefaultRowSize
void wxGrid_SetDefaultRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool resizeExistingRows=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "resizeExistingRows"))) {
  resizeExistingRows = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetDefaultRowSize(height,resizeExistingRows);

}

// wxGrid::SetGridCursor
void wxGrid_SetGridCursor_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetGridCursor(row,col);

}

// wxGrid::SetGridCursor
void wxGrid_SetGridCursor_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *coords_t;
  int coords_sz;
  if(!enif_get_tuple(env, argv[1], &coords_sz, &coords_t)) Badarg("coords");
  int coordsR;
  if(!enif_get_int(env, coords_t[0], &coordsR)) Badarg("coords");
  int coordsC;
  if(!enif_get_int(env, coords_t[1], &coordsC)) Badarg("coords");
  wxGridCellCoords coords = wxGridCellCoords(coordsR,coordsC);
  if(!This) throw wxe_badarg("This");
  This->SetGridCursor(coords);

}

// wxGrid::SetGridLineColour
void wxGrid_SetGridLineColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetGridLineColour(colour);

}

// wxGrid::SetLabelBackgroundColour
void wxGrid_SetLabelBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetLabelBackgroundColour(colour);

}

// wxGrid::SetLabelFont
void wxGrid_SetLabelFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetLabelFont(*font);

}

// wxGrid::SetLabelTextColour
void wxGrid_SetLabelTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetLabelTextColour(colour);

}

// wxGrid::SetMargins
void wxGrid_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int extraWidth;
  if(!enif_get_int(env, argv[1], &extraWidth)) Badarg("extraWidth"); // int
  int extraHeight;
  if(!enif_get_int(env, argv[2], &extraHeight)) Badarg("extraHeight"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMargins(extraWidth,extraHeight);

}

// wxGrid::SetReadOnly
void wxGrid_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool isReadOnly=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "isReadOnly"))) {
  isReadOnly = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetReadOnly(row,col,isReadOnly);

}

// wxGrid::SetRowAttr
void wxGrid_SetRowAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  if(!This) throw wxe_badarg("This");
  This->SetRowAttr(row,attr);

}

// wxGrid::SetRowLabelAlignment
void wxGrid_SetRowLabelAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int horiz;
  if(!enif_get_int(env, argv[1], &horiz)) Badarg("horiz"); // int
  int vert;
  if(!enif_get_int(env, argv[2], &vert)) Badarg("vert"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowLabelAlignment(horiz,vert);

}

// wxGrid::SetRowLabelSize
void wxGrid_SetRowLabelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowLabelSize(width);

}

// wxGrid::SetRowLabelValue
void wxGrid_SetRowLabelValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetRowLabelValue(row,value);

}

// wxGrid::SetRowMinimalHeight
void wxGrid_SetRowMinimalHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowMinimalHeight(row,height);

}

// wxGrid::SetRowMinimalAcceptableHeight
void wxGrid_SetRowMinimalAcceptableHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowMinimalAcceptableHeight(height);

}

// wxGrid::SetRowSize
void wxGrid_SetRowSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRowSize(row,height);

}

// wxGrid::SetScrollLineX
void wxGrid_SetScrollLineX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  if(!This) throw wxe_badarg("This");
  This->SetScrollLineX(x);

}

// wxGrid::SetScrollLineY
void wxGrid_SetScrollLineY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->SetScrollLineY(y);

}

// wxGrid::SetSelectionBackground
void wxGrid_SetSelectionBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  int cR;
  if(!enif_get_int(env, c_t[0], &cR)) Badarg("c");
  int cG;
  if(!enif_get_int(env, c_t[1], &cG)) Badarg("c");
  int cB;
  if(!enif_get_int(env, c_t[2], &cB)) Badarg("c");
  int cA;
  if(!enif_get_int(env, c_t[3], &cA)) Badarg("c");
  wxColour c = wxColour(cR,cG,cB,cA);
  if(!This) throw wxe_badarg("This");
  This->SetSelectionBackground(c);

}

// wxGrid::SetSelectionForeground
void wxGrid_SetSelectionForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  int cR;
  if(!enif_get_int(env, c_t[0], &cR)) Badarg("c");
  int cG;
  if(!enif_get_int(env, c_t[1], &cG)) Badarg("c");
  int cB;
  if(!enif_get_int(env, c_t[2], &cB)) Badarg("c");
  int cA;
  if(!enif_get_int(env, c_t[3], &cA)) Badarg("c");
  wxColour c = wxColour(cR,cG,cB,cA);
  if(!This) throw wxe_badarg("This");
  This->SetSelectionForeground(c);

}

// wxGrid::SetSelectionMode
void wxGrid_SetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  wxGrid::wxGridSelectionModes selmode;
  if(!enif_get_int(env, argv[1], (int *) &selmode)) Badarg("selmode"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetSelectionMode(selmode);

}

// wxGrid::ShowCellEditControl
void wxGrid_ShowCellEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ShowCellEditControl();

}

// wxGrid::XToCol
void wxGrid_XToCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clipToMinMax=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clipToMinMax"))) {
  clipToMinMax = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->XToCol(x,clipToMinMax);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::XToEdgeOfCol
void wxGrid_XToEdgeOfCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->XToEdgeOfCol(x);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::YToEdgeOfRow
void wxGrid_YToEdgeOfRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->YToEdgeOfRow(y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGrid::YToRow
void wxGrid_YToRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clipToMinMax=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGrid *This;
  This = (wxGrid *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clipToMinMax"))) {
  clipToMinMax = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->YToRow(y,clipToMinMax);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridCellRenderer::Draw
void wxGridCellRenderer_Draw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellRenderer *This;
  This = (wxGridCellRenderer *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[3], "dc");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[4], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  int row;
  if(!enif_get_int(env, argv[5], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[6], &col)) Badarg("col"); // int
  bool isSelected;
  isSelected = enif_is_identical(argv[7], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Draw(*grid,*attr,*dc,rect,row,col,isSelected);

}

// wxGridCellRenderer::GetBestSize
void wxGridCellRenderer_GetBestSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellRenderer *This;
  This = (wxGridCellRenderer *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  wxGridCellAttr *attr;
  attr = (wxGridCellAttr *) memenv->getPtr(env, argv[2], "attr");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[3], "dc");
  int row;
  if(!enif_get_int(env, argv[4], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[5], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetBestSize(*grid,*attr,*dc,row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridCellEditor::Create
void wxGridCellEditor_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  wxEvtHandler *evtHandler;
  evtHandler = (wxEvtHandler *) memenv->getPtr(env, argv[3], "evtHandler");
  if(!This) throw wxe_badarg("This");
  This->Create(parent,id,evtHandler);

}

// wxGridCellEditor::IsCreated
void wxGridCellEditor_IsCreated(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCreated();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellEditor::SetSize
void wxGridCellEditor_SetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->SetSize(rect);

}

// wxGridCellEditor::Show
void wxGridCellEditor_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellAttr * attr=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "attr"))) {
  attr = (wxGridCellAttr *) memenv->getPtr(env, tpl[1], "attr");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Show(show,attr);

}

// wxGridCellEditor::Reset
void wxGridCellEditor_Reset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Reset();

}

// wxGridCellEditor::StartingKey
void wxGridCellEditor_StartingKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  wxKeyEvent *event;
  event = (wxKeyEvent *) memenv->getPtr(env, argv[1], "event");
  if(!This) throw wxe_badarg("This");
  This->StartingKey(*event);

}

// wxGridCellEditor::StartingClick
void wxGridCellEditor_StartingClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StartingClick();

}

// wxGridCellEditor::HandleReturn
void wxGridCellEditor_HandleReturn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellEditor *This;
  This = (wxGridCellEditor *) memenv->getPtr(env, argv[0], "This");
  wxKeyEvent *event;
  event = (wxKeyEvent *) memenv->getPtr(env, argv[1], "event");
  if(!This) throw wxe_badarg("This");
  This->HandleReturn(*event);

}

// wxGridCellBoolRenderer::wxGridCellBoolRenderer
void wxGridCellBoolRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellBoolRenderer * Result = new wxGridCellBoolRenderer();
  app->newPtr((void *) Result, 24, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellBoolRenderer"));

}

// wxGridCellBoolRenderer::destroy
void wxGridCellBoolRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellBoolRenderer *This;
  This = (wxGridCellBoolRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellBoolEditor::wxGridCellBoolEditor
void wxGridCellBoolEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellBoolEditor * Result = new wxGridCellBoolEditor();
  app->newPtr((void *) Result, 25, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellBoolEditor"));

}

// wxGridCellBoolEditor::IsTrueValue
void wxGridCellBoolEditor_IsTrueValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[0], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  bool Result = wxGridCellBoolEditor::IsTrueValue(value);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellBoolEditor::UseStringValues
void wxGridCellBoolEditor_UseStringValues(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString valueTrue= "1";
  wxString valueFalse= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "valueTrue"))) {
  ErlNifBinary valueTrue_bin;
  if(!enif_inspect_binary(env, tpl[1], &valueTrue_bin)) Badarg("valueTrue");
  valueTrue = wxString(valueTrue_bin.data, wxConvUTF8, valueTrue_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "valueFalse"))) {
  ErlNifBinary valueFalse_bin;
  if(!enif_inspect_binary(env, tpl[1], &valueFalse_bin)) Badarg("valueFalse");
  valueFalse = wxString(valueFalse_bin.data, wxConvUTF8, valueFalse_bin.size);
    } else        Badarg("Options");
  };
  wxGridCellBoolEditor::UseStringValues(valueTrue,valueFalse);

}

// wxGridCellBoolEditor::destroy
void wxGridCellBoolEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellBoolEditor *This;
  This = (wxGridCellBoolEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellFloatRenderer::wxGridCellFloatRenderer
void wxGridCellFloatRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=-1;
  int precision=-1;
  int format=wxGRID_FLOAT_FORMAT_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "precision"))) {
  if(!enif_get_int(env, tpl[1], &precision)) Badarg("precision"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "format"))) {
  if(!enif_get_int(env, tpl[1], &format)) Badarg("format"); // int
    } else        Badarg("Options");
  };
  wxGridCellFloatRenderer * Result = new wxGridCellFloatRenderer(width,precision,format);
  app->newPtr((void *) Result, 26, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellFloatRenderer"));

}

// wxGridCellFloatRenderer::GetPrecision
void wxGridCellFloatRenderer_GetPrecision(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPrecision();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridCellFloatRenderer::GetWidth
void wxGridCellFloatRenderer_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridCellFloatRenderer::SetParameters
void wxGridCellFloatRenderer_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellFloatRenderer::SetPrecision
void wxGridCellFloatRenderer_SetPrecision(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  int precision;
  if(!enif_get_int(env, argv[1], &precision)) Badarg("precision"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPrecision(precision);

}

// wxGridCellFloatRenderer::SetWidth
void wxGridCellFloatRenderer_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetWidth(width);

}

// wxGridCellFloatRenderer::destroy
void wxGridCellFloatRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatRenderer *This;
  This = (wxGridCellFloatRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellFloatEditor::wxGridCellFloatEditor
void wxGridCellFloatEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=-1;
  int precision=-1;
  int format=wxGRID_FLOAT_FORMAT_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "precision"))) {
  if(!enif_get_int(env, tpl[1], &precision)) Badarg("precision"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "format"))) {
  if(!enif_get_int(env, tpl[1], &format)) Badarg("format"); // int
    } else        Badarg("Options");
  };
  wxGridCellFloatEditor * Result = new wxGridCellFloatEditor(width,precision,format);
  app->newPtr((void *) Result, 27, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellFloatEditor"));

}

// wxGridCellFloatEditor::SetParameters
void wxGridCellFloatEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatEditor *This;
  This = (wxGridCellFloatEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellFloatEditor::destroy
void wxGridCellFloatEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellFloatEditor *This;
  This = (wxGridCellFloatEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellStringRenderer::wxGridCellStringRenderer
void wxGridCellStringRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellStringRenderer * Result = new wxGridCellStringRenderer();
  app->newPtr((void *) Result, 28, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellStringRenderer"));

}

// wxGridCellStringRenderer::destroy
void wxGridCellStringRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellStringRenderer *This;
  This = (wxGridCellStringRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellTextEditor::wxGridCellTextEditor
void wxGridCellTextEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  size_t maxChars=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "maxChars"))) {
  if(!wxe_get_size_t(env, tpl[1], &maxChars)) Badarg("maxChars");
    } else        Badarg("Options");
  };
  wxGridCellTextEditor * Result = new wxGridCellTextEditor(maxChars);
  app->newPtr((void *) Result, 29, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellTextEditor"));

}

// wxGridCellTextEditor::SetParameters
void wxGridCellTextEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellTextEditor *This;
  This = (wxGridCellTextEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellTextEditor::destroy
void wxGridCellTextEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellTextEditor *This;
  This = (wxGridCellTextEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellChoiceEditor::wxGridCellChoiceEditor
void wxGridCellChoiceEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool allowOthers=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  wxArrayString choices;
  choicesTail = argv[0];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "allowOthers"))) {
  allowOthers = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxGridCellChoiceEditor * Result = new wxGridCellChoiceEditor(choices,allowOthers);
  app->newPtr((void *) Result, 30, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellChoiceEditor"));

}

// wxGridCellChoiceEditor::SetParameters
void wxGridCellChoiceEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellChoiceEditor *This;
  This = (wxGridCellChoiceEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellChoiceEditor::destroy
void wxGridCellChoiceEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellChoiceEditor *This;
  This = (wxGridCellChoiceEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellNumberRenderer::wxGridCellNumberRenderer
void wxGridCellNumberRenderer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGridCellNumberRenderer * Result = new wxGridCellNumberRenderer();
  app->newPtr((void *) Result, 31, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellNumberRenderer"));

}

// wxGridCellNumberRenderer::destroy
void wxGridCellNumberRenderer_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberRenderer *This;
  This = (wxGridCellNumberRenderer *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellNumberEditor::wxGridCellNumberEditor
void wxGridCellNumberEditor_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int min=-1;
  int max=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[0];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "min"))) {
  if(!enif_get_int(env, tpl[1], &min)) Badarg("min"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "max"))) {
  if(!enif_get_int(env, tpl[1], &max)) Badarg("max"); // int
    } else        Badarg("Options");
  };
  wxGridCellNumberEditor * Result = new wxGridCellNumberEditor(min,max);
  app->newPtr((void *) Result, 32, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellNumberEditor"));

}

// wxGridCellNumberEditor::GetValue
void wxGridCellNumberEditor_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberEditor *This;
  This = (wxGridCellNumberEditor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridCellNumberEditor::SetParameters
void wxGridCellNumberEditor_SetParameters(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberEditor *This;
  This = (wxGridCellNumberEditor *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary params_bin;
  wxString params;
  if(!enif_inspect_binary(env, argv[1], &params_bin)) Badarg("params");
  params = wxString(params_bin.data, wxConvUTF8, params_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetParameters(params);

}

// wxGridCellNumberEditor::destroy
void wxGridCellNumberEditor_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellNumberEditor *This;
  This = (wxGridCellNumberEditor *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxGridCellAttr::SetTextColour
void wxGridCellAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colText_t;
  int colText_sz;
  if(!enif_get_tuple(env, argv[1], &colText_sz, &colText_t)) Badarg("colText");
  int colTextR;
  if(!enif_get_int(env, colText_t[0], &colTextR)) Badarg("colText");
  int colTextG;
  if(!enif_get_int(env, colText_t[1], &colTextG)) Badarg("colText");
  int colTextB;
  if(!enif_get_int(env, colText_t[2], &colTextB)) Badarg("colText");
  int colTextA;
  if(!enif_get_int(env, colText_t[3], &colTextA)) Badarg("colText");
  wxColour colText = wxColour(colTextR,colTextG,colTextB,colTextA);
  if(!This) throw wxe_badarg("This");
  This->SetTextColour(colText);

}

// wxGridCellAttr::SetBackgroundColour
void wxGridCellAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colBack_t;
  int colBack_sz;
  if(!enif_get_tuple(env, argv[1], &colBack_sz, &colBack_t)) Badarg("colBack");
  int colBackR;
  if(!enif_get_int(env, colBack_t[0], &colBackR)) Badarg("colBack");
  int colBackG;
  if(!enif_get_int(env, colBack_t[1], &colBackG)) Badarg("colBack");
  int colBackB;
  if(!enif_get_int(env, colBack_t[2], &colBackB)) Badarg("colBack");
  int colBackA;
  if(!enif_get_int(env, colBack_t[3], &colBackA)) Badarg("colBack");
  wxColour colBack = wxColour(colBackR,colBackG,colBackB,colBackA);
  if(!This) throw wxe_badarg("This");
  This->SetBackgroundColour(colBack);

}

// wxGridCellAttr::SetFont
void wxGridCellAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxGridCellAttr::SetAlignment
void wxGridCellAttr_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  int hAlign;
  if(!enif_get_int(env, argv[1], &hAlign)) Badarg("hAlign"); // int
  int vAlign;
  if(!enif_get_int(env, argv[2], &vAlign)) Badarg("vAlign"); // int
  if(!This) throw wxe_badarg("This");
  This->SetAlignment(hAlign,vAlign);

}

// wxGridCellAttr::SetReadOnly
void wxGridCellAttr_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool isReadOnly=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "isReadOnly"))) {
  isReadOnly = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetReadOnly(isReadOnly);

}

// wxGridCellAttr::SetRenderer
void wxGridCellAttr_SetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGridCellRenderer *renderer;
  renderer = (wxGridCellRenderer *) memenv->getPtr(env, argv[1], "renderer");
  if(!This) throw wxe_badarg("This");
  This->SetRenderer(renderer);

}

// wxGridCellAttr::SetEditor
void wxGridCellAttr_SetEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGridCellEditor *editor;
  editor = (wxGridCellEditor *) memenv->getPtr(env, argv[1], "editor");
  if(!This) throw wxe_badarg("This");
  This->SetEditor(editor);

}

// wxGridCellAttr::HasTextColour
void wxGridCellAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasBackgroundColour
void wxGridCellAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasFont
void wxGridCellAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasAlignment
void wxGridCellAttr_HasAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasAlignment();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasRenderer
void wxGridCellAttr_HasRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::HasEditor
void wxGridCellAttr_HasEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasEditor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::GetTextColour
void wxGridCellAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxGridCellAttr::GetBackgroundColour
void wxGridCellAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxGridCellAttr::GetFont
void wxGridCellAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFont * Result = &This->GetFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxGridCellAttr::GetAlignment
void wxGridCellAttr_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int hAlign;
  int vAlign;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetAlignment(&hAlign,&vAlign);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(hAlign),
  rt.make_int(vAlign));
  rt.send(msg);

}

// wxGridCellAttr::GetRenderer
void wxGridCellAttr_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  int row;
  if(!enif_get_int(env, argv[2], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[3], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellRenderer * Result = (wxGridCellRenderer*)This->GetRenderer(grid,row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellRenderer"));

}

// wxGridCellAttr::GetEditor
void wxGridCellAttr_GetEditor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGrid *grid;
  grid = (wxGrid *) memenv->getPtr(env, argv[1], "grid");
  int row;
  if(!enif_get_int(env, argv[2], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[3], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxGridCellEditor * Result = (wxGridCellEditor*)This->GetEditor(grid,row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridCellEditor"));

}

// wxGridCellAttr::IsReadOnly
void wxGridCellAttr_IsReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsReadOnly();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridCellAttr::SetDefAttr
void wxGridCellAttr_SetDefAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridCellAttr *This;
  This = (wxGridCellAttr *) memenv->getPtr(env, argv[0], "This");
  wxGridCellAttr *defAttr;
  defAttr = (wxGridCellAttr *) memenv->getPtr(env, argv[1], "defAttr");
  if(!This) throw wxe_badarg("This");
  This->SetDefAttr(defAttr);

}

// wxDC::Blit
void wxDC_Blit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxRasterOperationMode rop=wxCOPY;
  bool useMask=false;
  wxPoint srcPtMask= wxDefaultPosition;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *dest_t;
  int dest_sz;
  if(!enif_get_tuple(env, argv[1], &dest_sz, &dest_t)) Badarg("dest");
  int destX;
  if(!enif_get_int(env, dest_t[0], &destX)) Badarg("dest");
  int destY;
  if(!enif_get_int(env, dest_t[1], &destY)) Badarg("dest");
  wxPoint dest = wxPoint(destX,destY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  wxDC *source;
  source = (wxDC *) memenv->getPtr(env, argv[3], "source");
  const ERL_NIF_TERM *src_t;
  int src_sz;
  if(!enif_get_tuple(env, argv[4], &src_sz, &src_t)) Badarg("src");
  int srcX;
  if(!enif_get_int(env, src_t[0], &srcX)) Badarg("src");
  int srcY;
  if(!enif_get_int(env, src_t[1], &srcY)) Badarg("src");
  wxPoint src = wxPoint(srcX,srcY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "rop"))) {
  if(!enif_get_int(env, tpl[1], (int *) &rop)) Badarg("rop"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "useMask"))) {
  useMask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "srcPtMask"))) {
  const ERL_NIF_TERM *srcPtMask_t;
  int srcPtMask_sz;
  if(!enif_get_tuple(env, tpl[1], &srcPtMask_sz, &srcPtMask_t)) Badarg("srcPtMask");
  int srcPtMaskX;
  if(!enif_get_int(env, srcPtMask_t[0], &srcPtMaskX)) Badarg("srcPtMask");
  int srcPtMaskY;
  if(!enif_get_int(env, srcPtMask_t[1], &srcPtMaskY)) Badarg("srcPtMask");
  srcPtMask = wxPoint(srcPtMaskX,srcPtMaskY);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Blit(dest,size,source,src,rop,useMask,srcPtMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDC::CalcBoundingBox
void wxDC_CalcBoundingBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  This->CalcBoundingBox(x,y);

}

// wxDC::Clear
void wxDC_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxDC::CrossHair
void wxDC_CrossHair(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->CrossHair(pt);

}

// wxDC::DestroyClippingRegion
void wxDC_DestroyClippingRegion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DestroyClippingRegion();

}

// wxDC::DeviceToLogicalX
void wxDC_DeviceToLogicalX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->DeviceToLogicalX(x);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::DeviceToLogicalXRel
void wxDC_DeviceToLogicalXRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->DeviceToLogicalXRel(x);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::DeviceToLogicalY
void wxDC_DeviceToLogicalY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->DeviceToLogicalY(y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::DeviceToLogicalYRel
void wxDC_DeviceToLogicalYRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->DeviceToLogicalYRel(y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::DrawArc
void wxDC_DrawArc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *ptStart_t;
  int ptStart_sz;
  if(!enif_get_tuple(env, argv[1], &ptStart_sz, &ptStart_t)) Badarg("ptStart");
  int ptStartX;
  if(!enif_get_int(env, ptStart_t[0], &ptStartX)) Badarg("ptStart");
  int ptStartY;
  if(!enif_get_int(env, ptStart_t[1], &ptStartY)) Badarg("ptStart");
  wxPoint ptStart = wxPoint(ptStartX,ptStartY);
  const ERL_NIF_TERM *ptEnd_t;
  int ptEnd_sz;
  if(!enif_get_tuple(env, argv[2], &ptEnd_sz, &ptEnd_t)) Badarg("ptEnd");
  int ptEndX;
  if(!enif_get_int(env, ptEnd_t[0], &ptEndX)) Badarg("ptEnd");
  int ptEndY;
  if(!enif_get_int(env, ptEnd_t[1], &ptEndY)) Badarg("ptEnd");
  wxPoint ptEnd = wxPoint(ptEndX,ptEndY);
  const ERL_NIF_TERM *centre_t;
  int centre_sz;
  if(!enif_get_tuple(env, argv[3], &centre_sz, &centre_t)) Badarg("centre");
  int centreX;
  if(!enif_get_int(env, centre_t[0], &centreX)) Badarg("centre");
  int centreY;
  if(!enif_get_int(env, centre_t[1], &centreY)) Badarg("centre");
  wxPoint centre = wxPoint(centreX,centreY);
  if(!This) throw wxe_badarg("This");
  This->DrawArc(ptStart,ptEnd,centre);

}

// wxDC::DrawBitmap
void wxDC_DrawBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool useMask=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[2], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "useMask"))) {
  useMask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawBitmap(*bmp,pt,useMask);

}

// wxDC::DrawCheckMark
void wxDC_DrawCheckMark(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->DrawCheckMark(rect);

}

// wxDC::DrawCircle
void wxDC_DrawCircle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  int radius;
  if(!enif_get_int(env, argv[2], &radius)) Badarg("radius"); // wxCoord
  if(!This) throw wxe_badarg("This");
  This->DrawCircle(pt,radius);

}

// wxDC::DrawEllipse
void wxDC_DrawEllipse_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->DrawEllipse(pt,size);

}

// wxDC::DrawEllipse
void wxDC_DrawEllipse_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->DrawEllipse(rect);

}

// wxDC::DrawEllipticArc
void wxDC_DrawEllipticArc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[2], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  double sa;
  if(!wxe_get_double(env, argv[3], &sa)) Badarg("sa");
  double ea;
  if(!wxe_get_double(env, argv[4], &ea)) Badarg("ea");
  if(!This) throw wxe_badarg("This");
  This->DrawEllipticArc(pt,sz,sa,ea);

}

// wxDC::DrawIcon
void wxDC_DrawIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[2], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->DrawIcon(*icon,pt);

}

// wxDC::DrawLabel
void wxDC_DrawLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int alignment=wxALIGN_LEFT|wxALIGN_TOP;
  int indexAccel=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[2], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "alignment"))) {
  if(!enif_get_int(env, tpl[1], &alignment)) Badarg("alignment"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "indexAccel"))) {
  if(!enif_get_int(env, tpl[1], &indexAccel)) Badarg("indexAccel"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawLabel(text,rect,alignment,indexAccel);

}

// wxDC::DrawLine
void wxDC_DrawLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt1_t;
  int pt1_sz;
  if(!enif_get_tuple(env, argv[1], &pt1_sz, &pt1_t)) Badarg("pt1");
  int pt1X;
  if(!enif_get_int(env, pt1_t[0], &pt1X)) Badarg("pt1");
  int pt1Y;
  if(!enif_get_int(env, pt1_t[1], &pt1Y)) Badarg("pt1");
  wxPoint pt1 = wxPoint(pt1X,pt1Y);
  const ERL_NIF_TERM *pt2_t;
  int pt2_sz;
  if(!enif_get_tuple(env, argv[2], &pt2_sz, &pt2_t)) Badarg("pt2");
  int pt2X;
  if(!enif_get_int(env, pt2_t[0], &pt2X)) Badarg("pt2");
  int pt2Y;
  if(!enif_get_int(env, pt2_t[1], &pt2Y)) Badarg("pt2");
  wxPoint pt2 = wxPoint(pt2X,pt2Y);
  if(!This) throw wxe_badarg("This");
  This->DrawLine(pt1,pt2);

}

// wxDC::DrawLines
void wxDC_DrawLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord xoffset=0;
  wxCoord yoffset=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  unsigned pointsLen;
  ERL_NIF_TERM pointsHead, pointsTail;
  const ERL_NIF_TERM *points_tpl;
  int points_tsz;
  if(!enif_get_list_length(env, argv[1], &pointsLen)) Badarg("points");
  std::vector <wxPoint> points;
  int x, y;
  pointsTail = argv[1];
  while(!enif_is_empty_list(env, pointsTail)) {
    if(!enif_get_list_cell(env, pointsTail, &pointsHead, &pointsTail)) Badarg("points");
    if(!enif_get_tuple(env, pointsHead, &points_tsz, &points_tpl) || points_tsz != 2) Badarg("points");
    if(!enif_get_int(env, points_tpl[0], &x)) Badarg("points");
    if(!enif_get_int(env, points_tpl[1], &y)) Badarg("points");
    points.push_back(wxPoint(x,y));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "xoffset"))) {
  if(!enif_get_int(env, tpl[1], &xoffset)) Badarg("xoffset"); // wxCoord
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "yoffset"))) {
  if(!enif_get_int(env, tpl[1], &yoffset)) Badarg("yoffset"); // wxCoord
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawLines(pointsLen,points.data(),xoffset,yoffset);

}

// wxDC::DrawPolygon
void wxDC_DrawPolygon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord xoffset=0;
  wxCoord yoffset=0;
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  unsigned pointsLen;
  ERL_NIF_TERM pointsHead, pointsTail;
  const ERL_NIF_TERM *points_tpl;
  int points_tsz;
  if(!enif_get_list_length(env, argv[1], &pointsLen)) Badarg("points");
  std::vector <wxPoint> points;
  int x, y;
  pointsTail = argv[1];
  while(!enif_is_empty_list(env, pointsTail)) {
    if(!enif_get_list_cell(env, pointsTail, &pointsHead, &pointsTail)) Badarg("points");
    if(!enif_get_tuple(env, pointsHead, &points_tsz, &points_tpl) || points_tsz != 2) Badarg("points");
    if(!enif_get_int(env, points_tpl[0], &x)) Badarg("points");
    if(!enif_get_int(env, points_tpl[1], &y)) Badarg("points");
    points.push_back(wxPoint(x,y));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "xoffset"))) {
  if(!enif_get_int(env, tpl[1], &xoffset)) Badarg("xoffset"); // wxCoord
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "yoffset"))) {
  if(!enif_get_int(env, tpl[1], &yoffset)) Badarg("yoffset"); // wxCoord
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawPolygon(pointsLen,points.data(),xoffset,yoffset,fillStyle);

}

// wxDC::DrawPoint
void wxDC_DrawPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->DrawPoint(pt);

}

// wxDC::DrawRectangle
void wxDC_DrawRectangle_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[2], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  This->DrawRectangle(pt,sz);

}

// wxDC::DrawRectangle
void wxDC_DrawRectangle_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->DrawRectangle(rect);

}

// wxDC::DrawRotatedText
void wxDC_DrawRotatedText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[2], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  double angle;
  if(!wxe_get_double(env, argv[3], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->DrawRotatedText(text,point,angle);

}

// wxDC::DrawRoundedRectangle
void wxDC_DrawRoundedRectangle_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[2], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  double radius;
  if(!wxe_get_double(env, argv[3], &radius)) Badarg("radius");
  if(!This) throw wxe_badarg("This");
  This->DrawRoundedRectangle(pt,sz,radius);

}

// wxDC::DrawRoundedRectangle
void wxDC_DrawRoundedRectangle_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  double radius;
  if(!wxe_get_double(env, argv[2], &radius)) Badarg("radius");
  if(!This) throw wxe_badarg("This");
  This->DrawRoundedRectangle(rect,radius);

}

// wxDC::DrawText
void wxDC_DrawText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[2], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->DrawText(text,pt);

}

// wxDC::EndDoc
void wxDC_EndDoc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EndDoc();

}

// wxDC::EndPage
void wxDC_EndPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EndPage();

}

// wxDC::FloodFill
void wxDC_FloodFill(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxFloodFillStyle style=wxFLOOD_SURFACE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[2], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  wxColour col = wxColour(colR,colG,colB,colA);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], (int *) &style)) Badarg("style"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->FloodFill(pt,col,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDC::GetBackground
void wxDC_GetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxBrush * Result = &This->GetBackground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBrush"));

}

// wxDC::GetBackgroundMode
void wxDC_GetBackgroundMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBackgroundMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::GetBrush
void wxDC_GetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxBrush * Result = &This->GetBrush();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBrush"));

}

// wxDC::GetCharHeight
void wxDC_GetCharHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetCharHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::GetCharWidth
void wxDC_GetCharWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetCharWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::GetClippingBox
void wxDC_GetClippingBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord x;
  wxCoord y;
  wxCoord width;
  wxCoord height;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetClippingBox(&x,&y,&width,&height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_int(x),
  rt.make_int(y),
  rt.make_int(width),
  rt.make_int(height));
  rt.send(msg);

}

// wxDC::GetFont
void wxDC_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFont * Result = &This->GetFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxDC::GetLayoutDirection
void wxDC_GetLayoutDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLayoutDirection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::GetLogicalFunction
void wxDC_GetLogicalFunction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLogicalFunction();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::GetMapMode
void wxDC_GetMapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMapMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::GetMultiLineTextExtent
void wxDC_GetMultiLineTextExtent_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord w;
  wxCoord h;
  wxCoord heightLine;
  const wxFont * font=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "font"))) {
  font = (wxFont *) memenv->getPtr(env, tpl[1], "font");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->GetMultiLineTextExtent(string,&w,&h,&heightLine,font);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_int(w),
  rt.make_int(h),
  rt.make_int(heightLine));
  rt.send(msg);

}

// wxDC::GetMultiLineTextExtent
void wxDC_GetMultiLineTextExtent_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetMultiLineTextExtent(string);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDC::GetPartialTextExtents
void wxDC_GetPartialTextExtents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayInt widths;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetPartialTextExtents(text,widths);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(widths));
  rt.send(msg);

}

// wxDC::GetPen
void wxDC_GetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxPen * Result = &This->GetPen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPen"));

}

// wxDC::GetPixel
void wxDC_GetPixel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour colour;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetPixel(pos,&colour);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(colour));
  rt.send(msg);

}

// wxDC::GetPPI
void wxDC_GetPPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetPPI();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDC::GetSize
void wxDC_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDC::GetSizeMM
void wxDC_GetSizeMM(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSizeMM();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDC::GetTextBackground
void wxDC_GetTextBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextBackground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxDC::GetTextExtent
void wxDC_GetTextExtent_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord w;
  wxCoord h;
  wxCoord descent;
  wxCoord externalLeading;
  const wxFont * theFont=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "theFont"))) {
  theFont = (wxFont *) memenv->getPtr(env, tpl[1], "theFont");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->GetTextExtent(string,&w,&h,&descent,&externalLeading,theFont);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_int(w),
  rt.make_int(h),
  rt.make_int(descent),
  rt.make_int(externalLeading));
  rt.send(msg);

}

// wxDC::GetTextExtent
void wxDC_GetTextExtent_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetTextExtent(string);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDC::GetTextForeground
void wxDC_GetTextForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextForeground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxDC::GetUserScale
void wxDC_GetUserScale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  double x;
  double y;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetUserScale(&x,&y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_double(x),
  rt.make_double(y));
  rt.send(msg);

}

// wxDC::GradientFillConcentric
void wxDC_GradientFillConcentric_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  const ERL_NIF_TERM *initialColour_t;
  int initialColour_sz;
  if(!enif_get_tuple(env, argv[2], &initialColour_sz, &initialColour_t)) Badarg("initialColour");
  int initialColourR;
  if(!enif_get_int(env, initialColour_t[0], &initialColourR)) Badarg("initialColour");
  int initialColourG;
  if(!enif_get_int(env, initialColour_t[1], &initialColourG)) Badarg("initialColour");
  int initialColourB;
  if(!enif_get_int(env, initialColour_t[2], &initialColourB)) Badarg("initialColour");
  int initialColourA;
  if(!enif_get_int(env, initialColour_t[3], &initialColourA)) Badarg("initialColour");
  wxColour initialColour = wxColour(initialColourR,initialColourG,initialColourB,initialColourA);
  const ERL_NIF_TERM *destColour_t;
  int destColour_sz;
  if(!enif_get_tuple(env, argv[3], &destColour_sz, &destColour_t)) Badarg("destColour");
  int destColourR;
  if(!enif_get_int(env, destColour_t[0], &destColourR)) Badarg("destColour");
  int destColourG;
  if(!enif_get_int(env, destColour_t[1], &destColourG)) Badarg("destColour");
  int destColourB;
  if(!enif_get_int(env, destColour_t[2], &destColourB)) Badarg("destColour");
  int destColourA;
  if(!enif_get_int(env, destColour_t[3], &destColourA)) Badarg("destColour");
  wxColour destColour = wxColour(destColourR,destColourG,destColourB,destColourA);
  if(!This) throw wxe_badarg("This");
  This->GradientFillConcentric(rect,initialColour,destColour);

}

// wxDC::GradientFillConcentric
void wxDC_GradientFillConcentric_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  const ERL_NIF_TERM *initialColour_t;
  int initialColour_sz;
  if(!enif_get_tuple(env, argv[2], &initialColour_sz, &initialColour_t)) Badarg("initialColour");
  int initialColourR;
  if(!enif_get_int(env, initialColour_t[0], &initialColourR)) Badarg("initialColour");
  int initialColourG;
  if(!enif_get_int(env, initialColour_t[1], &initialColourG)) Badarg("initialColour");
  int initialColourB;
  if(!enif_get_int(env, initialColour_t[2], &initialColourB)) Badarg("initialColour");
  int initialColourA;
  if(!enif_get_int(env, initialColour_t[3], &initialColourA)) Badarg("initialColour");
  wxColour initialColour = wxColour(initialColourR,initialColourG,initialColourB,initialColourA);
  const ERL_NIF_TERM *destColour_t;
  int destColour_sz;
  if(!enif_get_tuple(env, argv[3], &destColour_sz, &destColour_t)) Badarg("destColour");
  int destColourR;
  if(!enif_get_int(env, destColour_t[0], &destColourR)) Badarg("destColour");
  int destColourG;
  if(!enif_get_int(env, destColour_t[1], &destColourG)) Badarg("destColour");
  int destColourB;
  if(!enif_get_int(env, destColour_t[2], &destColourB)) Badarg("destColour");
  int destColourA;
  if(!enif_get_int(env, destColour_t[3], &destColourA)) Badarg("destColour");
  wxColour destColour = wxColour(destColourR,destColourG,destColourB,destColourA);
  const ERL_NIF_TERM *circleCenter_t;
  int circleCenter_sz;
  if(!enif_get_tuple(env, argv[4], &circleCenter_sz, &circleCenter_t)) Badarg("circleCenter");
  int circleCenterX;
  if(!enif_get_int(env, circleCenter_t[0], &circleCenterX)) Badarg("circleCenter");
  int circleCenterY;
  if(!enif_get_int(env, circleCenter_t[1], &circleCenterY)) Badarg("circleCenter");
  wxPoint circleCenter = wxPoint(circleCenterX,circleCenterY);
  if(!This) throw wxe_badarg("This");
  This->GradientFillConcentric(rect,initialColour,destColour,circleCenter);

}

// wxDC::GradientFillLinear
void wxDC_GradientFillLinear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxDirection nDirection=wxRIGHT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  const ERL_NIF_TERM *initialColour_t;
  int initialColour_sz;
  if(!enif_get_tuple(env, argv[2], &initialColour_sz, &initialColour_t)) Badarg("initialColour");
  int initialColourR;
  if(!enif_get_int(env, initialColour_t[0], &initialColourR)) Badarg("initialColour");
  int initialColourG;
  if(!enif_get_int(env, initialColour_t[1], &initialColourG)) Badarg("initialColour");
  int initialColourB;
  if(!enif_get_int(env, initialColour_t[2], &initialColourB)) Badarg("initialColour");
  int initialColourA;
  if(!enif_get_int(env, initialColour_t[3], &initialColourA)) Badarg("initialColour");
  wxColour initialColour = wxColour(initialColourR,initialColourG,initialColourB,initialColourA);
  const ERL_NIF_TERM *destColour_t;
  int destColour_sz;
  if(!enif_get_tuple(env, argv[3], &destColour_sz, &destColour_t)) Badarg("destColour");
  int destColourR;
  if(!enif_get_int(env, destColour_t[0], &destColourR)) Badarg("destColour");
  int destColourG;
  if(!enif_get_int(env, destColour_t[1], &destColourG)) Badarg("destColour");
  int destColourB;
  if(!enif_get_int(env, destColour_t[2], &destColourB)) Badarg("destColour");
  int destColourA;
  if(!enif_get_int(env, destColour_t[3], &destColourA)) Badarg("destColour");
  wxColour destColour = wxColour(destColourR,destColourG,destColourB,destColourA);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "nDirection"))) {
  if(!enif_get_int(env, tpl[1], (int *) &nDirection)) Badarg("nDirection"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->GradientFillLinear(rect,initialColour,destColour,nDirection);

}

// wxDC::LogicalToDeviceX
void wxDC_LogicalToDeviceX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->LogicalToDeviceX(x);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::LogicalToDeviceXRel
void wxDC_LogicalToDeviceXRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->LogicalToDeviceXRel(x);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::LogicalToDeviceY
void wxDC_LogicalToDeviceY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->LogicalToDeviceY(y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::LogicalToDeviceYRel
void wxDC_LogicalToDeviceYRel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->LogicalToDeviceYRel(y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::MaxX
void wxDC_MaxX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->MaxX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::MaxY
void wxDC_MaxY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->MaxY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::MinX
void wxDC_MinX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->MinX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::MinY
void wxDC_MinY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->MinY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDC::IsOk
void wxDC_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDC::ResetBoundingBox
void wxDC_ResetBoundingBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ResetBoundingBox();

}

// wxDC::SetAxisOrientation
void wxDC_SetAxisOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  bool xLeftRight;
  xLeftRight = enif_is_identical(argv[1], WXE_ATOM_true);
  bool yBottomUp;
  yBottomUp = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetAxisOrientation(xLeftRight,yBottomUp);

}

// wxDC::SetBackground
void wxDC_SetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxBrush *brush;
  brush = (wxBrush *) memenv->getPtr(env, argv[1], "brush");
  if(!This) throw wxe_badarg("This");
  This->SetBackground(*brush);

}

// wxDC::SetBackgroundMode
void wxDC_SetBackgroundMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int mode;
  if(!enif_get_int(env, argv[1], &mode)) Badarg("mode"); // int
  if(!This) throw wxe_badarg("This");
  This->SetBackgroundMode(mode);

}

// wxDC::SetBrush
void wxDC_SetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxBrush *brush;
  brush = (wxBrush *) memenv->getPtr(env, argv[1], "brush");
  if(!This) throw wxe_badarg("This");
  This->SetBrush(*brush);

}

// wxDC::SetClippingRegion
void wxDC_SetClippingRegion_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[2], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  This->SetClippingRegion(pt,sz);

}

// wxDC::SetClippingRegion
void wxDC_SetClippingRegion_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[1], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  if(!This) throw wxe_badarg("This");
  This->SetClippingRegion(rect);

}

// wxDC::SetDeviceOrigin
void wxDC_SetDeviceOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  This->SetDeviceOrigin(x,y);

}

// wxDC::SetFont
void wxDC_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxDC::SetLayoutDirection
void wxDC_SetLayoutDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxLayoutDirection dir;
  if(!enif_get_int(env, argv[1], (int *) &dir)) Badarg("dir"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetLayoutDirection(dir);

}

// wxDC::SetLogicalFunction
void wxDC_SetLogicalFunction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxRasterOperationMode function;
  if(!enif_get_int(env, argv[1], (int *) &function)) Badarg("function"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetLogicalFunction(function);

}

// wxDC::SetMapMode
void wxDC_SetMapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxMappingMode mode;
  if(!enif_get_int(env, argv[1], (int *) &mode)) Badarg("mode"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetMapMode(mode);

}

// wxDC::SetPalette
void wxDC_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxPalette *palette;
  palette = (wxPalette *) memenv->getPtr(env, argv[1], "palette");
  if(!This) throw wxe_badarg("This");
  This->SetPalette(*palette);

}

// wxDC::SetPen
void wxDC_SetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  wxPen *pen;
  pen = (wxPen *) memenv->getPtr(env, argv[1], "pen");
  if(!This) throw wxe_badarg("This");
  This->SetPen(*pen);

}

// wxDC::SetTextBackground
void wxDC_SetTextBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetTextBackground(colour);

}

// wxDC::SetTextForeground
void wxDC_SetTextForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[1], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  if(!This) throw wxe_badarg("This");
  This->SetTextForeground(colour);

}

// wxDC::SetUserScale
void wxDC_SetUserScale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  double xScale;
  if(!wxe_get_double(env, argv[1], &xScale)) Badarg("xScale");
  double yScale;
  if(!wxe_get_double(env, argv[2], &yScale)) Badarg("yScale");
  if(!This) throw wxe_badarg("This");
  This->SetUserScale(xScale,yScale);

}

// wxDC::StartDoc
void wxDC_StartDoc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->StartDoc(message);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDC::StartPage
void wxDC_StartPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *This;
  This = (wxDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StartPage();

}

// wxMirrorDC::wxMirrorDC
void wxMirrorDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[0], "dc");
  bool mirror;
  mirror = enif_is_identical(argv[1], WXE_ATOM_true);
  wxMirrorDC * Result = new EwxMirrorDC(*dc,mirror);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMirrorDC"));

}

// wxScreenDC::wxScreenDC
void wxScreenDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxScreenDC * Result = new EwxScreenDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxScreenDC"));

}

#if wxUSE_POSTSCRIPT
// wxPostScriptDC::wxPostScriptDC
void wxPostScriptDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPostScriptDC * Result = new EwxPostScriptDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPostScriptDC"));

}

// wxPostScriptDC::wxPostScriptDC
void wxPostScriptDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *printData;
  printData = (wxPrintData *) memenv->getPtr(env, argv[0], "printData");
  wxPostScriptDC * Result = new EwxPostScriptDC(*printData);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPostScriptDC"));

}

#endif // wxUSE_POSTSCRIPT
// wxWindowDC::wxWindowDC
void wxWindowDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  wxWindowDC * Result = new EwxWindowDC(window);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindowDC"));

}

// wxClientDC::wxClientDC
void wxClientDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  wxClientDC * Result = new EwxClientDC(window);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxClientDC"));

}



bool WxeApp::delete_object(void *ptr, wxeRefData *refd) {
 if(wxe_debug) {
     wxString msg;
	    const wxChar *class_info = wxT("unknown");
	    if(refd->type < 10) {
		wxClassInfo *cinfo = ((wxObject *)ptr)->GetClassInfo();
		    class_info = cinfo->GetClassName();
	       }
      msg.Printf(wxT("Deleting {wx_ref, %d, %s} at %p "), refd->ref, class_info, ptr);
      send_msg("debug", &msg);
 };
 switch(refd->type) {
#if wxUSE_GRAPHICS_CONTEXT
  case 4: delete (wxGraphicsObject *) ptr; break;
#endif
  case 24: delete (wxGridCellBoolRenderer *) ptr; break;
  case 25: delete (wxGridCellBoolEditor *) ptr; break;
  case 26: delete (wxGridCellFloatRenderer *) ptr; break;
  case 27: delete (wxGridCellFloatEditor *) ptr; break;
  case 28: delete (wxGridCellStringRenderer *) ptr; break;
  case 29: delete (wxGridCellTextEditor *) ptr; break;
  case 30: delete (wxGridCellChoiceEditor *) ptr; break;
  case 31: delete (wxGridCellNumberRenderer *) ptr; break;
  case 32: delete (wxGridCellNumberEditor *) ptr; break;
  case 62: delete (EwxIconBundle *) ptr; return false;
  case 70: delete (wxAcceleratorEntry *) ptr; break;
  case 71: /* delete (wxCaret *) ptr;These objects must be deleted by owner object */ break;
  case 73: delete (wxSizerFlags *) ptr; break;
  case 89: /* delete (wxCalendarDateAttr *) ptr;These objects must be deleted by owner object */ break;
  case 102: delete (wxListItemAttr *) ptr; break;
  case 104: delete (wxTextAttr *) ptr; break;
  case 158: delete (wxAuiPaneInfo *) ptr; break;
  case 162: /* delete (wxAuiSimpleTabArt *) ptr;These objects must be deleted by owner object */ break;
  case 216: /* delete (wxFileDataObject *) ptr;These objects must be deleted by owner object */ break;
  case 217: /* delete (wxTextDataObject *) ptr;These objects must be deleted by owner object */ break;
  case 218: /* delete (wxBitmapDataObject *) ptr;These objects must be deleted by owner object */ break;
  case 230: delete (wxLogNull *) ptr; break;
  case 234: delete (EwxLocale *) ptr; return false;
  case 239: delete (wxOverlay *) ptr; break;
  case 240: delete (EwxDCOverlay *) ptr; return false;
  case 242: delete (wxDisplay *) ptr; break;
  default: delete (wxObject *) ptr; return false;
  }
  return true;
}

