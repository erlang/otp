/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2008-2026. All Rights Reserved.
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

// wxSashEvent::GetEdge
void wxSashEvent_GetEdge(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashEvent *This;
  This = (wxSashEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetEdge();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSashEvent::GetDragRect
void wxSashEvent_GetDragRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashEvent *This;
  This = (wxSashEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetDragRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSashEvent::GetDragStatus
void wxSashEvent_GetDragStatus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashEvent *This;
  This = (wxSashEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDragStatus();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
  wxLayoutAlignment alignment;
  if(!enif_get_int(env, argv[1], (int *) &alignment)) Badarg("alignment"); // enum
  This->SetAlignment(alignment);

}

// wxSashLayoutWindow::SetDefaultSize
void wxSashLayoutWindow_SetDefaultSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->SetDefaultSize(size);

}

// wxSashLayoutWindow::SetOrientation
void wxSashLayoutWindow_SetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashLayoutWindow *This;
  This = (wxSashLayoutWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxLayoutOrientation orientation;
  if(!enif_get_int(env, argv[1], (int *) &orientation)) Badarg("orientation"); // enum
  This->SetOrientation(orientation);

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
  if(!This) throw wxe_badarg("This");
  wxSashEdgePosition edge;
  if(!enif_get_int(env, argv[1], (int *) &edge)) Badarg("edge"); // enum
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
  if(!This) throw wxe_badarg("This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  This->SetMaximumSizeX(min);

}

// wxSashWindow::SetMaximumSizeY
void wxSashWindow_SetMaximumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  This->SetMaximumSizeY(min);

}

// wxSashWindow::SetMinimumSizeX
void wxSashWindow_SetMinimumSizeX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  This->SetMinimumSizeX(min);

}

// wxSashWindow::SetMinimumSizeY
void wxSashWindow_SetMinimumSizeY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  This->SetMinimumSizeY(min);

}

// wxSashWindow::SetSashVisible
void wxSashWindow_SetSashVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSashWindow *This;
  This = (wxSashWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSashEdgePosition edge;
  if(!enif_get_int(env, argv[1], (int *) &edge)) Badarg("edge"); // enum
  bool visible;
  visible = enif_is_identical(argv[2], WXE_ATOM_true);
  This->SetSashVisible(edge,visible);

}

// wxScreenDC::wxScreenDC
void wxScreenDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxScreenDC * Result = new EwxScreenDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxScreenDC"));

}

// wxScrollBar::wxScrollBar
void wxScrollBar_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxScrollBar * Result = new EwxScrollBar();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxScrollBar"));

}

// wxScrollBar::wxScrollBar
void wxScrollBar_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSB_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxScrollBar * Result = new EwxScrollBar(parent,id,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxScrollBar"));

}

// wxScrollBar::Create
void wxScrollBar_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSB_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  bool Result = This->Create(parent,id,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxScrollBar::GetRange
void wxScrollBar_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRange();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollBar::GetPageSize
void wxScrollBar_GetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollBar::GetThumbPosition
void wxScrollBar_GetThumbPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetThumbPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollBar::GetThumbSize
void wxScrollBar_GetThumbSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetThumbSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollBar::SetThumbPosition
void wxScrollBar_SetThumbPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int viewStart;
  if(!enif_get_int(env, argv[1], &viewStart)) Badarg("viewStart"); // int
  This->SetThumbPosition(viewStart);

}

// wxScrollBar::SetScrollbar
void wxScrollBar_SetScrollbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool refresh=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollBar *This;
  This = (wxScrollBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int position;
  if(!enif_get_int(env, argv[1], &position)) Badarg("position"); // int
  int thumbSize;
  if(!enif_get_int(env, argv[2], &thumbSize)) Badarg("thumbSize"); // int
  int range;
  if(!enif_get_int(env, argv[3], &range)) Badarg("range"); // int
  int pageSize;
  if(!enif_get_int(env, argv[4], &pageSize)) Badarg("pageSize"); // int
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
  This->SetScrollbar(position,thumbSize,range,pageSize,refresh);

}

// wxScrollEvent::GetOrientation
void wxScrollEvent_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollEvent *This;
  This = (wxScrollEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOrientation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollEvent::GetPosition
void wxScrollEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollEvent *This;
  This = (wxScrollEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollWinEvent::GetOrientation
void wxScrollWinEvent_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollWinEvent *This;
  This = (wxScrollWinEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOrientation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxScrollWinEvent::GetPosition
void wxScrollWinEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrollWinEvent *This;
  This = (wxScrollWinEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

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
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
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
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
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
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
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
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
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
  if(!This) throw wxe_badarg("This");
  bool xScrolling;
  xScrolling = enif_is_identical(argv[1], WXE_ATOM_true);
  bool yScrolling;
  yScrolling = enif_is_identical(argv[2], WXE_ATOM_true);
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
  if(!This) throw wxe_badarg("This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  This->DoPrepareDC(*dc);

}

// wxScrolledWindow::PrepareDC
void wxScrolledWindow_PrepareDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  This->PrepareDC(*dc);

}

// wxScrolledWindow::Scroll
void wxScrolledWindow_Scroll_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  This->Scroll(x,y);

}

// wxScrolledWindow::Scroll
void wxScrolledWindow_Scroll_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
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
  if(!This) throw wxe_badarg("This");
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
  This->SetScrollbars(pixelsPerUnitX,pixelsPerUnitY,noUnitsX,noUnitsY,xPos,yPos,noRefresh);

}

// wxScrolledWindow::SetScrollRate
void wxScrolledWindow_SetScrollRate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int xstep;
  if(!enif_get_int(env, argv[1], &xstep)) Badarg("xstep"); // int
  int ystep;
  if(!enif_get_int(env, argv[2], &ystep)) Badarg("ystep"); // int
  This->SetScrollRate(xstep,ystep);

}

// wxScrolledWindow::SetTargetWindow
void wxScrolledWindow_SetTargetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxScrolledWindow *This;
  This = (wxScrolledWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  This->SetTargetWindow(window);

}

// wxSetCursorEvent::GetCursor
void wxSetCursorEvent_GetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSetCursorEvent *This;
  This = (wxSetCursorEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxCursor * Result = &This->GetCursor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCursor"));

}

// wxSetCursorEvent::GetX
void wxSetCursorEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSetCursorEvent *This;
  This = (wxSetCursorEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSetCursorEvent::GetY
void wxSetCursorEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSetCursorEvent *This;
  This = (wxSetCursorEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSetCursorEvent::HasCursor
void wxSetCursorEvent_HasCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSetCursorEvent *This;
  This = (wxSetCursorEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasCursor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSetCursorEvent::SetCursor
void wxSetCursorEvent_SetCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSetCursorEvent *This;
  This = (wxSetCursorEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCursor *cursor;
  cursor = (wxCursor *) memenv->getPtr(env, argv[1], "cursor");
  This->SetCursor(*cursor);

}

// wxShowEvent::SetShow
void wxShowEvent_SetShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxShowEvent *This;
  This = (wxShowEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetShow(show);

}

// wxShowEvent::IsShown
void wxShowEvent_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxShowEvent *This;
  This = (wxShowEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsShown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSingleChoiceDialog::wxSingleChoiceDialog
void wxSingleChoiceDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=wxCHOICEDLG_STYLE;
  wxPoint pos= wxDefaultPosition;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  ErlNifBinary caption_bin;
  wxString caption;
  if(!enif_inspect_binary(env, argv[2], &caption_bin)) Badarg("caption");
  caption = wxString(caption_bin.data, wxConvUTF8, caption_bin.size);
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  wxArrayString choices;
  choicesTail = argv[3];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "pos"))) {
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
  wxSingleChoiceDialog * Result = new EwxSingleChoiceDialog(parent,message,caption,choices,(void **) NULL,style,pos);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSingleChoiceDialog"));

}

// wxSingleChoiceDialog::GetSelection
void wxSingleChoiceDialog_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSingleChoiceDialog *This;
  This = (wxSingleChoiceDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSingleChoiceDialog::GetStringSelection
void wxSingleChoiceDialog_GetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSingleChoiceDialog *This;
  This = (wxSingleChoiceDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetStringSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSingleChoiceDialog::SetSelection
void wxSingleChoiceDialog_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSingleChoiceDialog *This;
  This = (wxSingleChoiceDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int selection;
  if(!enif_get_int(env, argv[1], &selection)) Badarg("selection"); // int
  This->SetSelection(selection);

}

// wxSizeEvent::GetSize
void wxSizeEvent_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizeEvent *This;
  This = (wxSizeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizeEvent::GetRect
void wxSizeEvent_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizeEvent *This;
  This = (wxSizeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizer::Add
void wxSizer_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[2], "flags");
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Add(static_cast<wxWindow*> (window),*flags);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Add(static_cast<wxSizer*> (window),*flags);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Add
void wxSizer_Add_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Add(static_cast<wxWindow*> (window),proportion,flag,border,userData);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Add(static_cast<wxSizer*> (window),proportion,flag,border,userData);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Add
void wxSizer_Add_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result = (wxSizerItem*)This->Add(width,height,proportion,flag,border,userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Add
void wxSizer_Add_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[3], "flags");
  wxSizerItem * Result = (wxSizerItem*)This->Add(width,height,*flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::AddSpacer
void wxSizer_AddSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int size;
  if(!enif_get_int(env, argv[1], &size)) Badarg("size"); // int
  wxSizerItem * Result = (wxSizerItem*)This->AddSpacer(size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::AddStretchSpacer
void wxSizer_AddStretchSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int prop=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "prop"))) {
  if(!enif_get_int(env, tpl[1], &prop)) Badarg("prop"); // int
    } else        Badarg("Options");
  };
  wxSizerItem * Result = (wxSizerItem*)This->AddStretchSpacer(prop);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::CalcMin
void wxSizer_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->CalcMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizer::Clear
void wxSizer_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool delete_windows=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "delete_windows"))) {
  delete_windows = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  This->Clear(delete_windows);

}

// wxSizer::Detach
void wxSizer_Detach_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->Detach(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->Detach(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Detach
void wxSizer_Detach_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  bool Result = This->Detach(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Fit
void wxSizer_Fit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  wxSize Result = This->Fit(window);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizer::FitInside
void wxSizer_FitInside(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  This->FitInside(window);

}

// wxSizer::GetChildren
void wxSizer_GetChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxSizerItemList Result = This->GetChildren();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_list_objs(Result, app, "wxSizerItem"));

}

// wxSizer::GetItem
void wxSizer_GetItem_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool recursive=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "recursive"))) {
  recursive = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->GetItem(static_cast<wxWindow*> (window),recursive);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->GetItem(static_cast<wxSizer*> (window),recursive);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::GetItem
void wxSizer_GetItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxSizerItem * Result = (wxSizerItem*)This->GetItem(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::GetSize
void wxSizer_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizer::GetPosition
void wxSizer_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizer::GetMinSize
void wxSizer_GetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetMinSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizer::Hide
void wxSizer_Hide_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool recursive=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "recursive"))) {
  recursive = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->Hide(static_cast<wxWindow*> (window),recursive);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->Hide(static_cast<wxSizer*> (window),recursive);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Hide
void wxSizer_Hide_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  bool Result = This->Hide(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Insert
void wxSizer_Insert_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[2], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[3], "flags");
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Insert(index,static_cast<wxWindow*> (window),*flags);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Insert(index,static_cast<wxSizer*> (window),*flags);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Insert
void wxSizer_Insert_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[2], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Insert(index,static_cast<wxWindow*> (window),proportion,flag,border,userData);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Insert(index,static_cast<wxSizer*> (window),proportion,flag,border,userData);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Insert
void wxSizer_Insert_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result = (wxSizerItem*)This->Insert(index,width,height,proportion,flag,border,userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Insert
void wxSizer_Insert_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[4], "flags");
  wxSizerItem * Result = (wxSizerItem*)This->Insert(index,width,height,*flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Insert
void wxSizer_Insert_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxSizerItem *item;
  item = (wxSizerItem *) memenv->getPtr(env, argv[2], "item");
  wxSizerItem * Result = (wxSizerItem*)This->Insert(index,item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::InsertSpacer
void wxSizer_InsertSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int size;
  if(!enif_get_int(env, argv[2], &size)) Badarg("size"); // int
  wxSizerItem * Result = (wxSizerItem*)This->InsertSpacer(index,size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::InsertStretchSpacer
void wxSizer_InsertStretchSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int prop=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "prop"))) {
  if(!enif_get_int(env, tpl[1], &prop)) Badarg("prop"); // int
    } else        Badarg("Options");
  };
  wxSizerItem * Result = (wxSizerItem*)This->InsertStretchSpacer(index,prop);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::IsShown
void wxSizer_IsShown_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->IsShown(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->IsShown(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::IsShown
void wxSizer_IsShown_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  bool Result = This->IsShown(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Layout
void wxSizer_Layout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Layout();

}

// wxSizer::Prepend
void wxSizer_Prepend_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[2], "flags");
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Prepend(static_cast<wxWindow*> (window),*flags);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Prepend(static_cast<wxSizer*> (window),*flags);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Prepend
void wxSizer_Prepend_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Prepend(static_cast<wxWindow*> (window),proportion,flag,border,userData);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Prepend(static_cast<wxSizer*> (window),proportion,flag,border,userData);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Prepend
void wxSizer_Prepend_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result = (wxSizerItem*)This->Prepend(width,height,proportion,flag,border,userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Prepend
void wxSizer_Prepend_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[3], "flags");
  wxSizerItem * Result = (wxSizerItem*)This->Prepend(width,height,*flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Prepend
void wxSizer_Prepend_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizerItem *item;
  item = (wxSizerItem *) memenv->getPtr(env, argv[1], "item");
  wxSizerItem * Result = (wxSizerItem*)This->Prepend(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::PrependSpacer
void wxSizer_PrependSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int size;
  if(!enif_get_int(env, argv[1], &size)) Badarg("size"); // int
  wxSizerItem * Result = (wxSizerItem*)This->PrependSpacer(size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::PrependStretchSpacer
void wxSizer_PrependStretchSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int prop=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "prop"))) {
  if(!enif_get_int(env, tpl[1], &prop)) Badarg("prop"); // int
    } else        Badarg("Options");
  };
  wxSizerItem * Result = (wxSizerItem*)This->PrependStretchSpacer(prop);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizer::Remove
void wxSizer_Remove_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  bool Result = This->Remove(sizer);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Remove
void wxSizer_Remove_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  bool Result = This->Remove(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Replace
void wxSizer_Replace_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool recursive=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM oldwin_type;
  void * oldwin = memenv->getPtr(env, argv[1], "oldwin", &oldwin_type);
  ERL_NIF_TERM newwin_type;
  void * newwin = memenv->getPtr(env, argv[2], "newwin", &newwin_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "recursive"))) {
  recursive = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  bool Result;
  if(enif_is_identical(oldwin_type, WXE_ATOM_wxWindow) && enif_is_identical(newwin_type, WXE_ATOM_wxWindow))
   Result =  This->Replace(static_cast<wxWindow*> (oldwin),static_cast<wxWindow*> (newwin),recursive);
  else if(enif_is_identical(oldwin_type, WXE_ATOM_wxSizer) && enif_is_identical(newwin_type, WXE_ATOM_wxSizer))
   Result =  This->Replace(static_cast<wxSizer*> (oldwin),static_cast<wxSizer*> (newwin),recursive);
  else throw wxe_badarg("oldwin");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Replace
void wxSizer_Replace_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxSizerItem *newitem;
  newitem = (wxSizerItem *) memenv->getPtr(env, argv[2], "newitem");
  bool Result = This->Replace(index,newitem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::SetDimension
void wxSizer_SetDimension_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // int
  This->SetDimension(x,y,width,height);

}

// wxSizer::SetDimension
void wxSizer_SetDimension_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->SetDimension(pos,size);

}

// wxSizer::SetMinSize
void wxSizer_SetMinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->SetMinSize(size);

}

// wxSizer::SetMinSize
void wxSizer_SetMinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  This->SetMinSize(width,height);

}

// wxSizer::SetItemMinSize
void wxSizer_SetItemMinSize_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->SetItemMinSize(static_cast<wxWindow*> (window),width,height);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->SetItemMinSize(static_cast<wxSizer*> (window),width,height);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::SetItemMinSize
void wxSizer_SetItemMinSize_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->SetItemMinSize(static_cast<wxWindow*> (window),size);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->SetItemMinSize(static_cast<wxSizer*> (window),size);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::SetItemMinSize
void wxSizer_SetItemMinSize_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  bool Result = This->SetItemMinSize(index,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::SetItemMinSize
void wxSizer_SetItemMinSize_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  bool Result = This->SetItemMinSize(index,size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::SetSizeHints
void wxSizer_SetSizeHints(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  This->SetSizeHints(window);

}

// wxSizer::Show
void wxSizer_Show_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  bool recursive=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "show"))) {
  show = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "recursive"))) {
  recursive = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->Show(static_cast<wxWindow*> (window),show,recursive);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->Show(static_cast<wxSizer*> (window),show,recursive);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Show
void wxSizer_Show_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
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
  bool Result = This->Show(index,show);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizer::Show
void wxSizer_Show_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  This->Show(show);

}

// wxSizer::ShowItems
void wxSizer_ShowItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  This->ShowItems(show);

}

// wxSizerFlags::wxSizerFlags
void wxSizerFlags_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else        Badarg("Options");
  };
  wxSizerFlags * Result = new wxSizerFlags(proportion);
  app->newPtr((void *) Result, 73, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Align
void wxSizerFlags_Align(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int alignment;
  if(!enif_get_int(env, argv[1], &alignment)) Badarg("alignment"); // int
  wxSizerFlags * Result = &This->Align(alignment);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Border
void wxSizerFlags_Border_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int direction;
  if(!enif_get_int(env, argv[1], &direction)) Badarg("direction"); // int
  int borderinpixels;
  if(!enif_get_int(env, argv[2], &borderinpixels)) Badarg("borderinpixels"); // int
  wxSizerFlags * Result = &This->Border(direction,borderinpixels);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Border
void wxSizerFlags_Border_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int direction=wxALL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "direction"))) {
  if(!enif_get_int(env, tpl[1], &direction)) Badarg("direction"); // int
    } else        Badarg("Options");
  };
  wxSizerFlags * Result = &This->Border(direction);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Center
void wxSizerFlags_Center(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizerFlags * Result = &This->Center();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Expand
void wxSizerFlags_Expand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizerFlags * Result = &This->Expand();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Left
void wxSizerFlags_Left(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizerFlags * Result = &This->Left();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Proportion
void wxSizerFlags_Proportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int proportion;
  if(!enif_get_int(env, argv[1], &proportion)) Badarg("proportion"); // int
  wxSizerFlags * Result = &This->Proportion(proportion);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::Right
void wxSizerFlags_Right(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizerFlags * Result = &This->Right();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerFlags"));

}

// wxSizerFlags::destroy
void wxSizerFlags_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerFlags *This;
  This = (wxSizerFlags *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxSizerItem::wxSizerItem
void wxSizerItem_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result = new EwxSizerItem(width,height,proportion,flag,border,userData);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizerItem::wxSizerItem
void wxSizerItem_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[0], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[1], "flags");
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
    Result = new EwxSizerItem(static_cast<wxWindow*> (window),*flags);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
    Result = new EwxSizerItem(static_cast<wxSizer*> (window),*flags);
  else throw wxe_badarg("window");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizerItem::wxSizerItem
void wxSizerItem_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[0], "window", &window_type);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "proportion"))) {
  if(!enif_get_int(env, tpl[1], &proportion)) Badarg("proportion"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
    Result = new EwxSizerItem(static_cast<wxWindow*> (window),proportion,flag,border,userData);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
    Result = new EwxSizerItem(static_cast<wxSizer*> (window),proportion,flag,border,userData);
  else throw wxe_badarg("window");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxSizerItem::CalcMin
void wxSizerItem_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->CalcMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizerItem::DeleteWindows
void wxSizerItem_DeleteWindows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DeleteWindows();

}

// wxSizerItem::DetachSizer
void wxSizerItem_DetachSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DetachSizer();

}

// wxSizerItem::GetBorder
void wxSizerItem_GetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBorder();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSizerItem::GetFlag
void wxSizerItem_GetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlag();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSizerItem::GetMinSize
void wxSizerItem_GetMinSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetMinSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizerItem::GetPosition
void wxSizerItem_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizerItem::GetProportion
void wxSizerItem_GetProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetProportion();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSizerItem::GetRatio
void wxSizerItem_GetRatio(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  float Result = This->GetRatio();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_double(Result));

}

// wxSizerItem::GetRect
void wxSizerItem_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizerItem::GetSize
void wxSizerItem_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizerItem::GetSizer
void wxSizerItem_GetSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizer * Result = (wxSizer*)This->GetSizer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizer"));

}

// wxSizerItem::GetSpacer
void wxSizerItem_GetSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSpacer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSizerItem::GetUserData
void wxSizerItem_GetUserData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxObject * Result = (wxObject*)This->GetUserData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxSizerItem::GetWindow
void wxSizerItem_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxSizerItem::IsSizer
void wxSizerItem_IsSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSizer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizerItem::IsShown
void wxSizerItem_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsShown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizerItem::IsSpacer
void wxSizerItem_IsSpacer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSpacer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizerItem::IsWindow
void wxSizerItem_IsWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSizerItem::SetBorder
void wxSizerItem_SetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int border;
  if(!enif_get_int(env, argv[1], &border)) Badarg("border"); // int
  This->SetBorder(border);

}

// wxSizerItem::SetDimension
void wxSizerItem_SetDimension(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->SetDimension(pos,size);

}

// wxSizerItem::SetFlag
void wxSizerItem_SetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int flag;
  if(!enif_get_int(env, argv[1], &flag)) Badarg("flag"); // int
  This->SetFlag(flag);

}

// wxSizerItem::SetInitSize
void wxSizerItem_SetInitSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  This->SetInitSize(x,y);

}

// wxSizerItem::SetMinSize
void wxSizerItem_SetMinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->SetMinSize(size);

}

// wxSizerItem::SetMinSize
void wxSizerItem_SetMinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  This->SetMinSize(x,y);

}

// wxSizerItem::SetProportion
void wxSizerItem_SetProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int proportion;
  if(!enif_get_int(env, argv[1], &proportion)) Badarg("proportion"); // int
  This->SetProportion(proportion);

}

// wxSizerItem::SetRatio
void wxSizerItem_SetRatio_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  This->SetRatio(width,height);

}

// wxSizerItem::SetRatio
void wxSizerItem_SetRatio_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->SetRatio(size);

}

// wxSizerItem::SetRatio
void wxSizerItem_SetRatio_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  float ratio;
  if(!wxe_get_float(env, argv[1], &ratio)) Badarg("ratio");
  This->SetRatio(ratio);

}

// wxSizerItem::AssignSizer
void wxSizerItem_AssignSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  This->AssignSizer(sizer);

}

// wxSizerItem::AssignSpacer
void wxSizerItem_AssignSpacer_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  This->AssignSpacer(size);

}

// wxSizerItem::AssignSpacer
void wxSizerItem_AssignSpacer_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int w;
  if(!enif_get_int(env, argv[1], &w)) Badarg("w"); // int
  int h;
  if(!enif_get_int(env, argv[2], &h)) Badarg("h"); // int
  This->AssignSpacer(w,h);

}

// wxSizerItem::AssignWindow
void wxSizerItem_AssignWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  This->AssignWindow(window);

}

// wxSizerItem::Show
void wxSizerItem_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  This->Show(show);

}

// wxSlider::wxSlider
void wxSlider_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSlider * Result = new EwxSlider();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSlider"));

}

// wxSlider::wxSlider
void wxSlider_new_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSL_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  int value;
  if(!enif_get_int(env, argv[2], &value)) Badarg("value"); // int
  int minValue;
  if(!enif_get_int(env, argv[3], &minValue)) Badarg("minValue"); // int
  int maxValue;
  if(!enif_get_int(env, argv[4], &maxValue)) Badarg("maxValue"); // int
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxSlider * Result = new EwxSlider(parent,id,value,minValue,maxValue,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSlider"));

}

// wxSlider::Create
void wxSlider_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSL_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  int value;
  if(!enif_get_int(env, argv[3], &value)) Badarg("value"); // int
  int minValue;
  if(!enif_get_int(env, argv[4], &minValue)) Badarg("minValue"); // int
  int maxValue;
  if(!enif_get_int(env, argv[5], &maxValue)) Badarg("maxValue"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[6];
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  bool Result = This->Create(parent,id,value,minValue,maxValue,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSlider::GetLineSize
void wxSlider_GetLineSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLineSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSlider::GetMax
void wxSlider_GetMax(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMax();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSlider::GetMin
void wxSlider_GetMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSlider::GetPageSize
void wxSlider_GetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSlider::GetThumbLength
void wxSlider_GetThumbLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetThumbLength();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSlider::GetValue
void wxSlider_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSlider::SetLineSize
void wxSlider_SetLineSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lineSize;
  if(!enif_get_int(env, argv[1], &lineSize)) Badarg("lineSize"); // int
  This->SetLineSize(lineSize);

}

// wxSlider::SetPageSize
void wxSlider_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pageSize;
  if(!enif_get_int(env, argv[1], &pageSize)) Badarg("pageSize"); // int
  This->SetPageSize(pageSize);

}

// wxSlider::SetRange
void wxSlider_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int minValue;
  if(!enif_get_int(env, argv[1], &minValue)) Badarg("minValue"); // int
  int maxValue;
  if(!enif_get_int(env, argv[2], &maxValue)) Badarg("maxValue"); // int
  This->SetRange(minValue,maxValue);

}

// wxSlider::SetThumbLength
void wxSlider_SetThumbLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int len;
  if(!enif_get_int(env, argv[1], &len)) Badarg("len"); // int
  This->SetThumbLength(len);

}

// wxSlider::SetValue
void wxSlider_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  This->SetValue(value);

}

// wxSpinButton::wxSpinButton
void wxSpinButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSpinButton * Result = new EwxSpinButton();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSpinButton"));

}

// wxSpinButton::wxSpinButton
void wxSpinButton_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=-1;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSP_VERTICAL;
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
  wxSpinButton * Result = new EwxSpinButton(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSpinButton"));

}

// wxSpinButton::Create
void wxSpinButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=-1;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSP_VERTICAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSpinButton::GetMax
void wxSpinButton_GetMax(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMax();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinButton::GetMin
void wxSpinButton_GetMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinButton::GetValue
void wxSpinButton_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinButton::SetRange
void wxSpinButton_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  int max;
  if(!enif_get_int(env, argv[2], &max)) Badarg("max"); // int
  This->SetRange(min,max);

}

// wxSpinButton::SetValue
void wxSpinButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  This->SetValue(value);

}

// wxSpinCtrl::wxSpinCtrl
void wxSpinCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSpinCtrl * Result = new EwxSpinCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSpinCtrl"));

}

// wxSpinCtrl::wxSpinCtrl
void wxSpinCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxString value= wxEmptyString;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSP_ARROW_KEYS;
  int min=0;
  int max=100;
  int initial=0;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "value"))) {
  ErlNifBinary value_bin;
  if(!enif_inspect_binary(env, tpl[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "min"))) {
  if(!enif_get_int(env, tpl[1], &min)) Badarg("min"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "max"))) {
  if(!enif_get_int(env, tpl[1], &max)) Badarg("max"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "initial"))) {
  if(!enif_get_int(env, tpl[1], &initial)) Badarg("initial"); // int
    } else        Badarg("Options");
  };
  wxSpinCtrl * Result = new EwxSpinCtrl(parent,id,value,pos,size,style,min,max,initial);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSpinCtrl"));

}

// wxSpinCtrl::Create
void wxSpinCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxString value= wxEmptyString;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSP_ARROW_KEYS;
  int min=0;
  int max=100;
  int initial=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "value"))) {
  ErlNifBinary value_bin;
  if(!enif_inspect_binary(env, tpl[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "min"))) {
  if(!enif_get_int(env, tpl[1], &min)) Badarg("min"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "max"))) {
  if(!enif_get_int(env, tpl[1], &max)) Badarg("max"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "initial"))) {
  if(!enif_get_int(env, tpl[1], &initial)) Badarg("initial"); // int
    } else        Badarg("Options");
  };
  bool Result = This->Create(parent,id,value,pos,size,style,min,max,initial);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSpinCtrl::SetValue
void wxSpinCtrl_SetValue_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->SetValue(text);

}

// wxSpinCtrl::SetValue
void wxSpinCtrl_SetValue_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  This->SetValue(value);

}

// wxSpinCtrl::GetValue
void wxSpinCtrl_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinCtrl::SetRange
void wxSpinCtrl_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int minVal;
  if(!enif_get_int(env, argv[1], &minVal)) Badarg("minVal"); // int
  int maxVal;
  if(!enif_get_int(env, argv[2], &maxVal)) Badarg("maxVal"); // int
  This->SetRange(minVal,maxVal);

}

// wxSpinCtrl::SetSelection
void wxSpinCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  This->SetSelection(from,to);

}

// wxSpinCtrl::GetMin
void wxSpinCtrl_GetMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinCtrl::GetMax
void wxSpinCtrl_GetMax(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMax();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinEvent::GetPosition
void wxSpinEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinEvent *This;
  This = (wxSpinEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSpinEvent::SetPosition
void wxSpinEvent_SetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinEvent *This;
  This = (wxSpinEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  This->SetPosition(pos);

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

// wxSplitterEvent::GetSashPosition
void wxSplitterEvent_GetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterEvent *This;
  This = (wxSplitterEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSashPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplitterEvent::GetX
void wxSplitterEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterEvent *This;
  This = (wxSplitterEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplitterEvent::GetY
void wxSplitterEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterEvent *This;
  This = (wxSplitterEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplitterEvent::GetWindowBeingRemoved
void wxSplitterEvent_GetWindowBeingRemoved(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterEvent *This;
  This = (wxSplitterEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindowBeingRemoved();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxSplitterEvent::SetSashPosition
void wxSplitterEvent_SetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterEvent *This;
  This = (wxSplitterEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  This->SetSashPosition(pos);

}

// wxSplitterWindow::wxSplitterWindow
void wxSplitterWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSplitterWindow * Result = new EwxSplitterWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSplitterWindow"));

}

// wxSplitterWindow::wxSplitterWindow
void wxSplitterWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSP_3D;
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
  wxSplitterWindow * Result = new EwxSplitterWindow(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSplitterWindow"));

}

// wxSplitterWindow::Create
void wxSplitterWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxSP_3D;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplitterWindow::GetMinimumPaneSize
void wxSplitterWindow_GetMinimumPaneSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMinimumPaneSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplitterWindow::GetSashGravity
void wxSplitterWindow_GetSashGravity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  double Result = This->GetSashGravity();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_double(Result));

}

// wxSplitterWindow::GetSashPosition
void wxSplitterWindow_GetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSashPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplitterWindow::GetSplitMode
void wxSplitterWindow_GetSplitMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSplitMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSplitterWindow::GetWindow1
void wxSplitterWindow_GetWindow1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow1();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxSplitterWindow::GetWindow2
void wxSplitterWindow_GetWindow2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow2();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxSplitterWindow::Initialize
void wxSplitterWindow_Initialize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  This->Initialize(window);

}

// wxSplitterWindow::IsSplit
void wxSplitterWindow_IsSplit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSplit();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplitterWindow::ReplaceWindow
void wxSplitterWindow_ReplaceWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *winOld;
  winOld = (wxWindow *) memenv->getPtr(env, argv[1], "winOld");
  wxWindow *winNew;
  winNew = (wxWindow *) memenv->getPtr(env, argv[2], "winNew");
  bool Result = This->ReplaceWindow(winOld,winNew);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplitterWindow::SetSashGravity
void wxSplitterWindow_SetSashGravity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  double gravity;
  if(!wxe_get_double(env, argv[1], &gravity)) Badarg("gravity");
  This->SetSashGravity(gravity);

}

// wxSplitterWindow::SetSashPosition
void wxSplitterWindow_SetSashPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool redraw=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int position;
  if(!enif_get_int(env, argv[1], &position)) Badarg("position"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "redraw"))) {
  redraw = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  This->SetSashPosition(position,redraw);

}

// wxSplitterWindow::SetMinimumPaneSize
void wxSplitterWindow_SetMinimumPaneSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int paneSize;
  if(!enif_get_int(env, argv[1], &paneSize)) Badarg("paneSize"); // int
  This->SetMinimumPaneSize(paneSize);

}

// wxSplitterWindow::SetSplitMode
void wxSplitterWindow_SetSplitMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int mode;
  if(!enif_get_int(env, argv[1], &mode)) Badarg("mode"); // int
  This->SetSplitMode(mode);

}

// wxSplitterWindow::SplitHorizontally
void wxSplitterWindow_SplitHorizontally(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int sashPosition=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window1;
  window1 = (wxWindow *) memenv->getPtr(env, argv[1], "window1");
  wxWindow *window2;
  window2 = (wxWindow *) memenv->getPtr(env, argv[2], "window2");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sashPosition"))) {
  if(!enif_get_int(env, tpl[1], &sashPosition)) Badarg("sashPosition"); // int
    } else        Badarg("Options");
  };
  bool Result = This->SplitHorizontally(window1,window2,sashPosition);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplitterWindow::SplitVertically
void wxSplitterWindow_SplitVertically(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int sashPosition=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *window1;
  window1 = (wxWindow *) memenv->getPtr(env, argv[1], "window1");
  wxWindow *window2;
  window2 = (wxWindow *) memenv->getPtr(env, argv[2], "window2");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sashPosition"))) {
  if(!enif_get_int(env, tpl[1], &sashPosition)) Badarg("sashPosition"); // int
    } else        Badarg("Options");
  };
  bool Result = This->SplitVertically(window1,window2,sashPosition);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplitterWindow::Unsplit
void wxSplitterWindow_Unsplit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * toRemove=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "toRemove"))) {
  toRemove = (wxWindow *) memenv->getPtr(env, tpl[1], "toRemove");
    } else        Badarg("Options");
  };
  bool Result = This->Unsplit(toRemove);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSplitterWindow::UpdateSize
void wxSplitterWindow_UpdateSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->UpdateSize();

}

// wxStaticBitmap::wxStaticBitmap
void wxStaticBitmap_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStaticBitmap * Result = new EwxStaticBitmap();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBitmap"));

}

// wxStaticBitmap::wxStaticBitmap
void wxStaticBitmap_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxBitmap *label;
  label = (wxBitmap *) memenv->getPtr(env, argv[2], "label");
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
  wxStaticBitmap * Result = new EwxStaticBitmap(parent,id,*label,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBitmap"));

}

// wxStaticBitmap::Create
void wxStaticBitmap_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticBitmap *This;
  This = (wxStaticBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  wxBitmap *label;
  label = (wxBitmap *) memenv->getPtr(env, argv[3], "label");
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
  bool Result = This->Create(parent,id,*label,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStaticBitmap::GetBitmap
void wxStaticBitmap_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticBitmap *This;
  This = (wxStaticBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmap()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxStaticBitmap::SetBitmap
void wxStaticBitmap_SetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticBitmap *This;
  This = (wxStaticBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap *label;
  label = (wxBitmap *) memenv->getPtr(env, argv[1], "label");
  This->SetBitmap(*label);

}

// wxStaticBox::wxStaticBox
void wxStaticBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStaticBox * Result = new EwxStaticBox();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBox"));

}

// wxStaticBox::wxStaticBox
void wxStaticBox_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
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
  wxStaticBox * Result = new EwxStaticBox(parent,id,label,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBox"));

}

// wxStaticBox::Create
void wxStaticBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticBox *This;
  This = (wxStaticBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[3], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
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
  bool Result = This->Create(parent,id,label,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStaticBoxSizer::wxStaticBoxSizer
void wxStaticBoxSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticBox *box;
  box = (wxStaticBox *) memenv->getPtr(env, argv[0], "box");
  int orient;
  if(!enif_get_int(env, argv[1], &orient)) Badarg("orient"); // int
  wxStaticBoxSizer * Result = new EwxStaticBoxSizer(box,orient);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBoxSizer"));

}

// wxStaticBoxSizer::wxStaticBoxSizer
void wxStaticBoxSizer_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString label= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int orient;
  if(!enif_get_int(env, argv[0], &orient)) Badarg("orient"); // int
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "label"))) {
  ErlNifBinary label_bin;
  if(!enif_inspect_binary(env, tpl[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
    } else        Badarg("Options");
  };
  wxStaticBoxSizer * Result = new EwxStaticBoxSizer(orient,parent,label);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBoxSizer"));

}

// wxStaticBoxSizer::GetStaticBox
void wxStaticBoxSizer_GetStaticBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticBoxSizer *This;
  This = (wxStaticBoxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxStaticBox * Result = (wxStaticBox*)This->GetStaticBox();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticBox"));

}

// wxStaticLine::wxStaticLine
void wxStaticLine_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStaticLine * Result = new EwxStaticLine();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticLine"));

}

// wxStaticLine::wxStaticLine
void wxStaticLine_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxLI_HORIZONTAL;
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
  wxStaticLine * Result = new EwxStaticLine(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticLine"));

}

// wxStaticLine::Create
void wxStaticLine_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxLI_HORIZONTAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticLine *This;
  This = (wxStaticLine *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStaticLine::IsVertical
void wxStaticLine_IsVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticLine *This;
  This = (wxStaticLine *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVertical();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStaticLine::GetDefaultSize
void wxStaticLine_GetDefaultSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxStaticLine::GetDefaultSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStaticText::wxStaticText
void wxStaticText_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStaticText * Result = new EwxStaticText();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticText"));

}

// wxStaticText::wxStaticText
void wxStaticText_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
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
  wxStaticText * Result = new EwxStaticText(parent,id,label,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStaticText"));

}

// wxStaticText::Create
void wxStaticText_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticText *This;
  This = (wxStaticText *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[3], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
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
  bool Result = This->Create(parent,id,label,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStaticText::GetLabel
void wxStaticText_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticText *This;
  This = (wxStaticText *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetLabel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStaticText::SetLabel
void wxStaticText_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticText *This;
  This = (wxStaticText *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  This->SetLabel(label);

}

// wxStaticText::Wrap
void wxStaticText_Wrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticText *This;
  This = (wxStaticText *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  This->Wrap(width);

}

// wxStatusBar::wxStatusBar
void wxStatusBar_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStatusBar * Result = new EwxStatusBar();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStatusBar"));

}

// wxStatusBar::wxStatusBar
void wxStatusBar_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID winid=wxID_ANY;
  long style=wxSTB_DEFAULT_STYLE;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxStatusBar * Result = new EwxStatusBar(parent,winid,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStatusBar"));

}

// wxStatusBar::Create
void wxStatusBar_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID winid=wxID_ANY;
  long style=wxSTB_DEFAULT_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "winid"))) {
  if(!enif_get_int(env, tpl[1], &winid)) Badarg("winid"); // wxWindowID
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  bool Result = This->Create(parent,winid,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStatusBar::GetFieldRect
void wxStatusBar_GetFieldRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRect rect;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int i;
  if(!enif_get_int(env, argv[1], &i)) Badarg("i"); // int
  bool Result = This->GetFieldRect(i,rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(rect));
  rt.send(msg);

}

// wxStatusBar::GetFieldsCount
void wxStatusBar_GetFieldsCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFieldsCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStatusBar::GetStatusText
void wxStatusBar_GetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
    } else        Badarg("Options");
  };
  wxString Result = This->GetStatusText(number);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStatusBar::PopStatusText
void wxStatusBar_PopStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
    } else        Badarg("Options");
  };
  This->PopStatusText(number);

}

// wxStatusBar::PushStatusText
void wxStatusBar_PushStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "number"))) {
  if(!enif_get_int(env, tpl[1], &number)) Badarg("number"); // int
    } else        Badarg("Options");
  };
  This->PushStatusText(string,number);

}

// wxStatusBar::SetFieldsCount
void wxStatusBar_SetFieldsCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int widthsLen;
  std::vector <int> widths;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int number;
  if(!enif_get_int(env, argv[1], &number)) Badarg("number"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "widths"))) {
  int widths_tmp;
  ERL_NIF_TERM widthsHead, widthsTail;
  if(!enif_get_list_length(env, tpl[1], &widthsLen)) Badarg("widths");
  widthsTail = tpl[1];
  while(!enif_is_empty_list(env, widthsTail)) {
    if(!enif_get_list_cell(env, widthsTail, &widthsHead, &widthsTail)) Badarg("widths");
    if(!enif_get_int(env, widthsHead, &widths_tmp)) Badarg("widths");
    widths.push_back( (int) widths_tmp);
  };
    } else        Badarg("Options");
  };
 if(!widths.empty() && (int)widthsLen != number) Badarg("widths");
  This->SetFieldsCount(number, widths.empty() ? (int *) NULL : widths.data());

}

// wxStatusBar::SetMinHeight
void wxStatusBar_SetMinHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  This->SetMinHeight(height);

}

// wxStatusBar::SetStatusText
void wxStatusBar_SetStatusText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int number=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
  This->SetStatusText(text,number);

}

// wxStatusBar::SetStatusWidths
void wxStatusBar_SetStatusWidths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
  This->SetStatusWidths(widths_fieldLen,widths_field.data());

}

// wxStatusBar::SetStatusStyles
void wxStatusBar_SetStatusStyles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int styles_tmp;
  unsigned int stylesLen;
  ERL_NIF_TERM stylesHead, stylesTail;
  if(!enif_get_list_length(env, argv[1], &stylesLen)) Badarg("styles");
  std::vector <int> styles;
  stylesTail = argv[1];
  while(!enif_is_empty_list(env, stylesTail)) {
    if(!enif_get_list_cell(env, stylesTail, &stylesHead, &stylesTail)) Badarg("styles");
    if(!enif_get_int(env, stylesHead, &styles_tmp)) Badarg("styles");
    styles.push_back( (int) styles_tmp);
  };
  This->SetStatusStyles(stylesLen,styles.data());

}

// wxStdDialogButtonSizer::wxStdDialogButtonSizer
void wxStdDialogButtonSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStdDialogButtonSizer * Result = new EwxStdDialogButtonSizer();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStdDialogButtonSizer"));

}

// wxStdDialogButtonSizer::AddButton
void wxStdDialogButtonSizer_AddButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  This->AddButton(button);

}

// wxStdDialogButtonSizer::Realize
void wxStdDialogButtonSizer_Realize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Realize();

}

// wxStdDialogButtonSizer::SetAffirmativeButton
void wxStdDialogButtonSizer_SetAffirmativeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  This->SetAffirmativeButton(button);

}

// wxStdDialogButtonSizer::SetCancelButton
void wxStdDialogButtonSizer_SetCancelButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  This->SetCancelButton(button);

}

// wxStdDialogButtonSizer::SetNegativeButton
void wxStdDialogButtonSizer_SetNegativeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  This->SetNegativeButton(button);

}

// wxStyledTextCtrl::wxStyledTextCtrl
void wxStyledTextCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
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
  wxStyledTextCtrl * Result = new EwxStyledTextCtrl(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStyledTextCtrl"));

}

// wxStyledTextCtrl::wxStyledTextCtrl
void wxStyledTextCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxStyledTextCtrl * Result = new EwxStyledTextCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStyledTextCtrl"));

}

// wxStyledTextCtrl::Create
void wxStyledTextCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
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
  bool Result = This->Create(parent,id,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AddText
void wxStyledTextCtrl_AddText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->AddText(text);

}

// wxStyledTextCtrl::InsertText
void wxStyledTextCtrl_InsertText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->InsertText(pos,text);

}

// wxStyledTextCtrl::ClearAll
void wxStyledTextCtrl_ClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearAll();

}

// wxStyledTextCtrl::ClearDocumentStyle
void wxStyledTextCtrl_ClearDocumentStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearDocumentStyle();

}

// wxStyledTextCtrl::GetLength
void wxStyledTextCtrl_GetLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLength();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCharAt
void wxStyledTextCtrl_GetCharAt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->GetCharAt(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCurrentPos
void wxStyledTextCtrl_GetCurrentPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCurrentPos();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetAnchor
void wxStyledTextCtrl_GetAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetAnchor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetStyleAt
void wxStyledTextCtrl_GetStyleAt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->GetStyleAt(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::Redo
void wxStyledTextCtrl_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Redo();

}

// wxStyledTextCtrl::SetUndoCollection
void wxStyledTextCtrl_SetUndoCollection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool collectUndo;
  collectUndo = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetUndoCollection(collectUndo);

}

// wxStyledTextCtrl::SelectAll
void wxStyledTextCtrl_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SelectAll();

}

// wxStyledTextCtrl::SetSavePoint
void wxStyledTextCtrl_SetSavePoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetSavePoint();

}

// wxStyledTextCtrl::CanRedo
void wxStyledTextCtrl_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanRedo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::MarkerLineFromHandle
void wxStyledTextCtrl_MarkerLineFromHandle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerHandle;
  if(!enif_get_int(env, argv[1], &markerHandle)) Badarg("markerHandle"); // int
  int Result = This->MarkerLineFromHandle(markerHandle);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::MarkerDeleteHandle
void wxStyledTextCtrl_MarkerDeleteHandle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerHandle;
  if(!enif_get_int(env, argv[1], &markerHandle)) Badarg("markerHandle"); // int
  This->MarkerDeleteHandle(markerHandle);

}

// wxStyledTextCtrl::GetUndoCollection
void wxStyledTextCtrl_GetUndoCollection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUndoCollection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::GetViewWhiteSpace
void wxStyledTextCtrl_GetViewWhiteSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetViewWhiteSpace();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetViewWhiteSpace
void wxStyledTextCtrl_SetViewWhiteSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int viewWS;
  if(!enif_get_int(env, argv[1], &viewWS)) Badarg("viewWS"); // int
  This->SetViewWhiteSpace(viewWS);

}

// wxStyledTextCtrl::PositionFromPoint
void wxStyledTextCtrl_PositionFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  int Result = This->PositionFromPoint(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::PositionFromPointClose
void wxStyledTextCtrl_PositionFromPointClose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  int Result = This->PositionFromPointClose(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GotoLine
void wxStyledTextCtrl_GotoLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  This->GotoLine(line);

}

// wxStyledTextCtrl::GotoPos
void wxStyledTextCtrl_GotoPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int caret;
  if(!enif_get_int(env, argv[1], &caret)) Badarg("caret"); // int
  This->GotoPos(caret);

}

// wxStyledTextCtrl::SetAnchor
void wxStyledTextCtrl_SetAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int anchor;
  if(!enif_get_int(env, argv[1], &anchor)) Badarg("anchor"); // int
  This->SetAnchor(anchor);

}

// wxStyledTextCtrl::GetCurLine
void wxStyledTextCtrl_GetCurLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int linePos;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCurLine(&linePos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make(Result),
    rt.make_int(linePos));
  rt.send(msg);

}

// wxStyledTextCtrl::GetEndStyled
void wxStyledTextCtrl_GetEndStyled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetEndStyled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::ConvertEOLs
void wxStyledTextCtrl_ConvertEOLs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int eolMode;
  if(!enif_get_int(env, argv[1], &eolMode)) Badarg("eolMode"); // int
  This->ConvertEOLs(eolMode);

}

// wxStyledTextCtrl::GetEOLMode
void wxStyledTextCtrl_GetEOLMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetEOLMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetEOLMode
void wxStyledTextCtrl_SetEOLMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int eolMode;
  if(!enif_get_int(env, argv[1], &eolMode)) Badarg("eolMode"); // int
  This->SetEOLMode(eolMode);

}

// wxStyledTextCtrl::StartStyling
void wxStyledTextCtrl_StartStyling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int unused=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int start;
  if(!enif_get_int(env, argv[1], &start)) Badarg("start"); // int
  This->StartStyling(start,unused);

}

// wxStyledTextCtrl::SetStyling
void wxStyledTextCtrl_SetStyling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int length;
  if(!enif_get_int(env, argv[1], &length)) Badarg("length"); // int
  int style;
  if(!enif_get_int(env, argv[2], &style)) Badarg("style"); // int
  This->SetStyling(length,style);

}

// wxStyledTextCtrl::GetBufferedDraw
void wxStyledTextCtrl_GetBufferedDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetBufferedDraw();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetBufferedDraw
void wxStyledTextCtrl_SetBufferedDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool buffered;
  buffered = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetBufferedDraw(buffered);

}

// wxStyledTextCtrl::SetTabWidth
void wxStyledTextCtrl_SetTabWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int tabWidth;
  if(!enif_get_int(env, argv[1], &tabWidth)) Badarg("tabWidth"); // int
  This->SetTabWidth(tabWidth);

}

// wxStyledTextCtrl::GetTabWidth
void wxStyledTextCtrl_GetTabWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetTabWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetCodePage
void wxStyledTextCtrl_SetCodePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int codePage;
  if(!enif_get_int(env, argv[1], &codePage)) Badarg("codePage"); // int
  This->SetCodePage(codePage);

}

// wxStyledTextCtrl::MarkerDefine
void wxStyledTextCtrl_MarkerDefine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour foreground= wxNullColour;
  wxColour background= wxNullColour;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerNumber;
  if(!enif_get_int(env, argv[1], &markerNumber)) Badarg("markerNumber"); // int
  int markerSymbol;
  if(!enif_get_int(env, argv[2], &markerSymbol)) Badarg("markerSymbol"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "foreground"))) {
  const ERL_NIF_TERM *foreground_t;
  int foreground_sz;
  if(!enif_get_tuple(env, tpl[1], &foreground_sz, &foreground_t)) Badarg("foreground");
  int foregroundR;
  if(!enif_get_int(env, foreground_t[0], &foregroundR)) Badarg("foreground");
  int foregroundG;
  if(!enif_get_int(env, foreground_t[1], &foregroundG)) Badarg("foreground");
  int foregroundB;
  if(!enif_get_int(env, foreground_t[2], &foregroundB)) Badarg("foreground");
  int foregroundA;
  if(!enif_get_int(env, foreground_t[3], &foregroundA)) Badarg("foreground");
  foreground = wxColour(foregroundR,foregroundG,foregroundB,foregroundA);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "background"))) {
  const ERL_NIF_TERM *background_t;
  int background_sz;
  if(!enif_get_tuple(env, tpl[1], &background_sz, &background_t)) Badarg("background");
  int backgroundR;
  if(!enif_get_int(env, background_t[0], &backgroundR)) Badarg("background");
  int backgroundG;
  if(!enif_get_int(env, background_t[1], &backgroundG)) Badarg("background");
  int backgroundB;
  if(!enif_get_int(env, background_t[2], &backgroundB)) Badarg("background");
  int backgroundA;
  if(!enif_get_int(env, background_t[3], &backgroundA)) Badarg("background");
  background = wxColour(backgroundR,backgroundG,backgroundB,backgroundA);
    } else        Badarg("Options");
  };
  This->MarkerDefine(markerNumber,markerSymbol,foreground,background);

}

// wxStyledTextCtrl::MarkerSetForeground
void wxStyledTextCtrl_MarkerSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerNumber;
  if(!enif_get_int(env, argv[1], &markerNumber)) Badarg("markerNumber"); // int
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->MarkerSetForeground(markerNumber,fore);

}

// wxStyledTextCtrl::MarkerSetBackground
void wxStyledTextCtrl_MarkerSetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerNumber;
  if(!enif_get_int(env, argv[1], &markerNumber)) Badarg("markerNumber"); // int
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[2], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->MarkerSetBackground(markerNumber,back);

}

// wxStyledTextCtrl::MarkerAdd
void wxStyledTextCtrl_MarkerAdd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int markerNumber;
  if(!enif_get_int(env, argv[2], &markerNumber)) Badarg("markerNumber"); // int
  int Result = This->MarkerAdd(line,markerNumber);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::MarkerDelete
void wxStyledTextCtrl_MarkerDelete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int markerNumber;
  if(!enif_get_int(env, argv[2], &markerNumber)) Badarg("markerNumber"); // int
  This->MarkerDelete(line,markerNumber);

}

// wxStyledTextCtrl::MarkerDeleteAll
void wxStyledTextCtrl_MarkerDeleteAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerNumber;
  if(!enif_get_int(env, argv[1], &markerNumber)) Badarg("markerNumber"); // int
  This->MarkerDeleteAll(markerNumber);

}

// wxStyledTextCtrl::MarkerGet
void wxStyledTextCtrl_MarkerGet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->MarkerGet(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::MarkerNext
void wxStyledTextCtrl_MarkerNext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lineStart;
  if(!enif_get_int(env, argv[1], &lineStart)) Badarg("lineStart"); // int
  int markerMask;
  if(!enif_get_int(env, argv[2], &markerMask)) Badarg("markerMask"); // int
  int Result = This->MarkerNext(lineStart,markerMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::MarkerPrevious
void wxStyledTextCtrl_MarkerPrevious(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lineStart;
  if(!enif_get_int(env, argv[1], &lineStart)) Badarg("lineStart"); // int
  int markerMask;
  if(!enif_get_int(env, argv[2], &markerMask)) Badarg("markerMask"); // int
  int Result = This->MarkerPrevious(lineStart,markerMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::MarkerDefineBitmap
void wxStyledTextCtrl_MarkerDefineBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerNumber;
  if(!enif_get_int(env, argv[1], &markerNumber)) Badarg("markerNumber"); // int
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[2], "bmp");
  This->MarkerDefineBitmap(markerNumber,*bmp);

}

// wxStyledTextCtrl::MarkerAddSet
void wxStyledTextCtrl_MarkerAddSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int markerSet;
  if(!enif_get_int(env, argv[2], &markerSet)) Badarg("markerSet"); // int
  This->MarkerAddSet(line,markerSet);

}

// wxStyledTextCtrl::MarkerSetAlpha
void wxStyledTextCtrl_MarkerSetAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int markerNumber;
  if(!enif_get_int(env, argv[1], &markerNumber)) Badarg("markerNumber"); // int
  int alpha;
  if(!enif_get_int(env, argv[2], &alpha)) Badarg("alpha"); // int
  This->MarkerSetAlpha(markerNumber,alpha);

}

// wxStyledTextCtrl::SetMarginType
void wxStyledTextCtrl_SetMarginType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  int marginType;
  if(!enif_get_int(env, argv[2], &marginType)) Badarg("marginType"); // int
  This->SetMarginType(margin,marginType);

}

// wxStyledTextCtrl::GetMarginType
void wxStyledTextCtrl_GetMarginType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  int Result = This->GetMarginType(margin);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetMarginWidth
void wxStyledTextCtrl_SetMarginWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  int pixelWidth;
  if(!enif_get_int(env, argv[2], &pixelWidth)) Badarg("pixelWidth"); // int
  This->SetMarginWidth(margin,pixelWidth);

}

// wxStyledTextCtrl::GetMarginWidth
void wxStyledTextCtrl_GetMarginWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  int Result = This->GetMarginWidth(margin);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetMarginMask
void wxStyledTextCtrl_SetMarginMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  int mask;
  if(!enif_get_int(env, argv[2], &mask)) Badarg("mask"); // int
  This->SetMarginMask(margin,mask);

}

// wxStyledTextCtrl::GetMarginMask
void wxStyledTextCtrl_GetMarginMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  int Result = This->GetMarginMask(margin);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetMarginSensitive
void wxStyledTextCtrl_SetMarginSensitive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  bool sensitive;
  sensitive = enif_is_identical(argv[2], WXE_ATOM_true);
  This->SetMarginSensitive(margin,sensitive);

}

// wxStyledTextCtrl::GetMarginSensitive
void wxStyledTextCtrl_GetMarginSensitive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  bool Result = This->GetMarginSensitive(margin);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::StyleClearAll
void wxStyledTextCtrl_StyleClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StyleClearAll();

}

// wxStyledTextCtrl::StyleSetForeground
void wxStyledTextCtrl_StyleSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->StyleSetForeground(style,fore);

}

// wxStyledTextCtrl::StyleSetBackground
void wxStyledTextCtrl_StyleSetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[2], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->StyleSetBackground(style,back);

}

// wxStyledTextCtrl::StyleSetBold
void wxStyledTextCtrl_StyleSetBold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  bool bold;
  bold = enif_is_identical(argv[2], WXE_ATOM_true);
  This->StyleSetBold(style,bold);

}

// wxStyledTextCtrl::StyleSetItalic
void wxStyledTextCtrl_StyleSetItalic(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  bool italic;
  italic = enif_is_identical(argv[2], WXE_ATOM_true);
  This->StyleSetItalic(style,italic);

}

// wxStyledTextCtrl::StyleSetSize
void wxStyledTextCtrl_StyleSetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  int sizePoints;
  if(!enif_get_int(env, argv[2], &sizePoints)) Badarg("sizePoints"); // int
  This->StyleSetSize(style,sizePoints);

}

// wxStyledTextCtrl::StyleSetFaceName
void wxStyledTextCtrl_StyleSetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  ErlNifBinary fontName_bin;
  wxString fontName;
  if(!enif_inspect_binary(env, argv[2], &fontName_bin)) Badarg("fontName");
  fontName = wxString(fontName_bin.data, wxConvUTF8, fontName_bin.size);
  This->StyleSetFaceName(style,fontName);

}

// wxStyledTextCtrl::StyleSetEOLFilled
void wxStyledTextCtrl_StyleSetEOLFilled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  bool eolFilled;
  eolFilled = enif_is_identical(argv[2], WXE_ATOM_true);
  This->StyleSetEOLFilled(style,eolFilled);

}

// wxStyledTextCtrl::StyleResetDefault
void wxStyledTextCtrl_StyleResetDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StyleResetDefault();

}

// wxStyledTextCtrl::StyleSetUnderline
void wxStyledTextCtrl_StyleSetUnderline(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  bool underline;
  underline = enif_is_identical(argv[2], WXE_ATOM_true);
  This->StyleSetUnderline(style,underline);

}

// wxStyledTextCtrl::StyleSetCase
void wxStyledTextCtrl_StyleSetCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  int caseVisible;
  if(!enif_get_int(env, argv[2], &caseVisible)) Badarg("caseVisible"); // int
  This->StyleSetCase(style,caseVisible);

}

// wxStyledTextCtrl::StyleSetHotSpot
void wxStyledTextCtrl_StyleSetHotSpot(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  bool hotspot;
  hotspot = enif_is_identical(argv[2], WXE_ATOM_true);
  This->StyleSetHotSpot(style,hotspot);

}

// wxStyledTextCtrl::SetSelForeground
void wxStyledTextCtrl_SetSelForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->SetSelForeground(useSetting,fore);

}

// wxStyledTextCtrl::SetSelBackground
void wxStyledTextCtrl_SetSelBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[2], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->SetSelBackground(useSetting,back);

}

// wxStyledTextCtrl::GetSelAlpha
void wxStyledTextCtrl_GetSelAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelAlpha();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetSelAlpha
void wxStyledTextCtrl_SetSelAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int alpha;
  if(!enif_get_int(env, argv[1], &alpha)) Badarg("alpha"); // int
  This->SetSelAlpha(alpha);

}

// wxStyledTextCtrl::SetCaretForeground
void wxStyledTextCtrl_SetCaretForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[1], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->SetCaretForeground(fore);

}

// wxStyledTextCtrl::CmdKeyAssign
void wxStyledTextCtrl_CmdKeyAssign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int key;
  if(!enif_get_int(env, argv[1], &key)) Badarg("key"); // int
  int modifiers;
  if(!enif_get_int(env, argv[2], &modifiers)) Badarg("modifiers"); // int
  int cmd;
  if(!enif_get_int(env, argv[3], &cmd)) Badarg("cmd"); // int
  This->CmdKeyAssign(key,modifiers,cmd);

}

// wxStyledTextCtrl::CmdKeyClear
void wxStyledTextCtrl_CmdKeyClear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int key;
  if(!enif_get_int(env, argv[1], &key)) Badarg("key"); // int
  int modifiers;
  if(!enif_get_int(env, argv[2], &modifiers)) Badarg("modifiers"); // int
  This->CmdKeyClear(key,modifiers);

}

// wxStyledTextCtrl::CmdKeyClearAll
void wxStyledTextCtrl_CmdKeyClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CmdKeyClearAll();

}

// wxStyledTextCtrl::SetStyleBytes
void wxStyledTextCtrl_SetStyleBytes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  char styleBytes;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int length;
  if(!enif_get_int(env, argv[1], &length)) Badarg("length"); // int
  This->SetStyleBytes(length,&styleBytes);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(styleBytes));

}

// wxStyledTextCtrl::StyleSetVisible
void wxStyledTextCtrl_StyleSetVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  bool visible;
  visible = enif_is_identical(argv[2], WXE_ATOM_true);
  This->StyleSetVisible(style,visible);

}

// wxStyledTextCtrl::GetCaretPeriod
void wxStyledTextCtrl_GetCaretPeriod(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCaretPeriod();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetCaretPeriod
void wxStyledTextCtrl_SetCaretPeriod(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int periodMilliseconds;
  if(!enif_get_int(env, argv[1], &periodMilliseconds)) Badarg("periodMilliseconds"); // int
  This->SetCaretPeriod(periodMilliseconds);

}

// wxStyledTextCtrl::SetWordChars
void wxStyledTextCtrl_SetWordChars(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary characters_bin;
  wxString characters;
  if(!enif_inspect_binary(env, argv[1], &characters_bin)) Badarg("characters");
  characters = wxString(characters_bin.data, wxConvUTF8, characters_bin.size);
  This->SetWordChars(characters);

}

// wxStyledTextCtrl::BeginUndoAction
void wxStyledTextCtrl_BeginUndoAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->BeginUndoAction();

}

// wxStyledTextCtrl::EndUndoAction
void wxStyledTextCtrl_EndUndoAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EndUndoAction();

}

// wxStyledTextCtrl::IndicatorSetStyle
void wxStyledTextCtrl_IndicatorSetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indicator;
  if(!enif_get_int(env, argv[1], &indicator)) Badarg("indicator"); // int
  int indicatorStyle;
  if(!enif_get_int(env, argv[2], &indicatorStyle)) Badarg("indicatorStyle"); // int
  This->IndicatorSetStyle(indicator,indicatorStyle);

}

// wxStyledTextCtrl::IndicatorGetStyle
void wxStyledTextCtrl_IndicatorGetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indicator;
  if(!enif_get_int(env, argv[1], &indicator)) Badarg("indicator"); // int
  int Result = This->IndicatorGetStyle(indicator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::IndicatorSetForeground
void wxStyledTextCtrl_IndicatorSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indicator;
  if(!enif_get_int(env, argv[1], &indicator)) Badarg("indicator"); // int
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->IndicatorSetForeground(indicator,fore);

}

// wxStyledTextCtrl::IndicatorGetForeground
void wxStyledTextCtrl_IndicatorGetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indicator;
  if(!enif_get_int(env, argv[1], &indicator)) Badarg("indicator"); // int
  wxColour Result = This->IndicatorGetForeground(indicator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::SetWhitespaceForeground
void wxStyledTextCtrl_SetWhitespaceForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->SetWhitespaceForeground(useSetting,fore);

}

// wxStyledTextCtrl::SetWhitespaceBackground
void wxStyledTextCtrl_SetWhitespaceBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[2], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->SetWhitespaceBackground(useSetting,back);

}

// wxStyledTextCtrl::GetStyleBits
void wxStyledTextCtrl_GetStyleBits(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStyleBits();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetLineState
void wxStyledTextCtrl_SetLineState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int state;
  if(!enif_get_int(env, argv[2], &state)) Badarg("state"); // int
  This->SetLineState(line,state);

}

// wxStyledTextCtrl::GetLineState
void wxStyledTextCtrl_GetLineState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->GetLineState(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetMaxLineState
void wxStyledTextCtrl_GetMaxLineState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaxLineState();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCaretLineVisible
void wxStyledTextCtrl_GetCaretLineVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetCaretLineVisible();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetCaretLineVisible
void wxStyledTextCtrl_SetCaretLineVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetCaretLineVisible(show);

}

// wxStyledTextCtrl::GetCaretLineBackground
void wxStyledTextCtrl_GetCaretLineBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCaretLineBackground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::SetCaretLineBackground
void wxStyledTextCtrl_SetCaretLineBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[1], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->SetCaretLineBackground(back);

}

// wxStyledTextCtrl::AutoCompShow
void wxStyledTextCtrl_AutoCompShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lengthEntered;
  if(!enif_get_int(env, argv[1], &lengthEntered)) Badarg("lengthEntered"); // int
  ErlNifBinary itemList_bin;
  wxString itemList;
  if(!enif_inspect_binary(env, argv[2], &itemList_bin)) Badarg("itemList");
  itemList = wxString(itemList_bin.data, wxConvUTF8, itemList_bin.size);
  This->AutoCompShow(lengthEntered,itemList);

}

// wxStyledTextCtrl::AutoCompCancel
void wxStyledTextCtrl_AutoCompCancel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->AutoCompCancel();

}

// wxStyledTextCtrl::AutoCompActive
void wxStyledTextCtrl_AutoCompActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AutoCompActive();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AutoCompPosStart
void wxStyledTextCtrl_AutoCompPosStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->AutoCompPosStart();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::AutoCompComplete
void wxStyledTextCtrl_AutoCompComplete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->AutoCompComplete();

}

// wxStyledTextCtrl::AutoCompStops
void wxStyledTextCtrl_AutoCompStops(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary characterSet_bin;
  wxString characterSet;
  if(!enif_inspect_binary(env, argv[1], &characterSet_bin)) Badarg("characterSet");
  characterSet = wxString(characterSet_bin.data, wxConvUTF8, characterSet_bin.size);
  This->AutoCompStops(characterSet);

}

// wxStyledTextCtrl::AutoCompSetSeparator
void wxStyledTextCtrl_AutoCompSetSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int separatorCharacter;
  if(!enif_get_int(env, argv[1], &separatorCharacter)) Badarg("separatorCharacter"); // int
  This->AutoCompSetSeparator(separatorCharacter);

}

// wxStyledTextCtrl::AutoCompGetSeparator
void wxStyledTextCtrl_AutoCompGetSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->AutoCompGetSeparator();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::AutoCompSelect
void wxStyledTextCtrl_AutoCompSelect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary select_bin;
  wxString select;
  if(!enif_inspect_binary(env, argv[1], &select_bin)) Badarg("select");
  select = wxString(select_bin.data, wxConvUTF8, select_bin.size);
  This->AutoCompSelect(select);

}

// wxStyledTextCtrl::AutoCompSetCancelAtStart
void wxStyledTextCtrl_AutoCompSetCancelAtStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool cancel;
  cancel = enif_is_identical(argv[1], WXE_ATOM_true);
  This->AutoCompSetCancelAtStart(cancel);

}

// wxStyledTextCtrl::AutoCompGetCancelAtStart
void wxStyledTextCtrl_AutoCompGetCancelAtStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AutoCompGetCancelAtStart();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AutoCompSetFillUps
void wxStyledTextCtrl_AutoCompSetFillUps(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary characterSet_bin;
  wxString characterSet;
  if(!enif_inspect_binary(env, argv[1], &characterSet_bin)) Badarg("characterSet");
  characterSet = wxString(characterSet_bin.data, wxConvUTF8, characterSet_bin.size);
  This->AutoCompSetFillUps(characterSet);

}

// wxStyledTextCtrl::AutoCompSetChooseSingle
void wxStyledTextCtrl_AutoCompSetChooseSingle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool chooseSingle;
  chooseSingle = enif_is_identical(argv[1], WXE_ATOM_true);
  This->AutoCompSetChooseSingle(chooseSingle);

}

// wxStyledTextCtrl::AutoCompGetChooseSingle
void wxStyledTextCtrl_AutoCompGetChooseSingle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AutoCompGetChooseSingle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AutoCompSetIgnoreCase
void wxStyledTextCtrl_AutoCompSetIgnoreCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool ignoreCase;
  ignoreCase = enif_is_identical(argv[1], WXE_ATOM_true);
  This->AutoCompSetIgnoreCase(ignoreCase);

}

// wxStyledTextCtrl::AutoCompGetIgnoreCase
void wxStyledTextCtrl_AutoCompGetIgnoreCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AutoCompGetIgnoreCase();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::UserListShow
void wxStyledTextCtrl_UserListShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int listType;
  if(!enif_get_int(env, argv[1], &listType)) Badarg("listType"); // int
  ErlNifBinary itemList_bin;
  wxString itemList;
  if(!enif_inspect_binary(env, argv[2], &itemList_bin)) Badarg("itemList");
  itemList = wxString(itemList_bin.data, wxConvUTF8, itemList_bin.size);
  This->UserListShow(listType,itemList);

}

// wxStyledTextCtrl::AutoCompSetAutoHide
void wxStyledTextCtrl_AutoCompSetAutoHide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool autoHide;
  autoHide = enif_is_identical(argv[1], WXE_ATOM_true);
  This->AutoCompSetAutoHide(autoHide);

}

// wxStyledTextCtrl::AutoCompGetAutoHide
void wxStyledTextCtrl_AutoCompGetAutoHide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AutoCompGetAutoHide();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AutoCompSetDropRestOfWord
void wxStyledTextCtrl_AutoCompSetDropRestOfWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool dropRestOfWord;
  dropRestOfWord = enif_is_identical(argv[1], WXE_ATOM_true);
  This->AutoCompSetDropRestOfWord(dropRestOfWord);

}

// wxStyledTextCtrl::AutoCompGetDropRestOfWord
void wxStyledTextCtrl_AutoCompGetDropRestOfWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AutoCompGetDropRestOfWord();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::RegisterImage
void wxStyledTextCtrl_RegisterImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int type;
  if(!enif_get_int(env, argv[1], &type)) Badarg("type"); // int
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[2], "bmp");
  This->RegisterImage(type,*bmp);

}

// wxStyledTextCtrl::ClearRegisteredImages
void wxStyledTextCtrl_ClearRegisteredImages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearRegisteredImages();

}

// wxStyledTextCtrl::AutoCompGetTypeSeparator
void wxStyledTextCtrl_AutoCompGetTypeSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->AutoCompGetTypeSeparator();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::AutoCompSetTypeSeparator
void wxStyledTextCtrl_AutoCompSetTypeSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int separatorCharacter;
  if(!enif_get_int(env, argv[1], &separatorCharacter)) Badarg("separatorCharacter"); // int
  This->AutoCompSetTypeSeparator(separatorCharacter);

}

// wxStyledTextCtrl::AutoCompSetMaxWidth
void wxStyledTextCtrl_AutoCompSetMaxWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int characterCount;
  if(!enif_get_int(env, argv[1], &characterCount)) Badarg("characterCount"); // int
  This->AutoCompSetMaxWidth(characterCount);

}

// wxStyledTextCtrl::AutoCompGetMaxWidth
void wxStyledTextCtrl_AutoCompGetMaxWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->AutoCompGetMaxWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::AutoCompSetMaxHeight
void wxStyledTextCtrl_AutoCompSetMaxHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int rowCount;
  if(!enif_get_int(env, argv[1], &rowCount)) Badarg("rowCount"); // int
  This->AutoCompSetMaxHeight(rowCount);

}

// wxStyledTextCtrl::AutoCompGetMaxHeight
void wxStyledTextCtrl_AutoCompGetMaxHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->AutoCompGetMaxHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetIndent
void wxStyledTextCtrl_SetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indentSize;
  if(!enif_get_int(env, argv[1], &indentSize)) Badarg("indentSize"); // int
  This->SetIndent(indentSize);

}

// wxStyledTextCtrl::GetIndent
void wxStyledTextCtrl_GetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetIndent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetUseTabs
void wxStyledTextCtrl_SetUseTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useTabs;
  useTabs = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetUseTabs(useTabs);

}

// wxStyledTextCtrl::GetUseTabs
void wxStyledTextCtrl_GetUseTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUseTabs();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetLineIndentation
void wxStyledTextCtrl_SetLineIndentation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int indentation;
  if(!enif_get_int(env, argv[2], &indentation)) Badarg("indentation"); // int
  This->SetLineIndentation(line,indentation);

}

// wxStyledTextCtrl::GetLineIndentation
void wxStyledTextCtrl_GetLineIndentation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->GetLineIndentation(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetLineIndentPosition
void wxStyledTextCtrl_GetLineIndentPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->GetLineIndentPosition(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetColumn
void wxStyledTextCtrl_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->GetColumn(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetUseHorizontalScrollBar
void wxStyledTextCtrl_SetUseHorizontalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool visible;
  visible = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetUseHorizontalScrollBar(visible);

}

// wxStyledTextCtrl::GetUseHorizontalScrollBar
void wxStyledTextCtrl_GetUseHorizontalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUseHorizontalScrollBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetIndentationGuides
void wxStyledTextCtrl_SetIndentationGuides(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indentView;
  if(!enif_get_int(env, argv[1], &indentView)) Badarg("indentView"); // int
  This->SetIndentationGuides(indentView);

}

// wxStyledTextCtrl::GetIndentationGuides
void wxStyledTextCtrl_GetIndentationGuides(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetIndentationGuides();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetHighlightGuide
void wxStyledTextCtrl_SetHighlightGuide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int column;
  if(!enif_get_int(env, argv[1], &column)) Badarg("column"); // int
  This->SetHighlightGuide(column);

}

// wxStyledTextCtrl::GetHighlightGuide
void wxStyledTextCtrl_GetHighlightGuide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetHighlightGuide();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetLineEndPosition
void wxStyledTextCtrl_GetLineEndPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->GetLineEndPosition(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCodePage
void wxStyledTextCtrl_GetCodePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCodePage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCaretForeground
void wxStyledTextCtrl_GetCaretForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCaretForeground();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::GetReadOnly
void wxStyledTextCtrl_GetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetReadOnly();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetCurrentPos
void wxStyledTextCtrl_SetCurrentPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int caret;
  if(!enif_get_int(env, argv[1], &caret)) Badarg("caret"); // int
  This->SetCurrentPos(caret);

}

// wxStyledTextCtrl::SetSelectionStart
void wxStyledTextCtrl_SetSelectionStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int anchor;
  if(!enif_get_int(env, argv[1], &anchor)) Badarg("anchor"); // int
  This->SetSelectionStart(anchor);

}

// wxStyledTextCtrl::GetSelectionStart
void wxStyledTextCtrl_GetSelectionStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelectionStart();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetSelectionEnd
void wxStyledTextCtrl_SetSelectionEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int caret;
  if(!enif_get_int(env, argv[1], &caret)) Badarg("caret"); // int
  This->SetSelectionEnd(caret);

}

// wxStyledTextCtrl::GetSelectionEnd
void wxStyledTextCtrl_GetSelectionEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelectionEnd();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetPrintMagnification
void wxStyledTextCtrl_SetPrintMagnification(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int magnification;
  if(!enif_get_int(env, argv[1], &magnification)) Badarg("magnification"); // int
  This->SetPrintMagnification(magnification);

}

// wxStyledTextCtrl::GetPrintMagnification
void wxStyledTextCtrl_GetPrintMagnification(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPrintMagnification();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetPrintColourMode
void wxStyledTextCtrl_SetPrintColourMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int mode;
  if(!enif_get_int(env, argv[1], &mode)) Badarg("mode"); // int
  This->SetPrintColourMode(mode);

}

// wxStyledTextCtrl::GetPrintColourMode
void wxStyledTextCtrl_GetPrintColourMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPrintColourMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::FindText
void wxStyledTextCtrl_FindText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int minPos;
  if(!enif_get_int(env, argv[1], &minPos)) Badarg("minPos"); // int
  int maxPos;
  if(!enif_get_int(env, argv[2], &maxPos)) Badarg("maxPos"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[3], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
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
  int Result = This->FindText(minPos,maxPos,text,flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::FormatRange
void wxStyledTextCtrl_FormatRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool doDraw;
  doDraw = enif_is_identical(argv[1], WXE_ATOM_true);
  int startPos;
  if(!enif_get_int(env, argv[2], &startPos)) Badarg("startPos"); // int
  int endPos;
  if(!enif_get_int(env, argv[3], &endPos)) Badarg("endPos"); // int
  wxDC *draw;
  draw = (wxDC *) memenv->getPtr(env, argv[4], "draw");
  wxDC *target;
  target = (wxDC *) memenv->getPtr(env, argv[5], "target");
  const ERL_NIF_TERM *renderRect_t;
  int renderRect_sz;
  if(!enif_get_tuple(env, argv[6], &renderRect_sz, &renderRect_t)) Badarg("renderRect");
  int renderRectX;
  if(!enif_get_int(env, renderRect_t[0], &renderRectX)) Badarg("renderRect");
  int renderRectY;
  if(!enif_get_int(env, renderRect_t[1], &renderRectY)) Badarg("renderRect");
  int renderRectW;
  if(!enif_get_int(env, renderRect_t[2], &renderRectW)) Badarg("renderRect");
  int renderRectH;
  if(!enif_get_int(env, renderRect_t[3], &renderRectH)) Badarg("renderRect");
  wxRect renderRect = wxRect(renderRectX,renderRectY,renderRectW,renderRectH);
  const ERL_NIF_TERM *pageRect_t;
  int pageRect_sz;
  if(!enif_get_tuple(env, argv[7], &pageRect_sz, &pageRect_t)) Badarg("pageRect");
  int pageRectX;
  if(!enif_get_int(env, pageRect_t[0], &pageRectX)) Badarg("pageRect");
  int pageRectY;
  if(!enif_get_int(env, pageRect_t[1], &pageRectY)) Badarg("pageRect");
  int pageRectW;
  if(!enif_get_int(env, pageRect_t[2], &pageRectW)) Badarg("pageRect");
  int pageRectH;
  if(!enif_get_int(env, pageRect_t[3], &pageRectH)) Badarg("pageRect");
  wxRect pageRect = wxRect(pageRectX,pageRectY,pageRectW,pageRectH);
  int Result = This->FormatRange(doDraw,startPos,endPos,draw,target,renderRect,pageRect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetFirstVisibleLine
void wxStyledTextCtrl_GetFirstVisibleLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFirstVisibleLine();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetLine
void wxStyledTextCtrl_GetLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  wxString Result = This->GetLine(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::GetLineCount
void wxStyledTextCtrl_GetLineCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLineCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetMarginLeft
void wxStyledTextCtrl_SetMarginLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pixelWidth;
  if(!enif_get_int(env, argv[1], &pixelWidth)) Badarg("pixelWidth"); // int
  This->SetMarginLeft(pixelWidth);

}

// wxStyledTextCtrl::GetMarginLeft
void wxStyledTextCtrl_GetMarginLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMarginLeft();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetMarginRight
void wxStyledTextCtrl_SetMarginRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pixelWidth;
  if(!enif_get_int(env, argv[1], &pixelWidth)) Badarg("pixelWidth"); // int
  This->SetMarginRight(pixelWidth);

}

// wxStyledTextCtrl::GetMarginRight
void wxStyledTextCtrl_GetMarginRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMarginRight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetModify
void wxStyledTextCtrl_GetModify(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetModify();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetSelection
void wxStyledTextCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  This->SetSelection(from,to);

}

// wxStyledTextCtrl::GetSelectedText
void wxStyledTextCtrl_GetSelectedText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetSelectedText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::GetTextRange
void wxStyledTextCtrl_GetTextRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int startPos;
  if(!enif_get_int(env, argv[1], &startPos)) Badarg("startPos"); // int
  int endPos;
  if(!enif_get_int(env, argv[2], &endPos)) Badarg("endPos"); // int
  wxString Result = This->GetTextRange(startPos,endPos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::HideSelection
void wxStyledTextCtrl_HideSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool hide;
  hide = enif_is_identical(argv[1], WXE_ATOM_true);
  This->HideSelection(hide);

}

// wxStyledTextCtrl::LineFromPosition
void wxStyledTextCtrl_LineFromPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->LineFromPosition(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::PositionFromLine
void wxStyledTextCtrl_PositionFromLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->PositionFromLine(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::LineScroll
void wxStyledTextCtrl_LineScroll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int columns;
  if(!enif_get_int(env, argv[1], &columns)) Badarg("columns"); // int
  int lines;
  if(!enif_get_int(env, argv[2], &lines)) Badarg("lines"); // int
  This->LineScroll(columns,lines);

}

// wxStyledTextCtrl::EnsureCaretVisible
void wxStyledTextCtrl_EnsureCaretVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EnsureCaretVisible();

}

// wxStyledTextCtrl::ReplaceSelection
void wxStyledTextCtrl_ReplaceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->ReplaceSelection(text);

}

// wxStyledTextCtrl::SetReadOnly
void wxStyledTextCtrl_SetReadOnly(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool readOnly;
  readOnly = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetReadOnly(readOnly);

}

// wxStyledTextCtrl::CanPaste
void wxStyledTextCtrl_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanPaste();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::CanUndo
void wxStyledTextCtrl_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanUndo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::EmptyUndoBuffer
void wxStyledTextCtrl_EmptyUndoBuffer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EmptyUndoBuffer();

}

// wxStyledTextCtrl::Undo
void wxStyledTextCtrl_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Undo();

}

// wxStyledTextCtrl::Cut
void wxStyledTextCtrl_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Cut();

}

// wxStyledTextCtrl::Copy
void wxStyledTextCtrl_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Copy();

}

// wxStyledTextCtrl::Paste
void wxStyledTextCtrl_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Paste();

}

// wxStyledTextCtrl::Clear
void wxStyledTextCtrl_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxStyledTextCtrl::SetText
void wxStyledTextCtrl_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->SetText(text);

}

// wxStyledTextCtrl::GetText
void wxStyledTextCtrl_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::GetTextLength
void wxStyledTextCtrl_GetTextLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetTextLength();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetOvertype
void wxStyledTextCtrl_GetOvertype(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetOvertype();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetCaretWidth
void wxStyledTextCtrl_SetCaretWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pixelWidth;
  if(!enif_get_int(env, argv[1], &pixelWidth)) Badarg("pixelWidth"); // int
  This->SetCaretWidth(pixelWidth);

}

// wxStyledTextCtrl::GetCaretWidth
void wxStyledTextCtrl_GetCaretWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCaretWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetTargetStart
void wxStyledTextCtrl_SetTargetStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int start;
  if(!enif_get_int(env, argv[1], &start)) Badarg("start"); // int
  This->SetTargetStart(start);

}

// wxStyledTextCtrl::GetTargetStart
void wxStyledTextCtrl_GetTargetStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetTargetStart();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetTargetEnd
void wxStyledTextCtrl_SetTargetEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int end;
  if(!enif_get_int(env, argv[1], &end)) Badarg("end"); // int
  This->SetTargetEnd(end);

}

// wxStyledTextCtrl::GetTargetEnd
void wxStyledTextCtrl_GetTargetEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetTargetEnd();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::ReplaceTarget
void wxStyledTextCtrl_ReplaceTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  int Result = This->ReplaceTarget(text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SearchInTarget
void wxStyledTextCtrl_SearchInTarget(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  int Result = This->SearchInTarget(text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetSearchFlags
void wxStyledTextCtrl_SetSearchFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int searchFlags;
  if(!enif_get_int(env, argv[1], &searchFlags)) Badarg("searchFlags"); // int
  This->SetSearchFlags(searchFlags);

}

// wxStyledTextCtrl::GetSearchFlags
void wxStyledTextCtrl_GetSearchFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSearchFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::CallTipShow
void wxStyledTextCtrl_CallTipShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  ErlNifBinary definition_bin;
  wxString definition;
  if(!enif_inspect_binary(env, argv[2], &definition_bin)) Badarg("definition");
  definition = wxString(definition_bin.data, wxConvUTF8, definition_bin.size);
  This->CallTipShow(pos,definition);

}

// wxStyledTextCtrl::CallTipCancel
void wxStyledTextCtrl_CallTipCancel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CallTipCancel();

}

// wxStyledTextCtrl::CallTipActive
void wxStyledTextCtrl_CallTipActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CallTipActive();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::CallTipPosAtStart
void wxStyledTextCtrl_CallTipPosAtStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->CallTipPosAtStart();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::CallTipSetHighlight
void wxStyledTextCtrl_CallTipSetHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int highlightStart;
  if(!enif_get_int(env, argv[1], &highlightStart)) Badarg("highlightStart"); // int
  int highlightEnd;
  if(!enif_get_int(env, argv[2], &highlightEnd)) Badarg("highlightEnd"); // int
  This->CallTipSetHighlight(highlightStart,highlightEnd);

}

// wxStyledTextCtrl::CallTipSetBackground
void wxStyledTextCtrl_CallTipSetBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[1], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->CallTipSetBackground(back);

}

// wxStyledTextCtrl::CallTipSetForeground
void wxStyledTextCtrl_CallTipSetForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[1], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->CallTipSetForeground(fore);

}

// wxStyledTextCtrl::CallTipSetForegroundHighlight
void wxStyledTextCtrl_CallTipSetForegroundHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[1], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->CallTipSetForegroundHighlight(fore);

}

// wxStyledTextCtrl::CallTipUseStyle
void wxStyledTextCtrl_CallTipUseStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int tabSize;
  if(!enif_get_int(env, argv[1], &tabSize)) Badarg("tabSize"); // int
  This->CallTipUseStyle(tabSize);

}

// wxStyledTextCtrl::VisibleFromDocLine
void wxStyledTextCtrl_VisibleFromDocLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int docLine;
  if(!enif_get_int(env, argv[1], &docLine)) Badarg("docLine"); // int
  int Result = This->VisibleFromDocLine(docLine);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::DocLineFromVisible
void wxStyledTextCtrl_DocLineFromVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int displayLine;
  if(!enif_get_int(env, argv[1], &displayLine)) Badarg("displayLine"); // int
  int Result = This->DocLineFromVisible(displayLine);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::WrapCount
void wxStyledTextCtrl_WrapCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int docLine;
  if(!enif_get_int(env, argv[1], &docLine)) Badarg("docLine"); // int
  int Result = This->WrapCount(docLine);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetFoldLevel
void wxStyledTextCtrl_SetFoldLevel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int level;
  if(!enif_get_int(env, argv[2], &level)) Badarg("level"); // int
  This->SetFoldLevel(line,level);

}

// wxStyledTextCtrl::GetFoldLevel
void wxStyledTextCtrl_GetFoldLevel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->GetFoldLevel(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetLastChild
void wxStyledTextCtrl_GetLastChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int level;
  if(!enif_get_int(env, argv[2], &level)) Badarg("level"); // int
  int Result = This->GetLastChild(line,level);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetFoldParent
void wxStyledTextCtrl_GetFoldParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->GetFoldParent(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::ShowLines
void wxStyledTextCtrl_ShowLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lineStart;
  if(!enif_get_int(env, argv[1], &lineStart)) Badarg("lineStart"); // int
  int lineEnd;
  if(!enif_get_int(env, argv[2], &lineEnd)) Badarg("lineEnd"); // int
  This->ShowLines(lineStart,lineEnd);

}

// wxStyledTextCtrl::HideLines
void wxStyledTextCtrl_HideLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lineStart;
  if(!enif_get_int(env, argv[1], &lineStart)) Badarg("lineStart"); // int
  int lineEnd;
  if(!enif_get_int(env, argv[2], &lineEnd)) Badarg("lineEnd"); // int
  This->HideLines(lineStart,lineEnd);

}

// wxStyledTextCtrl::GetLineVisible
void wxStyledTextCtrl_GetLineVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  bool Result = This->GetLineVisible(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetFoldExpanded
void wxStyledTextCtrl_SetFoldExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  bool expanded;
  expanded = enif_is_identical(argv[2], WXE_ATOM_true);
  This->SetFoldExpanded(line,expanded);

}

// wxStyledTextCtrl::GetFoldExpanded
void wxStyledTextCtrl_GetFoldExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  bool Result = This->GetFoldExpanded(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::ToggleFold
void wxStyledTextCtrl_ToggleFold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  This->ToggleFold(line);

}

// wxStyledTextCtrl::EnsureVisible
void wxStyledTextCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  This->EnsureVisible(line);

}

// wxStyledTextCtrl::SetFoldFlags
void wxStyledTextCtrl_SetFoldFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int flags;
  if(!enif_get_int(env, argv[1], &flags)) Badarg("flags"); // int
  This->SetFoldFlags(flags);

}

// wxStyledTextCtrl::EnsureVisibleEnforcePolicy
void wxStyledTextCtrl_EnsureVisibleEnforcePolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  This->EnsureVisibleEnforcePolicy(line);

}

// wxStyledTextCtrl::SetTabIndents
void wxStyledTextCtrl_SetTabIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool tabIndents;
  tabIndents = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetTabIndents(tabIndents);

}

// wxStyledTextCtrl::GetTabIndents
void wxStyledTextCtrl_GetTabIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetTabIndents();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetBackSpaceUnIndents
void wxStyledTextCtrl_SetBackSpaceUnIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool bsUnIndents;
  bsUnIndents = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetBackSpaceUnIndents(bsUnIndents);

}

// wxStyledTextCtrl::GetBackSpaceUnIndents
void wxStyledTextCtrl_GetBackSpaceUnIndents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetBackSpaceUnIndents();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetMouseDwellTime
void wxStyledTextCtrl_SetMouseDwellTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int periodMilliseconds;
  if(!enif_get_int(env, argv[1], &periodMilliseconds)) Badarg("periodMilliseconds"); // int
  This->SetMouseDwellTime(periodMilliseconds);

}

// wxStyledTextCtrl::GetMouseDwellTime
void wxStyledTextCtrl_GetMouseDwellTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMouseDwellTime();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::WordStartPosition
void wxStyledTextCtrl_WordStartPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  bool onlyWordCharacters;
  onlyWordCharacters = enif_is_identical(argv[2], WXE_ATOM_true);
  int Result = This->WordStartPosition(pos,onlyWordCharacters);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::WordEndPosition
void wxStyledTextCtrl_WordEndPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  bool onlyWordCharacters;
  onlyWordCharacters = enif_is_identical(argv[2], WXE_ATOM_true);
  int Result = This->WordEndPosition(pos,onlyWordCharacters);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetWrapMode
void wxStyledTextCtrl_SetWrapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int wrapMode;
  if(!enif_get_int(env, argv[1], &wrapMode)) Badarg("wrapMode"); // int
  This->SetWrapMode(wrapMode);

}

// wxStyledTextCtrl::GetWrapMode
void wxStyledTextCtrl_GetWrapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWrapMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetWrapVisualFlags
void wxStyledTextCtrl_SetWrapVisualFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int wrapVisualFlags;
  if(!enif_get_int(env, argv[1], &wrapVisualFlags)) Badarg("wrapVisualFlags"); // int
  This->SetWrapVisualFlags(wrapVisualFlags);

}

// wxStyledTextCtrl::GetWrapVisualFlags
void wxStyledTextCtrl_GetWrapVisualFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWrapVisualFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetWrapVisualFlagsLocation
void wxStyledTextCtrl_SetWrapVisualFlagsLocation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int wrapVisualFlagsLocation;
  if(!enif_get_int(env, argv[1], &wrapVisualFlagsLocation)) Badarg("wrapVisualFlagsLocation"); // int
  This->SetWrapVisualFlagsLocation(wrapVisualFlagsLocation);

}

// wxStyledTextCtrl::GetWrapVisualFlagsLocation
void wxStyledTextCtrl_GetWrapVisualFlagsLocation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWrapVisualFlagsLocation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetWrapStartIndent
void wxStyledTextCtrl_SetWrapStartIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int indent;
  if(!enif_get_int(env, argv[1], &indent)) Badarg("indent"); // int
  This->SetWrapStartIndent(indent);

}

// wxStyledTextCtrl::GetWrapStartIndent
void wxStyledTextCtrl_GetWrapStartIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWrapStartIndent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetLayoutCache
void wxStyledTextCtrl_SetLayoutCache(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int cacheMode;
  if(!enif_get_int(env, argv[1], &cacheMode)) Badarg("cacheMode"); // int
  This->SetLayoutCache(cacheMode);

}

// wxStyledTextCtrl::GetLayoutCache
void wxStyledTextCtrl_GetLayoutCache(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLayoutCache();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetScrollWidth
void wxStyledTextCtrl_SetScrollWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pixelWidth;
  if(!enif_get_int(env, argv[1], &pixelWidth)) Badarg("pixelWidth"); // int
  This->SetScrollWidth(pixelWidth);

}

// wxStyledTextCtrl::GetScrollWidth
void wxStyledTextCtrl_GetScrollWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetScrollWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::TextWidth
void wxStyledTextCtrl_TextWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  int Result = This->TextWidth(style,text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetEndAtLastLine
void wxStyledTextCtrl_GetEndAtLastLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEndAtLastLine();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::TextHeight
void wxStyledTextCtrl_TextHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->TextHeight(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetUseVerticalScrollBar
void wxStyledTextCtrl_SetUseVerticalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool visible;
  visible = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetUseVerticalScrollBar(visible);

}

// wxStyledTextCtrl::GetUseVerticalScrollBar
void wxStyledTextCtrl_GetUseVerticalScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUseVerticalScrollBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AppendText
void wxStyledTextCtrl_AppendText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->AppendText(text);

}

// wxStyledTextCtrl::GetTwoPhaseDraw
void wxStyledTextCtrl_GetTwoPhaseDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetTwoPhaseDraw();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetTwoPhaseDraw
void wxStyledTextCtrl_SetTwoPhaseDraw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool twoPhase;
  twoPhase = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetTwoPhaseDraw(twoPhase);

}

// wxStyledTextCtrl::TargetFromSelection
void wxStyledTextCtrl_TargetFromSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->TargetFromSelection();

}

// wxStyledTextCtrl::LinesJoin
void wxStyledTextCtrl_LinesJoin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LinesJoin();

}

// wxStyledTextCtrl::LinesSplit
void wxStyledTextCtrl_LinesSplit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pixelWidth;
  if(!enif_get_int(env, argv[1], &pixelWidth)) Badarg("pixelWidth"); // int
  This->LinesSplit(pixelWidth);

}

// wxStyledTextCtrl::SetFoldMarginColour
void wxStyledTextCtrl_SetFoldMarginColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[2], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->SetFoldMarginColour(useSetting,back);

}

// wxStyledTextCtrl::SetFoldMarginHiColour
void wxStyledTextCtrl_SetFoldMarginHiColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->SetFoldMarginHiColour(useSetting,fore);

}

// wxStyledTextCtrl::LineDown
void wxStyledTextCtrl_LineDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineDown();

}

// wxStyledTextCtrl::LineDownExtend
void wxStyledTextCtrl_LineDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineDownExtend();

}

// wxStyledTextCtrl::LineUp
void wxStyledTextCtrl_LineUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineUp();

}

// wxStyledTextCtrl::LineUpExtend
void wxStyledTextCtrl_LineUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineUpExtend();

}

// wxStyledTextCtrl::CharLeft
void wxStyledTextCtrl_CharLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CharLeft();

}

// wxStyledTextCtrl::CharLeftExtend
void wxStyledTextCtrl_CharLeftExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CharLeftExtend();

}

// wxStyledTextCtrl::CharRight
void wxStyledTextCtrl_CharRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CharRight();

}

// wxStyledTextCtrl::CharRightExtend
void wxStyledTextCtrl_CharRightExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CharRightExtend();

}

// wxStyledTextCtrl::WordLeft
void wxStyledTextCtrl_WordLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordLeft();

}

// wxStyledTextCtrl::WordLeftExtend
void wxStyledTextCtrl_WordLeftExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordLeftExtend();

}

// wxStyledTextCtrl::WordRight
void wxStyledTextCtrl_WordRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordRight();

}

// wxStyledTextCtrl::WordRightExtend
void wxStyledTextCtrl_WordRightExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordRightExtend();

}

// wxStyledTextCtrl::Home
void wxStyledTextCtrl_Home(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Home();

}

// wxStyledTextCtrl::HomeExtend
void wxStyledTextCtrl_HomeExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HomeExtend();

}

// wxStyledTextCtrl::LineEnd
void wxStyledTextCtrl_LineEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEnd();

}

// wxStyledTextCtrl::LineEndExtend
void wxStyledTextCtrl_LineEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEndExtend();

}

// wxStyledTextCtrl::DocumentStart
void wxStyledTextCtrl_DocumentStart(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DocumentStart();

}

// wxStyledTextCtrl::DocumentStartExtend
void wxStyledTextCtrl_DocumentStartExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DocumentStartExtend();

}

// wxStyledTextCtrl::DocumentEnd
void wxStyledTextCtrl_DocumentEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DocumentEnd();

}

// wxStyledTextCtrl::DocumentEndExtend
void wxStyledTextCtrl_DocumentEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DocumentEndExtend();

}

// wxStyledTextCtrl::PageUp
void wxStyledTextCtrl_PageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageUp();

}

// wxStyledTextCtrl::PageUpExtend
void wxStyledTextCtrl_PageUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageUpExtend();

}

// wxStyledTextCtrl::PageDown
void wxStyledTextCtrl_PageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageDown();

}

// wxStyledTextCtrl::PageDownExtend
void wxStyledTextCtrl_PageDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageDownExtend();

}

// wxStyledTextCtrl::EditToggleOvertype
void wxStyledTextCtrl_EditToggleOvertype(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->EditToggleOvertype();

}

// wxStyledTextCtrl::Cancel
void wxStyledTextCtrl_Cancel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Cancel();

}

// wxStyledTextCtrl::DeleteBack
void wxStyledTextCtrl_DeleteBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DeleteBack();

}

// wxStyledTextCtrl::Tab
void wxStyledTextCtrl_Tab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Tab();

}

// wxStyledTextCtrl::BackTab
void wxStyledTextCtrl_BackTab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->BackTab();

}

// wxStyledTextCtrl::NewLine
void wxStyledTextCtrl_NewLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->NewLine();

}

// wxStyledTextCtrl::FormFeed
void wxStyledTextCtrl_FormFeed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->FormFeed();

}

// wxStyledTextCtrl::VCHome
void wxStyledTextCtrl_VCHome(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->VCHome();

}

// wxStyledTextCtrl::VCHomeExtend
void wxStyledTextCtrl_VCHomeExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->VCHomeExtend();

}

// wxStyledTextCtrl::ZoomIn
void wxStyledTextCtrl_ZoomIn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ZoomIn();

}

// wxStyledTextCtrl::ZoomOut
void wxStyledTextCtrl_ZoomOut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ZoomOut();

}

// wxStyledTextCtrl::DelWordLeft
void wxStyledTextCtrl_DelWordLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DelWordLeft();

}

// wxStyledTextCtrl::DelWordRight
void wxStyledTextCtrl_DelWordRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DelWordRight();

}

// wxStyledTextCtrl::LineCut
void wxStyledTextCtrl_LineCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineCut();

}

// wxStyledTextCtrl::LineDelete
void wxStyledTextCtrl_LineDelete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineDelete();

}

// wxStyledTextCtrl::LineTranspose
void wxStyledTextCtrl_LineTranspose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineTranspose();

}

// wxStyledTextCtrl::LineDuplicate
void wxStyledTextCtrl_LineDuplicate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineDuplicate();

}

// wxStyledTextCtrl::LowerCase
void wxStyledTextCtrl_LowerCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LowerCase();

}

// wxStyledTextCtrl::UpperCase
void wxStyledTextCtrl_UpperCase(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->UpperCase();

}

// wxStyledTextCtrl::LineScrollDown
void wxStyledTextCtrl_LineScrollDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineScrollDown();

}

// wxStyledTextCtrl::LineScrollUp
void wxStyledTextCtrl_LineScrollUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineScrollUp();

}

// wxStyledTextCtrl::DeleteBackNotLine
void wxStyledTextCtrl_DeleteBackNotLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DeleteBackNotLine();

}

// wxStyledTextCtrl::HomeDisplay
void wxStyledTextCtrl_HomeDisplay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HomeDisplay();

}

// wxStyledTextCtrl::HomeDisplayExtend
void wxStyledTextCtrl_HomeDisplayExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HomeDisplayExtend();

}

// wxStyledTextCtrl::LineEndDisplay
void wxStyledTextCtrl_LineEndDisplay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEndDisplay();

}

// wxStyledTextCtrl::LineEndDisplayExtend
void wxStyledTextCtrl_LineEndDisplayExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEndDisplayExtend();

}

// wxStyledTextCtrl::HomeWrapExtend
void wxStyledTextCtrl_HomeWrapExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HomeWrapExtend();

}

// wxStyledTextCtrl::LineEndWrap
void wxStyledTextCtrl_LineEndWrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEndWrap();

}

// wxStyledTextCtrl::LineEndWrapExtend
void wxStyledTextCtrl_LineEndWrapExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEndWrapExtend();

}

// wxStyledTextCtrl::VCHomeWrap
void wxStyledTextCtrl_VCHomeWrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->VCHomeWrap();

}

// wxStyledTextCtrl::VCHomeWrapExtend
void wxStyledTextCtrl_VCHomeWrapExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->VCHomeWrapExtend();

}

// wxStyledTextCtrl::LineCopy
void wxStyledTextCtrl_LineCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineCopy();

}

// wxStyledTextCtrl::MoveCaretInsideView
void wxStyledTextCtrl_MoveCaretInsideView(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->MoveCaretInsideView();

}

// wxStyledTextCtrl::LineLength
void wxStyledTextCtrl_LineLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int Result = This->LineLength(line);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::BraceHighlight
void wxStyledTextCtrl_BraceHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int posA;
  if(!enif_get_int(env, argv[1], &posA)) Badarg("posA"); // int
  int posB;
  if(!enif_get_int(env, argv[2], &posB)) Badarg("posB"); // int
  This->BraceHighlight(posA,posB);

}

// wxStyledTextCtrl::BraceBadLight
void wxStyledTextCtrl_BraceBadLight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  This->BraceBadLight(pos);

}

// wxStyledTextCtrl::BraceMatch
void wxStyledTextCtrl_BraceMatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->BraceMatch(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetViewEOL
void wxStyledTextCtrl_GetViewEOL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetViewEOL();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetViewEOL
void wxStyledTextCtrl_SetViewEOL(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool visible;
  visible = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetViewEOL(visible);

}

// wxStyledTextCtrl::SetModEventMask
void wxStyledTextCtrl_SetModEventMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int eventMask;
  if(!enif_get_int(env, argv[1], &eventMask)) Badarg("eventMask"); // int
  This->SetModEventMask(eventMask);

}

// wxStyledTextCtrl::GetEdgeColumn
void wxStyledTextCtrl_GetEdgeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetEdgeColumn();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetEdgeColumn
void wxStyledTextCtrl_SetEdgeColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int column;
  if(!enif_get_int(env, argv[1], &column)) Badarg("column"); // int
  This->SetEdgeColumn(column);

}

// wxStyledTextCtrl::SetEdgeMode
void wxStyledTextCtrl_SetEdgeMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int edgeMode;
  if(!enif_get_int(env, argv[1], &edgeMode)) Badarg("edgeMode"); // int
  This->SetEdgeMode(edgeMode);

}

// wxStyledTextCtrl::GetEdgeMode
void wxStyledTextCtrl_GetEdgeMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetEdgeMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetEdgeColour
void wxStyledTextCtrl_GetEdgeColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetEdgeColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::SetEdgeColour
void wxStyledTextCtrl_SetEdgeColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const ERL_NIF_TERM *edgeColour_t;
  int edgeColour_sz;
  if(!enif_get_tuple(env, argv[1], &edgeColour_sz, &edgeColour_t)) Badarg("edgeColour");
  int edgeColourR;
  if(!enif_get_int(env, edgeColour_t[0], &edgeColourR)) Badarg("edgeColour");
  int edgeColourG;
  if(!enif_get_int(env, edgeColour_t[1], &edgeColourG)) Badarg("edgeColour");
  int edgeColourB;
  if(!enif_get_int(env, edgeColour_t[2], &edgeColourB)) Badarg("edgeColour");
  int edgeColourA;
  if(!enif_get_int(env, edgeColour_t[3], &edgeColourA)) Badarg("edgeColour");
  wxColour edgeColour = wxColour(edgeColourR,edgeColourG,edgeColourB,edgeColourA);
  This->SetEdgeColour(edgeColour);

}

// wxStyledTextCtrl::SearchAnchor
void wxStyledTextCtrl_SearchAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SearchAnchor();

}

// wxStyledTextCtrl::SearchNext
void wxStyledTextCtrl_SearchNext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int searchFlags;
  if(!enif_get_int(env, argv[1], &searchFlags)) Badarg("searchFlags"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  int Result = This->SearchNext(searchFlags,text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SearchPrev
void wxStyledTextCtrl_SearchPrev(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int searchFlags;
  if(!enif_get_int(env, argv[1], &searchFlags)) Badarg("searchFlags"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  int Result = This->SearchPrev(searchFlags,text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::LinesOnScreen
void wxStyledTextCtrl_LinesOnScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->LinesOnScreen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::UsePopUp
void wxStyledTextCtrl_UsePopUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int popUpMode;
  if(!enif_get_int(env, argv[1], &popUpMode)) Badarg("popUpMode"); // int
  This->UsePopUp(popUpMode);

}

// wxStyledTextCtrl::SelectionIsRectangle
void wxStyledTextCtrl_SelectionIsRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SelectionIsRectangle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetZoom
void wxStyledTextCtrl_SetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int zoomInPoints;
  if(!enif_get_int(env, argv[1], &zoomInPoints)) Badarg("zoomInPoints"); // int
  This->SetZoom(zoomInPoints);

}

// wxStyledTextCtrl::GetZoom
void wxStyledTextCtrl_GetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetZoom();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetModEventMask
void wxStyledTextCtrl_GetModEventMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetModEventMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetSTCFocus
void wxStyledTextCtrl_SetSTCFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool focus;
  focus = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetSTCFocus(focus);

}

// wxStyledTextCtrl::GetSTCFocus
void wxStyledTextCtrl_GetSTCFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSTCFocus();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetStatus
void wxStyledTextCtrl_SetStatus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int status;
  if(!enif_get_int(env, argv[1], &status)) Badarg("status"); // int
  This->SetStatus(status);

}

// wxStyledTextCtrl::GetStatus
void wxStyledTextCtrl_GetStatus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStatus();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetMouseDownCaptures
void wxStyledTextCtrl_SetMouseDownCaptures(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool captures;
  captures = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetMouseDownCaptures(captures);

}

// wxStyledTextCtrl::GetMouseDownCaptures
void wxStyledTextCtrl_GetMouseDownCaptures(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetMouseDownCaptures();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetSTCCursor
void wxStyledTextCtrl_SetSTCCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int cursorType;
  if(!enif_get_int(env, argv[1], &cursorType)) Badarg("cursorType"); // int
  This->SetSTCCursor(cursorType);

}

// wxStyledTextCtrl::GetSTCCursor
void wxStyledTextCtrl_GetSTCCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSTCCursor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetControlCharSymbol
void wxStyledTextCtrl_SetControlCharSymbol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int symbol;
  if(!enif_get_int(env, argv[1], &symbol)) Badarg("symbol"); // int
  This->SetControlCharSymbol(symbol);

}

// wxStyledTextCtrl::GetControlCharSymbol
void wxStyledTextCtrl_GetControlCharSymbol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetControlCharSymbol();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::WordPartLeft
void wxStyledTextCtrl_WordPartLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordPartLeft();

}

// wxStyledTextCtrl::WordPartLeftExtend
void wxStyledTextCtrl_WordPartLeftExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordPartLeftExtend();

}

// wxStyledTextCtrl::WordPartRight
void wxStyledTextCtrl_WordPartRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordPartRight();

}

// wxStyledTextCtrl::WordPartRightExtend
void wxStyledTextCtrl_WordPartRightExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordPartRightExtend();

}

// wxStyledTextCtrl::SetVisiblePolicy
void wxStyledTextCtrl_SetVisiblePolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int visiblePolicy;
  if(!enif_get_int(env, argv[1], &visiblePolicy)) Badarg("visiblePolicy"); // int
  int visibleSlop;
  if(!enif_get_int(env, argv[2], &visibleSlop)) Badarg("visibleSlop"); // int
  This->SetVisiblePolicy(visiblePolicy,visibleSlop);

}

// wxStyledTextCtrl::DelLineLeft
void wxStyledTextCtrl_DelLineLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DelLineLeft();

}

// wxStyledTextCtrl::DelLineRight
void wxStyledTextCtrl_DelLineRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DelLineRight();

}

// wxStyledTextCtrl::GetXOffset
void wxStyledTextCtrl_GetXOffset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetXOffset();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::ChooseCaretX
void wxStyledTextCtrl_ChooseCaretX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ChooseCaretX();

}

// wxStyledTextCtrl::SetXCaretPolicy
void wxStyledTextCtrl_SetXCaretPolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int caretPolicy;
  if(!enif_get_int(env, argv[1], &caretPolicy)) Badarg("caretPolicy"); // int
  int caretSlop;
  if(!enif_get_int(env, argv[2], &caretSlop)) Badarg("caretSlop"); // int
  This->SetXCaretPolicy(caretPolicy,caretSlop);

}

// wxStyledTextCtrl::SetYCaretPolicy
void wxStyledTextCtrl_SetYCaretPolicy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int caretPolicy;
  if(!enif_get_int(env, argv[1], &caretPolicy)) Badarg("caretPolicy"); // int
  int caretSlop;
  if(!enif_get_int(env, argv[2], &caretSlop)) Badarg("caretSlop"); // int
  This->SetYCaretPolicy(caretPolicy,caretSlop);

}

// wxStyledTextCtrl::GetPrintWrapMode
void wxStyledTextCtrl_GetPrintWrapMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPrintWrapMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetHotspotActiveForeground
void wxStyledTextCtrl_SetHotspotActiveForeground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *fore_t;
  int fore_sz;
  if(!enif_get_tuple(env, argv[2], &fore_sz, &fore_t)) Badarg("fore");
  int foreR;
  if(!enif_get_int(env, fore_t[0], &foreR)) Badarg("fore");
  int foreG;
  if(!enif_get_int(env, fore_t[1], &foreG)) Badarg("fore");
  int foreB;
  if(!enif_get_int(env, fore_t[2], &foreB)) Badarg("fore");
  int foreA;
  if(!enif_get_int(env, fore_t[3], &foreA)) Badarg("fore");
  wxColour fore = wxColour(foreR,foreG,foreB,foreA);
  This->SetHotspotActiveForeground(useSetting,fore);

}

// wxStyledTextCtrl::SetHotspotActiveBackground
void wxStyledTextCtrl_SetHotspotActiveBackground(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool useSetting;
  useSetting = enif_is_identical(argv[1], WXE_ATOM_true);
  const ERL_NIF_TERM *back_t;
  int back_sz;
  if(!enif_get_tuple(env, argv[2], &back_sz, &back_t)) Badarg("back");
  int backR;
  if(!enif_get_int(env, back_t[0], &backR)) Badarg("back");
  int backG;
  if(!enif_get_int(env, back_t[1], &backG)) Badarg("back");
  int backB;
  if(!enif_get_int(env, back_t[2], &backB)) Badarg("back");
  int backA;
  if(!enif_get_int(env, back_t[3], &backA)) Badarg("back");
  wxColour back = wxColour(backR,backG,backB,backA);
  This->SetHotspotActiveBackground(useSetting,back);

}

// wxStyledTextCtrl::SetHotspotActiveUnderline
void wxStyledTextCtrl_SetHotspotActiveUnderline(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool underline;
  underline = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetHotspotActiveUnderline(underline);

}

// wxStyledTextCtrl::SetHotspotSingleLine
void wxStyledTextCtrl_SetHotspotSingleLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool singleLine;
  singleLine = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetHotspotSingleLine(singleLine);

}

// wxStyledTextCtrl::ParaDownExtend
void wxStyledTextCtrl_ParaDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ParaDownExtend();

}

// wxStyledTextCtrl::ParaUp
void wxStyledTextCtrl_ParaUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ParaUp();

}

// wxStyledTextCtrl::ParaUpExtend
void wxStyledTextCtrl_ParaUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ParaUpExtend();

}

// wxStyledTextCtrl::PositionBefore
void wxStyledTextCtrl_PositionBefore(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->PositionBefore(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::PositionAfter
void wxStyledTextCtrl_PositionAfter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  int Result = This->PositionAfter(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::CopyRange
void wxStyledTextCtrl_CopyRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int start;
  if(!enif_get_int(env, argv[1], &start)) Badarg("start"); // int
  int end;
  if(!enif_get_int(env, argv[2], &end)) Badarg("end"); // int
  This->CopyRange(start,end);

}

// wxStyledTextCtrl::CopyText
void wxStyledTextCtrl_CopyText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int length;
  if(!enif_get_int(env, argv[1], &length)) Badarg("length"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  This->CopyText(length,text);

}

// wxStyledTextCtrl::SetSelectionMode
void wxStyledTextCtrl_SetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int selectionMode;
  if(!enif_get_int(env, argv[1], &selectionMode)) Badarg("selectionMode"); // int
  This->SetSelectionMode(selectionMode);

}

// wxStyledTextCtrl::GetSelectionMode
void wxStyledTextCtrl_GetSelectionMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelectionMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::LineDownRectExtend
void wxStyledTextCtrl_LineDownRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineDownRectExtend();

}

// wxStyledTextCtrl::LineUpRectExtend
void wxStyledTextCtrl_LineUpRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineUpRectExtend();

}

// wxStyledTextCtrl::CharLeftRectExtend
void wxStyledTextCtrl_CharLeftRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CharLeftRectExtend();

}

// wxStyledTextCtrl::CharRightRectExtend
void wxStyledTextCtrl_CharRightRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CharRightRectExtend();

}

// wxStyledTextCtrl::HomeRectExtend
void wxStyledTextCtrl_HomeRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HomeRectExtend();

}

// wxStyledTextCtrl::VCHomeRectExtend
void wxStyledTextCtrl_VCHomeRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->VCHomeRectExtend();

}

// wxStyledTextCtrl::LineEndRectExtend
void wxStyledTextCtrl_LineEndRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->LineEndRectExtend();

}

// wxStyledTextCtrl::PageUpRectExtend
void wxStyledTextCtrl_PageUpRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageUpRectExtend();

}

// wxStyledTextCtrl::PageDownRectExtend
void wxStyledTextCtrl_PageDownRectExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageDownRectExtend();

}

// wxStyledTextCtrl::StutteredPageUp
void wxStyledTextCtrl_StutteredPageUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StutteredPageUp();

}

// wxStyledTextCtrl::StutteredPageUpExtend
void wxStyledTextCtrl_StutteredPageUpExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StutteredPageUpExtend();

}

// wxStyledTextCtrl::StutteredPageDown
void wxStyledTextCtrl_StutteredPageDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StutteredPageDown();

}

// wxStyledTextCtrl::StutteredPageDownExtend
void wxStyledTextCtrl_StutteredPageDownExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StutteredPageDownExtend();

}

// wxStyledTextCtrl::WordLeftEnd
void wxStyledTextCtrl_WordLeftEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordLeftEnd();

}

// wxStyledTextCtrl::WordLeftEndExtend
void wxStyledTextCtrl_WordLeftEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordLeftEndExtend();

}

// wxStyledTextCtrl::WordRightEnd
void wxStyledTextCtrl_WordRightEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordRightEnd();

}

// wxStyledTextCtrl::WordRightEndExtend
void wxStyledTextCtrl_WordRightEndExtend(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->WordRightEndExtend();

}

// wxStyledTextCtrl::SetWhitespaceChars
void wxStyledTextCtrl_SetWhitespaceChars(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary characters_bin;
  wxString characters;
  if(!enif_inspect_binary(env, argv[1], &characters_bin)) Badarg("characters");
  characters = wxString(characters_bin.data, wxConvUTF8, characters_bin.size);
  This->SetWhitespaceChars(characters);

}

// wxStyledTextCtrl::SetCharsDefault
void wxStyledTextCtrl_SetCharsDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetCharsDefault();

}

// wxStyledTextCtrl::AutoCompGetCurrent
void wxStyledTextCtrl_AutoCompGetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->AutoCompGetCurrent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::Allocate
void wxStyledTextCtrl_Allocate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int bytes;
  if(!enif_get_int(env, argv[1], &bytes)) Badarg("bytes"); // int
  This->Allocate(bytes);

}

// wxStyledTextCtrl::FindColumn
void wxStyledTextCtrl_FindColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  int column;
  if(!enif_get_int(env, argv[2], &column)) Badarg("column"); // int
  int Result = This->FindColumn(line,column);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCaretSticky
void wxStyledTextCtrl_GetCaretSticky(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCaretSticky();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::SetCaretSticky
void wxStyledTextCtrl_SetCaretSticky(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int useCaretStickyBehaviour;
  if(!enif_get_int(env, argv[1], &useCaretStickyBehaviour)) Badarg("useCaretStickyBehaviour"); // int
  This->SetCaretSticky(useCaretStickyBehaviour);

}

// wxStyledTextCtrl::ToggleCaretSticky
void wxStyledTextCtrl_ToggleCaretSticky(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ToggleCaretSticky();

}

// wxStyledTextCtrl::SetPasteConvertEndings
void wxStyledTextCtrl_SetPasteConvertEndings(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool convert;
  convert = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetPasteConvertEndings(convert);

}

// wxStyledTextCtrl::GetPasteConvertEndings
void wxStyledTextCtrl_GetPasteConvertEndings(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetPasteConvertEndings();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SelectionDuplicate
void wxStyledTextCtrl_SelectionDuplicate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SelectionDuplicate();

}

// wxStyledTextCtrl::SetCaretLineBackAlpha
void wxStyledTextCtrl_SetCaretLineBackAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int alpha;
  if(!enif_get_int(env, argv[1], &alpha)) Badarg("alpha"); // int
  This->SetCaretLineBackAlpha(alpha);

}

// wxStyledTextCtrl::GetCaretLineBackAlpha
void wxStyledTextCtrl_GetCaretLineBackAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCaretLineBackAlpha();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::StartRecord
void wxStyledTextCtrl_StartRecord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StartRecord();

}

// wxStyledTextCtrl::StopRecord
void wxStyledTextCtrl_StopRecord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->StopRecord();

}

// wxStyledTextCtrl::SetLexer
void wxStyledTextCtrl_SetLexer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int lexer;
  if(!enif_get_int(env, argv[1], &lexer)) Badarg("lexer"); // int
  This->SetLexer(lexer);

}

// wxStyledTextCtrl::GetLexer
void wxStyledTextCtrl_GetLexer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLexer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::Colourise
void wxStyledTextCtrl_Colourise(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int start;
  if(!enif_get_int(env, argv[1], &start)) Badarg("start"); // int
  int end;
  if(!enif_get_int(env, argv[2], &end)) Badarg("end"); // int
  This->Colourise(start,end);

}

// wxStyledTextCtrl::SetProperty
void wxStyledTextCtrl_SetProperty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary key_bin;
  wxString key;
  if(!enif_inspect_binary(env, argv[1], &key_bin)) Badarg("key");
  key = wxString(key_bin.data, wxConvUTF8, key_bin.size);
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  This->SetProperty(key,value);

}

// wxStyledTextCtrl::SetKeyWords
void wxStyledTextCtrl_SetKeyWords(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int keyWordSet;
  if(!enif_get_int(env, argv[1], &keyWordSet)) Badarg("keyWordSet"); // int
  ErlNifBinary keyWords_bin;
  wxString keyWords;
  if(!enif_inspect_binary(env, argv[2], &keyWords_bin)) Badarg("keyWords");
  keyWords = wxString(keyWords_bin.data, wxConvUTF8, keyWords_bin.size);
  This->SetKeyWords(keyWordSet,keyWords);

}

// wxStyledTextCtrl::SetLexerLanguage
void wxStyledTextCtrl_SetLexerLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary language_bin;
  wxString language;
  if(!enif_inspect_binary(env, argv[1], &language_bin)) Badarg("language");
  language = wxString(language_bin.data, wxConvUTF8, language_bin.size);
  This->SetLexerLanguage(language);

}

// wxStyledTextCtrl::GetProperty
void wxStyledTextCtrl_GetProperty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary key_bin;
  wxString key;
  if(!enif_inspect_binary(env, argv[1], &key_bin)) Badarg("key");
  key = wxString(key_bin.data, wxConvUTF8, key_bin.size);
  wxString Result = This->GetProperty(key);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::GetStyleBitsNeeded
void wxStyledTextCtrl_GetStyleBitsNeeded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStyleBitsNeeded();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::GetCurrentLine
void wxStyledTextCtrl_GetCurrentLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCurrentLine();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::StyleSetSpec
void wxStyledTextCtrl_StyleSetSpec(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int styleNum;
  if(!enif_get_int(env, argv[1], &styleNum)) Badarg("styleNum"); // int
  ErlNifBinary spec_bin;
  wxString spec;
  if(!enif_inspect_binary(env, argv[2], &spec_bin)) Badarg("spec");
  spec = wxString(spec_bin.data, wxConvUTF8, spec_bin.size);
  This->StyleSetSpec(styleNum,spec);

}

// wxStyledTextCtrl::StyleSetFont
void wxStyledTextCtrl_StyleSetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int styleNum;
  if(!enif_get_int(env, argv[1], &styleNum)) Badarg("styleNum"); // int
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  This->StyleSetFont(styleNum,*font);

}

// wxStyledTextCtrl::StyleSetFontAttr
void wxStyledTextCtrl_StyleSetFontAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxFontEncoding encoding=wxFONTENCODING_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int styleNum;
  if(!enif_get_int(env, argv[1], &styleNum)) Badarg("styleNum"); // int
  int size;
  if(!enif_get_int(env, argv[2], &size)) Badarg("size"); // int
  ErlNifBinary faceName_bin;
  wxString faceName;
  if(!enif_inspect_binary(env, argv[3], &faceName_bin)) Badarg("faceName");
  faceName = wxString(faceName_bin.data, wxConvUTF8, faceName_bin.size);
  bool bold;
  bold = enif_is_identical(argv[4], WXE_ATOM_true);
  bool italic;
  italic = enif_is_identical(argv[5], WXE_ATOM_true);
  bool underline;
  underline = enif_is_identical(argv[6], WXE_ATOM_true);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[7];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "encoding"))) {
  if(!enif_get_int(env, tpl[1], (int *) &encoding)) Badarg("encoding"); // enum
    } else        Badarg("Options");
  };
  This->StyleSetFontAttr(styleNum,size,faceName,bold,italic,underline,encoding);

}

// wxStyledTextCtrl::StyleSetCharacterSet
void wxStyledTextCtrl_StyleSetCharacterSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  int characterSet;
  if(!enif_get_int(env, argv[2], &characterSet)) Badarg("characterSet"); // int
  This->StyleSetCharacterSet(style,characterSet);

}

// wxStyledTextCtrl::StyleSetFontEncoding
void wxStyledTextCtrl_StyleSetFontEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int style;
  if(!enif_get_int(env, argv[1], &style)) Badarg("style"); // int
  wxFontEncoding encoding;
  if(!enif_get_int(env, argv[2], (int *) &encoding)) Badarg("encoding"); // enum
  This->StyleSetFontEncoding(style,encoding);

}

// wxStyledTextCtrl::CmdKeyExecute
void wxStyledTextCtrl_CmdKeyExecute(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int cmd;
  if(!enif_get_int(env, argv[1], &cmd)) Badarg("cmd"); // int
  This->CmdKeyExecute(cmd);

}

// wxStyledTextCtrl::SetMargins
void wxStyledTextCtrl_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int left;
  if(!enif_get_int(env, argv[1], &left)) Badarg("left"); // int
  int right;
  if(!enif_get_int(env, argv[2], &right)) Badarg("right"); // int
  This->SetMargins(left,right);

}

// wxStyledTextCtrl::GetSelection
void wxStyledTextCtrl_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long from;
  long to;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetSelection(&from,&to);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(from),
  rt.make_int(to));
  rt.send(msg);

}

// wxStyledTextCtrl::PointFromPosition
void wxStyledTextCtrl_PointFromPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  wxPoint Result = This->PointFromPosition(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextCtrl::ScrollToLine
void wxStyledTextCtrl_ScrollToLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  This->ScrollToLine(line);

}

// wxStyledTextCtrl::ScrollToColumn
void wxStyledTextCtrl_ScrollToColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int column;
  if(!enif_get_int(env, argv[1], &column)) Badarg("column"); // int
  This->ScrollToColumn(column);

}

// wxStyledTextCtrl::SetVScrollBar
void wxStyledTextCtrl_SetVScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxScrollBar *bar;
  bar = (wxScrollBar *) memenv->getPtr(env, argv[1], "bar");
  This->SetVScrollBar(bar);

}

// wxStyledTextCtrl::SetHScrollBar
void wxStyledTextCtrl_SetHScrollBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxScrollBar *bar;
  bar = (wxScrollBar *) memenv->getPtr(env, argv[1], "bar");
  This->SetHScrollBar(bar);

}

// wxStyledTextCtrl::GetLastKeydownProcessed
void wxStyledTextCtrl_GetLastKeydownProcessed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetLastKeydownProcessed();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::SetLastKeydownProcessed
void wxStyledTextCtrl_SetLastKeydownProcessed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool val;
  val = enif_is_identical(argv[1], WXE_ATOM_true);
  This->SetLastKeydownProcessed(val);

}

// wxStyledTextCtrl::SaveFile
void wxStyledTextCtrl_SaveFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  bool Result = This->SaveFile(filename);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::LoadFile
void wxStyledTextCtrl_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  bool Result = This->LoadFile(filename);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::DoDragOver
void wxStyledTextCtrl_DoDragOver(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  wxDragResult defaultRes;
  if(!enif_get_int(env, argv[3], (int *) &defaultRes)) Badarg("defaultRes"); // enum
  int Result = This->DoDragOver(x,y,defaultRes);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextCtrl::DoDropText
void wxStyledTextCtrl_DoDropText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long x;
  if(!enif_get_long(env, argv[1], &x)) Badarg("x");
  long y;
  if(!enif_get_long(env, argv[2], &y)) Badarg("y");
  ErlNifBinary data_bin;
  wxString data;
  if(!enif_inspect_binary(env, argv[3], &data_bin)) Badarg("data");
  data = wxString(data_bin.data, wxConvUTF8, data_bin.size);
  bool Result = This->DoDropText(x,y,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::GetUseAntiAliasing
void wxStyledTextCtrl_GetUseAntiAliasing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUseAntiAliasing();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextCtrl::AddTextRaw
void wxStyledTextCtrl_AddTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int length=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const char * text;
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = (const char*) text_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "length"))) {
  if(!enif_get_int(env, tpl[1], &length)) Badarg("length"); // int
    } else        Badarg("Options");
  };
  This->AddTextRaw(text,length);

}

// wxStyledTextCtrl::InsertTextRaw
void wxStyledTextCtrl_InsertTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  const char * text;
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = (const char*) text_bin.data;
  This->InsertTextRaw(pos,text);

}

// wxStyledTextCtrl::GetCurLineRaw
void wxStyledTextCtrl_GetCurLineRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int linePos;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCharBuffer Result_cb = This->GetCurLineRaw(&linePos); char * Result = Result_cb.data();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_binary(Result, strlen(Result)),
    rt.make_int(linePos));
  rt.send(msg);

}

// wxStyledTextCtrl::GetLineRaw
void wxStyledTextCtrl_GetLineRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int line;
  if(!enif_get_int(env, argv[1], &line)) Badarg("line"); // int
  wxCharBuffer Result_cb = This->GetLineRaw(line); char * Result = Result_cb.data();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, strlen(Result)));

}

// wxStyledTextCtrl::GetSelectedTextRaw
void wxStyledTextCtrl_GetSelectedTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCharBuffer Result_cb = This->GetSelectedTextRaw(); char * Result = Result_cb.data();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, strlen(Result)));

}

// wxStyledTextCtrl::GetTextRangeRaw
void wxStyledTextCtrl_GetTextRangeRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int startPos;
  if(!enif_get_int(env, argv[1], &startPos)) Badarg("startPos"); // int
  int endPos;
  if(!enif_get_int(env, argv[2], &endPos)) Badarg("endPos"); // int
  wxCharBuffer Result_cb = This->GetTextRangeRaw(startPos,endPos); char * Result = Result_cb.data();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, strlen(Result)));

}

// wxStyledTextCtrl::SetTextRaw
void wxStyledTextCtrl_SetTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const char * text;
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = (const char*) text_bin.data;
  This->SetTextRaw(text);

}

// wxStyledTextCtrl::GetTextRaw
void wxStyledTextCtrl_GetTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCharBuffer Result_cb = This->GetTextRaw(); char * Result = Result_cb.data();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, strlen(Result)));

}

// wxStyledTextCtrl::AppendTextRaw
void wxStyledTextCtrl_AppendTextRaw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int length=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextCtrl *This;
  This = (wxStyledTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const char * text;
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = (const char*) text_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "length"))) {
  if(!enif_get_int(env, tpl[1], &length)) Badarg("length"); // int
    } else        Badarg("Options");
  };
  This->AppendTextRaw(text,length);

}

// wxStyledTextEvent::GetPosition
void wxStyledTextEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetKey
void wxStyledTextEvent_GetKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKey();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetModifiers
void wxStyledTextEvent_GetModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetModifiers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetModificationType
void wxStyledTextEvent_GetModificationType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetModificationType();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetText
void wxStyledTextEvent_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextEvent::GetLength
void wxStyledTextEvent_GetLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLength();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetLinesAdded
void wxStyledTextEvent_GetLinesAdded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLinesAdded();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetLine
void wxStyledTextEvent_GetLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLine();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetFoldLevelNow
void wxStyledTextEvent_GetFoldLevelNow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFoldLevelNow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetFoldLevelPrev
void wxStyledTextEvent_GetFoldLevelPrev(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFoldLevelPrev();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetMargin
void wxStyledTextEvent_GetMargin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMargin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetMessage
void wxStyledTextEvent_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMessage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetWParam
void wxStyledTextEvent_GetWParam(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWParam();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetLParam
void wxStyledTextEvent_GetLParam(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLParam();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetListType
void wxStyledTextEvent_GetListType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetListType();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetX
void wxStyledTextEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetY
void wxStyledTextEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetDragText
void wxStyledTextEvent_GetDragText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetDragText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxStyledTextEvent::GetDragAllowMove
void wxStyledTextEvent_GetDragAllowMove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetDragAllowMove();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextEvent::GetDragResult
void wxStyledTextEvent_GetDragResult(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDragResult();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxStyledTextEvent::GetShift
void wxStyledTextEvent_GetShift(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetShift();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextEvent::GetControl
void wxStyledTextEvent_GetControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxStyledTextEvent::GetAlt
void wxStyledTextEvent_GetAlt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStyledTextEvent *This;
  This = (wxStyledTextEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetAlt();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSystemOptions::GetOption
void wxSystemOptions_GetOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  wxString Result = wxSystemOptions::GetOption(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSystemOptions::GetOptionInt
void wxSystemOptions_GetOptionInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  int Result = wxSystemOptions::GetOptionInt(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSystemOptions::HasOption
void wxSystemOptions_HasOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  bool Result = wxSystemOptions::HasOption(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSystemOptions::IsFalse
void wxSystemOptions_IsFalse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  bool Result = wxSystemOptions::IsFalse(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxSystemOptions::SetOption
void wxSystemOptions_SetOption_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  wxSystemOptions::SetOption(name,value);

}

// wxSystemOptions::SetOption
void wxSystemOptions_SetOption_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  wxSystemOptions::SetOption(name,value);

}

// wxSystemSettings::GetColour
void wxSystemSettings_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSystemColour index;
  if(!enif_get_int(env, argv[0], (int *) &index)) Badarg("index"); // enum
  wxColour Result = wxSystemSettings::GetColour(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxSystemSettings::GetFont
void wxSystemSettings_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSystemFont index;
  if(!enif_get_int(env, argv[0], (int *) &index)) Badarg("index"); // enum
  wxFont * Result = new wxFont(wxSystemSettings::GetFont(index)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxSystemSettings::GetMetric
void wxSystemSettings_GetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * win=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSystemMetric index;
  if(!enif_get_int(env, argv[0], (int *) &index)) Badarg("index"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "win"))) {
  win = (wxWindow *) memenv->getPtr(env, tpl[1], "win");
    } else        Badarg("Options");
  };
  int Result = wxSystemSettings::GetMetric(index,win);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxSystemSettings::GetScreenType
void wxSystemSettings_GetScreenType(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxSystemSettings::GetScreenType();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

