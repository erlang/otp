/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2022. All Rights Reserved.
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

// wxMDIChildFrame::wxMDIChildFrame
void wxMDIChildFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMDIChildFrame * Result = new EwxMDIChildFrame();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIChildFrame"));

}

// wxMDIChildFrame::wxMDIChildFrame
void wxMDIChildFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *parent;
  parent = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "parent");
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
  wxMDIChildFrame * Result = new EwxMDIChildFrame(parent,id,title,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIChildFrame"));

}

// wxMDIChildFrame::Activate
void wxMDIChildFrame_Activate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIChildFrame *This;
  This = (wxMDIChildFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Activate();

}

// wxMDIChildFrame::Create
void wxMDIChildFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIChildFrame *This;
  This = (wxMDIChildFrame *) memenv->getPtr(env, argv[0], "This");
  wxMDIParentFrame *parent;
  parent = (wxMDIParentFrame *) memenv->getPtr(env, argv[1], "parent");
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

// wxMDIChildFrame::Maximize
void wxMDIChildFrame_Maximize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool maximize=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIChildFrame *This;
  This = (wxMDIChildFrame *) memenv->getPtr(env, argv[0], "This");
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

// wxMDIChildFrame::Restore
void wxMDIChildFrame_Restore(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIChildFrame *This;
  This = (wxMDIChildFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Restore();

}

// wxMDIClientWindow::wxMDIClientWindow
void wxMDIClientWindow_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMDIClientWindow * Result = new EwxMDIClientWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIClientWindow"));

}

// wxMDIClientWindow::CreateClient
void wxMDIClientWindow_CreateClient(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIClientWindow *This;
  This = (wxMDIClientWindow *) memenv->getPtr(env, argv[0], "This");
  wxMDIParentFrame *parent;
  parent = (wxMDIParentFrame *) memenv->getPtr(env, argv[1], "parent");
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
  bool Result = This->CreateClient(parent,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMDIParentFrame::wxMDIParentFrame
void wxMDIParentFrame_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMDIParentFrame * Result = new EwxMDIParentFrame();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIParentFrame"));

}

// wxMDIParentFrame::wxMDIParentFrame
void wxMDIParentFrame_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE|wxVSCROLL|wxHSCROLL;
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
  wxMDIParentFrame * Result = new EwxMDIParentFrame(parent,id,title,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIParentFrame"));

}

// wxMDIParentFrame::ActivateNext
void wxMDIParentFrame_ActivateNext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ActivateNext();

}

// wxMDIParentFrame::ActivatePrevious
void wxMDIParentFrame_ActivatePrevious(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ActivatePrevious();

}

// wxMDIParentFrame::ArrangeIcons
void wxMDIParentFrame_ArrangeIcons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ArrangeIcons();

}

// wxMDIParentFrame::Cascade
void wxMDIParentFrame_Cascade(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Cascade();

}

// wxMDIParentFrame::Create
void wxMDIParentFrame_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE|wxVSCROLL|wxHSCROLL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
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

// wxMDIParentFrame::GetActiveChild
void wxMDIParentFrame_GetActiveChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMDIChildFrame * Result = (wxMDIChildFrame*)This->GetActiveChild();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIChildFrame"));

}

// wxMDIParentFrame::GetClientWindow
void wxMDIParentFrame_GetClientWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMDIClientWindow * Result = (wxMDIClientWindow*)This->GetClientWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMDIClientWindow"));

}

// wxMDIParentFrame::Tile
void wxMDIParentFrame_Tile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxOrientation orient=wxHORIZONTAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMDIParentFrame *This;
  This = (wxMDIParentFrame *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "orient"))) {
  if(!enif_get_int(env, tpl[1], (int *) &orient)) Badarg("orient"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Tile(orient);

}

// wxMask::wxMask
void wxMask_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMask * Result = new EwxMask();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMask"));

}

// wxMask::wxMask
void wxMask_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[0], "bitmap");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  wxMask * Result = new EwxMask(*bitmap,index);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMask"));

}

// wxMask::wxMask
void wxMask_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[0], "bitmap");
  wxMask * Result = new EwxMask(*bitmap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMask"));

}

// wxMask::wxMask
void wxMask_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[0], "bitmap");
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
  wxMask * Result = new EwxMask(*bitmap,colour);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMask"));

}

// wxMask::Create
void wxMask_Create_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMask *This;
  This = (wxMask *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  int index;
  if(!enif_get_int(env, argv[2], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(*bitmap,index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMask::Create
void wxMask_Create_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMask *This;
  This = (wxMask *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(*bitmap);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMask::Create
void wxMask_Create_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMask *This;
  This = (wxMask *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[2], &colour_sz, &colour_t)) Badarg("colour");
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
  bool Result = This->Create(*bitmap,colour);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMemoryDC::wxMemoryDC
void wxMemoryDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMemoryDC * Result = new EwxMemoryDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMemoryDC"));

}

// wxMemoryDC::wxMemoryDC
void wxMemoryDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM dc_type;
  void * dc = memenv->getPtr(env, argv[0], "dc", &dc_type);
  wxMemoryDC * Result;
  if(enif_is_identical(dc_type, WXE_ATOM_wxDC))
    Result = new EwxMemoryDC(static_cast<wxDC*> (dc));
  else if(enif_is_identical(dc_type, WXE_ATOM_wxBitmap))
    Result = new EwxMemoryDC(* static_cast<wxBitmap*> (dc));
  else throw wxe_badarg("dc");
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMemoryDC"));

}

// wxMemoryDC::SelectObject
void wxMemoryDC_SelectObject(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMemoryDC *This;
  This = (wxMemoryDC *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SelectObject(*bitmap);

}

// wxMemoryDC::SelectObjectAsSource
void wxMemoryDC_SelectObjectAsSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMemoryDC *This;
  This = (wxMemoryDC *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SelectObjectAsSource(*bitmap);

}

// wxMenu::wxMenu
void wxMenu_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenu * Result = new EwxMenu();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenu::wxMenu
void wxMenu_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else        Badarg("Options");
  };
  wxMenu * Result = new EwxMenu(style);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenu::wxMenu
void wxMenu_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[0], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
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
    } else        Badarg("Options");
  };
  wxMenu * Result = new EwxMenu(title,style);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenu::Append
void wxMenu_Append_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
 wxItemKind kind=wxITEM_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[2], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Append(id,item,help,kind);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Append
void wxMenu_Append_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[2], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  wxMenu *subMenu;
  subMenu = (wxMenu *) memenv->getPtr(env, argv[3], "subMenu");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Append(id,item,subMenu,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Append
void wxMenu_Append_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  wxMenuItem *menuItem;
  menuItem = (wxMenuItem *) memenv->getPtr(env, argv[1], "menuItem");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Append(menuItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::AppendCheckItem
void wxMenu_AppendCheckItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[2], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->AppendCheckItem(id,item,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::AppendRadioItem
void wxMenu_AppendRadioItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[2], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->AppendRadioItem(id,item,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::AppendSeparator
void wxMenu_AppendSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->AppendSeparator();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Break
void wxMenu_Break(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Break();

}

// wxMenu::Check
void wxMenu_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  bool check;
  check = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Check(id,check);

}

// wxMenu::Delete
void wxMenu_Delete_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->Delete(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenu::Delete
void wxMenu_Delete_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  wxMenuItem *item;
  item = (wxMenuItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Delete(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenu::Destroy
void wxMenu_Destroy_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->Destroy(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenu::Destroy
void wxMenu_Destroy_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  wxMenuItem *item;
  item = (wxMenuItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Destroy(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenu::Enable
void wxMenu_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  bool enable;
  enable = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Enable(id,enable);

}

// wxMenu::FindItem
void wxMenu_FindItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary itemString_bin;
  wxString itemString;
  if(!enif_inspect_binary(env, argv[1], &itemString_bin)) Badarg("itemString");
  itemString = wxString(itemString_bin.data, wxConvUTF8, itemString_bin.size);
  if(!This) throw wxe_badarg("This");
  int Result = This->FindItem(itemString);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenu::FindItem
void wxMenu_FindItem_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenu ** menu = NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->FindItem(id,menu);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::FindItemByPosition
void wxMenu_FindItemByPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t position;
  if(!wxe_get_size_t(env, argv[1], &position)) Badarg("position");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->FindItemByPosition(position);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::GetHelpString
void wxMenu_GetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetHelpString(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenu::GetLabel
void wxMenu_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetLabel(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenu::GetMenuItemCount
void wxMenu_GetMenuItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetMenuItemCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenu::GetMenuItems
void wxMenu_GetMenuItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxMenuItemList Result = This->GetMenuItems();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_list_objs(Result, app, "wxMenuItem"));

}

// wxMenu::GetTitle
void wxMenu_GetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetTitle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenu::Insert
void wxMenu_Insert_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  wxMenuItem *menuItem;
  menuItem = (wxMenuItem *) memenv->getPtr(env, argv[2], "menuItem");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Insert(pos,menuItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Insert
void wxMenu_Insert_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString text= wxEmptyString;
  wxString help= wxEmptyString;
 wxItemKind kind=wxITEM_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "text"))) {
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, tpl[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Insert(pos,id,text,help,kind);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Insert
void wxMenu_Insert_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[3], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  wxMenu *submenu;
  submenu = (wxMenu *) memenv->getPtr(env, argv[4], "submenu");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Insert(pos,id,text,submenu,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::InsertCheckItem
void wxMenu_InsertCheckItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[3], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->InsertCheckItem(pos,id,item,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::InsertRadioItem
void wxMenu_InsertRadioItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[3], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->InsertRadioItem(pos,id,item,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::InsertSeparator
void wxMenu_InsertSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->InsertSeparator(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::IsChecked
void wxMenu_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsChecked(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenu::IsEnabled
void wxMenu_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEnabled(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenu::Prepend
void wxMenu_Prepend_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  wxMenuItem *item;
  item = (wxMenuItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Prepend(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Prepend
void wxMenu_Prepend_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString text= wxEmptyString;
  wxString help= wxEmptyString;
 wxItemKind kind=wxITEM_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "text"))) {
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, tpl[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Prepend(id,text,help,kind);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Prepend
void wxMenu_Prepend_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  wxMenu *submenu;
  submenu = (wxMenu *) memenv->getPtr(env, argv[3], "submenu");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Prepend(id,text,submenu,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::PrependCheckItem
void wxMenu_PrependCheckItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[2], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->PrependCheckItem(id,item,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::PrependRadioItem
void wxMenu_PrependRadioItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString help= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[2], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->PrependRadioItem(id,item,help);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::PrependSeparator
void wxMenu_PrependSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->PrependSeparator();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Remove
void wxMenu_Remove_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Remove(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::Remove
void wxMenu_Remove_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  wxMenuItem *item;
  item = (wxMenuItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->Remove(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenu::SetHelpString
void wxMenu_SetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary helpString_bin;
  wxString helpString;
  if(!enif_inspect_binary(env, argv[2], &helpString_bin)) Badarg("helpString");
  helpString = wxString(helpString_bin.data, wxConvUTF8, helpString_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetHelpString(id,helpString);

}

// wxMenu::SetLabel
void wxMenu_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetLabel(id,label);

}

// wxMenu::SetTitle
void wxMenu_SetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenu *This;
  This = (wxMenu *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[1], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetTitle(title);

}

// wxMenuBar::wxMenuBar
void wxMenuBar_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenuBar * Result = new EwxMenuBar();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

// wxMenuBar::wxMenuBar
void wxMenuBar_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  long style;
  if(!enif_get_long(env, argv[0], &style)) Badarg("style");
  wxMenuBar * Result = new EwxMenuBar(style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

// wxMenuBar::Append
void wxMenuBar_Append(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[1], "menu");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[2], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Append(menu,title);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuBar::Check
void wxMenuBar_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  bool check;
  check = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Check(id,check);

}

// wxMenuBar::Enable
void wxMenuBar_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  bool enable;
  enable = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Enable(id,enable);

}

// wxMenuBar::EnableTop
void wxMenuBar_EnableTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  bool enable;
  enable = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableTop(pos,enable);

}

// wxMenuBar::FindMenu
void wxMenuBar_FindMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[1], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  int Result = This->FindMenu(title);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenuBar::FindMenuItem
void wxMenuBar_FindMenuItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary menuString_bin;
  wxString menuString;
  if(!enif_inspect_binary(env, argv[1], &menuString_bin)) Badarg("menuString");
  menuString = wxString(menuString_bin.data, wxConvUTF8, menuString_bin.size);
  ErlNifBinary itemString_bin;
  wxString itemString;
  if(!enif_inspect_binary(env, argv[2], &itemString_bin)) Badarg("itemString");
  itemString = wxString(itemString_bin.data, wxConvUTF8, itemString_bin.size);
  if(!This) throw wxe_badarg("This");
  int Result = This->FindMenuItem(menuString,itemString);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenuBar::FindItem
void wxMenuBar_FindItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenu ** menu = NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxMenuItem * Result = (wxMenuItem*)This->FindItem(id,menu);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenuBar::GetHelpString
void wxMenuBar_GetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetHelpString(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuBar::GetLabel
void wxMenuBar_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetLabel(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuBar::GetMenuLabel
void wxMenuBar_GetMenuLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetMenuLabel(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuBar::GetMenuLabelText
void wxMenuBar_GetMenuLabelText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetMenuLabelText(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuBar::GetMenu
void wxMenuBar_GetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t menuIndex;
  if(!wxe_get_size_t(env, argv[1], &menuIndex)) Badarg("menuIndex");
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->GetMenu(menuIndex);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenuBar::GetMenuCount
void wxMenuBar_GetMenuCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetMenuCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenuBar::Insert
void wxMenuBar_Insert(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[2], "menu");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[3], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Insert(pos,menu,title);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuBar::IsChecked
void wxMenuBar_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsChecked(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if defined(__WXMAC__)
// wxMenuBar::SetAutoWindowMenu
void wxMenuBar_SetAutoWindowMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ERL_NIF_TERM * argv = Ecmd.args;
  bool enable;
  enable = enif_is_identical(argv[0], WXE_ATOM_true);
  wxMenuBar::SetAutoWindowMenu(enable);

}

#endif
#if defined(__WXMAC__)
// wxMenuBar::GetAutoWindowMenu
void wxMenuBar_GetAutoWindowMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool Result = wxMenuBar::GetAutoWindowMenu();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
#if defined(__WXMAC__)
// wxMenuBar::OSXGetAppleMenu
void wxMenuBar_OSXGetAppleMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->OSXGetAppleMenu();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

#endif
#if defined(__WXMAC__)
// wxMenuBar::MacGetCommonMenuBar
void wxMenuBar_MacGetCommonMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenuBar * Result = (wxMenuBar*)wxMenuBar::MacGetCommonMenuBar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

#endif
#if defined(__WXMAC__)
// wxMenuBar::MacSetCommonMenuBar
void wxMenuBar_MacSetCommonMenuBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *menubar;
  menubar = (wxMenuBar *) memenv->getPtr(env, argv[0], "menubar");
  wxMenuBar::MacSetCommonMenuBar(menubar);

}

#endif
// wxMenuBar::IsEnabled
void wxMenuBar_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEnabled(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuBar::Remove
void wxMenuBar_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->Remove(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenuBar::Replace
void wxMenuBar_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[2], "menu");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[3], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->Replace(pos,menu,title);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenuBar::SetHelpString
void wxMenuBar_SetHelpString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary helpString_bin;
  wxString helpString;
  if(!enif_inspect_binary(env, argv[2], &helpString_bin)) Badarg("helpString");
  helpString = wxString(helpString_bin.data, wxConvUTF8, helpString_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetHelpString(id,helpString);

}

// wxMenuBar::SetLabel
void wxMenuBar_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetLabel(id,label);

}

// wxMenuBar::SetMenuLabel
void wxMenuBar_SetMenuLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuBar *This;
  This = (wxMenuBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetMenuLabel(pos,label);

}

// wxMenuEvent::GetMenu
void wxMenuEvent_GetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuEvent *This;
  This = (wxMenuEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->GetMenu();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenuEvent::GetMenuId
void wxMenuEvent_GetMenuId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuEvent *This;
  This = (wxMenuEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMenuId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenuEvent::IsPopup
void wxMenuEvent_IsPopup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuEvent *This;
  This = (wxMenuEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsPopup();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuItem::wxMenuItem
void wxMenuItem_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenu * parentMenu=NULL;
  int id=wxID_SEPARATOR;
  wxString text= wxEmptyString;
  wxString help= wxEmptyString;
 wxItemKind kind=wxITEM_NORMAL;
  wxMenu * subMenu=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "parentMenu"))) {
  parentMenu = (wxMenu *) memenv->getPtr(env, tpl[1], "parentMenu");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "id"))) {
  if(!enif_get_int(env, tpl[1], &id)) Badarg("id"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "text"))) {
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, tpl[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "help"))) {
  ErlNifBinary help_bin;
  if(!enif_inspect_binary(env, tpl[1], &help_bin)) Badarg("help");
  help = wxString(help_bin.data, wxConvUTF8, help_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "subMenu"))) {
  subMenu = (wxMenu *) memenv->getPtr(env, tpl[1], "subMenu");
    } else        Badarg("Options");
  };
  wxMenuItem * Result = new EwxMenuItem(parentMenu,id,text,help,kind,subMenu);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuItem"));

}

// wxMenuItem::Check
void wxMenuItem_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool check=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "check"))) {
  check = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Check(check);

}

// wxMenuItem::Enable
void wxMenuItem_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
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
  This->Enable(enable);

}

// wxMenuItem::GetBitmap
void wxMenuItem_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmap()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxMenuItem::GetHelp
void wxMenuItem_GetHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetHelp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuItem::GetId
void wxMenuItem_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenuItem::GetKind
void wxMenuItem_GetKind(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKind();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMenuItem::GetLabelText
void wxMenuItem_GetLabelText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[0], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  wxString Result = wxMenuItem::GetLabelText(text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuItem::GetItemLabel
void wxMenuItem_GetItemLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetItemLabel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuItem::GetItemLabelText
void wxMenuItem_GetItemLabelText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetItemLabelText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMenuItem::GetMenu
void wxMenuItem_GetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->GetMenu();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenuItem::GetSubMenu
void wxMenuItem_GetSubMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->GetSubMenu();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxMenuItem::IsCheckable
void wxMenuItem_IsCheckable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCheckable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuItem::IsChecked
void wxMenuItem_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsChecked();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuItem::IsEnabled
void wxMenuItem_IsEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuItem::IsSeparator
void wxMenuItem_IsSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSeparator();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuItem::IsSubMenu
void wxMenuItem_IsSubMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSubMenu();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMenuItem::SetBitmap
void wxMenuItem_SetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  if(!This) throw wxe_badarg("This");
  This->SetBitmap(*bmp);

}

// wxMenuItem::SetHelp
void wxMenuItem_SetHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary helpString_bin;
  wxString helpString;
  if(!enif_inspect_binary(env, argv[1], &helpString_bin)) Badarg("helpString");
  helpString = wxString(helpString_bin.data, wxConvUTF8, helpString_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetHelp(helpString);

}

// wxMenuItem::SetMenu
void wxMenuItem_SetMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[1], "menu");
  if(!This) throw wxe_badarg("This");
  This->SetMenu(menu);

}

// wxMenuItem::SetSubMenu
void wxMenuItem_SetSubMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[1], "menu");
  if(!This) throw wxe_badarg("This");
  This->SetSubMenu(menu);

}

// wxMenuItem::SetItemLabel
void wxMenuItem_SetItemLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMenuItem *This;
  This = (wxMenuItem *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetItemLabel(label);

}

// wxMessageDialog::wxMessageDialog
void wxMessageDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString caption= wxMessageBoxCaptionStr;
  long style=wxOK|wxCENTRE;
  wxPoint pos= wxDefaultPosition;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "caption"))) {
  ErlNifBinary caption_bin;
  if(!enif_inspect_binary(env, tpl[1], &caption_bin)) Badarg("caption");
  caption = wxString(caption_bin.data, wxConvUTF8, caption_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
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
  wxMessageDialog * Result = new EwxMessageDialog(parent,message,caption,style,pos);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMessageDialog"));

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

// wxMouseCaptureChangedEvent::GetCapturedWindow
void wxMouseCaptureChangedEvent_GetCapturedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseCaptureChangedEvent *This;
  This = (wxMouseCaptureChangedEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCapturedWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxMouseEvent::AltDown
void wxMouseEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AltDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Button
void wxMouseEvent_Button(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  wxMouseButton but;
  if(!enif_get_int(env, argv[1], (int *) &but)) Badarg("but"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->Button(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::ButtonDClick
void wxMouseEvent_ButtonDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxMouseButton but=wxMOUSE_BTN_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "but"))) {
  if(!enif_get_int(env, tpl[1], (int *) &but)) Badarg("but"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ButtonDClick(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::ButtonDown
void wxMouseEvent_ButtonDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxMouseButton but=wxMOUSE_BTN_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "but"))) {
  if(!enif_get_int(env, tpl[1], (int *) &but)) Badarg("but"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ButtonDown(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::ButtonUp
void wxMouseEvent_ButtonUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxMouseButton but=wxMOUSE_BTN_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "but"))) {
  if(!enif_get_int(env, tpl[1], (int *) &but)) Badarg("but"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ButtonUp(but);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::CmdDown
void wxMouseEvent_CmdDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CmdDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::ControlDown
void wxMouseEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ControlDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Dragging
void wxMouseEvent_Dragging(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Dragging();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Entering
void wxMouseEvent_Entering(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Entering();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::GetButton
void wxMouseEvent_GetButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::GetPosition
void wxMouseEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMouseEvent::GetLogicalPosition
void wxMouseEvent_GetLogicalPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetLogicalPosition(*dc);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMouseEvent::GetLinesPerAction
void wxMouseEvent_GetLinesPerAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLinesPerAction();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::GetWheelRotation
void wxMouseEvent_GetWheelRotation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWheelRotation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::GetWheelDelta
void wxMouseEvent_GetWheelDelta(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWheelDelta();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::GetX
void wxMouseEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::GetY
void wxMouseEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::IsButton
void wxMouseEvent_IsButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::IsPageScroll
void wxMouseEvent_IsPageScroll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsPageScroll();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Leaving
void wxMouseEvent_Leaving(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Leaving();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::LeftDClick
void wxMouseEvent_LeftDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->LeftDClick();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::LeftDown
void wxMouseEvent_LeftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->LeftDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::LeftIsDown
void wxMouseEvent_LeftIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->LeftIsDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::LeftUp
void wxMouseEvent_LeftUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->LeftUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::MetaDown
void wxMouseEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MetaDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::MiddleDClick
void wxMouseEvent_MiddleDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MiddleDClick();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::MiddleDown
void wxMouseEvent_MiddleDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MiddleDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::MiddleIsDown
void wxMouseEvent_MiddleIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MiddleIsDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::MiddleUp
void wxMouseEvent_MiddleUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MiddleUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Moving
void wxMouseEvent_Moving(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Moving();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::RightDClick
void wxMouseEvent_RightDClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RightDClick();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::RightDown
void wxMouseEvent_RightDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RightDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::RightIsDown
void wxMouseEvent_RightIsDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RightIsDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::RightUp
void wxMouseEvent_RightUp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RightUp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::ShiftDown
void wxMouseEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShiftDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::GetWheelAxis
void wxMouseEvent_GetWheelAxis(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWheelAxis();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxMouseEvent::Aux1DClick
void wxMouseEvent_Aux1DClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Aux1DClick();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Aux1Down
void wxMouseEvent_Aux1Down(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Aux1Down();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Aux1Up
void wxMouseEvent_Aux1Up(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Aux1Up();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Aux2DClick
void wxMouseEvent_Aux2DClick(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Aux2DClick();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Aux2Down
void wxMouseEvent_Aux2Down(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Aux2Down();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMouseEvent::Aux2Up
void wxMouseEvent_Aux2Up(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMouseEvent *This;
  This = (wxMouseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Aux2Up();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxMoveEvent::GetPosition
void wxMoveEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMoveEvent *This;
  This = (wxMoveEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMoveEvent::GetRect
void wxMoveEvent_GetRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMoveEvent *This;
  This = (wxMoveEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMultiChoiceDialog::wxMultiChoiceDialog
void wxMultiChoiceDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxMultiChoiceDialog * Result = new EwxMultiChoiceDialog(parent,message,caption,choices,style,pos);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMultiChoiceDialog"));

}

// wxMultiChoiceDialog::GetSelections
void wxMultiChoiceDialog_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMultiChoiceDialog *This;
  This = (wxMultiChoiceDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxArrayInt Result = This->GetSelections();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxMultiChoiceDialog::SetSelections
void wxMultiChoiceDialog_SetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxMultiChoiceDialog *This;
  This = (wxMultiChoiceDialog *) memenv->getPtr(env, argv[0], "This");
  wxArrayInt selections;
  int selections_tmp;
  ERL_NIF_TERM selectionsHead, selectionsTail;
  selectionsTail = argv[1];
  while(!enif_is_empty_list(env, selectionsTail)) {
    if(!enif_get_list_cell(env, selectionsTail, &selectionsHead, &selectionsTail)) Badarg("selections");
    if(!enif_get_int(env, selectionsHead, &selections_tmp)) Badarg("selections");
    selections.Add(selections_tmp);
  };
  if(!This) throw wxe_badarg("This");
  This->SetSelections(selections);

}

// wxNavigationKeyEvent::GetDirection
void wxNavigationKeyEvent_GetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetDirection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxNavigationKeyEvent::SetDirection
void wxNavigationKeyEvent_SetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  bool direction;
  direction = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetDirection(direction);

}

// wxNavigationKeyEvent::IsWindowChange
void wxNavigationKeyEvent_IsWindowChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsWindowChange();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxNavigationKeyEvent::SetWindowChange
void wxNavigationKeyEvent_SetWindowChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  bool windowChange;
  windowChange = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetWindowChange(windowChange);

}

// wxNavigationKeyEvent::IsFromTab
void wxNavigationKeyEvent_IsFromTab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFromTab();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxNavigationKeyEvent::SetFromTab
void wxNavigationKeyEvent_SetFromTab(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  bool fromTab;
  fromTab = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetFromTab(fromTab);

}

// wxNavigationKeyEvent::GetCurrentFocus
void wxNavigationKeyEvent_GetCurrentFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentFocus();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxNavigationKeyEvent::SetCurrentFocus
void wxNavigationKeyEvent_SetCurrentFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNavigationKeyEvent *This;
  This = (wxNavigationKeyEvent *) memenv->getPtr(env, argv[0], "This");
  wxWindow *currentFocus;
  currentFocus = (wxWindow *) memenv->getPtr(env, argv[1], "currentFocus");
  if(!This) throw wxe_badarg("This");
  This->SetCurrentFocus(currentFocus);

}

// wxNotebook::wxNotebook
void wxNotebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxNotebook * Result = new EwxNotebook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxNotebook"));

}

// wxNotebook::wxNotebook
void wxNotebook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxNotebook * Result = new EwxNotebook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxNotebook"));

}

// wxNotebook::AssignImageList
void wxNotebook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxNotebook::Create
void wxNotebook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
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

// wxNotebook::GetImageList
void wxNotebook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxNotebook::GetPageImage
void wxNotebook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageImage(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxNotebook::GetRowCount
void wxNotebook_GetRowCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxNotebook::GetThemeBackgroundColour
void wxNotebook_GetThemeBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetThemeBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxNotebook::SetImageList
void wxNotebook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxNotebook::SetPadding
void wxNotebook_SetPadding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *padding_t;
  int padding_sz;
  if(!enif_get_tuple(env, argv[1], &padding_sz, &padding_t)) Badarg("padding");
  int paddingW;
  if(!enif_get_int(env, padding_t[0], &paddingW)) Badarg("padding");
  int paddingH;
  if(!enif_get_int(env, padding_t[1], &paddingH)) Badarg("padding");
  wxSize padding = wxSize(paddingW,paddingH);
  if(!This) throw wxe_badarg("This");
  This->SetPadding(padding);

}

// wxNotebook::SetPageSize
void wxNotebook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetPageSize(size);

}

// wxNotebook::SetPageImage
void wxNotebook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotebook *This;
  This = (wxNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageImage(page,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxNotificationMessage::wxNotificationMessage
void wxNotificationMessage_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxNotificationMessage * Result = new EwxNotificationMessage();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxNotificationMessage"));

}

// wxNotificationMessage::wxNotificationMessage
void wxNotificationMessage_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString message= wxEmptyString;
  wxWindow * parent=NULL;
  int flags=wxICON_INFORMATION;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[0], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "parent"))) {
  parent = (wxWindow *) memenv->getPtr(env, tpl[1], "parent");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  wxNotificationMessage * Result = new EwxNotificationMessage(title,message,parent,flags);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxNotificationMessage"));

}

#if wxCHECK_VERSION(3,1,0)
// wxNotificationMessage::AddAction
void wxNotificationMessage_AddAction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString label= wxString();
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  int actionid;
  if(!enif_get_int(env, argv[1], &actionid)) Badarg("actionid"); // wxWindowID
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
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddAction(actionid,label);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
// wxNotificationMessage::Close
void wxNotificationMessage_Close(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Close();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxNotificationMessage::SetFlags
void wxNotificationMessage_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  int flags;
  if(!enif_get_int(env, argv[1], &flags)) Badarg("flags"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

#if wxCHECK_VERSION(3,1,0)
// wxNotificationMessage::SetIcon
void wxNotificationMessage_SetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  if(!This) throw wxe_badarg("This");
  This->SetIcon(*icon);

}

#endif
// wxNotificationMessage::SetMessage
void wxNotificationMessage_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetMessage(message);

}

// wxNotificationMessage::SetParent
void wxNotificationMessage_SetParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  if(!This) throw wxe_badarg("This");
  This->SetParent(parent);

}

// wxNotificationMessage::SetTitle
void wxNotificationMessage_SetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[1], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetTitle(title);

}

// wxNotificationMessage::Show
void wxNotificationMessage_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int timeout=wxNotificationMessage::Timeout_Auto;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotificationMessage *This;
  This = (wxNotificationMessage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "timeout"))) {
  if(!enif_get_int(env, tpl[1], &timeout)) Badarg("timeout"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Show(timeout);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if __WXMSW__ 
// wxNotificationMessage::UseTaskBarIcon
void wxNotificationMessage_UseTaskBarIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTaskBarIcon *icon;
  icon = (wxTaskBarIcon *) memenv->getPtr(env, argv[0], "icon");
  wxTaskBarIcon * Result = (wxTaskBarIcon*)wxNotificationMessage::UseTaskBarIcon(icon);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTaskBarIcon"));

}

#endif
#if __WXMSW__ && wxCHECK_VERSION(3,1,0)
// wxNotificationMessage::MSWUseToasts
void wxNotificationMessage_MSWUseToasts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString shortcutPath= wxString();
  wxString appId= wxString();
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "shortcutPath"))) {
  ErlNifBinary shortcutPath_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortcutPath_bin)) Badarg("shortcutPath");
  shortcutPath = wxString(shortcutPath_bin.data, wxConvUTF8, shortcutPath_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "appId"))) {
  ErlNifBinary appId_bin;
  if(!enif_inspect_binary(env, tpl[1], &appId_bin)) Badarg("appId");
  appId = wxString(appId_bin.data, wxConvUTF8, appId_bin.size);
    } else        Badarg("Options");
  };
  bool Result = wxNotificationMessage::MSWUseToasts(shortcutPath,appId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif
// wxNotifyEvent::Allow
void wxNotifyEvent_Allow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotifyEvent *This;
  This = (wxNotifyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Allow();

}

// wxNotifyEvent::IsAllowed
void wxNotifyEvent_IsAllowed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotifyEvent *This;
  This = (wxNotifyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsAllowed();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxNotifyEvent::Veto
void wxNotifyEvent_Veto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxNotifyEvent *This;
  This = (wxNotifyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Veto();

}

// wxOverlay::wxOverlay
void wxOverlay_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxOverlay * Result = new wxOverlay();
  app->newPtr((void *) Result, 239, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxOverlay"));

}

// wxOverlay::~wxOverlay
void wxOverlay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxOverlay *This;
  This = (wxOverlay *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxOverlay::Reset
void wxOverlay_Reset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxOverlay *This;
  This = (wxOverlay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Reset();

}

// wxPageSetupDialog::wxPageSetupDialog
void wxPageSetupDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPageSetupDialogData * data=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxPageSetupDialogData *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  wxPageSetupDialog * Result = new EwxPageSetupDialog(parent,data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPageSetupDialog"));

}

// wxPageSetupDialog::GetPageSetupData
void wxPageSetupDialog_GetPageSetupData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialog *This;
  This = (wxPageSetupDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPageSetupDialogData * Result = &This->GetPageSetupData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPageSetupDialogData"));

}

// wxPageSetupDialog::ShowModal
void wxPageSetupDialog_ShowModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialog *This;
  This = (wxPageSetupDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->ShowModal();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPageSetupDialogData::wxPageSetupDialogData
void wxPageSetupDialogData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPageSetupDialogData * Result = new EwxPageSetupDialogData();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPageSetupDialogData"));

}

// wxPageSetupDialogData::wxPageSetupDialogData
void wxPageSetupDialogData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM printData_type;
  void * printData = memenv->getPtr(env, argv[0], "printData", &printData_type);
  wxPageSetupDialogData * Result;
  if(enif_is_identical(printData_type, WXE_ATOM_wxPrintData))
    Result = new EwxPageSetupDialogData(* static_cast<wxPrintData*> (printData));
  else if(enif_is_identical(printData_type, WXE_ATOM_wxPageSetupDialogData))
    Result = new EwxPageSetupDialogData(* static_cast<wxPageSetupDialogData*> (printData));
  else throw wxe_badarg("printData");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPageSetupDialogData"));

}

// wxPageSetupDialogData::EnableHelp
void wxPageSetupDialogData_EnableHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableHelp(flag);

}

// wxPageSetupDialogData::EnableMargins
void wxPageSetupDialogData_EnableMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableMargins(flag);

}

// wxPageSetupDialogData::EnableOrientation
void wxPageSetupDialogData_EnableOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableOrientation(flag);

}

// wxPageSetupDialogData::EnablePaper
void wxPageSetupDialogData_EnablePaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnablePaper(flag);

}

// wxPageSetupDialogData::EnablePrinter
void wxPageSetupDialogData_EnablePrinter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnablePrinter(flag);

}

// wxPageSetupDialogData::GetDefaultMinMargins
void wxPageSetupDialogData_GetDefaultMinMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetDefaultMinMargins();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetEnableMargins
void wxPageSetupDialogData_GetEnableMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnableMargins();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetEnableOrientation
void wxPageSetupDialogData_GetEnableOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnableOrientation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetEnablePaper
void wxPageSetupDialogData_GetEnablePaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnablePaper();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetEnablePrinter
void wxPageSetupDialogData_GetEnablePrinter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnablePrinter();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetEnableHelp
void wxPageSetupDialogData_GetEnableHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnableHelp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetDefaultInfo
void wxPageSetupDialogData_GetDefaultInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetDefaultInfo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::GetMarginTopLeft
void wxPageSetupDialogData_GetMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetMarginTopLeft();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPageSetupDialogData::GetMarginBottomRight
void wxPageSetupDialogData_GetMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetMarginBottomRight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPageSetupDialogData::GetMinMarginTopLeft
void wxPageSetupDialogData_GetMinMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetMinMarginTopLeft();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPageSetupDialogData::GetMinMarginBottomRight
void wxPageSetupDialogData_GetMinMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetMinMarginBottomRight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPageSetupDialogData::GetPaperId
void wxPageSetupDialogData_GetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPaperId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPageSetupDialogData::GetPaperSize
void wxPageSetupDialogData_GetPaperSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetPaperSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPageSetupDialogData::GetPrintData
void wxPageSetupDialogData_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxPrintData * Result = &This->GetPrintData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintData"));

}

// wxPageSetupDialogData::IsOk
void wxPageSetupDialogData_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPageSetupDialogData::SetDefaultInfo
void wxPageSetupDialogData_SetDefaultInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultInfo(flag);

}

// wxPageSetupDialogData::SetDefaultMinMargins
void wxPageSetupDialogData_SetDefaultMinMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultMinMargins(flag);

}

// wxPageSetupDialogData::SetMarginTopLeft
void wxPageSetupDialogData_SetMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->SetMarginTopLeft(pt);

}

// wxPageSetupDialogData::SetMarginBottomRight
void wxPageSetupDialogData_SetMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->SetMarginBottomRight(pt);

}

// wxPageSetupDialogData::SetMinMarginTopLeft
void wxPageSetupDialogData_SetMinMarginTopLeft(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->SetMinMarginTopLeft(pt);

}

// wxPageSetupDialogData::SetMinMarginBottomRight
void wxPageSetupDialogData_SetMinMarginBottomRight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->SetMinMarginBottomRight(pt);

}

// wxPageSetupDialogData::SetPaperId
void wxPageSetupDialogData_SetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  wxPaperSize id;
  if(!enif_get_int(env, argv[1], (int *) &id)) Badarg("id"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetPaperId(id);

}

// wxPageSetupDialogData::SetPaperSize
void wxPageSetupDialogData_SetPaperSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetPaperSize(size);

}

// wxPageSetupDialogData::SetPrintData
void wxPageSetupDialogData_SetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPageSetupDialogData *This;
  This = (wxPageSetupDialogData *) memenv->getPtr(env, argv[0], "This");
  wxPrintData *printData;
  printData = (wxPrintData *) memenv->getPtr(env, argv[1], "printData");
  if(!This) throw wxe_badarg("This");
  This->SetPrintData(*printData);

}

// wxPaintDC::wxPaintDC
void wxPaintDC_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  wxPaintDC * Result = new EwxPaintDC(window);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPaintDC"));

}

// wxPalette::wxPalette
void wxPalette_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPalette * Result = new EwxPalette();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPalette"));

}

// wxPalette::wxPalette
void wxPalette_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPalette *palette;
  palette = (wxPalette *) memenv->getPtr(env, argv[0], "palette");
  wxPalette * Result = new EwxPalette(*palette);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPalette"));

}

// wxPalette::wxPalette
void wxPalette_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  unsigned const char * red;
  ErlNifBinary red_bin;
  if(!enif_inspect_binary(env, argv[0], &red_bin)) Badarg("red");
  red = (unsigned const char*) red_bin.data;
  unsigned const char * green;
  ErlNifBinary green_bin;
  if(!enif_inspect_binary(env, argv[1], &green_bin)) Badarg("green");
  green = (unsigned const char*) green_bin.data;
  unsigned const char * blue;
  ErlNifBinary blue_bin;
  if(!enif_inspect_binary(env, argv[2], &blue_bin)) Badarg("blue");
  blue = (unsigned const char*) blue_bin.data;
  wxPalette * Result = new EwxPalette(red_bin.size,red,green,blue);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPalette"));

}

// wxPalette::Create
void wxPalette_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPalette *This;
  This = (wxPalette *) memenv->getPtr(env, argv[0], "This");
  unsigned const char * red;
  ErlNifBinary red_bin;
  if(!enif_inspect_binary(env, argv[1], &red_bin)) Badarg("red");
  red = (unsigned const char*) red_bin.data;
  unsigned const char * green;
  ErlNifBinary green_bin;
  if(!enif_inspect_binary(env, argv[2], &green_bin)) Badarg("green");
  green = (unsigned const char*) green_bin.data;
  unsigned const char * blue;
  ErlNifBinary blue_bin;
  if(!enif_inspect_binary(env, argv[3], &blue_bin)) Badarg("blue");
  blue = (unsigned const char*) blue_bin.data;
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(red_bin.size,red,green,blue);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPalette::GetColoursCount
void wxPalette_GetColoursCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPalette *This;
  This = (wxPalette *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColoursCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPalette::GetPixel
void wxPalette_GetPixel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPalette *This;
  This = (wxPalette *) memenv->getPtr(env, argv[0], "This");
  unsigned int red;
  if(!enif_get_uint(env, argv[1], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[2], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[3], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPixel(red,green,blue);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPalette::GetRGB
void wxPalette_GetRGB(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned char red;
  unsigned char green;
  unsigned char blue;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPalette *This;
  This = (wxPalette *) memenv->getPtr(env, argv[0], "This");
  int pixel;
  if(!enif_get_int(env, argv[1], &pixel)) Badarg("pixel"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetRGB(pixel,&red,&green,&blue);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_bool(Result),
    rt.make_uint(red),
  rt.make_uint(green),
  rt.make_uint(blue));
  rt.send(msg);

}

// wxPalette::IsOk
void wxPalette_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPalette *This;
  This = (wxPalette *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPaletteChangedEvent::SetChangedWindow
void wxPaletteChangedEvent_SetChangedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPaletteChangedEvent *This;
  This = (wxPaletteChangedEvent *) memenv->getPtr(env, argv[0], "This");
  wxWindow *win;
  win = (wxWindow *) memenv->getPtr(env, argv[1], "win");
  if(!This) throw wxe_badarg("This");
  This->SetChangedWindow(win);

}

// wxPaletteChangedEvent::GetChangedWindow
void wxPaletteChangedEvent_GetChangedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPaletteChangedEvent *This;
  This = (wxPaletteChangedEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetChangedWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

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

// wxPasswordEntryDialog::wxPasswordEntryDialog
void wxPasswordEntryDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString caption= wxGetPasswordFromUserPromptStr;
  wxString value= wxEmptyString;
  long style=wxTextEntryDialogStyle;
  wxPoint pos= wxDefaultPosition;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "caption"))) {
  ErlNifBinary caption_bin;
  if(!enif_inspect_binary(env, tpl[1], &caption_bin)) Badarg("caption");
  caption = wxString(caption_bin.data, wxConvUTF8, caption_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "value"))) {
  ErlNifBinary value_bin;
  if(!enif_inspect_binary(env, tpl[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
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
  wxPasswordEntryDialog * Result = new EwxPasswordEntryDialog(parent,message,caption,value,style,pos);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPasswordEntryDialog"));

}

// wxPen::wxPen
void wxPen_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPen * Result = new EwxPen();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPen"));

}

// wxPen::wxPen
void wxPen_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width=1;
 wxPenStyle style=wxPENSTYLE_SOLID;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *colour_t;
  int colour_sz;
  if(!enif_get_tuple(env, argv[0], &colour_sz, &colour_t)) Badarg("colour");
  int colourR;
  if(!enif_get_int(env, colour_t[0], &colourR)) Badarg("colour");
  int colourG;
  if(!enif_get_int(env, colour_t[1], &colourG)) Badarg("colour");
  int colourB;
  if(!enif_get_int(env, colour_t[2], &colourB)) Badarg("colour");
  int colourA;
  if(!enif_get_int(env, colour_t[3], &colourA)) Badarg("colour");
  wxColour colour = wxColour(colourR,colourG,colourB,colourA);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], (int *) &style)) Badarg("style"); // enum
    } else        Badarg("Options");
  };
  wxPen * Result = new EwxPen(colour,width,style);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPen"));

}

// wxPen::wxPen
void wxPen_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *pen;
  pen = (wxPen *) memenv->getPtr(env, argv[0], "pen");
  wxPen * Result = new EwxPen(*pen);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPen"));

}

// wxPen::GetCap
void wxPen_GetCap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCap();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPen::GetColour
void wxPen_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPen::GetJoin
void wxPen_GetJoin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetJoin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPen::GetStyle
void wxPen_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPen::GetWidth
void wxPen_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPen::IsOk
void wxPen_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPen::SetCap
void wxPen_SetCap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  wxPenCap capStyle;
  if(!enif_get_int(env, argv[1], (int *) &capStyle)) Badarg("capStyle"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetCap(capStyle);

}

// wxPen::SetColour
void wxPen_SetColour_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
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
  This->SetColour(colour);

}

// wxPen::SetColour
void wxPen_SetColour_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  unsigned int red;
  if(!enif_get_uint(env, argv[1], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[2], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[3], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  This->SetColour(red,green,blue);

}

// wxPen::SetJoin
void wxPen_SetJoin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  wxPenJoin join_style;
  if(!enif_get_int(env, argv[1], (int *) &join_style)) Badarg("join_style"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetJoin(join_style);

}

// wxPen::SetStyle
void wxPen_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  wxPenStyle style;
  if(!enif_get_int(env, argv[1], (int *) &style)) Badarg("style"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetStyle(style);

}

// wxPen::SetWidth
void wxPen_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPen *This;
  This = (wxPen *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetWidth(width);

}

// wxPickerBase::SetInternalMargin
void wxPickerBase_SetInternalMargin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  int margin;
  if(!enif_get_int(env, argv[1], &margin)) Badarg("margin"); // int
  if(!This) throw wxe_badarg("This");
  This->SetInternalMargin(margin);

}

// wxPickerBase::GetInternalMargin
void wxPickerBase_GetInternalMargin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetInternalMargin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPickerBase::SetTextCtrlProportion
void wxPickerBase_SetTextCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  int prop;
  if(!enif_get_int(env, argv[1], &prop)) Badarg("prop"); // int
  if(!This) throw wxe_badarg("This");
  This->SetTextCtrlProportion(prop);

}

// wxPickerBase::SetPickerCtrlProportion
void wxPickerBase_SetPickerCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  int prop;
  if(!enif_get_int(env, argv[1], &prop)) Badarg("prop"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPickerCtrlProportion(prop);

}

// wxPickerBase::GetTextCtrlProportion
void wxPickerBase_GetTextCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetTextCtrlProportion();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPickerBase::GetPickerCtrlProportion
void wxPickerBase_GetPickerCtrlProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPickerCtrlProportion();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPickerBase::HasTextCtrl
void wxPickerBase_HasTextCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextCtrl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPickerBase::GetTextCtrl
void wxPickerBase_GetTextCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->GetTextCtrl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxPickerBase::IsTextCtrlGrowable
void wxPickerBase_IsTextCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsTextCtrlGrowable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPickerBase::SetPickerCtrlGrowable
void wxPickerBase_SetPickerCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool grow=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "grow"))) {
  grow = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetPickerCtrlGrowable(grow);

}

// wxPickerBase::SetTextCtrlGrowable
void wxPickerBase_SetTextCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool grow=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "grow"))) {
  grow = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetTextCtrlGrowable(grow);

}

// wxPickerBase::IsPickerCtrlGrowable
void wxPickerBase_IsPickerCtrlGrowable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPickerBase *This;
  This = (wxPickerBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsPickerCtrlGrowable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxUSE_POPUPWIN
// wxPopupTransientWindow::wxPopupTransientWindow
void wxPopupTransientWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPopupTransientWindow * Result = new EwxPopupTransientWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPopupTransientWindow"));

}

// wxPopupTransientWindow::wxPopupTransientWindow
void wxPopupTransientWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=wxBORDER_NONE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  wxPopupTransientWindow * Result = new EwxPopupTransientWindow(parent,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPopupTransientWindow"));

}

// wxPopupTransientWindow::Popup
void wxPopupTransientWindow_Popup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * focus=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPopupTransientWindow *This;
  This = (wxPopupTransientWindow *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "focus"))) {
  focus = (wxWindow *) memenv->getPtr(env, tpl[1], "focus");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Popup(focus);

}

// wxPopupTransientWindow::Dismiss
void wxPopupTransientWindow_Dismiss(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPopupTransientWindow *This;
  This = (wxPopupTransientWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Dismiss();

}

#endif // wxUSE_POPUPWIN
#if wxUSE_POPUPWIN
// wxPopupWindow::wxPopupWindow
void wxPopupWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPopupWindow * Result = new EwxPopupWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPopupWindow"));

}

// wxPopupWindow::wxPopupWindow
void wxPopupWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxBORDER_NONE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  wxPopupWindow * Result = new EwxPopupWindow(parent,flags);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPopupWindow"));

}

// wxPopupWindow::Create
void wxPopupWindow_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxBORDER_NONE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPopupWindow *This;
  This = (wxPopupWindow *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPopupWindow::Position
void wxPopupWindow_Position(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPopupWindow *This;
  This = (wxPopupWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *ptOrigin_t;
  int ptOrigin_sz;
  if(!enif_get_tuple(env, argv[1], &ptOrigin_sz, &ptOrigin_t)) Badarg("ptOrigin");
  int ptOriginX;
  if(!enif_get_int(env, ptOrigin_t[0], &ptOriginX)) Badarg("ptOrigin");
  int ptOriginY;
  if(!enif_get_int(env, ptOrigin_t[1], &ptOriginY)) Badarg("ptOrigin");
  wxPoint ptOrigin = wxPoint(ptOriginX,ptOriginY);
  const ERL_NIF_TERM *sizePopup_t;
  int sizePopup_sz;
  if(!enif_get_tuple(env, argv[2], &sizePopup_sz, &sizePopup_t)) Badarg("sizePopup");
  int sizePopupW;
  if(!enif_get_int(env, sizePopup_t[0], &sizePopupW)) Badarg("sizePopup");
  int sizePopupH;
  if(!enif_get_int(env, sizePopup_t[1], &sizePopupH)) Badarg("sizePopup");
  wxSize sizePopup = wxSize(sizePopupW,sizePopupH);
  if(!This) throw wxe_badarg("This");
  This->Position(ptOrigin,sizePopup);

}

#endif // wxUSE_POPUPWIN
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
// wxPreviewControlBar::wxPreviewControlBar
void wxPreviewControlBar_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *preview;
  preview = (wxPrintPreview *) memenv->getPtr(env, argv[0], "preview");
  long buttons;
  if(!enif_get_long(env, argv[1], &buttons)) Badarg("buttons");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[2], "parent");
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
  wxPreviewControlBar * Result = new EwxPreviewControlBar(preview,buttons,parent,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPreviewControlBar"));

}

// wxPreviewControlBar::CreateButtons
void wxPreviewControlBar_CreateButtons(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewControlBar *This;
  This = (wxPreviewControlBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CreateButtons();

}

// wxPreviewControlBar::GetPrintPreview
void wxPreviewControlBar_GetPrintPreview(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewControlBar *This;
  This = (wxPreviewControlBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintPreview * Result = (wxPrintPreview*)This->GetPrintPreview();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintPreview"));

}

// wxPreviewControlBar::GetZoomControl
void wxPreviewControlBar_GetZoomControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewControlBar *This;
  This = (wxPreviewControlBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetZoomControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPreviewControlBar::SetZoomControl
void wxPreviewControlBar_SetZoomControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewControlBar *This;
  This = (wxPreviewControlBar *) memenv->getPtr(env, argv[0], "This");
  int percent;
  if(!enif_get_int(env, argv[1], &percent)) Badarg("percent"); // int
  if(!This) throw wxe_badarg("This");
  This->SetZoomControl(percent);

}

// wxPreviewFrame::wxPreviewFrame
void wxPreviewFrame_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString title= "Print Preview";
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_FRAME_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *preview;
  preview = (wxPrintPreview *) memenv->getPtr(env, argv[0], "preview");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "title"))) {
  ErlNifBinary title_bin;
  if(!enif_inspect_binary(env, tpl[1], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
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
  wxPreviewFrame * Result = new EwxPreviewFrame(preview,parent,title,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPreviewFrame"));

}

// wxPreviewFrame::CreateControlBar
void wxPreviewFrame_CreateControlBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewFrame *This;
  This = (wxPreviewFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CreateControlBar();

}

// wxPreviewFrame::CreateCanvas
void wxPreviewFrame_CreateCanvas(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewFrame *This;
  This = (wxPreviewFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CreateCanvas();

}

// wxPreviewFrame::Initialize
void wxPreviewFrame_Initialize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewFrame *This;
  This = (wxPreviewFrame *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Initialize();

}

// wxPreviewFrame::OnCloseWindow
void wxPreviewFrame_OnCloseWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPreviewFrame *This;
  This = (wxPreviewFrame *) memenv->getPtr(env, argv[0], "This");
  wxCloseEvent *event;
  event = (wxCloseEvent *) memenv->getPtr(env, argv[1], "event");
  if(!This) throw wxe_badarg("This");
  This->OnCloseWindow(*event);

}

// wxPrintData::wxPrintData
void wxPrintData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPrintData * Result = new EwxPrintData();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintData"));

}

// wxPrintData::wxPrintData
void wxPrintData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *data;
  data = (wxPrintData *) memenv->getPtr(env, argv[0], "data");
  wxPrintData * Result = new EwxPrintData(*data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintData"));

}

// wxPrintData::GetCollate
void wxPrintData_GetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetCollate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintData::GetBin
void wxPrintData_GetBin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintData::GetColour
void wxPrintData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintData::GetDuplex
void wxPrintData_GetDuplex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDuplex();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintData::GetNoCopies
void wxPrintData_GetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNoCopies();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintData::GetOrientation
void wxPrintData_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOrientation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintData::GetPaperId
void wxPrintData_GetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPaperId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintData::GetPrinterName
void wxPrintData_GetPrinterName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetPrinterName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPrintData::GetQuality
void wxPrintData_GetQuality(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintQuality Result = This->GetQuality();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintData::IsOk
void wxPrintData_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintData::SetBin
void wxPrintData_SetBin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  wxPrintBin flag;
  if(!enif_get_int(env, argv[1], (int *) &flag)) Badarg("flag"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetBin(flag);

}

// wxPrintData::SetCollate
void wxPrintData_SetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetCollate(flag);

}

// wxPrintData::SetColour
void wxPrintData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetColour(flag);

}

// wxPrintData::SetDuplex
void wxPrintData_SetDuplex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  wxDuplexMode mode;
  if(!enif_get_int(env, argv[1], (int *) &mode)) Badarg("mode"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetDuplex(mode);

}

// wxPrintData::SetNoCopies
void wxPrintData_SetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetNoCopies(n);

}

// wxPrintData::SetOrientation
void wxPrintData_SetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  wxPrintOrientation orientation;
  if(!enif_get_int(env, argv[1], (int *) &orientation)) Badarg("orientation"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetOrientation(orientation);

}

// wxPrintData::SetPaperId
void wxPrintData_SetPaperId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  wxPaperSize paperId;
  if(!enif_get_int(env, argv[1], (int *) &paperId)) Badarg("paperId"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetPaperId(paperId);

}

// wxPrintData::SetPrinterName
void wxPrintData_SetPrinterName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary printerName_bin;
  wxString printerName;
  if(!enif_inspect_binary(env, argv[1], &printerName_bin)) Badarg("printerName");
  printerName = wxString(printerName_bin.data, wxConvUTF8, printerName_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPrinterName(printerName);

}

// wxPrintData::SetQuality
void wxPrintData_SetQuality(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintData *This;
  This = (wxPrintData *) memenv->getPtr(env, argv[0], "This");
  int quality;
  if(!enif_get_int(env, argv[1], &quality)) Badarg("quality"); // wxPrintQuality
  if(!This) throw wxe_badarg("This");
  This->SetQuality(quality);

}

// wxPrintDialog::wxPrintDialog
void wxPrintDialog_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPrintDialogData * data=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxPrintDialogData *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  wxPrintDialog * Result = new EwxPrintDialog(parent,data);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintDialog"));

}

// wxPrintDialog::wxPrintDialog
void wxPrintDialog_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  wxPrintData *data;
  data = (wxPrintData *) memenv->getPtr(env, argv[1], "data");
  wxPrintDialog * Result = new EwxPrintDialog(parent,data);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintDialog"));

}

// wxPrintDialog::GetPrintDialogData
void wxPrintDialog_GetPrintDialogData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialog *This;
  This = (wxPrintDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintDialogData * Result = &This->GetPrintDialogData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintDialogData"));

}

// wxPrintDialog::GetPrintDC
void wxPrintDialog_GetPrintDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialog *This;
  This = (wxPrintDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDC * Result = (wxDC*)This->GetPrintDC();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDC"));

}

// wxPrintDialogData::wxPrintDialogData
void wxPrintDialogData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPrintDialogData * Result = new EwxPrintDialogData();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintDialogData"));

}

// wxPrintDialogData::wxPrintDialogData
void wxPrintDialogData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM dialogData_type;
  void * dialogData = memenv->getPtr(env, argv[0], "dialogData", &dialogData_type);
  wxPrintDialogData * Result;
  if(enif_is_identical(dialogData_type, WXE_ATOM_wxPrintDialogData))
    Result = new EwxPrintDialogData(* static_cast<wxPrintDialogData*> (dialogData));
  else if(enif_is_identical(dialogData_type, WXE_ATOM_wxPrintData))
    Result = new EwxPrintDialogData(* static_cast<wxPrintData*> (dialogData));
  else throw wxe_badarg("dialogData");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintDialogData"));

}

// wxPrintDialogData::EnableHelp
void wxPrintDialogData_EnableHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableHelp(flag);

}

// wxPrintDialogData::EnablePageNumbers
void wxPrintDialogData_EnablePageNumbers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnablePageNumbers(flag);

}

// wxPrintDialogData::EnablePrintToFile
void wxPrintDialogData_EnablePrintToFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnablePrintToFile(flag);

}

// wxPrintDialogData::EnableSelection
void wxPrintDialogData_EnableSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableSelection(flag);

}

// wxPrintDialogData::GetAllPages
void wxPrintDialogData_GetAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintDialogData::GetCollate
void wxPrintDialogData_GetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetCollate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintDialogData::GetFromPage
void wxPrintDialogData_GetFromPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFromPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintDialogData::GetMaxPage
void wxPrintDialogData_GetMaxPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaxPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintDialogData::GetMinPage
void wxPrintDialogData_GetMinPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMinPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintDialogData::GetNoCopies
void wxPrintDialogData_GetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNoCopies();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintDialogData::GetPrintData
void wxPrintDialogData_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintData * Result = &This->GetPrintData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintData"));

}

// wxPrintDialogData::GetPrintToFile
void wxPrintDialogData_GetPrintToFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetPrintToFile();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintDialogData::GetSelection
void wxPrintDialogData_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintDialogData::GetToPage
void wxPrintDialogData_GetToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetToPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintDialogData::IsOk
void wxPrintDialogData_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintDialogData::SetCollate
void wxPrintDialogData_SetCollate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetCollate(flag);

}

// wxPrintDialogData::SetFromPage
void wxPrintDialogData_SetFromPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFromPage(page);

}

// wxPrintDialogData::SetMaxPage
void wxPrintDialogData_SetMaxPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMaxPage(page);

}

// wxPrintDialogData::SetMinPage
void wxPrintDialogData_SetMinPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMinPage(page);

}

// wxPrintDialogData::SetNoCopies
void wxPrintDialogData_SetNoCopies(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetNoCopies(n);

}

// wxPrintDialogData::SetPrintData
void wxPrintDialogData_SetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  wxPrintData *printData;
  printData = (wxPrintData *) memenv->getPtr(env, argv[1], "printData");
  if(!This) throw wxe_badarg("This");
  This->SetPrintData(*printData);

}

// wxPrintDialogData::SetPrintToFile
void wxPrintDialogData_SetPrintToFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetPrintToFile(flag);

}

// wxPrintDialogData::SetSelection
void wxPrintDialogData_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetSelection(flag);

}

// wxPrintDialogData::SetToPage
void wxPrintDialogData_SetToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintDialogData *This;
  This = (wxPrintDialogData *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetToPage(page);

}

// wxPrintPreview::wxPrintPreview
void wxPrintPreview_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPrintout * printoutForPrinting=NULL;
  wxPrintDialogData * data=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *printout;
  printout = (wxPrintout *) memenv->getPtr(env, argv[0], "printout");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "printoutForPrinting"))) {
  printoutForPrinting = (wxPrintout *) memenv->getPtr(env, tpl[1], "printoutForPrinting");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxPrintDialogData *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  wxPrintPreview * Result = new EwxPrintPreview(printout,printoutForPrinting,data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintPreview"));

}

// wxPrintPreview::wxPrintPreview
void wxPrintPreview_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *printout;
  printout = (wxPrintout *) memenv->getPtr(env, argv[0], "printout");
  wxPrintout *printoutForPrinting;
  printoutForPrinting = (wxPrintout *) memenv->getPtr(env, argv[1], "printoutForPrinting");
  wxPrintData *data;
  data = (wxPrintData *) memenv->getPtr(env, argv[2], "data");
  wxPrintPreview * Result = new EwxPrintPreview(printout,printoutForPrinting,data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintPreview"));

}

// wxPrintPreview::GetCanvas
void wxPrintPreview_GetCanvas(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPreviewCanvas * Result = (wxPreviewCanvas*)This->GetCanvas();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPreviewCanvas"));

}

// wxPrintPreview::GetCurrentPage
void wxPrintPreview_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintPreview::GetFrame
void wxPrintPreview_GetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFrame * Result = (wxFrame*)This->GetFrame();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxPrintPreview::GetMaxPage
void wxPrintPreview_GetMaxPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaxPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintPreview::GetMinPage
void wxPrintPreview_GetMinPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMinPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrintPreview::GetPrintout
void wxPrintPreview_GetPrintout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintout * Result = (wxPrintout*)This->GetPrintout();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintout"));

}

// wxPrintPreview::GetPrintoutForPrinting
void wxPrintPreview_GetPrintoutForPrinting(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintout * Result = (wxPrintout*)This->GetPrintoutForPrinting();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintout"));

}

// wxPrintPreview::IsOk
void wxPrintPreview_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintPreview::PaintPage
void wxPrintPreview_PaintPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  wxPreviewCanvas *canvas;
  canvas = (wxPreviewCanvas *) memenv->getPtr(env, argv[1], "canvas");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[2], "dc");
  if(!This) throw wxe_badarg("This");
  bool Result = This->PaintPage(canvas,*dc);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintPreview::Print
void wxPrintPreview_Print(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  bool prompt;
  prompt = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Print(prompt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintPreview::RenderPage
void wxPrintPreview_RenderPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  int pageNum;
  if(!enif_get_int(env, argv[1], &pageNum)) Badarg("pageNum"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->RenderPage(pageNum);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintPreview::SetCanvas
void wxPrintPreview_SetCanvas(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  wxPreviewCanvas *window;
  window = (wxPreviewCanvas *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
  This->SetCanvas(window);

}

// wxPrintPreview::SetCurrentPage
void wxPrintPreview_SetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  int pageNum;
  if(!enif_get_int(env, argv[1], &pageNum)) Badarg("pageNum"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetCurrentPage(pageNum);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintPreview::SetFrame
void wxPrintPreview_SetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[1], "frame");
  if(!This) throw wxe_badarg("This");
  This->SetFrame(frame);

}

// wxPrintPreview::SetPrintout
void wxPrintPreview_SetPrintout(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  wxPrintout *printout;
  printout = (wxPrintout *) memenv->getPtr(env, argv[1], "printout");
  if(!This) throw wxe_badarg("This");
  This->SetPrintout(printout);

}

// wxPrintPreview::SetZoom
void wxPrintPreview_SetZoom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintPreview *This;
  This = (wxPrintPreview *) memenv->getPtr(env, argv[0], "This");
  int percent;
  if(!enif_get_int(env, argv[1], &percent)) Badarg("percent"); // int
  if(!This) throw wxe_badarg("This");
  This->SetZoom(percent);

}

// wxPrinter::wxPrinter
void wxPrinter_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPrintDialogData * data=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxPrintDialogData *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  wxPrinter * Result = new EwxPrinter(data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrinter"));

}

// wxPrinter::CreateAbortWindow
void wxPrinter_CreateAbortWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxPrintout *printout;
  printout = (wxPrintout *) memenv->getPtr(env, argv[2], "printout");
  if(!This) throw wxe_badarg("This");
  wxPrintAbortDialog * Result = (wxPrintAbortDialog*)This->CreateAbortWindow(parent,printout);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDialog"));

}

// wxPrinter::GetAbort
void wxPrinter_GetAbort(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetAbort();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrinter::GetLastError
void wxPrinter_GetLastError(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxPrinter::GetLastError();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxPrinter::GetPrintDialogData
void wxPrinter_GetPrintDialogData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintDialogData * Result = &This->GetPrintDialogData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintDialogData"));

}

// wxPrinter::Print
void wxPrinter_Print(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool prompt=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxPrintout *printout;
  printout = (wxPrintout *) memenv->getPtr(env, argv[2], "printout");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "prompt"))) {
  prompt = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Print(parent,printout,prompt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrinter::PrintDialog
void wxPrinter_PrintDialog(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  if(!This) throw wxe_badarg("This");
  wxDC * Result = (wxDC*)This->PrintDialog(parent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDC"));

}

// wxPrinter::ReportError
void wxPrinter_ReportError(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxPrintout *printout;
  printout = (wxPrintout *) memenv->getPtr(env, argv[2], "printout");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[3], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  if(!This) throw wxe_badarg("This");
  This->ReportError(parent,printout,message);

}

// wxPrinter::Setup
void wxPrinter_Setup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrinter *This;
  This = (wxPrinter *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Setup(parent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}


// wxPrintout::wxPrintout taylormade
void wxPrintout_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int onPreparePrinting=0,onBeginPrinting=0,onEndPrinting=0,onBeginDocument=0,
    onEndDocument=0,hasPage=0,getPageInfo=0;

  wxString title= wxT("Printout");
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary title_bin;
  int onPrintPage;
  if(!enif_inspect_binary(env, argv[0], &title_bin)) Badarg("title")
  wxString(title_bin.data, wxConvUTF8, title_bin.size);
  if(!enif_get_int(env, argv[1], &onPrintPage)) Badarg("onPrintPage");

  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;

  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "onPreparePrinting"))) {
      if(!enif_get_int(env, tpl[1], &onPreparePrinting)) Badarg("onPreparePrinting");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "onBeginPrinting"))) {
      if(!enif_get_int(env, tpl[1], &onBeginPrinting)) Badarg("onBeginPrinting");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "onEndPrinting"))) {
      if(!enif_get_int(env, tpl[1], &onEndPrinting)) Badarg("onEndPrinting");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "onBeginDocument"))) {
      if(!enif_get_int(env, tpl[1], &onBeginDocument)) Badarg("onBeginDocument");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "onEndDocument"))) {
      if(!enif_get_int(env, tpl[1], &onEndDocument)) Badarg("onEndDocument");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "hasPage"))) {
      if(!enif_get_int(env, tpl[1], &hasPage)) Badarg("hasPage");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "getPageInfo"))) {
      if(!enif_get_int(env, tpl[1], &getPageInfo)) Badarg("getPageInfo");
    } else        Badarg("Options");
  };
  EwxPrintout * Result = new EwxPrintout(title,
					 onPrintPage,
					 onPreparePrinting,
					 onBeginPrinting,onEndPrinting,
					 onBeginDocument,onEndDocument,
					 hasPage,getPageInfo);
  Result->me_ref = memenv->me_ref;
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintout"));
}

// wxPrintout::GetDC
void wxPrintout_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDC * Result = (wxDC*)This->GetDC();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDC"));

}

// wxPrintout::GetPageSizeMM
void wxPrintout_GetPageSizeMM(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int w;
  int h;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetPageSizeMM(&w,&h);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(w),
  rt.make_int(h));
  rt.send(msg);

}

// wxPrintout::GetPageSizePixels
void wxPrintout_GetPageSizePixels(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int w;
  int h;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetPageSizePixels(&w,&h);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(w),
  rt.make_int(h));
  rt.send(msg);

}

// wxPrintout::GetPaperRectPixels
void wxPrintout_GetPaperRectPixels(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetPaperRectPixels();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPrintout::GetPPIPrinter
void wxPrintout_GetPPIPrinter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int w;
  int h;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetPPIPrinter(&w,&h);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(w),
  rt.make_int(h));
  rt.send(msg);

}

// wxPrintout::GetPPIScreen
void wxPrintout_GetPPIScreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int w;
  int h;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetPPIScreen(&w,&h);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(w),
  rt.make_int(h));
  rt.send(msg);

}

// wxPrintout::GetTitle
void wxPrintout_GetTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetTitle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPrintout::IsPreview
void wxPrintout_IsPreview(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsPreview();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxPrintout::FitThisSizeToPaper
void wxPrintout_FitThisSizeToPaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *imageSize_t;
  int imageSize_sz;
  if(!enif_get_tuple(env, argv[1], &imageSize_sz, &imageSize_t)) Badarg("imageSize");
  int imageSizeW;
  if(!enif_get_int(env, imageSize_t[0], &imageSizeW)) Badarg("imageSize");
  int imageSizeH;
  if(!enif_get_int(env, imageSize_t[1], &imageSizeH)) Badarg("imageSize");
  wxSize imageSize = wxSize(imageSizeW,imageSizeH);
  if(!This) throw wxe_badarg("This");
  This->FitThisSizeToPaper(imageSize);

}

// wxPrintout::FitThisSizeToPage
void wxPrintout_FitThisSizeToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *imageSize_t;
  int imageSize_sz;
  if(!enif_get_tuple(env, argv[1], &imageSize_sz, &imageSize_t)) Badarg("imageSize");
  int imageSizeW;
  if(!enif_get_int(env, imageSize_t[0], &imageSizeW)) Badarg("imageSize");
  int imageSizeH;
  if(!enif_get_int(env, imageSize_t[1], &imageSizeH)) Badarg("imageSize");
  wxSize imageSize = wxSize(imageSizeW,imageSizeH);
  if(!This) throw wxe_badarg("This");
  This->FitThisSizeToPage(imageSize);

}

// wxPrintout::FitThisSizeToPageMargins
void wxPrintout_FitThisSizeToPageMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *imageSize_t;
  int imageSize_sz;
  if(!enif_get_tuple(env, argv[1], &imageSize_sz, &imageSize_t)) Badarg("imageSize");
  int imageSizeW;
  if(!enif_get_int(env, imageSize_t[0], &imageSizeW)) Badarg("imageSize");
  int imageSizeH;
  if(!enif_get_int(env, imageSize_t[1], &imageSizeH)) Badarg("imageSize");
  wxSize imageSize = wxSize(imageSizeW,imageSizeH);
  wxPageSetupDialogData *pageSetupData;
  pageSetupData = (wxPageSetupDialogData *) memenv->getPtr(env, argv[2], "pageSetupData");
  if(!This) throw wxe_badarg("This");
  This->FitThisSizeToPageMargins(imageSize,*pageSetupData);

}

// wxPrintout::MapScreenSizeToPaper
void wxPrintout_MapScreenSizeToPaper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->MapScreenSizeToPaper();

}

// wxPrintout::MapScreenSizeToPage
void wxPrintout_MapScreenSizeToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->MapScreenSizeToPage();

}

// wxPrintout::MapScreenSizeToPageMargins
void wxPrintout_MapScreenSizeToPageMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  wxPageSetupDialogData *pageSetupData;
  pageSetupData = (wxPageSetupDialogData *) memenv->getPtr(env, argv[1], "pageSetupData");
  if(!This) throw wxe_badarg("This");
  This->MapScreenSizeToPageMargins(*pageSetupData);

}

// wxPrintout::MapScreenSizeToDevice
void wxPrintout_MapScreenSizeToDevice(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->MapScreenSizeToDevice();

}

// wxPrintout::GetLogicalPaperRect
void wxPrintout_GetLogicalPaperRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetLogicalPaperRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPrintout::GetLogicalPageRect
void wxPrintout_GetLogicalPageRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetLogicalPageRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPrintout::GetLogicalPageMarginsRect
void wxPrintout_GetLogicalPageMarginsRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  wxPageSetupDialogData *pageSetupData;
  pageSetupData = (wxPageSetupDialogData *) memenv->getPtr(env, argv[1], "pageSetupData");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetLogicalPageMarginsRect(*pageSetupData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxPrintout::SetLogicalOrigin
void wxPrintout_SetLogicalOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  This->SetLogicalOrigin(x,y);

}

// wxPrintout::OffsetLogicalOrigin
void wxPrintout_OffsetLogicalOrigin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxPrintout *This;
  This = (wxPrintout *) memenv->getPtr(env, argv[0], "This");
  int xoff;
  if(!enif_get_int(env, argv[1], &xoff)) Badarg("xoff"); // wxCoord
  int yoff;
  if(!enif_get_int(env, argv[2], &yoff)) Badarg("yoff"); // wxCoord
  if(!This) throw wxe_badarg("This");
  This->OffsetLogicalOrigin(xoff,yoff);

}

// wxProgressDialog::wxProgressDialog
void wxProgressDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int maximum=100;
  wxWindow * parent=NULL;
  int style=wxPD_APP_MODAL|wxPD_AUTO_HIDE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary title_bin;
  wxString title;
  if(!enif_inspect_binary(env, argv[0], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "maximum"))) {
  if(!enif_get_int(env, tpl[1], &maximum)) Badarg("maximum"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "parent"))) {
  parent = (wxWindow *) memenv->getPtr(env, tpl[1], "parent");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  wxProgressDialog * Result = new EwxProgressDialog(title,message,maximum,parent,style);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxProgressDialog"));

}

// wxProgressDialog::Resume
void wxProgressDialog_Resume(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxProgressDialog *This;
  This = (wxProgressDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Resume();

}

// wxProgressDialog::Update
void wxProgressDialog_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString newmsg= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxProgressDialog *This;
  This = (wxProgressDialog *) memenv->getPtr(env, argv[0], "This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "newmsg"))) {
  ErlNifBinary newmsg_bin;
  if(!enif_inspect_binary(env, tpl[1], &newmsg_bin)) Badarg("newmsg");
  newmsg = wxString(newmsg_bin.data, wxConvUTF8, newmsg_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Update(value,newmsg);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxQueryNewPaletteEvent::SetPaletteRealized
void wxQueryNewPaletteEvent_SetPaletteRealized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxQueryNewPaletteEvent *This;
  This = (wxQueryNewPaletteEvent *) memenv->getPtr(env, argv[0], "This");
  bool realized;
  realized = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetPaletteRealized(realized);

}

// wxQueryNewPaletteEvent::GetPaletteRealized
void wxQueryNewPaletteEvent_GetPaletteRealized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxQueryNewPaletteEvent *This;
  This = (wxQueryNewPaletteEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetPaletteRealized();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioBox::wxRadioBox
void wxRadioBox_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int majorDim=0;
  long style=wxRA_SPECIFY_COLS;
  const wxValidator * val= &wxDefaultValidator;
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
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[3], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[4], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  wxArrayString choices;
  choicesTail = argv[5];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[6];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "majorDim"))) {
  if(!enif_get_int(env, tpl[1], &majorDim)) Badarg("majorDim"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "val"))) {
  val = (wxValidator *) memenv->getPtr(env, tpl[1], "val");
    } else        Badarg("Options");
  };
  wxRadioBox * Result = new EwxRadioBox(parent,id,label,pos,size,choices,majorDim,style,*val);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRadioBox"));

}

// wxRadioBox::Create
void wxRadioBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int majorDim=0;
  long style=wxRA_SPECIFY_COLS;
  const wxValidator * val= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[3], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[4], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[5], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  wxArrayString choices;
  choicesTail = argv[6];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[7];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "majorDim"))) {
  if(!enif_get_int(env, tpl[1], &majorDim)) Badarg("majorDim"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "val"))) {
  val = (wxValidator *) memenv->getPtr(env, tpl[1], "val");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,label,pos,size,choices,majorDim,style,*val);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioBox::Enable
void wxRadioBox_Enable_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
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

// wxRadioBox::Enable
void wxRadioBox_Enable_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
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
  bool Result = This->Enable(n,enable);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioBox::GetSelection
void wxRadioBox_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxRadioBox::GetString
void wxRadioBox_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetString(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxRadioBox::SetSelection
void wxRadioBox_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSelection(n);

}

// wxRadioBox::Show
void wxRadioBox_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int item;
  if(!enif_get_uint(env, argv[1], &item)) Badarg("item");
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
  if(!This) throw wxe_badarg("This");
  bool Result = This->Show(item,show);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioBox::GetColumnCount
void wxRadioBox_GetColumnCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumnCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxRadioBox::GetItemHelpText
void wxRadioBox_GetItemHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int item;
  if(!enif_get_uint(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetItemHelpText(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxRadioBox::GetItemToolTip
void wxRadioBox_GetItemToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int item;
  if(!enif_get_uint(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxToolTip * Result = (wxToolTip*)This->GetItemToolTip(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolTip"));

}

// wxRadioBox::GetItemFromPoint
void wxRadioBox_GetItemFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  int Result = This->GetItemFromPoint(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxRadioBox::GetRowCount
void wxRadioBox_GetRowCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRowCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxRadioBox::IsItemEnabled
void wxRadioBox_IsItemEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsItemEnabled(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioBox::IsItemShown
void wxRadioBox_IsItemShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsItemShown(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioBox::SetItemHelpText
void wxRadioBox_SetItemHelpText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int item;
  if(!enif_get_uint(env, argv[1], &item)) Badarg("item");
  ErlNifBinary helptext_bin;
  wxString helptext;
  if(!enif_inspect_binary(env, argv[2], &helptext_bin)) Badarg("helptext");
  helptext = wxString(helptext_bin.data, wxConvUTF8, helptext_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetItemHelpText(item,helptext);

}

// wxRadioBox::SetItemToolTip
void wxRadioBox_SetItemToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioBox *This;
  This = (wxRadioBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int item;
  if(!enif_get_uint(env, argv[1], &item)) Badarg("item");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetItemToolTip(item,text);

}

// wxRadioButton::wxRadioButton
void wxRadioButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRadioButton * Result = new EwxRadioButton();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRadioButton"));

}

// wxRadioButton::wxRadioButton
void wxRadioButton_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxRadioButton * Result = new EwxRadioButton(parent,id,label,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRadioButton"));

}

// wxRadioButton::Create
void wxRadioButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioButton *This;
  This = (wxRadioButton *) memenv->getPtr(env, argv[0], "This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,label,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioButton::GetValue
void wxRadioButton_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioButton *This;
  This = (wxRadioButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRadioButton::SetValue
void wxRadioButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRadioButton *This;
  This = (wxRadioButton *) memenv->getPtr(env, argv[0], "This");
  bool value;
  value = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetValue(value);

}

// wxRegion::wxRegion
void wxRegion_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRegion * Result = new EwxRegion();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRegion"));

}

// wxRegion::wxRegion
void wxRegion_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int x;
  if(!enif_get_int(env, argv[0], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[1], &y)) Badarg("y"); // wxCoord
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // wxCoord
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // wxCoord
  wxRegion * Result = new EwxRegion(x,y,width,height);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRegion"));

}

// wxRegion::wxRegion
void wxRegion_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *topLeft_t;
  int topLeft_sz;
  if(!enif_get_tuple(env, argv[0], &topLeft_sz, &topLeft_t)) Badarg("topLeft");
  int topLeftX;
  if(!enif_get_int(env, topLeft_t[0], &topLeftX)) Badarg("topLeft");
  int topLeftY;
  if(!enif_get_int(env, topLeft_t[1], &topLeftY)) Badarg("topLeft");
  wxPoint topLeft = wxPoint(topLeftX,topLeftY);
  const ERL_NIF_TERM *bottomRight_t;
  int bottomRight_sz;
  if(!enif_get_tuple(env, argv[1], &bottomRight_sz, &bottomRight_t)) Badarg("bottomRight");
  int bottomRightX;
  if(!enif_get_int(env, bottomRight_t[0], &bottomRightX)) Badarg("bottomRight");
  int bottomRightY;
  if(!enif_get_int(env, bottomRight_t[1], &bottomRightY)) Badarg("bottomRight");
  wxPoint bottomRight = wxPoint(bottomRightX,bottomRightY);
  wxRegion * Result = new EwxRegion(topLeft,bottomRight);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRegion"));

}

// wxRegion::wxRegion
void wxRegion_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *rect_t;
  int rect_sz;
  if(!enif_get_tuple(env, argv[0], &rect_sz, &rect_t)) Badarg("rect");
  int rectX;
  if(!enif_get_int(env, rect_t[0], &rectX)) Badarg("rect");
  int rectY;
  if(!enif_get_int(env, rect_t[1], &rectY)) Badarg("rect");
  int rectW;
  if(!enif_get_int(env, rect_t[2], &rectW)) Badarg("rect");
  int rectH;
  if(!enif_get_int(env, rect_t[3], &rectH)) Badarg("rect");
  wxRect rect = wxRect(rectX,rectY,rectW,rectH);
  wxRegion * Result = new EwxRegion(rect);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRegion"));

}

// wxRegion::wxRegion
void wxRegion_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[0], "bmp");
  wxRegion * Result = new EwxRegion(*bmp);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxRegion"));

}

// wxRegion::Clear
void wxRegion_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxRegion::Contains
void wxRegion_Contains_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  int Result = This->Contains(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxRegion::Contains
void wxRegion_Contains_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  int Result = This->Contains(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxRegion::Contains
void wxRegion_Contains_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // wxCoord
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // wxCoord
  if(!This) throw wxe_badarg("This");
  int Result = This->Contains(x,y,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxRegion::Contains
void wxRegion_Contains_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
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
  int Result = This->Contains(rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxRegion::ConvertToBitmap
void wxRegion_ConvertToBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->ConvertToBitmap()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxRegion::GetBox
void wxRegion_GetBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetBox();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxRegion::Intersect
void wxRegion_Intersect_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // wxCoord
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // wxCoord
  if(!This) throw wxe_badarg("This");
  bool Result = This->Intersect(x,y,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Intersect
void wxRegion_Intersect_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->Intersect(rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Intersect
void wxRegion_Intersect_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  wxRegion *region;
  region = (wxRegion *) memenv->getPtr(env, argv[1], "region");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Intersect(*region);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::IsEmpty
void wxRegion_IsEmpty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEmpty();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Subtract
void wxRegion_Subtract_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->Subtract(rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Subtract
void wxRegion_Subtract_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  wxRegion *region;
  region = (wxRegion *) memenv->getPtr(env, argv[1], "region");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Subtract(*region);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Offset
void wxRegion_Offset_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  bool Result = This->Offset(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Offset
void wxRegion_Offset_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Offset(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Union
void wxRegion_Union_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // wxCoord
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // wxCoord
  if(!This) throw wxe_badarg("This");
  bool Result = This->Union(x,y,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Union
void wxRegion_Union_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->Union(rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Union
void wxRegion_Union_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM region_type;
  void * region = memenv->getPtr(env, argv[1], "region", &region_type);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(region_type, WXE_ATOM_wxRegion))
   Result =  This->Union(* static_cast<wxRegion*> (region));
  else if(enif_is_identical(region_type, WXE_ATOM_wxBitmap))
   Result =  This->Union(* static_cast<wxBitmap*> (region));
  else throw wxe_badarg("region");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Union
void wxRegion_Union_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int tolerance=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  const ERL_NIF_TERM *transColour_t;
  int transColour_sz;
  if(!enif_get_tuple(env, argv[2], &transColour_sz, &transColour_t)) Badarg("transColour");
  int transColourR;
  if(!enif_get_int(env, transColour_t[0], &transColourR)) Badarg("transColour");
  int transColourG;
  if(!enif_get_int(env, transColour_t[1], &transColourG)) Badarg("transColour");
  int transColourB;
  if(!enif_get_int(env, transColour_t[2], &transColourB)) Badarg("transColour");
  int transColourA;
  if(!enif_get_int(env, transColour_t[3], &transColourA)) Badarg("transColour");
  wxColour transColour = wxColour(transColourR,transColourG,transColourB,transColourA);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "tolerance"))) {
  if(!enif_get_int(env, tpl[1], &tolerance)) Badarg("tolerance"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Union(*bmp,transColour,tolerance);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Xor
void wxRegion_Xor_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // wxCoord
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // wxCoord
  if(!This) throw wxe_badarg("This");
  bool Result = This->Xor(x,y,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Xor
void wxRegion_Xor_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->Xor(rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxRegion::Xor
void wxRegion_Xor_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxRegion *This;
  This = (wxRegion *) memenv->getPtr(env, argv[0], "This");
  wxRegion *region;
  region = (wxRegion *) memenv->getPtr(env, argv[1], "region");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Xor(*region);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

