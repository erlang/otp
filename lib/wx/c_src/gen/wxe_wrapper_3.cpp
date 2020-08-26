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

// wxAcceleratorTable::wxAcceleratorTable
void wxAcceleratorTable_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxAcceleratorTable * Result = new EwxAcceleratorTable();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAcceleratorTable"));

}

// wxAcceleratorTable::wxAcceleratorTable
void wxAcceleratorTable_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int n;
  if(!enif_get_int(env, argv[0], &n)) Badarg("n"); // int
  unsigned entriesLen;
  ERL_NIF_TERM entriesHead, entriesTail;
  if(!enif_get_list_length(env, argv[1], &entriesLen)) Badarg("entries");
  std::vector <wxAcceleratorEntry> entries;
  entriesTail = argv[1];
  while(!enif_is_empty_list(env, entriesTail)) {
    if(!enif_get_list_cell(env, entriesTail, &entriesHead, &entriesTail)) Badarg("entries");
    entries.push_back(* (wxAcceleratorEntry *) memenv->getPtr(env, entriesHead,"entries"));
  };
  wxAcceleratorTable * Result = new EwxAcceleratorTable(n,entries.data());
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAcceleratorTable"));

}

// wxAcceleratorTable::IsOk
void wxAcceleratorTable_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorTable *This;
  This = (wxAcceleratorTable *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAcceleratorEntry::wxAcceleratorEntry
void wxAcceleratorEntry_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=0;
  int keyCode=0;
  int cmd=0;
  wxMenuItem * item=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "keyCode"))) {
  if(!enif_get_int(env, tpl[1], &keyCode)) Badarg("keyCode"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "cmd"))) {
  if(!enif_get_int(env, tpl[1], &cmd)) Badarg("cmd"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "item"))) {
  item = (wxMenuItem *) memenv->getPtr(env, tpl[1], "item");
    } else        Badarg("Options");
  };
  wxAcceleratorEntry * Result = new wxAcceleratorEntry(flags,keyCode,cmd,item);
  app->newPtr((void *) Result, 70, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAcceleratorEntry"));

}

// wxAcceleratorEntry::wxAcceleratorEntry
void wxAcceleratorEntry_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorEntry *entry;
  entry = (wxAcceleratorEntry *) memenv->getPtr(env, argv[0], "entry");
  wxAcceleratorEntry * Result = new wxAcceleratorEntry(*entry);
  app->newPtr((void *) Result, 70, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAcceleratorEntry"));

}

// wxAcceleratorEntry::GetCommand
void wxAcceleratorEntry_GetCommand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorEntry *This;
  This = (wxAcceleratorEntry *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCommand();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAcceleratorEntry::GetFlags
void wxAcceleratorEntry_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorEntry *This;
  This = (wxAcceleratorEntry *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAcceleratorEntry::GetKeyCode
void wxAcceleratorEntry_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorEntry *This;
  This = (wxAcceleratorEntry *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAcceleratorEntry::Set
void wxAcceleratorEntry_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMenuItem * item=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorEntry *This;
  This = (wxAcceleratorEntry *) memenv->getPtr(env, argv[0], "This");
  int flags;
  if(!enif_get_int(env, argv[1], &flags)) Badarg("flags"); // int
  int keyCode;
  if(!enif_get_int(env, argv[2], &keyCode)) Badarg("keyCode"); // int
  int cmd;
  if(!enif_get_int(env, argv[3], &cmd)) Badarg("cmd"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "item"))) {
  item = (wxMenuItem *) memenv->getPtr(env, tpl[1], "item");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Set(flags,keyCode,cmd,item);

}

// wxAcceleratorEntry::destroy
void wxAcceleratorEntry_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxAcceleratorEntry *This;
  This = (wxAcceleratorEntry *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxCaret::wxCaret
void wxCaret_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  wxCaret * Result = new wxCaret(window,width,height);
  app->newPtr((void *) Result, 71, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCaret"));

}

// wxCaret::wxCaret
void wxCaret_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  wxCaret * Result = new wxCaret(window,size);
  app->newPtr((void *) Result, 71, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCaret"));

}

// wxCaret::Create
void wxCaret_Create_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(window,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCaret::Create
void wxCaret_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[2], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(window,size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCaret::GetBlinkTime
void wxCaret_GetBlinkTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxCaret::GetBlinkTime();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCaret::GetPosition
void wxCaret_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxCaret::GetSize
void wxCaret_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxCaret::GetWindow
void wxCaret_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxCaret::Hide
void wxCaret_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Hide();

}

// wxCaret::IsOk
void wxCaret_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCaret::IsVisible
void wxCaret_IsVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVisible();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCaret::Move
void wxCaret_Move_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->Move(x,y);

}

// wxCaret::Move
void wxCaret_Move_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  This->Move(pt);

}

// wxCaret::SetBlinkTime
void wxCaret_SetBlinkTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int milliseconds;
  if(!enif_get_int(env, argv[0], &milliseconds)) Badarg("milliseconds"); // int
  wxCaret::SetBlinkTime(milliseconds);

}

// wxCaret::SetSize
void wxCaret_SetSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSize(width,height);

}

// wxCaret::SetSize
void wxCaret_SetSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
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

// wxCaret::Show
void wxCaret_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
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
  This->Show(show);

}

// wxCaret::destroy
void wxCaret_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxCaret *This;
  This = (wxCaret *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxSizer::Add
void wxSizer_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[2], "flags");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[3], "flags");
  if(!This) throw wxe_badarg("This");
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
  int size;
  if(!enif_get_int(env, argv[1], &size)) Badarg("size"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
  This->Clear(delete_windows);

}

// wxSizer::Detach
void wxSizer_Detach_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
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
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
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
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
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
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[2], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[3], "flags");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[4], "flags");
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxSizerItem *item;
  item = (wxSizerItem *) memenv->getPtr(env, argv[2], "item");
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int size;
  if(!enif_get_int(env, argv[2], &size)) Badarg("size"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
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
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[2], "flags");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  wxSizerFlags *flags;
  flags = (wxSizerFlags *) memenv->getPtr(env, argv[3], "flags");
  if(!This) throw wxe_badarg("This");
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
  wxSizerItem *item;
  item = (wxSizerItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
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
  int size;
  if(!enif_get_int(env, argv[1], &size)) Badarg("size"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  if(!This) throw wxe_badarg("This");
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
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxSizerItem *newitem;
  newitem = (wxSizerItem *) memenv->getPtr(env, argv[2], "newitem");
  if(!This) throw wxe_badarg("This");
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
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  int width;
  if(!enif_get_int(env, argv[3], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[4], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetDimension(x,y,width,height);

}

// wxSizer::SetDimension
void wxSizer_SetDimension_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
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
  if(!This) throw wxe_badarg("This");
  This->SetDimension(pos,size);

}

// wxSizer::SetMinSize
void wxSizer_SetMinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
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

// wxSizer::SetMinSize
void wxSizer_SetMinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMinSize(width,height);

}

// wxSizer::SetItemMinSize
void wxSizer_SetItemMinSize_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[3], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Show(show);

}

// wxSizer::ShowItems
void wxSizer_ShowItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizer *This;
  This = (wxSizer *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
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
  int alignment;
  if(!enif_get_int(env, argv[1], &alignment)) Badarg("alignment"); // int
  if(!This) throw wxe_badarg("This");
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
  int direction;
  if(!enif_get_int(env, argv[1], &direction)) Badarg("direction"); // int
  int borderinpixels;
  if(!enif_get_int(env, argv[2], &borderinpixels)) Badarg("borderinpixels"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  int proportion;
  if(!enif_get_int(env, argv[1], &proportion)) Badarg("proportion"); // int
  if(!This) throw wxe_badarg("This");
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
  int border;
  if(!enif_get_int(env, argv[1], &border)) Badarg("border"); // int
  if(!This) throw wxe_badarg("This");
  This->SetBorder(border);

}

// wxSizerItem::SetDimension
void wxSizerItem_SetDimension(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
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
  if(!This) throw wxe_badarg("This");
  This->SetDimension(pos,size);

}

// wxSizerItem::SetFlag
void wxSizerItem_SetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  int flag;
  if(!enif_get_int(env, argv[1], &flag)) Badarg("flag"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFlag(flag);

}

// wxSizerItem::SetInitSize
void wxSizerItem_SetInitSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->SetInitSize(x,y);

}

// wxSizerItem::SetMinSize
void wxSizerItem_SetMinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
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

// wxSizerItem::SetMinSize
void wxSizerItem_SetMinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMinSize(x,y);

}

// wxSizerItem::SetProportion
void wxSizerItem_SetProportion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  int proportion;
  if(!enif_get_int(env, argv[1], &proportion)) Badarg("proportion"); // int
  if(!This) throw wxe_badarg("This");
  This->SetProportion(proportion);

}

// wxSizerItem::SetRatio
void wxSizerItem_SetRatio_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRatio(width,height);

}

// wxSizerItem::SetRatio
void wxSizerItem_SetRatio_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetRatio(size);

}

// wxSizerItem::SetRatio
void wxSizerItem_SetRatio_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  float ratio;
  if(!wxe_get_float(env, argv[1], &ratio)) Badarg("ratio");
  if(!This) throw wxe_badarg("This");
  This->SetRatio(ratio);

}

// wxSizerItem::AssignSizer
void wxSizerItem_AssignSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  wxSizer *sizer;
  sizer = (wxSizer *) memenv->getPtr(env, argv[1], "sizer");
  if(!This) throw wxe_badarg("This");
  This->AssignSizer(sizer);

}

// wxSizerItem::AssignSpacer
void wxSizerItem_AssignSpacer_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->AssignSpacer(size);

}

// wxSizerItem::AssignSpacer
void wxSizerItem_AssignSpacer_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  int w;
  if(!enif_get_int(env, argv[1], &w)) Badarg("w"); // int
  int h;
  if(!enif_get_int(env, argv[2], &h)) Badarg("h"); // int
  if(!This) throw wxe_badarg("This");
  This->AssignSpacer(w,h);

}

// wxSizerItem::AssignWindow
void wxSizerItem_AssignWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
  This->AssignWindow(window);

}

// wxSizerItem::Show
void wxSizerItem_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSizerItem *This;
  This = (wxSizerItem *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Show(show);

}

// wxBoxSizer::wxBoxSizer
void wxBoxSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int orient;
  if(!enif_get_int(env, argv[0], &orient)) Badarg("orient"); // int
  wxBoxSizer * Result = new EwxBoxSizer(orient);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBoxSizer"));

}

// wxBoxSizer::GetOrientation
void wxBoxSizer_GetOrientation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBoxSizer *This;
  This = (wxBoxSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOrientation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

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

// wxGridSizer::wxGridSizer
void wxGridSizer_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[1], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[2], &hgap)) Badarg("hgap"); // int
  wxGridSizer * Result = new EwxGridSizer(cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSize gap= wxSize(0, 0);
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "gap"))) {
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, tpl[1], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  gap = wxSize(gapW,gapH);
    } else        Badarg("Options");
  };
  wxGridSizer * Result = new EwxGridSizer(cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[2], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[3], &hgap)) Badarg("hgap"); // int
  wxGridSizer * Result = new EwxGridSizer(rows,cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::wxGridSizer
void wxGridSizer_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, argv[2], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  wxSize gap = wxSize(gapW,gapH);
  wxGridSizer * Result = new EwxGridSizer(rows,cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridSizer"));

}

// wxGridSizer::GetCols
void wxGridSizer_GetCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::GetHGap
void wxGridSizer_GetHGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetHGap();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::GetRows
void wxGridSizer_GetRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRows();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::GetVGap
void wxGridSizer_GetVGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetVGap();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGridSizer::SetCols
void wxGridSizer_SetCols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  if(!This) throw wxe_badarg("This");
  This->SetCols(cols);

}

// wxGridSizer::SetHGap
void wxGridSizer_SetHGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int gap;
  if(!enif_get_int(env, argv[1], &gap)) Badarg("gap"); // int
  if(!This) throw wxe_badarg("This");
  This->SetHGap(gap);

}

// wxGridSizer::SetRows
void wxGridSizer_SetRows(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int rows;
  if(!enif_get_int(env, argv[1], &rows)) Badarg("rows"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRows(rows);

}

// wxGridSizer::SetVGap
void wxGridSizer_SetVGap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridSizer *This;
  This = (wxGridSizer *) memenv->getPtr(env, argv[0], "This");
  int gap;
  if(!enif_get_int(env, argv[1], &gap)) Badarg("gap"); // int
  if(!This) throw wxe_badarg("This");
  This->SetVGap(gap);

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[1], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[2], &hgap)) Badarg("hgap"); // int
  wxFlexGridSizer * Result = new EwxFlexGridSizer(cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSize gap= wxSize(0, 0);
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int cols;
  if(!enif_get_int(env, argv[0], &cols)) Badarg("cols"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "gap"))) {
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, tpl[1], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  gap = wxSize(gapW,gapH);
    } else        Badarg("Options");
  };
  wxFlexGridSizer * Result = new EwxFlexGridSizer(cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  int vgap;
  if(!enif_get_int(env, argv[2], &vgap)) Badarg("vgap"); // int
  int hgap;
  if(!enif_get_int(env, argv[3], &hgap)) Badarg("hgap"); // int
  wxFlexGridSizer * Result = new EwxFlexGridSizer(rows,cols,vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::wxFlexGridSizer
void wxFlexGridSizer_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int rows;
  if(!enif_get_int(env, argv[0], &rows)) Badarg("rows"); // int
  int cols;
  if(!enif_get_int(env, argv[1], &cols)) Badarg("cols"); // int
  const ERL_NIF_TERM *gap_t;
  int gap_sz;
  if(!enif_get_tuple(env, argv[2], &gap_sz, &gap_t)) Badarg("gap");
  int gapW;
  if(!enif_get_int(env, gap_t[0], &gapW)) Badarg("gap");
  int gapH;
  if(!enif_get_int(env, gap_t[1], &gapH)) Badarg("gap");
  wxSize gap = wxSize(gapW,gapH);
  wxFlexGridSizer * Result = new EwxFlexGridSizer(rows,cols,gap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFlexGridSizer"));

}

// wxFlexGridSizer::AddGrowableCol
void wxFlexGridSizer_AddGrowableCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AddGrowableCol(idx,proportion);

}

// wxFlexGridSizer::AddGrowableRow
void wxFlexGridSizer_AddGrowableRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int proportion=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AddGrowableRow(idx,proportion);

}

// wxFlexGridSizer::GetFlexibleDirection
void wxFlexGridSizer_GetFlexibleDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlexibleDirection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFlexGridSizer::GetNonFlexibleGrowMode
void wxFlexGridSizer_GetNonFlexibleGrowMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNonFlexibleGrowMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFlexGridSizer::RemoveGrowableCol
void wxFlexGridSizer_RemoveGrowableCol(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
  if(!This) throw wxe_badarg("This");
  This->RemoveGrowableCol(idx);

}

// wxFlexGridSizer::RemoveGrowableRow
void wxFlexGridSizer_RemoveGrowableRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  size_t idx;
  if(!wxe_get_size_t(env, argv[1], &idx)) Badarg("idx");
  if(!This) throw wxe_badarg("This");
  This->RemoveGrowableRow(idx);

}

// wxFlexGridSizer::SetFlexibleDirection
void wxFlexGridSizer_SetFlexibleDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  int direction;
  if(!enif_get_int(env, argv[1], &direction)) Badarg("direction"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFlexibleDirection(direction);

}

// wxFlexGridSizer::SetNonFlexibleGrowMode
void wxFlexGridSizer_SetNonFlexibleGrowMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFlexGridSizer *This;
  This = (wxFlexGridSizer *) memenv->getPtr(env, argv[0], "This");
  wxFlexSizerGrowMode mode;
  if(!enif_get_int(env, argv[1], (int *) &mode)) Badarg("mode"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetNonFlexibleGrowMode(mode);

}

// wxGridBagSizer::wxGridBagSizer
void wxGridBagSizer_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int vgap=0;
  int hgap=0;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "vgap"))) {
  if(!enif_get_int(env, tpl[1], &vgap)) Badarg("vgap"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "hgap"))) {
  if(!enif_get_int(env, tpl[1], &hgap)) Badarg("hgap"); // int
    } else        Badarg("Options");
  };
  wxGridBagSizer * Result = new EwxGridBagSizer(vgap,hgap);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGridBagSizer"));

}

// wxGridBagSizer::Add
void wxGridBagSizer_Add_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSpan span= wxDefaultSpan;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "span"))) {
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, tpl[1], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  span = wxGBSpan(spanRS,spanCS);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxSizerItem*)This->Add(static_cast<wxWindow*> (window),pos,span,flag,border,userData);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxSizerItem*)This->Add(static_cast<wxSizer*> (window),pos,span,flag,border,userData);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxGridBagSizer::Add
void wxGridBagSizer_Add_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  wxGBSizerItem *item;
  item = (wxGBSizerItem *) memenv->getPtr(env, argv[1], "item");
  if(!This) throw wxe_badarg("This");
  wxSizerItem * Result = (wxSizerItem*)This->Add(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxGridBagSizer::Add
void wxGridBagSizer_Add_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSpan span= wxDefaultSpan;
  int flag=0;
  int border=0;
  wxObject * userData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[3], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "span"))) {
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, tpl[1], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  span = wxGBSpan(spanRS,spanCS);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], &border)) Badarg("border"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "userData"))) {
  userData = (wxObject *) memenv->getPtr(env, tpl[1], "userData");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxSizerItem * Result = (wxSizerItem*)This->Add(width,height,pos,span,flag,border,userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizerItem"));

}

// wxGridBagSizer::CalcMin
void wxGridBagSizer_CalcMin(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->CalcMin();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::CheckForIntersection
void wxGridBagSizer_CheckForIntersection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSizerItem * excludeItem=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  wxGBSizerItem *item;
  item = (wxGBSizerItem *) memenv->getPtr(env, argv[1], "item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "excludeItem"))) {
  excludeItem = (wxGBSizerItem *) memenv->getPtr(env, tpl[1], "excludeItem");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->CheckForIntersection(item,excludeItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::CheckForIntersection
void wxGridBagSizer_CheckForIntersection_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGBSizerItem * excludeItem=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, argv[2], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  wxGBSpan span = wxGBSpan(spanRS,spanCS);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "excludeItem"))) {
  excludeItem = (wxGBSizerItem *) memenv->getPtr(env, tpl[1], "excludeItem");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->CheckForIntersection(pos,span,excludeItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::FindItem
void wxGridBagSizer_FindItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  (wxGBSizerItem*)This->FindItem(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  (wxGBSizerItem*)This->FindItem(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::FindItemAtPoint
void wxGridBagSizer_FindItemAtPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result = (wxGBSizerItem*)This->FindItemAtPoint(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::FindItemAtPosition
void wxGridBagSizer_FindItemAtPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result = (wxGBSizerItem*)This->FindItemAtPosition(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::FindItemWithData
void wxGridBagSizer_FindItemWithData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  wxObject *userData;
  userData = (wxObject *) memenv->getPtr(env, argv[1], "userData");
  if(!This) throw wxe_badarg("This");
  wxGBSizerItem * Result = (wxGBSizerItem*)This->FindItemWithData(userData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGBSizerItem"));

}

// wxGridBagSizer::GetCellSize
void wxGridBagSizer_GetCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  int col;
  if(!enif_get_int(env, argv[2], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetCellSize(row,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetEmptyCellSize
void wxGridBagSizer_GetEmptyCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetEmptyCellSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemPosition
void wxGridBagSizer_GetItemPosition_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
  wxGBPosition Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->GetItemPosition(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->GetItemPosition(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemPosition
void wxGridBagSizer_GetItemPosition_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  wxGBPosition Result = This->GetItemPosition(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemSpan
void wxGridBagSizer_GetItemSpan_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  if(!This) throw wxe_badarg("This");
  wxGBSpan Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->GetItemSpan(static_cast<wxWindow*> (window));
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->GetItemSpan(static_cast<wxSizer*> (window));
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::GetItemSpan
void wxGridBagSizer_GetItemSpan_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  wxGBSpan Result = This->GetItemSpan(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGridBagSizer::SetEmptyCellSize
void wxGridBagSizer_SetEmptyCellSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  if(!This) throw wxe_badarg("This");
  This->SetEmptyCellSize(sz);

}

// wxGridBagSizer::SetItemPosition
void wxGridBagSizer_SetItemPosition_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->SetItemPosition(static_cast<wxWindow*> (window),pos);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->SetItemPosition(static_cast<wxSizer*> (window),pos);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::SetItemPosition
void wxGridBagSizer_SetItemPosition_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posR;
  if(!enif_get_int(env, pos_t[0], &posR)) Badarg("pos");
  int posC;
  if(!enif_get_int(env, pos_t[1], &posC)) Badarg("pos");
  wxGBPosition pos = wxGBPosition(posR,posC);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemPosition(index,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::SetItemSpan
void wxGridBagSizer_SetItemSpan_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM window_type;
  void * window = memenv->getPtr(env, argv[1], "window", &window_type);
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, argv[2], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  wxGBSpan span = wxGBSpan(spanRS,spanCS);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(window_type, WXE_ATOM_wxWindow))
   Result =  This->SetItemSpan(static_cast<wxWindow*> (window),span);
  else if(enif_is_identical(window_type, WXE_ATOM_wxSizer))
   Result =  This->SetItemSpan(static_cast<wxSizer*> (window),span);
  else throw wxe_badarg("window");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGridBagSizer::SetItemSpan
void wxGridBagSizer_SetItemSpan_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGridBagSizer *This;
  This = (wxGridBagSizer *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  const ERL_NIF_TERM *span_t;
  int span_sz;
  if(!enif_get_tuple(env, argv[2], &span_sz, &span_t)) Badarg("span");
  int spanRS;
  if(!enif_get_int(env, span_t[0], &spanRS)) Badarg("span");
  int spanCS;
  if(!enif_get_int(env, span_t[1], &spanCS)) Badarg("span");
  wxGBSpan span = wxGBSpan(spanRS,spanCS);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemSpan(index,span);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

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
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  if(!This) throw wxe_badarg("This");
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
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  if(!This) throw wxe_badarg("This");
  This->SetAffirmativeButton(button);

}

// wxStdDialogButtonSizer::SetCancelButton
void wxStdDialogButtonSizer_SetCancelButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  if(!This) throw wxe_badarg("This");
  This->SetCancelButton(button);

}

// wxStdDialogButtonSizer::SetNegativeButton
void wxStdDialogButtonSizer_SetNegativeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStdDialogButtonSizer *This;
  This = (wxStdDialogButtonSizer *) memenv->getPtr(env, argv[0], "This");
  wxButton *button;
  button = (wxButton *) memenv->getPtr(env, argv[1], "button");
  if(!This) throw wxe_badarg("This");
  This->SetNegativeButton(button);

}

// wxFont::wxFont
void wxFont_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFont * Result = new EwxFont();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[0], "font");
  wxFont * Result = new EwxFont(*font);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_5_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool underlined=false;
  wxString face= wxEmptyString;
 wxFontEncoding encoding=wxFONTENCODING_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int pointSize;
  if(!enif_get_int(env, argv[0], &pointSize)) Badarg("pointSize"); // int
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  wxFontStyle style;
  if(!enif_get_int(env, argv[2], (int *) &style)) Badarg("style"); // enum
  wxFontWeight weight;
  if(!enif_get_int(env, argv[3], (int *) &weight)) Badarg("weight"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "underlined"))) {
  underlined = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "face"))) {
  ErlNifBinary face_bin;
  if(!enif_inspect_binary(env, tpl[1], &face_bin)) Badarg("face");
  face = wxString(face_bin.data, wxConvUTF8, face_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "encoding"))) {
  if(!enif_get_int(env, tpl[1], (int *) &encoding)) Badarg("encoding"); // enum
    } else        Badarg("Options");
  };
  wxFont * Result = new EwxFont(pointSize,family,style,weight,underlined,face,encoding);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_5_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool underline=false;
  wxString faceName= wxEmptyString;
 wxFontEncoding encoding=wxFONTENCODING_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *pixelSize_t;
  int pixelSize_sz;
  if(!enif_get_tuple(env, argv[0], &pixelSize_sz, &pixelSize_t)) Badarg("pixelSize");
  int pixelSizeW;
  if(!enif_get_int(env, pixelSize_t[0], &pixelSizeW)) Badarg("pixelSize");
  int pixelSizeH;
  if(!enif_get_int(env, pixelSize_t[1], &pixelSizeH)) Badarg("pixelSize");
  wxSize pixelSize = wxSize(pixelSizeW,pixelSizeH);
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  wxFontStyle style;
  if(!enif_get_int(env, argv[2], (int *) &style)) Badarg("style"); // enum
  wxFontWeight weight;
  if(!enif_get_int(env, argv[3], (int *) &weight)) Badarg("weight"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "underline"))) {
  underline = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "faceName"))) {
  ErlNifBinary faceName_bin;
  if(!enif_inspect_binary(env, tpl[1], &faceName_bin)) Badarg("faceName");
  faceName = wxString(faceName_bin.data, wxConvUTF8, faceName_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "encoding"))) {
  if(!enif_get_int(env, tpl[1], (int *) &encoding)) Badarg("encoding"); // enum
    } else        Badarg("Options");
  };
  wxFont * Result = new EwxFont(pixelSize,family,style,weight,underline,faceName,encoding);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::wxFont
void wxFont_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary nativeInfoString_bin;
  wxString nativeInfoString;
  if(!enif_inspect_binary(env, argv[0], &nativeInfoString_bin)) Badarg("nativeInfoString");
  nativeInfoString = wxString(nativeInfoString_bin.data, wxConvUTF8, nativeInfoString_bin.size);
  wxFont * Result = new EwxFont(nativeInfoString);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFont::IsFixedWidth
void wxFont_IsFixedWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFixedWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::GetDefaultEncoding
void wxFont_GetDefaultEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxFont::GetDefaultEncoding();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetFaceName
void wxFont_GetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFaceName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFont::GetFamily
void wxFont_GetFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFamily();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetNativeFontInfoDesc
void wxFont_GetNativeFontInfoDesc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetNativeFontInfoDesc();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFont::GetNativeFontInfoUserDesc
void wxFont_GetNativeFontInfoUserDesc(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetNativeFontInfoUserDesc();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFont::GetPointSize
void wxFont_GetPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPointSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetStyle
void wxFont_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::GetUnderlined
void wxFont_GetUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetUnderlined();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::GetWeight
void wxFont_GetWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFont::IsOk
void wxFont_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::SetDefaultEncoding
void wxFont_SetDefaultEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontEncoding encoding;
  if(!enif_get_int(env, argv[0], (int *) &encoding)) Badarg("encoding"); // enum
  wxFont::SetDefaultEncoding(encoding);

}

// wxFont::SetFaceName
void wxFont_SetFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary faceName_bin;
  wxString faceName;
  if(!enif_inspect_binary(env, argv[1], &faceName_bin)) Badarg("faceName");
  faceName = wxString(faceName_bin.data, wxConvUTF8, faceName_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetFaceName(faceName);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFont::SetFamily
void wxFont_SetFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetFamily(family);

}

// wxFont::SetPointSize
void wxFont_SetPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  int pointSize;
  if(!enif_get_int(env, argv[1], &pointSize)) Badarg("pointSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPointSize(pointSize);

}

// wxFont::SetStyle
void wxFont_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  wxFontStyle style;
  if(!enif_get_int(env, argv[1], (int *) &style)) Badarg("style"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetStyle(style);

}

// wxFont::SetUnderlined
void wxFont_SetUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  bool underlined;
  underlined = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetUnderlined(underlined);

}

// wxFont::SetWeight
void wxFont_SetWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFont *This;
  This = (wxFont *) memenv->getPtr(env, argv[0], "This");
  wxFontWeight weight;
  if(!enif_get_int(env, argv[1], (int *) &weight)) Badarg("weight"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetWeight(weight);

}

// wxToolTip::Enable
void wxToolTip_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ERL_NIF_TERM * argv = Ecmd.args;
  bool flag;
  flag = enif_is_identical(argv[0], WXE_ATOM_true);
  wxToolTip::Enable(flag);

}

// wxToolTip::SetDelay
void wxToolTip_SetDelay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  long msecs;
  if(!enif_get_long(env, argv[0], &msecs)) Badarg("msecs");
  wxToolTip::SetDelay(msecs);

}

// wxToolTip::wxToolTip
void wxToolTip_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary tip_bin;
  wxString tip;
  if(!enif_inspect_binary(env, argv[0], &tip_bin)) Badarg("tip");
  tip = wxString(tip_bin.data, wxConvUTF8, tip_bin.size);
  wxToolTip * Result = new EwxToolTip(tip);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolTip"));

}

// wxToolTip::SetTip
void wxToolTip_SetTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolTip *This;
  This = (wxToolTip *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary tip_bin;
  wxString tip;
  if(!enif_inspect_binary(env, argv[1], &tip_bin)) Badarg("tip");
  tip = wxString(tip_bin.data, wxConvUTF8, tip_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetTip(tip);

}

// wxToolTip::GetTip
void wxToolTip_GetTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolTip *This;
  This = (wxToolTip *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetTip();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolTip::GetWindow
void wxToolTip_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolTip *This;
  This = (wxToolTip *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxButton::wxButton
void wxButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxButton * Result = new EwxButton();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxButton"));

}

// wxButton::wxButton
void wxButton_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString label= wxEmptyString;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxButton * Result = new EwxButton(parent,id,label,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxButton"));

}

// wxButton::Create
void wxButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString label= wxEmptyString;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "label"))) {
  ErlNifBinary label_bin;
  if(!enif_inspect_binary(env, tpl[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,label,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxButton::GetDefaultSize
void wxButton_GetDefaultSize_STAT_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxSize Result = wxButton::GetDefaultSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#if wxCHECK_VERSION(3,1,3)
// wxButton::GetDefaultSize
void wxButton_GetDefaultSize_STAT_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *win;
  win = (wxWindow *) memenv->getPtr(env, argv[0], "win");
  wxSize Result = wxButton::GetDefaultSize(win);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
// wxButton::SetDefault
void wxButton_SetDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->SetDefault();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxButton::SetLabel
void wxButton_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetLabel(label);

}

// wxButton::GetBitmapDisabled
void wxButton_GetBitmapDisabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmapDisabled()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxButton::GetBitmapFocus
void wxButton_GetBitmapFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmapFocus()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxButton::GetBitmapLabel
void wxButton_GetBitmapLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmapLabel()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxButton::SetBitmapDisabled
void wxButton_SetBitmapDisabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SetBitmapDisabled(*bitmap);

}

// wxButton::SetBitmapFocus
void wxButton_SetBitmapFocus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SetBitmapFocus(*bitmap);

}

// wxButton::SetBitmapLabel
void wxButton_SetBitmapLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxButton *This;
  This = (wxButton *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SetBitmapLabel(*bitmap);

}

// wxBitmapButton::wxBitmapButton
void wxBitmapButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxBitmapButton * Result = new EwxBitmapButton();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmapButton"));

}

// wxBitmapButton::wxBitmapButton
void wxBitmapButton_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[2], "bitmap");
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
  wxBitmapButton * Result = new EwxBitmapButton(parent,id,*bitmap,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmapButton"));

}

// wxBitmapButton::Create
void wxBitmapButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmapButton *This;
  This = (wxBitmapButton *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[3], "bitmap");
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
  bool Result = This->Create(parent,id,*bitmap,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmapButton::NewCloseButton
void wxBitmapButton_NewCloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int winid;
  if(!enif_get_int(env, argv[1], &winid)) Badarg("winid"); // wxWindowID
  wxBitmapButton * Result = (wxBitmapButton*)wxBitmapButton::NewCloseButton(parent,winid);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmapButton"));

}

// wxToggleButton::wxToggleButton
void wxToggleButton_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxToggleButton * Result = new EwxToggleButton();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToggleButton"));

}

// wxToggleButton::wxToggleButton
void wxToggleButton_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxToggleButton * Result = new EwxToggleButton(parent,id,label,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToggleButton"));

}

// wxToggleButton::Create
void wxToggleButton_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToggleButton *This;
  This = (wxToggleButton *) memenv->getPtr(env, argv[0], "This");
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

// wxToggleButton::GetValue
void wxToggleButton_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToggleButton *This;
  This = (wxToggleButton *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToggleButton::SetValue
void wxToggleButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToggleButton *This;
  This = (wxToggleButton *) memenv->getPtr(env, argv[0], "This");
  bool state;
  state = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetValue(state);

}

// wxCalendarCtrl::wxCalendarCtrl
void wxCalendarCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCalendarCtrl * Result = new EwxCalendarCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCalendarCtrl"));

}

// wxCalendarCtrl::wxCalendarCtrl
void wxCalendarCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDateTime date= wxDefaultDateTime;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCAL_SHOW_HOLIDAYS;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "date"))) {
  const ERL_NIF_TERM *date_t;
  int date_sz;
  if(!enif_get_tuple(env, tpl[1], &date_sz, &date_t)) Badarg("date");
  int dateD;
  if(!enif_get_int(env, date_t[0], &dateD)) Badarg("date");
  int dateMo;
  if(!enif_get_int(env, date_t[1], &dateMo)) Badarg("date");
  int dateY;
  if(!enif_get_int(env, date_t[2], &dateY)) Badarg("date");
  int dateH;
  if(!enif_get_int(env, date_t[3], &dateH)) Badarg("date");
  int dateMi;
  if(!enif_get_int(env, date_t[4], &dateMi)) Badarg("date");
  int dateS;
  if(!enif_get_int(env, date_t[5], &dateS)) Badarg("date");
 date = wxDateTime((wxDateTime::wxDateTime_t) dateD,(wxDateTime::Month) (dateMo-1),dateY,(wxDateTime::wxDateTime_t) dateH,(wxDateTime::wxDateTime_t) dateMi,(wxDateTime::wxDateTime_t) dateS);
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
  wxCalendarCtrl * Result = new EwxCalendarCtrl(parent,id,date,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCalendarCtrl"));

}

// wxCalendarCtrl::Create
void wxCalendarCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDateTime date= wxDefaultDateTime;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCAL_SHOW_HOLIDAYS;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "date"))) {
  const ERL_NIF_TERM *date_t;
  int date_sz;
  if(!enif_get_tuple(env, tpl[1], &date_sz, &date_t)) Badarg("date");
  int dateD;
  if(!enif_get_int(env, date_t[0], &dateD)) Badarg("date");
  int dateMo;
  if(!enif_get_int(env, date_t[1], &dateMo)) Badarg("date");
  int dateY;
  if(!enif_get_int(env, date_t[2], &dateY)) Badarg("date");
  int dateH;
  if(!enif_get_int(env, date_t[3], &dateH)) Badarg("date");
  int dateMi;
  if(!enif_get_int(env, date_t[4], &dateMi)) Badarg("date");
  int dateS;
  if(!enif_get_int(env, date_t[5], &dateS)) Badarg("date");
 date = wxDateTime((wxDateTime::wxDateTime_t) dateD,(wxDateTime::Month) (dateMo-1),dateY,(wxDateTime::wxDateTime_t) dateH,(wxDateTime::wxDateTime_t) dateMi,(wxDateTime::wxDateTime_t) dateS);
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
  bool Result = This->Create(parent,id,date,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarCtrl::SetDate
void wxCalendarCtrl_SetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *date_t;
  int date_sz;
  if(!enif_get_tuple(env, argv[1], &date_sz, &date_t)) Badarg("date");
  int dateD;
  if(!enif_get_int(env, date_t[0], &dateD)) Badarg("date");
  int dateMo;
  if(!enif_get_int(env, date_t[1], &dateMo)) Badarg("date");
  int dateY;
  if(!enif_get_int(env, date_t[2], &dateY)) Badarg("date");
  int dateH;
  if(!enif_get_int(env, date_t[3], &dateH)) Badarg("date");
  int dateMi;
  if(!enif_get_int(env, date_t[4], &dateMi)) Badarg("date");
  int dateS;
  if(!enif_get_int(env, date_t[5], &dateS)) Badarg("date");
 wxDateTime date = wxDateTime((wxDateTime::wxDateTime_t) dateD,(wxDateTime::Month) (dateMo-1),dateY,(wxDateTime::wxDateTime_t) dateH,(wxDateTime::wxDateTime_t) dateMi,(wxDateTime::wxDateTime_t) dateS);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetDate(date);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarCtrl::GetDate
void wxCalendarCtrl_GetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDateTime Result = This->GetDate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#if !wxCHECK_VERSION(2,9,0)
// wxCalendarCtrl::EnableYearChange
void wxCalendarCtrl_EnableYearChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
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
  This->EnableYearChange(enable);

}

#endif
// wxCalendarCtrl::EnableMonthChange
void wxCalendarCtrl_EnableMonthChange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool enable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->EnableMonthChange(enable);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarCtrl::EnableHolidayDisplay
void wxCalendarCtrl_EnableHolidayDisplay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool display=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "display"))) {
  display = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->EnableHolidayDisplay(display);

}

// wxCalendarCtrl::SetHeaderColours
void wxCalendarCtrl_SetHeaderColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colFg_t;
  int colFg_sz;
  if(!enif_get_tuple(env, argv[1], &colFg_sz, &colFg_t)) Badarg("colFg");
  int colFgR;
  if(!enif_get_int(env, colFg_t[0], &colFgR)) Badarg("colFg");
  int colFgG;
  if(!enif_get_int(env, colFg_t[1], &colFgG)) Badarg("colFg");
  int colFgB;
  if(!enif_get_int(env, colFg_t[2], &colFgB)) Badarg("colFg");
  int colFgA;
  if(!enif_get_int(env, colFg_t[3], &colFgA)) Badarg("colFg");
  wxColour colFg = wxColour(colFgR,colFgG,colFgB,colFgA);
  const ERL_NIF_TERM *colBg_t;
  int colBg_sz;
  if(!enif_get_tuple(env, argv[2], &colBg_sz, &colBg_t)) Badarg("colBg");
  int colBgR;
  if(!enif_get_int(env, colBg_t[0], &colBgR)) Badarg("colBg");
  int colBgG;
  if(!enif_get_int(env, colBg_t[1], &colBgG)) Badarg("colBg");
  int colBgB;
  if(!enif_get_int(env, colBg_t[2], &colBgB)) Badarg("colBg");
  int colBgA;
  if(!enif_get_int(env, colBg_t[3], &colBgA)) Badarg("colBg");
  wxColour colBg = wxColour(colBgR,colBgG,colBgB,colBgA);
  if(!This) throw wxe_badarg("This");
  This->SetHeaderColours(colFg,colBg);

}

// wxCalendarCtrl::GetHeaderColourFg
void wxCalendarCtrl_GetHeaderColourFg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetHeaderColourFg();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarCtrl::GetHeaderColourBg
void wxCalendarCtrl_GetHeaderColourBg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetHeaderColourBg();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarCtrl::SetHighlightColours
void wxCalendarCtrl_SetHighlightColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colFg_t;
  int colFg_sz;
  if(!enif_get_tuple(env, argv[1], &colFg_sz, &colFg_t)) Badarg("colFg");
  int colFgR;
  if(!enif_get_int(env, colFg_t[0], &colFgR)) Badarg("colFg");
  int colFgG;
  if(!enif_get_int(env, colFg_t[1], &colFgG)) Badarg("colFg");
  int colFgB;
  if(!enif_get_int(env, colFg_t[2], &colFgB)) Badarg("colFg");
  int colFgA;
  if(!enif_get_int(env, colFg_t[3], &colFgA)) Badarg("colFg");
  wxColour colFg = wxColour(colFgR,colFgG,colFgB,colFgA);
  const ERL_NIF_TERM *colBg_t;
  int colBg_sz;
  if(!enif_get_tuple(env, argv[2], &colBg_sz, &colBg_t)) Badarg("colBg");
  int colBgR;
  if(!enif_get_int(env, colBg_t[0], &colBgR)) Badarg("colBg");
  int colBgG;
  if(!enif_get_int(env, colBg_t[1], &colBgG)) Badarg("colBg");
  int colBgB;
  if(!enif_get_int(env, colBg_t[2], &colBgB)) Badarg("colBg");
  int colBgA;
  if(!enif_get_int(env, colBg_t[3], &colBgA)) Badarg("colBg");
  wxColour colBg = wxColour(colBgR,colBgG,colBgB,colBgA);
  if(!This) throw wxe_badarg("This");
  This->SetHighlightColours(colFg,colBg);

}

// wxCalendarCtrl::GetHighlightColourFg
void wxCalendarCtrl_GetHighlightColourFg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetHighlightColourFg();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarCtrl::GetHighlightColourBg
void wxCalendarCtrl_GetHighlightColourBg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetHighlightColourBg();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarCtrl::SetHolidayColours
void wxCalendarCtrl_SetHolidayColours(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *colFg_t;
  int colFg_sz;
  if(!enif_get_tuple(env, argv[1], &colFg_sz, &colFg_t)) Badarg("colFg");
  int colFgR;
  if(!enif_get_int(env, colFg_t[0], &colFgR)) Badarg("colFg");
  int colFgG;
  if(!enif_get_int(env, colFg_t[1], &colFgG)) Badarg("colFg");
  int colFgB;
  if(!enif_get_int(env, colFg_t[2], &colFgB)) Badarg("colFg");
  int colFgA;
  if(!enif_get_int(env, colFg_t[3], &colFgA)) Badarg("colFg");
  wxColour colFg = wxColour(colFgR,colFgG,colFgB,colFgA);
  const ERL_NIF_TERM *colBg_t;
  int colBg_sz;
  if(!enif_get_tuple(env, argv[2], &colBg_sz, &colBg_t)) Badarg("colBg");
  int colBgR;
  if(!enif_get_int(env, colBg_t[0], &colBgR)) Badarg("colBg");
  int colBgG;
  if(!enif_get_int(env, colBg_t[1], &colBgG)) Badarg("colBg");
  int colBgB;
  if(!enif_get_int(env, colBg_t[2], &colBgB)) Badarg("colBg");
  int colBgA;
  if(!enif_get_int(env, colBg_t[3], &colBgA)) Badarg("colBg");
  wxColour colBg = wxColour(colBgR,colBgG,colBgB,colBgA);
  if(!This) throw wxe_badarg("This");
  This->SetHolidayColours(colFg,colBg);

}

// wxCalendarCtrl::GetHolidayColourFg
void wxCalendarCtrl_GetHolidayColourFg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetHolidayColourFg();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarCtrl::GetHolidayColourBg
void wxCalendarCtrl_GetHolidayColourBg(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetHolidayColourBg();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarCtrl::GetAttr
void wxCalendarCtrl_GetAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  size_t day;
  if(!wxe_get_size_t(env, argv[1], &day)) Badarg("day");
  if(!This) throw wxe_badarg("This");
  wxCalendarDateAttr * Result = (wxCalendarDateAttr*)This->GetAttr(day);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCalendarDateAttr"));

}

// wxCalendarCtrl::SetAttr
void wxCalendarCtrl_SetAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  size_t day;
  if(!wxe_get_size_t(env, argv[1], &day)) Badarg("day");
  wxCalendarDateAttr *attr;
  attr = (wxCalendarDateAttr *) memenv->getPtr(env, argv[2], "attr");
  if(!This) throw wxe_badarg("This");
  This->SetAttr(day,attr);

}

// wxCalendarCtrl::SetHoliday
void wxCalendarCtrl_SetHoliday(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  size_t day;
  if(!wxe_get_size_t(env, argv[1], &day)) Badarg("day");
  if(!This) throw wxe_badarg("This");
  This->SetHoliday(day);

}

// wxCalendarCtrl::ResetAttr
void wxCalendarCtrl_ResetAttr(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  size_t day;
  if(!wxe_get_size_t(env, argv[1], &day)) Badarg("day");
  if(!This) throw wxe_badarg("This");
  This->ResetAttr(day);

}

// wxCalendarCtrl::HitTest
void wxCalendarCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDateTime date;
  wxDateTime::WeekDay wd;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarCtrl *This;
  This = (wxCalendarCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(pos,&date,&wd);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_int(Result),
    rt.make(date),
  rt.make_int(wd));
  rt.send(msg);

}

// wxCalendarDateAttr::wxCalendarDateAttr
void wxCalendarDateAttr_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour colText= wxNullColour;
  wxColour colBack= wxNullColour;
  wxColour colBorder= wxNullColour;
  const wxFont * font= &wxNullFont;
 wxCalendarDateBorder border=wxCAL_BORDER_NONE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "colText"))) {
  const ERL_NIF_TERM *colText_t;
  int colText_sz;
  if(!enif_get_tuple(env, tpl[1], &colText_sz, &colText_t)) Badarg("colText");
  int colTextR;
  if(!enif_get_int(env, colText_t[0], &colTextR)) Badarg("colText");
  int colTextG;
  if(!enif_get_int(env, colText_t[1], &colTextG)) Badarg("colText");
  int colTextB;
  if(!enif_get_int(env, colText_t[2], &colTextB)) Badarg("colText");
  int colTextA;
  if(!enif_get_int(env, colText_t[3], &colTextA)) Badarg("colText");
  colText = wxColour(colTextR,colTextG,colTextB,colTextA);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "colBack"))) {
  const ERL_NIF_TERM *colBack_t;
  int colBack_sz;
  if(!enif_get_tuple(env, tpl[1], &colBack_sz, &colBack_t)) Badarg("colBack");
  int colBackR;
  if(!enif_get_int(env, colBack_t[0], &colBackR)) Badarg("colBack");
  int colBackG;
  if(!enif_get_int(env, colBack_t[1], &colBackG)) Badarg("colBack");
  int colBackB;
  if(!enif_get_int(env, colBack_t[2], &colBackB)) Badarg("colBack");
  int colBackA;
  if(!enif_get_int(env, colBack_t[3], &colBackA)) Badarg("colBack");
  colBack = wxColour(colBackR,colBackG,colBackB,colBackA);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "colBorder"))) {
  const ERL_NIF_TERM *colBorder_t;
  int colBorder_sz;
  if(!enif_get_tuple(env, tpl[1], &colBorder_sz, &colBorder_t)) Badarg("colBorder");
  int colBorderR;
  if(!enif_get_int(env, colBorder_t[0], &colBorderR)) Badarg("colBorder");
  int colBorderG;
  if(!enif_get_int(env, colBorder_t[1], &colBorderG)) Badarg("colBorder");
  int colBorderB;
  if(!enif_get_int(env, colBorder_t[2], &colBorderB)) Badarg("colBorder");
  int colBorderA;
  if(!enif_get_int(env, colBorder_t[3], &colBorderA)) Badarg("colBorder");
  colBorder = wxColour(colBorderR,colBorderG,colBorderB,colBorderA);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "font"))) {
  font = (wxFont *) memenv->getPtr(env, tpl[1], "font");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "border"))) {
  if(!enif_get_int(env, tpl[1], (int *) &border)) Badarg("border"); // enum
    } else        Badarg("Options");
  };
  wxCalendarDateAttr * Result = new wxCalendarDateAttr(colText,colBack,colBorder,*font,border);
  app->newPtr((void *) Result, 89, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCalendarDateAttr"));

}

// wxCalendarDateAttr::wxCalendarDateAttr
void wxCalendarDateAttr_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour colBorder= wxNullColour;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateBorder border;
  if(!enif_get_int(env, argv[0], (int *) &border)) Badarg("border"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "colBorder"))) {
  const ERL_NIF_TERM *colBorder_t;
  int colBorder_sz;
  if(!enif_get_tuple(env, tpl[1], &colBorder_sz, &colBorder_t)) Badarg("colBorder");
  int colBorderR;
  if(!enif_get_int(env, colBorder_t[0], &colBorderR)) Badarg("colBorder");
  int colBorderG;
  if(!enif_get_int(env, colBorder_t[1], &colBorderG)) Badarg("colBorder");
  int colBorderB;
  if(!enif_get_int(env, colBorder_t[2], &colBorderB)) Badarg("colBorder");
  int colBorderA;
  if(!enif_get_int(env, colBorder_t[3], &colBorderA)) Badarg("colBorder");
  colBorder = wxColour(colBorderR,colBorderG,colBorderB,colBorderA);
    } else        Badarg("Options");
  };
  wxCalendarDateAttr * Result = new wxCalendarDateAttr(border,colBorder);
  app->newPtr((void *) Result, 89, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCalendarDateAttr"));

}

// wxCalendarDateAttr::SetTextColour
void wxCalendarDateAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxCalendarDateAttr::SetBackgroundColour
void wxCalendarDateAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxCalendarDateAttr::SetBorderColour
void wxCalendarDateAttr_SetBorderColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, argv[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  wxColour col = wxColour(colR,colG,colB,colA);
  if(!This) throw wxe_badarg("This");
  This->SetBorderColour(col);

}

// wxCalendarDateAttr::SetFont
void wxCalendarDateAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxCalendarDateAttr::SetBorder
void wxCalendarDateAttr_SetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  wxCalendarDateBorder border;
  if(!enif_get_int(env, argv[1], (int *) &border)) Badarg("border"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetBorder(border);

}

// wxCalendarDateAttr::SetHoliday
void wxCalendarDateAttr_SetHoliday(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  bool holiday;
  holiday = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetHoliday(holiday);

}

// wxCalendarDateAttr::HasTextColour
void wxCalendarDateAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarDateAttr::HasBackgroundColour
void wxCalendarDateAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarDateAttr::HasBorderColour
void wxCalendarDateAttr_HasBorderColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBorderColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarDateAttr::HasFont
void wxCalendarDateAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarDateAttr::HasBorder
void wxCalendarDateAttr_HasBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBorder();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarDateAttr::IsHoliday
void wxCalendarDateAttr_IsHoliday(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsHoliday();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCalendarDateAttr::GetTextColour
void wxCalendarDateAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarDateAttr::GetBackgroundColour
void wxCalendarDateAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarDateAttr::GetBorderColour
void wxCalendarDateAttr_GetBorderColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBorderColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxCalendarDateAttr::GetFont
void wxCalendarDateAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFont * Result = &This->GetFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxCalendarDateAttr::GetBorder
void wxCalendarDateAttr_GetBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetBorder();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCalendarDateAttr::destroy
void wxCalendarDateAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarDateAttr *This;
  This = (wxCalendarDateAttr *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxCheckBox::wxCheckBox
void wxCheckBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCheckBox * Result = new EwxCheckBox();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCheckBox"));

}

// wxCheckBox::wxCheckBox
void wxCheckBox_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxCheckBox * Result = new EwxCheckBox(parent,id,label,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCheckBox"));

}

// wxCheckBox::Create
void wxCheckBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
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

// wxCheckBox::GetValue
void wxCheckBox_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCheckBox::Get3StateValue
void wxCheckBox_Get3StateValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->Get3StateValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCheckBox::Is3rdStateAllowedForUser
void wxCheckBox_Is3rdStateAllowedForUser(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Is3rdStateAllowedForUser();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCheckBox::Is3State
void wxCheckBox_Is3State(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Is3State();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCheckBox::IsChecked
void wxCheckBox_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsChecked();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCheckBox::SetValue
void wxCheckBox_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  bool state;
  state = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetValue(state);

}

// wxCheckBox::Set3StateValue
void wxCheckBox_Set3StateValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckBox *This;
  This = (wxCheckBox *) memenv->getPtr(env, argv[0], "This");
  wxCheckBoxState state;
  if(!enif_get_int(env, argv[1], (int *) &state)) Badarg("state"); // enum
  if(!This) throw wxe_badarg("This");
  This->Set3StateValue(state);

}

// wxCheckListBox::wxCheckListBox
void wxCheckListBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCheckListBox * Result = new EwxCheckListBox();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCheckListBox"));

}

// wxCheckListBox::wxCheckListBox
void wxCheckListBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  wxArrayString choices;
  long style=0;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "choices"))) {
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  choicesTail = tpl[1];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxCheckListBox * Result = new EwxCheckListBox(parent,id,pos,size,choices,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCheckListBox"));

}

// wxCheckListBox::Check
void wxCheckListBox_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool check=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckListBox *This;
  This = (wxCheckListBox *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "check"))) {
  check = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Check(item,check);

}

// wxCheckListBox::IsChecked
void wxCheckListBox_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCheckListBox *This;
  This = (wxCheckListBox *) memenv->getPtr(env, argv[0], "This");
  unsigned int item;
  if(!enif_get_uint(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsChecked(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxChoice::wxChoice
void wxChoice_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxChoice * Result = new EwxChoice();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxChoice"));

}

// wxChoice::wxChoice
void wxChoice_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  wxArrayString choices;
  long style=0;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "choices"))) {
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  choicesTail = tpl[1];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxChoice * Result = new EwxChoice(parent,id,pos,size,choices,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxChoice"));

}

// wxChoice::Create
void wxChoice_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoice *This;
  This = (wxChoice *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,pos,size,choices,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxChoice::Delete
void wxChoice_Delete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoice *This;
  This = (wxChoice *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  This->Delete(n);

}

// wxChoice::GetColumns
void wxChoice_GetColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoice *This;
  This = (wxChoice *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumns();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxChoice::SetColumns
void wxChoice_SetColumns(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int n=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoice *This;
  This = (wxChoice *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "n"))) {
  if(!enif_get_int(env, tpl[1], &n)) Badarg("n"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetColumns(n);

}

// wxComboBox::wxComboBox
void wxComboBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxComboBox * Result = new EwxComboBox();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxComboBox"));

}

// wxComboBox::wxComboBox
void wxComboBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString value= wxEmptyString;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  wxArrayString choices;
  long style=0;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "value"))) {
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "choices"))) {
  ERL_NIF_TERM choicesHead, choicesTail;
  ErlNifBinary choices_bin;
  choicesTail = tpl[1];
  while(!enif_is_empty_list(env, choicesTail)) {
    if(!enif_get_list_cell(env, choicesTail, &choicesHead, &choicesTail)) Badarg("choices");
    if(!enif_inspect_binary(env, choicesHead, &choices_bin)) Badarg("choices");
    choices.Add(wxString(choices_bin.data, wxConvUTF8, choices_bin.size));
  };
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxComboBox * Result = new EwxComboBox(parent,id,value,pos,size,choices,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxComboBox"));

}

// wxComboBox::Create
void wxComboBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[3], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,value,pos,size,choices,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxComboBox::CanCopy
void wxComboBox_CanCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanCopy();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxComboBox::CanCut
void wxComboBox_CanCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanCut();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxComboBox::CanPaste
void wxComboBox_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanPaste();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxComboBox::CanRedo
void wxComboBox_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanRedo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxComboBox::CanUndo
void wxComboBox_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanUndo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxComboBox::Copy
void wxComboBox_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Copy();

}

// wxComboBox::Cut
void wxComboBox_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Cut();

}

// wxComboBox::GetInsertionPoint
void wxComboBox_GetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetInsertionPoint();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxComboBox::GetLastPosition
void wxComboBox_GetLastPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTextPos Result = This->GetLastPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxComboBox::GetValue
void wxComboBox_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxComboBox::Paste
void wxComboBox_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Paste();

}

// wxComboBox::Redo
void wxComboBox_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Redo();

}

// wxComboBox::Replace
void wxComboBox_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[3], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->Replace(from,to,value);

}

// wxComboBox::Remove
void wxComboBox_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  if(!This) throw wxe_badarg("This");
  This->Remove(from,to);

}

// wxComboBox::SetInsertionPoint
void wxComboBox_SetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  long pos;
  if(!enif_get_long(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->SetInsertionPoint(pos);

}

// wxComboBox::SetInsertionPointEnd
void wxComboBox_SetInsertionPointEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetInsertionPointEnd();

}

// wxComboBox::SetSelection
void wxComboBox_SetSelection_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  if(!This) throw wxe_badarg("This");
  This->SetSelection(from,to);

}

// wxComboBox::SetSelection
void wxComboBox_SetSelection_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSelection(n);

}

// wxComboBox::SetValue
void wxComboBox_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetValue(text);

}

// wxComboBox::Undo
void wxComboBox_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxComboBox *This;
  This = (wxComboBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Undo();

}

// wxGauge::wxGauge
void wxGauge_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGauge * Result = new EwxGauge();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGauge"));

}

// wxGauge::wxGauge
void wxGauge_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxGA_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // wxWindowID
  int range;
  if(!enif_get_int(env, argv[2], &range)) Badarg("range"); // int
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
  wxGauge * Result = new EwxGauge(parent,id,range,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGauge"));

}

// wxGauge::Create
void wxGauge_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxGA_HORIZONTAL;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int id;
  if(!enif_get_int(env, argv[2], &id)) Badarg("id"); // wxWindowID
  int range;
  if(!enif_get_int(env, argv[3], &range)) Badarg("range"); // int
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
  bool Result = This->Create(parent,id,range,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGauge::GetRange
void wxGauge_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRange();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGauge::GetValue
void wxGauge_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGauge::IsVertical
void wxGauge_IsVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVertical();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGauge::SetRange
void wxGauge_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  int range;
  if(!enif_get_int(env, argv[1], &range)) Badarg("range"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(range);

}

// wxGauge::SetValue
void wxGauge_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  if(!This) throw wxe_badarg("This");
  This->SetValue(pos);

}

// wxGauge::Pulse
void wxGauge_Pulse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGauge *This;
  This = (wxGauge *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Pulse();

}

// wxGenericDirCtrl::wxGenericDirCtrl
void wxGenericDirCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGenericDirCtrl * Result = new EwxGenericDirCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGenericDirCtrl"));

}

// wxGenericDirCtrl::wxGenericDirCtrl
void wxGenericDirCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxString dir= wxDirDialogDefaultFolderStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDIRCTRL_3D_INTERNAL;
  wxString filter= wxEmptyString;
  int defaultFilter=0;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  ErlNifBinary dir_bin;
  if(!enif_inspect_binary(env, tpl[1], &dir_bin)) Badarg("dir");
  dir = wxString(dir_bin.data, wxConvUTF8, dir_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "filter"))) {
  ErlNifBinary filter_bin;
  if(!enif_inspect_binary(env, tpl[1], &filter_bin)) Badarg("filter");
  filter = wxString(filter_bin.data, wxConvUTF8, filter_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultFilter"))) {
  if(!enif_get_int(env, tpl[1], &defaultFilter)) Badarg("defaultFilter"); // int
    } else        Badarg("Options");
  };
  wxGenericDirCtrl * Result = new EwxGenericDirCtrl(parent,id,dir,pos,size,style,filter,defaultFilter);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGenericDirCtrl"));

}

// wxGenericDirCtrl::Create
void wxGenericDirCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxString dir= wxDirDialogDefaultFolderStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDIRCTRL_3D_INTERNAL;
  wxString filter= wxEmptyString;
  int defaultFilter=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "dir"))) {
  ErlNifBinary dir_bin;
  if(!enif_inspect_binary(env, tpl[1], &dir_bin)) Badarg("dir");
  dir = wxString(dir_bin.data, wxConvUTF8, dir_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "filter"))) {
  ErlNifBinary filter_bin;
  if(!enif_inspect_binary(env, tpl[1], &filter_bin)) Badarg("filter");
  filter = wxString(filter_bin.data, wxConvUTF8, filter_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultFilter"))) {
  if(!enif_get_int(env, tpl[1], &defaultFilter)) Badarg("defaultFilter"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,dir,pos,size,style,filter,defaultFilter);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGenericDirCtrl::Init
void wxGenericDirCtrl_Init(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Init();

}

// wxGenericDirCtrl::CollapseTree
void wxGenericDirCtrl_CollapseTree(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CollapseTree();

}

// wxGenericDirCtrl::ExpandPath
void wxGenericDirCtrl_ExpandPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->ExpandPath(path);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGenericDirCtrl::GetDefaultPath
void wxGenericDirCtrl_GetDefaultPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetDefaultPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetPath
void wxGenericDirCtrl_GetPath_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetPath
void wxGenericDirCtrl_GetPath_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 itemId_tmp;
  if(!enif_get_uint64(env, argv[1], &itemId_tmp)) Badarg("itemId");
  wxTreeItemId itemId = wxTreeItemId((void *) (wxUint64) itemId_tmp);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath(itemId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetFilePath
void wxGenericDirCtrl_GetFilePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFilePath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetFilter
void wxGenericDirCtrl_GetFilter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFilter();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGenericDirCtrl::GetFilterIndex
void wxGenericDirCtrl_GetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFilterIndex();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGenericDirCtrl::GetRootId
void wxGenericDirCtrl_GetRootId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetRootId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxGenericDirCtrl::GetTreeCtrl
void wxGenericDirCtrl_GetTreeCtrl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeCtrl * Result = (wxTreeCtrl*)This->GetTreeCtrl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTreeCtrl"));

}

// wxGenericDirCtrl::ReCreateTree
void wxGenericDirCtrl_ReCreateTree(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ReCreateTree();

}

// wxGenericDirCtrl::SetDefaultPath
void wxGenericDirCtrl_SetDefaultPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetDefaultPath(path);

}

// wxGenericDirCtrl::SetFilter
void wxGenericDirCtrl_SetFilter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filter_bin;
  wxString filter;
  if(!enif_inspect_binary(env, argv[1], &filter_bin)) Badarg("filter");
  filter = wxString(filter_bin.data, wxConvUTF8, filter_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFilter(filter);

}

// wxGenericDirCtrl::SetFilterIndex
void wxGenericDirCtrl_SetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFilterIndex(n);

}

// wxGenericDirCtrl::SetPath
void wxGenericDirCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGenericDirCtrl *This;
  This = (wxGenericDirCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(path);

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
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,label,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

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

