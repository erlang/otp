/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

// wxActivateEvent::GetActive
void wxActivateEvent_GetActive(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxActivateEvent *This;
  This = (wxActivateEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetActive();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxArtProvider::GetBitmap
void wxArtProvider_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArtClient client= wxART_OTHER;
  wxSize size= wxDefaultSize;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary id_bin;
  wxString id;
  if(!enif_inspect_binary(env, argv[0], &id_bin)) Badarg("id");
  id = wxString(id_bin.data, wxConvUTF8, id_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "client"))) {
  ErlNifBinary client_bin;
  if(!enif_inspect_binary(env, tpl[1], &client_bin)) Badarg("client");
  client = wxString(client_bin.data, wxConvUTF8, client_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else        Badarg("Options");
  };
  wxBitmap * Result = new wxBitmap(wxArtProvider::GetBitmap(id,client,size)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxArtProvider::GetIcon
void wxArtProvider_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArtClient client= wxART_OTHER;
  wxSize size= wxDefaultSize;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary id_bin;
  wxString id;
  if(!enif_inspect_binary(env, argv[0], &id_bin)) Badarg("id");
  id = wxString(id_bin.data, wxConvUTF8, id_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "client"))) {
  ErlNifBinary client_bin;
  if(!enif_inspect_binary(env, tpl[1], &client_bin)) Badarg("client");
  client = wxString(client_bin.data, wxConvUTF8, client_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, tpl[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  size = wxSize(sizeW,sizeH);
    } else        Badarg("Options");
  };
  wxIcon * Result = new wxIcon(wxArtProvider::GetIcon(id,client,size)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

#if wxUSE_AUI
// wxAuiDockArt::GetColour
void wxAuiDockArt_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiDockArt *This;
  This = (wxAuiDockArt *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetColour(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxAuiDockArt::GetFont
void wxAuiDockArt_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiDockArt *This;
  This = (wxAuiDockArt *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetFont(id)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxAuiDockArt::GetMetric
void wxAuiDockArt_GetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiDockArt *This;
  This = (wxAuiDockArt *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMetric(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiDockArt::SetColour
void wxAuiDockArt_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiDockArt *This;
  This = (wxAuiDockArt *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
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
  This->SetColour(id,colour);

}

// wxAuiDockArt::SetFont
void wxAuiDockArt_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiDockArt *This;
  This = (wxAuiDockArt *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(id,*font);

}

// wxAuiDockArt::SetMetric
void wxAuiDockArt_SetMetric(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiDockArt *This;
  This = (wxAuiDockArt *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  int new_val;
  if(!enif_get_int(env, argv[2], &new_val)) Badarg("new_val"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMetric(id,new_val);

}

#endif // wxUSE_AUI
#if wxUSE_AUI
// wxAuiManager::wxAuiManager
void wxAuiManager_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * managed_wnd=NULL;
  unsigned int flags=wxAUI_MGR_DEFAULT;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "managed_wnd"))) {
  managed_wnd = (wxWindow *) memenv->getPtr(env, tpl[1], "managed_wnd");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_uint(env, tpl[1], &flags)) Badarg("flags");
    } else        Badarg("Options");
  };
  wxAuiManager * Result = new EwxAuiManager(managed_wnd,flags);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiManager"));

}

// wxAuiManager::AddPane
void wxAuiManager_AddPane_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  wxAuiPaneInfo *pane_info;
  pane_info = (wxAuiPaneInfo *) memenv->getPtr(env, argv[2], "pane_info");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPane(window,*pane_info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManager::AddPane
void wxAuiManager_AddPane_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int direction=wxLEFT;
  wxString caption= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "direction"))) {
  if(!enif_get_int(env, tpl[1], &direction)) Badarg("direction"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "caption"))) {
  ErlNifBinary caption_bin;
  if(!enif_inspect_binary(env, tpl[1], &caption_bin)) Badarg("caption");
  caption = wxString(caption_bin.data, wxConvUTF8, caption_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPane(window,direction,caption);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManager::AddPane
void wxAuiManager_AddPane_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  wxAuiPaneInfo *pane_info;
  pane_info = (wxAuiPaneInfo *) memenv->getPtr(env, argv[2], "pane_info");
  const ERL_NIF_TERM *drop_pos_t;
  int drop_pos_sz;
  if(!enif_get_tuple(env, argv[3], &drop_pos_sz, &drop_pos_t)) Badarg("drop_pos");
  int drop_posX;
  if(!enif_get_int(env, drop_pos_t[0], &drop_posX)) Badarg("drop_pos");
  int drop_posY;
  if(!enif_get_int(env, drop_pos_t[1], &drop_posY)) Badarg("drop_pos");
  wxPoint drop_pos = wxPoint(drop_posX,drop_posY);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPane(window,*pane_info,drop_pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManager::DetachPane
void wxAuiManager_DetachPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DetachPane(window);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManager::GetAllPanes
void wxAuiManager_GetAllPanes(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfoArray Result = This->GetAllPanes();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_array_objs(Result, app, "wxAuiPaneInfo"));

}

// wxAuiManager::GetArtProvider
void wxAuiManager_GetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiDockArt * Result = (wxAuiDockArt*)This->GetArtProvider();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiDockArt"));

}

// wxAuiManager::GetDockSizeConstraint
void wxAuiManager_GetDockSizeConstraint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  double widthpct;
  double heightpct;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetDockSizeConstraint(&widthpct,&heightpct);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_double(widthpct),
  rt.make_double(heightpct));
  rt.send(msg);

}

// wxAuiManager::GetFlags
void wxAuiManager_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxAuiManager::GetManagedWindow
void wxAuiManager_GetManagedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetManagedWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxAuiManager::GetManager
void wxAuiManager_GetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  wxAuiManager * Result = (wxAuiManager*)wxAuiManager::GetManager(window);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiManager"));

}

// wxAuiManager::GetPane
void wxAuiManager_GetPane_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->GetPane(window);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiManager::GetPane
void wxAuiManager_GetPane_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->GetPane(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiManager::HideHint
void wxAuiManager_HideHint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HideHint();

}

// wxAuiManager::InsertPane
void wxAuiManager_InsertPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int insert_level=wxAUI_INSERT_PANE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  wxAuiPaneInfo *insert_location;
  insert_location = (wxAuiPaneInfo *) memenv->getPtr(env, argv[2], "insert_location");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "insert_level"))) {
  if(!enif_get_int(env, tpl[1], &insert_level)) Badarg("insert_level"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertPane(window,*insert_location,insert_level);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManager::LoadPaneInfo
void wxAuiManager_LoadPaneInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary pane_part_bin;
  wxString pane_part;
  if(!enif_inspect_binary(env, argv[1], &pane_part_bin)) Badarg("pane_part");
  pane_part = wxString(pane_part_bin.data, wxConvUTF8, pane_part_bin.size);
  wxAuiPaneInfo *pane;
  pane = (wxAuiPaneInfo *) memenv->getPtr(env, argv[2], "pane");
  if(!This) throw wxe_badarg("This");
  This->LoadPaneInfo(pane_part,*pane);

}

// wxAuiManager::LoadPerspective
void wxAuiManager_LoadPerspective(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool update=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary perspective_bin;
  wxString perspective;
  if(!enif_inspect_binary(env, argv[1], &perspective_bin)) Badarg("perspective");
  perspective = wxString(perspective_bin.data, wxConvUTF8, perspective_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "update"))) {
  update = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadPerspective(perspective,update);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManager::SavePaneInfo
void wxAuiManager_SavePaneInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxAuiPaneInfo *pane;
  pane = (wxAuiPaneInfo *) memenv->getPtr(env, argv[1], "pane");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->SavePaneInfo(*pane);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxAuiManager::SavePerspective
void wxAuiManager_SavePerspective(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->SavePerspective();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxAuiManager::SetArtProvider
void wxAuiManager_SetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxAuiDockArt *art_provider;
  art_provider = (wxAuiDockArt *) memenv->getPtr(env, argv[1], "art_provider");
  if(!This) throw wxe_badarg("This");
  This->SetArtProvider(art_provider);

}

// wxAuiManager::SetDockSizeConstraint
void wxAuiManager_SetDockSizeConstraint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  double widthpct;
  if(!wxe_get_double(env, argv[1], &widthpct)) Badarg("widthpct");
  double heightpct;
  if(!wxe_get_double(env, argv[2], &heightpct)) Badarg("heightpct");
  if(!This) throw wxe_badarg("This");
  This->SetDockSizeConstraint(widthpct,heightpct);

}

// wxAuiManager::SetFlags
void wxAuiManager_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  unsigned int flags;
  if(!enif_get_uint(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

// wxAuiManager::SetManagedWindow
void wxAuiManager_SetManagedWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  wxWindow *managed_wnd;
  managed_wnd = (wxWindow *) memenv->getPtr(env, argv[1], "managed_wnd");
  if(!This) throw wxe_badarg("This");
  This->SetManagedWindow(managed_wnd);

}

// wxAuiManager::ShowHint
void wxAuiManager_ShowHint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
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
  This->ShowHint(rect);

}

// wxAuiManager::UnInit
void wxAuiManager_UnInit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->UnInit();

}

// wxAuiManager::Update
void wxAuiManager_Update(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManager *This;
  This = (wxAuiManager *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Update();

}

#endif // wxUSE_AUI
// wxAuiManagerEvent::SetManager
void wxAuiManagerEvent_SetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  wxAuiManager *manager;
  manager = (wxAuiManager *) memenv->getPtr(env, argv[1], "manager");
  if(!This) throw wxe_badarg("This");
  This->SetManager(manager);

}

// wxAuiManagerEvent::GetManager
void wxAuiManagerEvent_GetManager(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiManager * Result = (wxAuiManager*)This->GetManager();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiManager"));

}

// wxAuiManagerEvent::SetPane
void wxAuiManagerEvent_SetPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  wxAuiPaneInfo *pane;
  pane = (wxAuiPaneInfo *) memenv->getPtr(env, argv[1], "pane");
  if(!This) throw wxe_badarg("This");
  This->SetPane(pane);

}

// wxAuiManagerEvent::GetPane
void wxAuiManagerEvent_GetPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = (wxAuiPaneInfo*)This->GetPane();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiManagerEvent::SetButton
void wxAuiManagerEvent_SetButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  int button;
  if(!enif_get_int(env, argv[1], &button)) Badarg("button"); // int
  if(!This) throw wxe_badarg("This");
  This->SetButton(button);

}

// wxAuiManagerEvent::GetButton
void wxAuiManagerEvent_GetButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiManagerEvent::SetDC
void wxAuiManagerEvent_SetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  wxDC *pdc;
  pdc = (wxDC *) memenv->getPtr(env, argv[1], "pdc");
  if(!This) throw wxe_badarg("This");
  This->SetDC(pdc);

}

// wxAuiManagerEvent::GetDC
void wxAuiManagerEvent_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDC * Result = (wxDC*)This->GetDC();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDC"));

}

// wxAuiManagerEvent::Veto
void wxAuiManagerEvent_Veto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool veto=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "veto"))) {
  veto = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Veto(veto);

}

// wxAuiManagerEvent::GetVeto
void wxAuiManagerEvent_GetVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetVeto();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiManagerEvent::SetCanVeto
void wxAuiManagerEvent_SetCanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  bool can_veto;
  can_veto = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetCanVeto(can_veto);

}

// wxAuiManagerEvent::CanVeto
void wxAuiManagerEvent_CanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiManagerEvent *This;
  This = (wxAuiManagerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanVeto();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#if wxUSE_AUI
// wxAuiNotebook::wxAuiNotebook
void wxAuiNotebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxAuiNotebook * Result = new EwxAuiNotebook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiNotebook"));

}

// wxAuiNotebook::wxAuiNotebook
void wxAuiNotebook_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxAUI_NB_DEFAULT_STYLE;
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
  wxAuiNotebook * Result = new EwxAuiNotebook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiNotebook"));

}

// wxAuiNotebook::AddPage
void wxAuiNotebook_AddPage_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool select=false;
  const wxBitmap * bitmap= &wxNullBitmap;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[1], "page");
  ErlNifBinary caption_bin;
  wxString caption;
  if(!enif_inspect_binary(env, argv[2], &caption_bin)) Badarg("caption");
  caption = wxString(caption_bin.data, wxConvUTF8, caption_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "select"))) {
  select = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "bitmap"))) {
  bitmap = (wxBitmap *) memenv->getPtr(env, tpl[1], "bitmap");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPage(page,caption,select,*bitmap);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::AddPage
void wxAuiNotebook_AddPage_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[1], "page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  bool select;
  select = enif_is_identical(argv[3], WXE_ATOM_true);
  int imageId;
  if(!enif_get_int(env, argv[4], &imageId)) Badarg("imageId"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPage(page,text,select,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::Create
void wxAuiNotebook_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
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

// wxAuiNotebook::Create
void wxAuiNotebook_Create_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  int winid;
  if(!enif_get_int(env, argv[2], &winid)) Badarg("winid"); // wxWindowID
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
  bool Result = This->Create(parent,winid,pos,size,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::DeletePage
void wxAuiNotebook_DeletePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeletePage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::GetArtProvider
void wxAuiNotebook_GetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiTabArt * Result = (wxAuiTabArt*)This->GetArtProvider();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiTabArt"));

}

// wxAuiNotebook::GetPage
void wxAuiNotebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page_idx;
  if(!wxe_get_size_t(env, argv[1], &page_idx)) Badarg("page_idx");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page_idx);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxAuiNotebook::GetPageBitmap
void wxAuiNotebook_GetPageBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetPageBitmap(page)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxAuiNotebook::GetPageCount
void wxAuiNotebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiNotebook::GetPageIndex
void wxAuiNotebook_GetPageIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  wxWindow *page_wnd;
  page_wnd = (wxWindow *) memenv->getPtr(env, argv[1], "page_wnd");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageIndex(page_wnd);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiNotebook::GetPageText
void wxAuiNotebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxAuiNotebook::GetSelection
void wxAuiNotebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiNotebook::InsertPage
void wxAuiNotebook_InsertPage_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool select=false;
  const wxBitmap * bitmap= &wxNullBitmap;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page_idx;
  if(!wxe_get_size_t(env, argv[1], &page_idx)) Badarg("page_idx");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[2], "page");
  ErlNifBinary caption_bin;
  wxString caption;
  if(!enif_inspect_binary(env, argv[3], &caption_bin)) Badarg("caption");
  caption = wxString(caption_bin.data, wxConvUTF8, caption_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "select"))) {
  select = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "bitmap"))) {
  bitmap = (wxBitmap *) memenv->getPtr(env, tpl[1], "bitmap");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertPage(page_idx,page,caption,select,*bitmap);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::InsertPage
void wxAuiNotebook_InsertPage_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[2], "page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[3], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  bool select;
  select = enif_is_identical(argv[4], WXE_ATOM_true);
  int imageId;
  if(!enif_get_int(env, argv[5], &imageId)) Badarg("imageId"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertPage(index,page,text,select,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::RemovePage
void wxAuiNotebook_RemovePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RemovePage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::SetArtProvider
void wxAuiNotebook_SetArtProvider(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  wxAuiTabArt *art;
  art = (wxAuiTabArt *) memenv->getPtr(env, argv[1], "art");
  if(!This) throw wxe_badarg("This");
  This->SetArtProvider(art);

}

// wxAuiNotebook::SetFont
void wxAuiNotebook_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetFont(*font);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::SetPageBitmap
void wxAuiNotebook_SetPageBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[2], "bitmap");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageBitmap(page,*bitmap);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::SetPageText
void wxAuiNotebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageText(page,text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiNotebook::SetSelection
void wxAuiNotebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  size_t new_page;
  if(!wxe_get_size_t(env, argv[1], &new_page)) Badarg("new_page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(new_page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiNotebook::SetTabCtrlHeight
void wxAuiNotebook_SetTabCtrlHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetTabCtrlHeight(height);

}

// wxAuiNotebook::SetUniformBitmapSize
void wxAuiNotebook_SetUniformBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebook *This;
  This = (wxAuiNotebook *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetUniformBitmapSize(size);

}

#endif // wxUSE_AUI
// wxAuiNotebookEvent::SetSelection
void wxAuiNotebookEvent_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebookEvent *This;
  This = (wxAuiNotebookEvent *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSelection(page);

}

// wxAuiNotebookEvent::GetSelection
void wxAuiNotebookEvent_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebookEvent *This;
  This = (wxAuiNotebookEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiNotebookEvent::SetOldSelection
void wxAuiNotebookEvent_SetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebookEvent *This;
  This = (wxAuiNotebookEvent *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetOldSelection(page);

}

// wxAuiNotebookEvent::GetOldSelection
void wxAuiNotebookEvent_GetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebookEvent *This;
  This = (wxAuiNotebookEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOldSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiNotebookEvent::SetDragSource
void wxAuiNotebookEvent_SetDragSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebookEvent *This;
  This = (wxAuiNotebookEvent *) memenv->getPtr(env, argv[0], "This");
  wxAuiNotebook *s;
  s = (wxAuiNotebook *) memenv->getPtr(env, argv[1], "s");
  if(!This) throw wxe_badarg("This");
  This->SetDragSource(s);

}

// wxAuiNotebookEvent::GetDragSource
void wxAuiNotebookEvent_GetDragSource(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiNotebookEvent *This;
  This = (wxAuiNotebookEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiNotebook * Result = (wxAuiNotebook*)This->GetDragSource();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiNotebook"));

}

#if wxUSE_AUI
// wxAuiPaneInfo::wxAuiPaneInfo
void wxAuiPaneInfo_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxAuiPaneInfo * Result = new wxAuiPaneInfo();
  app->newPtr((void *) Result, 158, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::wxAuiPaneInfo
void wxAuiPaneInfo_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *c;
  c = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "c");
  wxAuiPaneInfo * Result = new wxAuiPaneInfo(*c);
  app->newPtr((void *) Result, 158, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::BestSize
void wxAuiPaneInfo_BestSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->BestSize(size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::BestSize
void wxAuiPaneInfo_BestSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->BestSize(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Bottom
void wxAuiPaneInfo_Bottom(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Bottom();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::BottomDockable
void wxAuiPaneInfo_BottomDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->BottomDockable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Caption
void wxAuiPaneInfo_Caption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary c_bin;
  wxString c;
  if(!enif_inspect_binary(env, argv[1], &c_bin)) Badarg("c");
  c = wxString(c_bin.data, wxConvUTF8, c_bin.size);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Caption(c);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::CaptionVisible
void wxAuiPaneInfo_CaptionVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->CaptionVisible(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Centre
void wxAuiPaneInfo_Centre(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Centre();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::CentrePane
void wxAuiPaneInfo_CentrePane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->CentrePane();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::CloseButton
void wxAuiPaneInfo_CloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->CloseButton(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::DefaultPane
void wxAuiPaneInfo_DefaultPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->DefaultPane();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::DestroyOnClose
void wxAuiPaneInfo_DestroyOnClose(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->DestroyOnClose(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Direction
void wxAuiPaneInfo_Direction(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int direction;
  if(!enif_get_int(env, argv[1], &direction)) Badarg("direction"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Direction(direction);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Dock
void wxAuiPaneInfo_Dock(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Dock();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Dockable
void wxAuiPaneInfo_Dockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Dockable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Fixed
void wxAuiPaneInfo_Fixed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Fixed();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Float
void wxAuiPaneInfo_Float(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Float();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Floatable
void wxAuiPaneInfo_Floatable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Floatable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::FloatingPosition
void wxAuiPaneInfo_FloatingPosition_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->FloatingPosition(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::FloatingPosition
void wxAuiPaneInfo_FloatingPosition_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->FloatingPosition(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::FloatingSize
void wxAuiPaneInfo_FloatingSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->FloatingSize(size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::FloatingSize
void wxAuiPaneInfo_FloatingSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->FloatingSize(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Gripper
void wxAuiPaneInfo_Gripper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Gripper(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::GripperTop
void wxAuiPaneInfo_GripperTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool attop=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "attop"))) {
  attop = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->GripperTop(attop);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::HasBorder
void wxAuiPaneInfo_HasBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBorder();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasCaption
void wxAuiPaneInfo_HasCaption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasCaption();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasCloseButton
void wxAuiPaneInfo_HasCloseButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasCloseButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasFlag
void wxAuiPaneInfo_HasFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int flag;
  if(!enif_get_int(env, argv[1], &flag)) Badarg("flag"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFlag(flag);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasGripper
void wxAuiPaneInfo_HasGripper(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasGripper();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasGripperTop
void wxAuiPaneInfo_HasGripperTop(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasGripperTop();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasMaximizeButton
void wxAuiPaneInfo_HasMaximizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasMaximizeButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasMinimizeButton
void wxAuiPaneInfo_HasMinimizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasMinimizeButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::HasPinButton
void wxAuiPaneInfo_HasPinButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasPinButton();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::Hide
void wxAuiPaneInfo_Hide(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Hide();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::IsBottomDockable
void wxAuiPaneInfo_IsBottomDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsBottomDockable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsDocked
void wxAuiPaneInfo_IsDocked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsDocked();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsFixed
void wxAuiPaneInfo_IsFixed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFixed();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsFloatable
void wxAuiPaneInfo_IsFloatable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFloatable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsFloating
void wxAuiPaneInfo_IsFloating(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsFloating();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsLeftDockable
void wxAuiPaneInfo_IsLeftDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsLeftDockable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsMovable
void wxAuiPaneInfo_IsMovable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsMovable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsOk
void wxAuiPaneInfo_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsResizable
void wxAuiPaneInfo_IsResizable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsResizable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsRightDockable
void wxAuiPaneInfo_IsRightDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsRightDockable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsShown
void wxAuiPaneInfo_IsShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsShown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsToolbar
void wxAuiPaneInfo_IsToolbar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsToolbar();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::IsTopDockable
void wxAuiPaneInfo_IsTopDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsTopDockable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxAuiPaneInfo::Layer
void wxAuiPaneInfo_Layer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int layer;
  if(!enif_get_int(env, argv[1], &layer)) Badarg("layer"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Layer(layer);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Left
void wxAuiPaneInfo_Left(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Left();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::LeftDockable
void wxAuiPaneInfo_LeftDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->LeftDockable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::MaxSize
void wxAuiPaneInfo_MaxSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->MaxSize(size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::MaxSize
void wxAuiPaneInfo_MaxSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->MaxSize(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::MaximizeButton
void wxAuiPaneInfo_MaximizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->MaximizeButton(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::MinSize
void wxAuiPaneInfo_MinSize_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->MinSize(size);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::MinSize
void wxAuiPaneInfo_MinSize_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->MinSize(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::MinimizeButton
void wxAuiPaneInfo_MinimizeButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->MinimizeButton(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Movable
void wxAuiPaneInfo_Movable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Movable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Name
void wxAuiPaneInfo_Name(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary n_bin;
  wxString n;
  if(!enif_inspect_binary(env, argv[1], &n_bin)) Badarg("n");
  n = wxString(n_bin.data, wxConvUTF8, n_bin.size);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Name(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::PaneBorder
void wxAuiPaneInfo_PaneBorder(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->PaneBorder(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::PinButton
void wxAuiPaneInfo_PinButton(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool visible=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "visible"))) {
  visible = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->PinButton(visible);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Position
void wxAuiPaneInfo_Position(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Position(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Resizable
void wxAuiPaneInfo_Resizable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool resizable=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "resizable"))) {
  resizable = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Resizable(resizable);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Right
void wxAuiPaneInfo_Right(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Right();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::RightDockable
void wxAuiPaneInfo_RightDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->RightDockable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Row
void wxAuiPaneInfo_Row(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int row;
  if(!enif_get_int(env, argv[1], &row)) Badarg("row"); // int
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Row(row);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::SafeSet
void wxAuiPaneInfo_SafeSet(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  wxAuiPaneInfo *source;
  source = (wxAuiPaneInfo *) memenv->getPtr(env, argv[1], "source");
  if(!This) throw wxe_badarg("This");
  This->SafeSet(*source);

}

// wxAuiPaneInfo::SetFlag
void wxAuiPaneInfo_SetFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  int flag;
  if(!enif_get_int(env, argv[1], &flag)) Badarg("flag"); // int
  bool option_state;
  option_state = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->SetFlag(flag,option_state);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Show
void wxAuiPaneInfo_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
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
  wxAuiPaneInfo * Result = &This->Show(show);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::ToolbarPane
void wxAuiPaneInfo_ToolbarPane(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->ToolbarPane();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Top
void wxAuiPaneInfo_Top(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Top();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::TopDockable
void wxAuiPaneInfo_TopDockable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool b=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  b = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->TopDockable(b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::Window
void wxAuiPaneInfo_Window(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
  wxWindow *w;
  w = (wxWindow *) memenv->getPtr(env, argv[1], "w");
  if(!This) throw wxe_badarg("This");
  wxAuiPaneInfo * Result = &This->Window(w);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiPaneInfo"));

}

// wxAuiPaneInfo::GetWindow
void wxAuiPaneInfo_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
 #endif
 if(!This) throw wxe_badarg(0);
 wxWindow* Result = This->window;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxAuiPaneInfo::GetFrame
void wxAuiPaneInfo_GetFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  wxFrame * Result = (wxFrame*)This->GetFrame();
 #endif
 if(!This) throw wxe_badarg(0);
 wxFrame* Result = This->frame;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxAuiPaneInfo::GetDirection
void wxAuiPaneInfo_GetDirection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDirection();
 #endif
 if(!This) throw wxe_badarg(0);
 int Result = This->dock_direction;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiPaneInfo::GetLayer
void wxAuiPaneInfo_GetLayer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLayer();
 #endif
 if(!This) throw wxe_badarg(0);
 int Result = This->dock_layer;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiPaneInfo::GetRow
void wxAuiPaneInfo_GetRow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRow();
 #endif
 if(!This) throw wxe_badarg(0);
 int Result = This->dock_row;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiPaneInfo::GetPosition
void wxAuiPaneInfo_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPosition();
 #endif
 if(!This) throw wxe_badarg(0);
 int Result = This->dock_pos;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxAuiPaneInfo::GetFloatingPosition
void wxAuiPaneInfo_GetFloatingPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetFloatingPosition();
 #endif
 if(!This) throw wxe_badarg(0);
 wxPoint Result = This->floating_pos;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxAuiPaneInfo::GetFloatingSize
void wxAuiPaneInfo_GetFloatingSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 #if 0
;
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetFloatingSize();
 #endif
 if(!This) throw wxe_badarg(0);
 wxSize Result = This->floating_size;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxAuiPaneInfo::destroy
void wxAuiPaneInfo_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiPaneInfo *This;
  This = (wxAuiPaneInfo *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

#endif // wxUSE_AUI
#if wxUSE_AUI
// wxAuiSimpleTabArt::wxAuiSimpleTabArt
void wxAuiSimpleTabArt_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxAuiSimpleTabArt * Result = new wxAuiSimpleTabArt();
  app->newPtr((void *) Result, 162, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxAuiSimpleTabArt"));

}

// wxAuiSimpleTabArt::destroy
void wxAuiSimpleTabArt_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiSimpleTabArt *This;
  This = (wxAuiSimpleTabArt *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

#endif // wxUSE_AUI
#if wxUSE_AUI
// wxAuiTabArt::SetFlags
void wxAuiTabArt_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiTabArt *This;
  This = (wxAuiTabArt *) memenv->getPtr(env, argv[0], "This");
  unsigned int flags;
  if(!enif_get_uint(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

// wxAuiTabArt::SetMeasuringFont
void wxAuiTabArt_SetMeasuringFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiTabArt *This;
  This = (wxAuiTabArt *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetMeasuringFont(*font);

}

// wxAuiTabArt::SetNormalFont
void wxAuiTabArt_SetNormalFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiTabArt *This;
  This = (wxAuiTabArt *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetNormalFont(*font);

}

// wxAuiTabArt::SetSelectedFont
void wxAuiTabArt_SetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiTabArt *This;
  This = (wxAuiTabArt *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetSelectedFont(*font);

}

// wxAuiTabArt::SetColour
void wxAuiTabArt_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiTabArt *This;
  This = (wxAuiTabArt *) memenv->getPtr(env, argv[0], "This");
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

// wxAuiTabArt::SetActiveColour
void wxAuiTabArt_SetActiveColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxAuiTabArt *This;
  This = (wxAuiTabArt *) memenv->getPtr(env, argv[0], "This");
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
  This->SetActiveColour(colour);

}

#endif // wxUSE_AUI
// wxBitmap::wxBitmap
void wxBitmap_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxBitmap * Result = new EwxBitmap();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::wxBitmap
void wxBitmap_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int depth=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  char * bits;
  ErlNifBinary bits_bin;
  if(!enif_inspect_binary(env, argv[0], &bits_bin)) Badarg("bits");
  bits = (char*) bits_bin.data;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "depth"))) {
  if(!enif_get_int(env, tpl[1], &depth)) Badarg("depth"); // int
    } else        Badarg("Options");
  };
  wxBitmap * Result = new EwxBitmap(bits,width,height,depth);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::wxBitmap
void wxBitmap_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int depth=wxBITMAP_SCREEN_DEPTH;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "depth"))) {
  if(!enif_get_int(env, tpl[1], &depth)) Badarg("depth"); // int
    } else        Badarg("Options");
  };
  wxBitmap * Result = new EwxBitmap(width,height,depth);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::wxBitmap
void wxBitmap_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int depth=wxBITMAP_SCREEN_DEPTH;
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
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "depth"))) {
  if(!enif_get_int(env, tpl[1], &depth)) Badarg("depth"); // int
    } else        Badarg("Options");
  };
  wxBitmap * Result = new EwxBitmap(sz,depth);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::wxBitmap
void wxBitmap_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_DEFAULT_TYPE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "type"))) {
  if(!enif_get_int(env, tpl[1], (int *) &type)) Badarg("type"); // enum
    } else        Badarg("Options");
  };
  wxBitmap * Result = new EwxBitmap(name,type);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::wxBitmap
void wxBitmap_new_2_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int depth=wxBITMAP_SCREEN_DEPTH;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *img;
  img = (wxImage *) memenv->getPtr(env, argv[0], "img");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "depth"))) {
  if(!enif_get_int(env, tpl[1], &depth)) Badarg("depth"); // int
    } else        Badarg("Options");
  };
  wxBitmap * Result = new EwxBitmap(*img,depth);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::wxBitmap
void wxBitmap_new_2_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM img_type;
  void * img = memenv->getPtr(env, argv[0], "img", &img_type);
  wxBitmap * Result;
  if(enif_is_identical(img_type, WXE_ATOM_wxImage))
    Result = new EwxBitmap(* static_cast<wxImage*> (img));
  else if(enif_is_identical(img_type, WXE_ATOM_wxBitmap))
    Result = new EwxBitmap(* static_cast<wxBitmap*> (img));
  else throw wxe_badarg("img");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::ConvertToImage
void wxBitmap_ConvertToImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToImage()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxBitmap::CopyFromIcon
void wxBitmap_CopyFromIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CopyFromIcon(*icon);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::Create
void wxBitmap_Create_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int depth=wxBITMAP_SCREEN_DEPTH;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "depth"))) {
  if(!enif_get_int(env, tpl[1], &depth)) Badarg("depth"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,depth);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::Create
void wxBitmap_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int depth=wxBITMAP_SCREEN_DEPTH;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "depth"))) {
  if(!enif_get_int(env, tpl[1], &depth)) Badarg("depth"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,depth);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::Create
void wxBitmap_Create_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[3], "dc");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,*dc);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::GetDepth
void wxBitmap_GetDepth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetDepth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBitmap::GetHeight
void wxBitmap_GetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBitmap::GetPalette
void wxBitmap_GetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPalette * Result = (wxPalette*)This->GetPalette();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPalette"));

}

// wxBitmap::GetMask
void wxBitmap_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxMask * Result = (wxMask*)This->GetMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMask"));

}

// wxBitmap::GetWidth
void wxBitmap_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBitmap::GetSubBitmap
void wxBitmap_GetSubBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
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
  wxBitmap * Result = new wxBitmap(This->GetSubBitmap(rect)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmap::LoadFile
void wxBitmap_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_DEFAULT_TYPE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "type"))) {
  if(!enif_get_int(env, tpl[1], (int *) &type)) Badarg("type"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(name,type);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::IsOk
void wxBitmap_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::SaveFile
void wxBitmap_SaveFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxPalette * palette=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[2], (int *) &type)) Badarg("type"); // enum
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "palette"))) {
  palette = (wxPalette *) memenv->getPtr(env, tpl[1], "palette");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name,type,palette);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBitmap::SetDepth
void wxBitmap_SetDepth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  int depth;
  if(!enif_get_int(env, argv[1], &depth)) Badarg("depth"); // int
  if(!This) throw wxe_badarg("This");
  This->SetDepth(depth);

}

// wxBitmap::SetHeight
void wxBitmap_SetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
  This->SetHeight(height);

}

// wxBitmap::SetMask
void wxBitmap_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  wxMask *mask;
  mask = (wxMask *) memenv->getPtr(env, argv[1], "mask");
  if(!This) throw wxe_badarg("This");
  This->SetMask(mask);

}

// wxBitmap::SetPalette
void wxBitmap_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  wxPalette *palette;
  palette = (wxPalette *) memenv->getPtr(env, argv[1], "palette");
  if(!This) throw wxe_badarg("This");
  This->SetPalette(*palette);

}

// wxBitmap::SetWidth
void wxBitmap_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *This;
  This = (wxBitmap *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetWidth(width);

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

// wxBitmapDataObject::wxBitmapDataObject
void wxBitmapDataObject_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[0], "bitmap");
  wxBitmapDataObject * Result = new wxBitmapDataObject(*bitmap);
  app->newPtr((void *) Result, 218, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmapDataObject"));

}

// wxBitmapDataObject::wxBitmapDataObject
void wxBitmapDataObject_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxBitmap * bitmap= &wxNullBitmap;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bitmap"))) {
  bitmap = (wxBitmap *) memenv->getPtr(env, tpl[1], "bitmap");
    } else        Badarg("Options");
  };
  wxBitmapDataObject * Result = new wxBitmapDataObject(*bitmap);
  app->newPtr((void *) Result, 218, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmapDataObject"));

}

// wxBitmapDataObject::GetBitmap
void wxBitmapDataObject_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmapDataObject *This;
  This = (wxBitmapDataObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmap()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBitmapDataObject::SetBitmap
void wxBitmapDataObject_SetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmapDataObject *This;
  This = (wxBitmapDataObject *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SetBitmap(*bitmap);

}

// wxBitmapDataObject::destroy
void wxBitmapDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxBitmapDataObject *This;
  This = (wxBitmapDataObject *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxBookCtrlBase::AddPage
void wxBookCtrlBase_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[1], "page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bSelect"))) {
  bSelect = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "imageId"))) {
  if(!enif_get_int(env, tpl[1], &imageId)) Badarg("imageId"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddPage(page,text,bSelect,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBookCtrlBase::InsertPage
void wxBookCtrlBase_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t index;
  if(!wxe_get_size_t(env, argv[1], &index)) Badarg("index");
  wxWindow *page;
  page = (wxWindow *) memenv->getPtr(env, argv[2], "page");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bSelect"))) {
  bSelect = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "imageId"))) {
  if(!enif_get_int(env, tpl[1], &imageId)) Badarg("imageId"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->InsertPage(index,page,text,bSelect,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBookCtrlBase::DeletePage
void wxBookCtrlBase_DeletePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeletePage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBookCtrlBase::RemovePage
void wxBookCtrlBase_RemovePage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RemovePage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBookCtrlBase::DeleteAllPages
void wxBookCtrlBase_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBookCtrlBase::GetPage
void wxBookCtrlBase_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxBookCtrlBase::GetPageCount
void wxBookCtrlBase_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBookCtrlBase::GetCurrentPage
void wxBookCtrlBase_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxBookCtrlBase::AdvanceSelection
void wxBookCtrlBase_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool forward=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "forward"))) {
  forward = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->AdvanceSelection(forward);

}

// wxBookCtrlBase::SetSelection
void wxBookCtrlBase_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBookCtrlBase::GetSelection
void wxBookCtrlBase_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBookCtrlBase::ChangeSelection
void wxBookCtrlBase_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->ChangeSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBookCtrlBase::HitTest
void wxBookCtrlBase_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[1], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(pt,&flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(Result),
    rt.make_int(flags));
  rt.send(msg);

}

// wxBookCtrlBase::GetPageText
void wxBookCtrlBase_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxBookCtrlBase::SetPageText
void wxBookCtrlBase_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlBase *This;
  This = (wxBookCtrlBase *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageText(page,text);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBookCtrlEvent::GetOldSelection
void wxBookCtrlEvent_GetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlEvent *This;
  This = (wxBookCtrlEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOldSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBookCtrlEvent::GetSelection
void wxBookCtrlEvent_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlEvent *This;
  This = (wxBookCtrlEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBookCtrlEvent::SetOldSelection
void wxBookCtrlEvent_SetOldSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlEvent *This;
  This = (wxBookCtrlEvent *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetOldSelection(page);

}

// wxBookCtrlEvent::SetSelection
void wxBookCtrlEvent_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBookCtrlEvent *This;
  This = (wxBookCtrlEvent *) memenv->getPtr(env, argv[0], "This");
  int page;
  if(!enif_get_int(env, argv[1], &page)) Badarg("page"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSelection(page);

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

// wxBrush::wxBrush
void wxBrush_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxBrush * Result = new EwxBrush();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBrush"));

}

// wxBrush::wxBrush
void wxBrush_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBrushStyle style=wxBRUSHSTYLE_SOLID;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], (int *) &style)) Badarg("style"); // enum
    } else        Badarg("Options");
  };
  wxBrush * Result = new EwxBrush(colour,style);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBrush"));

}

// wxBrush::wxBrush
void wxBrush_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM brush_type;
  void * brush = memenv->getPtr(env, argv[0], "brush", &brush_type);
  wxBrush * Result;
  if(enif_is_identical(brush_type, WXE_ATOM_wxBrush))
    Result = new EwxBrush(* static_cast<wxBrush*> (brush));
  else if(enif_is_identical(brush_type, WXE_ATOM_wxBitmap))
    Result = new EwxBrush(* static_cast<wxBitmap*> (brush));
  else throw wxe_badarg("brush");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBrush"));

}

// wxBrush::GetColour
void wxBrush_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxBrush::GetStipple
void wxBrush_GetStipple(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = (wxBitmap*)This->GetStipple();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxBrush::GetStyle
void wxBrush_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxBrush::IsHatch
void wxBrush_IsHatch(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsHatch();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBrush::IsOk
void wxBrush_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxBrush::SetColour
void wxBrush_SetColour_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
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

// wxBrush::SetColour
void wxBrush_SetColour_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  unsigned int red;
  if(!enif_get_uint(env, argv[1], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[2], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[3], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  This->SetColour(red,green,blue);

}

// wxBrush::SetStipple
void wxBrush_SetStipple(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  if(!This) throw wxe_badarg("This");
  This->SetStipple(*bitmap);

}

// wxBrush::SetStyle
void wxBrush_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBrush *This;
  This = (wxBrush *) memenv->getPtr(env, argv[0], "This");
  wxBrushStyle style;
  if(!enif_get_int(env, argv[1], (int *) &style)) Badarg("style"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetStyle(style);

}

// wxBufferedDC::wxBufferedDC
void wxBufferedDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxBufferedDC * Result = new EwxBufferedDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBufferedDC"));

}

// wxBufferedDC::wxBufferedDC
void wxBufferedDC_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=wxBUFFER_CLIENT_AREA;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[0], "dc");
  const ERL_NIF_TERM *area_t;
  int area_sz;
  if(!enif_get_tuple(env, argv[1], &area_sz, &area_t)) Badarg("area");
  int areaW;
  if(!enif_get_int(env, area_t[0], &areaW)) Badarg("area");
  int areaH;
  if(!enif_get_int(env, area_t[1], &areaH)) Badarg("area");
  wxSize area = wxSize(areaW,areaH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
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
  wxBufferedDC * Result = new EwxBufferedDC(dc,area,style);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBufferedDC"));

}

// wxBufferedDC::wxBufferedDC
void wxBufferedDC_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxBitmap * buffer= &wxNullBitmap;
  int style=wxBUFFER_CLIENT_AREA;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[0], "dc");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "buffer"))) {
  buffer = (wxBitmap *) memenv->getPtr(env, tpl[1], "buffer");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  wxBufferedDC * Result = new EwxBufferedDC(dc,*buffer,style);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBufferedDC"));

}

// wxBufferedDC::Init
void wxBufferedDC_Init_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=wxBUFFER_CLIENT_AREA;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBufferedDC *This;
  This = (wxBufferedDC *) memenv->getPtr(env, argv[0], "This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  const ERL_NIF_TERM *area_t;
  int area_sz;
  if(!enif_get_tuple(env, argv[2], &area_sz, &area_t)) Badarg("area");
  int areaW;
  if(!enif_get_int(env, area_t[0], &areaW)) Badarg("area");
  int areaH;
  if(!enif_get_int(env, area_t[1], &areaH)) Badarg("area");
  wxSize area = wxSize(areaW,areaH);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
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
  if(!This) throw wxe_badarg("This");
  This->Init(dc,area,style);

}

// wxBufferedDC::Init
void wxBufferedDC_Init_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxBitmap * buffer= &wxNullBitmap;
  int style=wxBUFFER_CLIENT_AREA;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxBufferedDC *This;
  This = (wxBufferedDC *) memenv->getPtr(env, argv[0], "This");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "buffer"))) {
  buffer = (wxBitmap *) memenv->getPtr(env, tpl[1], "buffer");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Init(dc,*buffer,style);

}

// wxBufferedPaintDC::wxBufferedPaintDC
void wxBufferedPaintDC_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=wxBUFFER_CLIENT_AREA;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  wxBitmap *buffer;
  buffer = (wxBitmap *) memenv->getPtr(env, argv[1], "buffer");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
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
  wxBufferedPaintDC * Result = new EwxBufferedPaintDC(window,*buffer,style);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBufferedPaintDC"));

}

// wxBufferedPaintDC::wxBufferedPaintDC
void wxBufferedPaintDC_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=wxBUFFER_CLIENT_AREA;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
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
  wxBufferedPaintDC * Result = new EwxBufferedPaintDC(window,style);
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBufferedPaintDC"));

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

