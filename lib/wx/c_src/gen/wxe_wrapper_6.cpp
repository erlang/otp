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

#if wxUSE_GLCANVAS
// wxGLContext::wxGLContext
void wxGLContext_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGLContext * other=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *win;
  win = (wxGLCanvas *) memenv->getPtr(env, argv[0], "win");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "other"))) {
  other = (wxGLContext *) memenv->getPtr(env, tpl[1], "other");
    } else        Badarg("Options");
  };
  wxGLContext * Result = new EwxGLContext(win,other);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGLContext"));

}

// wxGLContext::SetCurrent
void wxGLContext_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLContext *This;
  This = (wxGLContext *) memenv->getPtr(env, argv[0], "This");
  wxGLCanvas *win;
  win = (wxGLCanvas *) memenv->getPtr(env, argv[1], "win");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetCurrent(*win);
 setActiveGL(memenv, Ecmd.caller, win, This);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif // wxUSE_GLCANVAS
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

// wxLayoutAlgorithm::wxLayoutAlgorithm
void wxLayoutAlgorithm_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxLayoutAlgorithm * Result = new EwxLayoutAlgorithm();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLayoutAlgorithm"));

}

// wxLayoutAlgorithm::LayoutFrame
void wxLayoutAlgorithm_LayoutFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * mainWindow=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLayoutAlgorithm *This;
  This = (wxLayoutAlgorithm *) memenv->getPtr(env, argv[0], "This");
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[1], "frame");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mainWindow"))) {
  mainWindow = (wxWindow *) memenv->getPtr(env, tpl[1], "mainWindow");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LayoutFrame(frame,mainWindow);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLayoutAlgorithm::LayoutMDIFrame
void wxLayoutAlgorithm_LayoutMDIFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRect *rect=NULL; wxRect rectTmp;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLayoutAlgorithm *This;
  This = (wxLayoutAlgorithm *) memenv->getPtr(env, argv[0], "This");
  wxMDIParentFrame *frame;
  frame = (wxMDIParentFrame *) memenv->getPtr(env, argv[1], "frame");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
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
  bool Result = This->LayoutMDIFrame(frame,rect);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLayoutAlgorithm::LayoutWindow
void wxLayoutAlgorithm_LayoutWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * mainWindow=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLayoutAlgorithm *This;
  This = (wxLayoutAlgorithm *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mainWindow"))) {
  mainWindow = (wxWindow *) memenv->getPtr(env, tpl[1], "mainWindow");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LayoutWindow(parent,mainWindow);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::GetId
void wxEvent_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxEvent::GetSkipped
void wxEvent_GetSkipped(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSkipped();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::GetTimestamp
void wxEvent_GetTimestamp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetTimestamp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxEvent::IsCommandEvent
void wxEvent_IsCommandEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsCommandEvent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::ResumePropagation
void wxEvent_ResumePropagation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  int propagationLevel;
  if(!enif_get_int(env, argv[1], &propagationLevel)) Badarg("propagationLevel"); // int
  if(!This) throw wxe_badarg("This");
  This->ResumePropagation(propagationLevel);

}

// wxEvent::ShouldPropagate
void wxEvent_ShouldPropagate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShouldPropagate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxEvent::Skip
void wxEvent_Skip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool skip=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "skip"))) {
  skip = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Skip(skip);

}

// wxEvent::StopPropagation
void wxEvent_StopPropagation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEvent *This;
  This = (wxEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->StopPropagation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCommandEvent::GetClientObject
void wxCommandEvent_getClientData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxeErlTerm * Result = (wxeErlTerm*)This->GetClientObject();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ext2term(Result));

}

// wxCommandEvent::GetExtraLong
void wxCommandEvent_GetExtraLong(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetExtraLong();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCommandEvent::GetInt
void wxCommandEvent_GetInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetInt();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCommandEvent::GetSelection
void wxCommandEvent_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCommandEvent::GetString
void wxCommandEvent_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetString();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxCommandEvent::IsChecked
void wxCommandEvent_IsChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsChecked();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCommandEvent::IsSelection
void wxCommandEvent_IsSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCommandEvent::SetInt
void wxCommandEvent_SetInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  int intCommand;
  if(!enif_get_int(env, argv[1], &intCommand)) Badarg("intCommand"); // int
  if(!This) throw wxe_badarg("This");
  This->SetInt(intCommand);

}

// wxCommandEvent::SetString
void wxCommandEvent_SetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCommandEvent *This;
  This = (wxCommandEvent *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetString(string);

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
  wxCursor *cursor;
  cursor = (wxCursor *) memenv->getPtr(env, argv[1], "cursor");
  if(!This) throw wxe_badarg("This");
  This->SetCursor(*cursor);

}

// wxKeyEvent::AltDown
void wxKeyEvent_AltDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AltDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::CmdDown
void wxKeyEvent_CmdDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CmdDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::ControlDown
void wxKeyEvent_ControlDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ControlDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::GetKeyCode
void wxKeyEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetModifiers
void wxKeyEvent_GetModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetModifiers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetPosition
void wxKeyEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxKeyEvent::GetRawKeyCode
void wxKeyEvent_GetRawKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRawKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxKeyEvent::GetRawKeyFlags
void wxKeyEvent_GetRawKeyFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetRawKeyFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxKeyEvent::GetUnicodeKey
void wxKeyEvent_GetUnicodeKey(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxChar Result = This->GetUnicodeKey();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetX
void wxKeyEvent_GetX(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetX();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::GetY
void wxKeyEvent_GetY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxCoord Result = This->GetY();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxKeyEvent::HasModifiers
void wxKeyEvent_HasModifiers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasModifiers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::MetaDown
void wxKeyEvent_MetaDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->MetaDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxKeyEvent::ShiftDown
void wxKeyEvent_ShiftDown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyEvent *This;
  This = (wxKeyEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->ShiftDown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

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

// wxEraseEvent::GetDC
void wxEraseEvent_GetDC(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxEraseEvent *This;
  This = (wxEraseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDC * Result = (wxDC*)This->GetDC();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDC"));

}

// wxFocusEvent::GetWindow
void wxFocusEvent_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFocusEvent *This;
  This = (wxFocusEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxChildFocusEvent::GetWindow
void wxChildFocusEvent_GetWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChildFocusEvent *This;
  This = (wxChildFocusEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetWindow();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

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

// wxCloseEvent::CanVeto
void wxCloseEvent_CanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCloseEvent *This;
  This = (wxCloseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanVeto();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCloseEvent::GetLoggingOff
void wxCloseEvent_GetLoggingOff(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCloseEvent *This;
  This = (wxCloseEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetLoggingOff();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCloseEvent::SetCanVeto
void wxCloseEvent_SetCanVeto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCloseEvent *This;
  This = (wxCloseEvent *) memenv->getPtr(env, argv[0], "This");
  bool canVeto;
  canVeto = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetCanVeto(canVeto);

}

// wxCloseEvent::SetLoggingOff
void wxCloseEvent_SetLoggingOff(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCloseEvent *This;
  This = (wxCloseEvent *) memenv->getPtr(env, argv[0], "This");
  bool loggingOff;
  loggingOff = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetLoggingOff(loggingOff);

}

// wxCloseEvent::Veto
void wxCloseEvent_Veto(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool veto=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCloseEvent *This;
  This = (wxCloseEvent *) memenv->getPtr(env, argv[0], "This");
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

// wxShowEvent::SetShow
void wxShowEvent_SetShow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxShowEvent *This;
  This = (wxShowEvent *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
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

// wxIconizeEvent::IsIconized
void wxIconizeEvent_IsIconized(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconizeEvent *This;
  This = (wxIconizeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsIconized();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

