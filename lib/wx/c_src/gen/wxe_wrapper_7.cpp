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

// utils::wxGetKeyState
void utils_wxGetKeyState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxKeyCode key;
  if(!enif_get_int(env, argv[0], (int *) &key)) Badarg("key"); // enum
  bool Result = ::wxGetKeyState(key);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxGetMousePosition
void utils_wxGetMousePosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint Result = ::wxGetMousePosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// utils::wxGetMouseState
void utils_wxGetMouseState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxMouseState Result = ::wxGetMouseState();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// utils::wxSetDetectableAutoRepeat
void utils_wxSetDetectableAutoRepeat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ERL_NIF_TERM * argv = Ecmd.args;
  bool flag;
  flag = enif_is_identical(argv[0], WXE_ATOM_true);
  bool Result = ::wxSetDetectableAutoRepeat(flag);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxBell
void utils_wxBell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ::wxBell();

}

// utils::wxFindMenuItemId
void utils_wxFindMenuItemId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[0], "frame");
  ErlNifBinary menuString_bin;
  wxString menuString;
  if(!enif_inspect_binary(env, argv[1], &menuString_bin)) Badarg("menuString");
  menuString = wxString(menuString_bin.data, wxConvUTF8, menuString_bin.size);
  ErlNifBinary itemString_bin;
  wxString itemString;
  if(!enif_inspect_binary(env, argv[2], &itemString_bin)) Badarg("itemString");
  itemString = wxString(itemString_bin.data, wxConvUTF8, itemString_bin.size);
  int Result = ::wxFindMenuItemId(frame,menuString,itemString);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// utils::wxFindWindowAtPoint
void utils_wxFindWindowAtPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[0], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  wxWindow * Result = (wxWindow*)::wxFindWindowAtPoint(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// utils::wxBeginBusyCursor
void utils_wxBeginBusyCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxCursor * cursor=wxHOURGLASS_CURSOR;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "cursor"))) {
  cursor = (wxCursor *) memenv->getPtr(env, tpl[1], "cursor");
    } else        Badarg("Options");
  };
  ::wxBeginBusyCursor(cursor);

}

// utils::wxEndBusyCursor
void utils_wxEndBusyCursor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ::wxEndBusyCursor();

}

// utils::wxIsBusy
void utils_wxIsBusy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool Result = ::wxIsBusy();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxShutdown
void utils_wxShutdown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxSHUTDOWN_POWEROFF;
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
    } else        Badarg("Options");
  };
  bool Result = ::wxShutdown(flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxShell
void utils_wxShell(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString command= wxEmptyString;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "command"))) {
  ErlNifBinary command_bin;
  if(!enif_inspect_binary(env, tpl[1], &command_bin)) Badarg("command");
  command = wxString(command_bin.data, wxConvUTF8, command_bin.size);
    } else        Badarg("Options");
  };
  bool Result = ::wxShell(command);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxLaunchDefaultBrowser
void utils_wxLaunchDefaultBrowser(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary url_bin;
  wxString url;
  if(!enif_inspect_binary(env, argv[0], &url_bin)) Badarg("url");
  url = wxString(url_bin.data, wxConvUTF8, url_bin.size);
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
  bool Result = ::wxLaunchDefaultBrowser(url,flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxGetEmailAddress
void utils_wxGetEmailAddress(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = ::wxGetEmailAddress();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// utils::wxGetUserId
void utils_wxGetUserId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = ::wxGetUserId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// utils::wxGetHomeDir
void utils_wxGetHomeDir(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = ::wxGetHomeDir();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// utils::wxNewId
void utils_wxNewId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID Result = ::wxNewId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// utils::wxRegisterId
void utils_wxRegisterId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int id;
  if(!enif_get_int(env, argv[0], &id)) Badarg("id"); // wxWindowID
  ::wxRegisterId(id);

}

// utils::wxGetCurrentId
void utils_wxGetCurrentId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID Result = ::wxGetCurrentId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// utils::wxGetOsDescription
void utils_wxGetOsDescription(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = ::wxGetOsDescription();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// utils::wxIsPlatformLittleEndian
void utils_wxIsPlatformLittleEndian(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool Result = ::wxIsPlatformLittleEndian();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// utils::wxIsPlatform64Bit
void utils_wxIsPlatform64Bit(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool Result = ::wxIsPlatform64Bit();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}


// wxTaskBarIcon::wxTaskBarIcon
void wxTaskBarIcon_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTaskBarIconType iconType = wxTBI_DEFAULT_TYPE;
  int createPopupMenu = 0;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "iconType"))) {
      if(!enif_get_int(env, tpl[1], (int *) &iconType)) Badarg("iconType");
    } else if(enif_is_identical(tpl[0], enif_make_atom(env, "createPopupMenu"))) {
      if(!enif_get_int(env, tpl[1], &createPopupMenu)) Badarg("createPopupMenu");
    } else  Badarg("Options");
  };

  EwxTaskBarIcon * Result = new EwxTaskBarIcon(iconType);
  if(createPopupMenu) {
    Result->createPopupMenu = createPopupMenu;
    Result->me_ref = memenv->me_ref;
  }
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTaskBarIcon"));

}

// wxTaskBarIcon::PopupMenu
void wxTaskBarIcon_PopupMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTaskBarIcon *This;
  This = (wxTaskBarIcon *) memenv->getPtr(env, argv[0], "This");
  wxMenu *menu;
  menu = (wxMenu *) memenv->getPtr(env, argv[1], "menu");
  if(!This) throw wxe_badarg("This");
  bool Result = This->PopupMenu(menu);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTaskBarIcon::RemoveIcon
void wxTaskBarIcon_RemoveIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTaskBarIcon *This;
  This = (wxTaskBarIcon *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RemoveIcon();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTaskBarIcon::SetIcon
void wxTaskBarIcon_SetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString tooltip= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTaskBarIcon *This;
  This = (wxTaskBarIcon *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "tooltip"))) {
  ErlNifBinary tooltip_bin;
  if(!enif_inspect_binary(env, tpl[1], &tooltip_bin)) Badarg("tooltip");
  tooltip = wxString(tooltip_bin.data, wxConvUTF8, tooltip_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetIcon(*icon,tooltip);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextAttr::wxTextAttr
void wxTextAttr_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTextAttr * Result = new wxTextAttr();
  app->newPtr((void *) Result, 104, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextAttr"));

}

// wxTextAttr::wxTextAttr
void wxTextAttr_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour colBack= wxNullColour;
  const wxFont * font= &wxNullFont;
 wxTextAttrAlignment alignment=wxTEXT_ALIGNMENT_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  const ERL_NIF_TERM *colText_t;
  int colText_sz;
  if(!enif_get_tuple(env, argv[0], &colText_sz, &colText_t)) Badarg("colText");
  int colTextR;
  if(!enif_get_int(env, colText_t[0], &colTextR)) Badarg("colText");
  int colTextG;
  if(!enif_get_int(env, colText_t[1], &colTextG)) Badarg("colText");
  int colTextB;
  if(!enif_get_int(env, colText_t[2], &colTextB)) Badarg("colText");
  int colTextA;
  if(!enif_get_int(env, colText_t[3], &colTextA)) Badarg("colText");
  wxColour colText = wxColour(colTextR,colTextG,colTextB,colTextA);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "colBack"))) {
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "font"))) {
  font = (wxFont *) memenv->getPtr(env, tpl[1], "font");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "alignment"))) {
  if(!enif_get_int(env, tpl[1], (int *) &alignment)) Badarg("alignment"); // enum
    } else        Badarg("Options");
  };
  wxTextAttr * Result = new wxTextAttr(colText,colBack,*font,alignment);
  app->newPtr((void *) Result, 104, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextAttr"));

}

// wxTextAttr::wxTextAttr
void wxTextAttr_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *attr;
  attr = (wxTextAttr *) memenv->getPtr(env, argv[0], "attr");
  wxTextAttr * Result = new wxTextAttr(*attr);
  app->newPtr((void *) Result, 104, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextAttr"));

}

// wxTextAttr::GetAlignment
void wxTextAttr_GetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetAlignment();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetBackgroundColour
void wxTextAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxTextAttr::GetFont
void wxTextAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxTextAttr::GetFontEncoding
void wxTextAttr_GetFontEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFontEncoding();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetFontFaceName
void wxTextAttr_GetFontFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetFontFaceName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextAttr::GetFontSize
void wxTextAttr_GetFontSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFontSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetFontStyle
void wxTextAttr_GetFontStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFontStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetFontUnderlined
void wxTextAttr_GetFontUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetFontUnderlined();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextAttr::GetFontWeight
void wxTextAttr_GetFontWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFontWeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetLeftIndent
void wxTextAttr_GetLeftIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetLeftIndent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetLeftSubIndent
void wxTextAttr_GetLeftSubIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetLeftSubIndent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetRightIndent
void wxTextAttr_GetRightIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetRightIndent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::GetTabs
void wxTextAttr_GetTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxArrayInt Result = This->GetTabs();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextAttr::GetTextColour
void wxTextAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxTextAttr::HasBackgroundColour
void wxTextAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextAttr::HasFont
void wxTextAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextAttr::HasTextColour
void wxTextAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextAttr::GetFlags
void wxTextAttr_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextAttr::IsDefault
void wxTextAttr_IsDefault(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsDefault();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextAttr::SetAlignment
void wxTextAttr_SetAlignment(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxTextAttrAlignment alignment;
  if(!enif_get_int(env, argv[1], (int *) &alignment)) Badarg("alignment"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetAlignment(alignment);

}

// wxTextAttr::SetBackgroundColour
void wxTextAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxTextAttr::SetFlags
void wxTextAttr_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  long flags;
  if(!enif_get_long(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

// wxTextAttr::SetFont
void wxTextAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxTEXT_ATTR_FONT &~wxTEXT_ATTR_FONT_PIXEL_SIZE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
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
  This->SetFont(*font,flags);

}

// wxTextAttr::SetFontEncoding
void wxTextAttr_SetFontEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxFontEncoding encoding;
  if(!enif_get_int(env, argv[1], (int *) &encoding)) Badarg("encoding"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetFontEncoding(encoding);

}

// wxTextAttr::SetFontFaceName
void wxTextAttr_SetFontFaceName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary faceName_bin;
  wxString faceName;
  if(!enif_inspect_binary(env, argv[1], &faceName_bin)) Badarg("faceName");
  faceName = wxString(faceName_bin.data, wxConvUTF8, faceName_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFontFaceName(faceName);

}

// wxTextAttr::SetFontFamily
void wxTextAttr_SetFontFamily(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxFontFamily family;
  if(!enif_get_int(env, argv[1], (int *) &family)) Badarg("family"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetFontFamily(family);

}

// wxTextAttr::SetFontSize
void wxTextAttr_SetFontSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  int pointSize;
  if(!enif_get_int(env, argv[1], &pointSize)) Badarg("pointSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFontSize(pointSize);

}

// wxTextAttr::SetFontPointSize
void wxTextAttr_SetFontPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  int pointSize;
  if(!enif_get_int(env, argv[1], &pointSize)) Badarg("pointSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFontPointSize(pointSize);

}

// wxTextAttr::SetFontPixelSize
void wxTextAttr_SetFontPixelSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  int pixelSize;
  if(!enif_get_int(env, argv[1], &pixelSize)) Badarg("pixelSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFontPixelSize(pixelSize);

}

// wxTextAttr::SetFontStyle
void wxTextAttr_SetFontStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxFontStyle fontStyle;
  if(!enif_get_int(env, argv[1], (int *) &fontStyle)) Badarg("fontStyle"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetFontStyle(fontStyle);

}

// wxTextAttr::SetFontUnderlined
void wxTextAttr_SetFontUnderlined(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  bool underlined;
  underlined = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetFontUnderlined(underlined);

}

// wxTextAttr::SetFontWeight
void wxTextAttr_SetFontWeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxFontWeight fontWeight;
  if(!enif_get_int(env, argv[1], (int *) &fontWeight)) Badarg("fontWeight"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetFontWeight(fontWeight);

}

// wxTextAttr::SetLeftIndent
void wxTextAttr_SetLeftIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int subIndent=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  int indent;
  if(!enif_get_int(env, argv[1], &indent)) Badarg("indent"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "subIndent"))) {
  if(!enif_get_int(env, tpl[1], &subIndent)) Badarg("subIndent"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetLeftIndent(indent,subIndent);

}

// wxTextAttr::SetRightIndent
void wxTextAttr_SetRightIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  int indent;
  if(!enif_get_int(env, argv[1], &indent)) Badarg("indent"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRightIndent(indent);

}

// wxTextAttr::SetTabs
void wxTextAttr_SetTabs(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
  wxArrayInt tabs;
  int tabs_tmp;
  ERL_NIF_TERM tabsHead, tabsTail;
  tabsTail = argv[1];
  while(!enif_is_empty_list(env, tabsTail)) {
    if(!enif_get_list_cell(env, tabsTail, &tabsHead, &tabsTail)) Badarg("tabs");
    if(!enif_get_int(env, tabsHead, &tabs_tmp)) Badarg("tabs");
    tabs.Add(tabs_tmp);
  };
  if(!This) throw wxe_badarg("This");
  This->SetTabs(tabs);

}

// wxTextAttr::SetTextColour
void wxTextAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxTextAttr::destroy
void wxTextAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxTextAttr *This;
  This = (wxTextAttr *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxTextCtrl::wxTextCtrl
void wxTextCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTextCtrl * Result = new EwxTextCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxTextCtrl::wxTextCtrl
void wxTextCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString value= wxEmptyString;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxTextCtrl * Result = new EwxTextCtrl(parent,id,value,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxTextCtrl::AppendText
void wxTextCtrl_AppendText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->AppendText(text);

}

// wxTextCtrl::CanCopy
void wxTextCtrl_CanCopy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanCopy();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::CanCut
void wxTextCtrl_CanCut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanCut();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::CanPaste
void wxTextCtrl_CanPaste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanPaste();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::CanRedo
void wxTextCtrl_CanRedo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanRedo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::CanUndo
void wxTextCtrl_CanUndo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->CanUndo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::Clear
void wxTextCtrl_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxTextCtrl::Copy
void wxTextCtrl_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Copy();

}

// wxTextCtrl::Create
void wxTextCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString value= wxEmptyString;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_long(env, tpl[1], &style)) Badarg("style");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,value,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::Cut
void wxTextCtrl_Cut(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Cut();

}

// wxTextCtrl::DiscardEdits
void wxTextCtrl_DiscardEdits(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DiscardEdits();

}

// wxTextCtrl::ChangeValue
void wxTextCtrl_ChangeValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->ChangeValue(value);

}

// wxTextCtrl::EmulateKeyPress
void wxTextCtrl_EmulateKeyPress(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  wxKeyEvent *event;
  event = (wxKeyEvent *) memenv->getPtr(env, argv[1], "event");
  if(!This) throw wxe_badarg("This");
  bool Result = This->EmulateKeyPress(*event);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::GetDefaultStyle
void wxTextCtrl_GetDefaultStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxTextAttr * Result = &This->GetDefaultStyle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextAttr"));

}

// wxTextCtrl::GetInsertionPoint
void wxTextCtrl_GetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetInsertionPoint();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextCtrl::GetLastPosition
void wxTextCtrl_GetLastPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTextPos Result = This->GetLastPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextCtrl::GetLineLength
void wxTextCtrl_GetLineLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long lineNo;
  if(!enif_get_long(env, argv[1], &lineNo)) Badarg("lineNo");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLineLength(lineNo);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextCtrl::GetLineText
void wxTextCtrl_GetLineText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long lineNo;
  if(!enif_get_long(env, argv[1], &lineNo)) Badarg("lineNo");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetLineText(lineNo);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextCtrl::GetNumberOfLines
void wxTextCtrl_GetNumberOfLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNumberOfLines();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextCtrl::GetRange
void wxTextCtrl_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetRange(from,to);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextCtrl::GetSelection
void wxTextCtrl_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long from;
  long to;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetSelection(&from,&to);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(from),
  rt.make_int(to));
  rt.send(msg);

}

// wxTextCtrl::GetStringSelection
void wxTextCtrl_GetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetStringSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextCtrl::GetStyle
void wxTextCtrl_GetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long position;
  if(!enif_get_long(env, argv[1], &position)) Badarg("position");
  wxTextAttr *style;
  style = (wxTextAttr *) memenv->getPtr(env, argv[2], "style");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetStyle(position,*style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::GetValue
void wxTextCtrl_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextCtrl::IsEditable
void wxTextCtrl_IsEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEditable();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::IsModified
void wxTextCtrl_IsModified(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsModified();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::IsMultiLine
void wxTextCtrl_IsMultiLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsMultiLine();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::IsSingleLine
void wxTextCtrl_IsSingleLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSingleLine();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::LoadFile
void wxTextCtrl_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int fileType=wxTEXT_TYPE_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fileType"))) {
  if(!enif_get_int(env, tpl[1], &fileType)) Badarg("fileType"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(filename,fileType);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::MarkDirty
void wxTextCtrl_MarkDirty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->MarkDirty();

}

// wxTextCtrl::Paste
void wxTextCtrl_Paste(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Paste();

}

// wxTextCtrl::PositionToXY
void wxTextCtrl_PositionToXY(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long x;
  long y;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long pos;
  if(!enif_get_long(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  bool Result = This->PositionToXY(pos,&x,&y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_bool(Result),
    rt.make_int(x),
  rt.make_int(y));
  rt.send(msg);

}

// wxTextCtrl::Redo
void wxTextCtrl_Redo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Redo();

}

// wxTextCtrl::Remove
void wxTextCtrl_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  if(!This) throw wxe_badarg("This");
  This->Remove(from,to);

}

// wxTextCtrl::Replace
void wxTextCtrl_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
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

// wxTextCtrl::SaveFile
void wxTextCtrl_SaveFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString file= wxEmptyString;
  int fileType=wxTEXT_TYPE_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "file"))) {
  ErlNifBinary file_bin;
  if(!enif_inspect_binary(env, tpl[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "fileType"))) {
  if(!enif_get_int(env, tpl[1], &fileType)) Badarg("fileType"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(file,fileType);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::SetDefaultStyle
void wxTextCtrl_SetDefaultStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  wxTextAttr *style;
  style = (wxTextAttr *) memenv->getPtr(env, argv[1], "style");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetDefaultStyle(*style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::SetEditable
void wxTextCtrl_SetEditable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  bool editable;
  editable = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetEditable(editable);

}

// wxTextCtrl::SetInsertionPoint
void wxTextCtrl_SetInsertionPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long pos;
  if(!enif_get_long(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->SetInsertionPoint(pos);

}

// wxTextCtrl::SetInsertionPointEnd
void wxTextCtrl_SetInsertionPointEnd(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SetInsertionPointEnd();

}

// wxTextCtrl::SetMaxLength
void wxTextCtrl_SetMaxLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long len;
  if(!enif_get_long(env, argv[1], &len)) Badarg("len");
  if(!This) throw wxe_badarg("This");
  This->SetMaxLength(len);

}

// wxTextCtrl::SetSelection
void wxTextCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  if(!This) throw wxe_badarg("This");
  This->SetSelection(from,to);

}

// wxTextCtrl::SetStyle
void wxTextCtrl_SetStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long start;
  if(!enif_get_long(env, argv[1], &start)) Badarg("start");
  long end;
  if(!enif_get_long(env, argv[2], &end)) Badarg("end");
  wxTextAttr *style;
  style = (wxTextAttr *) memenv->getPtr(env, argv[3], "style");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetStyle(start,end,*style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTextCtrl::SetValue
void wxTextCtrl_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetValue(value);

}

// wxTextCtrl::ShowPosition
void wxTextCtrl_ShowPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long pos;
  if(!enif_get_long(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->ShowPosition(pos);

}

// wxTextCtrl::Undo
void wxTextCtrl_Undo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Undo();

}

// wxTextCtrl::WriteText
void wxTextCtrl_WriteText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->WriteText(text);

}

// wxTextCtrl::XYToPosition
void wxTextCtrl_XYToPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextCtrl *This;
  This = (wxTextCtrl *) memenv->getPtr(env, argv[0], "This");
  long x;
  if(!enif_get_long(env, argv[1], &x)) Badarg("x");
  long y;
  if(!enif_get_long(env, argv[2], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  long Result = This->XYToPosition(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextDataObject::wxTextDataObject
void wxTextDataObject_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString text= wxEmptyString;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "text"))) {
  ErlNifBinary text_bin;
  if(!enif_inspect_binary(env, tpl[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
    } else        Badarg("Options");
  };
  wxTextDataObject * Result = new wxTextDataObject(text);
  app->newPtr((void *) Result, 217, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextDataObject"));

}

// wxTextDataObject::GetTextLength
void wxTextDataObject_GetTextLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextDataObject *This;
  This = (wxTextDataObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetTextLength();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTextDataObject::GetText
void wxTextDataObject_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextDataObject *This;
  This = (wxTextDataObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextDataObject::SetText
void wxTextDataObject_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextDataObject *This;
  This = (wxTextDataObject *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary strText_bin;
  wxString strText;
  if(!enif_inspect_binary(env, argv[1], &strText_bin)) Badarg("strText");
  strText = wxString(strText_bin.data, wxConvUTF8, strText_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetText(strText);

}

// wxTextDataObject::destroy
void wxTextDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxTextDataObject *This;
  This = (wxTextDataObject *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxTextEntryDialog::wxTextEntryDialog
void wxTextEntryDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTextEntryDialog * Result = new EwxTextEntryDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextEntryDialog"));

}

// wxTextEntryDialog::wxTextEntryDialog
void wxTextEntryDialog_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString caption= wxGetTextFromUserPromptStr;
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
  wxTextEntryDialog * Result = new EwxTextEntryDialog(parent,message,caption,value,style,pos);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextEntryDialog"));

}

// wxTextEntryDialog::GetValue
void wxTextEntryDialog_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextEntryDialog *This;
  This = (wxTextEntryDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTextEntryDialog::SetValue
void wxTextEntryDialog_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTextEntryDialog *This;
  This = (wxTextEntryDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[1], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetValue(value);

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

// wxToolBar::AddControl
void wxToolBar_AddControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString label= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  wxControl *control;
  control = (wxControl *) memenv->getPtr(env, argv[1], "control");
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
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddControl(control,label);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddSeparator
void wxToolBar_AddSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddSeparator();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddTool
void wxToolBar_AddTool_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  wxToolBarToolBase *tool;
  tool = (wxToolBarToolBase *) memenv->getPtr(env, argv[1], "tool");
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddTool(tool);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddTool
void wxToolBar_AddTool_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString shortHelp= wxEmptyString;
 wxItemKind kind=wxITEM_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "shortHelp"))) {
  ErlNifBinary shortHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortHelp_bin)) Badarg("shortHelp");
  shortHelp = wxString(shortHelp_bin.data, wxConvUTF8, shortHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddTool(toolId,label,*bitmap,shortHelp,kind);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddTool
void wxToolBar_AddTool_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxItemKind kind=wxITEM_NORMAL;
  wxString shortHelp= wxEmptyString;
  wxString longHelp= wxEmptyString;
  wxObject * data=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[3], "bitmap");
  wxBitmap *bmpDisabled;
  bmpDisabled = (wxBitmap *) memenv->getPtr(env, argv[4], "bmpDisabled");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "shortHelp"))) {
  ErlNifBinary shortHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortHelp_bin)) Badarg("shortHelp");
  shortHelp = wxString(shortHelp_bin.data, wxConvUTF8, shortHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "longHelp"))) {
  ErlNifBinary longHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &longHelp_bin)) Badarg("longHelp");
  longHelp = wxString(longHelp_bin.data, wxConvUTF8, longHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxObject *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddTool(toolId,label,*bitmap,*bmpDisabled,kind,shortHelp,longHelp,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddCheckTool
void wxToolBar_AddCheckTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxBitmap * bmpDisabled= &wxNullBitmap;
  wxString shortHelp= wxEmptyString;
  wxString longHelp= wxEmptyString;
  wxObject * data=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  wxBitmap *bitmap1;
  bitmap1 = (wxBitmap *) memenv->getPtr(env, argv[3], "bitmap1");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bmpDisabled"))) {
  bmpDisabled = (wxBitmap *) memenv->getPtr(env, tpl[1], "bmpDisabled");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "shortHelp"))) {
  ErlNifBinary shortHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortHelp_bin)) Badarg("shortHelp");
  shortHelp = wxString(shortHelp_bin.data, wxConvUTF8, shortHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "longHelp"))) {
  ErlNifBinary longHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &longHelp_bin)) Badarg("longHelp");
  longHelp = wxString(longHelp_bin.data, wxConvUTF8, longHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxObject *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddCheckTool(toolId,label,*bitmap1,*bmpDisabled,shortHelp,longHelp,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddRadioTool
void wxToolBar_AddRadioTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxBitmap * bmpDisabled= &wxNullBitmap;
  wxString shortHelp= wxEmptyString;
  wxString longHelp= wxEmptyString;
  wxObject * data=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  wxBitmap *bitmap1;
  bitmap1 = (wxBitmap *) memenv->getPtr(env, argv[3], "bitmap1");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bmpDisabled"))) {
  bmpDisabled = (wxBitmap *) memenv->getPtr(env, tpl[1], "bmpDisabled");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "shortHelp"))) {
  ErlNifBinary shortHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortHelp_bin)) Badarg("shortHelp");
  shortHelp = wxString(shortHelp_bin.data, wxConvUTF8, shortHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "longHelp"))) {
  ErlNifBinary longHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &longHelp_bin)) Badarg("longHelp");
  longHelp = wxString(longHelp_bin.data, wxConvUTF8, longHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxObject *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddRadioTool(toolId,label,*bitmap1,*bmpDisabled,shortHelp,longHelp,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::AddStretchableSpace
void wxToolBar_AddStretchableSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->AddStretchableSpace();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::InsertStretchableSpace
void wxToolBar_InsertStretchableSpace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->InsertStretchableSpace(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::DeleteTool
void wxToolBar_DeleteTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteTool(toolId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolBar::DeleteToolByPos
void wxToolBar_DeleteToolByPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteToolByPos(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolBar::EnableTool
void wxToolBar_EnableTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  bool enable;
  enable = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableTool(toolId,enable);

}

// wxToolBar::FindById
void wxToolBar_FindById(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->FindById(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::FindControl
void wxToolBar_FindControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxControl * Result = (wxControl*)This->FindControl(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxControl"));

}

// wxToolBar::FindToolForPosition
void wxToolBar_FindToolForPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // wxCoord
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // wxCoord
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->FindToolForPosition(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::GetToolSize
void wxToolBar_GetToolSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetToolSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolBar::GetToolBitmapSize
void wxToolBar_GetToolBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetToolBitmapSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolBar::GetMargins
void wxToolBar_GetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetMargins();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolBar::GetToolEnabled
void wxToolBar_GetToolEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetToolEnabled(toolId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolBar::GetToolLongHelp
void wxToolBar_GetToolLongHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetToolLongHelp(toolId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolBar::GetToolPacking
void wxToolBar_GetToolPacking(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetToolPacking();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolBar::GetToolPos
void wxToolBar_GetToolPos(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetToolPos(toolId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolBar::GetToolSeparation
void wxToolBar_GetToolSeparation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetToolSeparation();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolBar::GetToolShortHelp
void wxToolBar_GetToolShortHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetToolShortHelp(toolId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolBar::GetToolState
void wxToolBar_GetToolState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetToolState(toolId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolBar::InsertControl
void wxToolBar_InsertControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString label= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  wxControl *control;
  control = (wxControl *) memenv->getPtr(env, argv[2], "control");
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->InsertControl(pos,control,label);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::InsertSeparator
void wxToolBar_InsertSeparator(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->InsertSeparator(pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::InsertTool
void wxToolBar_InsertTool_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxBitmap * bmpDisabled= &wxNullBitmap;
 wxItemKind kind=wxITEM_NORMAL;
  wxString shortHelp= wxEmptyString;
  wxString longHelp= wxEmptyString;
  wxObject * clientData=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  int toolId;
  if(!enif_get_int(env, argv[2], &toolId)) Badarg("toolId"); // int
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[3], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[4], "bitmap");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bmpDisabled"))) {
  bmpDisabled = (wxBitmap *) memenv->getPtr(env, tpl[1], "bmpDisabled");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "kind"))) {
  if(!enif_get_int(env, tpl[1], (int *) &kind)) Badarg("kind"); // enum
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "shortHelp"))) {
  ErlNifBinary shortHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortHelp_bin)) Badarg("shortHelp");
  shortHelp = wxString(shortHelp_bin.data, wxConvUTF8, shortHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "longHelp"))) {
  ErlNifBinary longHelp_bin;
  if(!enif_inspect_binary(env, tpl[1], &longHelp_bin)) Badarg("longHelp");
  longHelp = wxString(longHelp_bin.data, wxConvUTF8, longHelp_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "clientData"))) {
  clientData = (wxObject *) memenv->getPtr(env, tpl[1], "clientData");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->InsertTool(pos,toolId,label,*bitmap,*bmpDisabled,kind,shortHelp,longHelp,clientData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::InsertTool
void wxToolBar_InsertTool_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  size_t pos;
  if(!wxe_get_size_t(env, argv[1], &pos)) Badarg("pos");
  wxToolBarToolBase *tool;
  tool = (wxToolBarToolBase *) memenv->getPtr(env, argv[2], "tool");
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->InsertTool(pos,tool);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::Realize
void wxToolBar_Realize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Realize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolBar::RemoveTool
void wxToolBar_RemoveTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  wxToolBarToolBase * Result = (wxToolBarToolBase*)This->RemoveTool(id);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wx"));

}

// wxToolBar::SetMargins
void wxToolBar_SetMargins(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMargins(x,y);

}

// wxToolBar::SetToolBitmapSize
void wxToolBar_SetToolBitmapSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  if(!This) throw wxe_badarg("This");
  This->SetToolBitmapSize(size);

}

// wxToolBar::SetToolLongHelp
void wxToolBar_SetToolLongHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  ErlNifBinary helpString_bin;
  wxString helpString;
  if(!enif_inspect_binary(env, argv[2], &helpString_bin)) Badarg("helpString");
  helpString = wxString(helpString_bin.data, wxConvUTF8, helpString_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetToolLongHelp(toolId,helpString);

}

// wxToolBar::SetToolPacking
void wxToolBar_SetToolPacking(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int packing;
  if(!enif_get_int(env, argv[1], &packing)) Badarg("packing"); // int
  if(!This) throw wxe_badarg("This");
  This->SetToolPacking(packing);

}

// wxToolBar::SetToolShortHelp
void wxToolBar_SetToolShortHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  ErlNifBinary helpString_bin;
  wxString helpString;
  if(!enif_inspect_binary(env, argv[2], &helpString_bin)) Badarg("helpString");
  helpString = wxString(helpString_bin.data, wxConvUTF8, helpString_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetToolShortHelp(toolId,helpString);

}

// wxToolBar::SetToolSeparation
void wxToolBar_SetToolSeparation(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int separation;
  if(!enif_get_int(env, argv[1], &separation)) Badarg("separation"); // int
  if(!This) throw wxe_badarg("This");
  This->SetToolSeparation(separation);

}

// wxToolBar::ToggleTool
void wxToolBar_ToggleTool(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolBar *This;
  This = (wxToolBar *) memenv->getPtr(env, argv[0], "This");
  int toolId;
  if(!enif_get_int(env, argv[1], &toolId)) Badarg("toolId"); // int
  bool toggle;
  toggle = enif_is_identical(argv[2], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->ToggleTool(toolId,toggle);

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

// wxToolbook::wxToolbook
void wxToolbook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxToolbook * Result = new EwxToolbook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolbook"));

}

// wxToolbook::wxToolbook
void wxToolbook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxToolbook * Result = new EwxToolbook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolbook"));

}

// wxToolbook::AddPage
void wxToolbook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::AdvanceSelection
void wxToolbook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool forward=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::AssignImageList
void wxToolbook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxToolbook::Create
void wxToolbook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::DeleteAllPages
void wxToolbook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolbook::GetCurrentPage
void wxToolbook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxToolbook::GetImageList
void wxToolbook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxToolbook::GetPage
void wxToolbook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxToolbook::GetPageCount
void wxToolbook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolbook::GetPageImage
void wxToolbook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageImage(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolbook::GetPageText
void wxToolbook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxToolbook::GetSelection
void wxToolbook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolbook::HitTest
void wxToolbook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::InsertPage
void wxToolbook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::SetImageList
void wxToolbook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxToolbook::SetPageSize
void wxToolbook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::SetPageImage
void wxToolbook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageImage(page,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxToolbook::SetPageText
void wxToolbook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
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

// wxToolbook::SetSelection
void wxToolbook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxToolbook::ChangeSelection
void wxToolbook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxToolbook *This;
  This = (wxToolbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->ChangeSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

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

// wxTreeCtrl::wxTreeCtrl
void wxTreeCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTreeCtrl * Result = new EwxTreeCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTreeCtrl"));

}

// wxTreeCtrl::wxTreeCtrl
void wxTreeCtrl_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxTR_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxTreeCtrl * Result = new EwxTreeCtrl(parent,id,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTreeCtrl"));

}

// wxTreeCtrl::AddRoot
void wxTreeCtrl_AddRoot(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int image=-1;
  int selectedImage=-1;
  wxETreeItemData * data= NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "image"))) {
  if(!enif_get_int(env, tpl[1], &image)) Badarg("image"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "selectedImage"))) {
  if(!enif_get_int(env, tpl[1], &selectedImage)) Badarg("selectedImage"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = new wxETreeItemData(tpl[1]);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->AddRoot(text,image,selectedImage,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::AppendItem
void wxTreeCtrl_AppendItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int image=-1;
  int selectedImage=-1;
  wxETreeItemData * data= NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 parent_tmp;
  if(!enif_get_uint64(env, argv[1], &parent_tmp)) Badarg("parent");
  wxTreeItemId parent = wxTreeItemId((void *) (wxUint64) parent_tmp);
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "image"))) {
  if(!enif_get_int(env, tpl[1], &image)) Badarg("image"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "selectedImage"))) {
  if(!enif_get_int(env, tpl[1], &selectedImage)) Badarg("selectedImage"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = new wxETreeItemData(tpl[1]);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->AppendItem(parent,text,image,selectedImage,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::AssignImageList
void wxTreeCtrl_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxTreeCtrl::AssignStateImageList
void wxTreeCtrl_AssignStateImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignStateImageList(imageList);

}

// wxTreeCtrl::Collapse
void wxTreeCtrl_Collapse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->Collapse(item);

}

// wxTreeCtrl::CollapseAndReset
void wxTreeCtrl_CollapseAndReset(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->CollapseAndReset(item);

}

// wxTreeCtrl::Create
void wxTreeCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxTR_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,id,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreeCtrl::Delete
void wxTreeCtrl_Delete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->Delete(item);

}

// wxTreeCtrl::DeleteAllItems
void wxTreeCtrl_DeleteAllItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->DeleteAllItems();

}

// wxTreeCtrl::DeleteChildren
void wxTreeCtrl_DeleteChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->DeleteChildren(item);

}

// wxTreeCtrl::EditLabel
void wxTreeCtrl_EditLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->EditLabel(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxTreeCtrl::EnsureVisible
void wxTreeCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->EnsureVisible(item);

}

// wxTreeCtrl::Expand
void wxTreeCtrl_Expand(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->Expand(item);

}

// wxTreeCtrl::GetBoundingRect
void wxTreeCtrl_GetBoundingRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRect rect;
  bool textOnly=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "textOnly"))) {
  textOnly = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetBoundingRect(item,rect,textOnly);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(rect));
  rt.send(msg);

}

// wxTreeCtrl::GetChildrenCount
void wxTreeCtrl_GetChildrenCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool recursively=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "recursively"))) {
  recursively = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetChildrenCount(item,recursively);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreeCtrl::GetCount
void wxTreeCtrl_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxTreeCtrl::GetEditControl
void wxTreeCtrl_GetEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->GetEditControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxTreeCtrl::GetFirstChild
void wxTreeCtrl_GetFirstChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTreeItemIdValue cookie;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetFirstChild(item,cookie);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make((wxUIntPtr *) Result.m_pItem),
    rt.make((wxUIntPtr *) cookie));
  rt.send(msg);

}

// wxTreeCtrl::GetNextChild
void wxTreeCtrl_GetNextChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ErlNifUInt64 cookie_tmp;
  if(!enif_get_uint64(env, argv[2], &cookie_tmp)) Badarg("cookie");
  wxTreeItemIdValue cookie = (wxTreeItemIdValue) cookie_tmp;
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetNextChild(item,cookie);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make((wxUIntPtr *) Result.m_pItem),
    rt.make((wxUIntPtr *) cookie));
  rt.send(msg);

}

// wxTreeCtrl::GetFirstVisibleItem
void wxTreeCtrl_GetFirstVisibleItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetFirstVisibleItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetImageList
void wxTreeCtrl_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxTreeCtrl::GetIndent
void wxTreeCtrl_GetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetIndent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxTreeCtrl::GetItemBackgroundColour
void wxTreeCtrl_GetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetItemBackgroundColour(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTreeCtrl::GetItemData
void wxTreeCtrl_GetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxETreeItemData * Result = (wxETreeItemData*)This->GetItemData(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ext2term(Result));

}

// wxTreeCtrl::GetItemFont
void wxTreeCtrl_GetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetItemFont(item)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxTreeCtrl::GetItemImage
void wxTreeCtrl_GetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxTreeItemIcon which=wxTreeItemIcon_Normal;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "which"))) {
  if(!enif_get_int(env, tpl[1], (int *) &which)) Badarg("which"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->GetItemImage(item,which);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreeCtrl::GetItemText
void wxTreeCtrl_GetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetItemText(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTreeCtrl::GetItemTextColour
void wxTreeCtrl_GetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetItemTextColour(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTreeCtrl::GetLastChild
void wxTreeCtrl_GetLastChild(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetLastChild(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetNextSibling
void wxTreeCtrl_GetNextSibling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetNextSibling(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetNextVisible
void wxTreeCtrl_GetNextVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetNextVisible(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetItemParent
void wxTreeCtrl_GetItemParent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetItemParent(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetPrevSibling
void wxTreeCtrl_GetPrevSibling(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetPrevSibling(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetPrevVisible
void wxTreeCtrl_GetPrevVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetPrevVisible(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetRootItem
void wxTreeCtrl_GetRootItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetRootItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetSelection
void wxTreeCtrl_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::GetSelections
void wxTreeCtrl_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayTreeItemIds selection;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetSelections(selection);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(Result),
    rt.make_array_objs(selection));
  rt.send(msg);

}

// wxTreeCtrl::GetStateImageList
void wxTreeCtrl_GetStateImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetStateImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxTreeCtrl::HitTest
void wxTreeCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[1], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->HitTest(point,flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make((wxUIntPtr *) Result.m_pItem),
    rt.make_int(flags));
  rt.send(msg);

}

// wxTreeCtrl::InsertItem
void wxTreeCtrl_InsertItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int image=-1;
  int selImage=-1;
  wxETreeItemData * data= NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 parent_tmp;
  if(!enif_get_uint64(env, argv[1], &parent_tmp)) Badarg("parent");
  wxTreeItemId parent = wxTreeItemId((void *) (wxUint64) parent_tmp);
  ErlNifUInt64 previous_tmp;
  if(!enif_get_uint64(env, argv[2], &previous_tmp)) Badarg("previous");
  wxTreeItemId previous = wxTreeItemId((void *) (wxUint64) previous_tmp);
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "image"))) {
  if(!enif_get_int(env, tpl[1], &image)) Badarg("image"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "selImage"))) {
  if(!enif_get_int(env, tpl[1], &selImage)) Badarg("selImage"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = new wxETreeItemData(tpl[1]);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->InsertItem(parent,previous,text,image,selImage,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::IsBold
void wxTreeCtrl_IsBold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsBold(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreeCtrl::IsExpanded
void wxTreeCtrl_IsExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsExpanded(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreeCtrl::IsSelected
void wxTreeCtrl_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelected(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreeCtrl::IsVisible
void wxTreeCtrl_IsVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsVisible(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreeCtrl::ItemHasChildren
void wxTreeCtrl_ItemHasChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  bool Result = This->ItemHasChildren(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}


// wxTreeCtrl::IsTreeItemIdOk
void wxTreeCtrl_IsTreeItemIdOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifUInt64 tmp;
  if(!enif_get_uint64(Ecmd.env, Ecmd.args[0], &tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) tmp);
  bool Result = item.IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller,true);
  rt.send(  rt.make_bool(Result));
}

// wxTreeCtrl::PrependItem
void wxTreeCtrl_PrependItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int image=-1;
  int selectedImage=-1;
  wxETreeItemData * data= NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 parent_tmp;
  if(!enif_get_uint64(env, argv[1], &parent_tmp)) Badarg("parent");
  wxTreeItemId parent = wxTreeItemId((void *) (wxUint64) parent_tmp);
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "image"))) {
  if(!enif_get_int(env, tpl[1], &image)) Badarg("image"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "selectedImage"))) {
  if(!enif_get_int(env, tpl[1], &selectedImage)) Badarg("selectedImage"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = new wxETreeItemData(tpl[1]);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->PrependItem(parent,text,image,selectedImage,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeCtrl::ScrollTo
void wxTreeCtrl_ScrollTo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->ScrollTo(item);

}

// wxTreeCtrl::SelectItem
void wxTreeCtrl_SelectItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool select=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "select"))) {
  select = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SelectItem(item,select);

}

// wxTreeCtrl::SetIndent
void wxTreeCtrl_SetIndent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  unsigned int indent;
  if(!enif_get_uint(env, argv[1], &indent)) Badarg("indent");
  if(!This) throw wxe_badarg("This");
  This->SetIndent(indent);

}

// wxTreeCtrl::SetImageList
void wxTreeCtrl_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxTreeCtrl::SetItemBackgroundColour
void wxTreeCtrl_SetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
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
  if(!This) throw wxe_badarg("This");
  This->SetItemBackgroundColour(item,col);

}

// wxTreeCtrl::SetItemBold
void wxTreeCtrl_SetItemBold(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bold=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bold"))) {
  bold = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetItemBold(item,bold);

}

// wxTreeCtrl::SetItemData
void wxTreeCtrl_SetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  wxETreeItemData * data;
  data = new wxETreeItemData(argv[2]);
  if(!This) throw wxe_badarg("This");
  This->SetItemData(item,data);

}

// wxTreeCtrl::SetItemDropHighlight
void wxTreeCtrl_SetItemDropHighlight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool highlight=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "highlight"))) {
  highlight = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetItemDropHighlight(item,highlight);

}

// wxTreeCtrl::SetItemFont
void wxTreeCtrl_SetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  if(!This) throw wxe_badarg("This");
  This->SetItemFont(item,*font);

}

// wxTreeCtrl::SetItemHasChildren
void wxTreeCtrl_SetItemHasChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool has=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "has"))) {
  has = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetItemHasChildren(item,has);

}

// wxTreeCtrl::SetItemImage
void wxTreeCtrl_SetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxTreeItemIcon which=wxTreeItemIcon_Normal;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "which"))) {
  if(!enif_get_int(env, tpl[1], (int *) &which)) Badarg("which"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetItemImage(item,image,which);

}

// wxTreeCtrl::SetItemText
void wxTreeCtrl_SetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetItemText(item,text);

}

// wxTreeCtrl::SetItemTextColour
void wxTreeCtrl_SetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
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
  if(!This) throw wxe_badarg("This");
  This->SetItemTextColour(item,col);

}

// wxTreeCtrl::SetStateImageList
void wxTreeCtrl_SetStateImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetStateImageList(imageList);

}

// wxTreeCtrl::SetWindowStyle
void wxTreeCtrl_SetWindowStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  long styles;
  if(!enif_get_long(env, argv[1], &styles)) Badarg("styles");
  if(!This) throw wxe_badarg("This");
  This->SetWindowStyle(styles);

}

// wxTreeCtrl::SortChildren
void wxTreeCtrl_SortChildren(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->SortChildren(item);

}

// wxTreeCtrl::Toggle
void wxTreeCtrl_Toggle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->Toggle(item);

}

// wxTreeCtrl::ToggleItemSelection
void wxTreeCtrl_ToggleItemSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->ToggleItemSelection(item);

}

// wxTreeCtrl::Unselect
void wxTreeCtrl_Unselect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Unselect();

}

// wxTreeCtrl::UnselectAll
void wxTreeCtrl_UnselectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->UnselectAll();

}

// wxTreeCtrl::UnselectItem
void wxTreeCtrl_UnselectItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeCtrl *This;
  This = (wxTreeCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifUInt64 item_tmp;
  if(!enif_get_uint64(env, argv[1], &item_tmp)) Badarg("item");
  wxTreeItemId item = wxTreeItemId((void *) (wxUint64) item_tmp);
  if(!This) throw wxe_badarg("This");
  This->UnselectItem(item);

}

// wxTreeEvent::GetKeyCode
void wxTreeEvent_GetKeyCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetKeyCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreeEvent::GetItem
void wxTreeEvent_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeEvent::GetKeyEvent
void wxTreeEvent_GetKeyEvent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxKeyEvent * Result = &This->GetKeyEvent();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxKeyEvent"));

}

// wxTreeEvent::GetLabel
void wxTreeEvent_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetLabel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTreeEvent::GetOldItem
void wxTreeEvent_GetOldItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTreeItemId Result = This->GetOldItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((wxUIntPtr *) Result.m_pItem));

}

// wxTreeEvent::GetPoint
void wxTreeEvent_GetPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPoint();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTreeEvent::IsEditCancelled
void wxTreeEvent_IsEditCancelled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEditCancelled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreeEvent::SetToolTip
void wxTreeEvent_SetToolTip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreeEvent *This;
  This = (wxTreeEvent *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary tooltip_bin;
  wxString tooltip;
  if(!enif_inspect_binary(env, argv[1], &tooltip_bin)) Badarg("tooltip");
  tooltip = wxString(tooltip_bin.data, wxConvUTF8, tooltip_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetToolTip(tooltip);

}

// wxTreebook::wxTreebook
void wxTreebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxTreebook * Result = new EwxTreebook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTreebook"));

}

// wxTreebook::wxTreebook
void wxTreebook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxBK_DEFAULT;
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
  wxTreebook * Result = new EwxTreebook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTreebook"));

}

// wxTreebook::AddPage
void wxTreebook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxNOT_FOUND;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
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

// wxTreebook::AdvanceSelection
void wxTreebook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool forward=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
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

// wxTreebook::AssignImageList
void wxTreebook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxTreebook::Create
void wxTreebook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxBK_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
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

// wxTreebook::DeleteAllPages
void wxTreebook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreebook::GetCurrentPage
void wxTreebook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxTreebook::GetImageList
void wxTreebook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxTreebook::GetPage
void wxTreebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxTreebook::GetPageCount
void wxTreebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreebook::GetPageImage
void wxTreebook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageImage(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreebook::GetPageText
void wxTreebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxTreebook::GetSelection
void wxTreebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreebook::ExpandNode
void wxTreebook_ExpandNode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool expand=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t pageId;
  if(!wxe_get_size_t(env, argv[1], &pageId)) Badarg("pageId");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "expand"))) {
  expand = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ExpandNode(pageId,expand);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreebook::IsNodeExpanded
void wxTreebook_IsNodeExpanded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t pageId;
  if(!wxe_get_size_t(env, argv[1], &pageId)) Badarg("pageId");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsNodeExpanded(pageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreebook::HitTest
void wxTreebook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
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

// wxTreebook::InsertPage
void wxTreebook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxNOT_FOUND;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t pagePos;
  if(!wxe_get_size_t(env, argv[1], &pagePos)) Badarg("pagePos");
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
  bool Result = This->InsertPage(pagePos,page,text,bSelect,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreebook::InsertSubPage
void wxTreebook_InsertSubPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxNOT_FOUND;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t pagePos;
  if(!wxe_get_size_t(env, argv[1], &pagePos)) Badarg("pagePos");
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
  bool Result = This->InsertSubPage(pagePos,page,text,bSelect,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreebook::SetImageList
void wxTreebook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxTreebook::SetPageSize
void wxTreebook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
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

// wxTreebook::SetPageImage
void wxTreebook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageImage(page,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxTreebook::SetPageText
void wxTreebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
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

// wxTreebook::SetSelection
void wxTreebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxTreebook::ChangeSelection
void wxTreebook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxTreebook *This;
  This = (wxTreebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->ChangeSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxUpdateUIEvent::CanUpdate
void wxUpdateUIEvent_CanUpdate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  bool Result = wxUpdateUIEvent::CanUpdate(window);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::Check
void wxUpdateUIEvent_Check(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  bool check;
  check = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Check(check);

}

// wxUpdateUIEvent::Enable
void wxUpdateUIEvent_Enable(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  bool enable;
  enable = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Enable(enable);

}

// wxUpdateUIEvent::Show
void wxUpdateUIEvent_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  bool show;
  show = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->Show(show);

}

// wxUpdateUIEvent::GetChecked
void wxUpdateUIEvent_GetChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetChecked();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetEnabled
void wxUpdateUIEvent_GetEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetShown
void wxUpdateUIEvent_GetShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetShown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetSetChecked
void wxUpdateUIEvent_GetSetChecked(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSetChecked();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetSetEnabled
void wxUpdateUIEvent_GetSetEnabled(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSetEnabled();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetSetShown
void wxUpdateUIEvent_GetSetShown(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSetShown();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetSetText
void wxUpdateUIEvent_GetSetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxUpdateUIEvent::GetText
void wxUpdateUIEvent_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxUpdateUIEvent::GetMode
void wxUpdateUIEvent_GetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxUpdateUIEvent::GetMode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxUpdateUIEvent::GetUpdateInterval
void wxUpdateUIEvent_GetUpdateInterval(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long Result = wxUpdateUIEvent::GetUpdateInterval();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxUpdateUIEvent::ResetUpdateTime
void wxUpdateUIEvent_ResetUpdateTime(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxUpdateUIEvent::ResetUpdateTime();

}

// wxUpdateUIEvent::SetMode
void wxUpdateUIEvent_SetMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIMode mode;
  if(!enif_get_int(env, argv[0], (int *) &mode)) Badarg("mode"); // enum
  wxUpdateUIEvent::SetMode(mode);

}

// wxUpdateUIEvent::SetText
void wxUpdateUIEvent_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxUpdateUIEvent *This;
  This = (wxUpdateUIEvent *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetText(text);

}

// wxUpdateUIEvent::SetUpdateInterval
void wxUpdateUIEvent_SetUpdateInterval(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  long updateInterval;
  if(!enif_get_long(env, argv[0], &updateInterval)) Badarg("updateInterval");
  wxUpdateUIEvent::SetUpdateInterval(updateInterval);

}

