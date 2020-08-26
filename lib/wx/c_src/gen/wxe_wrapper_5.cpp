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

// wxFilePickerCtrl::wxFilePickerCtrl
void wxFilePickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFilePickerCtrl * Result = new EwxFilePickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFilePickerCtrl"));

}

// wxFilePickerCtrl::wxFilePickerCtrl
void wxFilePickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString path= wxEmptyString;
  wxString message= wxFileSelectorPromptStr;
  wxString wildcard= wxFileSelectorDefaultWildcardStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFLP_DEFAULT_STYLE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "path"))) {
  ErlNifBinary path_bin;
  if(!enif_inspect_binary(env, tpl[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "wildcard"))) {
  ErlNifBinary wildcard_bin;
  if(!enif_inspect_binary(env, tpl[1], &wildcard_bin)) Badarg("wildcard");
  wildcard = wxString(wildcard_bin.data, wxConvUTF8, wildcard_bin.size);
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
  wxFilePickerCtrl * Result = new EwxFilePickerCtrl(parent,id,path,message,wildcard,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFilePickerCtrl"));

}

// wxFilePickerCtrl::Create
void wxFilePickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString path= wxEmptyString;
  wxString message= wxFileSelectorPromptStr;
  wxString wildcard= wxFileSelectorDefaultWildcardStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFLP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFilePickerCtrl *This;
  This = (wxFilePickerCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "path"))) {
  ErlNifBinary path_bin;
  if(!enif_inspect_binary(env, tpl[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "wildcard"))) {
  ErlNifBinary wildcard_bin;
  if(!enif_inspect_binary(env, tpl[1], &wildcard_bin)) Badarg("wildcard");
  wildcard = wxString(wildcard_bin.data, wxConvUTF8, wildcard_bin.size);
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
  bool Result = This->Create(parent,id,path,message,wildcard,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFilePickerCtrl::GetPath
void wxFilePickerCtrl_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFilePickerCtrl *This;
  This = (wxFilePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFilePickerCtrl::SetPath
void wxFilePickerCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFilePickerCtrl *This;
  This = (wxFilePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(filename);

}

// wxDirPickerCtrl::wxDirPickerCtrl
void wxDirPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDirPickerCtrl * Result = new EwxDirPickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDirPickerCtrl"));

}

// wxDirPickerCtrl::wxDirPickerCtrl
void wxDirPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString path= wxEmptyString;
  wxString message= wxDirSelectorPromptStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDIRP_DEFAULT_STYLE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "path"))) {
  ErlNifBinary path_bin;
  if(!enif_inspect_binary(env, tpl[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
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
  wxDirPickerCtrl * Result = new EwxDirPickerCtrl(parent,id,path,message,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDirPickerCtrl"));

}

// wxDirPickerCtrl::Create
void wxDirPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString path= wxEmptyString;
  wxString message= wxDirSelectorPromptStr;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDIRP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirPickerCtrl *This;
  This = (wxDirPickerCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "path"))) {
  ErlNifBinary path_bin;
  if(!enif_inspect_binary(env, tpl[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
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
  bool Result = This->Create(parent,id,path,message,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDirPickerCtrl::GetPath
void wxDirPickerCtrl_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirPickerCtrl *This;
  This = (wxDirPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDirPickerCtrl::SetPath
void wxDirPickerCtrl_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirPickerCtrl *This;
  This = (wxDirPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary dirname_bin;
  wxString dirname;
  if(!enif_inspect_binary(env, argv[1], &dirname_bin)) Badarg("dirname");
  dirname = wxString(dirname_bin.data, wxConvUTF8, dirname_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(dirname);

}

// wxColourPickerCtrl::wxColourPickerCtrl
void wxColourPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColourPickerCtrl * Result = new EwxColourPickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxColourPickerCtrl"));

}

// wxColourPickerCtrl::wxColourPickerCtrl
void wxColourPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour col= *wxBLACK;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCLRP_DEFAULT_STYLE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, tpl[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  col = wxColour(colR,colG,colB,colA);
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
  wxColourPickerCtrl * Result = new EwxColourPickerCtrl(parent,id,col,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxColourPickerCtrl"));

}

// wxColourPickerCtrl::Create
void wxColourPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour col= *wxBLACK;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxCLRP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourPickerCtrl *This;
  This = (wxColourPickerCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  const ERL_NIF_TERM *col_t;
  int col_sz;
  if(!enif_get_tuple(env, tpl[1], &col_sz, &col_t)) Badarg("col");
  int colR;
  if(!enif_get_int(env, col_t[0], &colR)) Badarg("col");
  int colG;
  if(!enif_get_int(env, col_t[1], &colG)) Badarg("col");
  int colB;
  if(!enif_get_int(env, col_t[2], &colB)) Badarg("col");
  int colA;
  if(!enif_get_int(env, col_t[3], &colA)) Badarg("col");
  col = wxColour(colR,colG,colB,colA);
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
  bool Result = This->Create(parent,id,col,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxColourPickerCtrl::GetColour
void wxColourPickerCtrl_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourPickerCtrl *This;
  This = (wxColourPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxColourPickerCtrl::SetColour
void wxColourPickerCtrl_SetColour_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourPickerCtrl *This;
  This = (wxColourPickerCtrl *) memenv->getPtr(env, argv[0], "This");
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
  This->SetColour(col);

}

// wxColourPickerCtrl::SetColour
void wxColourPickerCtrl_SetColour_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourPickerCtrl *This;
  This = (wxColourPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary colname_bin;
  wxString colname;
  if(!enif_inspect_binary(env, argv[1], &colname_bin)) Badarg("colname");
  colname = wxString(colname_bin.data, wxConvUTF8, colname_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetColour(colname);

}

// wxDatePickerCtrl::wxDatePickerCtrl
void wxDatePickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDatePickerCtrl * Result = new EwxDatePickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDatePickerCtrl"));

}

// wxDatePickerCtrl::wxDatePickerCtrl
void wxDatePickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDateTime date= wxDefaultDateTime;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDP_DEFAULT|wxDP_SHOWCENTURY;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "validator"))) {
  validator = (wxValidator *) memenv->getPtr(env, tpl[1], "validator");
    } else        Badarg("Options");
  };
  wxDatePickerCtrl * Result = new EwxDatePickerCtrl(parent,id,date,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDatePickerCtrl"));

}

// wxDatePickerCtrl::GetRange
void wxDatePickerCtrl_GetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDatePickerCtrl *This;
  This = (wxDatePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *dt1_t;
  int dt1_sz;
  if(!enif_get_tuple(env, argv[1], &dt1_sz, &dt1_t)) Badarg("dt1");
  int dt1D;
  if(!enif_get_int(env, dt1_t[0], &dt1D)) Badarg("dt1");
  int dt1Mo;
  if(!enif_get_int(env, dt1_t[1], &dt1Mo)) Badarg("dt1");
  int dt1Y;
  if(!enif_get_int(env, dt1_t[2], &dt1Y)) Badarg("dt1");
  int dt1H;
  if(!enif_get_int(env, dt1_t[3], &dt1H)) Badarg("dt1");
  int dt1Mi;
  if(!enif_get_int(env, dt1_t[4], &dt1Mi)) Badarg("dt1");
  int dt1S;
  if(!enif_get_int(env, dt1_t[5], &dt1S)) Badarg("dt1");
 wxDateTime dt1 = wxDateTime((wxDateTime::wxDateTime_t) dt1D,(wxDateTime::Month) (dt1Mo-1),dt1Y,(wxDateTime::wxDateTime_t) dt1H,(wxDateTime::wxDateTime_t) dt1Mi,(wxDateTime::wxDateTime_t) dt1S);
  const ERL_NIF_TERM *dt2_t;
  int dt2_sz;
  if(!enif_get_tuple(env, argv[2], &dt2_sz, &dt2_t)) Badarg("dt2");
  int dt2D;
  if(!enif_get_int(env, dt2_t[0], &dt2D)) Badarg("dt2");
  int dt2Mo;
  if(!enif_get_int(env, dt2_t[1], &dt2Mo)) Badarg("dt2");
  int dt2Y;
  if(!enif_get_int(env, dt2_t[2], &dt2Y)) Badarg("dt2");
  int dt2H;
  if(!enif_get_int(env, dt2_t[3], &dt2H)) Badarg("dt2");
  int dt2Mi;
  if(!enif_get_int(env, dt2_t[4], &dt2Mi)) Badarg("dt2");
  int dt2S;
  if(!enif_get_int(env, dt2_t[5], &dt2S)) Badarg("dt2");
 wxDateTime dt2 = wxDateTime((wxDateTime::wxDateTime_t) dt2D,(wxDateTime::Month) (dt2Mo-1),dt2Y,(wxDateTime::wxDateTime_t) dt2H,(wxDateTime::wxDateTime_t) dt2Mi,(wxDateTime::wxDateTime_t) dt2S);
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetRange(&dt1,&dt2);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDatePickerCtrl::GetValue
void wxDatePickerCtrl_GetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDatePickerCtrl *This;
  This = (wxDatePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxDateTime Result = This->GetValue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDatePickerCtrl::SetRange
void wxDatePickerCtrl_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDatePickerCtrl *This;
  This = (wxDatePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *dt1_t;
  int dt1_sz;
  if(!enif_get_tuple(env, argv[1], &dt1_sz, &dt1_t)) Badarg("dt1");
  int dt1D;
  if(!enif_get_int(env, dt1_t[0], &dt1D)) Badarg("dt1");
  int dt1Mo;
  if(!enif_get_int(env, dt1_t[1], &dt1Mo)) Badarg("dt1");
  int dt1Y;
  if(!enif_get_int(env, dt1_t[2], &dt1Y)) Badarg("dt1");
  int dt1H;
  if(!enif_get_int(env, dt1_t[3], &dt1H)) Badarg("dt1");
  int dt1Mi;
  if(!enif_get_int(env, dt1_t[4], &dt1Mi)) Badarg("dt1");
  int dt1S;
  if(!enif_get_int(env, dt1_t[5], &dt1S)) Badarg("dt1");
 wxDateTime dt1 = wxDateTime((wxDateTime::wxDateTime_t) dt1D,(wxDateTime::Month) (dt1Mo-1),dt1Y,(wxDateTime::wxDateTime_t) dt1H,(wxDateTime::wxDateTime_t) dt1Mi,(wxDateTime::wxDateTime_t) dt1S);
  const ERL_NIF_TERM *dt2_t;
  int dt2_sz;
  if(!enif_get_tuple(env, argv[2], &dt2_sz, &dt2_t)) Badarg("dt2");
  int dt2D;
  if(!enif_get_int(env, dt2_t[0], &dt2D)) Badarg("dt2");
  int dt2Mo;
  if(!enif_get_int(env, dt2_t[1], &dt2Mo)) Badarg("dt2");
  int dt2Y;
  if(!enif_get_int(env, dt2_t[2], &dt2Y)) Badarg("dt2");
  int dt2H;
  if(!enif_get_int(env, dt2_t[3], &dt2H)) Badarg("dt2");
  int dt2Mi;
  if(!enif_get_int(env, dt2_t[4], &dt2Mi)) Badarg("dt2");
  int dt2S;
  if(!enif_get_int(env, dt2_t[5], &dt2S)) Badarg("dt2");
 wxDateTime dt2 = wxDateTime((wxDateTime::wxDateTime_t) dt2D,(wxDateTime::Month) (dt2Mo-1),dt2Y,(wxDateTime::wxDateTime_t) dt2H,(wxDateTime::wxDateTime_t) dt2Mi,(wxDateTime::wxDateTime_t) dt2S);
  if(!This) throw wxe_badarg("This");
  This->SetRange(dt1,dt2);

}

// wxDatePickerCtrl::SetValue
void wxDatePickerCtrl_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDatePickerCtrl *This;
  This = (wxDatePickerCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *dt_t;
  int dt_sz;
  if(!enif_get_tuple(env, argv[1], &dt_sz, &dt_t)) Badarg("dt");
  int dtD;
  if(!enif_get_int(env, dt_t[0], &dtD)) Badarg("dt");
  int dtMo;
  if(!enif_get_int(env, dt_t[1], &dtMo)) Badarg("dt");
  int dtY;
  if(!enif_get_int(env, dt_t[2], &dtY)) Badarg("dt");
  int dtH;
  if(!enif_get_int(env, dt_t[3], &dtH)) Badarg("dt");
  int dtMi;
  if(!enif_get_int(env, dt_t[4], &dtMi)) Badarg("dt");
  int dtS;
  if(!enif_get_int(env, dt_t[5], &dtS)) Badarg("dt");
 wxDateTime dt = wxDateTime((wxDateTime::wxDateTime_t) dtD,(wxDateTime::Month) (dtMo-1),dtY,(wxDateTime::wxDateTime_t) dtH,(wxDateTime::wxDateTime_t) dtMi,(wxDateTime::wxDateTime_t) dtS);
  if(!This) throw wxe_badarg("This");
  This->SetValue(dt);

}

// wxFontPickerCtrl::wxFontPickerCtrl
void wxFontPickerCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFontPickerCtrl * Result = new EwxFontPickerCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontPickerCtrl"));

}

// wxFontPickerCtrl::wxFontPickerCtrl
void wxFontPickerCtrl_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxFont * initial= &wxNullFont;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFNTP_DEFAULT_STYLE;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "initial"))) {
  initial = (wxFont *) memenv->getPtr(env, tpl[1], "initial");
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
  wxFontPickerCtrl * Result = new EwxFontPickerCtrl(parent,id,*initial,pos,size,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontPickerCtrl"));

}

// wxFontPickerCtrl::Create
void wxFontPickerCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  const wxFont * initial= &wxNullFont;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxFNTP_DEFAULT_STYLE;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "initial"))) {
  initial = (wxFont *) memenv->getPtr(env, tpl[1], "initial");
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
  bool Result = This->Create(parent,id,*initial,pos,size,style,*validator);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontPickerCtrl::GetSelectedFont
void wxFontPickerCtrl_GetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetSelectedFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFontPickerCtrl::SetSelectedFont
void wxFontPickerCtrl_SetSelectedFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetSelectedFont(*font);

}

// wxFontPickerCtrl::GetMaxPointSize
void wxFontPickerCtrl_GetMaxPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetMaxPointSize();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxFontPickerCtrl::SetMaxPointSize
void wxFontPickerCtrl_SetMaxPointSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontPickerCtrl *This;
  This = (wxFontPickerCtrl *) memenv->getPtr(env, argv[0], "This");
  unsigned int max;
  if(!enif_get_uint(env, argv[1], &max)) Badarg("max");
  if(!This) throw wxe_badarg("This");
  This->SetMaxPointSize(max);

}

// wxFindReplaceDialog::wxFindReplaceDialog
void wxFindReplaceDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFindReplaceDialog * Result = new EwxFindReplaceDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceDialog"));

}

// wxFindReplaceDialog::wxFindReplaceDialog
void wxFindReplaceDialog_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  wxFindReplaceData *data;
  data = (wxFindReplaceData *) memenv->getPtr(env, argv[1], "data");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  wxFindReplaceDialog * Result = new EwxFindReplaceDialog(parent,data,title,style);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceDialog"));

}

// wxFindReplaceDialog::Create
void wxFindReplaceDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceDialog *This;
  This = (wxFindReplaceDialog *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxFindReplaceData *data;
  data = (wxFindReplaceData *) memenv->getPtr(env, argv[2], "data");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "style"))) {
  if(!enif_get_int(env, tpl[1], &style)) Badarg("style"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,data,title,style);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFindReplaceDialog::GetData
void wxFindReplaceDialog_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceDialog *This;
  This = (wxFindReplaceDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFindReplaceData * Result = (wxFindReplaceData*)This->GetData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceData"));

}

// wxFindReplaceData::wxFindReplaceData
void wxFindReplaceData_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int flags=0;
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
  if(!enif_get_uint(env, tpl[1], &flags)) Badarg("flags");
    } else        Badarg("Options");
  };
  wxFindReplaceData * Result = new EwxFindReplaceData(flags);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFindReplaceData"));

}

// wxFindReplaceData::GetFindString
void wxFindReplaceData_GetFindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetFindString();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFindReplaceData::GetReplaceString
void wxFindReplaceData_GetReplaceString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetReplaceString();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFindReplaceData::GetFlags
void wxFindReplaceData_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFindReplaceData::SetFlags
void wxFindReplaceData_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  unsigned int flags;
  if(!enif_get_uint(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

// wxFindReplaceData::SetFindString
void wxFindReplaceData_SetFindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFindString(str);

}

// wxFindReplaceData::SetReplaceString
void wxFindReplaceData_SetReplaceString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFindReplaceData *This;
  This = (wxFindReplaceData *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetReplaceString(str);

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
  int selection;
  if(!enif_get_int(env, argv[1], &selection)) Badarg("selection"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSelection(selection);

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

// wxFontData::wxFontData
void wxFontData_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFontData * Result = new EwxFontData();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontData"));

}

// wxFontData::wxFontData
void wxFontData_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *data;
  data = (wxFontData *) memenv->getPtr(env, argv[0], "data");
  wxFontData * Result = new EwxFontData(*data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontData"));

}

// wxFontData::EnableEffects
void wxFontData_EnableEffects(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  bool enable;
  enable = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->EnableEffects(enable);

}

// wxFontData::GetAllowSymbols
void wxFontData_GetAllowSymbols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetAllowSymbols();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontData::GetColour
void wxFontData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxFontData::GetChosenFont
void wxFontData_GetChosenFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetChosenFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFontData::GetEnableEffects
void wxFontData_GetEnableEffects(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetEnableEffects();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontData::GetInitialFont
void wxFontData_GetInitialFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetInitialFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxFontData::GetShowHelp
void wxFontData_GetShowHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetShowHelp();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontData::SetAllowSymbols
void wxFontData_SetAllowSymbols(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  bool allowSymbols;
  allowSymbols = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetAllowSymbols(allowSymbols);

}

// wxFontData::SetChosenFont
void wxFontData_SetChosenFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetChosenFont(*font);

}

// wxFontData::SetColour
void wxFontData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
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

// wxFontData::SetInitialFont
void wxFontData_SetInitialFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetInitialFont(*font);

}

// wxFontData::SetRange
void wxFontData_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  int max;
  if(!enif_get_int(env, argv[2], &max)) Badarg("max"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(min,max);

}

// wxFontData::SetShowHelp
void wxFontData_SetShowHelp(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontData *This;
  This = (wxFontData *) memenv->getPtr(env, argv[0], "This");
  bool showHelp;
  showHelp = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetShowHelp(showHelp);

}

// wxFontDialog::wxFontDialog
void wxFontDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFontDialog * Result = new EwxFontDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontDialog"));

}

// wxFontDialog::wxFontDialog
void wxFontDialog_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[0], "parent");
  wxFontData *data;
  data = (wxFontData *) memenv->getPtr(env, argv[1], "data");
  wxFontDialog * Result = new EwxFontDialog(parent,*data);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontDialog"));

}

// wxFontDialog::Create
void wxFontDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontDialog *This;
  This = (wxFontDialog *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  wxFontData *data;
  data = (wxFontData *) memenv->getPtr(env, argv[2], "data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,*data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxFontDialog::GetFontData
void wxFontDialog_GetFontData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFontDialog *This;
  This = (wxFontDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFontData * Result = &This->GetFontData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFontData"));

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

// wxXmlResource::wxXmlResource
void wxXmlResource_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxXRC_USE_LOCALE;
  wxString domain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary filemask_bin;
  wxString filemask;
  if(!enif_inspect_binary(env, argv[0], &filemask_bin)) Badarg("filemask");
  filemask = wxString(filemask_bin.data, wxConvUTF8, filemask_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "domain"))) {
  ErlNifBinary domain_bin;
  if(!enif_inspect_binary(env, tpl[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
    } else        Badarg("Options");
  };
  wxXmlResource * Result = new EwxXmlResource(filemask,flags,domain);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxXmlResource"));

}

// wxXmlResource::wxXmlResource
void wxXmlResource_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxXRC_USE_LOCALE;
  wxString domain= wxEmptyString;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "domain"))) {
  ErlNifBinary domain_bin;
  if(!enif_inspect_binary(env, tpl[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
    } else        Badarg("Options");
  };
  wxXmlResource * Result = new EwxXmlResource(flags,domain);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxXmlResource"));

}

// wxXmlResource::AttachUnknownControl
void wxXmlResource_AttachUnknownControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindow * parent=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  wxWindow *control;
  control = (wxWindow *) memenv->getPtr(env, argv[2], "control");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
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
  if(!This) throw wxe_badarg("This");
  bool Result = This->AttachUnknownControl(name,control,parent);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxXmlResource::ClearHandlers
void wxXmlResource_ClearHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearHandlers();

}

// wxXmlResource::CompareVersion
void wxXmlResource_CompareVersion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  int major;
  if(!enif_get_int(env, argv[1], &major)) Badarg("major"); // int
  int minor;
  if(!enif_get_int(env, argv[2], &minor)) Badarg("minor"); // int
  int release;
  if(!enif_get_int(env, argv[3], &release)) Badarg("release"); // int
  int revision;
  if(!enif_get_int(env, argv[4], &revision)) Badarg("revision"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->CompareVersion(major,minor,release,revision);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxXmlResource::Get
void wxXmlResource_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxXmlResource * Result = (wxXmlResource*)wxXmlResource::Get();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxXmlResource"));

}

// wxXmlResource::GetFlags
void wxXmlResource_GetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFlags();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxXmlResource::GetVersion
void wxXmlResource_GetVersion(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetVersion();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxXmlResource::GetXRCID
void wxXmlResource_GetXRCID(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int value_if_not_found=wxID_NONE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary str_id_bin;
  wxString str_id;
  if(!enif_inspect_binary(env, argv[0], &str_id_bin)) Badarg("str_id");
  str_id = wxString(str_id_bin.data, wxConvUTF8, str_id_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "value_if_not_found"))) {
  if(!enif_get_int(env, tpl[1], &value_if_not_found)) Badarg("value_if_not_found"); // int
    } else        Badarg("Options");
  };
  int Result = wxXmlResource::GetXRCID(str_id,value_if_not_found);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxXmlResource::InitAllHandlers
void wxXmlResource_InitAllHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InitAllHandlers();

}

// wxXmlResource::Load
void wxXmlResource_Load(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filemask_bin;
  wxString filemask;
  if(!enif_inspect_binary(env, argv[1], &filemask_bin)) Badarg("filemask");
  filemask = wxString(filemask_bin.data, wxConvUTF8, filemask_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Load(filemask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxXmlResource::LoadBitmap
void wxXmlResource_LoadBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->LoadBitmap(name)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxXmlResource::LoadDialog
void wxXmlResource_LoadDialog_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[2], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxDialog * Result = (wxDialog*)This->LoadDialog(parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDialog"));

}

// wxXmlResource::LoadDialog
void wxXmlResource_LoadDialog_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxDialog *dlg;
  dlg = (wxDialog *) memenv->getPtr(env, argv[1], "dlg");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[2], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[3], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadDialog(dlg,parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxXmlResource::LoadFrame
void wxXmlResource_LoadFrame_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[2], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxFrame * Result = (wxFrame*)This->LoadFrame(parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxXmlResource::LoadFrame
void wxXmlResource_LoadFrame_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[1], "frame");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[2], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[3], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFrame(frame,parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxXmlResource::LoadIcon
void wxXmlResource_LoadIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxIcon * Result = new wxIcon(This->LoadIcon(name)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxXmlResource::LoadMenu
void wxXmlResource_LoadMenu(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxMenu * Result = (wxMenu*)This->LoadMenu(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenu"));

}

// wxXmlResource::LoadMenuBar
void wxXmlResource_LoadMenuBar_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[2], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxMenuBar * Result = (wxMenuBar*)This->LoadMenuBar(parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

// wxXmlResource::LoadMenuBar
void wxXmlResource_LoadMenuBar_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxMenuBar * Result = (wxMenuBar*)This->LoadMenuBar(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxMenuBar"));

}

// wxXmlResource::LoadPanel
void wxXmlResource_LoadPanel_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[2], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxPanel * Result = (wxPanel*)This->LoadPanel(parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPanel"));

}

// wxXmlResource::LoadPanel
void wxXmlResource_LoadPanel_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxPanel *panel;
  panel = (wxPanel *) memenv->getPtr(env, argv[1], "panel");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[2], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[3], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadPanel(panel,parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxXmlResource::LoadToolBar
void wxXmlResource_LoadToolBar(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  wxWindow *parent;
  parent = (wxWindow *) memenv->getPtr(env, argv[1], "parent");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[2], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxToolBar * Result = (wxToolBar*)This->LoadToolBar(parent,name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxToolBar"));

}

// wxXmlResource::Set
void wxXmlResource_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *res;
  res = (wxXmlResource *) memenv->getPtr(env, argv[0], "res");
  wxXmlResource * Result = (wxXmlResource*)wxXmlResource::Set(res);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxXmlResource"));

}

// wxXmlResource::SetFlags
void wxXmlResource_SetFlags(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  int flags;
  if(!enif_get_int(env, argv[1], &flags)) Badarg("flags"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFlags(flags);

}

// wxXmlResource::Unload
void wxXmlResource_Unload(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxXmlResource *This;
  This = (wxXmlResource *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->Unload(filename);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}


// XRCTRL macro implemented in erlang funcid wxXmlResource_xrcctrl
// wxHtmlEasyPrinting::wxHtmlEasyPrinting
void wxHtmlEasyPrinting_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString name= "Printing";
  wxWindow * parentWindow=NULL;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "name"))) {
  ErlNifBinary name_bin;
  if(!enif_inspect_binary(env, tpl[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "parentWindow"))) {
  parentWindow = (wxWindow *) memenv->getPtr(env, tpl[1], "parentWindow");
    } else        Badarg("Options");
  };
  wxHtmlEasyPrinting * Result = new EwxHtmlEasyPrinting(name,parentWindow);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxHtmlEasyPrinting"));

}

// wxHtmlEasyPrinting::GetPrintData
void wxHtmlEasyPrinting_GetPrintData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPrintData * Result = (wxPrintData*)This->GetPrintData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPrintData"));

}

// wxHtmlEasyPrinting::GetPageSetupData
void wxHtmlEasyPrinting_GetPageSetupData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPageSetupDialogData * Result = (wxPageSetupDialogData*)This->GetPageSetupData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPageSetupDialogData"));

}

// wxHtmlEasyPrinting::PreviewFile
void wxHtmlEasyPrinting_PreviewFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmlfile_bin;
  wxString htmlfile;
  if(!enif_inspect_binary(env, argv[1], &htmlfile_bin)) Badarg("htmlfile");
  htmlfile = wxString(htmlfile_bin.data, wxConvUTF8, htmlfile_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->PreviewFile(htmlfile);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PreviewText
void wxHtmlEasyPrinting_PreviewText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString basepath= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmltext_bin;
  wxString htmltext;
  if(!enif_inspect_binary(env, argv[1], &htmltext_bin)) Badarg("htmltext");
  htmltext = wxString(htmltext_bin.data, wxConvUTF8, htmltext_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "basepath"))) {
  ErlNifBinary basepath_bin;
  if(!enif_inspect_binary(env, tpl[1], &basepath_bin)) Badarg("basepath");
  basepath = wxString(basepath_bin.data, wxConvUTF8, basepath_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->PreviewText(htmltext,basepath);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PrintFile
void wxHtmlEasyPrinting_PrintFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmlfile_bin;
  wxString htmlfile;
  if(!enif_inspect_binary(env, argv[1], &htmlfile_bin)) Badarg("htmlfile");
  htmlfile = wxString(htmlfile_bin.data, wxConvUTF8, htmlfile_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->PrintFile(htmlfile);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PrintText
void wxHtmlEasyPrinting_PrintText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString basepath= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary htmltext_bin;
  wxString htmltext;
  if(!enif_inspect_binary(env, argv[1], &htmltext_bin)) Badarg("htmltext");
  htmltext = wxString(htmltext_bin.data, wxConvUTF8, htmltext_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "basepath"))) {
  ErlNifBinary basepath_bin;
  if(!enif_inspect_binary(env, tpl[1], &basepath_bin)) Badarg("basepath");
  basepath = wxString(basepath_bin.data, wxConvUTF8, basepath_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->PrintText(htmltext,basepath);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlEasyPrinting::PageSetup
void wxHtmlEasyPrinting_PageSetup(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->PageSetup();

}

// wxHtmlEasyPrinting::SetFonts
void wxHtmlEasyPrinting_SetFonts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int sizesLen;
  std::vector <int> sizes;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary normal_face_bin;
  wxString normal_face;
  if(!enif_inspect_binary(env, argv[1], &normal_face_bin)) Badarg("normal_face");
  normal_face = wxString(normal_face_bin.data, wxConvUTF8, normal_face_bin.size);
  ErlNifBinary fixed_face_bin;
  wxString fixed_face;
  if(!enif_inspect_binary(env, argv[2], &fixed_face_bin)) Badarg("fixed_face");
  fixed_face = wxString(fixed_face_bin.data, wxConvUTF8, fixed_face_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "sizes"))) {
  int sizes_tmp;
  ERL_NIF_TERM sizesHead, sizesTail;
  if(!enif_get_list_length(env, tpl[1], &sizesLen)) Badarg("sizes");
  sizesTail = tpl[1];
  while(!enif_is_empty_list(env, sizesTail)) {
    if(!enif_get_list_cell(env, sizesTail, &sizesHead, &sizesTail)) Badarg("sizes");
    if(!enif_get_int(env, sizesHead, &sizes_tmp)) Badarg("sizes");
    sizes.push_back( (int) sizes_tmp);
  };
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetFonts(normal_face,fixed_face, sizes.empty() ? (int *) NULL : sizes.data());

}

// wxHtmlEasyPrinting::SetHeader
void wxHtmlEasyPrinting_SetHeader(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pg=wxPAGE_ALL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary header_bin;
  wxString header;
  if(!enif_inspect_binary(env, argv[1], &header_bin)) Badarg("header");
  header = wxString(header_bin.data, wxConvUTF8, header_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pg"))) {
  if(!enif_get_int(env, tpl[1], &pg)) Badarg("pg"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetHeader(header,pg);

}

// wxHtmlEasyPrinting::SetFooter
void wxHtmlEasyPrinting_SetFooter(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int pg=wxPAGE_ALL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlEasyPrinting *This;
  This = (wxHtmlEasyPrinting *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary footer_bin;
  wxString footer;
  if(!enif_inspect_binary(env, argv[1], &footer_bin)) Badarg("footer");
  footer = wxString(footer_bin.data, wxConvUTF8, footer_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "pg"))) {
  if(!enif_get_int(env, tpl[1], &pg)) Badarg("pg"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetFooter(footer,pg);

}

#if wxUSE_GLCANVAS
// wxGLCanvas::wxGLCanvas
void wxGLCanvas_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  unsigned int attribListLen;
  std::vector <int> attribList;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  wxString name= "GLCanvas";
  const wxPalette * palette= &wxNullPalette;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "attribList"))) {
  int attribList_tmp;
  ERL_NIF_TERM attribListHead, attribListTail;
  if(!enif_get_list_length(env, tpl[1], &attribListLen)) Badarg("attribList");
  attribListTail = tpl[1];
  while(!enif_is_empty_list(env, attribListTail)) {
    if(!enif_get_list_cell(env, attribListTail, &attribListHead, &attribListTail)) Badarg("attribList");
    if(!enif_get_int(env, attribListHead, &attribList_tmp)) Badarg("attribList");
    attribList.push_back( (int) attribList_tmp);
  };
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "name"))) {
  ErlNifBinary name_bin;
  if(!enif_inspect_binary(env, tpl[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "palette"))) {
  palette = (wxPalette *) memenv->getPtr(env, tpl[1], "palette");
    } else        Badarg("Options");
  };
  wxGLCanvas * Result = new EwxGLCanvas(parent,id, attribList.empty() ? NULL : attribList.data(),pos,size,style,name,*palette);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGLCanvas"));

}

// wxGLCanvas::SetCurrent
void wxGLCanvas_SetCurrent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *This;
  This = (wxGLCanvas *) memenv->getPtr(env, argv[0], "This");
  wxGLContext *context;
  context = (wxGLContext *) memenv->getPtr(env, argv[1], "context");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetCurrent(*context);
 setActiveGL(memenv, Ecmd.caller, This, context);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGLCanvas::SwapBuffers
void wxGLCanvas_SwapBuffers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGLCanvas *This;
  This = (wxGLCanvas *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SwapBuffers();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif // wxUSE_GLCANVAS
