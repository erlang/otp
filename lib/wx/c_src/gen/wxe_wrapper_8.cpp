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

// wxFileDataObject::wxFileDataObject
void wxFileDataObject_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxFileDataObject * Result = new wxFileDataObject();
  app->newPtr((void *) Result, 216, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFileDataObject"));

}

// wxFileDataObject::AddFile
void wxFileDataObject_AddFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDataObject *This;
  This = (wxFileDataObject *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  if(!This) throw wxe_badarg("This");
  This->AddFile(file);

}

// wxFileDataObject::GetFilenames
void wxFileDataObject_GetFilenames(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDataObject *This;
  This = (wxFileDataObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxArrayString Result = This->GetFilenames();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDataObject::destroy
void wxFileDataObject_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDataObject *This;
  This = (wxFileDataObject *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
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

// wxClipboard::wxClipboard
void wxClipboard_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxClipboard * Result = new EwxClipboard();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxClipboard"));

}

// wxClipboard::AddData
void wxClipboard_AddData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  wxDataObject *data;
  data = (wxDataObject *) memenv->getPtr(env, argv[1], "data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddData(data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::Clear
void wxClipboard_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxClipboard::Close
void wxClipboard_Close(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Close();

}

// wxClipboard::Flush
void wxClipboard_Flush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Flush();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::GetData
void wxClipboard_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  wxDataObject *data;
  data = (wxDataObject *) memenv->getPtr(env, argv[1], "data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetData(*data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::IsOpened
void wxClipboard_IsOpened(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOpened();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::Open
void wxClipboard_Open(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Open();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::SetData
void wxClipboard_SetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  wxDataObject *data;
  data = (wxDataObject *) memenv->getPtr(env, argv[1], "data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetData(data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::UsePrimarySelection
void wxClipboard_UsePrimarySelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool primary=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "primary"))) {
  primary = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->UsePrimarySelection(primary);

}

// wxClipboard::IsSupported
void wxClipboard_IsSupported(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxClipboard *This;
  This = (wxClipboard *) memenv->getPtr(env, argv[0], "This");
  wxDataFormatId format;
  if(!enif_get_int(env, argv[1], (int *) &format)) Badarg("format"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSupported(format);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxClipboard::Get
void wxClipboard_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxClipboard * Result = (wxClipboard*)wxClipboard::Get();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxClipboard"));

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
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPosition(pos);

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
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[1], "window");
  if(!This) throw wxe_badarg("This");
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
  wxWindow *winOld;
  winOld = (wxWindow *) memenv->getPtr(env, argv[1], "winOld");
  wxWindow *winNew;
  winNew = (wxWindow *) memenv->getPtr(env, argv[2], "winNew");
  if(!This) throw wxe_badarg("This");
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
  double gravity;
  if(!wxe_get_double(env, argv[1], &gravity)) Badarg("gravity");
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
  This->SetSashPosition(position,redraw);

}

// wxSplitterWindow::SetMinimumPaneSize
void wxSplitterWindow_SetMinimumPaneSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  int paneSize;
  if(!enif_get_int(env, argv[1], &paneSize)) Badarg("paneSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetMinimumPaneSize(paneSize);

}

// wxSplitterWindow::SetSplitMode
void wxSplitterWindow_SetSplitMode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSplitterWindow *This;
  This = (wxSplitterWindow *) memenv->getPtr(env, argv[0], "This");
  int mode;
  if(!enif_get_int(env, argv[1], &mode)) Badarg("mode"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  int pos;
  if(!enif_get_int(env, argv[1], &pos)) Badarg("pos"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSashPosition(pos);

}

// wxHtmlWindow::wxHtmlWindow
void wxHtmlWindow_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxHtmlWindow * Result = new EwxHtmlWindow();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxHtmlWindow"));

}

// wxHtmlWindow::wxHtmlWindow
void wxHtmlWindow_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID id=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxHW_DEFAULT_STYLE;
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
  wxHtmlWindow * Result = new EwxHtmlWindow(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxHtmlWindow"));

}

// wxHtmlWindow::AppendToPage
void wxHtmlWindow_AppendToPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary source_bin;
  wxString source;
  if(!enif_inspect_binary(env, argv[1], &source_bin)) Badarg("source");
  source = wxString(source_bin.data, wxConvUTF8, source_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AppendToPage(source);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::GetOpenedAnchor
void wxHtmlWindow_GetOpenedAnchor(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOpenedAnchor();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::GetOpenedPage
void wxHtmlWindow_GetOpenedPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOpenedPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::GetOpenedPageTitle
void wxHtmlWindow_GetOpenedPageTitle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOpenedPageTitle();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::GetRelatedFrame
void wxHtmlWindow_GetRelatedFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFrame * Result = (wxFrame*)This->GetRelatedFrame();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFrame"));

}

// wxHtmlWindow::HistoryBack
void wxHtmlWindow_HistoryBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryBack();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::HistoryCanBack
void wxHtmlWindow_HistoryCanBack(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryCanBack();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::HistoryCanForward
void wxHtmlWindow_HistoryCanForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryCanForward();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::HistoryClear
void wxHtmlWindow_HistoryClear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->HistoryClear();

}

// wxHtmlWindow::HistoryForward
void wxHtmlWindow_HistoryForward(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HistoryForward();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::LoadFile
void wxHtmlWindow_LoadFile(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary filename_bin;
  wxString filenameStr;
  if(!enif_inspect_binary(env, argv[1], &filename_bin)) Badarg("filename");
  filenameStr = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
  wxFileName  filename = wxFileName(filenameStr);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(filename);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::LoadPage
void wxHtmlWindow_LoadPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary location_bin;
  wxString location;
  if(!enif_inspect_binary(env, argv[1], &location_bin)) Badarg("location");
  location = wxString(location_bin.data, wxConvUTF8, location_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadPage(location);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::SelectAll
void wxHtmlWindow_SelectAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->SelectAll();

}

// wxHtmlWindow::SelectionToText
void wxHtmlWindow_SelectionToText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->SelectionToText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlWindow::SelectLine
void wxHtmlWindow_SelectLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  This->SelectLine(pos);

}

// wxHtmlWindow::SelectWord
void wxHtmlWindow_SelectWord(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[1], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  This->SelectWord(pos);

}

// wxHtmlWindow::SetBorders
void wxHtmlWindow_SetBorders(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  int b;
  if(!enif_get_int(env, argv[1], &b)) Badarg("b"); // int
  if(!This) throw wxe_badarg("This");
  This->SetBorders(b);

}

// wxHtmlWindow::SetFonts
void wxHtmlWindow_SetFonts(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int sizesLen;
  std::vector <int> sizes;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
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
  This->SetFonts(normal_face,fixed_face, sizes.empty() ? NULL : sizes.data());

}

// wxHtmlWindow::SetPage
void wxHtmlWindow_SetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary source_bin;
  wxString source;
  if(!enif_inspect_binary(env, argv[1], &source_bin)) Badarg("source");
  source = wxString(source_bin.data, wxConvUTF8, source_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPage(source);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxHtmlWindow::SetRelatedFrame
void wxHtmlWindow_SetRelatedFrame(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  wxFrame *frame;
  frame = (wxFrame *) memenv->getPtr(env, argv[1], "frame");
  ErlNifBinary format_bin;
  wxString format;
  if(!enif_inspect_binary(env, argv[2], &format_bin)) Badarg("format");
  format = wxString(format_bin.data, wxConvUTF8, format_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetRelatedFrame(frame,format);

}

// wxHtmlWindow::SetRelatedStatusBar
void wxHtmlWindow_SetRelatedStatusBar_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRelatedStatusBar(index);

}

// wxHtmlWindow::SetRelatedStatusBar
void wxHtmlWindow_SetRelatedStatusBar_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int index=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  wxStatusBar *statusbar;
  statusbar = (wxStatusBar *) memenv->getPtr(env, argv[1], "statusbar");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetRelatedStatusBar(statusbar,index);

}

// wxHtmlWindow::ToText
void wxHtmlWindow_ToText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlWindow *This;
  This = (wxHtmlWindow *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->ToText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxHtmlLinkEvent::GetLinkInfo
void wxHtmlLinkEvent_GetLinkInfo(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxHtmlLinkEvent *This;
  This = (wxHtmlLinkEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxHtmlLinkInfo * Result = &This->GetLinkInfo();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

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

// wxLogNull::wxLogNull
void wxLogNull_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxLogNull * Result = new wxLogNull();
  app->newPtr((void *) Result, 230, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLogNull"));

}

// wxLogNull::~wxLogNull
void wxLogNull_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxLogNull *This;
  This = (wxLogNull *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
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

// wxLocale::wxLocale
void wxLocale_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxLocale * Result = new EwxLocale();
  app->newPtr((void *) Result, 234, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLocale"));

}

// wxLocale::wxLocale
void wxLocale_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxLOCALE_LOAD_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int language;
  if(!enif_get_int(env, argv[0], &language)) Badarg("language"); // int
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
  wxLocale * Result = new EwxLocale(language,flags);
  app->newPtr((void *) Result, 234, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLocale"));

}

// wxLocale::wxLocale
void wxLocale_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString shortName= wxEmptyString;
  wxString locale= wxEmptyString;
  bool bLoadDefault=true;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "shortName"))) {
  ErlNifBinary shortName_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortName_bin)) Badarg("shortName");
  shortName = wxString(shortName_bin.data, wxConvUTF8, shortName_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "locale"))) {
  ErlNifBinary locale_bin;
  if(!enif_inspect_binary(env, tpl[1], &locale_bin)) Badarg("locale");
  locale = wxString(locale_bin.data, wxConvUTF8, locale_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "bLoadDefault"))) {
  bLoadDefault = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxLocale * Result = new EwxLocale(name,shortName,locale,bLoadDefault);
  app->newPtr((void *) Result, 234, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxLocale"));

}

// wxLocale::~wxLocale
void wxLocale_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxLocale::Init
void wxLocale_Init_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int language=wxLANGUAGE_DEFAULT;
  int flags=wxLOCALE_LOAD_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "language"))) {
  if(!enif_get_int(env, tpl[1], &language)) Badarg("language"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Init(language,flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::Init
void wxLocale_Init_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString shortName= wxEmptyString;
  wxString locale= wxEmptyString;
  bool bLoadDefault=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "shortName"))) {
  ErlNifBinary shortName_bin;
  if(!enif_inspect_binary(env, tpl[1], &shortName_bin)) Badarg("shortName");
  shortName = wxString(shortName_bin.data, wxConvUTF8, shortName_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "locale"))) {
  ErlNifBinary locale_bin;
  if(!enif_inspect_binary(env, tpl[1], &locale_bin)) Badarg("locale");
  locale = wxString(locale_bin.data, wxConvUTF8, locale_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "bLoadDefault"))) {
  bLoadDefault = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Init(name,shortName,locale,bLoadDefault);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalog
void wxLocale_AddCatalog_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddCatalog(domain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalog
void wxLocale_AddCatalog_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  wxLanguage msgIdLanguage;
  if(!enif_get_int(env, argv[2], (int *) &msgIdLanguage)) Badarg("msgIdLanguage"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddCatalog(domain,msgIdLanguage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalog
void wxLocale_AddCatalog_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  wxLanguage msgIdLanguage;
  if(!enif_get_int(env, argv[2], (int *) &msgIdLanguage)) Badarg("msgIdLanguage"); // enum
  ErlNifBinary msgIdCharset_bin;
  wxString msgIdCharset;
  if(!enif_inspect_binary(env, argv[3], &msgIdCharset_bin)) Badarg("msgIdCharset");
  msgIdCharset = wxString(msgIdCharset_bin.data, wxConvUTF8, msgIdCharset_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->AddCatalog(domain,msgIdLanguage,msgIdCharset);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::AddCatalogLookupPathPrefix
void wxLocale_AddCatalogLookupPathPrefix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary prefix_bin;
  wxString prefix;
  if(!enif_inspect_binary(env, argv[0], &prefix_bin)) Badarg("prefix");
  prefix = wxString(prefix_bin.data, wxConvUTF8, prefix_bin.size);
  wxLocale::AddCatalogLookupPathPrefix(prefix);

}

// wxLocale::GetCanonicalName
void wxLocale_GetCanonicalName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetCanonicalName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetLanguage
void wxLocale_GetLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetLanguage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::GetLanguageName
void wxLocale_GetLanguageName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int lang;
  if(!enif_get_int(env, argv[0], &lang)) Badarg("lang"); // int
  wxString Result = wxLocale::GetLanguageName(lang);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetLocale
void wxLocale_GetLocale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetLocale();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetName
void wxLocale_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetString
void wxLocale_GetString_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString szDomain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary origString_bin;
  wxString origString;
  if(!enif_inspect_binary(env, argv[1], &origString_bin)) Badarg("origString");
  origString = wxString(origString_bin.data, wxConvUTF8, origString_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "szDomain"))) {
  ErlNifBinary szDomain_bin;
  if(!enif_inspect_binary(env, tpl[1], &szDomain_bin)) Badarg("szDomain");
  szDomain = wxString(szDomain_bin.data, wxConvUTF8, szDomain_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetString(origString,szDomain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetString
void wxLocale_GetString_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString szDomain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary origString_bin;
  wxString origString;
  if(!enif_inspect_binary(env, argv[1], &origString_bin)) Badarg("origString");
  origString = wxString(origString_bin.data, wxConvUTF8, origString_bin.size);
  ErlNifBinary origString2_bin;
  wxString origString2;
  if(!enif_inspect_binary(env, argv[2], &origString2_bin)) Badarg("origString2");
  origString2 = wxString(origString2_bin.data, wxConvUTF8, origString2_bin.size);
  unsigned int n;
  if(!enif_get_uint(env, argv[3], &n)) Badarg("n");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "szDomain"))) {
  ErlNifBinary szDomain_bin;
  if(!enif_inspect_binary(env, tpl[1], &szDomain_bin)) Badarg("szDomain");
  szDomain = wxString(szDomain_bin.data, wxConvUTF8, szDomain_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetString(origString,origString2,n,szDomain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetHeaderValue
void wxLocale_GetHeaderValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString szDomain= wxEmptyString;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "szDomain"))) {
  ErlNifBinary szDomain_bin;
  if(!enif_inspect_binary(env, tpl[1], &szDomain_bin)) Badarg("szDomain");
  szDomain = wxString(szDomain_bin.data, wxConvUTF8, szDomain_bin.size);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetHeaderValue(header,szDomain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetSysName
void wxLocale_GetSysName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetSysName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetSystemEncoding
void wxLocale_GetSystemEncoding(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxLocale::GetSystemEncoding();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::GetSystemEncodingName
void wxLocale_GetSystemEncodingName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = wxLocale::GetSystemEncodingName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxLocale::GetSystemLanguage
void wxLocale_GetSystemLanguage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxLocale::GetSystemLanguage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxLocale::IsLoaded
void wxLocale_IsLoaded(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary domain_bin;
  wxString domain;
  if(!enif_inspect_binary(env, argv[1], &domain_bin)) Badarg("domain");
  domain = wxString(domain_bin.data, wxConvUTF8, domain_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsLoaded(domain);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxLocale::IsOk
void wxLocale_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxLocale *This;
  This = (wxLocale *) memenv->getPtr(env, argv[0], "This");
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

// wxDCOverlay::wxDCOverlay
void wxDCOverlay_new_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxOverlay *overlay;
  overlay = (wxOverlay *) memenv->getPtr(env, argv[0], "overlay");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  int x;
  if(!enif_get_int(env, argv[2], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[3], &y)) Badarg("y"); // int
  int width;
  if(!enif_get_int(env, argv[4], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[5], &height)) Badarg("height"); // int
  wxDCOverlay * Result = new EwxDCOverlay(*overlay,dc,x,y,width,height);
  app->newPtr((void *) Result, 240, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDCOverlay"));

}

// wxDCOverlay::wxDCOverlay
void wxDCOverlay_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxOverlay *overlay;
  overlay = (wxOverlay *) memenv->getPtr(env, argv[0], "overlay");
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[1], "dc");
  wxDCOverlay * Result = new EwxDCOverlay(*overlay,dc);
  app->newPtr((void *) Result, 240, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDCOverlay"));

}

// wxDCOverlay::~wxDCOverlay
void wxDCOverlay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxDCOverlay *This;
  This = (wxDCOverlay *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxDCOverlay::Clear
void wxDCOverlay_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDCOverlay *This;
  This = (wxDCOverlay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxDropFilesEvent::GetPosition
void wxDropFilesEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDropFilesEvent *This;
  This = (wxDropFilesEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint Result = This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDropFilesEvent::GetNumberOfFiles
void wxDropFilesEvent_GetNumberOfFiles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDropFilesEvent *This;
  This = (wxDropFilesEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetNumberOfFiles();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDropFilesEvent::GetFiles
void wxDropFilesEvent_GetFiles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDropFilesEvent *This;
  This = (wxDropFilesEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString * Result = (wxString*)This->GetFiles();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_list_strings(This->m_noFiles, Result)
);

}

#if wxUSE_DISPLAY
// wxDisplay::wxDisplay
void wxDisplay_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDisplay * Result = new wxDisplay();
  app->newPtr((void *) Result, 242, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDisplay"));

}

// wxDisplay::wxDisplay
void wxDisplay_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  unsigned int index;
  if(!enif_get_uint(env, argv[0], &index)) Badarg("index");
  wxDisplay * Result = new wxDisplay(index);
  app->newPtr((void *) Result, 242, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDisplay"));

}

#if wxCHECK_VERSION(3,1,3)
// wxDisplay::wxDisplay
void wxDisplay_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *window;
  window = (wxWindow *) memenv->getPtr(env, argv[0], "window");
  wxDisplay * Result = new wxDisplay(window);
  app->newPtr((void *) Result, 242, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDisplay"));

}

#endif
// wxDisplay::~wxDisplay
void wxDisplay_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxDisplay::IsOk
void wxDisplay_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDisplay::GetClientArea
void wxDisplay_GetClientArea(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetClientArea();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDisplay::GetGeometry
void wxDisplay_GetGeometry(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetGeometry();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDisplay::GetName
void wxDisplay_GetName(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetName();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDisplay::IsPrimary
void wxDisplay_IsPrimary(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsPrimary();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDisplay::GetCount
void wxDisplay_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int Result = wxDisplay::GetCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxDisplay::GetFromPoint
void wxDisplay_GetFromPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  int Result = wxDisplay::GetFromPoint(pt);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDisplay::GetFromWindow
void wxDisplay_GetFromWindow(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxWindow *win;
  win = (wxWindow *) memenv->getPtr(env, argv[0], "win");
  int Result = wxDisplay::GetFromWindow(win);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

#if wxCHECK_VERSION(3,1,2)
// wxDisplay::GetPPI
void wxDisplay_GetPPI(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDisplay *This;
  This = (wxDisplay *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetPPI();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

#endif
#endif // wxUSE_DISPLAY
#if wxUSE_GRAPHICS_CONTEXT
// wxGCDC::wxGCDC
void wxGCDC_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM windowDC_type;
  void * windowDC = memenv->getPtr(env, argv[0], "windowDC", &windowDC_type);
  wxGCDC * Result;
  if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindowDC))
    Result = new EwxGCDC(* static_cast<wxWindowDC*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxMemoryDC))
    Result = new EwxGCDC(* static_cast<wxMemoryDC*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxGraphicsContext))
    Result = new EwxGCDC(static_cast<wxGraphicsContext*> (windowDC));
  else throw wxe_badarg("windowDC");
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGCDC"));

}

// wxGCDC::wxGCDC
void wxGCDC_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGCDC * Result = new EwxGCDC();
  app->newPtr((void *) Result, 8, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGCDC"));

}

// wxGCDC::GetGraphicsContext
void wxGCDC_GetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGCDC *This;
  This = (wxGCDC *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsContext * Result = (wxGraphicsContext*)This->GetGraphicsContext();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGCDC::SetGraphicsContext
void wxGCDC_SetGraphicsContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGCDC *This;
  This = (wxGCDC *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsContext *context;
  context = (wxGraphicsContext *) memenv->getPtr(env, argv[1], "context");
  if(!This) throw wxe_badarg("This");
  This->SetGraphicsContext(context);

}

#endif // wxUSE_GRAPHICS_CONTEXT
