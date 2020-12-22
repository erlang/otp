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

// wxListBox::wxListBox
void wxListBox_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListBox * Result = new EwxListBox();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListBox"));

}

// wxListBox::wxListBox
void wxListBox_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxListBox * Result = new EwxListBox(parent,id,pos,size,choices,style,*validator);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListBox"));

}

// wxListBox::Create
void wxListBox_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long style=0;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
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

// wxListBox::Deselect
void wxListBox_Deselect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->Deselect(n);

}

// wxListBox::GetSelections
void wxListBox_GetSelections(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayInt selections;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelections(selections);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_int(Result),
    rt.make(selections));
  rt.send(msg);

}

// wxListBox::InsertItems
void wxListBox_InsertItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM itemsHead, itemsTail;
  ErlNifBinary items_bin;
  wxArrayString items;
  itemsTail = argv[1];
  while(!enif_is_empty_list(env, itemsTail)) {
    if(!enif_get_list_cell(env, itemsTail, &itemsHead, &itemsTail)) Badarg("items");
    if(!enif_inspect_binary(env, itemsHead, &items_bin)) Badarg("items");
    items.Add(wxString(items_bin.data, wxConvUTF8, items_bin.size));
  };
  unsigned int pos;
  if(!enif_get_uint(env, argv[2], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->InsertItems(items,pos);

}

// wxListBox::IsSelected
void wxListBox_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelected(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListBox::Set
void wxListBox_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM itemsHead, itemsTail;
  ErlNifBinary items_bin;
  wxArrayString items;
  itemsTail = argv[1];
  while(!enif_is_empty_list(env, itemsTail)) {
    if(!enif_get_list_cell(env, itemsTail, &itemsHead, &itemsTail)) Badarg("items");
    if(!enif_inspect_binary(env, itemsHead, &items_bin)) Badarg("items");
    items.Add(wxString(items_bin.data, wxConvUTF8, items_bin.size));
  };
  if(!This) throw wxe_badarg("This");
  This->Set(items);

}

// wxListBox::HitTest
void wxListBox_HitTest_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[1], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(point);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListBox::HitTest
void wxListBox_HitTest_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->HitTest(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListBox::SetFirstItem
void wxListBox_SetFirstItem_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFirstItem(n);

}

// wxListBox::SetFirstItem
void wxListBox_SetFirstItem_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListBox *This;
  This = (wxListBox *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFirstItem(string);

}


void wxListCtrl_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListCtrl * Result = new EwxListCtrl();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListCtrl"));
}


  // skipped wxListCtrl_new_2
// wxListCtrl::Arrange
void wxListCtrl_Arrange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flag=wxLIST_ALIGN_DEFAULT;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flag"))) {
  if(!enif_get_int(env, tpl[1], &flag)) Badarg("flag"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Arrange(flag);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::AssignImageList
void wxListCtrl_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  int which;
  if(!enif_get_int(env, argv[2], &which)) Badarg("which"); // int
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList,which);

}

// wxListCtrl::ClearAll
void wxListCtrl_ClearAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ClearAll();

}


// wxListCtrl::Create
void wxListCtrl_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxWindowID winid=wxID_ANY;
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxLC_ICON;
  const wxValidator * validator= &wxDefaultValidator;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;

  int onGetItemText = 0, onGetItemAttr = 0, onGetItemColumnImage = 0;

  EwxListCtrl *This;
  This = (EwxListCtrl *) memenv->getPtr(env, argv[0], "This");
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
      if(!enif_get_int(env, tpl[1], &winid)) Badarg("winid"); // "wxWindowID"
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "onGetItemText"))) {
      if(!enif_get_int(env, tpl[1], &onGetItemText)) Badarg("onGetItemText");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "onGetItemAttr"))) {
      if(!enif_get_int(env, tpl[1], &onGetItemAttr)) Badarg("onGetItemAttr");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "onGetItemColumnImage"))) {
      if(!enif_get_int(env, tpl[1], &onGetItemColumnImage)) Badarg("onGetItemColumnImage");
    } else      Badarg("Options");
  };
  if(!This) throw wxe_badarg(0);
  bool Result = This->Create(parent,winid,pos,size,style,*validator);

  This->onGetItemText = onGetItemText;
  This->onGetItemAttr = onGetItemAttr;
  This->onGetItemColumnImage = onGetItemColumnImage;
  This->me_ref = memenv->me_ref;

  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::DeleteAllItems
void wxListCtrl_DeleteAllItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllItems();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::DeleteColumn
void wxListCtrl_DeleteColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteColumn(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::DeleteItem
void wxListCtrl_DeleteItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteItem(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::EditLabel
void wxListCtrl_EditLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->EditLabel(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxListCtrl::EnsureVisible
void wxListCtrl_EnsureVisible(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->EnsureVisible(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::FindItem
void wxListCtrl_FindItem_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool partial=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long start;
  if(!enif_get_long(env, argv[1], &start)) Badarg("start");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[2], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "partial"))) {
  partial = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  long Result = This->FindItem(start,str,partial);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::FindItem
void wxListCtrl_FindItem_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long start;
  if(!enif_get_long(env, argv[1], &start)) Badarg("start");
  const ERL_NIF_TERM *pt_t;
  int pt_sz;
  if(!enif_get_tuple(env, argv[2], &pt_sz, &pt_t)) Badarg("pt");
  int ptX;
  if(!enif_get_int(env, pt_t[0], &ptX)) Badarg("pt");
  int ptY;
  if(!enif_get_int(env, pt_t[1], &ptY)) Badarg("pt");
  wxPoint pt = wxPoint(ptX,ptY);
  int direction;
  if(!enif_get_int(env, argv[3], &direction)) Badarg("direction"); // int
  if(!This) throw wxe_badarg("This");
  long Result = This->FindItem(start,pt,direction);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetColumn
void wxListCtrl_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  wxListItem *item;
  item = (wxListItem *) memenv->getPtr(env, argv[2], "item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetColumn(col,*item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::GetColumnCount
void wxListCtrl_GetColumnCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumnCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetColumnWidth
void wxListCtrl_GetColumnWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumnWidth(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetCountPerPage
void wxListCtrl_GetCountPerPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCountPerPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetEditControl
void wxListCtrl_GetEditControl(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxTextCtrl * Result = (wxTextCtrl*)This->GetEditControl();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxTextCtrl"));

}

// wxListCtrl::GetImageList
void wxListCtrl_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int which;
  if(!enif_get_int(env, argv[1], &which)) Badarg("which"); // int
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList(which);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxListCtrl::GetItem
void wxListCtrl_GetItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[1], "info");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetItem(*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::GetItemBackgroundColour
void wxListCtrl_GetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetItemBackgroundColour(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetItemCount
void wxListCtrl_GetItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetItemCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetItemData
void wxListCtrl_GetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxUIntPtr Result = This->GetItemData(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetItemFont
void wxListCtrl_GetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetItemFont(item)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxListCtrl::GetItemPosition
void wxListCtrl_GetItemPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetItemPosition(item,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(pos));
  rt.send(msg);

}

// wxListCtrl::GetItemRect
void wxListCtrl_GetItemRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxRect rect;
  int code=wxLIST_RECT_BOUNDS;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "code"))) {
  if(!enif_get_int(env, tpl[1], &code)) Badarg("code"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetItemRect(item,rect,code);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_bool(Result),
    rt.make(rect));
  rt.send(msg);

}

// wxListCtrl::GetItemSpacing
void wxListCtrl_GetItemSpacing(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxSize Result = This->GetItemSpacing();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetItemState
void wxListCtrl_GetItemState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long stateMask;
  if(!enif_get_long(env, argv[2], &stateMask)) Badarg("stateMask");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetItemState(item,stateMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetItemText
void wxListCtrl_GetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int col=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
  if(!enif_get_int(env, tpl[1], &col)) Badarg("col"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetItemText(item,col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetItemTextColour
void wxListCtrl_GetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetItemTextColour(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetNextItem
void wxListCtrl_GetNextItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int geometry=wxLIST_NEXT_ALL;
  int state=wxLIST_STATE_DONTCARE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "geometry"))) {
  if(!enif_get_int(env, tpl[1], &geometry)) Badarg("geometry"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "state"))) {
  if(!enif_get_int(env, tpl[1], &state)) Badarg("state"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  long Result = This->GetNextItem(item,geometry,state);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetSelectedItemCount
void wxListCtrl_GetSelectedItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelectedItemCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetTextColour
void wxListCtrl_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::GetTopItem
void wxListCtrl_GetTopItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetTopItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::GetViewRect
void wxListCtrl_GetViewRect(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect Result = This->GetViewRect();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListCtrl::HitTest
void wxListCtrl_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags;
  long ptrSubItem;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[1], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  if(!This) throw wxe_badarg("This");
  long Result = This->HitTest(point,flags,&ptrSubItem);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_int(Result),
    rt.make_int(flags),
  rt.make_int(ptrSubItem));
  rt.send(msg);

}

// wxListCtrl::InsertColumn
void wxListCtrl_InsertColumn_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long col;
  if(!enif_get_long(env, argv[1], &col)) Badarg("col");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[2], "info");
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertColumn(col,*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertColumn
void wxListCtrl_InsertColumn_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int format=wxLIST_FORMAT_LEFT;
  int width=wxLIST_AUTOSIZE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long col;
  if(!enif_get_long(env, argv[1], &col)) Badarg("col");
  ErlNifBinary heading_bin;
  wxString heading;
  if(!enif_inspect_binary(env, argv[2], &heading_bin)) Badarg("heading");
  heading = wxString(heading_bin.data, wxConvUTF8, heading_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "format"))) {
  if(!enif_get_int(env, tpl[1], &format)) Badarg("format"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "width"))) {
  if(!enif_get_int(env, tpl[1], &width)) Badarg("width"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertColumn(col,heading,format,width);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[1], "info");
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(index,label);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  int imageIndex;
  if(!enif_get_int(env, argv[2], &imageIndex)) Badarg("imageIndex"); // int
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(index,imageIndex);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::InsertItem
void wxListCtrl_InsertItem_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[2], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  int imageIndex;
  if(!enif_get_int(env, argv[3], &imageIndex)) Badarg("imageIndex"); // int
  if(!This) throw wxe_badarg("This");
  long Result = This->InsertItem(index,label,imageIndex);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListCtrl::RefreshItem
void wxListCtrl_RefreshItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  This->RefreshItem(item);

}

// wxListCtrl::RefreshItems
void wxListCtrl_RefreshItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long itemFrom;
  if(!enif_get_long(env, argv[1], &itemFrom)) Badarg("itemFrom");
  long itemTo;
  if(!enif_get_long(env, argv[2], &itemTo)) Badarg("itemTo");
  if(!This) throw wxe_badarg("This");
  This->RefreshItems(itemFrom,itemTo);

}

// wxListCtrl::ScrollList
void wxListCtrl_ScrollList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int dx;
  if(!enif_get_int(env, argv[1], &dx)) Badarg("dx"); // int
  int dy;
  if(!enif_get_int(env, argv[2], &dy)) Badarg("dy"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->ScrollList(dx,dy);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetBackgroundColour
void wxListCtrl_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
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
  bool Result = This->SetBackgroundColour(col);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetColumn
void wxListCtrl_SetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  wxListItem *item;
  item = (wxListItem *) memenv->getPtr(env, argv[2], "item");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetColumn(col,*item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetColumnWidth
void wxListCtrl_SetColumnWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int width;
  if(!enif_get_int(env, argv[2], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetColumnWidth(col,width);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetImageList
void wxListCtrl_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  int which;
  if(!enif_get_int(env, argv[2], &which)) Badarg("which"); // int
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList,which);

}

// wxListCtrl::SetItem
void wxListCtrl_SetItem_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  wxListItem *info;
  info = (wxListItem *) memenv->getPtr(env, argv[1], "info");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItem(*info);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItem
void wxListCtrl_SetItem_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int imageId=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  int column;
  if(!enif_get_int(env, argv[2], &column)) Badarg("column"); // int
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "imageId"))) {
  if(!enif_get_int(env, tpl[1], &imageId)) Badarg("imageId"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItem(index,column,label,imageId);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemBackgroundColour
void wxListCtrl_SetItemBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
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

// wxListCtrl::SetItemCount
void wxListCtrl_SetItemCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long count;
  if(!enif_get_long(env, argv[1], &count)) Badarg("count");
  if(!This) throw wxe_badarg("This");
  This->SetItemCount(count);

}

// wxListCtrl::SetItemData
void wxListCtrl_SetItemData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long data;
  if(!enif_get_long(env, argv[2], &data)) Badarg("data");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemData(item,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemFont
void wxListCtrl_SetItemFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  if(!This) throw wxe_badarg("This");
  This->SetItemFont(item,*font);

}

// wxListCtrl::SetItemImage
void wxListCtrl_SetItemImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int selImage=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "selImage"))) {
  if(!enif_get_int(env, tpl[1], &selImage)) Badarg("selImage"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemImage(item,image,selImage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemColumnImage
void wxListCtrl_SetItemColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long column;
  if(!enif_get_long(env, argv[2], &column)) Badarg("column");
  int image;
  if(!enif_get_int(env, argv[3], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemColumnImage(item,column,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemPosition
void wxListCtrl_SetItemPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemPosition(item,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemState
void wxListCtrl_SetItemState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  long state;
  if(!enif_get_long(env, argv[2], &state)) Badarg("state");
  long stateMask;
  if(!enif_get_long(env, argv[3], &stateMask)) Badarg("stateMask");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetItemState(item,state,stateMask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListCtrl::SetItemText
void wxListCtrl_SetItemText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[2], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetItemText(item,text);

}

// wxListCtrl::SetItemTextColour
void wxListCtrl_SetItemTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
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

// wxListCtrl::SetSingleStyle
void wxListCtrl_SetSingleStyle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool add=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long style;
  if(!enif_get_long(env, argv[1], &style)) Badarg("style");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "add"))) {
  add = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetSingleStyle(style,add);

}

// wxListCtrl::SetTextColour
void wxListCtrl_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
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
  This->SetTextColour(col);

}

// wxListCtrl::SetWindowStyleFlag
void wxListCtrl_SetWindowStyleFlag(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListCtrl *This;
  This = (wxListCtrl *) memenv->getPtr(env, argv[0], "This");
  long style;
  if(!enif_get_long(env, argv[1], &style)) Badarg("style");
  if(!This) throw wxe_badarg("This");
  This->SetWindowStyleFlag(style);

}


void wxListCtrl_SortItems(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  EwxListCtrl *This;
  This = (EwxListCtrl *) memenv->getPtr(env, argv[0], "This");
  // wxListCtrlCompare *fn;
  // fn = (wxListCtrlCompare *) memenv->getPtr(env, argv[1]);
  callbackInfo cb = callbackInfo();
  cb.me_ref = memenv->me_ref;
  if(!enif_get_int(env, argv[1], &cb.callbackID)) Badarg("CallBack");
  // long data;
  // if(!enif_get_long(env, argv[2], &data)) Badarg("data");
  if(!This) throw wxe_badarg(0);
  bool Result = This->SortItems(wxEListCtrlCompare, (wxeIntPtr) &cb);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));
  wxeReturn rt2 = wxeReturn(memenv, memenv->owner, false);
  rt2.send( enif_make_tuple2(rt2.env,
			     rt2.make_atom("wx_delete_cb"),
			     rt2.make_int(cb.callbackID)));
}

// wxListView::ClearColumnImage
void wxListView_ClearColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->ClearColumnImage(col);

}

// wxListView::Focus
void wxListView_Focus(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  This->Focus(index);

}

// wxListView::GetFirstSelected
void wxListView_GetFirstSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetFirstSelected();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListView::GetFocusedItem
void wxListView_GetFocusedItem(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetFocusedItem();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListView::GetNextSelected
void wxListView_GetNextSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long item;
  if(!enif_get_long(env, argv[1], &item)) Badarg("item");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetNextSelected(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListView::IsSelected
void wxListView_IsSelected(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long index;
  if(!enif_get_long(env, argv[1], &index)) Badarg("index");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsSelected(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListView::Select
void wxListView_Select(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool on=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  long n;
  if(!enif_get_long(env, argv[1], &n)) Badarg("n");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "on"))) {
  on = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Select(n,on);

}

// wxListView::SetColumnImage
void wxListView_SetColumnImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListView *This;
  This = (wxListView *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColumnImage(col,image);

}

// wxListItem::wxListItem
void wxListItem_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListItem * Result = new EwxListItem();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItem"));

}

// wxListItem::wxListItem
void wxListItem_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *item;
  item = (wxListItem *) memenv->getPtr(env, argv[0], "item");
  wxListItem * Result = new EwxListItem(*item);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItem"));

}

// wxListItem::Clear
void wxListItem_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxListItem::GetAlign
void wxListItem_GetAlign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetAlign();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetBackgroundColour
void wxListItem_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListItem::GetColumn
void wxListItem_GetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetColumn();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetFont
void wxListItem_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxFont * Result = new wxFont(This->GetFont()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxListItem::GetId
void wxListItem_GetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetImage
void wxListItem_GetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetImage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetMask
void wxListItem_GetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetState
void wxListItem_GetState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  long Result = This->GetState();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::GetText
void wxListItem_GetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxString Result = This->GetText();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListItem::GetTextColour
void wxListItem_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListItem::GetWidth
void wxListItem_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListItem::SetAlign
void wxListItem_SetAlign(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  wxListColumnFormat align;
  if(!enif_get_int(env, argv[1], (int *) &align)) Badarg("align"); // enum
  if(!This) throw wxe_badarg("This");
  This->SetAlign(align);

}

// wxListItem::SetBackgroundColour
void wxListItem_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
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

// wxListItem::SetColumn
void wxListItem_SetColumn(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  int col;
  if(!enif_get_int(env, argv[1], &col)) Badarg("col"); // int
  if(!This) throw wxe_badarg("This");
  This->SetColumn(col);

}

// wxListItem::SetFont
void wxListItem_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxListItem::SetId
void wxListItem_SetId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long id;
  if(!enif_get_long(env, argv[1], &id)) Badarg("id");
  if(!This) throw wxe_badarg("This");
  This->SetId(id);

}

// wxListItem::SetImage
void wxListItem_SetImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  int image;
  if(!enif_get_int(env, argv[1], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  This->SetImage(image);

}

// wxListItem::SetMask
void wxListItem_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long mask;
  if(!enif_get_long(env, argv[1], &mask)) Badarg("mask");
  if(!This) throw wxe_badarg("This");
  This->SetMask(mask);

}

// wxListItem::SetState
void wxListItem_SetState(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long state;
  if(!enif_get_long(env, argv[1], &state)) Badarg("state");
  if(!This) throw wxe_badarg("This");
  This->SetState(state);

}

// wxListItem::SetStateMask
void wxListItem_SetStateMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  long stateMask;
  if(!enif_get_long(env, argv[1], &stateMask)) Badarg("stateMask");
  if(!This) throw wxe_badarg("This");
  This->SetStateMask(stateMask);

}

// wxListItem::SetText
void wxListItem_SetText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetText(text);

}

// wxListItem::SetTextColour
void wxListItem_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
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

// wxListItem::SetWidth
void wxListItem_SetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItem *This;
  This = (wxListItem *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->SetWidth(width);

}

// wxListItemAttr::wxListItemAttr
void wxListItemAttr_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListItemAttr * Result = new wxListItemAttr();
  app->newPtr((void *) Result, 102, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItemAttr"));

}

// wxListItemAttr::wxListItemAttr
void wxListItemAttr_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
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
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[2], "font");
  wxListItemAttr * Result = new wxListItemAttr(colText,colBack,*font);
  app->newPtr((void *) Result, 102, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListItemAttr"));

}

// wxListItemAttr::GetBackgroundColour
void wxListItemAttr_GetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxListItemAttr::GetFont
void wxListItemAttr_GetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxFont * Result = &This->GetFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFont"));

}

// wxListItemAttr::GetTextColour
void wxListItemAttr_GetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxColour * Result = &This->GetTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxListItemAttr::HasBackgroundColour
void wxListItemAttr_HasBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasBackgroundColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItemAttr::HasFont
void wxListItemAttr_HasFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasFont();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItemAttr::HasTextColour
void wxListItemAttr_HasTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasTextColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListItemAttr::SetBackgroundColour
void wxListItemAttr_SetBackgroundColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxListItemAttr::SetFont
void wxListItemAttr_SetFont(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxListItemAttr::SetTextColour
void wxListItemAttr_SetTextColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
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

// wxListItemAttr::destroy
void wxListItemAttr_destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxListItemAttr *This;
  This = (wxListItemAttr *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxImageList::wxImageList
void wxImageList_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxImageList * Result = new EwxImageList();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxImageList::wxImageList
void wxImageList_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool mask=true;
  int initialCount=1;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mask"))) {
  mask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "initialCount"))) {
  if(!enif_get_int(env, tpl[1], &initialCount)) Badarg("initialCount"); // int
    } else        Badarg("Options");
  };
  wxImageList * Result = new EwxImageList(width,height,mask,initialCount);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxImageList::Add
void wxImageList_Add_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  wxBitmap *mask;
  mask = (wxBitmap *) memenv->getPtr(env, argv[2], "mask");
  if(!This) throw wxe_badarg("This");
  int Result = This->Add(*bitmap,*mask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::Add
void wxImageList_Add_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[1], "bitmap");
  const ERL_NIF_TERM *maskColour_t;
  int maskColour_sz;
  if(!enif_get_tuple(env, argv[2], &maskColour_sz, &maskColour_t)) Badarg("maskColour");
  int maskColourR;
  if(!enif_get_int(env, maskColour_t[0], &maskColourR)) Badarg("maskColour");
  int maskColourG;
  if(!enif_get_int(env, maskColour_t[1], &maskColourG)) Badarg("maskColour");
  int maskColourB;
  if(!enif_get_int(env, maskColour_t[2], &maskColourB)) Badarg("maskColour");
  int maskColourA;
  if(!enif_get_int(env, maskColour_t[3], &maskColourA)) Badarg("maskColour");
  wxColour maskColour = wxColour(maskColourR,maskColourG,maskColourB,maskColourA);
  if(!This) throw wxe_badarg("This");
  int Result = This->Add(*bitmap,maskColour);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::Add
void wxImageList_Add_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM icon_type;
  void * icon = memenv->getPtr(env, argv[1], "icon", &icon_type);
  if(!This) throw wxe_badarg("This");
  int Result;
  if(enif_is_identical(icon_type, WXE_ATOM_wxIcon))
   Result =  This->Add(* static_cast<wxIcon*> (icon));
  else if(enif_is_identical(icon_type, WXE_ATOM_wxBitmap))
   Result =  This->Add(* static_cast<wxBitmap*> (icon));
  else throw wxe_badarg("icon");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::Create
void wxImageList_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool mask=true;
  int initialCount=1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mask"))) {
  mask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "initialCount"))) {
  if(!enif_get_int(env, tpl[1], &initialCount)) Badarg("initialCount"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,mask,initialCount);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::Draw
void wxImageList_Draw(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxIMAGELIST_DRAW_NORMAL;
  bool solidBackground=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  wxDC *dc;
  dc = (wxDC *) memenv->getPtr(env, argv[2], "dc");
  int x;
  if(!enif_get_int(env, argv[3], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[4], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "solidBackground"))) {
  solidBackground = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Draw(index,*dc,x,y,flags,solidBackground);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::GetBitmap
void wxImageList_GetBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  wxBitmap * Result = new wxBitmap(This->GetBitmap(index)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxBitmap"));

}

// wxImageList::GetIcon
void wxImageList_GetIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  wxIcon * Result = new wxIcon(This->GetIcon(index)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxImageList::GetImageCount
void wxImageList_GetImageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetImageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImageList::GetSize
void wxImageList_GetSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int width;
  int height;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetSize(index,width,height);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple3(rt.env,
  rt.make_bool(Result),
    rt.make_int(width),
  rt.make_int(height));
  rt.send(msg);

}

// wxImageList::Remove
void wxImageList_Remove(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->Remove(index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::RemoveAll
void wxImageList_RemoveAll(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->RemoveAll();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::Replace
void wxImageList_Replace_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  wxBitmap *bitmap;
  bitmap = (wxBitmap *) memenv->getPtr(env, argv[2], "bitmap");
  wxBitmap *mask;
  mask = (wxBitmap *) memenv->getPtr(env, argv[3], "mask");
  if(!This) throw wxe_badarg("This");
  bool Result = This->Replace(index,*bitmap,*mask);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImageList::Replace
void wxImageList_Replace_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImageList *This;
  This = (wxImageList *) memenv->getPtr(env, argv[0], "This");
  int index;
  if(!enif_get_int(env, argv[1], &index)) Badarg("index"); // int
  ERL_NIF_TERM icon_type;
  void * icon = memenv->getPtr(env, argv[2], "icon", &icon_type);
  if(!This) throw wxe_badarg("This");
  bool Result;
  if(enif_is_identical(icon_type, WXE_ATOM_wxIcon))
   Result =  This->Replace(index,* static_cast<wxIcon*> (icon));
  else if(enif_is_identical(icon_type, WXE_ATOM_wxBitmap))
   Result =  This->Replace(index,* static_cast<wxBitmap*> (icon));
  else throw wxe_badarg("icon");
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

// wxChoicebook::wxChoicebook
void wxChoicebook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxChoicebook * Result = new EwxChoicebook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxChoicebook"));

}

// wxChoicebook::wxChoicebook
void wxChoicebook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxChoicebook * Result = new EwxChoicebook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxChoicebook"));

}

// wxChoicebook::AddPage
void wxChoicebook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::AdvanceSelection
void wxChoicebook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool forward=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::AssignImageList
void wxChoicebook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxChoicebook::Create
void wxChoicebook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::DeleteAllPages
void wxChoicebook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxChoicebook::GetCurrentPage
void wxChoicebook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxChoicebook::GetImageList
void wxChoicebook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxChoicebook::GetPage
void wxChoicebook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxChoicebook::GetPageCount
void wxChoicebook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxChoicebook::GetPageImage
void wxChoicebook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageImage(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxChoicebook::GetPageText
void wxChoicebook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxChoicebook::GetSelection
void wxChoicebook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxChoicebook::HitTest
void wxChoicebook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::InsertPage
void wxChoicebook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::SetImageList
void wxChoicebook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxChoicebook::SetPageSize
void wxChoicebook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::SetPageImage
void wxChoicebook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageImage(page,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxChoicebook::SetPageText
void wxChoicebook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
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

// wxChoicebook::SetSelection
void wxChoicebook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxChoicebook::ChangeSelection
void wxChoicebook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxChoicebook *This;
  This = (wxChoicebook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->ChangeSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

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

// wxListbook::wxListbook
void wxListbook_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxListbook * Result = new EwxListbook();
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListbook"));

}

// wxListbook::wxListbook
void wxListbook_new_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
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
  wxListbook * Result = new EwxListbook(parent,id,pos,size,style);
  app->newPtr((void *) Result, 0, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxListbook"));

}

// wxListbook::AddPage
void wxListbook_AddPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::AdvanceSelection
void wxListbook_AdvanceSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool forward=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::AssignImageList
void wxListbook_AssignImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->AssignImageList(imageList);

}

// wxListbook::Create
void wxListbook_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::DeleteAllPages
void wxListbook_DeleteAllPages(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->DeleteAllPages();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::GetCurrentPage
void wxListbook_GetCurrentPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetCurrentPage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxListbook::GetImageList
void wxListbook_GetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImageList * Result = (wxImageList*)This->GetImageList();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImageList"));

}

// wxListbook::GetPage
void wxListbook_GetPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  wxWindow * Result = (wxWindow*)This->GetPage(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxWindow"));

}

// wxListbook::GetPageCount
void wxListbook_GetPageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetPageCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::GetPageImage
void wxListbook_GetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetPageImage(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::GetPageText
void wxListbook_GetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t nPage;
  if(!wxe_get_size_t(env, argv[1], &nPage)) Badarg("nPage");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPageText(nPage);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxListbook::GetSelection
void wxListbook_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::HitTest
void wxListbook_HitTest(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  long flags;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::InsertPage
void wxListbook_InsertPage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bSelect=false;
  int imageId=wxBookCtrlBase::NO_IMAGE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::SetImageList
void wxListbook_SetImageList(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  wxImageList *imageList;
  imageList = (wxImageList *) memenv->getPtr(env, argv[1], "imageList");
  if(!This) throw wxe_badarg("This");
  This->SetImageList(imageList);

}

// wxListbook::SetPageSize
void wxListbook_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::SetPageImage
void wxListbook_SetPageImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  int image;
  if(!enif_get_int(env, argv[2], &image)) Badarg("image"); // int
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetPageImage(page,image);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxListbook::SetPageText
void wxListbook_SetPageText(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
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

// wxListbook::SetSelection
void wxListbook_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->SetSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxListbook::ChangeSelection
void wxListbook_ChangeSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxListbook *This;
  This = (wxListbook *) memenv->getPtr(env, argv[0], "This");
  size_t page;
  if(!wxe_get_size_t(env, argv[1], &page)) Badarg("page");
  if(!This) throw wxe_badarg("This");
  int Result = This->ChangeSelection(page);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

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
  if(!This) throw wxe_badarg("This");
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
  int viewStart;
  if(!enif_get_int(env, argv[1], &viewStart)) Badarg("viewStart"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
  This->SetScrollbar(position,thumbSize,range,pageSize,refresh);

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
  int min;
  if(!enif_get_int(env, argv[1], &min)) Badarg("min"); // int
  int max;
  if(!enif_get_int(env, argv[2], &max)) Badarg("max"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(min,max);

}

// wxSpinButton::SetValue
void wxSpinButton_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinButton *This;
  This = (wxSpinButton *) memenv->getPtr(env, argv[0], "This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetValue(text);

}

// wxSpinCtrl::SetValue
void wxSpinCtrl_SetValue_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  if(!This) throw wxe_badarg("This");
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
  int minVal;
  if(!enif_get_int(env, argv[1], &minVal)) Badarg("minVal"); // int
  int maxVal;
  if(!enif_get_int(env, argv[2], &maxVal)) Badarg("maxVal"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(minVal,maxVal);

}

// wxSpinCtrl::SetSelection
void wxSpinCtrl_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSpinCtrl *This;
  This = (wxSpinCtrl *) memenv->getPtr(env, argv[0], "This");
  long from;
  if(!enif_get_long(env, argv[1], &from)) Badarg("from");
  long to;
  if(!enif_get_long(env, argv[2], &to)) Badarg("to");
  if(!This) throw wxe_badarg("This");
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
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetLabel(label);

}

// wxStaticText::Wrap
void wxStaticText_Wrap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStaticText *This;
  This = (wxStaticText *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  if(!This) throw wxe_badarg("This");
  This->Wrap(width);

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
  if(!This) throw wxe_badarg("This");
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
  wxBitmap *label;
  label = (wxBitmap *) memenv->getPtr(env, argv[1], "label");
  if(!This) throw wxe_badarg("This");
  This->SetBitmap(*label);

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
  if(!This) throw wxe_badarg("This");
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
  int lineSize;
  if(!enif_get_int(env, argv[1], &lineSize)) Badarg("lineSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetLineSize(lineSize);

}

// wxSlider::SetPageSize
void wxSlider_SetPageSize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  int pageSize;
  if(!enif_get_int(env, argv[1], &pageSize)) Badarg("pageSize"); // int
  if(!This) throw wxe_badarg("This");
  This->SetPageSize(pageSize);

}

// wxSlider::SetRange
void wxSlider_SetRange(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  int minValue;
  if(!enif_get_int(env, argv[1], &minValue)) Badarg("minValue"); // int
  int maxValue;
  if(!enif_get_int(env, argv[2], &maxValue)) Badarg("maxValue"); // int
  if(!This) throw wxe_badarg("This");
  This->SetRange(minValue,maxValue);

}

// wxSlider::SetThumbLength
void wxSlider_SetThumbLength(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  int len;
  if(!enif_get_int(env, argv[1], &len)) Badarg("len"); // int
  if(!This) throw wxe_badarg("This");
  This->SetThumbLength(len);

}

// wxSlider::SetValue
void wxSlider_SetValue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxSlider *This;
  This = (wxSlider *) memenv->getPtr(env, argv[0], "This");
  int value;
  if(!enif_get_int(env, argv[1], &value)) Badarg("value"); // int
  if(!This) throw wxe_badarg("This");
  This->SetValue(value);

}

// wxDialog::wxDialog
void wxDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDialog * Result = new EwxDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDialog"));

}

// wxDialog::wxDialog
void wxDialog_new_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_DIALOG_STYLE;
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
  wxDialog * Result = new EwxDialog(parent,id,title,pos,size,style);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDialog"));

}

// wxDialog::Create
void wxDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxPoint pos= wxDefaultPosition;
  wxSize size= wxDefaultSize;
  long style=wxDEFAULT_DIALOG_STYLE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
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

// wxDialog::CreateButtonSizer
void wxDialog_CreateButtonSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  long flags;
  if(!enif_get_long(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  wxSizer * Result = (wxSizer*)This->CreateButtonSizer(flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxSizer"));

}

// wxDialog::CreateStdDialogButtonSizer
void wxDialog_CreateStdDialogButtonSizer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  long flags;
  if(!enif_get_long(env, argv[1], &flags)) Badarg("flags");
  if(!This) throw wxe_badarg("This");
  wxStdDialogButtonSizer * Result = (wxStdDialogButtonSizer*)This->CreateStdDialogButtonSizer(flags);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxStdDialogButtonSizer"));

}

// wxDialog::EndModal
void wxDialog_EndModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  int retCode;
  if(!enif_get_int(env, argv[1], &retCode)) Badarg("retCode"); // int
  if(!This) throw wxe_badarg("This");
  This->EndModal(retCode);

}

// wxDialog::GetAffirmativeId
void wxDialog_GetAffirmativeId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetAffirmativeId();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDialog::GetReturnCode
void wxDialog_GetReturnCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetReturnCode();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxDialog::IsModal
void wxDialog_IsModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsModal();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxDialog::SetAffirmativeId
void wxDialog_SetAffirmativeId(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  int id;
  if(!enif_get_int(env, argv[1], &id)) Badarg("id"); // int
  if(!This) throw wxe_badarg("This");
  This->SetAffirmativeId(id);

}

// wxDialog::SetReturnCode
void wxDialog_SetReturnCode(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  int retCode;
  if(!enif_get_int(env, argv[1], &retCode)) Badarg("retCode"); // int
  if(!This) throw wxe_badarg("This");
  This->SetReturnCode(retCode);

}

// wxDialog::Show
void wxDialog_Show(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool show=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
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

// wxDialog::ShowModal
void wxDialog_ShowModal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDialog *This;
  This = (wxDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->ShowModal();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxColourDialog::wxColourDialog
void wxColourDialog_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColourDialog * Result = new EwxColourDialog();
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxColourDialog"));

}

// wxColourDialog::wxColourDialog
void wxColourDialog_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColourData * data=NULL;
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
  data = (wxColourData *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  wxColourDialog * Result = new EwxColourDialog(parent,data);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxColourDialog"));

}

// wxColourDialog::Create
void wxColourDialog_Create(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColourData * data=NULL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourDialog *This;
  This = (wxColourDialog *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "data"))) {
  data = (wxColourData *) memenv->getPtr(env, tpl[1], "data");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(parent,data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxColourDialog::GetColourData
void wxColourDialog_GetColourData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourDialog *This;
  This = (wxColourDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColourData * Result = &This->GetColourData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxColourData"));

}

// wxColourData::wxColourData
void wxColourData_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColourData * Result = new EwxColourData();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxColourData"));

}

// wxColourData::GetChooseFull
void wxColourData_GetChooseFull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourData *This;
  This = (wxColourData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetChooseFull();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxColourData::GetColour
void wxColourData_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourData *This;
  This = (wxColourData *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour * Result = &This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxColourData::GetCustomColour
void wxColourData_GetCustomColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourData *This;
  This = (wxColourData *) memenv->getPtr(env, argv[0], "This");
  int i;
  if(!enif_get_int(env, argv[1], &i)) Badarg("i"); // int
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetCustomColour(i);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxColourData::SetChooseFull
void wxColourData_SetChooseFull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourData *This;
  This = (wxColourData *) memenv->getPtr(env, argv[0], "This");
  bool flag;
  flag = enif_is_identical(argv[1], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->SetChooseFull(flag);

}

// wxColourData::SetColour
void wxColourData_SetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourData *This;
  This = (wxColourData *) memenv->getPtr(env, argv[0], "This");
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

// wxColourData::SetCustomColour
void wxColourData_SetCustomColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourData *This;
  This = (wxColourData *) memenv->getPtr(env, argv[0], "This");
  int i;
  if(!enif_get_int(env, argv[1], &i)) Badarg("i"); // int
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
  This->SetCustomColour(i,colour);

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

// wxDirDialog::wxDirDialog
void wxDirDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString title= wxDirSelectorPromptStr;
  wxString defaultPath= wxEmptyString;
  long style=wxDD_DEFAULT_STYLE;
  wxPoint pos= wxDefaultPosition;
  wxSize sz= wxDefaultSize;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "title"))) {
  ErlNifBinary title_bin;
  if(!enif_inspect_binary(env, tpl[1], &title_bin)) Badarg("title");
  title = wxString(title_bin.data, wxConvUTF8, title_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultPath"))) {
  ErlNifBinary defaultPath_bin;
  if(!enif_inspect_binary(env, tpl[1], &defaultPath_bin)) Badarg("defaultPath");
  defaultPath = wxString(defaultPath_bin.data, wxConvUTF8, defaultPath_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "sz"))) {
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, tpl[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  sz = wxSize(szW,szH);
    } else        Badarg("Options");
  };
  wxDirDialog * Result = new EwxDirDialog(parent,title,defaultPath,style,pos,sz);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxDirDialog"));

}

// wxDirDialog::GetPath
void wxDirDialog_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirDialog *This;
  This = (wxDirDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDirDialog::GetMessage
void wxDirDialog_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirDialog *This;
  This = (wxDirDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetMessage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxDirDialog::SetMessage
void wxDirDialog_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirDialog *This;
  This = (wxDirDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetMessage(message);

}

// wxDirDialog::SetPath
void wxDirDialog_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDirDialog *This;
  This = (wxDirDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(path);

}

// wxFileDialog::wxFileDialog
void wxFileDialog_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString message= wxFileSelectorPromptStr;
  wxString defaultDir= wxEmptyString;
  wxString defaultFile= wxEmptyString;
  wxString wildCard= wxFileSelectorDefaultWildcardStr;
  long style=wxFD_DEFAULT_STYLE;
  wxPoint pos= wxDefaultPosition;
  wxSize sz= wxDefaultSize;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "message"))) {
  ErlNifBinary message_bin;
  if(!enif_inspect_binary(env, tpl[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultDir"))) {
  ErlNifBinary defaultDir_bin;
  if(!enif_inspect_binary(env, tpl[1], &defaultDir_bin)) Badarg("defaultDir");
  defaultDir = wxString(defaultDir_bin.data, wxConvUTF8, defaultDir_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "defaultFile"))) {
  ErlNifBinary defaultFile_bin;
  if(!enif_inspect_binary(env, tpl[1], &defaultFile_bin)) Badarg("defaultFile");
  defaultFile = wxString(defaultFile_bin.data, wxConvUTF8, defaultFile_bin.size);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "wildCard"))) {
  ErlNifBinary wildCard_bin;
  if(!enif_inspect_binary(env, tpl[1], &wildCard_bin)) Badarg("wildCard");
  wildCard = wxString(wildCard_bin.data, wxConvUTF8, wildCard_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "sz"))) {
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, tpl[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  sz = wxSize(szW,szH);
    } else        Badarg("Options");
  };
  wxFileDialog * Result = new EwxFileDialog(parent,message,defaultDir,defaultFile,wildCard,style,pos,sz);
  app->newPtr((void *) Result, 2, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxFileDialog"));

}

// wxFileDialog::GetDirectory
void wxFileDialog_GetDirectory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetDirectory();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetFilename
void wxFileDialog_GetFilename(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetFilename();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetFilenames
void wxFileDialog_GetFilenames(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayString filenames;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetFilenames(filenames);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(filenames));

}

// wxFileDialog::GetFilterIndex
void wxFileDialog_GetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetFilterIndex();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxFileDialog::GetMessage
void wxFileDialog_GetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetMessage();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetPath
void wxFileDialog_GetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetPath();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::GetPaths
void wxFileDialog_GetPaths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayString paths;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->GetPaths(paths);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(paths));

}

// wxFileDialog::GetWildcard
void wxFileDialog_GetWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetWildcard();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxFileDialog::SetDirectory
void wxFileDialog_SetDirectory(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary directory_bin;
  wxString directory;
  if(!enif_inspect_binary(env, argv[1], &directory_bin)) Badarg("directory");
  directory = wxString(directory_bin.data, wxConvUTF8, directory_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetDirectory(directory);

}

// wxFileDialog::SetFilename
void wxFileDialog_SetFilename(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary setfilename_bin;
  wxString setfilename;
  if(!enif_inspect_binary(env, argv[1], &setfilename_bin)) Badarg("setfilename");
  setfilename = wxString(setfilename_bin.data, wxConvUTF8, setfilename_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetFilename(setfilename);

}

// wxFileDialog::SetFilterIndex
void wxFileDialog_SetFilterIndex(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  int filterIndex;
  if(!enif_get_int(env, argv[1], &filterIndex)) Badarg("filterIndex"); // int
  if(!This) throw wxe_badarg("This");
  This->SetFilterIndex(filterIndex);

}

// wxFileDialog::SetMessage
void wxFileDialog_SetMessage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary message_bin;
  wxString message;
  if(!enif_inspect_binary(env, argv[1], &message_bin)) Badarg("message");
  message = wxString(message_bin.data, wxConvUTF8, message_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetMessage(message);

}

// wxFileDialog::SetPath
void wxFileDialog_SetPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary path_bin;
  wxString path;
  if(!enif_inspect_binary(env, argv[1], &path_bin)) Badarg("path");
  path = wxString(path_bin.data, wxConvUTF8, path_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetPath(path);

}

// wxFileDialog::SetWildcard
void wxFileDialog_SetWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxFileDialog *This;
  This = (wxFileDialog *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary wildCard_bin;
  wxString wildCard;
  if(!enif_inspect_binary(env, argv[1], &wildCard_bin)) Badarg("wildCard");
  wildCard = wxString(wildCard_bin.data, wxConvUTF8, wildCard_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetWildcard(wildCard);

}

