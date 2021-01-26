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

// wxCalendarEvent::GetWeekDay
void wxCalendarEvent_GetWeekDay(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarEvent *This;
  This = (wxCalendarEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWeekDay();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxCalendarEvent::GetDate
void wxCalendarEvent_GetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCalendarEvent *This;
  This = (wxCalendarEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxDateTime * Result = &This->GetDate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

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

// wxColourPickerEvent::GetColour
void wxColourPickerEvent_GetColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxColourPickerEvent *This;
  This = (wxColourPickerEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

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

// wxContextMenuEvent::GetPosition
void wxContextMenuEvent_GetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxContextMenuEvent *This;
  This = (wxContextMenuEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxPoint * Result = &This->GetPosition();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

}

// wxContextMenuEvent::SetPosition
void wxContextMenuEvent_SetPosition(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxContextMenuEvent *This;
  This = (wxContextMenuEvent *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *point_t;
  int point_sz;
  if(!enif_get_tuple(env, argv[1], &point_sz, &point_t)) Badarg("point");
  int pointX;
  if(!enif_get_int(env, point_t[0], &pointX)) Badarg("point");
  int pointY;
  if(!enif_get_int(env, point_t[1], &pointY)) Badarg("point");
  wxPoint point = wxPoint(pointX,pointY);
  if(!This) throw wxe_badarg("This");
  This->SetPosition(point);

}

// wxControl::GetLabel
void wxControl_GetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControl *This;
  This = (wxControl *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetLabel();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxControl::SetLabel
void wxControl_SetLabel(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControl *This;
  This = (wxControl *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary label_bin;
  wxString label;
  if(!enif_inspect_binary(env, argv[1], &label_bin)) Badarg("label");
  label = wxString(label_bin.data, wxConvUTF8, label_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetLabel(label);

}

// wxControlWithItems::Append
void wxControlWithItems_Append_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[1], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  if(!This) throw wxe_badarg("This");
  int Result = This->Append(item);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Append
void wxControlWithItems_Append_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[1], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  wxeErlTerm * clientData;
  clientData = new wxeErlTerm(argv[2]);
  if(!This) throw wxe_badarg("This");
  int Result = This->Append(item,clientData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Append
void wxControlWithItems_appendStrings_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
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
  int Result = This->Append(items);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Append
void wxControlWithItems_appendStrings_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM itemsHead, itemsTail;
  ErlNifBinary items_bin;
  wxArrayString items;
  itemsTail = argv[1];
  while(!enif_is_empty_list(env, itemsTail)) {
    if(!enif_get_list_cell(env, itemsTail, &itemsHead, &itemsTail)) Badarg("items");
    if(!enif_inspect_binary(env, itemsHead, &items_bin)) Badarg("items");
    items.Add(wxString(items_bin.data, wxConvUTF8, items_bin.size));
  };
  unsigned int clientsDataLen;
  if(!enif_get_list_length(env, argv[2], &clientsDataLen)) Badarg("clientsData");
  std::vector <wxeErlTerm *> clientsData;
  ERL_NIF_TERM clientsDataHead, clientsDataTail;
  clientsDataTail = argv[2];
  while(!enif_is_empty_list(env, clientsDataTail)) {
    if(!enif_get_list_cell(env, clientsDataTail, &clientsDataHead, &clientsDataTail)) Badarg("clientsData");
    clientsData.push_back(new wxeErlTerm(argv[2]));
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->Append(items,(wxClientData **) clientsData.data());
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Clear
void wxControlWithItems_Clear(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Clear();

}

// wxControlWithItems::Delete
void wxControlWithItems_Delete(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  This->Delete(n);

}

// wxControlWithItems::FindString
void wxControlWithItems_FindString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool bCase=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "bCase"))) {
  bCase = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->FindString(string,bCase);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::GetClientObject
void wxControlWithItems_getClientData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  wxeErlTerm * Result = (wxeErlTerm*)This->GetClientObject(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ext2term(Result));

}

// wxControlWithItems::SetClientObject
void wxControlWithItems_setClientData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  wxeErlTerm * data;
  data = new wxeErlTerm(argv[2]);
  if(!This) throw wxe_badarg("This");
  This->SetClientObject(n,data);

}

// wxControlWithItems::GetCount
void wxControlWithItems_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxControlWithItems::GetSelection
void wxControlWithItems_GetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::GetString
void wxControlWithItems_GetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetString(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxControlWithItems::GetStringSelection
void wxControlWithItems_GetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetStringSelection();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxControlWithItems::Insert
void wxControlWithItems_Insert_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[1], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  unsigned int pos;
  if(!enif_get_uint(env, argv[2], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  int Result = This->Insert(item,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Insert
void wxControlWithItems_Insert_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary item_bin;
  wxString item;
  if(!enif_inspect_binary(env, argv[1], &item_bin)) Badarg("item");
  item = wxString(item_bin.data, wxConvUTF8, item_bin.size);
  unsigned int pos;
  if(!enif_get_uint(env, argv[2], &pos)) Badarg("pos");
  wxeErlTerm * clientData;
  clientData = new wxeErlTerm(argv[3]);
  if(!This) throw wxe_badarg("This");
  int Result = This->Insert(item,pos,clientData);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Insert
void wxControlWithItems_insertStrings_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
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
  int Result = This->Insert(items,pos);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::Insert
void wxControlWithItems_insertStrings_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
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
  unsigned int clientsDataLen;
  if(!enif_get_list_length(env, argv[3], &clientsDataLen)) Badarg("clientsData");
  std::vector <wxeErlTerm *> clientsData;
  ERL_NIF_TERM clientsDataHead, clientsDataTail;
  clientsDataTail = argv[3];
  while(!enif_is_empty_list(env, clientsDataTail)) {
    if(!enif_get_list_cell(env, clientsDataTail, &clientsDataHead, &clientsDataTail)) Badarg("clientsData");
    clientsData.push_back(new wxeErlTerm(argv[3]));
  };
  if(!This) throw wxe_badarg("This");
  int Result = This->Insert(items,pos,(wxClientData **) clientsData.data());
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxControlWithItems::IsEmpty
void wxControlWithItems_IsEmpty(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEmpty();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxControlWithItems::Select
void wxControlWithItems_Select(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->Select(n);

}

// wxControlWithItems::SetSelection
void wxControlWithItems_SetSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  int n;
  if(!enif_get_int(env, argv[1], &n)) Badarg("n"); // int
  if(!This) throw wxe_badarg("This");
  This->SetSelection(n);

}

// wxControlWithItems::SetString
void wxControlWithItems_SetString(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[2], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetString(n,string);

}

// wxControlWithItems::SetStringSelection
void wxControlWithItems_SetStringSelection(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxControlWithItems *This;
  This = (wxControlWithItems *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary string_bin;
  wxString string;
  if(!enif_inspect_binary(env, argv[1], &string_bin)) Badarg("string");
  string = wxString(string_bin.data, wxConvUTF8, string_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetStringSelection(string);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxCursor::wxCursor
void wxCursor_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCursor * Result = new EwxCursor();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCursor"));

}

// wxCursor::wxCursor
void wxCursor_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxCURSOR_DEFAULT_TYPE;
  int hotSpotX=0;
  int hotSpotY=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary cursorName_bin;
  wxString cursorName;
  if(!enif_inspect_binary(env, argv[0], &cursorName_bin)) Badarg("cursorName");
  cursorName = wxString(cursorName_bin.data, wxConvUTF8, cursorName_bin.size);
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "hotSpotX"))) {
  if(!enif_get_int(env, tpl[1], &hotSpotX)) Badarg("hotSpotX"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "hotSpotY"))) {
  if(!enif_get_int(env, tpl[1], &hotSpotY)) Badarg("hotSpotY"); // int
    } else        Badarg("Options");
  };
  wxCursor * Result = new EwxCursor(cursorName,type,hotSpotX,hotSpotY);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCursor"));

}

// wxCursor::wxCursor
void wxCursor_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStockCursor cursorId;
  if(!enif_get_int(env, argv[0], (int *) &cursorId)) Badarg("cursorId"); // enum
  wxCursor * Result = new EwxCursor(cursorId);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCursor"));

}

// wxCursor::wxCursor
void wxCursor_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM image_type;
  void * image = memenv->getPtr(env, argv[0], "image", &image_type);
  wxCursor * Result;
  if(enif_is_identical(image_type, WXE_ATOM_wxImage))
    Result = new EwxCursor(* static_cast<wxImage*> (image));
  else if(enif_is_identical(image_type, WXE_ATOM_wxCursor))
    Result = new EwxCursor(* static_cast<wxCursor*> (image));
  else throw wxe_badarg("image");
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxCursor"));

}

// wxCursor::IsOk
void wxCursor_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxCursor *This;
  This = (wxCursor *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

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

// wxDateEvent::GetDate
void wxDateEvent_GetDate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxDateEvent *This;
  This = (wxDateEvent *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxDateTime * Result = &This->GetDate();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make((*Result)));

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

