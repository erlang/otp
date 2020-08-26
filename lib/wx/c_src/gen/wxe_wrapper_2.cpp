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

#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsObject::GetRenderer
void wxGraphicsObject_GetRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsObject *This;
  This = (wxGraphicsObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsRenderer * Result = (wxGraphicsRenderer*)This->GetRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsRenderer"));

}

// wxGraphicsObject::IsNull
void wxGraphicsObject_IsNull(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsObject *This;
  This = (wxGraphicsObject *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsNull();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsContext::Create
void wxGraphicsContext_Create_STAT_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM windowDC_type;
  void * windowDC = memenv->getPtr(env, argv[0], "windowDC", &windowDC_type);
  wxGraphicsContext * Result;
    if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindowDC))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(* static_cast<wxWindowDC*> (windowDC));
  else   if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindow))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(static_cast<wxWindow*> (windowDC));
  else   if(enif_is_identical(windowDC_type, WXE_ATOM_wxMemoryDC))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(* static_cast<wxMemoryDC*> (windowDC));
  else   if(enif_is_identical(windowDC_type, WXE_ATOM_wxImage))
    Result =  (wxGraphicsContext*)wxGraphicsContext::Create(* static_cast<wxImage*> (windowDC));
  else throw wxe_badarg("windowDC");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGraphicsContext::Create
void wxGraphicsContext_Create_STAT_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGraphicsContext * Result = (wxGraphicsContext*)wxGraphicsContext::Create();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGraphicsContext::CreatePen
void wxGraphicsContext_CreatePen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxPen *pen;
  pen = (wxPen *) memenv->getPtr(env, argv[1], "pen");
  if(!This) throw wxe_badarg("This");
  wxGraphicsPen * Result = new wxGraphicsPen(This->CreatePen(*pen)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsPen"));

}

// wxGraphicsContext::CreateBrush
void wxGraphicsContext_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxBrush *brush;
  brush = (wxBrush *) memenv->getPtr(env, argv[1], "brush");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateBrush(*brush)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateRadialGradientBrush
void wxGraphicsContext_CreateRadialGradientBrush_7(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double startX;
  if(!wxe_get_double(env, argv[1], &startX)) Badarg("startX");
  double startY;
  if(!wxe_get_double(env, argv[2], &startY)) Badarg("startY");
  double endX;
  if(!wxe_get_double(env, argv[3], &endX)) Badarg("endX");
  double endY;
  if(!wxe_get_double(env, argv[4], &endY)) Badarg("endY");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  const ERL_NIF_TERM *oColor_t;
  int oColor_sz;
  if(!enif_get_tuple(env, argv[6], &oColor_sz, &oColor_t)) Badarg("oColor");
  int oColorR;
  if(!enif_get_int(env, oColor_t[0], &oColorR)) Badarg("oColor");
  int oColorG;
  if(!enif_get_int(env, oColor_t[1], &oColorG)) Badarg("oColor");
  int oColorB;
  if(!enif_get_int(env, oColor_t[2], &oColorB)) Badarg("oColor");
  int oColorA;
  if(!enif_get_int(env, oColor_t[3], &oColorA)) Badarg("oColor");
  wxColour oColor = wxColour(oColorR,oColorG,oColorB,oColorA);
  const ERL_NIF_TERM *cColor_t;
  int cColor_sz;
  if(!enif_get_tuple(env, argv[7], &cColor_sz, &cColor_t)) Badarg("cColor");
  int cColorR;
  if(!enif_get_int(env, cColor_t[0], &cColorR)) Badarg("cColor");
  int cColorG;
  if(!enif_get_int(env, cColor_t[1], &cColorG)) Badarg("cColor");
  int cColorB;
  if(!enif_get_int(env, cColor_t[2], &cColorB)) Badarg("cColor");
  int cColorA;
  if(!enif_get_int(env, cColor_t[3], &cColorA)) Badarg("cColor");
  wxColour cColor = wxColour(cColorR,cColorG,cColorB,cColorA);
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateRadialGradientBrush(startX,startY,endX,endY,radius,oColor,cColor)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateRadialGradientBrush
void wxGraphicsContext_CreateRadialGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double startX;
  if(!wxe_get_double(env, argv[1], &startX)) Badarg("startX");
  double startY;
  if(!wxe_get_double(env, argv[2], &startY)) Badarg("startY");
  double endX;
  if(!wxe_get_double(env, argv[3], &endX)) Badarg("endX");
  double endY;
  if(!wxe_get_double(env, argv[4], &endY)) Badarg("endY");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[6], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateRadialGradientBrush(startX,startY,endX,endY,radius,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateLinearGradientBrush
void wxGraphicsContext_CreateLinearGradientBrush_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  const ERL_NIF_TERM *c1_t;
  int c1_sz;
  if(!enif_get_tuple(env, argv[5], &c1_sz, &c1_t)) Badarg("c1");
  int c1R;
  if(!enif_get_int(env, c1_t[0], &c1R)) Badarg("c1");
  int c1G;
  if(!enif_get_int(env, c1_t[1], &c1G)) Badarg("c1");
  int c1B;
  if(!enif_get_int(env, c1_t[2], &c1B)) Badarg("c1");
  int c1A;
  if(!enif_get_int(env, c1_t[3], &c1A)) Badarg("c1");
  wxColour c1 = wxColour(c1R,c1G,c1B,c1A);
  const ERL_NIF_TERM *c2_t;
  int c2_sz;
  if(!enif_get_tuple(env, argv[6], &c2_sz, &c2_t)) Badarg("c2");
  int c2R;
  if(!enif_get_int(env, c2_t[0], &c2R)) Badarg("c2");
  int c2G;
  if(!enif_get_int(env, c2_t[1], &c2G)) Badarg("c2");
  int c2B;
  if(!enif_get_int(env, c2_t[2], &c2B)) Badarg("c2");
  int c2A;
  if(!enif_get_int(env, c2_t[3], &c2A)) Badarg("c2");
  wxColour c2 = wxColour(c2R,c2G,c2B,c2A);
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateLinearGradientBrush(x1,y1,x2,y2,c1,c2)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateLinearGradientBrush
void wxGraphicsContext_CreateLinearGradientBrush_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[5], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateLinearGradientBrush(x1,y1,x2,y2,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsContext::CreateFont
void wxGraphicsContext_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(*font,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsContext::CreateFont
void wxGraphicsContext_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxFONTFLAG_DEFAULT;
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double sizeInPixels;
  if(!wxe_get_double(env, argv[1], &sizeInPixels)) Badarg("sizeInPixels");
  ErlNifBinary facename_bin;
  wxString facename;
  if(!enif_inspect_binary(env, argv[2], &facename_bin)) Badarg("facename");
  facename = wxString(facename_bin.data, wxConvUTF8, facename_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(sizeInPixels,facename,flags,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsContext::CreateMatrix
void wxGraphicsContext_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a=1.0;
  wxDouble b=0.0;
  wxDouble c=0.0;
  wxDouble d=1.0;
  wxDouble tx=0.0;
  wxDouble ty=0.0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "a"))) {
  if(!wxe_get_double(env, tpl[1], &a)) Badarg("a");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!wxe_get_double(env, tpl[1], &b)) Badarg("b");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "c"))) {
  if(!wxe_get_double(env, tpl[1], &c)) Badarg("c");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "d"))) {
  if(!wxe_get_double(env, tpl[1], &d)) Badarg("d");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "tx"))) {
  if(!wxe_get_double(env, tpl[1], &tx)) Badarg("tx");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "ty"))) {
  if(!wxe_get_double(env, tpl[1], &ty)) Badarg("ty");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsMatrix * Result = new wxGraphicsMatrix(This->CreateMatrix(a,b,c,d,tx,ty)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsMatrix"));

}

// wxGraphicsContext::CreatePath
void wxGraphicsContext_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsPath * Result = new wxGraphicsPath(This->CreatePath()); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsPath"));

}

// wxGraphicsContext::Clip
void wxGraphicsContext_Clip_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxRegion *region;
  region = (wxRegion *) memenv->getPtr(env, argv[1], "region");
  if(!This) throw wxe_badarg("This");
  This->Clip(*region);

}

// wxGraphicsContext::Clip
void wxGraphicsContext_Clip_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->Clip(x,y,w,h);

}

// wxGraphicsContext::ResetClip
void wxGraphicsContext_ResetClip(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->ResetClip();

}

// wxGraphicsContext::DrawBitmap
void wxGraphicsContext_DrawBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[4], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[5], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawBitmap(*bmp,x,y,w,h);

}

// wxGraphicsContext::DrawEllipse
void wxGraphicsContext_DrawEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawEllipse(x,y,w,h);

}

// wxGraphicsContext::DrawIcon
void wxGraphicsContext_DrawIcon(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[4], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[5], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawIcon(*icon,x,y,w,h);

}

// wxGraphicsContext::DrawLines
void wxGraphicsContext_DrawLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  unsigned pointsLen;
  ERL_NIF_TERM pointsHead, pointsTail;
  const ERL_NIF_TERM *points_tpl;
  int points_tsz;
  if(!enif_get_list_length(env, argv[1], &pointsLen)) Badarg("points");
  std::vector <wxPoint2DDouble> points;
  double x, y;
  pointsTail = argv[1];
  while(!enif_is_empty_list(env, pointsTail)) {
    if(!enif_get_list_cell(env, pointsTail, &pointsHead, &pointsTail)) Badarg("points");
    if(!enif_get_tuple(env, pointsHead, &points_tsz, &points_tpl) || points_tsz != 2) Badarg("points");
    if(!wxe_get_double(env, points_tpl[0], &x)) Badarg("points");
    if(!wxe_get_double(env, points_tpl[1], &y)) Badarg("points");
    points.push_back(wxPoint2DDouble(x,y));
  };
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawLines(pointsLen,points.data(),fillStyle);

}

// wxGraphicsContext::DrawPath
void wxGraphicsContext_DrawPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->DrawPath(*path,fillStyle);

}

// wxGraphicsContext::DrawRectangle
void wxGraphicsContext_DrawRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->DrawRectangle(x,y,w,h);

}

// wxGraphicsContext::DrawRoundedRectangle
void wxGraphicsContext_DrawRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  if(!This) throw wxe_badarg("This");
  This->DrawRoundedRectangle(x,y,w,h,radius);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double angle;
  if(!wxe_get_double(env, argv[4], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y,angle);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  wxGraphicsBrush *backgroundBrush;
  backgroundBrush = (wxGraphicsBrush *) memenv->getPtr(env, argv[4], "backgroundBrush");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y,*backgroundBrush);

}

// wxGraphicsContext::DrawText
void wxGraphicsContext_DrawText_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary str_bin;
  wxString str;
  if(!enif_inspect_binary(env, argv[1], &str_bin)) Badarg("str");
  str = wxString(str_bin.data, wxConvUTF8, str_bin.size);
  double x;
  if(!wxe_get_double(env, argv[2], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[3], &y)) Badarg("y");
  double angle;
  if(!wxe_get_double(env, argv[4], &angle)) Badarg("angle");
  wxGraphicsBrush *backgroundBrush;
  backgroundBrush = (wxGraphicsBrush *) memenv->getPtr(env, argv[5], "backgroundBrush");
  if(!This) throw wxe_badarg("This");
  This->DrawText(str,x,y,angle,*backgroundBrush);

}

// wxGraphicsContext::FillPath
void wxGraphicsContext_FillPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->FillPath(*path,fillStyle);

}

// wxGraphicsContext::StrokePath
void wxGraphicsContext_StrokePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  if(!This) throw wxe_badarg("This");
  This->StrokePath(*path);

}

// wxGraphicsContext::GetPartialTextExtents
void wxGraphicsContext_GetPartialTextExtents(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxArrayDouble widths;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->GetPartialTextExtents(text,widths);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(widths));

}

// wxGraphicsContext::GetTextExtent
void wxGraphicsContext_GetTextExtent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble width;
  wxDouble height;
  wxDouble descent;
  wxDouble externalLeading;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary text_bin;
  wxString text;
  if(!enif_inspect_binary(env, argv[1], &text_bin)) Badarg("text");
  text = wxString(text_bin.data, wxConvUTF8, text_bin.size);
  if(!This) throw wxe_badarg("This");
  This->GetTextExtent(text,&width,&height,&descent,&externalLeading);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_double(width),
  rt.make_double(height),
  rt.make_double(descent),
  rt.make_double(externalLeading));
  rt.send(msg);

}

// wxGraphicsContext::Rotate
void wxGraphicsContext_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->Rotate(angle);

}

// wxGraphicsContext::Scale
void wxGraphicsContext_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double xScale;
  if(!wxe_get_double(env, argv[1], &xScale)) Badarg("xScale");
  double yScale;
  if(!wxe_get_double(env, argv[2], &yScale)) Badarg("yScale");
  if(!This) throw wxe_badarg("This");
  This->Scale(xScale,yScale);

}

// wxGraphicsContext::Translate
void wxGraphicsContext_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double dx;
  if(!wxe_get_double(env, argv[1], &dx)) Badarg("dx");
  double dy;
  if(!wxe_get_double(env, argv[2], &dy)) Badarg("dy");
  if(!This) throw wxe_badarg("This");
  This->Translate(dx,dy);

}

// wxGraphicsContext::GetTransform
void wxGraphicsContext_GetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsMatrix * Result = new wxGraphicsMatrix(This->GetTransform()); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsMatrix"));

}

// wxGraphicsContext::SetTransform
void wxGraphicsContext_SetTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *matrix;
  matrix = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "matrix");
  if(!This) throw wxe_badarg("This");
  This->SetTransform(*matrix);

}

// wxGraphicsContext::ConcatTransform
void wxGraphicsContext_ConcatTransform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *matrix;
  matrix = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "matrix");
  if(!This) throw wxe_badarg("This");
  This->ConcatTransform(*matrix);

}

// wxGraphicsContext::SetBrush
void wxGraphicsContext_SetBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM brush_type;
  void * brush = memenv->getPtr(env, argv[1], "brush", &brush_type);
  if(!This) throw wxe_badarg("This");
  if(enif_is_identical(brush_type, WXE_ATOM_wxGraphicsBrush))
   This->SetBrush(* static_cast<wxGraphicsBrush*> (brush));
  else if(enif_is_identical(brush_type, WXE_ATOM_wxBrush))
   This->SetBrush(* static_cast<wxBrush*> (brush));
  else throw wxe_badarg("brush");

}

// wxGraphicsContext::SetFont
void wxGraphicsContext_SetFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxFont *font;
  font = (wxFont *) memenv->getPtr(env, argv[1], "font");
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
  This->SetFont(*font,colour);

}

// wxGraphicsContext::SetFont
void wxGraphicsContext_SetFont_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsFont *font;
  font = (wxGraphicsFont *) memenv->getPtr(env, argv[1], "font");
  if(!This) throw wxe_badarg("This");
  This->SetFont(*font);

}

// wxGraphicsContext::SetPen
void wxGraphicsContext_SetPen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM pen_type;
  void * pen = memenv->getPtr(env, argv[1], "pen", &pen_type);
  if(!This) throw wxe_badarg("This");
  if(enif_is_identical(pen_type, WXE_ATOM_wxPen))
   This->SetPen(* static_cast<wxPen*> (pen));
  else if(enif_is_identical(pen_type, WXE_ATOM_wxGraphicsPen))
   This->SetPen(* static_cast<wxGraphicsPen*> (pen));
  else throw wxe_badarg("pen");

}

// wxGraphicsContext::StrokeLine
void wxGraphicsContext_StrokeLine(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  if(!This) throw wxe_badarg("This");
  This->StrokeLine(x1,y1,x2,y2);

}

// wxGraphicsContext::StrokeLines
void wxGraphicsContext_StrokeLines(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsContext *This;
  This = (wxGraphicsContext *) memenv->getPtr(env, argv[0], "This");
  unsigned pointsLen;
  ERL_NIF_TERM pointsHead, pointsTail;
  const ERL_NIF_TERM *points_tpl;
  int points_tsz;
  if(!enif_get_list_length(env, argv[1], &pointsLen)) Badarg("points");
  std::vector <wxPoint2DDouble> points;
  double x, y;
  pointsTail = argv[1];
  while(!enif_is_empty_list(env, pointsTail)) {
    if(!enif_get_list_cell(env, pointsTail, &pointsHead, &pointsTail)) Badarg("points");
    if(!enif_get_tuple(env, pointsHead, &points_tsz, &points_tpl) || points_tsz != 2) Badarg("points");
    if(!wxe_get_double(env, points_tpl[0], &x)) Badarg("points");
    if(!wxe_get_double(env, points_tpl[1], &y)) Badarg("points");
    points.push_back(wxPoint2DDouble(x,y));
  };
  if(!This) throw wxe_badarg("This");
  This->StrokeLines(pointsLen,points.data());

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsMatrix::Concat
void wxGraphicsMatrix_Concat(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *t;
  t = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "t");
  if(!This) throw wxe_badarg("This");
  This->Concat(t);

}

// wxGraphicsMatrix::Get
void wxGraphicsMatrix_Get(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a;
  wxDouble b;
  wxDouble c;
  wxDouble d;
  wxDouble tx;
  wxDouble ty;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Get(&a,&b,&c,&d,&tx,&ty);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple6(rt.env,
  rt.make_double(a),
  rt.make_double(b),
  rt.make_double(c),
  rt.make_double(d),
  rt.make_double(tx),
  rt.make_double(ty));
  rt.send(msg);

}

// wxGraphicsMatrix::Invert
void wxGraphicsMatrix_Invert(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Invert();

}

// wxGraphicsMatrix::IsEqual
void wxGraphicsMatrix_IsEqual(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *t;
  t = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "t");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsEqual(t);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsMatrix::IsIdentity
void wxGraphicsMatrix_IsIdentity(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsIdentity();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsMatrix::Rotate
void wxGraphicsMatrix_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->Rotate(angle);

}

// wxGraphicsMatrix::Scale
void wxGraphicsMatrix_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  double xScale;
  if(!wxe_get_double(env, argv[1], &xScale)) Badarg("xScale");
  double yScale;
  if(!wxe_get_double(env, argv[2], &yScale)) Badarg("yScale");
  if(!This) throw wxe_badarg("This");
  This->Scale(xScale,yScale);

}

// wxGraphicsMatrix::Translate
void wxGraphicsMatrix_Translate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  double dx;
  if(!wxe_get_double(env, argv[1], &dx)) Badarg("dx");
  double dy;
  if(!wxe_get_double(env, argv[2], &dy)) Badarg("dy");
  if(!This) throw wxe_badarg("This");
  This->Translate(dx,dy);

}

// wxGraphicsMatrix::Set
void wxGraphicsMatrix_Set(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a=1.0;
  wxDouble b=0.0;
  wxDouble c=0.0;
  wxDouble d=1.0;
  wxDouble tx=0.0;
  wxDouble ty=0.0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "a"))) {
  if(!wxe_get_double(env, tpl[1], &a)) Badarg("a");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!wxe_get_double(env, tpl[1], &b)) Badarg("b");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "c"))) {
  if(!wxe_get_double(env, tpl[1], &c)) Badarg("c");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "d"))) {
  if(!wxe_get_double(env, tpl[1], &d)) Badarg("d");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "tx"))) {
  if(!wxe_get_double(env, tpl[1], &tx)) Badarg("tx");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "ty"))) {
  if(!wxe_get_double(env, tpl[1], &ty)) Badarg("ty");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->Set(a,b,c,d,tx,ty);

}

// wxGraphicsMatrix::TransformPoint
void wxGraphicsMatrix_TransformPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble x;
  wxDouble y;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->TransformPoint(&x,&y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_double(x),
  rt.make_double(y));
  rt.send(msg);

}

// wxGraphicsMatrix::TransformDistance
void wxGraphicsMatrix_TransformDistance(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble dx;
  wxDouble dy;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsMatrix *This;
  This = (wxGraphicsMatrix *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->TransformDistance(&dx,&dy);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple2(rt.env,
  rt.make_double(dx),
  rt.make_double(dy));
  rt.send(msg);

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsPath::MoveToPoint
void wxGraphicsPath_MoveToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->MoveToPoint(x,y);

}

// wxGraphicsPath::MoveToPoint
void wxGraphicsPath_MoveToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *p_t;
  int p_sz;
  if(!enif_get_tuple(env, argv[1], &p_sz, &p_t)) Badarg("p");
  double pX;
  if(!wxe_get_double(env, p_t[0], &pX)) Badarg("p");
  double pY;
  if(!wxe_get_double(env, p_t[1], &pY)) Badarg("p");
  wxPoint2DDouble p = wxPoint2DDouble(pX,pY);
  if(!This) throw wxe_badarg("This");
  This->MoveToPoint(p);

}

// wxGraphicsPath::AddArc
void wxGraphicsPath_AddArc_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double r;
  if(!wxe_get_double(env, argv[3], &r)) Badarg("r");
  double startAngle;
  if(!wxe_get_double(env, argv[4], &startAngle)) Badarg("startAngle");
  double endAngle;
  if(!wxe_get_double(env, argv[5], &endAngle)) Badarg("endAngle");
  bool clockwise;
  clockwise = enif_is_identical(argv[6], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->AddArc(x,y,r,startAngle,endAngle,clockwise);

}

// wxGraphicsPath::AddArc
void wxGraphicsPath_AddArc_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  double cX;
  if(!wxe_get_double(env, c_t[0], &cX)) Badarg("c");
  double cY;
  if(!wxe_get_double(env, c_t[1], &cY)) Badarg("c");
  wxPoint2DDouble c = wxPoint2DDouble(cX,cY);
  double r;
  if(!wxe_get_double(env, argv[2], &r)) Badarg("r");
  double startAngle;
  if(!wxe_get_double(env, argv[3], &startAngle)) Badarg("startAngle");
  double endAngle;
  if(!wxe_get_double(env, argv[4], &endAngle)) Badarg("endAngle");
  bool clockwise;
  clockwise = enif_is_identical(argv[5], WXE_ATOM_true);
  if(!This) throw wxe_badarg("This");
  This->AddArc(c,r,startAngle,endAngle,clockwise);

}

// wxGraphicsPath::AddArcToPoint
void wxGraphicsPath_AddArcToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  double r;
  if(!wxe_get_double(env, argv[5], &r)) Badarg("r");
  if(!This) throw wxe_badarg("This");
  This->AddArcToPoint(x1,y1,x2,y2,r);

}

// wxGraphicsPath::AddCircle
void wxGraphicsPath_AddCircle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double r;
  if(!wxe_get_double(env, argv[3], &r)) Badarg("r");
  if(!This) throw wxe_badarg("This");
  This->AddCircle(x,y,r);

}

// wxGraphicsPath::AddCurveToPoint
void wxGraphicsPath_AddCurveToPoint_6(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double cx1;
  if(!wxe_get_double(env, argv[1], &cx1)) Badarg("cx1");
  double cy1;
  if(!wxe_get_double(env, argv[2], &cy1)) Badarg("cy1");
  double cx2;
  if(!wxe_get_double(env, argv[3], &cx2)) Badarg("cx2");
  double cy2;
  if(!wxe_get_double(env, argv[4], &cy2)) Badarg("cy2");
  double x;
  if(!wxe_get_double(env, argv[5], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[6], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->AddCurveToPoint(cx1,cy1,cx2,cy2,x,y);

}

// wxGraphicsPath::AddCurveToPoint
void wxGraphicsPath_AddCurveToPoint_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c1_t;
  int c1_sz;
  if(!enif_get_tuple(env, argv[1], &c1_sz, &c1_t)) Badarg("c1");
  double c1X;
  if(!wxe_get_double(env, c1_t[0], &c1X)) Badarg("c1");
  double c1Y;
  if(!wxe_get_double(env, c1_t[1], &c1Y)) Badarg("c1");
  wxPoint2DDouble c1 = wxPoint2DDouble(c1X,c1Y);
  const ERL_NIF_TERM *c2_t;
  int c2_sz;
  if(!enif_get_tuple(env, argv[2], &c2_sz, &c2_t)) Badarg("c2");
  double c2X;
  if(!wxe_get_double(env, c2_t[0], &c2X)) Badarg("c2");
  double c2Y;
  if(!wxe_get_double(env, c2_t[1], &c2Y)) Badarg("c2");
  wxPoint2DDouble c2 = wxPoint2DDouble(c2X,c2Y);
  const ERL_NIF_TERM *e_t;
  int e_sz;
  if(!enif_get_tuple(env, argv[3], &e_sz, &e_t)) Badarg("e");
  double eX;
  if(!wxe_get_double(env, e_t[0], &eX)) Badarg("e");
  double eY;
  if(!wxe_get_double(env, e_t[1], &eY)) Badarg("e");
  wxPoint2DDouble e = wxPoint2DDouble(eX,eY);
  if(!This) throw wxe_badarg("This");
  This->AddCurveToPoint(c1,c2,e);

}

// wxGraphicsPath::AddEllipse
void wxGraphicsPath_AddEllipse(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->AddEllipse(x,y,w,h);

}

// wxGraphicsPath::AddLineToPoint
void wxGraphicsPath_AddLineToPoint_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->AddLineToPoint(x,y);

}

// wxGraphicsPath::AddLineToPoint
void wxGraphicsPath_AddLineToPoint_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *p_t;
  int p_sz;
  if(!enif_get_tuple(env, argv[1], &p_sz, &p_t)) Badarg("p");
  double pX;
  if(!wxe_get_double(env, p_t[0], &pX)) Badarg("p");
  double pY;
  if(!wxe_get_double(env, p_t[1], &pY)) Badarg("p");
  wxPoint2DDouble p = wxPoint2DDouble(pX,pY);
  if(!This) throw wxe_badarg("This");
  This->AddLineToPoint(p);

}

// wxGraphicsPath::AddPath
void wxGraphicsPath_AddPath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsPath *path;
  path = (wxGraphicsPath *) memenv->getPtr(env, argv[1], "path");
  if(!This) throw wxe_badarg("This");
  This->AddPath(*path);

}

// wxGraphicsPath::AddQuadCurveToPoint
void wxGraphicsPath_AddQuadCurveToPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double cx;
  if(!wxe_get_double(env, argv[1], &cx)) Badarg("cx");
  double cy;
  if(!wxe_get_double(env, argv[2], &cy)) Badarg("cy");
  double x;
  if(!wxe_get_double(env, argv[3], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[4], &y)) Badarg("y");
  if(!This) throw wxe_badarg("This");
  This->AddQuadCurveToPoint(cx,cy,x,y);

}

// wxGraphicsPath::AddRectangle
void wxGraphicsPath_AddRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  if(!This) throw wxe_badarg("This");
  This->AddRectangle(x,y,w,h);

}

// wxGraphicsPath::AddRoundedRectangle
void wxGraphicsPath_AddRoundedRectangle(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  double w;
  if(!wxe_get_double(env, argv[3], &w)) Badarg("w");
  double h;
  if(!wxe_get_double(env, argv[4], &h)) Badarg("h");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  if(!This) throw wxe_badarg("This");
  This->AddRoundedRectangle(x,y,w,h,radius);

}

// wxGraphicsPath::CloseSubpath
void wxGraphicsPath_CloseSubpath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->CloseSubpath();

}

// wxGraphicsPath::Contains
void wxGraphicsPath_Contains_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *c_t;
  int c_sz;
  if(!enif_get_tuple(env, argv[1], &c_sz, &c_t)) Badarg("c");
  double cX;
  if(!wxe_get_double(env, c_t[0], &cX)) Badarg("c");
  double cY;
  if(!wxe_get_double(env, c_t[1], &cY)) Badarg("c");
  wxPoint2DDouble c = wxPoint2DDouble(cX,cY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Contains(c,fillStyle);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsPath::Contains
void wxGraphicsPath_Contains_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxPolygonFillMode fillStyle=wxODDEVEN_RULE;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  double x;
  if(!wxe_get_double(env, argv[1], &x)) Badarg("x");
  double y;
  if(!wxe_get_double(env, argv[2], &y)) Badarg("y");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "fillStyle"))) {
  if(!enif_get_int(env, tpl[1], (int *) &fillStyle)) Badarg("fillStyle"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Contains(x,y,fillStyle);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxGraphicsPath::GetBox
void wxGraphicsPath_GetBox(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxRect2DDouble Result = This->GetBox();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsPath::GetCurrentPoint
void wxGraphicsPath_GetCurrentPoint(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxPoint2DDouble Result = This->GetCurrentPoint();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsPath::Transform
void wxGraphicsPath_Transform(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsPath *This;
  This = (wxGraphicsPath *) memenv->getPtr(env, argv[0], "This");
  wxGraphicsMatrix *matrix;
  matrix = (wxGraphicsMatrix *) memenv->getPtr(env, argv[1], "matrix");
  if(!This) throw wxe_badarg("This");
  This->Transform(*matrix);

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsRenderer::GetDefaultRenderer
void wxGraphicsRenderer_GetDefaultRenderer(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxGraphicsRenderer * Result = (wxGraphicsRenderer*)wxGraphicsRenderer::GetDefaultRenderer();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsRenderer"));

}

// wxGraphicsRenderer::CreateContext
void wxGraphicsRenderer_CreateContext(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM windowDC_type;
  void * windowDC = memenv->getPtr(env, argv[1], "windowDC", &windowDC_type);
  if(!This) throw wxe_badarg("This");
  wxGraphicsContext * Result;
  if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindowDC))
   Result =  (wxGraphicsContext*)This->CreateContext(* static_cast<wxWindowDC*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxWindow))
   Result =  (wxGraphicsContext*)This->CreateContext(static_cast<wxWindow*> (windowDC));
  else if(enif_is_identical(windowDC_type, WXE_ATOM_wxMemoryDC))
   Result =  (wxGraphicsContext*)This->CreateContext(* static_cast<wxMemoryDC*> (windowDC));
  else throw wxe_badarg("windowDC");
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv,8), "wxGraphicsContext")
);

}

// wxGraphicsRenderer::CreateBrush
void wxGraphicsRenderer_CreateBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  wxBrush *brush;
  brush = (wxBrush *) memenv->getPtr(env, argv[1], "brush");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateBrush(*brush)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsRenderer::CreateLinearGradientBrush
void wxGraphicsRenderer_CreateLinearGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  double x1;
  if(!wxe_get_double(env, argv[1], &x1)) Badarg("x1");
  double y1;
  if(!wxe_get_double(env, argv[2], &y1)) Badarg("y1");
  double x2;
  if(!wxe_get_double(env, argv[3], &x2)) Badarg("x2");
  double y2;
  if(!wxe_get_double(env, argv[4], &y2)) Badarg("y2");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[5], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateLinearGradientBrush(x1,y1,x2,y2,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsRenderer::CreateRadialGradientBrush
void wxGraphicsRenderer_CreateRadialGradientBrush(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  double startX;
  if(!wxe_get_double(env, argv[1], &startX)) Badarg("startX");
  double startY;
  if(!wxe_get_double(env, argv[2], &startY)) Badarg("startY");
  double endX;
  if(!wxe_get_double(env, argv[3], &endX)) Badarg("endX");
  double endY;
  if(!wxe_get_double(env, argv[4], &endY)) Badarg("endY");
  double radius;
  if(!wxe_get_double(env, argv[5], &radius)) Badarg("radius");
  wxGraphicsGradientStops *stops;
  stops = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[6], "stops");
  if(!This) throw wxe_badarg("This");
  wxGraphicsBrush * Result = new wxGraphicsBrush(This->CreateRadialGradientBrush(startX,startY,endX,endY,radius,*stops)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsBrush"));

}

// wxGraphicsRenderer::CreateFont
void wxGraphicsRenderer_CreateFont_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(*font,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsRenderer::CreateFont
void wxGraphicsRenderer_CreateFont_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxFONTFLAG_DEFAULT;
  wxColour col= *wxBLACK;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  double sizeInPixels;
  if(!wxe_get_double(env, argv[1], &sizeInPixels)) Badarg("sizeInPixels");
  ErlNifBinary facename_bin;
  wxString facename;
  if(!enif_inspect_binary(env, argv[2], &facename_bin)) Badarg("facename");
  facename = wxString(facename_bin.data, wxConvUTF8, facename_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "col"))) {
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
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsFont * Result = new wxGraphicsFont(This->CreateFont(sizeInPixels,facename,flags,col)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsFont"));

}

// wxGraphicsRenderer::CreateMatrix
void wxGraphicsRenderer_CreateMatrix(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxDouble a=1.0;
  wxDouble b=0.0;
  wxDouble c=0.0;
  wxDouble d=1.0;
  wxDouble tx=0.0;
  wxDouble ty=0.0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "a"))) {
  if(!wxe_get_double(env, tpl[1], &a)) Badarg("a");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!wxe_get_double(env, tpl[1], &b)) Badarg("b");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "c"))) {
  if(!wxe_get_double(env, tpl[1], &c)) Badarg("c");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "d"))) {
  if(!wxe_get_double(env, tpl[1], &d)) Badarg("d");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "tx"))) {
  if(!wxe_get_double(env, tpl[1], &tx)) Badarg("tx");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "ty"))) {
  if(!wxe_get_double(env, tpl[1], &ty)) Badarg("ty");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxGraphicsMatrix * Result = new wxGraphicsMatrix(This->CreateMatrix(a,b,c,d,tx,ty)); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsMatrix"));

}

// wxGraphicsRenderer::CreatePath
void wxGraphicsRenderer_CreatePath(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsRenderer *This;
  This = (wxGraphicsRenderer *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxGraphicsPath * Result = new wxGraphicsPath(This->CreatePath()); app->newPtr((void *) Result,4, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsPath"));

}

#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
#endif // wxUSE_GRAPHICS_CONTEXT
#if wxUSE_GRAPHICS_CONTEXT
// wxGraphicsGradientStops::wxGraphicsGradientStops
void wxGraphicsGradientStops_new(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxColour startCol = wxTransparentColour;
 wxColour endCol = wxTransparentColour;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "startCol"))) {
  const ERL_NIF_TERM *startCol_t;
  int startCol_sz;
  if(!enif_get_tuple(env, tpl[1], &startCol_sz, &startCol_t)) Badarg("startCol");
  int startColR;
  if(!enif_get_int(env, startCol_t[0], &startColR)) Badarg("startCol");
  int startColG;
  if(!enif_get_int(env, startCol_t[1], &startColG)) Badarg("startCol");
  int startColB;
  if(!enif_get_int(env, startCol_t[2], &startColB)) Badarg("startCol");
  int startColA;
  if(!enif_get_int(env, startCol_t[3], &startColA)) Badarg("startCol");
  startCol = wxColour(startColR,startColG,startColB,startColA);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "endCol"))) {
  const ERL_NIF_TERM *endCol_t;
  int endCol_sz;
  if(!enif_get_tuple(env, tpl[1], &endCol_sz, &endCol_t)) Badarg("endCol");
  int endColR;
  if(!enif_get_int(env, endCol_t[0], &endColR)) Badarg("endCol");
  int endColG;
  if(!enif_get_int(env, endCol_t[1], &endColG)) Badarg("endCol");
  int endColB;
  if(!enif_get_int(env, endCol_t[2], &endColB)) Badarg("endCol");
  int endColA;
  if(!enif_get_int(env, endCol_t[3], &endColA)) Badarg("endCol");
  endCol = wxColour(endColR,endColG,endColB,endColA);
    } else        Badarg("Options");
  };
  wxGraphicsGradientStops * Result = new EwxGraphicsGradientStops(startCol,endCol);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxGraphicsGradientStops"));

}

// wxGraphicsGradientStops::Item
void wxGraphicsGradientStops_Item(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  unsigned int n;
  if(!enif_get_uint(env, argv[1], &n)) Badarg("n");
  if(!This) throw wxe_badarg("This");
  wxGraphicsGradientStop Result = This->Item(n);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsGradientStops::GetCount
void wxGraphicsGradientStops_GetCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  size_t Result = This->GetCount();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxGraphicsGradientStops::SetStartColour
void wxGraphicsGradientStops_SetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
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
  This->SetStartColour(col);

}

// wxGraphicsGradientStops::GetStartColour
void wxGraphicsGradientStops_GetStartColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetStartColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsGradientStops::SetEndColour
void wxGraphicsGradientStops_SetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
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
  This->SetEndColour(col);

}

// wxGraphicsGradientStops::GetEndColour
void wxGraphicsGradientStops_GetEndColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxColour Result = This->GetEndColour();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxGraphicsGradientStops::Add
void wxGraphicsGradientStops_Add(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxGraphicsGradientStops *This;
  This = (wxGraphicsGradientStops *) memenv->getPtr(env, argv[0], "This");
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
  float pos;
  if(!wxe_get_float(env, argv[2], &pos)) Badarg("pos");
  if(!This) throw wxe_badarg("This");
  This->Add(col,pos);

}

#endif // wxUSE_GRAPHICS_CONTEXT
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
  const wxBitmap * Result = &This->GetBitmap();
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
  if(!This) throw wxe_badarg("This");
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
  int i;
  if(!enif_get_int(env, argv[1], &i)) Badarg("i"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
  This->SetFieldsCount(number, widths.empty() ? (int *) NULL : widths.data());

}

// wxStatusBar::SetMinHeight
void wxStatusBar_SetMinHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  if(!This) throw wxe_badarg("This");
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
  if(!This) throw wxe_badarg("This");
  This->SetStatusText(text,number);

}

// wxStatusBar::SetStatusWidths
void wxStatusBar_SetStatusWidths(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
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
  if(!This) throw wxe_badarg("This");
  This->SetStatusWidths(widths_fieldLen,widths_field.data());

}

// wxStatusBar::SetStatusStyles
void wxStatusBar_SetStatusStyles(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxStatusBar *This;
  This = (wxStatusBar *) memenv->getPtr(env, argv[0], "This");
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
  if(!This) throw wxe_badarg("This");
  This->SetStatusStyles(stylesLen,styles.data());

}

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

// wxIcon::wxIcon
void wxIcon_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxIcon * Result = new EwxIcon();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIcon::wxIcon
void wxIcon_new_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[0], "icon");
  wxIcon * Result = new EwxIcon(*icon);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIcon::wxIcon
void wxIcon_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxICON_DEFAULT_TYPE;
  int desiredWidth=-1;
  int desiredHeight=-1;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "desiredWidth"))) {
  if(!enif_get_int(env, tpl[1], &desiredWidth)) Badarg("desiredWidth"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "desiredHeight"))) {
  if(!enif_get_int(env, tpl[1], &desiredHeight)) Badarg("desiredHeight"); // int
    } else        Badarg("Options");
  };
  wxIcon * Result = new EwxIcon(name,type,desiredWidth,desiredHeight);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIcon::CopyFromBitmap
void wxIcon_CopyFromBitmap(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIcon *This;
  This = (wxIcon *) memenv->getPtr(env, argv[0], "This");
  wxBitmap *bmp;
  bmp = (wxBitmap *) memenv->getPtr(env, argv[1], "bmp");
  if(!This) throw wxe_badarg("This");
  This->CopyFromBitmap(*bmp);

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxIconBundle * Result = new EwxIconBundle();
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[0], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  wxIconBundle * Result = new EwxIconBundle(file);
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[0], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[1], (int *) &type)) Badarg("type"); // enum
  wxIconBundle * Result = new EwxIconBundle(file,type);
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::wxIconBundle
void wxIconBundle_new_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ERL_NIF_TERM ic_type;
  void * ic = memenv->getPtr(env, argv[0], "ic", &ic_type);
  wxIconBundle * Result;
  if(enif_is_identical(ic_type, WXE_ATOM_wxIconBundle))
    Result = new EwxIconBundle(* static_cast<wxIconBundle*> (ic));
  else if(enif_is_identical(ic_type, WXE_ATOM_wxIcon))
    Result = new EwxIconBundle(* static_cast<wxIcon*> (ic));
  else throw wxe_badarg("ic");
  app->newPtr((void *) Result, 62, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIconBundle"));

}

// wxIconBundle::~wxIconBundle
void wxIconBundle_destruct(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 ErlNifEnv *env = Ecmd.env;
 ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
 if(This) {   ((WxeApp *) wxTheApp)->clearPtr((void *) This);
   delete This;}
}

// wxIconBundle::AddIcon
void wxIconBundle_AddIcon_1_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  if(!This) throw wxe_badarg("This");
  This->AddIcon(file);

}

// wxIconBundle::AddIcon
void wxIconBundle_AddIcon_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary file_bin;
  wxString file;
  if(!enif_inspect_binary(env, argv[1], &file_bin)) Badarg("file");
  file = wxString(file_bin.data, wxConvUTF8, file_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[2], (int *) &type)) Badarg("type"); // enum
  if(!This) throw wxe_badarg("This");
  This->AddIcon(file,type);

}

// wxIconBundle::AddIcon
void wxIconBundle_AddIcon_1_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  wxIcon *icon;
  icon = (wxIcon *) memenv->getPtr(env, argv[1], "icon");
  if(!This) throw wxe_badarg("This");
  This->AddIcon(*icon);

}

// wxIconBundle::GetIcon
void wxIconBundle_GetIcon_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int flags=wxIconBundle::FALLBACK_SYSTEM;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
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
  wxIcon * Result = new wxIcon(This->GetIcon(size,flags)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

}

// wxIconBundle::GetIcon
void wxIconBundle_GetIcon_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxCoord size=wxDefaultCoord;
  int flags=wxIconBundle::FALLBACK_SYSTEM;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxIconBundle *This;
  This = (wxIconBundle *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "size"))) {
  if(!enif_get_int(env, tpl[1], &size)) Badarg("size"); // wxCoord
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "flags"))) {
  if(!enif_get_int(env, tpl[1], &flags)) Badarg("flags"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxIcon * Result = new wxIcon(This->GetIcon(size,flags)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxIcon"));

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

// wxImage::wxImage
void wxImage_new_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxImage * Result = new EwxImage();
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(width,height,clear);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(sz,clear);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    memcpy(data,data_bin.data,data_bin.size);}
;
  wxImage * Result = new EwxImage(width,height,data,static_data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
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
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(sz,data,static_data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  int width;
  if(!enif_get_int(env, argv[0], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[1], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[3], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char*) alpha_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    alpha = (unsigned char *) malloc(alpha_bin.size);
    memcpy(data,data_bin.data,data_bin.size);
    memcpy(alpha,alpha_bin.data,alpha_bin.size);}
;
  wxImage * Result = new EwxImage(width,height,data,alpha,static_data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
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
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[2], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char*) alpha_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    memcpy(data,data_bin.data,data_bin.size);}
;
  wxImage * Result = new EwxImage(sz,data,alpha,static_data);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_TYPE_ANY;
  int index=-1;
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  wxImage * Result = new EwxImage(name,type,index);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::wxImage
void wxImage_new_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary mimetype_bin;
  wxString mimetype;
  if(!enif_inspect_binary(env, argv[1], &mimetype_bin)) Badarg("mimetype");
  mimetype = wxString(mimetype_bin.data, wxConvUTF8, mimetype_bin.size);
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
  wxImage * Result = new EwxImage(name,mimetype,index);
  app->newPtr((void *) Result, 1, memenv);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Blur
void wxImage_Blur(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int blurRadius;
  if(!enif_get_int(env, argv[1], &blurRadius)) Badarg("blurRadius"); // int
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Blur(blurRadius)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::BlurHorizontal
void wxImage_BlurHorizontal(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int blurRadius;
  if(!enif_get_int(env, argv[1], &blurRadius)) Badarg("blurRadius"); // int
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->BlurHorizontal(blurRadius)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::BlurVertical
void wxImage_BlurVertical(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int blurRadius;
  if(!enif_get_int(env, argv[1], &blurRadius)) Badarg("blurRadius"); // int
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->BlurVertical(blurRadius)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::ConvertAlphaToMask
void wxImage_ConvertAlphaToMask_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int threshold=wxIMAGE_ALPHA_THRESHOLD;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "threshold"))) {
  if(!enif_get_uint(env, tpl[1], &threshold)) Badarg("threshold");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ConvertAlphaToMask(threshold);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::ConvertAlphaToMask
void wxImage_ConvertAlphaToMask_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int threshold=wxIMAGE_ALPHA_THRESHOLD;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int mr;
  if(!enif_get_uint(env, argv[1], &mr)) Badarg("mr");
  unsigned int mg;
  if(!enif_get_uint(env, argv[2], &mg)) Badarg("mg");
  unsigned int mb;
  if(!enif_get_uint(env, argv[3], &mb)) Badarg("mb");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "threshold"))) {
  if(!enif_get_uint(env, tpl[1], &threshold)) Badarg("threshold");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->ConvertAlphaToMask(mr,mg,mb,threshold);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::ConvertToGreyscale
void wxImage_ConvertToGreyscale_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  double weight_r;
  if(!wxe_get_double(env, argv[1], &weight_r)) Badarg("weight_r");
  double weight_g;
  if(!wxe_get_double(env, argv[2], &weight_g)) Badarg("weight_g");
  double weight_b;
  if(!wxe_get_double(env, argv[3], &weight_b)) Badarg("weight_b");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToGreyscale(weight_r,weight_g,weight_b)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::ConvertToGreyscale
void wxImage_ConvertToGreyscale_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToGreyscale()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::ConvertToMono
void wxImage_ConvertToMono(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int r;
  if(!enif_get_uint(env, argv[1], &r)) Badarg("r");
  unsigned int g;
  if(!enif_get_uint(env, argv[2], &g)) Badarg("g");
  unsigned int b;
  if(!enif_get_uint(env, argv[3], &b)) Badarg("b");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->ConvertToMono(r,g,b)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Copy
void wxImage_Copy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Copy()); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Create
void wxImage_Create_3_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,clear);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clear=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clear"))) {
  clear = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,clear);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_4_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[3], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    memcpy(data,data_bin.data,data_bin.size);}
;
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,data,static_data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_3_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,data,static_data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int width;
  if(!enif_get_int(env, argv[1], &width)) Badarg("width"); // int
  int height;
  if(!enif_get_int(env, argv[2], &height)) Badarg("height"); // int
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[3], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[4], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char*) alpha_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[5];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    alpha = (unsigned char *) malloc(alpha_bin.size);
    memcpy(data,data_bin.data,data_bin.size);
    memcpy(alpha,alpha_bin.data,alpha_bin.size);}
;
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(width,height,data,alpha,static_data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Create
void wxImage_Create_4_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *sz_t;
  int sz_sz;
  if(!enif_get_tuple(env, argv[1], &sz_sz, &sz_t)) Badarg("sz");
  int szW;
  if(!enif_get_int(env, sz_t[0], &szW)) Badarg("sz");
  int szH;
  if(!enif_get_int(env, sz_t[1], &szH)) Badarg("sz");
  wxSize sz = wxSize(szW,szH);
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[2], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[3], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char*) alpha_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    memcpy(data,data_bin.data,data_bin.size);}
;
  if(!This) throw wxe_badarg("This");
  bool Result = This->Create(sz,data,alpha,static_data);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Destroy
void wxImage_Destroy(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->Destroy();

}

// wxImage::FindFirstUnusedColour
void wxImage_FindFirstUnusedColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned char r;
  unsigned char g;
  unsigned char b;
  unsigned int startR=1;
  unsigned int startG=0;
  unsigned int startB=0;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "startR"))) {
  if(!enif_get_uint(env, tpl[1], &startR)) Badarg("startR");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "startG"))) {
  if(!enif_get_uint(env, tpl[1], &startG)) Badarg("startG");
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "startB"))) {
  if(!enif_get_uint(env, tpl[1], &startB)) Badarg("startB");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->FindFirstUnusedColour(&r,&g,&b,startR,startG,startB);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_bool(Result),
    rt.make_uint(r),
  rt.make_uint(g),
  rt.make_uint(b));
  rt.send(msg);

}

// wxImage::GetImageExtWildcard
void wxImage_GetImageExtWildcard(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxString Result = wxImage::GetImageExtWildcard();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxImage::GetAlpha
void wxImage_GetAlpha_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char * Result = (char*)This->GetAlpha();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, (This->GetWidth()*This->GetHeight())));

}

// wxImage::GetAlpha
void wxImage_GetAlpha_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetAlpha(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetBlue
void wxImage_GetBlue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetBlue(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetData
void wxImage_GetData(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char * Result = (char*)This->GetData();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_binary(Result, (This->GetWidth()*This->GetHeight()*3)));

}

// wxImage::GetGreen
void wxImage_GetGreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetGreen(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetImageCount
void wxImage_GetImageCount(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_TYPE_ANY;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary filename_bin;
  wxString filename;
  if(!enif_inspect_binary(env, argv[0], &filename_bin)) Badarg("filename");
  filename = wxString(filename_bin.data, wxConvUTF8, filename_bin.size);
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
  int Result = wxImage::GetImageCount(filename,type);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::GetHeight
void wxImage_GetHeight(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetHeight();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::GetMaskBlue
void wxImage_GetMaskBlue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char Result = This->GetMaskBlue();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetMaskGreen
void wxImage_GetMaskGreen(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char Result = This->GetMaskGreen();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetMaskRed
void wxImage_GetMaskRed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  char Result = This->GetMaskRed();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetOrFindMaskColour
void wxImage_GetOrFindMaskColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned char r;
  unsigned char g;
  unsigned char b;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->GetOrFindMaskColour(&r,&g,&b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  ERL_NIF_TERM msg = enif_make_tuple4(rt.env,
  rt.make_bool(Result),
    rt.make_uint(r),
  rt.make_uint(g),
  rt.make_uint(b));
  rt.send(msg);

}

// wxImage::GetPalette
void wxImage_GetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  const wxPalette * Result = &This->GetPalette();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxPalette"));

}

// wxImage::GetRed
void wxImage_GetRed(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  if(!This) throw wxe_badarg("This");
  char Result = This->GetRed(x,y);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_uint(Result));

}

// wxImage::GetSubImage
void wxImage_GetSubImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
  wxImage * Result = new EwxImage(This->GetSubImage(rect)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::GetWidth
void wxImage_GetWidth(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  int Result = This->GetWidth();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::HasAlpha
void wxImage_HasAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasAlpha();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::HasMask
void wxImage_HasMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasMask();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::GetOption
void wxImage_GetOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  wxString Result = This->GetOption(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make(Result));

}

// wxImage::GetOptionInt
void wxImage_GetOptionInt(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  int Result = This->GetOptionInt(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_int(Result));

}

// wxImage::HasOption
void wxImage_HasOption(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->HasOption(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::InitAlpha
void wxImage_InitAlpha(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  This->InitAlpha();

}

// wxImage::InitStandardHandlers
void wxImage_InitStandardHandlers(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  wxImage::InitStandardHandlers();

}

// wxImage::IsTransparent
void wxImage_IsTransparent(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  unsigned int threshold=wxIMAGE_ALPHA_THRESHOLD;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "threshold"))) {
  if(!enif_get_uint(env, tpl[1], &threshold)) Badarg("threshold");
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsTransparent(x,y,threshold);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::LoadFile
void wxImage_LoadFile_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxBitmapType type=wxBITMAP_TYPE_ANY;
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "index"))) {
  if(!enif_get_int(env, tpl[1], &index)) Badarg("index"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  bool Result = This->LoadFile(name,type,index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::LoadFile
void wxImage_LoadFile_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int index=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary mimetype_bin;
  wxString mimetype;
  if(!enif_inspect_binary(env, argv[2], &mimetype_bin)) Badarg("mimetype");
  mimetype = wxString(mimetype_bin.data, wxConvUTF8, mimetype_bin.size);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
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
  bool Result = This->LoadFile(name,mimetype,index);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::IsOk
void wxImage_IsOk(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  if(!This) throw wxe_badarg("This");
  bool Result = This->IsOk();
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::RemoveHandler
void wxImage_RemoveHandler(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[0], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  bool Result = wxImage::RemoveHandler(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Mirror
void wxImage_Mirror(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool horizontally=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "horizontally"))) {
  horizontally = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Mirror(horizontally)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Replace
void wxImage_Replace(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int r1;
  if(!enif_get_uint(env, argv[1], &r1)) Badarg("r1");
  unsigned int g1;
  if(!enif_get_uint(env, argv[2], &g1)) Badarg("g1");
  unsigned int b1;
  if(!enif_get_uint(env, argv[3], &b1)) Badarg("b1");
  unsigned int r2;
  if(!enif_get_uint(env, argv[4], &r2)) Badarg("r2");
  unsigned int g2;
  if(!enif_get_uint(env, argv[5], &g2)) Badarg("g2");
  unsigned int b2;
  if(!enif_get_uint(env, argv[6], &b2)) Badarg("b2");
  if(!This) throw wxe_badarg("This");
  This->Replace(r1,g1,b1,r2,g2,b2);

}

// wxImage::Rescale
void wxImage_Rescale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxImageResizeQuality quality=wxIMAGE_QUALITY_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "quality"))) {
  if(!enif_get_int(env, tpl[1], (int *) &quality)) Badarg("quality"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = &This->Rescale(width,height,quality);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Resize
void wxImage_Resize(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int r=-1;
  int g=-1;
  int b=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "r"))) {
  if(!enif_get_int(env, tpl[1], &r)) Badarg("r"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "g"))) {
  if(!enif_get_int(env, tpl[1], &g)) Badarg("g"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!enif_get_int(env, tpl[1], &b)) Badarg("b"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = &This->Resize(size,pos,r,g,b);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Rotate
void wxImage_Rotate(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool interpolating=true;
  wxPoint *offset_after_rotation=NULL; wxPoint offset_after_rotationTmp;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  const ERL_NIF_TERM *rotationCentre_t;
  int rotationCentre_sz;
  if(!enif_get_tuple(env, argv[2], &rotationCentre_sz, &rotationCentre_t)) Badarg("rotationCentre");
  int rotationCentreX;
  if(!enif_get_int(env, rotationCentre_t[0], &rotationCentreX)) Badarg("rotationCentre");
  int rotationCentreY;
  if(!enif_get_int(env, rotationCentre_t[1], &rotationCentreY)) Badarg("rotationCentre");
  wxPoint rotationCentre = wxPoint(rotationCentreX,rotationCentreY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "interpolating"))) {
  interpolating = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "offset_after_rotation"))) {
  const ERL_NIF_TERM *offset_after_rotation_t;
  int offset_after_rotation_sz;
  if(!enif_get_tuple(env, tpl[1], &offset_after_rotation_sz, &offset_after_rotation_t)) Badarg("offset_after_rotation");
  int offset_after_rotationX;
  if(!enif_get_int(env, offset_after_rotation_t[0], &offset_after_rotationX)) Badarg("offset_after_rotation");
  int offset_after_rotationY;
  if(!enif_get_int(env, offset_after_rotation_t[1], &offset_after_rotationY)) Badarg("offset_after_rotation");
  offset_after_rotationTmp = wxPoint(offset_after_rotationX,offset_after_rotationY); offset_after_rotation = & offset_after_rotationTmp;
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Rotate(angle,rotationCentre,interpolating,offset_after_rotation)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::RotateHue
void wxImage_RotateHue(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  double angle;
  if(!wxe_get_double(env, argv[1], &angle)) Badarg("angle");
  if(!This) throw wxe_badarg("This");
  This->RotateHue(angle);

}

// wxImage::Rotate90
void wxImage_Rotate90(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool clockwise=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "clockwise"))) {
  clockwise = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Rotate90(clockwise)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::SaveFile
void wxImage_SaveFile_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  wxBitmapType type;
  if(!enif_get_int(env, argv[2], (int *) &type)) Badarg("type"); // enum
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name,type);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::SaveFile
void wxImage_SaveFile_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary mimetype_bin;
  wxString mimetype;
  if(!enif_inspect_binary(env, argv[2], &mimetype_bin)) Badarg("mimetype");
  mimetype = wxString(mimetype_bin.data, wxConvUTF8, mimetype_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name,mimetype);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::SaveFile
void wxImage_SaveFile_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  if(!This) throw wxe_badarg("This");
  bool Result = This->SaveFile(name);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::Scale
void wxImage_Scale(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
 wxImageResizeQuality quality=wxIMAGE_QUALITY_NORMAL;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
    if(enif_is_identical(tpl[0], enif_make_atom(env, "quality"))) {
  if(!enif_get_int(env, tpl[1], (int *) &quality)) Badarg("quality"); // enum
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Scale(width,height,quality)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::Size
void wxImage_Size(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  int r=-1;
  int g=-1;
  int b=-1;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  const ERL_NIF_TERM *size_t;
  int size_sz;
  if(!enif_get_tuple(env, argv[1], &size_sz, &size_t)) Badarg("size");
  int sizeW;
  if(!enif_get_int(env, size_t[0], &sizeW)) Badarg("size");
  int sizeH;
  if(!enif_get_int(env, size_t[1], &sizeH)) Badarg("size");
  wxSize size = wxSize(sizeW,sizeH);
  const ERL_NIF_TERM *pos_t;
  int pos_sz;
  if(!enif_get_tuple(env, argv[2], &pos_sz, &pos_t)) Badarg("pos");
  int posX;
  if(!enif_get_int(env, pos_t[0], &posX)) Badarg("pos");
  int posY;
  if(!enif_get_int(env, pos_t[1], &posY)) Badarg("pos");
  wxPoint pos = wxPoint(posX,posY);
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[3];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "r"))) {
  if(!enif_get_int(env, tpl[1], &r)) Badarg("r"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "g"))) {
  if(!enif_get_int(env, tpl[1], &g)) Badarg("g"); // int
    } else     if(enif_is_identical(tpl[0], enif_make_atom(env, "b"))) {
  if(!enif_get_int(env, tpl[1], &b)) Badarg("b"); // int
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  wxImage * Result = new EwxImage(This->Size(size,pos,r,g,b)); app->newPtr((void *) Result,3, memenv);;
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_ref(app->getRef((void *)Result,memenv), "wxImage"));

}

// wxImage::SetAlpha
void wxImage_SetAlpha_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned char * alpha;
  ErlNifBinary alpha_bin;
  if(!enif_inspect_binary(env, argv[1], &alpha_bin)) Badarg("alpha");
  alpha = (unsigned char*) alpha_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    alpha = (unsigned char *) malloc(alpha_bin.size);
    memcpy(alpha,alpha_bin.data,alpha_bin.size);}
;
  if(!This) throw wxe_badarg("This");
  This->SetAlpha(alpha,static_data);

}

// wxImage::SetAlpha
void wxImage_SetAlpha_3(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  unsigned int alpha;
  if(!enif_get_uint(env, argv[3], &alpha)) Badarg("alpha");
  if(!This) throw wxe_badarg("This");
  This->SetAlpha(x,y,alpha);

}

// wxImage::SetData
void wxImage_SetData_2(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[2];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    memcpy(data,data_bin.data,data_bin.size);}
;
  if(!This) throw wxe_badarg("This");
  This->SetData(data,static_data);

}

// wxImage::SetData
void wxImage_SetData_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool static_data=false;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned char * data;
  ErlNifBinary data_bin;
  if(!enif_inspect_binary(env, argv[1], &data_bin)) Badarg("data");
  data = (unsigned char*) data_bin.data;
  int new_width;
  if(!enif_get_int(env, argv[2], &new_width)) Badarg("new_width"); // int
  int new_height;
  if(!enif_get_int(env, argv[3], &new_height)) Badarg("new_height"); // int
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[4];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "static_data"))) {
  static_data = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
 if(!static_data) {
    data = (unsigned char *) malloc(data_bin.size);
    memcpy(data,data_bin.data,data_bin.size);}
;
  if(!This) throw wxe_badarg("This");
  This->SetData(data,new_width,new_height,static_data);

}

// wxImage::SetMask
void wxImage_SetMask(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  bool mask=true;
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ERL_NIF_TERM lstHead, lstTail;
  lstTail = argv[1];
  if(!enif_is_list(env, lstTail)) Badarg("Options");
  const ERL_NIF_TERM *tpl;
  int tpl_sz;
  while(!enif_is_empty_list(env, lstTail)) {
    if(!enif_get_list_cell(env, lstTail, &lstHead, &lstTail)) Badarg("Options");
    if(!enif_get_tuple(env, lstHead, &tpl_sz, &tpl) || tpl_sz != 2) Badarg("Options");
    if(enif_is_identical(tpl[0], enif_make_atom(env, "mask"))) {
  mask = enif_is_identical(tpl[1], WXE_ATOM_true);
    } else        Badarg("Options");
  };
  if(!This) throw wxe_badarg("This");
  This->SetMask(mask);

}

// wxImage::SetMaskColour
void wxImage_SetMaskColour(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  unsigned int red;
  if(!enif_get_uint(env, argv[1], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[2], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[3], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  This->SetMaskColour(red,green,blue);

}

// wxImage::SetMaskFromImage
void wxImage_SetMaskFromImage(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  wxImage *mask;
  mask = (wxImage *) memenv->getPtr(env, argv[1], "mask");
  unsigned int mr;
  if(!enif_get_uint(env, argv[2], &mr)) Badarg("mr");
  unsigned int mg;
  if(!enif_get_uint(env, argv[3], &mg)) Badarg("mg");
  unsigned int mb;
  if(!enif_get_uint(env, argv[4], &mb)) Badarg("mb");
  if(!This) throw wxe_badarg("This");
  bool Result = This->SetMaskFromImage(*mask,mr,mg,mb);
  wxeReturn rt = wxeReturn(memenv, Ecmd.caller, true);
  rt.send(  rt.make_bool(Result));

}

// wxImage::SetOption
void wxImage_SetOption_2_1(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  ErlNifBinary value_bin;
  wxString value;
  if(!enif_inspect_binary(env, argv[2], &value_bin)) Badarg("value");
  value = wxString(value_bin.data, wxConvUTF8, value_bin.size);
  if(!This) throw wxe_badarg("This");
  This->SetOption(name,value);

}

// wxImage::SetOption
void wxImage_SetOption_2_0(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  ErlNifBinary name_bin;
  wxString name;
  if(!enif_inspect_binary(env, argv[1], &name_bin)) Badarg("name");
  name = wxString(name_bin.data, wxConvUTF8, name_bin.size);
  int value;
  if(!enif_get_int(env, argv[2], &value)) Badarg("value"); // int
  if(!This) throw wxe_badarg("This");
  This->SetOption(name,value);

}

// wxImage::SetPalette
void wxImage_SetPalette(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  wxPalette *palette;
  palette = (wxPalette *) memenv->getPtr(env, argv[1], "palette");
  if(!This) throw wxe_badarg("This");
  This->SetPalette(*palette);

}

// wxImage::SetRGB
void wxImage_SetRGB_5(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
  int x;
  if(!enif_get_int(env, argv[1], &x)) Badarg("x"); // int
  int y;
  if(!enif_get_int(env, argv[2], &y)) Badarg("y"); // int
  unsigned int r;
  if(!enif_get_uint(env, argv[3], &r)) Badarg("r");
  unsigned int g;
  if(!enif_get_uint(env, argv[4], &g)) Badarg("g");
  unsigned int b;
  if(!enif_get_uint(env, argv[5], &b)) Badarg("b");
  if(!This) throw wxe_badarg("This");
  This->SetRGB(x,y,r,g,b);

}

// wxImage::SetRGB
void wxImage_SetRGB_4(WxeApp *app, wxeMemEnv *memenv, wxeCommand& Ecmd)
{
  ErlNifEnv *env = Ecmd.env;
  ERL_NIF_TERM * argv = Ecmd.args;
  wxImage *This;
  This = (wxImage *) memenv->getPtr(env, argv[0], "This");
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
  unsigned int red;
  if(!enif_get_uint(env, argv[2], &red)) Badarg("red");
  unsigned int green;
  if(!enif_get_uint(env, argv[3], &green)) Badarg("green");
  unsigned int blue;
  if(!enif_get_uint(env, argv[4], &blue)) Badarg("blue");
  if(!This) throw wxe_badarg("This");
  This->SetRGB(rect,red,green,blue);

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

