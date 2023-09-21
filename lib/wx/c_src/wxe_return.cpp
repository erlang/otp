/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

#include "wxe_return.h"

#define INLINE

wxeReturn::wxeReturn (wxeMemEnv * _memenv,
                      ErlNifPid   _caller,
		      bool        _isResult) {
  memenv  = _memenv;
  env     = _memenv->tmp_env;
  caller  = _caller;

  isResult = _isResult;
}

//clear everything so we can re-use if we want
void wxeReturn::reset() {
  enif_clear_env(env);
}

wxeReturn::~wxeReturn () {
}

int wxeReturn::send(ERL_NIF_TERM msg) {
  int res;
  if(wxe_debug) {
    if(isResult) {
      enif_fprintf(stderr, "return to %T: ", caller);
      wx_print_term(env, msg);
      enif_fprintf(stderr, "\r\n");
    }
  }
  if(isResult) {
    res = enif_send(NULL, &caller, env,
                    enif_make_tuple2(env, WXE_ATOM_reply, msg));
  } else {
    res = enif_send(NULL, &caller, env, msg);
  }
  reset();
  return res;
}

// Helper for callbacks
void wxeReturn::send_callback(int callback, wxObject *obj, const char *class_name, ERL_NIF_TERM tail)
{
  ERL_NIF_TERM ref = make_ref(((WxeApp *) wxTheApp)->getRef((void *)obj, memenv), class_name);
  ERL_NIF_TERM args = enif_make_list_cell(env, ref, tail);
  send_callback(callback, args);
}

void wxeReturn::send_callback(int callback, ERL_NIF_TERM args)
{
    ERL_NIF_TERM cb_term = enif_make_tuple4(env,
                                          make_atom("_wx_invoke_cb_"),
                                          make_int(callback),
                                          args,
                                          make_atom("undefined"));
  if(wxe_debug) {
    enif_fprintf(stderr, "send CB %T:  %T\r\n", caller, cb_term);
  }
  send(cb_term);
  handle_event_callback(memenv->me_ref, caller);
}

INLINE
ERL_NIF_TERM wxeReturn::make_ref(const unsigned int ref, ERL_NIF_TERM className) {
  return enif_make_tuple4(env,
                          WXE_ATOM_wx_ref,
                          enif_make_uint(env, ref),
                          className,
                          enif_make_list(env, 0));
}

INLINE
ERL_NIF_TERM wxeReturn::make_ref(const unsigned int ref, const char* className) {
  return enif_make_tuple4(env,
                          WXE_ATOM_wx_ref,
                          enif_make_uint(env, ref),
                          enif_make_atom(env, className),
                          enif_make_list(env, 0));
}

INLINE
ERL_NIF_TERM wxeReturn::make_atom(const char* atomName) {
  return enif_make_atom(env, atomName);
}

INLINE
ERL_NIF_TERM wxeReturn::make_binary(const char* buf, const size_t size) {
  ERL_NIF_TERM bin;
  unsigned char * data;
  if(buf) {
    data = enif_make_new_binary(env, size, &bin);
    memcpy(data, buf, size);
    return bin;
  } else {
    return make_atom("null");
  }
}

INLINE
ERL_NIF_TERM wxeReturn::make_ext2term(wxeErlTerm *term) {
  if(term) {
    return enif_make_copy(env, term->term);
  } else {
    return enif_make_list(env, 0);
  }
}

INLINE
ERL_NIF_TERM wxeReturn::make_ext2term(wxETreeItemData *val) {
  if(val) {
    return enif_make_copy(env, val->term);
  } else
    return enif_make_list(env, 0);
}

INLINE
ERL_NIF_TERM wxeReturn::make_bool(int val) {
    if (val) {
      return WXE_ATOM_true;
    } else {
      return WXE_ATOM_false;
    }
}

ERL_NIF_TERM wxeReturn::make_int(int val) {
  return  enif_make_int(env, val);
}
ERL_NIF_TERM wxeReturn::make_uint(unsigned int val) {
  return  enif_make_uint(env, val);
}

ERL_NIF_TERM wxeReturn::make_double(double val) {
  return enif_make_double(env, val);
}

INLINE
ERL_NIF_TERM wxeReturn::make(const wxString s) {
    int strLen = s.Len();
    wxCharBuffer resultCB = s.mb_str(utfConverter);
    int * resultPtr = (int *) resultCB.data();
    ERL_NIF_TERM head, tail;
    tail = enif_make_list(env, 0);
    for (int i = strLen-1; i >= 0; i--) {
      head = enif_make_int(env, resultPtr[i]);
      tail = enif_make_list_cell(env, head, tail);
    }
    return tail;
}

INLINE
ERL_NIF_TERM wxeReturn::make(const wxString* s) {
  return make(*s);
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxArrayString val) {
    unsigned int len = val.GetCount();

    ERL_NIF_TERM head, tail;
    tail = enif_make_list(env, 0);
    for (int i = len-1; i >= 0; i--) {
      head = make(val[i]);
      tail = enif_make_list_cell(env, head, tail);
    }
    return tail;
}

ERL_NIF_TERM wxeReturn::make_list_objs(const wxSizerItemList& list, WxeApp *app, const char *cname)
{
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM class_name = enif_make_atom(env, cname);
  tail = enif_make_list(env, 0);
  for(wxSizerItemList::const_reverse_iterator it = list.rbegin(); it != list.rend(); ++it) {
    void * ResultTmp = *it;
    head = make_ref(app->getRef(ResultTmp,memenv), class_name);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}

ERL_NIF_TERM wxeReturn::make_list_objs(const wxMenuItemList& list, WxeApp *app, const char *cname)
{
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM class_name = enif_make_atom(env, cname);
  tail = enif_make_list(env, 0);
  for(wxMenuItemList::const_reverse_iterator it = list.rbegin(); it != list.rend(); ++it) {
    void * ResultTmp = *it;
    head = make_ref(app->getRef(ResultTmp,memenv), class_name);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}

ERL_NIF_TERM wxeReturn::make_list_objs(const wxWindowList& list, WxeApp *app, const char *cname)
{
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM class_name = enif_make_atom(env, cname);
  tail = enif_make_list(env, 0);
  for(wxWindowList::const_reverse_iterator it = list.rbegin(); it != list.rend(); ++it) {
    void * ResultTmp = *it;
    head = make_ref(app->getRef(ResultTmp,memenv), class_name);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}


ERL_NIF_TERM wxeReturn::make_array_objs(wxAuiPaneInfoArray& arr, WxeApp *app, const char *cname)
{
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM class_name = enif_make_atom(env, cname);
  tail = enif_make_list(env, 0);
  for(unsigned int i = arr.GetCount() -1; i >= 0; i--) {
    head = make_ref(app->getRef((void *) &arr.Item(i),memenv), class_name);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}


ERL_NIF_TERM wxeReturn::make_array_objs(wxArrayTreeItemIds& arr)
{
  ERL_NIF_TERM head, tail;
  tail = enif_make_list(env, 0);
  for(unsigned int i = arr.GetCount() -1; i >= 0; i--) {
    head = make((wxUIntPtr *) arr[i].m_pItem);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}


ERL_NIF_TERM wxeReturn::make_array_objs(wxGridCellCoordsArray& arr)
{
  ERL_NIF_TERM head, tail;
  tail = enif_make_list(env, 0);
  for(unsigned int i = arr.GetCount() -1; i >= 0; i--) {
    head = make(arr[i]);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}


INLINE
ERL_NIF_TERM wxeReturn::make_list_strings(size_t size, wxString* strings)
{
  wxArrayString tmpArrayStr(size, strings);
  return make(tmpArrayStr);
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxArrayInt val) {
    unsigned int len = val.GetCount();
    ERL_NIF_TERM head, tail;
    tail = enif_make_list(env, 0);
    for (int i = len-1; i >= 0; i--) {
      head = enif_make_int(env, val[i]);
      tail = enif_make_list_cell(env, head, tail);
    }
    return tail;
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxArrayDouble val) {
  unsigned int len = val.GetCount();

  ERL_NIF_TERM head, tail;
  tail = enif_make_list(env, 0);
  for (int i = len-1; i >= 0; i--) {
    head = enif_make_double(env, val[i]);
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}

INLINE
ERL_NIF_TERM wxeReturn::make(wxUIntPtr *val) {
  return enif_make_uint64(env, (ErlNifUInt64) val);
}

INLINE
ERL_NIF_TERM wxeReturn::make(wxPoint pt) {
  return enif_make_tuple2(env,
                          enif_make_int(env,pt.x),
                          enif_make_int(env,pt.y)
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxPoint2DDouble pt) {
  return enif_make_tuple2(env,
                          enif_make_double(env,pt.m_x),
                          enif_make_double(env,pt.m_y)
                          );
}

INLINE
ERL_NIF_TERM wxeReturn::make(wxSize size) {
  return enif_make_tuple2(env,
                          enif_make_int(env,size.GetWidth()),
                          enif_make_int(env,size.GetHeight())
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxRect rect) {
  return enif_make_tuple4(env,
                          enif_make_int(env,rect.x),
                          enif_make_int(env,rect.y),
                          enif_make_int(env,rect.width),
                          enif_make_int(env,rect.height)
                          );
}

INLINE
ERL_NIF_TERM wxeReturn::make(wxColour colour) {
  return enif_make_tuple4(env,
                          enif_make_int(env, colour.Red()),
                          enif_make_int(env, colour.Green()),
                          enif_make_int(env, colour.Blue()),
                          enif_make_int(env, colour.Alpha())
                          );
}

INLINE
ERL_NIF_TERM wxeReturn::make(wxGraphicsGradientStop stop) {
  return enif_make_tuple2(env,
                          make(stop.GetColour()),
                          enif_make_double(env, (double) stop.GetPosition()));
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxDateTime dateTime) {
  return enif_make_tuple2(env,
                          make_date(dateTime),
                          make_time(dateTime)
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make_date(wxDateTime dateTime) {
  return enif_make_tuple3(env,
                          enif_make_int(env, dateTime.GetYear()),
                          enif_make_int(env, dateTime.GetMonth()+1), // c++ month is zero based
                          enif_make_int(env, dateTime.GetDay())
                          );
}

INLINE ERL_NIF_TERM  wxeReturn::make_time(wxDateTime dateTime) {
  return enif_make_tuple3(env,
                          enif_make_int(env, dateTime.GetHour()),
                          enif_make_int(env, dateTime.GetMinute()),
                          enif_make_int(env, dateTime.GetSecond())
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxRect2DDouble rect2D) {
  return enif_make_tuple4(env,
                          enif_make_double(env, rect2D.m_x),
                          enif_make_double(env, rect2D.m_y),
                          enif_make_double(env, rect2D.m_width),
                          enif_make_double(env, rect2D.m_height)
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxGridCellCoords val) {
  return enif_make_tuple2(env,
                          enif_make_int(env, val.GetRow()),
                          enif_make_int(env, val.GetCol())
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxGBPosition val) {
  return enif_make_tuple2(env,
                          enif_make_int(env, val.GetRow()),
                          enif_make_int(env, val.GetCol())
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxGBSpan val) {
  return enif_make_tuple2(env,
                          enif_make_int(env, val.GetRowspan()),
                          enif_make_int(env, val.GetColspan())
                          );
}

INLINE
ERL_NIF_TERM  wxeReturn::make(wxMouseState val) {
  return enif_make_tuple(env, 13,
                         enif_make_atom(env, "wxMouseState"),
                         // TODO not int?
                         enif_make_uint(env, val.GetX()),
                         enif_make_uint(env, val.GetY()),
                         make_bool(val.LeftIsDown()),
                         make_bool(val.MiddleIsDown()),
                         make_bool(val.RightIsDown()),
                         make_bool(val.ControlDown()),
                         make_bool(val.ShiftDown()),
                         make_bool(val.AltDown()),
                         make_bool(val.MetaDown()),
                         make_bool(val.CmdDown()),
                         make_bool(val.Aux1IsDown()),
                         make_bool(val.Aux2IsDown())
                         );
}

INLINE
ERL_NIF_TERM wxeReturn::make(const wxHtmlLinkInfo *val) {
  return enif_make_tuple3(env,
                          make_atom("wxHtmlLinkInfo"),
                          make(val->GetHref()),
                          make(val->GetTarget())
                          );
}

INLINE
ERL_NIF_TERM wxeReturn::make(const wxHtmlLinkInfo &val) {
  return enif_make_tuple3(env,
                          make_atom("wxHtmlLinkInfo"),
                          make(val.GetHref()),
                          make(val.GetTarget())
                          );
}
