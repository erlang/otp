/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

wxeReturn::wxeReturn (ErlDrvTermData   _port,
		      ErlDrvTermData   _caller,
		      bool             _isResult) {
    port    = _port;
    caller  = _caller;

    isResult = _isResult;
    rtb = buff;
    rt_max = RT_BUFF_SZ;
    rt_n = 0;
    if (isResult) {
      addAtom("_wxe_result_");
    }
}

//clear everything so we can re-use if we want
void wxeReturn::reset() {
  rt_n = 0;
  temp_float.empty();
}

wxeReturn::~wxeReturn () {
  if(rtb != buff)
    driver_free(rtb);
}

int wxeReturn::send() {
  if ((rt_n == 2 && isResult) || rt_n == 0)
    return 1;  // not a call bail out

  if (isResult) {
    addTupleCount(2);
  }

  int res = erl_drv_send_term(port, caller, rtb, rt_n);

#ifdef DEBUG
  if(res == -1) {
    fprintf(stderr, "Failed to send return or event msg\r\n");
  }
#endif

  reset();
  return res;
}

INLINE
unsigned  int wxeReturn::size() {
  return rt_n;
}


INLINE
void wxeReturn::ensureFloatCount(size_t n) {
  temp_float.Alloc(n);
}

INLINE
void wxeReturn::do_add(ErlDrvTermData val) {
  if(rt_n >= rt_max) {  // realloc
    rt_max += RT_BUFF_SZ;
    if(rtb == buff) {
      rtb = (ErlDrvTermData *) driver_alloc(rt_max * sizeof(ErlDrvTermData));
      for(int i = 0; i < RT_BUFF_SZ; i++)
	rtb[i] = buff[i];
    } else {
      rtb = (ErlDrvTermData *) driver_realloc(rtb, rt_max * sizeof(ErlDrvTermData));
    }
  }
  rtb[rt_n++] = val;
}


INLINE
void wxeReturn::add(ErlDrvTermData type, ErlDrvTermData data) {
  do_add(type);
  do_add(data);
}


INLINE 
void wxeReturn::addRef(const unsigned int ref, const char* className) {
   addAtom("wx_ref");
   addUint(ref);
   addAtom(className);
   do_add(ERL_DRV_NIL);
   addTupleCount(4);
}


INLINE 
void wxeReturn::addAtom(const char* atomName) {
    add(ERL_DRV_ATOM, driver_mk_atom((char *)atomName));
}

INLINE 
void wxeReturn::addBinary(const char* buf, const size_t size) {
    do_add(ERL_DRV_BUF2BINARY);
    do_add((ErlDrvTermData)buf);
    do_add((ErlDrvTermData)size);
}

INLINE 
void wxeReturn::addExt2Term(wxeErlTerm *term) {
  if(term) {
    do_add(ERL_DRV_EXT2TERM);
    do_add((ErlDrvTermData)term->bin);
    do_add((ErlDrvTermData)term->size);
  } else {
    do_add(ERL_DRV_NIL);
  }
}

INLINE
void wxeReturn::addExt2Term(wxETreeItemData *val) {
  if(val) {
    do_add(ERL_DRV_EXT2TERM);
    do_add((ErlDrvTermData)(val->bin));
    do_add((ErlDrvTermData)(val->size));
  } else
    do_add(ERL_DRV_NIL);
}

INLINE
void  wxeReturn::addUint(unsigned int n) {
    add(ERL_DRV_UINT, (ErlDrvTermData)n);
}

INLINE
void  wxeReturn::addInt(int n) {
    add(ERL_DRV_INT, (ErlDrvTermData)n);
}

INLINE 
void wxeReturn::addFloat(double f) {
  // Erlang expects a pointer to double...
  // Hmm is temp_float moved if reallocated
  // the pointer may be wrong.
  // Harryhuk - use a list instead?
  temp_float.Add(f);
  add(ERL_DRV_FLOAT, (ErlDrvTermData)&temp_float.Last());
}

INLINE  
void wxeReturn::addTupleCount(unsigned int n) {
    add(ERL_DRV_TUPLE, (ErlDrvTermData)n);
}

INLINE 
void wxeReturn::endList(unsigned int n) {
  do_add(ERL_DRV_NIL);
  add(ERL_DRV_LIST, (ErlDrvTermData)(n+1));
}

INLINE 
void wxeReturn::addBool(int val) {
    if (val) {
        addAtom("true");
    } else {
        addAtom("false");
    }
}

INLINE 
void wxeReturn::add(const wxString s) {
    int strLen = s.Len();
    wxCharBuffer resultCB = s.mb_str(utfConverter);
    int * resultPtr = (int *) resultCB.data();

    for (int i = 0; i < strLen; i++, resultPtr++) {
        addInt(*resultPtr);
    }
    endList(strLen);       
}

INLINE 
void wxeReturn::add(const wxString* s) {
    add(*s);
}

INLINE
void  wxeReturn::add(wxArrayString val) {
    unsigned int len = val.GetCount();

    for (unsigned int i = 0; i< len; i++) {
        add(val[i]);       
    }
    endList(len);
}

INLINE 
void  wxeReturn::add(wxArrayInt val) {
    unsigned int len = val.GetCount();

    for (unsigned int i = 0; i< len; i++) {
        addInt(val[i]);       
    }
    endList(len);
}

INLINE
void  wxeReturn::add(wxArrayDouble val) {
  unsigned int len = val.GetCount();

  temp_float.Alloc(len);
  for (unsigned int i = 0; i< len; i++) {
    addFloat(val[i]);
  }
  endList(len);
}

INLINE 
void wxeReturn::add(wxUIntPtr *val) {
  add(ERL_DRV_UINT, (ErlDrvTermData) val);
}

INLINE 
void wxeReturn::add(wxPoint pt) {
    addInt(pt.x);
    addInt(pt.y);
    addTupleCount(2);
}

INLINE
void  wxeReturn::add(wxPoint2DDouble pt) {
    addFloat(pt.m_x);
    addFloat(pt.m_y);
    addTupleCount(2);
}

INLINE 
void wxeReturn::add(wxSize size) {
    addInt(size.GetWidth());
    addInt(size.GetHeight());
    addTupleCount(2);
}

INLINE
void  wxeReturn::add(wxRect rect) {
    addInt(rect.x);
    addInt(rect.y);
    addInt(rect.width);
    addInt(rect.height);
    addTupleCount(4);
}

INLINE 
void wxeReturn::add(wxColour colour) {
    addInt(colour.Red());
    addInt(colour.Green());
    addInt(colour.Blue());
    addInt(colour.Alpha());
    addTupleCount(4);
}

INLINE
void  wxeReturn::add(wxDateTime dateTime) {
  addDate(dateTime);
  addTime(dateTime);
  addTupleCount(2);
}

INLINE 
void  wxeReturn::addDate(wxDateTime dateTime) {
    addInt(dateTime.GetYear());
    addInt(dateTime.GetMonth()+1); // c++ month is zero based
    addInt(dateTime.GetDay());
    addTupleCount(3);
}

INLINE void  wxeReturn::addTime(wxDateTime dateTime) {
    addInt(dateTime.GetHour());
    addInt(dateTime.GetMinute());
    addInt(dateTime.GetSecond());
    addTupleCount(3);
}

INLINE
void  wxeReturn::add(wxRect2DDouble rect2D) {
    addFloat(rect2D.m_x);
    addFloat(rect2D.m_y);
    addFloat(rect2D.m_width);
    addFloat(rect2D.m_height);
    addTupleCount(4);
}

INLINE
void  wxeReturn::add(wxGridCellCoords val) {
    addInt(val.GetRow());
    addInt(val.GetCol());
    addTupleCount(2);
}

INLINE
void  wxeReturn::add(wxGBPosition val) {
    addInt(val.GetRow());
    addInt(val.GetCol());
    addTupleCount(2);
}

INLINE
void  wxeReturn::add(wxGBSpan val) {
    addInt(val.GetRowspan());
    addInt(val.GetColspan());
    addTupleCount(2);
}

INLINE
void  wxeReturn::add(wxMouseState val) {
    addAtom("wxMouseState");
    // TODO not int?
    addUint(val.GetX());
    addUint(val.GetY());
    addBool(val.LeftDown());
    addBool(val.MiddleDown());
    addBool(val.RightDown());
    addBool(val.ControlDown());
    addBool(val.ShiftDown());
    addBool(val.AltDown());
    addBool(val.MetaDown());
    addBool(val.CmdDown());
    addTupleCount(11);     
}

INLINE
void wxeReturn::add(const wxHtmlLinkInfo *val) {
  addAtom("wxHtmlLinkInfo");
  add(val->GetHref());
  add(val->GetTarget());
  addTupleCount(3);
}

INLINE
void wxeReturn::add(const wxHtmlLinkInfo &val) {
  addAtom("wxHtmlLinkInfo");
  add(val.GetHref());
  add(val.GetTarget());
  addTupleCount(3);
}

