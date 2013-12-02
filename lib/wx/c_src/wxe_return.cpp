/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2013. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd% 
 */

#include "wxe_return.h"

// see http://docs.wxwidgets.org/stable/wx_wxarray.html#arraymacros
// this is a magic incantation which must be done!
#include <wx/arrimpl.cpp> 
WX_DEFINE_OBJARRAY(wxErlDrvTermDataArray);

#define INLINE

wxeReturn::wxeReturn (ErlDrvTermData   _port,
		      ErlDrvTermData   _caller,
		      bool             _isResult) {
    port    = _port;
    caller  = _caller;
    
    isResult = _isResult;
    
     if (isResult) {
         addAtom("_wxe_result_");
     }
}

wxeReturn::~wxeReturn () {
    //depending on which version of wxArray we use, we may have to clear it ourselves.
}

int wxeReturn::send() {      
    if ((rt.GetCount() == 2 && isResult) || rt.GetCount() == 0)
      return 1;  // not a call bail out
    
    if (isResult) {
        addTupleCount(2);
    }    

    // rt to array
    unsigned int rtLength = rt.GetCount(); //signed int

    size_t size = sizeof(ErlDrvTermData)*(rtLength);
    
    ErlDrvTermData* rtData = (ErlDrvTermData *) driver_alloc(size);
    for (unsigned int i=0; i < rtLength; i++) {
        rtData[i] = rt[i];
    }
 
    int res = erl_drv_send_term(port, caller, rtData, rtLength);
    driver_free(rtData);

#ifdef DEBUG
    if(res == -1) {
      fprintf(stderr, "Failed to send return or event msg\r\n");
    }
#endif

    reset();
    return res;
}

//clear everything so we can re-use if we want
 void wxeReturn::reset() {
     rt.empty();
     temp_float.empty();
}

INLINE
unsigned  int wxeReturn::size() {
     return rt.GetCount();
 }
 
INLINE
void wxeReturn::add(ErlDrvTermData type, ErlDrvTermData data) {
    rt.Add(type);
    rt.Add(data);
}


// INLINE 
// void wxeReturn::addRef(const void *ptr, const char* className) {
//    unsigned int ref_idx = wxe_app->getRef((void *)ptr, memEnv); 
//    addRef(ref_idx, className);
// }

INLINE 
void wxeReturn::addRef(const unsigned int ref, const char* className) {
   addAtom("wx_ref");
   addUint(ref);
   addAtom(className);
   rt.Add(ERL_DRV_NIL);
   addTupleCount(4);
}


INLINE 
void wxeReturn::addAtom(const char* atomName) {
    add(ERL_DRV_ATOM, driver_mk_atom((char *)atomName));
}

INLINE 
void wxeReturn::addBinary(const char* buf, const size_t size) {
    rt.Add(ERL_DRV_BUF2BINARY);
    rt.Add((ErlDrvTermData)buf);
    rt.Add((ErlDrvTermData)size);
}

INLINE 
void wxeReturn::addExt2Term(wxeErlTerm *term) {
  if(term) {
    rt.Add(ERL_DRV_EXT2TERM);
    rt.Add((ErlDrvTermData)term->bin);
    rt.Add((ErlDrvTermData)term->size);
  } else {
    rt.Add(ERL_DRV_NIL);
  }
}

INLINE
void wxeReturn::addExt2Term(wxETreeItemData *val) {
  if(val) {
    rt.Add(ERL_DRV_EXT2TERM);
    rt.Add((ErlDrvTermData)(val->bin));
    rt.Add((ErlDrvTermData)(val->size));
  } else
    rt.Add(ERL_DRV_NIL);
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
    rt.Add(ERL_DRV_NIL);
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

