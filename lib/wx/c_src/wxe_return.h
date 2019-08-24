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

/* 
 * File:   wxe_return.h
 * Author: matthew
 *
 * Created on 11 October 2008, 20:33
 */

#ifndef _WXE_RETURN_H
#define	_WXE_RETURN_H
//#define wxUSEGUI
#include "wxe_impl.h"
extern "C" {
#include "wxe_driver.h"
}
#include <wx/wx.h>
#include <wx/geometry.h>
#include <wx/colour.h>
#include <wx/grid.h>
#include <wx/gbsizer.h>
#include <wx/dynarray.h>
#include <wx/html/htmlcell.h>


#define RT_BUFF_SZ 64

class wxeReturn {

public:
    wxeReturn (ErlDrvTermData      _port,
	       ErlDrvTermData      _caller,
	       bool                _isResult=false);

    ~wxeReturn();


    void add(ErlDrvTermData type, ErlDrvTermData data);

    void addRef(const unsigned int ref, const char* className);
    void addAtom(const char* atomName);
    
    void addBinary(const char* atomName, size_t size);
    void addExt2Term(wxeErlTerm * term);
    void addExt2Term(wxETreeItemData * term);

    void addNil() { do_add(ERL_DRV_NIL); };

    void addUint(unsigned int n);
    
    void addInt(int n);
    
    void addFloat(double f);
    
    void addTupleCount(unsigned int n);
    
    /** @param n length of the list (not including the NIL terminator */
    void endList(unsigned int n);
    
    void addBool(int val);
    
    void add(const wxString s);
    void add(const wxString* s);
    void add(wxArrayString val);
    
    void add(wxPoint pt);
    
    void add( wxPoint2DDouble point2D);
        
    void add(wxSize size);
    
    void add(wxRect rect);
    
    void add(wxRect2DDouble rect2D);
    
    void add(wxDateTime dateTime);
        
    void add(wxColour colour);
    
    void add(wxGridCellCoords val);

    void add(wxGBPosition val);

    void add(wxGBSpan val);

    void add(wxMouseState val);

    void add(wxArrayInt val);

    void add(wxArrayDouble val);

    void add(wxUIntPtr *val);

    void add(const wxHtmlLinkInfo *val);

    void add(const wxHtmlLinkInfo &val);

    void do_add(ErlDrvTermData val);

    void ensureFloatCount(size_t n);

    int  send();
    
    void reset();
    
    unsigned int size();
    
private:

    inline void  addDate(wxDateTime dateTime);

    inline void  addTime(wxDateTime dateTime);

    ErlDrvTermData          caller;
    ErlDrvTermData          port;
    wxArrayDouble           temp_float;
    wxMBConvUTF32           utfConverter;
    bool                    isResult;

    unsigned int            rt_max;
    unsigned int            rt_n;
    ErlDrvTermData          *rtb;
    ErlDrvTermData          buff[RT_BUFF_SZ];
};

#endif	/* _WXE_RETURN_H */

