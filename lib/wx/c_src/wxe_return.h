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


// #define send() send_term(__FILE__, __LINE__)

// see http://docs.wxwidgets.org/stable/wx_wxarray.html
WX_DECLARE_OBJARRAY(ErlDrvTermData, wxErlDrvTermDataArray);

class wxeReturn {

public:
    wxeReturn (ErlDrvTermData      _port,
	       ErlDrvTermData      _caller,
	       bool                _isResult=false);

    ~wxeReturn();


    void add(ErlDrvTermData type, ErlDrvTermData data);

    //  void addRef(const void *ptr, const char* className);
    void addRef(const unsigned int ref, const char* className);
    void addAtom(const char* atomName);
    
    void addBinary(const char* atomName, size_t size);
    void addExt2Term(wxeErlTerm * term);
    void addExt2Term(wxETreeItemData * term);

    void addNil() { rt.Add(ERL_DRV_NIL); };
    
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

    int  send();
    
    void reset();
    
    unsigned int size();
    
private:

    inline void  addDate(wxDateTime dateTime);

    inline void  addTime(wxDateTime dateTime);
    
//    WxeApp*                 wxe_app;
    ErlDrvTermData          caller;
    ErlDrvTermData          port;
//    wxeMemEnv               *memEnv;
    wxErlDrvTermDataArray   rt;
    wxArrayDouble           temp_float;
    wxMBConvUTF32           utfConverter;
    bool                    isResult;
};

#endif	/* _WXE_RETURN_H */

