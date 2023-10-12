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
#include <wx/wx.h>
#include <wx/aui/aui.h>
#include <wx/geometry.h>
#include <wx/colour.h>
#include <wx/grid.h>
#include <wx/gbsizer.h>
#include <wx/dynarray.h>
#include <wx/html/htmlcell.h>
#include <wx/graphics.h>

#define RT_BUFF_SZ 64



class wxeReturn {

public:
    wxeReturn (wxeMemEnv           *_memenv,
               ErlNifPid           _caller,
	       bool                _isResult=false);

    ~wxeReturn();
    //    void add(ErlDrvTermData type, ErlDrvTermData data);

    ERL_NIF_TERM make_ref(const unsigned int ref, ERL_NIF_TERM className);
    ERL_NIF_TERM make_ref(const unsigned int ref, const char* className);
    ERL_NIF_TERM make_atom(const char* atomName);

    ERL_NIF_TERM make_binary(const char* atomName, size_t size);
    ERL_NIF_TERM make_ext2term(wxeErlTerm * term);
    ERL_NIF_TERM make_ext2term(wxETreeItemData * term);

    ERL_NIF_TERM make_list_strings(size_t size, wxString* atomName);

    ERL_NIF_TERM make_list_objs(const wxWindowList& wx_list, WxeApp *app, const char *cname);
    ERL_NIF_TERM make_list_objs(const wxSizerItemList& wx_list, WxeApp *app, const char *cname);
    ERL_NIF_TERM make_list_objs(const wxMenuItemList& wx_list, WxeApp *app, const char *cname);

    ERL_NIF_TERM make_array_objs(wxGridCellCoordsArray& arr);
    // ERL_NIF_TERM make_array_objs(const wxList& wx_list, WxeApp *app, const char *cname);
    ERL_NIF_TERM make_array_objs(wxArrayTreeItemIds& arr);
    ERL_NIF_TERM make_array_objs(wxAuiPaneInfoArray&, WxeApp *app, const char *cname);

    ERL_NIF_TERM make_bool(int val);
    ERL_NIF_TERM make_int(int val);
    ERL_NIF_TERM make_uint(unsigned int val);
    ERL_NIF_TERM make_double(double val);

    ERL_NIF_TERM make(const wxString s);
    ERL_NIF_TERM make(const wxString* s);
    ERL_NIF_TERM make(wxArrayString val);

    ERL_NIF_TERM make(wxPoint pt);
    ERL_NIF_TERM make( wxPoint2DDouble point2D);

    ERL_NIF_TERM make(wxSize size);
    ERL_NIF_TERM make(wxRect rect);

    ERL_NIF_TERM make(wxRect2DDouble rect2D);

    ERL_NIF_TERM make(wxDateTime dateTime);

    ERL_NIF_TERM make(wxColour colour);
    ERL_NIF_TERM make(wxGraphicsGradientStop colour);

    ERL_NIF_TERM make(wxGridCellCoords val);

    ERL_NIF_TERM make(wxGBPosition val);
    ERL_NIF_TERM make(wxGBSpan val);

    ERL_NIF_TERM make(wxMouseState val);

    ERL_NIF_TERM make(wxArrayInt val);
    ERL_NIF_TERM make(wxArrayDouble val);
    ERL_NIF_TERM make(wxUIntPtr *val);

    ERL_NIF_TERM make(const wxHtmlLinkInfo *val);
    ERL_NIF_TERM make(const wxHtmlLinkInfo &val);

    int send(ERL_NIF_TERM msg);
    void send_callback(int callback, ERL_NIF_TERM args);
    void send_callback(int callback, wxObject *obj, const char *class_name, ERL_NIF_TERM args);

    void reset();

    ErlNifEnv              *env;

private:

    inline ERL_NIF_TERM  make_date(wxDateTime dateTime);
    inline ERL_NIF_TERM  make_time(wxDateTime dateTime);

    ErlNifPid               caller;
    wxMBConvUTF32           utfConverter;
    bool                    isResult;
    wxeMemEnv               *memenv;
};

#endif	/* _WXE_RETURN_H */

