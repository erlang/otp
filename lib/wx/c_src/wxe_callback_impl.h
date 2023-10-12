/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2021. All Rights Reserved.
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

#ifndef _WXE_CALLBACK_IMPL_H
#define	_WXE_CALLBACK_IMPL_H

void pre_callback();
void handle_event_callback(wxe_me_ref *mr, ErlNifPid process);

#define wxeIntPtr wxIntPtr

/* Fun Callback id */
class wxeEvtListener : public wxEvtHandler
{
public:
   wxeEvtListener(ErlNifPid caller, int req, ERL_NIF_TERM req_type,
		  int funcb, int skip_ev, wxeErlTerm * userData, wxe_me_ref *mr);
   ~wxeEvtListener();
   void forward(wxEvent& event);
   ErlNifPid    listener;
   int          fun_id;
   int          obj;
   ERL_NIF_TERM class_name;
   int          skip;
   wxeErlTerm * user_data;
   wxe_me_ref * me_ref;
};

void clear_cb(wxe_me_ref *, int callback);

// Implementation of wxListCtrlCompare
struct callbackInfo {
    wxe_me_ref * me_ref;
    int callbackID;
};

int wxCALLBACK wxEListCtrlCompare(wxeIntPtr item1, wxeIntPtr item2, wxeIntPtr callbackInfoPtr);

#endif
