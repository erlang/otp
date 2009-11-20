/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2009. All Rights Reserved.
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

#ifndef __WXE_EVENT_H__
#define __WXE_EVENT_H__

#include "wxe_driver.h"

class wxeEtype 
{
public: 
  wxeEtype (const char *, int);
  const char *eName;
  int cID;
};

void initEventTable();
int  wxeEventTypeFromAtom(char *etype_atom);

/* Fun Callback id */ 
class wxeCallbackData : public wxObject
{
public:
   wxeCallbackData(ErlDrvTermData caller, void *req, char *req_type, 
		   int funcb, int skip_ev, wxeErlTerm * userData);
   ~wxeCallbackData();
   ErlDrvTermData listener;
   int          fun_id;
   void *       obj;
   char         class_name[40];
   int          skip;
   wxeErlTerm * user_data;
};

/* One EvtListener per listening erlang process */ 
/* If callbacks are used the receiver is wxe_master process */ 
/* and a wxeEvtListener pre callback is registered */ 
class wxeEvtListener : public wxEvtHandler
{
public:
   wxeEvtListener(ErlDrvPort Thisport) : port(Thisport) 
      {} 
   // {fprintf(stderr, "Creating %x\r\n", (unsigned int) this); fflush(stderr);}
   void forward(wxEvent& event);
   ~wxeEvtListener() {
      ((WxeApp *)wxTheApp)->clearPtr(this);
      // fprintf(stderr, "Deleteing %x\r\n", (unsigned int) this); fflush(stderr);
   };
   ErlDrvPort port;
};

#endif
