/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2008-2014. All Rights Reserved.
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

bool sendevent(wxEvent * event, ErlDrvTermData port);

class wxeEtype 
{
public: 
  wxeEtype (const char *, int);
  const char *eName;
  int cID;
};

void initEventTable();
int  wxeEventTypeFromAtom(char *etype_atom);

#endif
