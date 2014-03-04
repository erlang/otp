/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014. All Rights Reserved.
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

#ifndef _WXE_MEMORY_H
#define	_WXE_MEMORY_H

class wxeMemEnv
{
public:
    wxeMemEnv()
    {
	ref2ptr = (void **) driver_alloc(128*sizeof(void *));
	ref2ptr[0] = NULL;
	next = 1;
	max = 128;
    };
  ~wxeMemEnv()
  { driver_free(ref2ptr); };
  int  next;
  int  max;
  void ** ref2ptr;
  intList  free;
  ErlDrvTermData owner;
};

class wxeRefData {
 public:
   wxeRefData(unsigned int dref, int ttype, int is_new, wxeMemEnv *menv) :
   ref(dref), type(ttype), alloc_in_erl(is_new), memenv(menv), pid(-1) { } ;
   int ref;
   int type;
   // 0 = wxWindow subclasses, 1 = wxObject subclasses
   // 2 = wxDialog subclasses, 3 = allocated wxObjects but not returned from new
   // > 3 classes which lack virtual destr, or are supposed to be allocated on
   //     the stack
   bool alloc_in_erl;
   wxeMemEnv *memenv;
   ErlDrvTermData pid;
};

WX_DECLARE_HASH_MAP(ErlDrvTermData, wxeMemEnv*, wxIntegerHash, wxIntegerEqual, wxeMemMap);

WX_DECLARE_VOIDPTR_HASH_MAP(wxeRefData *, ptrMap);

#endif
