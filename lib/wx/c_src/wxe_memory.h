/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2014-2016. All Rights Reserved.
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
   // 4 = wxGraphicsObjects or it's subclasses that can no be overloaded
   // 8 = wxObjects that should always be deleted directly (wxDC derivates)
   // > 10 classes which lack virtual destr, or are supposed to be allocated on
   //     the stack
   bool alloc_in_erl;
   wxeMemEnv *memenv;
   ErlDrvTermData pid;
};

WX_DECLARE_HASH_MAP(ErlDrvTermData, wxeMemEnv*, wxIntegerHash, wxIntegerEqual, wxeMemMap);

WX_DECLARE_VOIDPTR_HASH_MAP(wxeRefData *, ptrMap);

#endif
