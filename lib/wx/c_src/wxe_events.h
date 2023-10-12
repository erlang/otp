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

#ifndef __WXE_EVENT_H__
#define __WXE_EVENT_H__

// #include "wxe_nif.h"

bool sendevent(wxEvent * event, wxeMemEnv *env);

typedef struct {
    int ev_type;
    int class_id;
    const char * ev_name;
    const char* ev_class;
    const char* ev_rec;
} wxe_evInfo;

class wxeEtype 
{
public: 
    wxeEtype (ErlNifEnv *env, wxe_evInfo* info)
    {
        cID = info->class_id;
        evName = enif_make_atom(env, info->ev_name);
        evClass = enif_make_atom(env, info->ev_class);
        evRecord = enif_make_atom(env, info->ev_rec);
    } ;
  int cID;
  ERL_NIF_TERM evName;
  ERL_NIF_TERM evClass;
  ERL_NIF_TERM evRecord;
};

void initEventTable();
int  wxeEventTypeFromAtom(ERL_NIF_TERM atom);

#endif
