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

#ifndef _WXE_HELPER_H
#define	_WXE_HELPER_H

#include "wxe_memory.h"
#include <list>

DECLARE_EVENT_TYPE(wxeEVT_META_COMMAND, -1)

class wxeMetaCommand : public wxEvent
{
 public:
 wxeMetaCommand(ErlNifPid self, int EvId, wxe_me_ref *me)
     : wxEvent(EvId, wxeEVT_META_COMMAND)
    {  caller = self; me_ref = me; } ;
 wxeMetaCommand(const wxeMetaCommand& event)
     : wxEvent(event)
    {  caller = event.caller; me_ref = event.me_ref; };
    virtual ~wxeMetaCommand() {};
    virtual wxEvent *Clone() const { return new wxeMetaCommand(*this); }

    ErlNifPid  caller;
    wxe_me_ref * me_ref;
};

class wxeCommand
{
 public:
    wxeCommand();
    virtual ~wxeCommand(); // Use Delete()

    wxeCommand * Save(int Op) { op = Op; return this; };
    void Delete();

    ErlNifPid    caller;
    int          op;   // -1 in use by wx thread; -2 deleted/unused
    ErlNifEnv    *env;
    int          argc;
    ERL_NIF_TERM args[16];
    wxe_me_ref   * me_ref;
};

class wxeFifo {
 public:
  wxeFifo(unsigned int size);
  virtual ~wxeFifo();

  int Add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int, wxe_me_ref *, ErlNifPid caller);
  void Append(wxeCommand *Other);

  wxeCommand * Get();
  // wxeCommand * Peek(unsigned int *item);

  std::list <wxeCommand *> m_q;
};

class wxeErlTerm : public wxClientData
{
 public:
    wxeErlTerm(ERL_NIF_TERM in_term)
    {
        env = enif_alloc_env();
        term = enif_make_copy(env, in_term);
    } ;
    ~wxeErlTerm() { enif_free_env(env); };

    ErlNifEnv *env;
    ERL_NIF_TERM term;
};

class wxETreeItemData : public wxTreeItemData
{
 public:
    wxETreeItemData(ERL_NIF_TERM in_term)
    {
        env = enif_alloc_env();
        term = enif_make_copy(env, in_term);
    } ;
    ~wxETreeItemData() { enif_free_env(env); };
    ErlNifEnv *env;
    ERL_NIF_TERM term;
};

#endif
