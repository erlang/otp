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

#ifndef _WXE_HELPER_H
#define	_WXE_HELPER_H

#include "wxe_memory.h"
#include <vector>
#include <deque>

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
  virtual ~wxeCommand();
  void Init(int argc, const ERL_NIF_TERM argv[], int op, wxe_me_ref *mr, ErlNifPid caller);

  ErlNifPid    caller;
  int          op;
  ErlNifEnv    *env;
  int          argc;
  ERL_NIF_TERM args[16];
  wxe_me_ref   * me_ref;
};

class wxeFifo {
 public:
  wxeFifo(unsigned int size);
  virtual ~wxeFifo();

  int Add(int argc, const ERL_NIF_TERM argv[], int, wxe_me_ref *, ErlNifPid caller);
  void Append(wxeCommand *Other);
  void DelQueue(unsigned int it);
  void DeleteCmd(wxeCommand *);
  unsigned int Size();

  wxeCommand * Get();

  std::deque <wxeCommand *> m_q;
  std::vector <wxeCommand *> free;
  unsigned int size;  // keep own counter list::size() is not O(1) in old impl
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
