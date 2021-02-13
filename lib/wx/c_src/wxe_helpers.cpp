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

#include <wx/wx.h>
#include "wxe_impl.h"

/* ****************************************************************************
 * Erlang Commands
 * ****************************************************************************/

wxeCommand::wxeCommand()
{
  env = enif_alloc_env();
}

wxeCommand::~wxeCommand()
{
  if(env)
    enif_free_env(env);
}

void wxeCommand::Init(int u_argc, const ERL_NIF_TERM u_argv[], int u_op, wxe_me_ref *u_mr, ErlNifPid u_caller)
{
  op  = u_op;
  caller = u_caller;

  argc = u_argc;
  for(int i=0; i<argc; i++)
    args[i] = enif_make_copy(env, u_argv[i]);
  me_ref = u_mr;
}


/* ****************************************************************************
 * wxeFifo
 * ****************************************************************************/
wxeFifo::wxeFifo(unsigned int sz)
{
  size = 0;
}

wxeFifo::~wxeFifo() {
  for (wxeCommand *cmd : m_q) {
    delete cmd;
  }
}

wxeCommand * wxeFifo::Get()
{
  if(m_q.empty())
    return NULL;
  else {
    wxeCommand * cmd = m_q.front();
    m_q.pop_front();
    size--;
    return cmd;
  }
}

int wxeFifo::Add(int argc, const ERL_NIF_TERM argv[], int op, wxe_me_ref *mr, ErlNifPid caller)
{
  wxeCommand * curr = new wxeCommand();
  curr->Init(argc, argv, op, mr, caller);
  m_q.push_back(curr);
  size++;
  return size;
}

std::list<wxeCommand *>::iterator wxeFifo::DelQueue(std::list<wxeCommand *>::iterator it)
{
  size--;
  return m_q.erase(it);
}


void wxeFifo::DeleteCmd(wxeCommand *orig)
{
  delete orig;
}

unsigned int wxeFifo::Size()
{
  return size;
}

void wxeFifo::Append(wxeCommand *orig)
{
  wxeCommand * curr = new wxeCommand();
  curr->op  = orig->op;
  curr->caller = orig->caller;
  curr->argc = orig->argc;
  for(int i=0; i<curr->argc; i++)
    curr->args[i] = orig->args[i];
  ErlNifEnv * temp = curr->env;
  curr->env = orig->env;
  orig->env = temp;
  curr->me_ref = orig->me_ref;
  m_q.push_back(curr);
}
