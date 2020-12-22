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
}

wxeCommand::~wxeCommand()
{
  Delete();
}

void wxeCommand::Delete()
{
  op = -2;
  enif_clear_env(env);
}

/* ****************************************************************************
 * wxeFifo
 * ****************************************************************************/
wxeFifo::wxeFifo(unsigned int sz)
{
  m_q = (wxeCommand *) enif_alloc(sizeof(wxeCommand) * sz);
  m_orig_sz = sz;
  m_max = sz;
  m_n = 0;
  m_first = 0;
  cb_start = 0;
  m_old = NULL;
  for(unsigned int i = 0; i < sz; i++) {
    m_q[i].op = -1;
    m_q[i].env = enif_alloc_env();
  }
}

wxeFifo::~wxeFifo() {
  // dealloc all memory buffers
  for(unsigned int i=0; i < m_max; i++) {
    enif_free_env(m_q[i].env);
  }
  enif_free(m_q);
}

wxeCommand * wxeFifo::Get()
{
  unsigned int pos;
  do {
    if(m_n <= 0)
      return NULL;

    pos = m_first++;
    m_n--;
    m_first %= m_max;
  } while(m_q[pos].op < 0);
  return &m_q[pos];
}

wxeCommand * wxeFifo::Peek(unsigned int *i)
{
  unsigned int pos;
  do {
    if(*i >= m_n || m_n <= 0)
      return NULL;
    pos = (m_first+*i) % m_max;
    (*i)++;
  } while(m_q[pos].op < 0);
  return &m_q[pos];
}

int wxeFifo::Add(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], int op, wxe_me_ref *mr, ErlNifPid caller)
{
  int i;
  unsigned int pos;
  wxeCommand *curr;

  if(m_n == (m_max-1)) { // resize
    Realloc();
  }

  pos = (m_first + m_n) % m_max;
  m_n++;

  curr = &m_q[pos];
  curr->op  = op;
  curr->caller = caller;

  curr->argc = argc;
  for(i=0; i<argc; i++)
    curr->args[i] = enif_make_copy(curr->env, argv[i]);

  curr->me_ref = mr;
  return m_n;
}

void wxeFifo::Append(wxeCommand *orig)
{
  unsigned int pos;
  wxeCommand *curr;
  ErlNifEnv *temp;
  if(m_n == (m_max-1)) { // resize
    Realloc();
  }

  pos = (m_first + m_n) % m_max;
  m_n++;

  curr = &m_q[pos];
  curr->op = orig->op;
  if(curr->op == -1) return;
  curr->caller = orig->caller;

  temp = orig->env;
  orig->env = curr->env;
  curr->env = temp;

  curr->argc = orig->argc;
  for(int i=0; i < 16; i++) {
    curr->args[i] = orig->args[i];
  }
  curr->me_ref = orig->me_ref;
  orig->op = -1;
}

void wxeFifo::Realloc()
{
  unsigned int i, j;
  unsigned int growth = m_orig_sz / 2;
  unsigned int new_sz = growth + m_max;
  unsigned int max  = m_max;
  unsigned int first = m_first;
  unsigned int n = m_n;
  wxeCommand * old = m_q;
  wxeCommand * queue = (wxeCommand *)enif_alloc(new_sz*sizeof(wxeCommand));

  // fprintf(stderr, "\r\nrealloc qsz %d\r\n", new_sz);fflush(stderr);

  m_max=new_sz;
  m_first = 0;
  m_n=0;
  m_q = queue;

  for(i=0; i < new_sz; i++) {
    m_q[i].env = NULL;
  }
  for(i=0; i < n; i++) {
    unsigned int pos = (i+first)%max;
    if(old[pos].op >= 0)
      Append(&old[pos]);
  }
  // Optimize later
  for(i=0, j=m_n; i < n; i++) {
    if(old[i].env)
      m_q[j++].env = old[i].env;
  }
  for(i = m_n; i < new_sz; i++) { // Reset the rest
    m_q[i].op = -1;
    if(!m_q[i].env)
      m_q[i].env = enif_alloc_env();
  }
  for(i=0; i < new_sz; i++) {
    if(m_q[i].env == NULL) {
      fprintf(stderr, "i %d \r\n", i);
      fflush(stderr);
      abort();
    }
  }
  // Can not free old queue here it can be used in the wx thread
  m_old = old;
}

// Strip end of queue if ops are already taken care of, avoids reallocs
void wxeFifo::Strip()
{
  while((m_n > 0) && (m_q[(m_first + m_n - 1)%m_max].op < -1)) {
    m_n--;
  }
}

unsigned int wxeFifo::Cleanup(unsigned int def)
{
  if(m_old) {
    enif_free(m_old);
    m_old = NULL;
    // Realloced we need to start from the beginning
    return 0;
  } else {
    return def < cb_start? def : cb_start;
  }
}
