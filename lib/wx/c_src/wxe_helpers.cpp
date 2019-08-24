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
  int n = 0;

  if(buffer) {
    while(bin[n].from) {
      if(bin[n].bin)
	driver_free_binary(bin[n].bin);
      n++;
    }
    if(len > 64)
      driver_free(buffer);
    buffer = NULL;
  }
  op = -2;
}

/* ****************************************************************************
 * wxeFifo
 * ****************************************************************************/
wxeFifo::wxeFifo(unsigned int sz)
{
  m_q = (wxeCommand *) driver_alloc(sizeof(wxeCommand) * sz);
  m_orig_sz = sz;
  m_max = sz;
  m_n = 0;
  m_first = 0;
  cb_start = 0;
  m_old = NULL;
  for(unsigned int i = 0; i < sz; i++) {
    m_q[i].buffer = NULL;
    m_q[i].op = -1;
  }
}

wxeFifo::~wxeFifo() {
  // dealloc all memory buffers
  driver_free(m_q);
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


int wxeFifo::Add(int fc, char * cbuf,int buflen, wxe_data *sd)
{
  unsigned int pos;
  wxeCommand *curr;

  int n = 0;

  if(m_n == (m_max-1)) { // resize
    Realloc();
  }

  pos = (m_first + m_n) % m_max;
  m_n++;

  curr = &m_q[pos];
  curr->caller = driver_caller(sd->port_handle);
  curr->port   = sd->port;
  curr->op  = fc;
  curr->len = buflen;
  curr->bin[0].from = 0;
  curr->bin[1].from = 0;
  curr->bin[2].from = 0;

  if(cbuf) {
    if(buflen > 64)
      curr->buffer = (char *) driver_alloc(buflen);
    else
      curr->buffer = curr->c_buf;
    memcpy((void *) curr->buffer, (void *) cbuf, buflen);

    for(unsigned int i=0; i<sd->max_bins; i++) {
      if(curr->caller == sd->bin[i].from) {
	sd->bin[i].from = 0; // Mark copied
	curr->bin[n].bin  = sd->bin[i].bin;
	curr->bin[n].base = sd->bin[i].base;
	curr->bin[n].size = sd->bin[i].size;
	curr->bin[n].from = 1;
	n++;
      }
    }
  } else {   // No-op only PING currently
    curr->buffer = NULL;
  }
  return m_n;
}

void wxeFifo::Append(wxeCommand *orig)
{
  unsigned int pos;
  wxeCommand *curr;
  if(m_n == (m_max-1)) { // resize
    Realloc();
  }

  pos = (m_first + m_n) % m_max;
  m_n++;

  curr = &m_q[pos];
  curr->op = orig->op;
  if(curr->op == -1) return;
  curr->caller = orig->caller;
  curr->port   = orig->port;
  curr->len = orig->len;
  curr->bin[0] = orig->bin[0];
  curr->bin[1] = orig->bin[1];
  curr->bin[2] = orig->bin[2];

  if(orig->len > 64)
    curr->buffer = orig->buffer;
  else {
    curr->buffer = curr->c_buf;
    memcpy((void *) curr->buffer, (void *) orig->buffer, orig->len);
  }
  orig->op = -1;
  orig->buffer = NULL;
  orig->bin[0].from = 0;
}

void wxeFifo::Realloc()
{
  unsigned int i;
  unsigned int growth = m_orig_sz / 2;
  unsigned int new_sz = growth + m_max;
  unsigned int max  = m_max;
  unsigned int first = m_first;
  unsigned int n = m_n;
  wxeCommand * old = m_q;
  wxeCommand * queue = (wxeCommand *)driver_alloc(new_sz*sizeof(wxeCommand));

  // fprintf(stderr, "\r\nrealloc qsz %d\r\n", new_sz);fflush(stderr);

  m_max=new_sz;
  m_first = 0;
  m_n=0;
  m_q = queue;

  for(i=0; i < n; i++) {
    unsigned int pos = (i+first)%max;
    if(old[pos].op >= 0)
      Append(&old[pos]);
  }

  for(i = m_n; i < new_sz; i++) { // Reset the rest
    m_q[i].buffer = NULL;
    m_q[i].op = -1;
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
    driver_free(m_old);
    m_old = NULL;
    // Realloced we need to start from the beginning
    return 0;
  } else {
    return def < cb_start? def : cb_start;
  }
}

/* ****************************************************************************
 * TreeItemData
 * ****************************************************************************/

wxETreeItemData::wxETreeItemData(int sz, char * data) {
  size = sz;
  bin = (char *) driver_alloc(sz);
  memcpy(bin, data, sz);
}

wxETreeItemData::~wxETreeItemData()
{
  driver_free(bin);
}
