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
    while(bin[n]) {
      if(bin[n]->bin)
	driver_free_binary(bin[n]->bin);
      driver_free(bin[n++]);
    }
    if(len > 64)
      driver_free(buffer);
    buffer = NULL;
    op = -1;
  }
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
  if(m_n > 0) {
    pos = m_first++;
    m_n--;
    m_first %= m_max;
    return &m_q[pos];
  }
  return NULL;
}

void wxeFifo::Add(int fc, char * cbuf,int buflen, wxe_data *sd)
{
  unsigned int pos;
  wxeCommand *curr;

  WXEBinRef *temp, *start, *prev;
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
  curr->bin[0] = NULL;
  curr->bin[1] = NULL;
  curr->bin[2] = NULL;

  if(cbuf) {
    if(buflen > 64)
      curr->buffer = (char *) driver_alloc(buflen);
    else
      curr->buffer = curr->c_buf;
    memcpy((void *) curr->buffer, (void *) cbuf, buflen);

    temp = sd->bin;

    prev  = NULL;
    start = temp;

    while(temp) {
      if(curr->caller == temp->from) {
	curr->bin[n++] = temp;
	if(prev) {
	  prev->next = temp->next;
	} else {
	  start = temp->next;
	}
	temp = temp->next;
      } else {
	prev = temp;
	temp = temp->next;
      }
    }
    sd->bin = start;
  } else {   // No-op only PING currently
    curr->buffer = NULL;
  }
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
  curr->caller = orig->caller;
  curr->port   = orig->port;
  curr->op = orig->op;
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
  orig->bin[0] = NULL;
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

  m_max=new_sz;
  m_first = 0;
  m_n=0;
  m_q = queue;

  for(i=0; i < n; i++) {
    unsigned int pos = i+first;
    if(old[pos%max].op >= 0) {
      Append(&old[pos%max]);
    }
  }
  for(i = m_n; i < new_sz; i++) { // Reset the rest
    m_q[i].buffer = NULL;
    m_q[i].op = -1;
  }
  // Can not free old queue here it can be used in the wx thread
  m_old = old;
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
