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

wxeCommand::wxeCommand(int fc,char * cbuf,int buflen, wxe_data *sd)
  : wxObject()
{
  WXEBinRef *temp, *start, *prev;
  int n = 0;
  ref_count = 1;
  caller = driver_caller(sd->port_handle);
  port   = sd->port;
  op = fc;
  len = buflen;
  bin[0] = NULL;
  bin[1] = NULL;
  bin[2] = NULL;

  if(cbuf) {
    buffer = (char *) driver_alloc(len);
    memcpy((void *) buffer, (void *) cbuf, len);;

    temp = sd->bin;

    prev  = NULL;
    start = temp;

    while(temp) {
      if(caller == temp->from) {
	bin[n++] = temp;
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
    buffer = NULL;
  }
}

wxeCommand::~wxeCommand() {
  int n = 0;
  if(buffer) {
    while(bin[n]) {
      if(bin[n]->bin)
	driver_free_binary(bin[n]->bin);
      driver_free(bin[n++]);
    }
    driver_free(buffer);
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
