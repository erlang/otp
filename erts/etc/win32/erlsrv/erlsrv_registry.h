/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#ifndef _ERLSRV_REGISTRY_H
#define _ERLSRV_REGISTRY_H

typedef struct _reg_entry {
  wchar_t *name;
  DWORD type;
  union {
    wchar_t *string;
    DWORD value;
    struct {
      wchar_t *string;
      wchar_t *unexpanded;
    } expand;
  } data;
} RegEntry;

typedef struct _reg_entry_desc {
  wchar_t *servicename;
  RegEntry *entries;
} RegEntryDesc;

enum {
  StopAction,
  OnFail,
  Machine,
  Env,
  WorkDir,
  Priority,
  SName,
  Name,
  Args,
  DebugType,
  InternalServiceName,
  Comment
};

#define ON_FAIL_IGNORE 0
#define ON_FAIL_RESTART 1
#define ON_FAIL_REBOOT 2
#define ON_FAIL_RESTART_ALWAYS 3 

#define DEBUG_TYPE_NO_DEBUG 0
#define DEBUG_TYPE_NEW 1
#define DEBUG_TYPE_REUSE 2
#define DEBUG_TYPE_CONSOLE 3

extern int num_reg_entries;

RegEntry *empty_reg_tab(void);
void free_keys(RegEntry *keys);
void free_all_keys(RegEntryDesc *descs);
RegEntry *get_keys(wchar_t *servicename);
int set_keys(wchar_t *servicename, RegEntry *keys);
RegEntryDesc *get_all_keys(void);
int remove_keys(wchar_t *servicename);
int register_logkeys(void);
#endif /* _ERLSRV_REGISTRY_H */

