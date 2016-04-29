/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

