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
#ifndef _ELOG_REGISTRY_H
#define _ELOG_REGISTRY_H
/*
 * Module: elog_registry
 * Purpouse: Handle the registry reading and writing.
 */

#include "elog_global.h"

/*
 * A structure for the registry keys.
 */
typedef struct _reg_keys {
  char facility_name[MAX_FACILITY_NAME];
  DWORD latest_record;
  DWORD latest_time;
} RegKeys;

MessageFiles get_messagefiles(const char *category, const char *facility,
			      char *eventbuff, int eventbufflen,
			      char *parambuff, int parambufflen);
/*
 * Return the EventMessageFile and then ParameterMessageFile for
 * a facility under a category.
 * The buffers should be at least MAX_PATH long to make
 * sure the filenames can be stored.
 * If facility is not found, both fields in the record are NULL,
 * if the ParameterMessageFile is not found, the param filed of the record 
 * is NULL.
 */

int create_regkeys(char *identifier);
/* 
 * Creates registry entries for this log identifier 
 */
int set_regkeys(char *identifier, RegKeys *keys, int num_keys);
/*
 * Updates the registry keys for the identifier. Multiple
 * categories can be specified in the keys array.
 */
int get_regkeys(char *identifier, RegKeys *keys, int *num_keys /* in out */);
/*
 * Reads the keys from the registry database for this
 * identifier, creating them if needed. The values
 * for the different categories are stored in the 
 * keys array as long as there is place.
 */

#endif /* _ELOG_REGISTRY_H */
