/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
#ifndef _EI_GLOBAL_H
#define _EI_GLOBAL_H
#include "ei.h"

char **ei_global_names(ei_cnode *ec, int fd, int *count);
int ei_global_whereis(ei_cnode *ec, int fd, const char *name, erlang_pid* pid, char *node);
int ei_global_register(int fd, const char *name, erlang_pid *self);
int ei_global_unregister(ei_cnode *ec, int fd, const char *name);

#endif /* _EI_GLOBAL_H */
