/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2004-2009. All Rights Reserved.
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
 *

 */
#include "ei.h"
#include "ei_internal.h"
#include "ei_portio.h"

void
ei_send_tock_tmo(int fd, int ms)
{
    char tock[] = {0,0,0,0};
    ei_write_fill_t(fd, tock, sizeof(tock), ms);
}

void
ei_send_tock(int fd)
{
    ei_send_tock_tmo(fd, 0);
}
