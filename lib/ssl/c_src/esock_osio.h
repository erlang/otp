/*
 *  %CopyrightBegin%
 *  
 *  Copyright Ericsson AB 1999-2009. All Rights Reserved.
 *  
 *  The contents of this file are subject to the Erlang Public License,
 *  Version 1.1, (the "License"); you may not use this file except in
 *  compliance with the License. You should have received a copy of the
 *  Erlang Public License along with this software. If not, it can be
 *  retrieved online at http://www.erlang.org/.
 *  
 *  Software distributed under the License is distributed on an "AS IS"
 *  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 *  the License for the specific language governing rights and limitations
 *  under the License.
 *  
 *  %CopyrightEnd%
 */

#ifndef ESOCK_OSIO_H
#define ESOCK_OSIO_H

extern FD local_read_fd;

#ifdef __WIN32__
int set_binary_mode(void);
int esock_osio_init(void);
void esock_osio_finish(void);
#endif
int set_break_handler(void);
int read_ctrl(unsigned char **ebufp);
int write_ctrl(unsigned char *buf, int len);

#endif
