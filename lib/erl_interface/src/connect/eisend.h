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
#ifndef _EISEND_H
#define _EISEND_H

/* FIXME strange, is this for debugging?! */
#define EI_HAVE_TIMEOUT 1

int ei_send_exit(int fd, const erlang_pid *from, const erlang_pid *to, 
		 const char *msg);
int ei_send_exit_tmo(int fd, const erlang_pid *from, 
		     const erlang_pid *to, 
		     const char *msg, unsigned ms);

/* FIXME ei_send_*() functions not used */
#if 0
int ei_send_link(int fd, const erlang_pid *from, const erlang_pid *to);
int ei_send_unlink(int fd, const erlang_pid *from, const erlang_pid *to);
int ei_send_link_tmo(int fd, const erlang_pid *from, 
		     const erlang_pid *to, unsigned ms);
int ei_send_unlink_tmo(int fd, const erlang_pid *from, 
		       const erlang_pid *to, unsigned ms);
#endif /* Not used */

#endif /* _EISEND_H */
