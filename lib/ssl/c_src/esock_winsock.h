/*<copyright>
 * <year>2003-2008</year>
 * <holder>Ericsson AB, All Rights Reserved</holder>
 *</copyright>
 *<legalnotice>
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
 * The Initial Developer of the Original Code is Ericsson AB.
 *</legalnotice>
 */
/*
 * Purpose:  Control winsock version and setting of FD_SETSIZE.
 *
 */

/* Maybe set FD_SETSIZE */

#ifdef ESOCK_WINSOCK2
#include <winsock2.h>
#else
#include <winsock.h>
/* These are defined in winsock2.h but not in winsock.h */
#define SD_RECEIVE      0x00
#define SD_SEND         0x01
#define SD_BOTH         0x02
#endif

