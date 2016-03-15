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

/* The port number is defined in a makefile */

/* Definitions of message codes */

/* Registration and queries */
#define EPMD_ALIVE2_REQ 'x'
#define EPMD_PORT2_REQ 'z'
#define EPMD_ALIVE2_RESP 'y'
#define EPMD_PORT2_RESP 'w'
#define EPMD_NAMES_REQ 'n'

/* Interactive client command codes */
#define EPMD_DUMP_REQ 'd'
#define EPMD_KILL_REQ 'k'
#define EPMD_STOP_REQ 's'
