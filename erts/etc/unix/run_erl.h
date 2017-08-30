/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2008-2016. All Rights Reserved.
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

/*
 * The protocol version number used between to_erl and run_erl.
 */
#define RUN_ERL_HI_VER 1  /* My preferred protocol version */
#define RUN_ERL_LO_VER 0  /* The lowest version I accept to talk with */

/* Version history:
 * 0: Older, without version handshake
 * 1: R12B-3, version handshake + window size ctrl
 */

