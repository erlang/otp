/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2013. All Rights Reserved.
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
#ifndef ERL_TO_ERL_H
#define ERL_TO_ERL_H

#define TO_ERL_USAGE "to_erl [-h|-F] %s\n"			\
  "\t-h\tThis help text.\n"						\
  "\t-f\tForce connection even though pipe is locked by other to_erl process."

int to_erl(int argc, char **argv);

#endif
