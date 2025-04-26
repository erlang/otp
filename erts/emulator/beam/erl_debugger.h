/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

#ifndef _ERL_DEBUGGER_H_
#define _ERL_DEBUGGER_H_

#define ERTS_DEBUGGER_ENABLED          ((Uint)1 << 0)
#define ERTS_DEBUGGER_LINE_BREAKPOINTS ((Uint)1 << 1)

#define ERTS_DEBUGGER_IS_ENABLED_IN(Var, Flgs) \
    ((Var & (Flgs | ERTS_DEBUGGER_ENABLED)) == (Flgs | ERTS_DEBUGGER_ENABLED))

#define ERTS_DEBUGGER_IS_ENABLED(Flgs) \
    ERTS_DEBUGGER_IS_ENABLED_IN(erts_debugger_flags, Flgs)

extern Uint erts_debugger_flags;

void erts_init_debugger(void);
int erts_send_debugger_event(Process *c_p, Eterm event);

#endif /* _ERL_DEBUGGER_H */
