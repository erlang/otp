/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2004-2016. All Rights Reserved.
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


#ifndef HIPE_PPC_GLUE_H
#define HIPE_PPC_GLUE_H

#include "hipe_ppc_asm.h"		/* for NR_ARG_REGS, PPC_LEAF_WORDS */
#define NR_LEAF_WORDS			PPC_LEAF_WORDS
#define HIPE_ARCH_CALL_TO_NATIVE	hipe_ppc_call_to_native
#define HIPE_ARCH_RETURN_TO_NATIVE	hipe_ppc_return_to_native
#define HIPE_ARCH_TAILCALL_TO_NATIVE	hipe_ppc_tailcall_to_native
#define HIPE_ARCH_THROW_TO_NATIVE	hipe_ppc_throw_to_native
#include "hipe_risc_glue.h"

#endif /* HIPE_PPC_GLUE_H */
