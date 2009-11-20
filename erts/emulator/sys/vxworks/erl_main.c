/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"

#if defined(__GNUC__)
/*
 * The generated assembler does the usual trick (relative
 * branch-and-link to next instruction) to get a copy of the
 * instruction ptr. Instead of branching to an explicit zero offset,
 * it branches to the symbol `__eabi' --- which is expected to be
 * undefined and thus zero (if it is defined as non-zero, things will
 * be interesting --- as in the Chinese curse). To shut up the VxWorks
 * linker, we define `__eabi' as zero.
 *
 * This is just a work around. It's really Wind River's GCC's code
 * generator that should be fixed.
 */
__asm__(".equ __eabi, 0");
#endif

void 
erl_main(int argc, char **argv)
{
    erl_start(argc, argv);
}
