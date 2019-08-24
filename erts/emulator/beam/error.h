/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
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

#ifndef __ERROR_H__
#define __ERROR_H__

#include "code_ix.h"

/*
 * There are three primary exception classes:
 *
 *	- exit			Process termination - not an error.
 *	- error			Error (adds stacktrace; will be logged).
 *	- thrown		Nonlocal return (turns into a 'nocatch'
 *				error if not caught by the process).
 *
 * In addition, we define a number of exit codes as a convenient
 * short-hand: instead of building the error descriptor term at the time
 * the exception is raised, it is built as necessary when the exception
 * is handled. Examples are EXC_NORMAL, EXC_BADARG, EXC_BADARITH, etc.
 * Some of these have convenient aliases, like BADARG and BADARITH.
 */

/*
 * Exception class tags (indices into the 'exception_tag' array)
 */
#define EXTAG_OFFSET	0
#define EXTAG_BITS	2

#define EXTAG_ERROR	0
#define EXTAG_EXIT	1
#define EXTAG_THROWN	2

#define NUMBER_EXC_TAGS 3	/* The number of exception class tags */

/*
 * Index to the 'exception class tag' table.
 */
#define EXC_CLASSBITS	((1<<EXTAG_BITS)-1)
#define GET_EXC_CLASS(x) ((x) & EXC_CLASSBITS)

/*
 * Exit code flags
 *
 * These flags make is easier and quicker to decide what to do with the
 * exception in the early stages, before a handler is found, and also
 * maintains some separation between the class tag and the actions.
 */
#define EXF_OFFSET	EXTAG_BITS
#define EXF_BITS	7

#define EXF_PANIC	(1<<(0+EXF_OFFSET))	/* ignore catches */
#define EXF_THROWN	(1<<(1+EXF_OFFSET))	/* nonlocal return */
#define EXF_LOG		(1<<(2+EXF_OFFSET))	/* write to logger on termination */
#define EXF_NATIVE	(1<<(3+EXF_OFFSET))	/* occurred in native code */
#define EXF_SAVETRACE	(1<<(4+EXF_OFFSET))	/* save stack trace in internal form */
#define EXF_ARGLIST	(1<<(5+EXF_OFFSET))	/* has arglist for top of trace */
#define EXF_RESTORE_NIF	(1<<(6+EXF_OFFSET))	/* restore original bif/nif */

#define EXC_FLAGBITS	(((1<<(EXF_BITS+EXF_OFFSET))-1) \
			 & ~((1<<(EXF_OFFSET))-1))

/*
 * The primary fields of an exception code
 */
#define EXF_PRIMARY	(EXF_PANIC | EXF_THROWN | EXF_LOG | EXF_NATIVE)
#define PRIMARY_EXCEPTION(x) ((x) & (EXF_PRIMARY | EXC_CLASSBITS))
#define NATIVE_EXCEPTION(x) ((x) | EXF_NATIVE)

/*
 * Error code used for indexing into
 * the short-hand error descriptor table.
 */
#define EXC_OFFSET	(EXF_OFFSET+EXF_BITS)
#define EXC_BITS	5

#define EXC_INDEXBITS	(((1<<(EXC_BITS+EXC_OFFSET))-1) \
			 & ~((1<<(EXC_OFFSET))-1))

#define GET_EXC_INDEX(x) (((x) & EXC_INDEXBITS) >> EXC_OFFSET)

/*
 * Exit codes used for raising a fresh exception. The primary exceptions
 * share index 0 in the descriptor table. EXC_NULL signals that no
 * exception has occurred. The primary exit codes EXC_EXIT, EXC_ERROR
 * and EXC_THROWN are the basis for all other exit codes, and must
 * always have the EXF_SAVETRACE flag set so that a trace is saved
 * whenever a new exception occurs; the flag is then cleared.
 */
#define EXC_NULL 0			/* Initial value for p->freason */
#define EXC_PRIMARY (0 | EXF_SAVETRACE)
#define EXC_ERROR  (EXC_PRIMARY | EXTAG_ERROR | EXF_LOG)
					/* Generic error (exit term
					 * in p->fvalue) */
#define EXC_EXIT   (EXC_PRIMARY | EXTAG_EXIT)
					/* Generic exit (exit term
					 * in p->fvalue) */
#define EXC_THROWN (EXC_PRIMARY | EXTAG_THROWN | EXF_THROWN)
					/* Generic nonlocal return
					 * (thrown term in p->fvalue) */

#define EXC_ERROR_2 (EXC_ERROR | EXF_ARGLIST)
					/* Error with given arglist term
					 * (exit reason in p->fvalue) */

#define EXC_NORMAL		((1 << EXC_OFFSET) | EXC_EXIT)
					/* Normal exit (reason 'normal') */
#define EXC_INTERNAL_ERROR	((2 << EXC_OFFSET) | EXC_ERROR | EXF_PANIC)
					/* Things that shouldn't happen */
#define EXC_BADARG		((3 << EXC_OFFSET) | EXC_ERROR)
					/* Bad argument to a BIF */
#define EXC_BADARITH		((4 << EXC_OFFSET) | EXC_ERROR)
					/* Bad arithmetic */
#define EXC_BADMATCH		((5 << EXC_OFFSET) | EXC_ERROR)
					/* Bad match in function body */
#define EXC_FUNCTION_CLAUSE	((6 << EXC_OFFSET) | EXC_ERROR)
					 /* No matching function head */
#define EXC_CASE_CLAUSE		((7 << EXC_OFFSET) | EXC_ERROR)
					/* No matching case clause */
#define EXC_IF_CLAUSE		((8 << EXC_OFFSET) | EXC_ERROR)
					/* No matching if clause */
#define EXC_UNDEF		((9 << EXC_OFFSET) | EXC_ERROR)
				 	/* No farity that matches */
#define EXC_BADFUN		((10 << EXC_OFFSET) | EXC_ERROR)
					/* Not an existing fun */
#define EXC_BADARITY		((11 << EXC_OFFSET) | EXC_ERROR)
					/* Attempt to call fun with
					 * wrong number of arguments. */
#define EXC_TIMEOUT_VALUE	((12 << EXC_OFFSET) | EXC_ERROR)
					/* Bad time out value */
#define EXC_NOPROC		((13 << EXC_OFFSET) | EXC_ERROR)
					/* No process or port */
#define EXC_NOTALIVE		((14 << EXC_OFFSET) | EXC_ERROR)
					/* Not distributed */
#define EXC_SYSTEM_LIMIT	((15 << EXC_OFFSET) | EXC_ERROR)
					/* Ran out of something */
#define EXC_TRY_CLAUSE		((16 << EXC_OFFSET) | EXC_ERROR)
					/* No matching try clause */
#define EXC_NOTSUP		((17 << EXC_OFFSET) | EXC_ERROR)
					/* Not supported */

#define EXC_BADMAP		((18 << EXC_OFFSET) | EXC_ERROR)
					/* Bad map */

#define EXC_BADKEY		((19 << EXC_OFFSET) | EXC_ERROR)
					/* Bad key in map */

#define NUMBER_EXIT_CODES 20	/* The number of exit code indices */

/*
 * Internal pseudo-error codes.
 */
#define TRAP		(1 << EXC_OFFSET)	/* BIF Trap to erlang code */

/*
 * Aliases for some common exit codes.
 */
#define BADARG EXC_BADARG
#define BADARITH EXC_BADARITH
#define BADKEY EXC_BADKEY
#define BADMAP EXC_BADMAP
#define BADMATCH EXC_BADMATCH
#define SYSTEM_LIMIT EXC_SYSTEM_LIMIT


/*
 * Pseudo error codes (these are never seen by the user).
 */
#define TLOAD_OK 0              /* The threaded code linking was successful */
#define TLOAD_MAGIC_NUMBER 1    /* Wrong kind of object file */
#define TLOAD_FORMAT 2          /* Format error while reading object code */
#define TLOAD_MODULE 3          /* Module name in object code does not match */
#define TLOAD_SIZE 4            /* Given size in object code differs from actual size */

/*
 * The exception stack trace parameters.
 */
#define MAX_BACKTRACE_SIZE 64    /* whatever - just not too huge */
#define DEFAULT_BACKTRACE_SIZE 8

/*
 * The table translating an exception code to an atom.
 */
extern Eterm error_atom[NUMBER_EXIT_CODES];

/*
 * The exception tag table.
 */
extern Eterm exception_tag[NUMBER_EXC_TAGS];

/* 
 * The quick-saved stack trace structure
 */
struct StackTrace {
    Eterm header;	/* bignum header - must be first in struct */
    Eterm freason; /* original exception reason is saved in the struct */
    BeamInstr* pc;
    ErtsCodeMFA* current;
    int depth;	/* number of saved pointers in trace[] */
    BeamInstr *trace[1];  /* varying size - must be last in struct */
};

#endif /* __ERROR_H__ */
