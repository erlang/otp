/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2018. All Rights Reserved.
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
 * hipe_native_bif.c
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_bits.h"
#include "erl_binary.h"
#include "hipe_mode_switch.h"
#include "hipe_native_bif.h"
#include "hipe_arch.h"
#include "hipe_stack.h"
#include "erl_proc_sig_queue.h"

/*
 * These are wrappers for BIFs that may trigger a native
 * stack walk with p->hipe.narity != 0.
 */

/* for -Wmissing-prototypes :-( */
extern Eterm nbif_impl_hipe_erts_internal_check_process_code_1(NBIF_ALIST_1);
extern Eterm nbif_impl_hipe_show_nstack_1(NBIF_ALIST_1);

/* Used when a BIF can trigger a stack walk. */
static __inline__ void hipe_set_narity(Process *p, unsigned int arity)
{
    p->hipe.narity = arity;
}

/* Called via standard_bif_interface_2 */
Eterm nbif_impl_hipe_erts_internal_check_process_code_1(NBIF_ALIST_1)
{
    Eterm ret;

    hipe_set_narity(BIF_P, 1);
    ret = nbif_impl_erts_internal_check_process_code_1(NBIF_CALL_ARGS);
    hipe_set_narity(BIF_P, 0);
    return ret;
}

/* Called via standard_bif_interface_1 */
Eterm nbif_impl_hipe_show_nstack_1(NBIF_ALIST_1)
{
    Eterm ret;

    hipe_set_narity(BIF_P, 1);
    ret = nbif_impl_hipe_bifs_show_nstack_1(NBIF_CALL_ARGS);
    hipe_set_narity(BIF_P, 0);
    return ret;
}

/*
 * This is called when inlined heap allocation in native code fails.
 * The 'need' parameter is the number of heap words needed.
 * The value is tagged as a fixnum to avoid untagged data on
 * the x86 stack while the gc is running.
 */
void hipe_gc(Process *p, Eterm need)
{
    hipe_set_narity(p, 1);
    erts_garbage_collect(p, unsigned_val(need), NULL, 0);
    hipe_set_narity(p, 0);
}

/* This is like the OP_setTimeout JAM instruction.
 *  Transformation to the BEAM instruction wait_timeout_fs
 *  has begun.
 * XXX: BUG: native code should check return status
 */
BIF_RETTYPE nbif_impl_hipe_set_timeout(NBIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm timeout_value = BIF_ARG_1;
    /* XXX: This should be converted to follow BEAM conventions,
     * but that requires some compiler changes.
     *
     * In BEAM, set_timeout saves TWO CP values, and suspends.
     * p->def_arg_reg[0] and p->i are both defined and used.
     * If a message arrives, BEAM resumes at p->i.
     * If a timeout fires, BEAM resumes at p->def_arg_reg[0].
     * (See erts_set_proc_timer() and proc_timeout_common() in
     * erl_hl_timer.c.)
     *
     * Here we set p->def_arg_reg[0] to hipe_beam_pc_resume.
     * Assuming our caller invokes suspend immediately after
     * our return, then hipe_mode_switch() will also set
     * p->i to hipe_beam_pc_resume. Thus we'll resume in the same
     * way regardless of the cause (message or timeout).
     * hipe_mode_switch() checks for F_TIMO and returns a
     * flag to native code indicating the cause.
     */

    /*
     * def_arg_reg[0] is (re)set unconditionally, in case this is the
     * 2nd/3rd/... iteration through the receive loop: in order to pass
     * a boolean flag to native code indicating timeout or new message,
     * our mode switch has to clobber def_arg_reg[0]. This is ok, but if
     * we re-suspend (because we ignored a received message) we also have
     * to reinitialise def_arg_reg[0] with the BEAM resume label.
     *
     * XXX: A better solution would be to pass two parameters to
     * set_timeout: the timeout and the on-timeout resume label.
     * We could put the resume label in def_arg_reg[1] and resume
     * at it without having to load a flag in a register and generate
     * code to test it. Requires a HiPE compiler change though.
     */
    p->def_arg_reg[0] = (Eterm) hipe_beam_pc_resume;

    /*
     * If we have already set the timer, we must NOT set it again.  Therefore,
     * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
     */
    if (p->flags & (F_INSLPQUEUE | F_TIMO))
	return NIL;	/* caller had better call nbif_suspend ASAP! */

    if (timeout_value == am_infinity) {
	/* p->flags |= F_TIMO; */	/* XXX: nbif_suspend_msg_timeout */
    }
    else {
	int tres = erts_set_proc_timer_term(p, timeout_value);
	if (tres != 0) { /* Wrong time */
	    if (p->hipe_smp.have_receive_locks) {
		p->hipe_smp.have_receive_locks = 0;
		erts_proc_unlock(p, ERTS_PROC_LOCKS_MSG_RECEIVE);
	    }
	    BIF_ERROR(p, EXC_TIMEOUT_VALUE);
	}
    }
    return NIL;	/* caller had better call nbif_suspend ASAP! */
}

/* This is like the remove_message BEAM instruction
 */
void hipe_select_msg(Process *p)
{
    ErtsMessage *msgp;

    msgp = PEEK_MESSAGE(p);
    UNLINK_MESSAGE(p, msgp);	/* decrements global 'erts_proc_tot_mem' variable */
    JOIN_MESSAGE(p);
    CANCEL_TIMER(p);		/* calls erts_cancel_proc_timer() */
    erts_save_message_in_proc(p, msgp);
    p->flags &= ~F_DELAY_GC;
    if (ERTS_IS_GC_DESIRED(p)) {
	/*
	 * We want to GC soon but we leave a few
	 * reductions giving the message some time
	 * to turn into garbage.
	 */
	ERTS_VBUMP_LEAVE_REDS(p, 5);
    }
}

void hipe_fclearerror_error(Process *p)
{
#if !defined(NO_FPE_SIGNALS)
    erts_fp_check_init_error(&p->fp_exception);
#else
    erts_exit(ERTS_ABORT_EXIT, "Emulated FPE not cleared by HiPE");
#endif
}

/* Saving a stacktrace from native mode. Right now, we only create a
 *  minimal struct with no fields filled in except freason. The flag
 *  EXF_NATIVE is set, so that build_stacktrace (in beam_emu.c) does not
 *  try to interpret any other field.
 */
static void hipe_save_stacktrace(Process* c_p, Eterm args)
{
    Eterm *hp;
    struct StackTrace* s;
    int sz;
    int depth = erts_backtrace_depth;    /* max depth (never negative) */

    /* Create a container for the exception data. This must be done just
       as in the save_stacktrace function in beam_emu.c */
    sz = (offsetof(struct StackTrace, trace) + sizeof(Eterm)*depth
	  + sizeof(Eterm) - 1) / sizeof(Eterm);
    hp = HAlloc(c_p, 2 + 1 + sz);
    s = (struct StackTrace *) (hp + 2);
    c_p->ftrace = CONS(hp, args, make_big((Eterm *) s));
    s->header = make_pos_bignum_header(sz);
    s->current = NULL;
    s->pc = NULL;

    s->depth = hipe_fill_stacktrace(c_p, depth, s->trace);

    /* Must mark this as a native-code exception. */
    s->freason = NATIVE_EXCEPTION(c_p->freason);
    return;
}

/*
 * hipe_handle_exception() is called from hipe_${ARCH}_glue.S when an
 * exception has been thrown, to expand the exception value, set the
 * stack trace, and locate the current handler.
 */
void hipe_handle_exception(Process *c_p)
{
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */

    /*
     * Check if we have an arglist for the top level call. If so, this
     * is encoded in Value, so we have to dig out the real Value as well
     * as the Arglist.
     */
    if (c_p->freason & EXF_ARGLIST) {
	Eterm *tp;
	ASSERT(is_tuple(Value));
	tp = tuple_val(Value);
	Value = tp[1];
	Args = tp[2];
    }

    /* If necessary, build a stacktrace object. */
    if (c_p->freason & EXF_SAVETRACE)
	hipe_save_stacktrace(c_p, Args);

    /* Get the fully expanded error term */
    Value = expand_error_value(c_p, c_p->freason, Value);

    /* Save final error term and stabilize the exception flags so no
       further expansion is done. */
    c_p->fvalue = Value;
    c_p->freason = PRIMARY_EXCEPTION(c_p->freason);

    /* Synthesized to avoid having to generate code for it. */
    c_p->def_arg_reg[0] = exception_tag[GET_EXC_CLASS(c_p->freason)];

    ERTS_RECV_MARK_CLEAR(c_p);  /* No longer safe to use this position */

    hipe_find_handler(c_p);
}

/* This is duplicated from beam_emu.c for now */
static struct StackTrace *get_trace_from_exc(Eterm exc)
{
    if (exc == NIL)
	return NULL;
    else
	return (struct StackTrace *) big_val(CDR(list_val(exc)));
}

/*
 * This does what the (misnamed) Beam instruction 'raise_ss' does,
 * namely, a proper re-throw of an exception that was caught by 'try'.
 */
/* Called via standard_bif_interface_2 */
BIF_RETTYPE nbif_impl_hipe_rethrow(NBIF_ALIST_2)
{
    Process *c_p = BIF_P;
    Eterm exc = BIF_ARG_1;
    Eterm value = BIF_ARG_2;

    c_p->fvalue = value;
    if (c_p->freason == EXC_NULL) {
	/* a safety check for the R10-0 case; should not happen */
	c_p->ftrace = NIL;
	BIF_ERROR(c_p, EXC_ERROR);
    }
    /* For R10-0 code, 'exc' might be an atom. In that case, just
       keep the existing c_p->ftrace. */
    switch (exc) {
      case am_throw:
	BIF_ERROR(c_p, (EXC_THROWN & ~EXF_SAVETRACE));
	break;
      case am_error:
	BIF_ERROR(c_p, (EXC_ERROR & ~EXF_SAVETRACE));
	break;
      case am_exit:
	BIF_ERROR(c_p, (EXC_EXIT & ~EXF_SAVETRACE));
	break;
      default:
	{/* R10-1 and later
	    XXX note: should do sanity check on given exception if it can be
	    passed from a user! Currently only expecting generated calls.
	 */
	    struct StackTrace *s;
	    c_p->ftrace = exc;
	    s = get_trace_from_exc(exc);
	    if (s == NULL) {
		BIF_ERROR(c_p, EXC_ERROR);
	    } else {
		BIF_ERROR(c_p, PRIMARY_EXCEPTION(s->freason));
	    }
	}
    }
}

/* Called via standard_bif_interface_3 */
BIF_RETTYPE nbif_impl_hipe_raw_raise(NBIF_ALIST_3)
{
    Process *c_p = BIF_P;
    Eterm class = BIF_ARG_1;
    Eterm value = BIF_ARG_2;
    Eterm stacktrace = BIF_ARG_3;
    Eterm reason;

    if (class == am_error) {
	c_p->fvalue = value;
	reason = EXC_ERROR;
    } else if (class == am_exit) {
	c_p->fvalue = value;
	reason = EXC_EXIT;
    } else if (class == am_throw) {
	c_p->fvalue = value;
	reason = EXC_THROWN;
    } else {
        return am_badarg;
    }
    reason &= ~EXF_SAVETRACE;
    c_p->ftrace = stacktrace;
    BIF_ERROR(c_p, reason);
}

/*
 * Support for compiled binary syntax operations.
 */

char *hipe_bs_allocate(int len)
{
    Binary *bptr;

    bptr = erts_bin_nrml_alloc(len);
    return bptr->orig_bytes;
}

Binary *hipe_bs_reallocate(Binary* oldbptr, int newsize)
{
    Binary *bptr;

    bptr = erts_bin_realloc(oldbptr, newsize);
    return bptr;
}

int hipe_bs_put_big_integer(
    Process *p,
    Eterm arg, Uint num_bits, byte* base, unsigned offset, unsigned flags)
{
    byte *save_bin_buf;
    Uint save_bin_offset;
    int res;
    ERL_BITS_DEFINE_STATEP(p);

    save_bin_buf = erts_current_bin;
    save_bin_offset = erts_bin_offset;
    erts_current_bin = base;
    erts_bin_offset = offset;
    res = erts_new_bs_put_integer(ERL_BITS_ARGS_3(arg, num_bits, flags));
    erts_current_bin = save_bin_buf;
    erts_bin_offset = save_bin_offset;
    return res;
}

int hipe_bs_put_small_float(
    Process *p,
    Eterm arg, Uint num_bits, byte* base, unsigned offset, unsigned flags)
{
    byte *save_bin_buf;
    Uint save_bin_offset;
    int res;
    ERL_BITS_DEFINE_STATEP(p);

    save_bin_buf = erts_current_bin;
    save_bin_offset = erts_bin_offset;
    erts_current_bin = base;
    erts_bin_offset = offset;
    res = erts_new_bs_put_float(p, arg, num_bits, flags);
    erts_current_bin = save_bin_buf;
    erts_bin_offset = save_bin_offset;
    return res;
}

void hipe_bs_put_bits(
    Eterm arg, Uint num_bits, byte* base, unsigned offset, unsigned flags)
{
    Uint Bitoffs, Bitsize;
    byte *Bytep;

    ERTS_GET_BINARY_BYTES(arg, Bytep, Bitoffs, Bitsize);
    erts_copy_bits(Bytep, Bitoffs, 1, base, offset, 1, num_bits);
}

Eterm hipe_bs_utf8_size(Eterm arg)
{
    /* See beam_emu.c:OpCase(i_bs_utf8_size_sd): error handling
       is delayed to the subsequent put_utf8 operation. */
    if (arg < make_small(0x80UL))
	return make_small(1);
    else if (arg < make_small(0x800UL))
	return make_small(2);
    else if (arg < make_small(0x10000UL))
	return make_small(3);
    else
	return make_small(4);
}

Eterm hipe_bs_put_utf8(Process* p, Eterm arg, byte* base, Uint offset)
{
    byte *save_bin_buf;
    Uint save_bin_offset;
    int res;
    unsigned int new_offset;
    ERL_BITS_DEFINE_STATEP(p);

    save_bin_buf = erts_current_bin;
    save_bin_offset = erts_bin_offset;
    erts_current_bin = base;
    erts_bin_offset = offset;
    res = erts_bs_put_utf8(ERL_BITS_ARGS_1(arg));
    new_offset = erts_bin_offset;
    erts_current_bin = save_bin_buf;
    erts_bin_offset = save_bin_offset;
    if (res == 0)
        return 0;
    ASSERT(new_offset != 0);
    return new_offset;
}

Eterm hipe_bs_utf16_size(Eterm arg)
{
    /* See beam_emu.c:OpCase(i_bs_utf16_size_sd): error handling
       is delayed to the subsequent put_utf16 operation. */
    if (arg >= make_small(0x10000UL))
	return make_small(4);
    else
	return make_small(2);
}

/* This would have used standard_bif_interface_4, which doesn't exist.
 * Instead we call it via wrappers for the two relevant cases:
 * (flags & BSF_LITTLE) != 0 and (flags & BSF_LITTLE) == 0.
 */
static
Eterm hipe_bs_put_utf16(Process *p, Eterm arg, byte *base, unsigned int offset, Uint flags)
{
    byte *save_bin_buf;
    Uint save_bin_offset;
    int res;
    unsigned int new_offset;
    ERL_BITS_DEFINE_STATEP(p);

    save_bin_buf = erts_current_bin;
    save_bin_offset = erts_bin_offset;
    erts_current_bin = base;
    erts_bin_offset = offset;
    res = erts_bs_put_utf16(ERL_BITS_ARGS_2(arg, flags));
    new_offset = erts_bin_offset;
    erts_current_bin = save_bin_buf;
    erts_bin_offset = save_bin_offset;
    if (res == 0)
	BIF_ERROR(p, BADARG);
    return new_offset;
}

BIF_RETTYPE nbif_impl_hipe_bs_put_utf16be(NBIF_ALIST_3)
{
    Process *p = BIF_P;
    Eterm arg = BIF_ARG_1;
    byte *base = (byte*) BIF_ARG_2; 
    Uint offset = (Uint) BIF_ARG_3;
    return hipe_bs_put_utf16(p, arg, base, offset, 0);
}

BIF_RETTYPE nbif_impl_hipe_bs_put_utf16le(NBIF_ALIST_3)
{
    Process *p = BIF_P;
    Eterm arg = BIF_ARG_1;
    byte *base = (byte*) BIF_ARG_2; 
    Uint offset = (Uint) BIF_ARG_3;
    return hipe_bs_put_utf16(p, arg, base, offset, BSF_LITTLE);
}

static int validate_unicode(Eterm arg)
{
    if (is_not_small(arg) ||
	arg > make_small(0x10FFFFUL) ||
	(make_small(0xD800UL) <= arg && arg <= make_small(0xDFFFUL)))
	return 0;
    return 1;
}

Uint hipe_is_unicode(Eterm arg)
{
    return (Uint) validate_unicode(arg);
}


int hipe_bs_validate_unicode_retract(ErlBinMatchBuffer* mb, Eterm arg)
{
    if (!validate_unicode(arg)) {
	mb->offset -= 32;
	return 0;
    }
    return 1;
}

Uint hipe_is_divisible(Uint dividend, Uint divisor)
{
    if (dividend % divisor) {
        return 0;
    } else {
        return 1;
    }
}

/* This is like the loop_rec_fr BEAM instruction
 */
Eterm hipe_check_get_msg(Process *c_p)
{
    ErtsMessage *msgp;

    c_p->flags |= F_DELAY_GC;

 next_message:

    msgp = PEEK_MESSAGE(c_p);

    if (!msgp) {
        int get_out;
        (void) erts_proc_sig_receive_helper(c_p, CONTEXT_REDS, 0,
                                            &msgp, &get_out);
        /* FIXME: Need to bump reductions... */
        if (!msgp) {
            if (get_out) {
                if (get_out < 0) {
                    /*
                     * FIXME: We should get out yielding
                     * here...
                     */
                    goto next_message;
                }
                /* Go exit... */
                return THE_NON_VALUE;
            }

            /*
             * If there are no more messages in queue
             * (and we are not yielding or exiting)
             * erts_proc_sig_receive_helper()
             * returns with message queue lock locked...
             */

            /* XXX: BEAM doesn't need this */
            c_p->hipe_smp.have_receive_locks = 1;
            c_p->flags &= ~F_DELAY_GC;
            return THE_NON_VALUE;
        }
    }

    ASSERT(msgp == PEEK_MESSAGE(c_p));
    ASSERT(msgp && ERTS_SIG_IS_MSG(msgp));

    if (ERTS_SIG_IS_EXTERNAL_MSG(msgp)) {
        /* FIXME: bump appropriate amount... */
        if (!erts_decode_dist_message(c_p, ERTS_PROC_LOCK_MAIN, msgp, 0)) {
            /*
             * A corrupt distribution message that we weren't able to decode;
             * remove it...
             */
            /* TODO: Add DTrace probe for this bad message situation? */
            UNLINK_MESSAGE(c_p, msgp);
            msgp->next = NULL;
            erts_cleanup_messages(msgp);
            goto next_message;
        }
    }

    ASSERT(msgp == PEEK_MESSAGE(c_p));
    ASSERT(ERTS_SIG_IS_INTERNAL_MSG(msgp));

    return ERL_MESSAGE_TERM(msgp);
}

/*
 * SMP-specific stuff
 */

/*
 * This is like the timeout BEAM instruction.
 */
void hipe_clear_timeout(Process *c_p)
{
    /*
     * A timeout has occurred.  Reset the save pointer so that the next
     * receive statement will examine the first message first.
     */
    /* XXX: BEAM has different entries for the locked and unlocked
       cases. HiPE doesn't, so we must check dynamically. */
    if (c_p->hipe_smp.have_receive_locks) {
	c_p->hipe_smp.have_receive_locks = 0;
	erts_proc_unlock(c_p, ERTS_PROC_LOCKS_MSG_RECEIVE);
    }
    if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE)) {
	trace_receive(c_p, am_clock_service, am_timeout, NULL);
    }
    c_p->flags &= ~F_TIMO;
    JOIN_MESSAGE(c_p);
}

void hipe_atomic_inc(int *counter)
{
    erts_atomic_inc_nob((erts_atomic_t*)counter);
}

