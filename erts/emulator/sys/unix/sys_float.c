/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_process.h"


#ifdef NO_FPE_SIGNALS

void
erts_sys_init_float(void)
{
# ifdef SIGFPE
    sys_signal(SIGFPE, SIG_IGN); /* Ignore so we can test for NaN and Inf */
# endif
}

#else  /* !NO_FPE_SIGNALS */

#ifdef ERTS_SMP
static erts_tsd_key_t fpe_key;

/* once-only initialisation early in the main thread (via erts_sys_init_float()) */
static void erts_init_fp_exception(void)
{
    /* XXX: the wrappers prevent using a pthread destructor to
       deallocate the key's value; so when/where do we do that? */
    erts_tsd_key_create(&fpe_key,"fp_exception");
}

void erts_thread_init_fp_exception(void)
{
    unsigned long *fpe = erts_alloc(ERTS_ALC_T_FP_EXCEPTION, sizeof(*fpe));
    *fpe = 0;
    erts_tsd_set(fpe_key, fpe);
}

static ERTS_INLINE volatile unsigned long *erts_thread_get_fp_exception(void)
{
    return (volatile unsigned long*)erts_tsd_get(fpe_key);
}
#else /* !SMP */
#define erts_init_fp_exception()	/*empty*/
static volatile unsigned long fp_exception;
#define erts_thread_get_fp_exception()	(&fp_exception)
#endif /* SMP */

volatile unsigned long *erts_get_current_fp_exception(void)
{
    Process *c_p;

    c_p = erts_get_current_process();
    if (c_p)
	return &c_p->fp_exception;
    return erts_thread_get_fp_exception();
}

static void set_current_fp_exception(unsigned long pc)
{
    volatile unsigned long *fpexnp = erts_get_current_fp_exception();
    ASSERT(fpexnp != NULL);
    *fpexnp = pc;
}

void erts_fp_check_init_error(volatile unsigned long *fpexnp)
{
    char buf[128];
    snprintf(buf, sizeof buf, "ERTS_FP_CHECK_INIT at %p: detected unhandled FPE at %p\r\n",
	     __builtin_return_address(0), (void*)*fpexnp);
    if (write(2, buf, strlen(buf)) <= 0)
	erts_exit(ERTS_ABORT_EXIT, "%s", buf);
    *fpexnp = 0;
#if defined(__i386__) || defined(__x86_64__)
    erts_restore_fpu();
#endif
}

/* Is there no standard identifier for Darwin/MacOSX ? */
#if defined(__APPLE__) && defined(__MACH__) && !defined(__DARWIN__)
#define __DARWIN__ 1
#endif

/*
 * Define two processor and possibly OS-specific primitives:
 *
 * static void unmask_fpe(void);
 * -- unmask invalid, overflow, and divide-by-zero exceptions
 *
 * static int mask_fpe(void);
 * -- mask invalid, overflow, and divide-by-zero exceptions
 * -- return non-zero if the previous state was unmasked
 */

#if (defined(__i386__) || defined(__x86_64__)) && defined(__GNUC__)

static void unmask_x87(void)
{
    unsigned short cw;

    __asm__ __volatile__("fstcw %0" : "=m"(cw));
    cw &= ~(0x01|0x04|0x08);   /* unmask IM, ZM, OM */
    __asm__ __volatile__("fldcw %0" : : "m"(cw));
}

static int mask_x87(void)
{
    unsigned short cw;
    int unmasked;

    __asm__ __volatile__("fstcw %0" : "=m"(cw));
    unmasked = (cw & (0x01|0x04|0x08)) == 0;
    /* or just set cw = 0x37f */
    cw |= (0x01|0x04|0x08); /* mask IM, ZM, OM */
    __asm__ __volatile__("fldcw %0" : : "m"(cw));
    return unmasked;
}

static void unmask_sse2(void)
{
    unsigned int mxcsr;

    __asm__ __volatile__("stmxcsr %0" : "=m"(mxcsr));
    mxcsr &= ~(0x003F|0x0680); /* clear exn flags, unmask OM, ZM, IM (not PM, UM, DM) */
    __asm__ __volatile__("ldmxcsr %0" : : "m"(mxcsr));
}

static int mask_sse2(void)
{
    unsigned int mxcsr;
    int unmasked;

    __asm__ __volatile__("stmxcsr %0" : "=m"(mxcsr));
    unmasked = (mxcsr & 0x0680) == 0;
    /* or just set mxcsr = 0x1f80 */
    mxcsr &= ~0x003F; /* clear exn flags */
    mxcsr |=  0x0680; /* mask OM, ZM, IM (not PM, UM, DM) */
    __asm__ __volatile__("ldmxcsr %0" : : "m"(mxcsr));
    return unmasked;
}

#if defined(__x86_64__)

static ERTS_INLINE int cpu_has_sse2(void) { return 1; }

#else /* !__x86_64__ */

/*
 * Check if an x86-32 processor has SSE2.
 */
static unsigned int xor_eflags(unsigned int mask)
{
    unsigned int eax, edx;

    eax = mask;			/* eax = mask */
    __asm__("pushfl\n\t"
	    "popl %0\n\t"	/* edx = original EFLAGS */
	    "xorl %0, %1\n\t"	/* eax = mask ^ EFLAGS */
	    "pushl %1\n\t"
	    "popfl\n\t"		/* new EFLAGS = mask ^ original EFLAGS */
	    "pushfl\n\t"
	    "popl %1\n\t"	/* eax = new EFLAGS */
	    "xorl %0, %1\n\t"	/* eax = new EFLAGS ^ old EFLAGS */
	    "pushl %0\n\t"
	    "popfl"		/* restore original EFLAGS */
	    : "=d"(edx), "=a"(eax)
	    : "1"(eax));
    return eax;
}

static ERTS_INLINE unsigned int cpuid_eax(unsigned int op)
{
    unsigned int eax, save_ebx;

    /* In PIC mode i386 reserves EBX. So we must save
       and restore it ourselves to not upset gcc. */
    __asm__(
	"movl %%ebx, %1\n\t"
	"cpuid\n\t"
	"movl %1, %%ebx"
	: "=a"(eax), "=m"(save_ebx)
	: "0"(op)
	: "cx", "dx");
    return eax;
}

static ERTS_INLINE unsigned int cpuid_edx(unsigned int op)
{
    unsigned int eax, edx, save_ebx;
 
    /* In PIC mode i386 reserves EBX. So we must save
       and restore it ourselves to not upset gcc. */
    __asm__(
	"movl %%ebx, %2\n\t"
	"cpuid\n\t"
	"movl %2, %%ebx"
	: "=a"(eax), "=d"(edx), "=m"(save_ebx)
	: "0"(op)
	: "cx");
    return edx;
}

/* The AC bit, bit #18, is a new bit introduced in the EFLAGS
 * register on the Intel486 processor to generate alignment
 * faults. This bit cannot be set on the Intel386 processor.
 */
static ERTS_INLINE int is_386(void)
{
    return ((xor_eflags(1<<18) >> 18) & 1) == 0;
}

/* Newer x86 processors have a CPUID instruction, as indicated by
 * the ID bit (#21) in EFLAGS being modifiable.
 */
static ERTS_INLINE int has_CPUID(void)
{
    return (xor_eflags(1<<21) >> 21) & 1;
}

static int cpu_has_sse2(void)
{
    unsigned int maxlev, features;
    static int has_sse2 = -1;

    if (has_sse2 >= 0)
	return has_sse2;
    has_sse2 = 0;

    if (is_386())
	return 0;
    if (!has_CPUID())
	return 0;
    maxlev = cpuid_eax(0);
    /* Intel A-step Pentium had a preliminary version of CPUID.
       It also didn't have SSE2. */
    if ((maxlev & 0xFFFFFF00) == 0x0500)
	return 0;
    /* If max level is zero then CPUID cannot report any features. */
    if (maxlev == 0)
	return 0;
    features = cpuid_edx(1);
    has_sse2 = (features & (1 << 26)) != 0;

    return has_sse2;
}
#endif /* !__x86_64__ */

static void unmask_fpe_internal(void)
{
    unmask_x87();
    if (cpu_has_sse2())
	unmask_sse2();
}

static void unmask_fpe(void)
{
    __asm__ __volatile__("fnclex");
    unmask_fpe_internal();
}

static int mask_fpe(void)
{
    int unmasked;

    unmasked = mask_x87();
    if (cpu_has_sse2())
	unmasked |= mask_sse2();
    return unmasked;
}

void erts_restore_fpu(void)
{
    __asm__ __volatile__("fninit");
    unmask_fpe_internal();
}

#elif defined(__sparc__) && defined(__linux__)

#if defined(__arch64__)
#define LDX "ldx"
#define STX "stx"
#else
#define LDX "ld"
#define STX "st"
#endif

static void unmask_fpe(void)
{
    unsigned long fsr;

    __asm__(STX " %%fsr, %0" : "=m"(fsr));
    fsr &= ~(0x1FUL << 23);	/* clear FSR[TEM] field */
    fsr |= (0x1AUL << 23);	/* enable NV, OF, DZ exceptions */
    __asm__ __volatile__(LDX " %0, %%fsr" : : "m"(fsr));
}

static int mask_fpe(void)
{
    unsigned long fsr;
    int unmasked;

    __asm__(STX " %%fsr, %0" : "=m"(fsr));
    unmasked = ((fsr >> 23) & 0x1A) == 0x1A;
    fsr &= ~(0x1FUL << 23);	/* clear FSR[TEM] field */
    __asm__ __volatile__(LDX " %0, %%fsr" : : "m"(fsr));
    return unmasked;
}

#elif (defined(__powerpc__) && defined(__linux__)) || (defined(__ppc__) && defined(__DARWIN__))

#if defined(__linux__)
#include <sys/prctl.h>

static void set_fpexc_precise(void)
{
    if (prctl(PR_SET_FPEXC, PR_FP_EXC_PRECISE) < 0) {
	perror("PR_SET_FPEXC");
	exit(1);
    }
}

#elif defined(__DARWIN__)

#include <mach/mach.h>
#include <pthread.h>

/*
 * FE0 FE1	MSR bits
 *  0   0	floating-point exceptions disabled
 *  0   1	floating-point imprecise nonrecoverable
 *  1   0	floating-point imprecise recoverable
 *  1   1	floating-point precise mode
 *
 * Apparently:
 * - Darwin 5.5 (MacOS X <= 10.1) starts with FE0 == FE1 == 0,
 *   and resets FE0 and FE1 to 0 after each SIGFPE.
 * - Darwin 6.0 (MacOS X 10.2) starts with FE0 == FE1 == 1,
 *   and does not reset FE0 or FE1 after a SIGFPE.
 */
#define FE0_MASK	(1<<11)
#define FE1_MASK	(1<<8)

/* a thread cannot get or set its own MSR bits */
static void *fpu_fpe_enable(void *arg)
{
    thread_t t = *(thread_t*)arg;
    struct ppc_thread_state state;
    unsigned int state_size = PPC_THREAD_STATE_COUNT;

    if (thread_get_state(t, PPC_THREAD_STATE, (natural_t*)&state, &state_size) != KERN_SUCCESS) {
	perror("thread_get_state");
	exit(1);
    }
    if ((state.srr1 & (FE1_MASK|FE0_MASK)) != (FE1_MASK|FE0_MASK)) {
#if 1
	/* This would also have to be performed in the SIGFPE handler
	   to work around the MSR reset older Darwin releases do. */
	state.srr1 |= (FE1_MASK|FE0_MASK);
	thread_set_state(t, PPC_THREAD_STATE, (natural_t*)&state, state_size);
#else
	fprintf(stderr, "srr1 == 0x%08x, your Darwin is too old\n", state.srr1);
	exit(1);
#endif
    }
    return NULL; /* Ok, we appear to be on Darwin 6.0 or later */
}

static void set_fpexc_precise(void)
{
    thread_t self = mach_thread_self();
    pthread_t enabler;

    if (pthread_create(&enabler, NULL, fpu_fpe_enable, &self)) {
	perror("pthread_create");
    } else if (pthread_join(enabler, NULL)) {
	perror("pthread_join");
    }
}

#endif

static void set_fpscr(unsigned int fpscr)
{
    union {
	double d;
	unsigned int fpscr[2];
    } u;

    u.fpscr[0] = 0xFFF80000;
    u.fpscr[1] = fpscr;
    __asm__ __volatile__("mtfsf 255,%0" : : "f"(u.d));
}

static unsigned int get_fpscr(void)
{
    union {
	double d;
	unsigned int fpscr[2];
    } u;

    __asm__("mffs %0" : "=f"(u.d));
    return u.fpscr[1];
}

static void unmask_fpe(void)
{
    set_fpexc_precise();
    set_fpscr(0x80|0x40|0x10);	/* VE, OE, ZE; not UE or XE */
}

static int mask_fpe(void)
{
    int unmasked;

    unmasked = (get_fpscr() & (0x80|0x40|0x10)) == (0x80|0x40|0x10);
    set_fpscr(0x00);
    return unmasked;
}

#else /* !(x86 || (sparc && linux) || (powerpc && (linux || darwin))) */

static void unmask_fpe(void)
{
    fpsetmask(FP_X_INV | FP_X_OFL | FP_X_DZ);
}

static int mask_fpe(void)
{
    const fp_except unmasked_mask = FP_X_INV | FP_X_OFL | FP_X_DZ;
    fp_except old_mask;

    old_mask = fpsetmask(0);
    return (old_mask & unmasked_mask) == unmasked_mask;
}

#endif

/*
 * Define a processor and OS-specific SIGFPE handler.
 *
 * The effect of receiving a SIGFPE should be:
 * 1. Update the processor context:
 *    a) on x86: mask FP exceptions, do not skip faulting instruction
 *    b) on SPARC and PowerPC: unmask FP exceptions, skip faulting instruction
 * 2. call set_current_fp_exception with the PC of the faulting instruction
 */

#if (defined(__linux__) && (defined(__i386__) || defined(__x86_64__) || defined(__sparc__) || defined(__powerpc__))) || (defined(__DARWIN__) && (defined(__i386__) || defined(__x86_64__) || defined(__ppc__))) || (defined(__FreeBSD__) && (defined(__x86_64__) || defined(__i386__))) || ((defined(__NetBSD__) || defined(__OpenBSD__)) && defined(__x86_64__)) || (defined(__sun__) && defined(__x86_64__))

#if defined(__linux__) && defined(__i386__)
#if !defined(X86_FXSR_MAGIC)
#define X86_FXSR_MAGIC 0x0000
#endif
#elif defined(__FreeBSD__) && defined(__x86_64__)
#include <sys/types.h>
#include <machine/fpu.h>
#elif defined(__FreeBSD__) && defined(__i386__)
#include <sys/types.h>
#include <machine/npx.h>
#elif defined(__DARWIN__)
#include <machine/signal.h>
#elif defined(__OpenBSD__) && defined(__x86_64__)
#include <sys/types.h>
#include <machine/fpu.h>
#endif
#if !(defined(__OpenBSD__) && defined(__x86_64__))
#include <ucontext.h>
#endif
#include <string.h>

#if defined(__linux__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->gregs[REG_RIP])
#elif defined(__linux__) && defined(__i386__)
#define mc_pc(mc)	((mc)->gregs[REG_EIP])
#elif defined(__DARWIN__)
# error "Floating-point exceptions not supported on MacOS X"
#elif defined(__FreeBSD__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->mc_rip)
#elif defined(__FreeBSD__) && defined(__i386__)
#define mc_pc(mc)	((mc)->mc_eip)
#elif defined(__NetBSD__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->__gregs[_REG_RIP])
#elif defined(__NetBSD__) && defined(__i386__)
#define mc_pc(mc)	((mc)->__gregs[_REG_EIP])
#elif defined(__OpenBSD__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->sc_rip)
#elif defined(__sun__) && defined(__x86_64__)
#define mc_pc(mc)	((mc)->gregs[REG_RIP])
#endif

static void fpe_sig_action(int sig, siginfo_t *si, void *puc)
{
    ucontext_t *uc = puc;
    unsigned long pc;

#if defined(__linux__) && defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    fpregset_t fpstate = mc->fpregs;
    pc = mc_pc(mc);
    /* A failed SSE2 instruction will restart. To avoid
       looping we mask SSE2 exceptions now and unmask them
       again later in erts_check_fpe()/erts_restore_fpu().
       On RISCs we update PC to skip the failed instruction,
       but the ever increasing complexity of the x86 instruction
       set encoding makes that a poor solution here. */
    fpstate->mxcsr = 0x1F80;
    fpstate->swd &= ~0xFF;
#elif defined(__linux__) && defined(__i386__)
    mcontext_t *mc = &uc->uc_mcontext;
    fpregset_t fpstate = mc->fpregs;
    pc = mc_pc(mc);
    if ((fpstate->status >> 16) == X86_FXSR_MAGIC)
	((struct _fpstate*)fpstate)->mxcsr = 0x1F80;
    fpstate->sw &= ~0xFF;
#elif defined(__linux__) && defined(__sparc__) && defined(__arch64__)
    /* on SPARC the 3rd parameter points to a sigcontext not a ucontext */
    struct sigcontext *sc = (struct sigcontext*)puc;
    pc = sc->sigc_regs.tpc;
    sc->sigc_regs.tpc = sc->sigc_regs.tnpc;
    sc->sigc_regs.tnpc += 4;
#elif defined(__linux__) && defined(__sparc__)
    /* on SPARC the 3rd parameter points to a sigcontext not a ucontext */
    struct sigcontext *sc = (struct sigcontext*)puc;
    pc = sc->si_regs.pc;
    sc->si_regs.pc = sc->si_regs.npc;
    sc->si_regs.npc = (unsigned long)sc->si_regs.npc + 4;
#elif defined(__linux__) && defined(__powerpc__)
#if defined(__powerpc64__)
    mcontext_t *mc = &uc->uc_mcontext;
    unsigned long *regs = &mc->gp_regs[0];
#else
    mcontext_t *mc = uc->uc_mcontext.uc_regs;
    unsigned long *regs = &mc->gregs[0];
#endif
    pc = regs[PT_NIP];
    regs[PT_NIP] += 4;
    regs[PT_FPSCR] = 0x80|0x40|0x10;	/* VE, OE, ZE; not UE or XE */
#elif defined(__DARWIN__) && (defined(__i386__) || defined(__x86_64__))
# error "Floating-point exceptions not supported on MacOS X"
#elif defined(__DARWIN__) && defined(__ppc__)
    mcontext_t mc = uc->uc_mcontext;
    pc = mc->ss.srr0;
    mc->ss.srr0 += 4;
    mc->fs.fpscr = 0x80|0x40|0x10;
#elif defined(__FreeBSD__) && defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    struct savefpu *savefpu = (struct savefpu*)&mc->mc_fpstate;
    struct envxmm *envxmm = &savefpu->sv_env;
    pc = mc_pc(mc);
    envxmm->en_mxcsr = 0x1F80;
    envxmm->en_sw &= ~0xFF;
#elif defined(__FreeBSD__) && defined(__i386__)
    mcontext_t *mc = &uc->uc_mcontext;
    union savefpu *savefpu = (union savefpu*)&mc->mc_fpstate;
    pc = mc_pc(mc);
    if (mc->mc_fpformat == _MC_FPFMT_XMM) {
	struct envxmm *envxmm = &savefpu->sv_xmm.sv_env;
	envxmm->en_mxcsr = 0x1F80;
	envxmm->en_sw &= ~0xFF;
    } else {
	struct env87 *env87 = &savefpu->sv_87.sv_env;
	env87->en_sw &= ~0xFF;
    }
#elif defined(__NetBSD__) && defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    struct fxsave64 *fxsave = (struct fxsave64 *)&mc->__fpregs;
    pc = mc_pc(mc);
    fxsave->fx_mxcsr = 0x1F80;
    fxsave->fx_fsw &= ~0xFF;
#elif defined(__NetBSD__) && defined(__i386__)
    mcontext_t *mc = &uc->uc_mcontext;
    pc = mc_pc(mc);
    if (uc->uc_flags & _UC_FXSAVE) {
	struct envxmm *envxmm = (struct envxmm *)&mc->__fpregs;
	envxmm->en_mxcsr = 0x1F80;
	envxmm->en_sw &= ~0xFF;
    } else {
	struct env87 *env87 = (struct env87 *)&mc->__fpregs;
	env87->en_sw &= ~0xFF;
    }
#elif defined(__OpenBSD__) && defined(__x86_64__)
    struct fxsave64 *fxsave = uc->sc_fpstate;
    pc = mc_pc(uc);
    fxsave->fx_mxcsr = 0x1F80;
    fxsave->fx_fsw &= ~0xFF;
#elif defined(__sun__) && defined(__x86_64__)
    mcontext_t *mc = &uc->uc_mcontext;
    struct fpchip_state *fpstate = &mc->fpregs.fp_reg_set.fpchip_state;
    pc = mc_pc(mc);
    fpstate->mxcsr = 0x1F80;
    fpstate->sw &= ~0xFF;
#endif
#if 0
    {
	char buf[128];
	snprintf(buf, sizeof buf, "%s: FPE at %p\r\n", __FUNCTION__, (void*)pc);
	write(2, buf, strlen(buf));
    }
#endif
    set_current_fp_exception(pc);
}

static void erts_thread_catch_fp_exceptions(void)
{
    struct sigaction act;
    memset(&act, 0, sizeof act);
    act.sa_sigaction = fpe_sig_action;
    act.sa_flags = SA_SIGINFO;
    sigaction(SIGFPE, &act, NULL);
    unmask_fpe();
}

#else  /* !((__linux__ && (__i386__ || __x86_64__ || __powerpc__)) || (__DARWIN__ && (__i386__ || __x86_64__ || __ppc__))) */

static void fpe_sig_handler(int sig)
{
    set_current_fp_exception(1); /* XXX: convert to sigaction so we can get the trap PC */
}

static void erts_thread_catch_fp_exceptions(void)
{
    sys_signal(SIGFPE, fpe_sig_handler);
    unmask_fpe();
}

#endif /* (__linux__ && (__i386__ || __x86_64__ || __powerpc__)) || (__DARWIN__ && (__i386__ || __x86_64__ || __ppc__))) */

/* once-only initialisation early in the main thread */
void erts_sys_init_float(void)
{
    erts_init_fp_exception();
    erts_thread_catch_fp_exceptions();
    erts_printf_block_fpe = erts_sys_block_fpe;
    erts_printf_unblock_fpe = erts_sys_unblock_fpe;
}

#endif /* NO_FPE_SIGNALS */

void erts_thread_init_float(void)
{
#ifdef ERTS_SMP
    /* This allows Erlang schedulers to leave Erlang-process context
       and still have working FP exceptions. XXX: is this needed? */
    erts_thread_init_fp_exception();
#endif

#ifndef NO_FPE_SIGNALS
    /* NOTE:
     *  erts_thread_disable_fpe() is called in all threads at
     *  creation. We at least need to call unmask_fpe()
     */
#if defined(__DARWIN__) || defined(__FreeBSD__)
    /* Darwin (7.9.0) does not appear to propagate FP exception settings
       to a new thread from its parent. So if we want FP exceptions, we
       must manually re-enable them in each new thread.
       FreeBSD 6.1 appears to suffer from a similar issue. */
    erts_thread_catch_fp_exceptions();
#else
    unmask_fpe();
#endif

#endif
}

void erts_thread_disable_fpe(void)
{
#if !defined(NO_FPE_SIGNALS)
    (void)mask_fpe();
#endif
}

#if !defined(NO_FPE_SIGNALS)
int erts_sys_block_fpe(void)
{
    return mask_fpe();
}

void erts_sys_unblock_fpe(int unmasked)
{
    if (unmasked)
	unmask_fpe();
}
#endif

/* The following check is incorporated from the Vee machine */
    
#define ISDIGIT(d) ((d) >= '0' && (d) <= '9')

/* 
 ** Convert a double to ascii format 0.dddde[+|-]ddd
 ** return number of characters converted or -1 if error.
 **
 ** These two functions should maybe use localeconv() to pick up
 ** the current radix character, but since it is uncertain how
 ** expensive such a system call is, and since no-one has heard
 ** of other radix characters than '.' and ',' an ad-hoc 
 ** low execution time solution is used instead.
 */

int
sys_double_to_chars_ext(double fp, char *buffer, size_t buffer_size, size_t decimals)
{
    char *s = buffer;

    if (erts_snprintf(buffer, buffer_size, "%.*e", decimals, fp) >= buffer_size)
        return -1;
    /* Search upto decimal point */
    if (*s == '+' || *s == '-') s++;
    while (ISDIGIT(*s)) s++;
    if (*s == ',') *s++ = '.'; /* Replace ',' with '.' */
    /* Scan to end of string */
    while (*s) s++;
    return s-buffer; /* i.e strlen(buffer) */
}

/* Float conversion */

int
sys_chars_to_double(char* buf, double* fp)
{
#ifndef NO_FPE_SIGNALS
    volatile unsigned long *fpexnp = erts_get_current_fp_exception();
#endif
    char *s = buf, *t, *dp;

    /* Robert says that something like this is what he really wanted:
     * (The [.,] radix test is NOT what Robert wanted - it was added later)
     *
     * 7 == sscanf(Tbuf, "%[+-]%[0-9][.,]%[0-9]%[eE]%[+-]%[0-9]%s", ....);
     * if (*s2 == 0 || *s3 == 0 || *s4 == 0 || *s6 == 0 || *s7)
     *   break;
     */

    /* Scan string to check syntax. */
    if (*s == '+' || *s == '-') s++;
    if (!ISDIGIT(*s))		/* Leading digits. */
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s != '.' && *s != ',')	/* Decimal part. */
      return -1;
    dp = s++;			/* Remember decimal point pos just in case */
    if (!ISDIGIT(*s))
      return -1;
    while (ISDIGIT(*s)) s++;
    if (*s == 'e' || *s == 'E') {
	/* There is an exponent. */
	s++;
	if (*s == '+' || *s == '-') s++;
	if (!ISDIGIT(*s))
	  return -1;
	while (ISDIGIT(*s)) s++;
    }
    if (*s)			/* That should be it */
      return -1;

#ifdef NO_FPE_SIGNALS
    errno = 0;
#endif
    __ERTS_FP_CHECK_INIT(fpexnp);
    *fp = strtod(buf, &t);
    __ERTS_FP_ERROR_THOROUGH(fpexnp, *fp, return -1);
    if (t != s) {		/* Whole string not scanned */
	/* Try again with other radix char */
	*dp = (*dp == '.') ? ',' : '.';
	errno = 0;
	__ERTS_FP_CHECK_INIT(fpexnp);
	*fp = strtod(buf, &t);
	__ERTS_FP_ERROR_THOROUGH(fpexnp, *fp, return -1);
    }

#ifdef NO_FPE_SIGNALS
    if (errno == ERANGE) {
	if (*fp == HUGE_VAL || *fp == -HUGE_VAL) {
	    /* overflow, should give error */
	    return -1;
	} else if (t == s && *fp == 0.0) {
	    /* This should give 0.0 - OTP-7178 */
	    errno = 0;

	} else if (*fp == 0.0) {
	    return -1;
	}
    }
#endif
    return 0;
}

#ifdef USE_MATHERR

int
matherr(struct exception *exc)
{
    return 1;
}

#endif
