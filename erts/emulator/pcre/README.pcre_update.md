# How to update the PCRE version used by Erlang

## The basic changes to the PCRE library

To work with the Erlang VM, PCRE has been changed in three important ways:

1. The main execution machine in pcre\_exec has been modified so that
matching can be interrupted and restarted. This functionality utilizes
the code that implements recursion by allocating explicit
"stack-frames" in heap space, which basically means that all local
variables in the loop are part of a struct which is kept in "malloced"
memory on the heap and there are no real stack variables that need to
be pushed on the C stack in the case of recursive calls. This is a
technique we also use inside the VM to avoid building large C
stacks. In PCRE this is enabled by the NO\_RECURSE define, so that is a
prerequisite for the ERLANG\_INTEGRATION define which also adds labels
at restart points and counts "reductions".

2. All visible symbols in PCRE gets the erts_ prefix, so that NIF's
and such using a "real" pcre library does not get confused (or 're'
gets confused when a "real" pcre library get's loaded into the VM
process).

3. All irrelevant functionality has been stripped from the library,
which means for example UTF16 support, jit, DFA execution
etc. Basically the source files handling this are removed, together
with any build support from the PCRE project. We have our own
makefiles etc.

## Setting up an environment for the work

I work with four temporary directories when doing this (the examples
are from the updating of pcre-7.6 to pcre-8.33);

       ~/tmp/pcre> ls
       epcre-7.6  epcre-8.33  pcre-7.6  pcre-8.33

I've unpacked the plain pcre sources in pcre-* and will work with our
patched sources in the epcre-* directories.

Make sure your ERL_TOP contains a *built* version of Erlang (and you have made a branch)

First unpack the pcre libraries (which will create the pcre-*
directories) and then copy our code to the old epcre directory:

      ~/tmp/pcre> tar jxf $ERL_TOP/erts/emulator/pcre/pcre-7.6.tar.bz2 
      ~/tmp/pcre> tar jxf ~/Downloads/pcre-8.33.tar.bz2 
      ~/tmp/pcre> mkdir epcre-7.6  epcre-8.33
      ~/tmp/pcre> cd epcre-7.6/
      ~/tmp/pcre/epcre-7.6> cp -r $ERL_TOP/erts/emulator/pcre/* .
      ~/tmp/pcre/epcre-7.6> rm pcre-7.6.tar.bz2

Leave the obj directory, you may need the libepcre.a file...

If you find it easier, you can revert the commit in GIT that adds the
erts_ prefix to the previous version before continuing work, but as
that is a quite small diff in newer versions of PCRE, it is probably
not worth it. Still, you will find the erts_ prefix being a separate
commit when integrating 8.33, so if you're nice, you will do the same
for the person coming after you...

## Generating a diff for our changes to PCRE

Before you generate a diff (that, in an ideal world, would be used to
automatically patch the newer version of pcre, which will probably
only work for minor PCRE updates), we need to configure the old pcre.

       ~/tmp/pcre/epcre-7.6> cd ../pcre-7.6
       ~/tmp/pcre/pcre-7.6> ./configure --enable-utf8 --enable-unicode-properties --disable-shared --disable-stack-for-recursion

Note that for newer versions, the configure flag '--enable-utf8'
should be replaced with '--enable-utf'

So we now generate a diff:

    ~/tmp/pcre/pcre-7.6> cd ../epcre-7.6
   ~/tmp/pcre/epcre-7.6> (for x in *.[ch]; do if [ -f ../pcre-7.6/$x ]; then diff -c ../pcre-7.6/$x $x; fi; done ) > ../epcre-7.6_clean.diff

### What the diff means

Let's now walk through the relevant parts of the diff. Some of the
differences might come from patches that probably are already in the
new version, For example in out 7.6, we had a security patch which
added the define WORK_SIZE_CHECK and used it in some places. Those can
probably safely be ignored, but to be on the safe side, check what's
already integrated in the new version.

The interesting part is in pcre_exec.c. You will see things like 

    #ifdef ERLANG_INTEGRATION
    ...
    #endif

or
 
    #if defined(ERLANG_INTEGRATION)
    ...
    #endif

and a lot of

    COST_CHK(1);

or

    COST(min);

and

    /* LOOP_COUNT: Ok */
    /* LOOP_COUNT: CHK */
    /* LOOP_COUNT: COST */
	

scattered over the main loop. Those mean the following:

* COST(int x) - consume reductions proportional to the integer
  parameter, but no need for interruption here (it's like
  bump_reductions without trapping). The loop they apply to also has a
  'LOOP_COUNT: COST' comment at it's head.

* COST\_CHK(int x) - like COST(x), but also check that the reduction
counter does not reach zero. If it does, leave the execution loop to
be restarted at a later point. No real stack variables can be live
here. Note that variables like 'max' and 'min' are *not* real stack
variables, the NO\_RECURSION setting has taken care of that. 'i' is a
stack variable that's explicitly saved when trapping, so that will
also be correct when returning from a trap. So will 'c', 'rrc' and
flags like 'utf8', 'minimize' and 'posessive'. Those can also be
regarded as "non C-stack variables". The loop where they reside also
has a 'LOOP\_COUNT: CHK' comment.

* /* LOOP_COUNT: Ok */ - means that I have checked the loop and it
  only runs a deterministic set of iterations regardless of input, or
  it has a call to RRECURSE in it's body, why we need not add more
  cost than the normal reduction counting that will occur for each
  instruction demands.

The thing is that each loop in the function 'match' should be marked
with one of these comments. If no comment is present after you patched
the new release (if you successfully manage to do it automatically),
it may be a new regexp instruction that is added since the last
release.

You will need to manually go through the main 'match' loop after
upgrading to verify that there are no unhandled loops in the regexp
machine loop (!).

The COST\_CHK macro works like this:

1. Add to the loop count.
2. If loop count > limit:
    1. Store the line (+100) in the Xwhere member of the frame structure
    2. Goto LOOP\_COUNT\_BREAK, which ultimately returns from the function
3. Insert a label, which is named L\_LOOP\_COUNT\_&lt;line number&gt;

LOOP\_COUNT\_BREAK code will create an extra "stack frame" on the heap
allocated stack used if NO\_RECURSION is set, and will store the few
locals that are not already in the ordinary stack frame there (like
'c' and 'i').

When we continue execution (after a trap up to the main Erlang
scheduler), we will jump to LOOP\_COUNT\_RETURN, which will restore
the local variables and will jump to the labels. The jump code looks
like this in the C source:

     	switch (frame->Xwhere) 
      	       {
      #include "pcre_exec_loop_break_cases.inc"
      	       default:
		        DPRINTF(("jump error in pcre match: label %d non-existent\n", frame->Xwhere));
        		return PCRE_ERROR_INTERNAL;
      		}

When building, pcre\_exec\_loop\_break\_cases.inc will be generated
during build by pcre.mk, it will look like:

     case 791: goto L_LOOP_COUNT_691;
     case 1892: goto L_LOOP_COUNT_1792;
     case 1999: goto L_LOOP_COUNT_1899;

etc

So, simply put, all C-stack variables are saved when we have consumed
our reductions, we return from the function and, as there is no real
recursion we immediately fall out into the re:run BIF, which with the
help of a magic binary keeps track of the heap allocated stack for the
regexp machine. When we return from trapping out to the scheduler, all
vital data is restored and we continue from exactly the same state as
we left. What's needed is to patch this into the new pcre_exec and
check all new instructions to determine what might need updating in
terms of COST, COST\_CHK etc.

Well, that's *almost* everything, because there is of course more...

The actual interface function, 'pcre\_exec', needs the same treatment
as the actual regexp machine loop, that is we need to store all local
variables between restarts. Unfortunately the NO\_RECURSE setting does
not do this, we need to do it ourselves. So there's quite a diff in
that function too, where a big struct is declared, containing every
local variable in that function, together with either local copies
that are swapped in and out, or macros that directly access the heap
allocated struct. The struct is called `PcreExecContext`.

If a context is present, we are restarting and therefore restore
everything. If we are restarting we can also skip all initialization
code in the function and jump more or less directly to the
RESTART_INTERRUPTED label and the call to 'match', which is the actual
regexp machine loop.

There are a few places in the pcre_exec we need to do some housekeeping, you will see code like:

 	if ((extra_data->flags & PCRE_EXTRA_LOOP_LIMIT) != 0) 
 	  {
		*extra_data->loop_counter_return = 
 	    		(extra_data->loop_limit - md->loop_limit);
          }

Make sure, after updating, that this housekeeping is done whenever we
do not reach the call to 'match'.

So, now we in theory know what to do, so let's do it:

But...

## File changes in the new version of PCRE

First we need to go through what's changed in the new library
version. Files may have new names, functions may have moved and so on.

Start by building the new library:

      ~/tmp/pcre> cd pcre-8.33/
      ~/tmp/pcre/pcre-8.33> ./configure --enable-utf --enable-unicode-properties --disable-shared --disable-stack-for-recursion
      ~/tmp/pcre/pcre-8.33> make

In the make process, you will probably notice most files that are
used, but you can bet that's not all not all...

To begin with you will need a default table for Latin-1 characters, so:

   	 ~/tmp/pcre/pcre-8.33> cc -DHAVE_CONFIG_H -o dftables dftables.c
	 ~/tmp/pcre/pcre-8.33> LANG=sv_SE ./dftables -L ../epcre-8.33/pcre_latin_1_table.c

Compare it to the pcre\_latin\_1\_table.c in the old version, they
should not differ in any significant way. If they do, it might be
that you do not have the sv_SE locale installed on your machine.

A good starting point is then to try to find all files in the new
version of the library that have (probably) the same names as the
one's in our distribution:

	~/tmp/pcre/pcre-8.33> cd ../epcre-7.6/
	~/tmp/pcre/epcre-7.6> for x in *.[ch]; do if [ '!' -f ../pcre-8.33/$x ]; then echo $x; else cp ../pcre-8.33/$x ../epcre-8.33/; fi; done

This will output a list of files not found in the new distro. Let's
look at the list from the example upgrade:

     local_config.h
     make_latin1_table.c
     pcre_info.c
     pcre_latin_1_table.c
     pcre_make_latin1_default.c
     pcre_try_flipped.c
     pcre_ucp_searchfuncs.c
     ucpinternal.h
     ucptable.h

* local\_config.h - OK, that's our child, it contains PCRE-specific
  configure-results (i.e. the #defines that are results from out
  parameters to configure, like NO\_RECURSE etc). Just copy it and
  edit it according to what specific settings you can find in the
  generated config.h from the real library build. In our example case,
  the #define SUPPORT\_UTF8 should be renamed to #define SUPPORT\_UTF
  and #define VERSION "7.6" should be changed to #define VERSION
  "8.33"...

* make\_latin1\_table.c - it was renamed to dftables.c, so we copy
  that instead.

* pcre\_info.c - It was simply removed from the library. Good, because
  it was useless... So just ignore.

* pcre\_latin\_1\_table.c - No problem, we generated a new one in the
  earlier stage.

* pcre\_make\_latin1\_default.c - No longer used, a hack that's not
  needed with dftables. Ignored

* pcre\_try\_flipped.c - This functionality has been removed from
  pcre\_exec, you cannot compile on one endianess and execute on
  another any more :( Ignored.

* pcre\_ucp\_searchfuncs.c, ucpinternal.h, ucptable.h - this
  functionality is moved to pcre\_ucd.c, copy that one instead.

OK, now go the other way and look at what was actually built for the new version of pcre:

    ~/tmp/pcre/epcre-7.6> cd ../pcre-8.33/
    ~/tmp/pcre/pcre-8.33> nm ./.libs/libpcre.a | egrep 'lib.*.o:'

The output for this release was:

    libpcre_la-pcre_byte_order.o:
    libpcre_la-pcre_compile.o:
    libpcre_la-pcre_config.o:
    libpcre_la-pcre_dfa_exec.o:
    libpcre_la-pcre_exec.o:
    libpcre_la-pcre_fullinfo.o:
    libpcre_la-pcre_get.o:
    libpcre_la-pcre_globals.o:
    libpcre_la-pcre_jit_compile.o:
    libpcre_la-pcre_maketables.o:
    libpcre_la-pcre_newline.o:
    libpcre_la-pcre_ord2utf8.o:
    libpcre_la-pcre_refcount.o:
    libpcre_la-pcre_string_utils.o:
    libpcre_la-pcre_study.o:
    libpcre_la-pcre_tables.o:
    libpcre_la-pcre_ucd.o:
    libpcre_la-pcre_valid_utf8.o:
    libpcre_la-pcre_version.o:
    libpcre_la-pcre_xclass.o:
    libpcre_la-pcre_chartables.o:

Libtool has changed the object names, but we can fix that and see what
sources we have already decided should exist:

	~/tmp/pcre/pcre-8.33> NAMES=`nm ./.libs/libpcre.a | egrep 'lib.*.o:'| sed 's,libpcre_la-,,' | sed 's,.o:$,,'`
	~/tmp/pcre/pcre-8.33> for x in $NAMES; do if [ '!' -f ../epcre-8.33/$x.c ]; then echo $x; fi; done

And the list contained:

    pcre_byte_order
    pcre_jit_compile
    pcre_string_utils

pcre\_jit\_compile is actually needed, even though we have not enabled
jit, and the other two contain functionality needed, so just copy the
sources...

    ~/tmp/pcre/pcre-8.33> for x in $NAMES; do if [ '!' -f ../epcre-8.33/$x.c ]; then cp $x.c ../epcre-8.33/; fi; done

## Test build of stripped down version of new PCRE

Time to do a test build. Copy and edit the pcre.mk makefile and try to
get something that builds...

I made a wrapper Makefile, hacked pcre.mk a little and did a few
changes to a few files, namely added:
  
	#ifdef ERLANG_INTEGRATION
	#include "local_config.h"
	#endif

to pcre\_config.c and pcre\_internal.h. Also pcre.mk needs to get the
new files added and the old files removed, directory names need to be
changed and the wrapper can define most. My wrapper Makefile looked
like this:

    EPCRE_LIB = ./obj/libepcre.a
    PCRE_GENINC = ./pcre_exec_loop_break_cases.inc
    PCRE_OBJDIR = ./obj
    V_AR = ar
    V_CC = gcc
    CFLAGS = -g -O2 -DHAVE_CONFIG_H -I/ldisk/pan/git/otp/erts/x86_64-unknown-linux-gnu
    gen_verbose = 
    PCRE_DIR=.
    include pcre.mk

And the according variables were removed together with dependencies
from pcre.mk. Note that you will need to put things back in order in
pcre.mk after all testing is done. Once a 'make' is successful, you
can generate new dependencies:

    ~/tmp/pcre/epcre-8.33> gcc -MM -c -g -O2 -DHAVE_CONFIG_H -I/ldisk/pan/git/otp/erts/x86_64-unknown-linux-gnu -DERLANG_INTEGRATION *.c | grep -v $ERL_TOP

Well, then you have to add $(PCRE\_OBJDIR)/ to each object and
$(PCRE\_DIR)/ to each header. I did it manually, it's just a couple of
files. Now your pcre.mk is fairly up to date and it's time to start
patching in the changes...

## Actually patching in the changes to the C code

### Fixing the functionality (interruptable pcre\_run etc)

Begin with only pcre\_exec.c, that's the important part:

    ~/tmp/pcre/epcre-8.33> cd ../epcre-7.6/
    ~/tmp/pcre/epcre-7.6> diff -c ../pcre-7.6/pcre_exec.c ./pcre_exec.c > ../epcre_exec.c_7.6.diff
    ~/tmp/pcre/epcre-7.6> cd ../epcre-8.33 

Now - if you are lucky, you can patch the new pcre\_exec with the
patch command from the diff, but that may not be the case... Even if:

    ~/tmp/pcre/epcre-8.33> patch -p0 < ../epcre_exec.c_7.6.diff

works like a charm, you still have to go through the main loop and see
that all do, while and for loops in the code contains COST\_CHK or at
least COST, or, if it's a small loop (over, say one UTF character),
mark it as OK with a comment.

You should also check for other changes, like new local variables in
the pcre\_exec code etc.

What will probably happen, is that the majority of chunks
fail. pcre\_exec is the main file for PCRE, one that is constantly
optimized and where every new feature ends up. You will probably see
so many failed HUNK's that you feel like giving up, but do not
despair, it's just a matter of patience and hard work:

* First, fix the 'pcre\_exec' function.
 
    * Change the struct PcreExecContext to reflect the local variables
      in this version of the code.

    * Add/update the defines that makes local variables in the code
      actually stay in an allocated "exec\_context" and be sure to
      initialize the "pseudo-stack-variables" in the same way as in
      the declarations for the original version of the code.

    * The macros SWAPIN and SWAPOUT should be for variables that are
      used a lot and we do not want to always access through the
      struct. Also a few parameters are saved by SWAPIN and SWAPOUT.

    * What might be tricky is to get things deallocated in a proper
      way, there is a function that's called from the BIF code to
      clean up an exec\_context, be especially observant about how the
      stack in the 'match' function is allocated! The first frame is
      supposed to be on the C stack, but in our case is allocated in
      the exec\_context. The rest of the frames are allocated but
      never freed, not until the match is done.

      The variable 'frame' in the 'match' function is stored in our
      additional field of the 'md' structure, that is the stack top,
      but not necessarily the uppermost frame (due to reuse of old
      frames, which is supposed to be an optimization...).

    * The housekeeping of the "reduction counter" in the extra\_data
      struct needs to be added to all places where we break out of the
      main loop of pcre\_exec. Look for 'break' and you will see the
      places. Make sure to update
      '*extra\_data->loop\_counter\_return' whenever you leave this
      function. It all boils down to some code that loops over the
      call for match and returns PCRE\_ERROR\_LOOP\_LIMIT and get's
      jumped back to when the BIF is restarted. You will see it in
      your diff and you will find a similar place in the new version
      where you put basically the same code.

    * Fixing pcre\_exec takes about an hour of concentrated work, it
      could be worse...

* Next, go for the match function. It's simpler in some ways but
  harder in other. The elimination of the C stack is already there,
  you just need to modify it a little:

    * In the RRETURN macro for NO\_RECURSE, add updating of
      md->loop\_limit before returning. You can see how it's done in
      the diff.

    * RMATCH can be left as it is, at least it could in earlier
      versions. Note however that you should mimic the allocation
      strategies of RMATCH and RRETURN in the code at another place
      later... The principle of the labels HEAP\_RECURSE and
      HEAP\_RETURN are mimicked by our code in LOOP\_COUNT\_BREAK and
      LOOP\_COUNT\_RETURN. You'll see later...

    * COST and COST\_CHK, together with the jump to
      LOOP\_COUNT\_RETURN label are in the beginning of the function
      'match'. It's a block of macros and declaration of our local
      variables loop\_count and loop\_limit. We patch in the code for
      that, but may need to adopt it to new variable names etc. It's
      important to handle the 'frames' variable correctly, dig it out
      of the 'md' struct when we are restarting, but initialize it as
      is done in normal NO\_RECURSE code otherwise. Note that the
      COST\_CHK macro reuses the Xwhere field of the frame struct, it
      is not needed when trapping.

    * The LOOP\_COUNT\_BREAK and the LOOP\_COUNT\_RETURN code can now
      be added. Make sure to check both how a new stack frame should be
      properly allocated by mimicking the code in RMATCH, and how (if)
      it should be freed by mimicking RRETURN. Also check which
      variables need to be saved. They are properly pointed out in
      8.33 with the comment 'These variables do not need to be
      preserved over recursion' and appear in the beginning of the
      function. Find variables of similar type in the frame structure
      and reuse them. In 8.33 there are eight such variables. They are
      placed at the end of the function 'match'. If You are reading
      the diff, you need to scroll past all the COST\_CHK calls,
      i.e. past the whole regexp machine loop.

    * Now take the time to add things like debug macros to the top of
      the file and one single COST\_CHK (preferably the one right
      after for(;;) in 'match'), and see if you can compile. You will
      probably need to add some fields in the structures in pcre.h,
      see from a larger diff what you need there and iterate until you
      can compile.

    * So, what's left is to add all the COST and COST\_CHK macros,
      plus marking all harmless loops as OK. There are a few rules
      here:

        * Mark *every* loop with the comment 'LOOP\_COUNT: xxxx',
          where xxxx is either 'Ok', 'COST' or 'CHK'. There are 175
          'LOOP\_COUNT:' comments in 8.33.

        * Loops marked 'Ok' need no macro, either because they are so
          short (like over an UTF character) or because they contain
          an RMATCH macro, in which case they will be accounted for
          anyway.

        * Loops marked 'COST' will have an associated 'COST(N)' macro,
          either before, if we know the amount of iterations, or
          within. Reductions are counted, but we will not
          interrupt. This is typically in what is expected to be
          medium long loops or at places where interruption is hard
          (like where we have local variables that are alive. The
          selection between 'COST' and 'COST\_CHK' is hard. 'COST' is
          much cheaper and usually enough, but when in doubt about the
          loop length, try to use 'COST\_CHK', while making very sure
          there are no live block-local variables that need to be
          saved over the trap. There are 49 'COST' macros in 8.33.

        * Loops marked 'CHK' shall contain a 'COST\_CHK(N)'
          macro. This macro both counts reductions and may result in
          an interrupt and a return to Erlang space. It is expensive
          and it is vital to ensure that there are no unexpected local
          variables that live past the macro. Most variables are in
          the pseudo stack frame, but some regexp instructions declare
          temporaries inside blocks. Make sure they are not expected
          to be alive after a COST\_CHK if they are not in the
          'heapframe' structure. If they are, you need to
          conditionally move them to the 'heapframe' #if
          defined(ERLANG\_INTEGRATION). in 8.33 the variables 'lgb'
          and 'rgb' are preserved in this way. There are 54
          'COST\_CHK's in 8.33.

        * I've marked a few block-local variables with warnings, but
          look thoroughly through the main loop to detect any new
          ones.

        * Be careful when it comes to freeing the context from Erlang
          (the function erts\_pcre\_free\_restart\_data), Whatever is
          done there has to work *both* when the context is freed in
          the middle of an operation (because of trapping) and when
          some things have been freed by a successful
          return. Specifically, make sure to set md->offset\_vector to
          NULL whenever it's freed (in the rest of the code) and
          construct release\_match\_heapframes so that it can be
          called multiple times for the same heapframe (set the next
          pointer in the "static" frame, i.e. the one allocated in the
          md to NULL after freeing).

    * To add the costs to the main loop takes less than one work day,
      keep calm and continue...

OK, now you are done with the pcre\_exec (or at least, you think
so). The rest is simpler. You have probably already handled 'pcre.h'
and 'pcre\_internal.h' to add fields to the structures etc. Looking at
a diff from an earlier version, you will see what's left. In upgrading
to 8.33, the following things was left to do after pcre\_exec was
fixed, remember you could generate a diff with:

    ~/tmp/pcre/epcre-8.33> cd ../epcre-7.6/
    ~/tmp/pcre/epcre-7.6> (for x in *.[ch]; do if [ -f ../pcre-7.6/$x ]; then diff -c ../pcre-7.6/$x $x; fi; done) > ../epcre-7.6.diff 

Open the diff in your favorite editor and remove whatever changes you
have already made, like everything that has to do with pcre\_exec.c
and probably a large part of pcre.h/pcre\_internal.h.

The expected result is a diff that either contains only the
'%ExternalCopyright%' comments or contains them and the addition of
the erts\_ prefix, depending on if you reverted the prefix change
(using 'git revert') before starting to work. With a little luck, the
patch of the remaining stuff should be possible to apply
automatically. If anything fails, just add it manually.

### Fixing the erts\_prefix

The erts\_ prefix is mostly implemented by adding '#if
defined(ERLANG\_INTEGRATION)' to a lot of function headers, inside the
COMPILE\_UTF8 part. If you then also change the PRIV and PUBL macros
in pcre\_internal.h. Typical diffs look like:

        #if defined COMPILE_PCRE8
      + #if defined(ERLANG_INTEGRATION)
      + #ifndef PUBL
      + #define PUBL(name) erts_pcre_##name
      + #endif
      + #ifndef PRIV
      + #define PRIV(name) _erts_pcre_##name
      + #endif
      + #else
        #ifndef PUBL
        #define PUBL(name) pcre_##name
        #endif
        #ifndef PRIV
        #define PRIV(name) _pcre_##name
        #endif
      + #endif

and

	    #if defined COMPILE_PCRE8
	  + #if defined(ERLANG_INTEGRATION)
	  + PCRE_EXP_DECL int erts_pcre_pattern_to_host_byte_order(pcre *argument_re,
	  +   erts_pcre_extra *extra_data, const unsigned char *tables)
	  + #else
  	    PCRE_EXP_DECL int pcre_pattern_to_host_byte_order(pcre *argument_re,
    	      pcre_extra *extra_data, const unsigned char *tables)
	  + #endif

Note that some data types, like pcre\_extra are accessed with the PUBL
macro, so they need to explicitly get the prefix added. pcre.h is a
pig, as it declares prototypes for all functions regardless of
compilation ode, so there is quite a lot of '#if
defined(ERLANG\_INTEGRATION)' to add there.

Anyway, now try to patch, using a diff where you have removed the
changes you made manually (probably to pcre\_exec.c) but make sure to
save your work (temporary git repository?) before, so you can revert
any disasters...

    ~/tmp/pcre/epcre-7.6> cd ../epcre-8.33/
    ~/tmp/pcre/epcre-8.33> patch -p0 < ../epcre-7.6_clean2.diff

Some hunks may certainly still fail, read through the .rej file and fix it.

### ExternalCopyright

Now you should check that the 'ExternalCopyright' comment is present
in all source files:

    ~/tmp/pcre/epcre-8.33> for x in *.[ch]; do if grep ExternalCopyright $x > /dev/null; then true; else echo $x; fi; done

In this upgrade (from 7.6 to 8.33) we certainly had some new and
renamed files:

    dftables.c
    pcre_byte_order.c
    pcre_chartables.c
    pcre_jit_compile.c
    pcre_latin_1_table.c
    pcre_string_utils.c
    pcre_ucd.c

Go through them manually and add the 'ExternalCopyright' comment.

## Integrate with Erlang

Now you are done with most of the tedious work. It's time to move this
into your branch of the Erlang source tree, remove old files and add
new ones, plus add the tar file with the original pcre dist. Remember
to fix your hacked version of pcre.mk and then try to build
Erlang. You might need to update 'erl\_bif\_re.c' to reflect any
changes in the PCRE library. When it builds, run the test suites.

Make sure to rename any files that has new names and remove any files
that are no longer present before copying in the new versions from
your temporary directory. In our example we remove 'pcre\_info.c',
'pcre\_make\_latin1\_default.c', 'pcre\_try\_flipped.c',
'ucpinternal.h' and 'ucptable.h'. We rename 'make\_latin1\_table.c' to
'dftables.c' and 'pcre\_ucp\_searchfuncs.c' to 'pcre\_ucd.c'.

After copying in the sources, we can try to build. Do not forget to
fix whatever you did in pcre.mk to make it build locally.

## Update test suites

The next step is to integrate the updated PCRE tests into our test suites.

Copy testoutput[1-9] from the testdata directory of your new version
of pcre, to the re\_SUITE\_data in stdlib's test suites. Run the
test suites and remove any bugs. Usually the bugs come from the fact
that the PCRE test suites get better and from our implementation of
global matching, which may have bugs outside of the PCRE library. The
test suite 'pcre' is the one that runs these tests. Also copy
testoutput11-8 to testoutput10, the testoutput10 file in pcre is
nowadays for the DFA, which we do not use.

The next step is to regenerate re\_testoutput1\_replacement\_test. How
to do that is in a comment in the beginning of the file. The key
module is run\_pcre\_tests.erl, which both driver the pcre test and
generate re\_testoutput1\_replacement\_test.erl. Watch during the
generation that you do not get to many of the "Fishy character"
messages, if they are more than, say 20, you will probably need to
address the UTF8 issues in the Perl execution. As it is now, we skip
non latin1 characters in this test. You will need to run iconv on the
generated module to make it UTF-8 before running tests. Try to use a
perl version that is as new as possible.

The exact same procedure goes for the re\_testoutput1\_split\_test.erl. 

Make a note about perl version used in the commit updating the replace
and split test files.

Note that the perl version you are using may not be completely
compatible with the PCRE version you are upgrading to. If this is the
case you might get failures when running the replace and split tests.
If you get failures, you need to inspect the failures and decide what
to do. If there are only a small amount of failures you will probably
end up preferring the behavior of PCRE, and manually changing these
tests. Do these changes in a separate commit so it is easy to see
what differed.

Also add copyright headers to the files after converting them to UTF-8.

After ironing out the rest of the bugs, you should be done with the
code.

## Update documentation

Now it's time for the documentation, which is fairly
straightforward. Diff the pcrepattern man pages from the old and new
PCRE distros and update the re.xml file accordingly. It may help to
have the generated HTML file from the new version to cut and paste
from, but as you will notice, it's quite a few changes from HTML to
XML. All lists are reformatted, the &lt;pre&gt; tags are made into
either &lt;code&gt; or &lt;quite&gt; etc. Also the &lt;P&gt; tags are
converted to lowercase and all mentioned options and function calls
are converted to their Erlang counterpart. Really awesome work that
requires thorough reading of all new text.  For the upgrade from 7.6
to 8.33, the update of the pcrepattern part of our manual page took
about eight hours.

## Update Licence

Copy the LICENCE file to `erts/emulator/pcre/LICENCE` and update
the `[PCRE]` section in `system/COPYRIGHT` with the content of
the `LICENCE` file.

## Add new relevant options to re

Then, when all this is done, you should add any new relevant options
from the PCRE library to both the code (erl\_bif\_re.c), the specs and
the Erlang function 'copt/1' (re.erl) and the manual page
(re.xml). Make sure the options are really relevant to add to the
Erlang API, check if they are compile or run-time options (or both) and
add them to the 'parse\_options' function of erl\_bif\_re.c. Adding an
option that is just passed through to PCRE is pretty simple, at least
"code wise".
 
Now you are done. Run all test suites on all machines and you will be happy.

## Final notes

To avoid the work of a major upgrade, it is probably worth it to keep
in pace with the changes to PCRE. The upgrade from 7.6 to 8.33,
including tracking down bugs etc, took me a total of two weeks. If
smaller diffs from the PCRE development were integrated in a more
incremental fashion, it will be much easier each time and you will
have the PCRE library up to date. PCRE should probably be updated for
each major release, instead of every five years...