<!--
SPDX-License-Identifier: Apache-2.0
-->

# How to update the PCRE version used by Erlang

## The basic changes to the PCRE library

To work with the Erlang VM, PCRE has been changed in three important ways:

1. The main execution machine in pcre2\_match has been modified so that
matching can be interrupted and restarted. This functionality utilizes
the code that implements recursion by allocating explicit
"stack-frames" in heap space, which basically means that all local
variables in the loop are part of a struct which is kept in "malloced"
memory on the heap and there are no real stack variables that need to
be pushed on the C stack in the case of recursive calls. This is a
technique we also use inside the VM to avoid building large C
stacks.

2. All visible symbols in PCRE gets the erts_ prefix, so that NIF's
and such using a "real" pcre library does not get confused (or 're'
gets confused when a "real" pcre library get's loaded into the VM
process). We currently do not do this for PCRE2.

3. All irrelevant functionality has been stripped from the library,
which means for example UTF16 support, jit, DFA execution
etc. Basically the source files handling this are removed, together
with any build support from the PCRE2 project. We have our own
makefiles etc.

## Update PCRE2 Repo, quick guide
1. Check out our PCRE2 fork:
`git clone https://github.com/frazze-jobb/pcre2.git`

2. Change directory: `cd pcre2`

3. Add upstream repo remote in git repo:
```
git remote add upstream https://github.com/PCRE2Project/pcre2.git
git fetch upstream
```

4. Checkout the branch:
`git checkout origin/ERLANG_INTEGRATION_pcre2-10.44`

5. Create a new branch
`git co -b ERLANG_INTEGRATION_pcre2-10.45`

6. Rebase the branch on top of pcre2-10.45:
`git rebase pcre2-10.45`

Jump to *what the diff means* below, for more details on merge conflicts.

7. Fix all the conflicts. Once that is done review it all to verify that conflicts automatically resolved are sane.
`gitk`

8. Check if there are new loops in pcre2_match.c's match function, that are missing
comments /* LOOP_COUNT: Ok/COST/CHK */

9. Check that it builds
mkdir build
cd build
../autogen.sh
../configure
make

10. Push the changes
`git push -u origin ERLANG_INTEGRATION_pcre2-10.45`

## Updating OTP repo with pcre2 changes

1. Check if there are new files we need to deal with:
```
git worktree add ../pcre2-10.44 pcre2-10.44
git worktree add ../pcre2-10.45 pcre2-10.45
cd ../pcre2-10.44
mkdir build
cd build
../autogen.sh
../configure
make

cd ../pcre2-10.44
mkdir build
cd build
../autogen.sh
../configure
make
# go back to pcre folder
cd ../pcre2

# list files

ls pcre2-10.44/src > A
ls pcre2-10.44/build/src >> A
ls pcre2-10.45/src > B
ls pcre2-10.45/build/src >> B

# diff files
diff A B
```
Note them somewhere, they are needed in the next step.

2. Copy files from the src directory and the build directory
cd src/
cp `ls $ERL_TOP/erts/emulator/pcre` $ERL_TOP/erts/emulator/pcre
cd build/src
cp `ls $ERL_TOP/erts/emulator/pcre` $ERL_TOP/erts/emulator/pcre

Make sure everything builds in the OTP repo, if not add files from the diff in step 1 one at a time.
Remember to update pcre.mk.

3. Build pcre
`(cd $ERL_TOP/erts/emulator && make)`

4. Test re module
```
$ERL_TOP/bin/erl
1> re:run("erlang/otp","otp", []).
```
Run the re test suite:
`make stdlib_test ARGS="-suite re_SUITE"`

### What the diff means

The interesting part is in pcre2_match.c. You will see things like

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
  here. 'i' is a
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

LOOP\_COUNT\_BREAK code will create an extra "stack frame" on the heap, and will store the few
locals that are not already in the ordinary stack frame there (like
'c' and 'i').

When we continue execution (after a trap up to the main Erlang
scheduler), we will jump to LOOP\_COUNT\_RETURN, which will restore
the local variables and will jump to the labels. The jump code looks
like this in the C source:

  switch (F->return_id) 
    {

#include "pcre2_match_loop_break_cases.inc"
     default:
       EDEBUGF(("jump error in pcre match: label %d non-existent\n", F->return_id));
       return PCRE2_ERROR_INTERNAL;
     }

When building, pcre2\_match\_loop\_break\_cases.inc will be generated
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
we left. What's needed is to patch this into the new pcre2_match and
check all new instructions to determine what might need updating in
terms of COST, COST\_CHK etc.

Well, that's *almost* everything, because there is of course more...

The actual interface function, 'pcre2\_match', needs the same treatment
as the actual regexp machine loop, that is we need to store all local
variables between restarts. So there's quite a diff in
that function too, where a big struct is declared, containing every
local variable in that function, together with either local copies
that are swapped in and out, or macros that directly access the heap
allocated struct. The struct is called `PcreExecContext`.

If a context is present, we are restarting and therefore restore
everything. If we are restarting we can also skip all initialization
code in the function and jump more or less directly to the
RESTART_INTERRUPTED label and the call to 'match', which is the actual
regexp machine loop.

So, now we in theory know what to do, so let's do it:

## Test build of stripped down version of new PCRE2

Time to do a test build. Copy and edit the pcre.mk makefile and try to
get something that builds...

I made a wrapper Makefile, hacked pcre.mk a little and did a few
changes to a few files, namely added:
  
	#ifdef ERLANG_INTEGRATION
	#include "local_config.h"
	#endif

to pcre2\_config.c and pcre2\_internal.h. Also pcre.mk needs to get the
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

## Updating pcre2_match.c

If git rebase works like a charm, you still have to go through the main loop and see
that all do, while and for loops in the code contains COST\_CHK or at
least COST, or, if it's a small loop (over, say one UTF character),
mark it as OK with a comment.

You should also check for other changes, like new local variables in
the pcre2\_match function etc.

What will probably happen (didn't happen when doing update from 10.44 to 10.45),
is that the majority of chunks fail. pcre2\_match is the main file for PCRE2, one that is constantly
optimized and where every new feature ends up. You will probably see
so many failed HUNK's that you feel like giving up, but do not
despair, it's just a matter of patience and hard work:

* First, fix the 'pcre2\_match' function.
 
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
      clean up an exec\_context, pcre2_free_restart_data, be especially
      observant about how the stack in the 'match' function is allocated!
      The first frame is supposed to be on the C stack, but in our case
      is allocated in the exec\_context. The rest of the frames are
      allocated but never freed, not until the match is done.

      The variable 'F' in the 'match' function is stored in our additional
      field of the 'match_block (mb)' structure, that is the stack top,
      but not necessarily the uppermost frame (due to reuse of old
      frames, which is supposed to be an optimization...).

    * Fixing pcre2\_match function takes about an hour of concentrated work, it
      could be worse...

* Next, go for the match function. It's simpler in some ways but
  harder in other. The elimination of the C stack is already there,
  you just need to modify it a little:

    * COST and COST\_CHK, together with the jump to
      LOOP\_COUNT\_RETURN label are in the beginning of the function
      'match'. It's a block of macros and declaration of our local
      variable loops\_left. We patch in the code for
      that, but may need to adopt it to new variable names etc. It's
      important to handle the 'F' variable correctly, dig it out
      of the 'mb->state_save' when we are restarting, but initialize it as
      is done in normal code otherwise.

    * The LOOP\_COUNT\_BREAK and the LOOP\_COUNT\_RETURN code can now
      be added. We have already allocated a hidden frame at the end of
      stack which we may use.
```
heapframe *newframe = (heapframe*)((char*)F + frame_size);
match_local_variable_store *store = (match_local_variable_store *) newframe;
```
      Also check which variables need to be saved. They are properly pointed
      out in the beginning of the match function with the comment
      `/* Local variables that do not need to be preserved over calls to RRMATCH(). */`
      and appear in the beginning of the
      function.

    * Now take the time to add things like debug macros to the top of
      the file and one single COST\_CHK (preferably the one right
      after for(;;) in 'match'), and see if you can compile. You will
      probably need to add some fields in the structures in pcre2.h,
      see from a larger diff what you need there and iterate until you
      can compile.

    * So, what's left is to add all the COST and COST\_CHK macros,
      plus marking all harmless loops as OK. There are a few rules
      here:

        * Mark *every* loop with the comment 'LOOP\_COUNT: xxxx',
          where xxxx is either 'Ok', 'COST' or 'CHK'. There are 182
          'LOOP\_COUNT:' comments in 10.45.

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
          saved over the trap. There are 67 'COST' macros in 10.45.

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
          defined(ERLANG\_INTEGRATION). in 10.45 the variables 'lgb'
          and 'rgb' are preserved in this way. There are 49
          'COST\_CHK's in 10.45.

        * I've marked a few block-local variables with warnings, but
          look thoroughly through the main loop to detect any new
          ones.

        * Be careful when it comes to freeing the context from Erlang
          (the function pcre2\_free\_restart\_data), Whatever is
          done there has to work *both* when the context is freed in
          the middle of an operation (because of trapping) and when
          some things have been freed by a successful
          return.

    * To add the costs to the main loop takes less than one work day,
      keep calm and continue...

OK, now you are done with the pcre2\_match (or at least, you think
so). The rest is simpler. You have probably already handled 'pcre2.h.in'
and 'pcre2\_internal.h' to add fields to the structures etc. Looking at
a diff from an earlier version, you will see what's left.

### Licensing

Check that all files have /* SPDX-License-Identifier: BSD-3-Clause */:
`for x in *.[ch]; do if grep "SPDX-License-Identifier: BSD-3-Clause" $x > /dev/null; then true; else echo $x; fi; done`

## Integrate with Erlang/OTP

You might need to update 'erl\_bif\_re.c' to reflect any
changes in the PCRE2 library. When it builds, run the test suites.

Make sure to rename any files that has new names and remove any files
that are no longer present before copying in the new versions from
your temporary directory. In our example we remove 'pcre\_info.c',
'pcre\_make\_latin1\_default.c', 'pcre\_try\_flipped.c',
'ucpinternal.h' and 'ucptable.h'. We rename 'make\_latin1\_table.c' to
'dftables.c' and 'pcre\_ucp\_searchfuncs.c' to 'pcre\_ucd.c'.

Update 'local_config.h' by updating the values from pcre2/src/config.h.

After copying in the sources, we can try to build. Do not forget to
fix whatever you did in pcre.mk to make it build locally.

## Update test suites

The next step is to integrate the updated PCRE2 tests into our test suites.
Copy testoutput[1-5,10] from the testdata directory of your new version
of pcre2, to the re\_SUITE\_data in stdlib's test suites. Run the
test suites and remove any bugs. Usually the bugs come from the fact
that the PCRE test suites get better and from our implementation of
global matching, which may have bugs outside of the PCRE2 library. The
test suite 'pcre2' is the one that runs these tests.

The next step is to regenerate re\_testoutput1\_replacement\_test. How
to do that is in a comment in the beginning of the file. The key
module is run\_pcre\_tests.erl, which both driver the pcre test and
generate re\_testoutput1\_replacement\_test.erl. Watch during the
generation that you do not get to many of the "Fishy character"
messages, if they are more than, say 20, you will probably need to
address the UTF8 issues in the Perl execution. As it is now, we skip
non latin1 characters in this test. The generation will end by running
'iconv' to convert the module from latin1 to UTF-8.

The exact same procedure goes for the re\_testoutput1\_split\_test.erl.

Try to use a perl version that is as new as possible. Make a note about perl
version used in the commit updating the replace and split test files.

Note that the perl version you are using may not be completely
compatible with the PCRE version you are upgrading to. If this is the
case you might get failures when running the replace and split tests.
If you get failures, you need to inspect the failures and decide what
to do. If there are only a small amount of failures you will probably
end up preferring the behavior of PCRE, and manually changing these
tests. Do these changes in a separate commit so it is easy to see
what differed.

After ironing out the rest of the bugs, you should be done with the
code.

## Update documentation

Now it's time for the documentation, which is fairly
straightforward. Diff the pcrepattern man pages from the old and new
PCRE distros and update the re.md file accordingly. It may help to
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
from the PCRE2 library to both the code (erl\_bif\_re.c), the specs and
the Erlang function 'copt/1' (re.erl) and the manual page
(re.md). Make sure the options are really relevant to add to the
Erlang API, check if they are compile or run-time options (or both) and
add them to the 'parse\_options' function of erl\_bif\_re.c. Adding an
option that is just passed through to PCRE2 is pretty simple, at least
"code wise".
 
Now you are done. Run all test suites on all machines and you will be happy.

## Final notes

To avoid the work of a major upgrade, it is probably worth it to keep
in pace with the changes to PCRE2. PCRE2 should probably be updated for
each major release, instead of every five years. There seems to be an
interest from the maintainers of PCRE2 to support yielding. Which
would deprecates most of this file.