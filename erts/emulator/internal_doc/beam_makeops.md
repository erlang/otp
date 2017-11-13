The beam\_makeops script
=======================

This document describes the **beam\_makeops** script.

Introduction
------------

The **beam\_makeops** Perl script is used at build-time by both the
compiler and runtime system.  Given a number of input files (all with
the extension `.tab`), it will generate source files used by the
Erlang compiler and by the runtime system to load and execute BEAM
instructions.

Essentially those `.tab` files define:

* External generic BEAM instructions.  They are the instructions that
are known to both the compiler and the runtime system.  Generic
instructions are stable between releases.  New generic instructions
with high numbers than previous instructions can be added in major
releases.  The OTP 20 release has 159 external generic instructions.

* Internal generic instructions.  They are known only to the runtime
system and can be changed at any time without compatibility issues.
They are created by transformation rules (described next).

* Rules for transforming one or more generic instructions to other
generic instructions.  The transformation rules allow combining,
splitting, and removal of instructions, as well as shuffling operands.
Because of the transformation rules, the runtime can have many
internal generic instructions that are only known to runtime system.

* Specific BEAM instructions.  The specific instructions are the
instructions that are actually executed by the runtime system.  They
can be changed at any time without causing compatibility issues.
The loader translates generic instructions to specific instructions.
In general, for each generic instruction, there exists a family of
specific instructions.  The OTP 20 release has 389 specific
instructions.

* The implementation of specific instructions.

Generic instructions have typed operands. Here are a few examples of
operands for `move/2`:

    {move,{atom,id},{x,5}}.
    {move,{x,3},{x,0}}.
    {move,{x,2},{y,1}}.

When those instructions are loaded, the loader rewrites them
to specific instructions:

    move_cx id 5
    move_xx 3 0
    move_xy 2 1

Corresponding to each generic instruction, there is a family of
specific instructions.  The types that an instance of a specific
instruction can handle are encoded in the instruction names.  For
example, `move_xy` takes an X register number as the first operand and
a Y register number as the second operand.  `move_cx` takes a tagged
Erlang term as the first operand and an X register number as the
second operand.

An example: the move instruction
--------------------------------

Using the `move` instruction as an example, we will give a quick
tour to show the main features of **beam\_makeops**.

In the `compiler` application, in the file `genop.tab`, there is the
following line:

    64: move/2

This is a definition of an external generic BEAM instruction. Most
importantly it specifices that the opcode is 64.  It also defines that
it has two operands.  The BEAM assembler will use the opcode when
creating `.beam` files.  The compiler does not really need the arity,
but it will use it as an internal sanity check when assembling the
BEAM code.

Let's have a look at `ops.tab` in `erts/emulator/beam`, where the
specific `move` instructions are defined.  Here are a few of them:

    move x x
    move x y
    move c x

Each specific instructions is defined by following the name of the
instruction with the types for each operand.  An operand type is a
single letter.  For example, `x` means an X register, `y`
means a Y register, and `c` is a "constant" (a tagged term such as
an integer, an atom, or a literal).

Now let's look at the implementation of the `move` instruction.  There
are multiple files containing implementations of instructions in the
`erts/emulator/beam` directory.  The `move` instruction is defined in
`instrs.tab`.  It looks like this:

    move(Src, Dst) {
        $Dst = $Src;
    }

The implementation for an instruction largely follows the C syntax,
except that the variables in the function head don't have any types.
The `$` before an identifier denotes a macro expansion.  Thus,
`$Src` will expand to the code to pick up the source operand for
the instruction and `$Dst` to the code for the destination register.

We will look at the code for each specific instruction in turn.  To
make the code easier to understand, let's first look at the memory
layout for the instruction `{move,{atom,id},{x,5}}`:

         +--------------------+--------------------+
    I -> |                 40 |       &&lb_move_cx |
         +--------------------+--------------------+
         |                        Tagged atom 'id' |
         +--------------------+--------------------+

This example and all other examples in the document assumes a 64-bit
archictecture, and furthermore that pointers to C code fit in 32 bits.

`I` in the BEAM virtual machine is the instruction pointer.  When BEAM
executes an instruction, `I` points to the first word of the
instruction.

`&&lb_move_cx` is the address to C code that implements `move_cx`.  It
is stored in the lower 32 bits of the word.  In the upper 32 bits is
the byte offset to the X register; the register number 5 has been
multiplied by the word size size 8.

In the next word the tagged atom `id` is stored.

With that background, we can look at the generated code for `move_cx`
in `beam_hot.h`:

    OpCase(move_cx):
    {
      BeamInstr next_pf = BeamCodeAddr(I[2]);
      xb(BeamExtraData(I[0])) = I[1];
      I += 2;
      ASSERT(VALID_INSTR(next_pf));
      GotoPF(next_pf);
    }

We will go through each line in turn.

* `OpCase(move_cx):` defines a label for the instruction.  The
`OpCase()` macro is defined in `beam_emu.c`.  It will expand this line
to `lb_move_cx:`.

* `BeamInstr next_pf = BeamCodeAddr(I[2]);` fetches the pointer to
code for the next instruction to be executed.  The `BeamCodeAddr()`
macro extracts the pointer from the lower 32 bits of the instruction
word.

* `xb(BeamExtraData(I[0])) = I[1];` is the expansion of `$Dst = $Src`.
`BeamExtraData()` is a macro that will extract the upper 32 bits from
the instruction word.  In this example, it will return 40 which is the
byte offset for X register 5.  The `xb()` macro will cast a byte
pointer to an `Eterm` pointer and dereference it.  The `I[1]` on
the right side of the `=` fetches an Erlang term (the atom `id` in
this case).

* `I += 2` advances the instruction pointer to the next
instruction.

* In a debug-compiled emulator, `ASSERT(VALID_INSTR(next_pf));` makes
sure that `next_pf` is a valid instruction (that is, that it points
within the `process_main()` function in `beam_emu.c`).

* `GotoPF(next_pf);` transfers control to the next instruction.

Now let's look at the implementation of `move_xx`:

    OpCase(move_xx):
    {
      Eterm tmp_packed1 = BeamExtraData(I[0]);
      BeamInstr next_pf = BeamCodeAddr(I[1]);
      xb((tmp_packed1>>BEAM_TIGHT_SHIFT)) = xb(tmp_packed1&BEAM_TIGHT_MASK);
      I += 1;
      ASSERT(VALID_INSTR(next_pf));
      GotoPF(next_pf);
    }

We will go through the lines that are new or have changed compared to
`move_cx`.

* `Eterm tmp_packed1 = BeamExtraData(I[0]);` picks up both X register
numbers packed into the upper 32 bits of the instruction word.

* `BeamInstr next_pf = BeamCodeAddr(I[1]);` pre-fetches the address of
the next instruction. Note that because both X registers operands fits
into the instruction word, the next instruction is in the very next
word.

* `xb((tmp_packed1>>BEAM_TIGHT_SHIFT)) = xb(tmp_packed1&BEAM_TIGHT_MASK);`
copies the source to the destination.  (For a 64-bit architecture,
`BEAM_TIGHT_SHIFT` is 16 and `BEAM_TIGHT_MASK` is `0xFFFF`.)

* `I += 1;` advances the instruction pointer to the next instruction.

`move_xy` is almost identical to `move_xx`.  The only difference is
the use of the `yb()` macro instead of `xb()` to reference the
destination register:

    OpCase(move_xy):
    {
      Eterm tmp_packed1 = BeamExtraData(I[0]);
      BeamInstr next_pf = BeamCodeAddr(I[1]);
      yb((tmp_packed1>>BEAM_TIGHT_SHIFT)) = xb(tmp_packed1&BEAM_TIGHT_MASK);
      I += 1;
      ASSERT(VALID_INSTR(next_pf));
      GotoPF(next_pf);
    }

### Transformation rules ###

Next let's look at how we can do some optimizations using transformation
rules.  For simple instructions such as `move/2`, the instruction dispatch
overhead can be substantial.  A simple optimization is to combine common
instructions sequences to a single instruction.  One such common sequence
is multiple `move` instructions moving X registers to Y registers.

Using the following rule we can combine two `move` instructions
to a `move2` instruction:

    move X1=x Y1=y | move X2=x Y2=y => move2 X1 Y1 X2 Y2

The left side of the arrow (`=>`) is a pattern.  If the pattern
matches, the matching instructions will be replaced by the
instructions on the right side.  Variables in a pattern must start
with an uppercase letter just as in Erlang.  A pattern variable may be
followed `=` and one or more type letters to constrain the match to
one of those types.  The variables that are bound on the left side can
be used on the right side.

We will also need to define a specific instruction and an implementation:

    # In ops.tab
    move2 x y x y

    // In instrs.tab
    move2(S1, D1, S2, D2) {
        Eterm V1, V2;
        V1 = $S1;
        V2 = $S2;
        $D1 = V1;
        $D2 = V2;
    }

When the loader has found a match and replaced the matched instructions,
it will match the new instructions against the transformation rules.
Because of that, we can define the rule for a `move3/6` instruction
as follows:

    move2 X1=x Y1=y X2=x Y2=y | move X3=x Y3=y => \
          move3 X1 Y1 X2 Y2 X3 Y3

(A `\` before a newline can be used to break a long line for readability.)

It would also be possible to define it like this:

    move X1=x Y1=y | move X2=x Y2=y | move X3=x Y3=y => \
         move3 X1 Y1 X2 Y2 X3 Y3

but in that case it must be defined before the rule for `move2/4`
because the first matching rule will be applied.

One must be careful not to create infinite loops.  For example, if we
for some reason would want to reverse the operand order for the `move`
instruction, we must not do like this:

    move Src Dst => move Dst Src

The loader would swap the operands forever.  To avoid the loop, we must
rename the instruction.  For example:

    move Src Dst => assign Dst Src

This concludes the quick tour of the features of **beam\_makeops**.

Short overview of instruction loading
-------------------------------------

To give some background to the rest of this document, here follows a
quick overview of how instructions are loaded.

* The loader reads and decodes one instruction at a time from the BEAM
code and creates a generic instruction.  Many transformation rules
must look at multiple instructions, so the loader will
keep multiple generic instructions in a linked list.

* The loader tries to apply transformation rules against the
generic instructions in the linked list.  If a rule matches, the
matched instructions will be removed and replaced with new
generic instructions constructed from the right side of the
transformation.

* If a transformation rule matched, the loader applies the
transformation rules again.

* If no transformation rule match, the loader will begin rewriting
the first of generic instructions to a specific instruction.

* First the loader will search for a specific operation where the
types for all operands match the type for the generic instruction.
The first matching instruction will be selected.  **beam\_makeops**
has ordered the specific instructions so that instructions with more
specific operands comes before instructions with less specific
operands.  For example, `move_nx` is more specific than `move_cx`.  If
the first operand is `[]` (NIL), `move_nx` will be selected.

* Given the opcode for the selected specific instruction, the loader
looks up the pointer to the C code for the instruction and stores
in the code area for the module being loaded.

* The loader translates each operand to a machine word and stores it
in the code area.  The operand type for the selected specific
instruction guides the translation.  For example, if the type is `e`,
the value of the operand is an index into an arry of external
functions and will be translated to a pointer to the export entry for
the function to call.  If the type is `x`, the number of the X
register will be multiplied by the word size to produce a byte offset.

* The loader runs the packing engine to pack multiple operands into a
single word.  The packing engine is controlled by a small program,
which is a string where each character is an instruction.  For
example, the code to pack the operands for `move_xy` is `"22#"` (on a
64-bit machine).  That program will pack the byte offsets for both
registers into the same word as the pointer to C code.

Running beam_makeops
--------------------

**beam\_makeops** is found in `$ERL_TOP/erts/emulator/utils`.  Options
start with a hyphen (`-`).  The options are followed by the name of
the input files.  By convention, all input files have the extension
`.tab`, but is not enforced by **beam\_makeops**.

### The -outdir option ###

The option `-outdir Directory` specifies the output directory for
the generated files.  Default is the current working directory.

### Running beam_makeops for the compiler ###

Give the option `-compiler` to produce output files for the compiler.
The following files will be written to the output directory:

* `beam_opcodes.erl` - Used primarily by `beam_asm` and `beam_diasm`.

* `beam_opcode.hrl` - Used by `beam_asm`.  It contains tag definitions
used for encoding instruction operands.

The input file should only contain the definition of BEAM_FORMAT_NUMBER
and external generic instructions.  (Everything else would be ignored.)

### Running beam_makeops for the emulator ###

Give the option `-emulator` to produce output files for the emulator.
The following output files will be generated in the output directory.

* `beam_hot.h`, `beam_warm.h`, `beam_cold.`h - Implementation of
instructions.  Included inside the `process_main()` function in
`beam_emu.c`.

* `beam_opcodes.c` - Defines static data used by the loader
(`beam_load.c`).  Data about generic instructions, specific
instructions (including how to pack their operands), and
transformation rules are all part of this file.

* `beam_opcodes.h` - Miscellanous preprocessor definitions, mainly
used by `beam_load.c` but also by `beam_{hot,warm,cold}.h`.

* `beam_pred_funcs.h` - Included by `beam_load.c`.  Contains defines
needed to call guard constraints in transformation rules.

* `beam_tr_funcs.h` - Included by `beam_load.c`.  Contains defines
needed to call a C function to the right of a transformation rule.

The following options can be given:

* `wordsize 32|64` - Defines the word size.  Default is 32.

* `code-model Model` - The code model as given to `-mcmodel` option
for GCC.  Default is `unknown`.  If the code model is `small` (and
the word size is 64 bits), **beam\_makeops** will pack operands
into the upper 32 bits of the instruction word.

* `DSymbol=0|1` - Defines the value for a symbol.  The symbol can be
used in `%if` and `%unless` directives.

Syntax of .tab files
--------------------

### Comments ###

Any line starting with `#` is a comment and is ignored.

A line with `//` is also a comment.  It is recommended to only
use this style of comments in files that define implementations of
instructions.

A long line can be broken into shorter lines by a placing a`\` before
the newline.

### Variable definitions ###

A variable definition binds a variable to a Perl variable.  It is only
meaningful to add a new definition if **beam\_makeops** is updated
at the same time to use the variable.  A variable definition looks this:

*name*=*value*[;]

where *name* is the name of a Perl variable in **beam\_makeops**,
and *value* is the value to be given to the variable.  The line
can optionally end with a `;` (to avoid messing up the
C indentation mode in Emacs).

Here follows a description of the variables that are defined.

#### BEAM\_FORMAT\_NUMBER ####

`genop.tab` has the following definition:

    BEAM_FORMAT_NUMBER=0

It defines the version of the instruction set (which will be
included in the code header in the BEAM code).  Theoretically,
the version could be bumped, and all instructions changed.
In practice, we would have two support two instruction sets
in the runtime system for at least two releases, so it will
probably never happen in practice.

#### GC\_REGEXP ####

In `macros.tab`, there is a definition of `GC_REGEXP`.
It will be described in [a later section](#the-gc_regexp-definition).

### Directives ###

There are directives to classify specific instructions depending
on how frequently used they are:

* `%hot` - Implementation will be placed in `beam_hot.h`. Frequently
executed instructions.

* `%warm` - Implementation will be placed in `beam_warm.h`.  Binary
syntax instructions.

* `%cold` - Implementation will be placed in `beam_cold.h`. Trace
instructions and infrequently used instructions.

Default is `%hot`.  The directives will be applied to declarations
of the specific instruction that follow.  Here is an example:

    %cold
    is_number f? xy
    %hot

#### Conditional compilation directives ####

The `%if` directive includes a range of lines if a condition is
true.  For example:

    %if ARCH_64
    i_bs_get_integer_32 x f? x
    %endif

The specific instruction `i_bs_get_integer_32` will only be defined
on a 64-bit machine.

The condition can be inverted by using `%unless` instead of `%if`:

    %unless NO_FPE_SIGNALS
    fcheckerror p => i_fcheckerror
    i_fcheckerror
    fclearerror
    %endif

It is also possible to add an `%else` clause:

    %if ARCH_64
    BS_SAFE_MUL(A, B, Fail, Dst) {
        Uint64 res = ($A) * ($B);
        if (res / $B != $A) {
            $Fail;
        }
        $Dst = res;
    }
    %else
    BS_SAFE_MUL(A, B, Fail, Dst) {
        Uint64 res = (Uint64)($A) * (Uint64)($B);
        if ((res >> (8*sizeof(Uint))) != 0) {
            $Fail;
        }
        $Dst = res;
    }
    %endif

#### Symbols that are defined in directives ####

The following symbols are always defined.

* `ARCH_64` - is 1 for a 64-bit machine, and 0 otherwise.
* `ARCH_32` - is 1 for 32-bit machine, and 1 otherwise.

The `Makefile` for building the emulator currently defines the
following symbols by using the `-D` option on the command line for
**beam\_makeops**.

* `NO_FPE_SIGNALS` - 1 if FPE signals are not enable in runtime system,
0 otherwise.
* `USE_VM_PROBES` - 1 if the runtime system is compiled to use VM probes (support for dtrace or systemtap), 0 otherwise.

### Defining external generic instructions ###

External generic BEAM instructions are known to both the compiler and
the runtime system.  They remain stable between releases.  A new major
release may add more external generic instructions, but must not change
the semantics for a previously defined instruction.

The syntax for an external generic instruction is as follows:

*opcode*: [-]*name*/*arity*

*opcode* is an integer greater than or equal to 1.

*name* is an identifier starting with a lowercase letter.  *arity* is
an integer denoting the number of operands.

*name* can optionally be preceded by `-` to indicate that it has been
obsoleted.  The compiler is not allowed to generate BEAM files that
use obsolete instructions and the loader will refuse to load BEAM
files that use obsolete instructions.

It only makes sense to define external generic instructions in the
file `genop.tab` in `lib/compiler/src`, because the compiler must
know about them in order to use them.

New instructions must be added at the end of the file, with higher
numbers than the previous instructions.

### Defining internal generic instructions ###

Internal generic instructions are known only to the runtime
system and can be changed at any time without compatibility issues.

There are two ways to define internal generic instructions:

* Implicitly when a specific instruction is defined.  This is by far
the most common way.  Whenever a specific instruction is created,
**beam\_makeops** automatically creates an internal generic instruction
if it does not previously exist.

* Explicitly.  This is necessary only when a generic instruction does
not have any corresponding specific instruction.

The syntax for an internal generic instruction is as follows:

*name*/*arity*

*name* is an identifier starting with a lowercase letter.  *arity* is
an integer denoting the number of operands.

### About generic instructions in general ###

Each generic instruction has an opcode.  The opcode is an integer,
greater than or equal to 1. For an external generic instruction, it
must be explicitly given `genop.tab`, while internal generic
instructions are automatically numbered by **beam\_makeops**.

The identity of a generic instruction is its name combined with its
arity.  That means that it is allowed to define two distinct generic
instructions having the same name but with different arities.  For
example:

    move_window/5
    move_window/6

Each operand of a generic instruction is tagged with its type.  A generic
instruction can have one of the following types:

* `x` - X register.

* `y` - Y register.

* `l` - Floating point register number.

* `i` - Tagged literal integer.

* `a` - Tagged literal atom.

* `n` - NIL (`[]`, the empty list).

* `q` - Literal that don't fit in a word, that is an object stored on
the heap such as a list or tuple.  Any heap object type is supported,
even types that don't have real literals such as external references.

* `f` - Non-zero failure label.

* `p` - Zero failure label.

* `u` - Untagged integer that fits in a machine word.  It is used for many
different purposes, such as the number of live registers in `test_heap/2`,
as a reference to the export for `call_ext/2`, and as the flags operand for
binary syntax instructions.  When the generic instruction is translated to a
specific instruction, the type for the operand in the specific operation will
tell the loader how to treat the operand.

* `o` - Overflow.  If the value for an `u` operand does not fit in a machine
word, the type of the operand will be changed to `o` (with no associated
value).  Currently only used internally in the loader in the guard constraint
function `binary_too_big()`.

* `v` - Arity value.  Only used internally in the loader.


### Defining specific instructions ###

The specific instructions are known only to the runtime system and
are the instructions that are actually executed.  They can be changed
at any time without causing compatibility issues.

A specific instruction can have at most 6 operands.

A specific instruction is defined by first giving its name followed by
the types for each operand.  For example:

     move x y

Internally, for example in the generated code and in the output from
the BEAM disassembler, the instruction `move x y` will be called `move_xy`.

The name for a specific instruction is an identifier starting with a
lowercase letter.  A type is an lowercase or uppercase letter.

All specific instructions with a given name must have the same number
of operands. That is, the following is **not** allowed:

     move x x
     move x y x y

Here follows the type letters that more or less directly corresponds
to the types for generic instructions.

* `x` - X register.  Will be loaded as a byte offset to the X register
relative to the base of X register array.  (Can be packed with other
operands.)

* `y` - Y register.  Will be loaded as a byte offset to the Y register
relative to the stack frame. (Can be packed with other operands.)

* `r` - X register 0.  An implicit operand that will not be stored in
the loaded code.

* `l` - Floating point register number.  (Can be packed with other
operands.)

* `i` - Tagged literal integer (a SMALL that will fit in one word).

* `a` - Tagged atom.

* `n` - NIL or the empty list.  (Will not be stored in the loaded code.)

* `q` - Tagged CONS or BOXED pointer.  That is, a term such as a list
or tuple.  Any heap object type is supported, even types that don't
have real literals such as external references.

* `f` - Failure label (non-zero).  The target for a branch
or call instruction.

* `p` - The 0 failure label, meaning that an exception should be raised
if the instruction fails.  (Will not be stored in the loaded code.)

* `c` - Any literal term; that is, immediate literals such as SMALL,
and CONS or BOXED pointers to literals.  (Can be used where the
operand in the generic instruction has one of the types `i`, `a`, `n`,
or `q`.)

The types that follow do a type test of the operand at runtime; thus,
they are generally more expensive in terms of runtime than the types
described earlier.  However, those operand types are needed to avoid a
combinatorial explosion in the number of specific instructions and
overall code size of `process_main()`.

* `s` - Tagged source: X register, Y register, or a literal term.  The
tag will be tested at runtime to retrieve the value from an X
register, a Y register, or simply use the value as a tagged Erlang
term.  (Implementation note: An X register is tagged as a pid, and a Y
register as a port.  Therefore the literal term must not contain a
port or pid.)

* `S` - Tagged source register (X or Y).  The tag will be tested at
runtime to retrieve the value from an X register or a Y register.  Slighly
cheaper than `s`.

* `d` - Tagged destination register (X or Y).  The tag will be tested
at runtime to set up a pointer to the destination register.  If the
instrution performs a garbarge collection, it must use the
`$REFRESH_GEN_DEST()` macro to refresh the pointer before storing to
it (there are more details about that in a later section).

* `j` - A failure label (combination of `f` and `p`).  If the branch target 0,
an exception will be raised if instruction fails, otherwise control will be
transfered to the target address.

The types that follows are all applied to an operand that has the `u`
type.

* `t` - An untagged integer that will fit in 12 bits (0-4096).  It can be
packed with other operands in a word.  Most often used as the number
of live registers in instructions such as `test_heap`.

* `I` - An untagged integer that will fit in 32 bits.  It can be
packed with other operands in a word on a 64-bit system.

* `W` - Untagged integer or pointer.  Not possible to pack with other
operands.

* `e` - Pointer to an export entry.  Use by call instructions that call
other modules, such as `call_ext`.

* `L` - A label.  Only used by the `label/1` instruction.

* `b` - Pointer to BIF.  Used by instructions that BIFs, such as
`call_bif`.

* `A` - A tagged arityvalue.  Used in instructions that test the arity
of a tuple.

* `P` - A byte offset into a tuple.

* `Q` - A byte offset into the stack.  Used for updating the frame
pointer register.  Can be packed with other operands.

When the loader translates a generic instruction a specific
instruction, it will choose the most specific instruction that will
fit the types.  Consider the following two instructions:

    move c x
    move n x

The `c` operand can encode any literal value, including NIL.  The
`n` operand only works for NIL.  If we have the generic instruction
`{move,nil,{x,1}}`, the loader will translate it to `move_nx 1`
because `move n x` is more specific.  `move_nx` could be slightly
faster or smaller (depending on the architecture), because the `[]`
is not stored explicitly as an operand.

#### Syntactic sugar for specific instructions ####

It is possible to specify more than one type letter for each operand.
Here is an example:

    move cxy xy

This is syntactic sugar for:

    move c x
    move c y
    move x x
    move x y
    move y x
    move y y

Note the difference between `move c xy` and `move c d`.  Note that `move c xy`
is equivalent to the following two definitions:

    move c x
    move c y

On the other hand, `move c d` is a single instruction.  At runtime,
the `d` operand will be tested to see whether it refers to an X
register or a Y register, and a pointer to the register will be set
up.

#### The '?' type modifier ####

The character `?` can be added to the end of an operand to indicate
that the operand will not be used every time the instruction is executed.
For example:

    allocate_heap t I t?
    is_eq_exact f? x xy

In `allocate_heap`, the last operand is the number of live registers.
It will only be used if there is not enough heap space and a garbage
collection must be performed.

In `is_eq_exact`, the failure address (the first operand) will only be
used if the two register operands are not equal.

Knowing that an operand is not always used can improve how packing
is done for some instructions.

For the `allocate_heap` instruction, without the `?` the packing would
be done like this:

         +--------------------+--------------------+
    I -> |       Stack needed | &&lb_allocate_heap +
         +--------------------+--------------------+
         |        Heap needed | Live registers     +
         +--------------------+--------------------+

"Stack needed" and "Heap needed" are always used, but they are in
different words.  Thus, at runtime the `allocate_heap` instruction
must read both words from memory even though it will not always use
"Live registers".

With the `?`, the operands will be packed like this:

         +--------------------+--------------------+
    I -> |     Live registers | &&lb_allocate_heap +
         +--------------------+--------------------+
         |        Heap needed |       Stack needed +
         +--------------------+--------------------+

Now "Stack needed" and "Heap needed" are in the same word.

### Defining transformation rules ###

Transformation rules are used to rewrite generic instructions to other
generic instructions.  The transformations rules are applied
repeatedly until no rule match.  At that point, the first instruction
in the resulting instruction sequence will be converted to a specific
instruction and added to the code for the module being loaded.  Then
the transformation rules for the remaining instructions are run in the
same way.

A rule is recognized by its right-pointer arrow: `=>`.  To the left of
the arrow is one or more instruction patterns, separated by `|`.  To
the right of the arrow is zero or more instructions, separated by `|`.
If the instructions from the BEAM code matches the instruction
patterns on the left side, they will be replaced with instructions on
the right side (or removed if there are no instructions on the right).

#### Defining instruction patterns ####

We will start looking at the patterns on the left side of the arrow.

A pattern for an instruction consists of its name, followed by a pattern
for each of its operands.  The operand patterns are separated by spaces.

The simplest possible pattern is a variable.  Just like in Erlang,
a variable must begin with an uppercase letter.  If the same variable is
used in multiple operands, the pattern will only match if the operands
are equal.  For example:

    move Same Same =>

This pattern will match if the operands for `move` are the same.  If
the pattern match, the instruction will be removed.  (That used to be an
actual rule a long time ago when the compiler would occasionally produce
instructions such as `{move,{x,2},{x,2}}`.)

Variables that have been bound on the left side can be used on the
right side.  For example, this rule will rewrite all `move` instructions
to `assign` instructions with the operands swapped:

    move Src Dst => assign Dst Src

If we only want to match operands of a certain type, we can
use a type constraint.  A type constraint consists of one or more
lowercase letters, each specifying a type.  For example:

    is_integer Fail an => jump Fail

The second operand pattern, `an`, will match if the second operand is
either an atom or NIL (the empty list).  In case of a match, the
`is_integer/2` instruction will be replaced with a `jump/1`
instruction.

An operand pattern can bind a variable and constrain the type at the
same time by following the variable with a `=` and the constraint.
For example:

    is_eq_exact Fail=f R=xy C=q => i_is_eq_exact_literal Fail R C

Here the `is_eq_exact` instruction is replaced with a specialized instruction
that only compares literals, but only if the first operand is a register and
the second operand is a literal.

#### Further constraining patterns ####

In addition to specifying a type letter, the actual value for the type can
be specified.  For example:

    move C=c x==1 => move_x1 C

Here the second operand of `move` is constrained to be X register 1.

When specifying an atom constraint, the atom is written as it would be
in the C source code.  That is, it needs an `am_` prefix, and it must
be listed in `atom.names`.  For example:

    is_boolean Fail=f a==am_true =>
    is_boolean Fail=f a==am_false =>

There are several constraints available for testing whether a call is to a BIF
or a function.

The constraint `u$is_bif` will test whether the given operand refers to a BIF.
For example:

    call_ext u Bif=u$is_bif => call_bif Bif
    call_ext u Func         => i_call_ext Func

The `call_ext` instruction can be used to call functions written in
Erlang as well as BIFs (or more properly called SNIFs).  The
`u$is_bif` constraint will match if the operand refers to a BIF (that
is, if it is listed in the file `bif.tab`).  Note that `u$is_bif`
should only be applied to operands that are known to contain an index
to the import table chunk in the BEAM file (such operands have the
type `b` or `e` in the corresponding specific instruction).  If
applied to other `u` operands, it will at best return a nonsense
result.

The `u$is_not_bif` constraint matches if the operand does not refer to
a BIF (not listed in `bif.tab`).  For example:

    move S X0=x==0 | line Loc | call_ext_last Ar Func=u$is_not_bif D => \
         move S X0 | call_ext_last Ar Func D

The `u$bif:Module:Name/Arity` constraint tests whether the given
operand refers to a specific BIF.  Note that `Module:Name/Arity`
**must** be an existing BIF defined in `bif.tab`, or there will
be a compilation error.  It is useful when a call to a specific BIF
should be replaced with an instruction as in this example:

    gc_bif2 Fail Live u$bif:erlang:splus/2 S1 S2 Dst => \
         gen_plus Fail Live S1 S2 Dst

Here the call to the GC BIF `'+'/2` will be replaced with the instruction
`gen_plus/5`.  Note that the same name as used in the C source code must be
used for the BIF, which in this case is `splus`.  It is defined like this
in `bit.tab`:

    ubif erlang:'+'/2 splus_2

The `u$func:Module:Name/Arity` will test whether the given operand is a
a specific function.  Here is an example:

    bif1 Fail u$func:erlang:is_constant/1 Src Dst => too_old_compiler

`is_constant/1` used to be a BIF a long time ago.  The transformation
replaces the call with the `too_old_compiler` instruction which will produce
a nicer error message than the default error would be for a missing guard BIF.

#### Type constraints allowed in patterns ####

Here are all type letters that are allowed on the left side of a transformation
rule.

* `u` - An untagged integer that fits in a machine word.

* `x` - X register.

* `y` - Y register.

* `l` - Floating point register number.

* `i` - Tagged literal integer.

* `a` - Tagged literal atom.

* `n` - NIL (`[]`, the empty list).

* `q` - Literals that don't fit in a word, such as list or tuples.

* `f` - Non-zero failure label.

* `p` - The zero failure label.

* `j` - Any label.  Equivalent to `fp`.

* `c` - Any literal term.  Equivalent to `ainq`.

* `s` - X register, Y register, or any literal term.  Equivalent to `xyc`.

* `d` - X or Y register.  Equivalent to `xy`.  (In a pattern `d` will
match both source and destination registers.  As an operand in a specific
instruction, it must only be used for a destination register.)

* `o` - Overflow.  An untagged integer that does not fit in a machine word.

#### Guard constraints ####

If the constraints described so far is not enough, additional
constraints can be written in C in `beam_load.c` and be called as a
guard function on the left side of the transformation.  If the guard
function returns a non-zero value, the matching of the rule will
continue, otherwise the match will fail.  For example:

    ensure_map Lit=q | literal_is_map(Lit) =>

The guard test `literal_is_map/1` tests whether the given literal is a map.
If the literal is a map, the instruction is unnecessary and can be removed.

It is outside the scope for this document to describe in detail how such
guard functions are written, but for the curious here is the implementation
of `literal_is_map()`:

    static int
    literal_is_map(LoaderState* stp, GenOpArg Lit)
    {
        Eterm term;

        ASSERT(Lit.type == TAG_q);
        term = stp->literals[Lit.val].term;
        return is_map(term);
    }

#### Handling instruction with variable number of operands ####

Some instructions, such as `select_val/3`, essentially has a variable
number of operands.  Such instructions have a `{list,[...]}` operand
as their last operand in the BEAM assembly code.  For example:

    {select_val,{x,0},
                {f,1},
                {list,[{atom,b},{f,4},{atom,a},{f,5}]}}.

The loader will convert a `{list,[...]}` operand to an `u` operand whose
value is the number of elements in the list, followed by each element in
the list.  The instruction above would be translated to the following
generic instruction:

    {select_val,{x,0},{f,1},{u,4},{atom,b},{f,4},{atom,a},{f,5}}

To match a variable number of arguments we need to use the special
operand type `*` like this:

    select_val Src=aiq Fail=f Size=u List=* => \
        i_const_select_val Src Fail Size List

This transformation renames a `select_val/3` instruction
with a constant source operand to `i_const_select_val/3`.

#### Constructing new instructions on the right side ####

The most common operand on the right side is a variable that was bound while
matching the left side.  For example:

    trim N Remaining => i_trim N

An operand can also be a type letter to construct an operand of that type.
Each type has a default value.  For example, the type `x` has the default
value 1023, which is the highest X register.  That makes `x` on the right
side a convenient shortcut for a temporary X register.  For example:

    is_number Fail Literal=q => move Literal x | is_number Fail x

If the second operand for `is_number/2` is a literal, it will be moved to
X register 1023.  Then `is_number/2` will test whether the value stored in
X register 1023 is a number.

This kind of transformation is useful when it is rare that an operand can
be anything else but a register.  In the case of `is_number/2`, the second
operand is always a register unless the compiler optimizations have been
disabled.

If the default value is not suitable, the type letter can be followed
by `=` and a value.  Most types take an integer value.  The value for
an atom is written the same way as in the C source code.  For example,
the atom `false` is written as `am_false`.  The atom must be listed in
`atom.names`.

Here is an example showing how values can be specified:

    bs_put_utf32 Fail=j Flags=u Src=s => \
        i_bs_validate_unicode Fail Src | \
        bs_put_integer Fail i=32 u=1 Flags Src

#### Type letters on the right side ####

Here follows all types that are allowed to be used in operands for
instructions being constructed on the right side of a transformation
rule.

* `u` - Construct an untagged integer.  The default value is 0.

* `x` - X register.  The default value is 1023.  That makes `x` convenient to
use as a temporary X register.

* `y` - Y register.  The default value is 0.

* `l` - Foating point register number.  The default value is 0.

* `i` - Tagged literal integer.  The default value is 0.

* `a` - Tagged atom.  The default value is the empty atom (`am_Empty`).

* `n` - NIL (`[]`, the empty list).

#### Function call on the right side ####

Transformations that are not possible to describe with the rule
language as described here can be written as a C function in
`beam_load.c` and called from the right side of a transformation.  The
left side of the transformation will perform the match and bind
operands to variables.  The variables can then be passed to a
generator function on the right side.  For example:

    bif2 Fail=j u$bif:erlang:element/2 Index=s Tuple=xy Dst=d => \
        gen_element(Jump, Index, Tuple, Dst)

This transformation rule matches a call to the BIF `element/2`.
The operands will be captured and the function `gen_element()` will
be called.

`gen_element()` will produce one of two instructions depending
on `Index`.  If `Index` is an integer in the range from 1 up to
the maximum tuple size, the instruction `i_fast_element/2` will
be produced, otherwise the instruction `i_element/4` will be
produced.  The corresponding specific instructions are:

    i_fast_element xy j? I d
    i_element xy j? s d

The `i_fast_element/2` instruction is faster because the tuple is
already an untagged integer.  It also knows that the index is at least
1, so it does not have to test for that.  The `i_element/4`
instruction will have to fetch the index from a register, test that it
is an integer, and untag the integer.

It is outside the scope of this document to describe in detail how
generator functions are written, but for the curious, here is the
implementation of `gen_element()`:

    static GenOp*
    gen_element(LoaderState* stp, GenOpArg Fail,
       GenOpArg Index, GenOpArg Tuple, GenOpArg Dst)
    {
        GenOp* op;

        NEW_GENOP(stp, op);
        op->arity = 4;
        op->next = NULL;

        if (Index.type == TAG_i && Index.val > 0 &&
           Index.val <= ERTS_MAX_TUPLE_SIZE &&
           (Tuple.type == TAG_x || Tuple.type == TAG_y)) {
            op->op = genop_i_fast_element_4;
            op->a[0] = Tuple;
            op->a[1] = Fail;
            op->a[2].type = TAG_u;
            op->a[2].val = Index.val;
            op->a[3] = Dst;
        } else {
            op->op = genop_i_element_4;
            op->a[0] = Tuple;
            op->a[1] = Fail;
            op->a[2] = Index;
            op->a[3] = Dst;
        }

        return op;
    }
}

### Defining the implementation ###

The actual implementation of instructions are also defined in `.tab`
files processed by **beam\_makeops**.  For practical reasons,
instruction definitions are stored in several files, at the time of
writing in the following files:

    bif_instrs.tab
    arith_instrs.tab
    bs_instrs.tab
    float_instrs.tab
    instrs.tab
    map_instrs.tab
    msg_instrs.tab
    select_instrs.tab
    trace_instrs.tab

There is also a file that only contains macro definitions:

    macros.tab

The syntax of each file is similar to C code.  In fact, most of
the contents *is* C code, interspersed with macro invocations.

To allow Emacs to auto-indent the code, each file starts with the
following line:

    // -*- c -*-

To avoid messing up the indentation, all comments are written
as C++ style comments (`//`) instead of `#`.  Note that a comment
must start at the beginning of a line.

The meat of an instruction definition file are macro definitions.
We have seen this macro definition before:

    move(Src, Dst) {
        $Dst = $Src;
    }

A macro definitions must start at the beginning of the line (no spaces
allowed), the opening curly bracket must be on the same line, and the
finishing curly bracket must be at the beginning of a line.  It is
recommended that the macro body is properly indented.

As a convention, the macro arguments in the head all start with an
uppercase letter.  In the body, the macro arguments can be expanded
by preceding them with `$`.

A macro definition whose name and arity matches a family of
specific instructions is assumed to be the implementation of that
instruction.

A macro can also be invoked from within another macro.  For example,
`move_deallocate_return/2` avoids repeating code by invoking
`$deallocate_return()` as a macro:

    move_deallocate_return(Src, Deallocate) {
        x(0) = $Src;
        $deallocate_return($Deallocate);
    }

Here is the definition of `deallocate_return/1`:

    deallocate_return(Deallocate) {
        //| -no_next
        int words_to_pop = $Deallocate;
        SET_I((BeamInstr *) cp_val(*E));
        E = ADD_BYTE_OFFSET(E, words_to_pop);
        CHECK_TERM(x(0));
        DispatchReturn;
    }

The expanded code for `move_deallocate_return` will look this:

    OpCase(move_deallocate_return_cQ):
    {
      x(0) = I[1];
      do {
        int words_to_pop = Qb(BeamExtraData(I[0]));
        SET_I((BeamInstr *) cp_val(*E));
        E = ADD_BYTE_OFFSET(E, words_to_pop);
        CHECK_TERM(x(0));
        DispatchReturn;
      } while (0);
    }

When expanding macros, **beam\_makeops** wraps the expansion in a
`do`/`while` wrapper unless **beam\_makeops** can clearly see that no
wrapper is needed.  In this case, the wrapper is needed.

Note that arguments for macros cannot be complex expressions, because
the arguments are split on `,`.  For example, the following would
not work because **beam\_makeops** would split the expression into
two arguments:

    $deallocate_return(get_deallocation(y, $Deallocate));

#### Code generation directives ####

Within macro definitions, `//` comments are in general not treated
specially.  They will be copied to the file with the generated code
along with the rest of code in the body.

However, there is an exception. Within a macro definition, a line that
starts with whitespace followed by `//|` is treated specially.  The
rest of the line is assumed to contain directives to control code
generation.

Currently, two code generation directives are recognized:

* `-no_prefetch`
* `-no_next`

##### The -no_prefetch directive #####

To see what `-no_prefetch` does, let's first look at the default code
generation.  Here is the code generated for `move_cx`:

    OpCase(move_cx):
    {
      BeamInstr next_pf = BeamCodeAddr(I[2]);
      xb(BeamExtraData(I[0])) = I[1];
      I += 2;
      ASSERT(VALID_INSTR(next_pf));
      GotoPF(next_pf);
    }

Note that the very first thing done is to fetch the address to the
next instruction.  The reason is that it usually improves performance.

Just as a demonstration, we can add a `-no_prefetch` directive to
the `move/2` instruction:

    move(Src, Dst) {
        //| -no_prefetch
        $Dst = $Src;
    }

We can see that the prefetch is no longer done:

    OpCase(move_cx):
    {
      xb(BeamExtraData(I[0])) = I[1];
      I += 2;
      ASSERT(VALID_INSTR(*I));
      Goto(*I);
    }

When would we want to turn off the prefetch in practice?

In instructions that will not always execute the next instruction.
For example:

    is_atom(Fail, Src) {
        if (is_not_atom($Src)) {
            $FAIL($Fail);
        }
    }

    // From macros.tab
    FAIL(Fail) {
        //| -no_prefetch
        $SET_I_REL($Fail);
        Goto(*I);
    }

`is_atom/2` may either execute the next instruction (if the second
operand is an atom) or branch to the failure label.

The generated code looks like this:

    OpCase(is_atom_fx):
    {
      if (is_not_atom(xb(I[1]))) {
        ASSERT(VALID_INSTR(*(I + (fb(BeamExtraData(I[0]))) + 0)));
        I += fb(BeamExtraData(I[0])) + 0;;
        Goto(*I);;
      }
      I += 2;
      ASSERT(VALID_INSTR(*I));
      Goto(*I);
    }

##### The -no_next directive #####

Next we will look at when the `-no_next` directive can be used.  Here
is the `jump/1` instruction:

    jump(Fail) {
        $JUMP($Fail);
    }

    // From macros.tab
    JUMP(Fail) {
        //| -no_next
        $SET_I_REL($Fail);
        Goto(*I);
    }

The generated code looks like this:

    OpCase(jump_f):
    {
      ASSERT(VALID_INSTR(*(I + (fb(BeamExtraData(I[0]))) + 0)));
      I += fb(BeamExtraData(I[0])) + 0;;
      Goto(*I);;
    }

If we remove the `-no_next` directive, the code would look like this:

    OpCase(jump_f):
    {
      BeamInstr next_pf = BeamCodeAddr(I[1]);
      ASSERT(VALID_INSTR(*(I + (fb(BeamExtraData(I[0]))) + 0)));
      I += fb(BeamExtraData(I[0])) + 0;;
      Goto(*I);;
      I += 1;
      ASSERT(VALID_INSTR(next_pf));
      GotoPF(next_pf);
    }

In the end, the C compiler will probably optimize this code to the
same native code as the first version, but the first version is certainly
much easier to read for human readers.

#### Macros in the macros.tab file ####

The file `macros.tab` contains many useful macros.  When implementing
new instructions it is good practice to look through `macros.tab` to
see if any of existing macros can be used rather than re-inventing
the wheel.

We will describe a few of the most useful macros here.

##### The GC_REGEXP definition #####

The following line defines a regular expression that will recognize
a call to a function that does a garbage collection:

     GC_REGEXP=erts_garbage_collect|erts_gc|GcBifFunction;

The purpose is that **beam\_makeops** can verify that an instruction
that does a garbage collection and has an `d` operand uses the
`$REFRESH_GEN_DEST()` macro.

If you need to define a new function that does garbage collection,
you should give it the prefix `erts_gc_`.  If that is not possible
you should update the regular expression so that it will match your
new function.

##### FAIL(Fail) #####

Branch to `$Fail`.  Will suppress prefetch (`-no_prefetch`).  Typical use:

    is_nonempty_list(Fail, Src) {
        if (is_not_list($Src)) {
            $FAIL($Fail);
        }
    }

##### JUMP(Fail) #####

Branch to `$Fail`.  Suppresses generation of dispatch of the next
instruction (`-no_next`).  Typical use:

    jump(Fail) {
        $JUMP($Fail);
    }

##### GC_TEST(NeedStack, NeedHeap, Live) #####

`$GC_TEST(NeedStack, NeedHeap, Live)` tests that given amount of
stack space and heap space is available.  If not it will do a
garbage collection.  Typical use:

    test_heap(Nh, Live) {
        $GC_TEST(0, $Nh, $Live);
    }

##### AH(NeedStack, NeedHeap, Live) #####

`AH(NeedStack, NeedHeap, Live)` allocates a stack frame and
optionally additional heap space.

#### Pre-defined macros and variables ####

**beam\_makeops** defines several built-in macros and pre-bound variables.

##### The NEXT_INSTRUCTION pre-bound variable #####

The NEXT_INSTRUCTION is a pre-bound variable that is available in
all instructions.  It expands to the address of the next instruction.

Here is an example:

    i_call(CallDest) {
        SET_CP(c_p, $NEXT_INSTRUCTION);
        $DISPATCH_REL($CallDest);
    }

When calling a function, the return address is first stored in `c_p->cp`
(using the `SET_CP()` macro defined in `beam_emu.c`), and then control is
transferred to the callee.  Here is the generated code:

    OpCase(i_call_f):
    {
      SET_CP(c_p, I+1);
      ASSERT(VALID_INSTR(*(I + (fb(BeamExtraData(I[0]))) + 0)));
      I += fb(BeamExtraData(I[0])) + 0;;
      DTRACE_LOCAL_CALL(c_p, erts_code_to_codemfa(I));
      Dispatch();;
    }

We can see that that `$NEXT_INSTRUCTION` has been expanded to `I+1`.
That makes sense since the size of the `i_call_f/1` instruction is
one word.

##### The IP_ADJUSTMENT pre-bound variable #####

`$IP_ADJUSTMENT` is usually 0.  In a few combined instructions
(described below) it can be non-zero.  It is used like this
in `macros.tab`:

    SET_I_REL(Offset) {
        ASSERT(VALID_INSTR(*(I + ($Offset) + $IP_ADJUSTMENT)));
        I += $Offset + $IP_ADJUSTMENT;
    }

Avoid using `IP_ADJUSTMENT` directly.  Use `SET_I_REL()` or
one of the macros that invoke such as `FAIL()` or `JUMP()`
defined in `macros.tab`.

#### Pre-defined macro functions ####

##### The IF() macro #####

`$IF(Expr, IfTrue, IfFalse)` evaluates `Expr`, which must be a valid
Perl expression (which for simple numeric expressions have the same
syntax as C).  If `Expr` evaluates to 0, the entire `IF()` expression will be
replaced with `IfFalse`, otherwise it will be replaced with `IfTrue`.

See the description of `OPERAND_POSITION()` for an example.

##### The OPERAND\_POSITION() macro #####

`$OPERAND_POSITION(Expr)` returns the position for `Expr`, if
`Expr` is an operand that is not packed.  The first operand is
at position 1.

Returns 0 otherwise.

This macro could be used like this in order to share code:

    FAIL(Fail) {
        //| -no_prefetch
        $IF($OPERAND_POSITION($Fail) == 1 && $IP_ADJUSTMENT == 0,
            goto common_jump,
            $DO_JUMP($Fail));
    }

    DO_JUMP(Fail) {
        $SET_I_REL($Fail);
        Goto(*I));
    }

    // In beam_emu.c:
    common_jump:
       I += I[1];
       Goto(*I));


#### The $REFRESH\_GEN\_DEST() macro ####

When a specific instruction has a `d` operand, early during execution
of the instruction, a pointer will be initialized to point to the X or
Y register in question.

If there is a garbage collection before the result is stored,
the stack will move and if the `d` operand refered to a Y
register, the pointer will no longer be valid.  (Y registers are
stored on the stack.)

In those circumstances, `$REFRESH_GEN_DEST()` must be invoked
to set up the pointer again.  **beam\_makeops** will notice
if there is a call to a function that does a garbage collection and
`$REFRESH_GEN_DEST()` is not called.

Here is a complete example.  The `new_map` instruction is defined
like this:

    new_map d t I

It is implemented like this:

    new_map(Dst, Live, N) {
        Eterm res;

        HEAVY_SWAPOUT;
        res = erts_gc_new_map(c_p, reg, $Live, $N, $NEXT_INSTRUCTION);
        HEAVY_SWAPIN;
        $REFRESH_GEN_DEST();
        $Dst = res;
        $NEXT($NEXT_INSTRUCTION+$N);
    }

If we have forgotten the `$REFRESH_GEN_DEST()` there would be a message
similar to this:

    pointer to destination register is invalid after GC -- use $REFRESH_GEN_DEST()
    ... from the body of new_map at beam/map_instrs.tab(30)

#### Combined instructions ####

**Problem**: For frequently executed instructions we want to use
"fast" operands types such as `x` and `y`, as opposed to `s` or `S`.
To avoid an explosion in code size, we want to share most of the
implementation between the instructions.  Here are the specific
instructions for `i_increment/5`:

    i_increment r W t d
    i_increment x W t d
    i_increment y W t d

The `i_increment` instruction is implemented like this:

    i_increment(Source, IncrementVal, Live, Dst) {
        Eterm increment_reg_source = $Source;
        Eterm increment_val = $IncrementVal;
        Uint live;
        Eterm result;

        if (ERTS_LIKELY(is_small(increment_reg_val))) {
            Sint i = signed_val(increment_reg_val) + increment_val;
            if (ERTS_LIKELY(IS_SSMALL(i))) {
                $Dst = make_small(i);
                $NEXT0();
            }
        }
        live = $Live;
        HEAVY_SWAPOUT;
        reg[live] = increment_reg_val;
        reg[live+1] = make_small(increment_val);
        result = erts_gc_mixed_plus(c_p, reg, live);
        HEAVY_SWAPIN;
        ERTS_HOLE_CHECK(c_p);
        if (ERTS_LIKELY(is_value(result))) {
            $REFRESH_GEN_DEST();
            $Dst = result;
            $NEXT0();
        }
        ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
        goto find_func_info;
    }

There will be three almost identical copies of the code.  Given the
size of the code, that could be too high cost to pay.

To avoid the three copies of the code, we could use only one specific
instruction:

    i_increment S W t d

(The same implementation as above will work.)

That reduces the code size, but is slower because `S` means that
there will be extra code to test whether the operand refers to an X
register or a Y register.

**Solution**: We can use "combined instructions".  Combined
instructions are combined from instruction fragments.  The
bulk of the code can be shared.

Here we will show how `i_increment` can be implemented as a combined
instruction.  We will show each individual fragment first, and then
show how to connect them together.  First we will need a variable that
we can store the value fetched from the register in:

    increment.head() {
        Eterm increment_reg_val;
    }

The name `increment` is the name of the group that the fragment
belongs to.  Note that it does not need to have the same
name as the instruction.  The group name is followed by `.` and
the name of the fragment.  The name `head` is pre-defined.
The code in it will be placed at the beginning of a block, so
that all fragments in the group can access it.

Next we define the fragment that will pick up the value from the
register from the first operand:

    increment.fetch(Src) {
        increment_reg_val = $Src;
    }

We call this fragment `fetch`.  This fragment will be duplicated three
times, one for each value of the first operand (`r`, `x`, and `y`).

Next we define the main part of the code that do the actual incrementing.

    increment.execute(IncrementVal, Live, Dst) {
        Eterm increment_val = $IncrementVal;
        Uint live;
        Eterm result;

        if (ERTS_LIKELY(is_small(increment_reg_val))) {
            Sint i = signed_val(increment_reg_val) + increment_val;
            if (ERTS_LIKELY(IS_SSMALL(i))) {
                $Dst = make_small(i);
                $NEXT0();
            }
        }
        live = $Live;
        HEAVY_SWAPOUT;
        reg[live] = increment_reg_val;
        reg[live+1] = make_small(increment_val);
        result = erts_gc_mixed_plus(c_p, reg, live);
        HEAVY_SWAPIN;
        ERTS_HOLE_CHECK(c_p);
        if (ERTS_LIKELY(is_value(result))) {
            $REFRESH_GEN_DEST();
            $Dst = result;
            $NEXT0();
        }
        ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
        goto find_func_info;
    }

We call this fragment `execute`.  It will handle the three remaining
operands (`W t d`).  There will only be one copy of this fragment.

Now that we have defined the fragments, we need to inform
**beam\_makeops** how they should be connected:

    i_increment := increment.fetch.execute;

To the left of the `:=` is the name of the specific instruction that
should be implemented by the fragments, in this case `i_increment`.
To the right of `:=` is the name of the group with the fragments,
followed by a `.`.  Then the name of the fragments in the group are
listed in the order they should be executed.  Note that the `head`
fragment is not listed.

The line ends in `;` (to avoid messing up the indentation in Emacs).

(Note that in practice the `:=` line is usually placed before the
fragments.)

The generated code looks like this:

    {
      Eterm increment_reg_val;
      OpCase(i_increment_rWtd):
      {
        increment_reg_val = r(0);
      }
      goto increment__execute;

      OpCase(i_increment_xWtd):
      {
        increment_reg_val = xb(BeamExtraData(I[0]));
      }
      goto increment__execute;

      OpCase(i_increment_yWtd):
      {
        increment_reg_val = yb(BeamExtraData(I[0]));
      }
      goto increment__execute;

      increment__execute:
      {
        // Here follows the code from increment.execute()
        .
        .
        .
    }

##### Some notes about combined instructions #####

The operands that are different must be at
the beginning of the instruction.  All operands in the last
fragment must have the same operands in all variants of
the specific instruction.

As an example, the following specific instructions cannot be
implemented as a combined instruction:

    i_times j? t x x d
    i_times j? t x y d
    i_times j? t s s d

We would have to change the order of the operands so that the
two operands that are different are placed first:

    i_times x x j? t d
    i_times x y j? t d
    i_times s s j? t d

We can then define:

    i_times := times.fetch.execute;

    times.head {
        Eterm op1, op2;
    }

    times.fetch(Src1, Src2) {
        op1 = $Src1;
        op2 = $Src2;
    }

    times.execute(Fail, Live, Dst) {
        // Multiply op1 and op2.
        .
        .
        .
    }

Several instructions can share a group.  As an example, the following
instructions have different names, but in the end they all create a
binary.  The last two operands are common for all of them:

    i_bs_init_fail       xy j? t? x
    i_bs_init_fail_heap s I j? t? x
    i_bs_init                W t? x
    i_bs_init_heap         W I t? x

The instructions are defined like this (formatted with extra
spaces for clarity):

    i_bs_init_fail_heap := bs_init . fail_heap . verify . execute;
    i_bs_init_fail      := bs_init . fail      . verify . execute;
    i_bs_init           := bs_init .           .  plain . execute;
    i_bs_init_heap      := bs_init .               heap . execute;

Note that the first two instruction have three fragments, while the
other two only have two fragments.  Here are the fragments:

    bs_init_bits.head() {
        Eterm num_bits_term;
        Uint num_bits;
        Uint alloc;
    }

    bs_init_bits.plain(NumBits) {
        num_bits = $NumBits;
        alloc = 0;
    }

    bs_init_bits.heap(NumBits, Alloc) {
        num_bits = $NumBits;
        alloc = $Alloc;
    }

    bs_init_bits.fail(NumBitsTerm) {
        num_bits_term = $NumBitsTerm;
        alloc = 0;
    }

    bs_init_bits.fail_heap(NumBitsTerm, Alloc) {
        num_bits_term = $NumBitsTerm;
        alloc = $Alloc;
    }

    bs_init_bits.verify(Fail) {
        // Verify the num_bits_term, fail using $FAIL
        // if there is a problem.
	.
	.
	.
    }

    bs_init_bits.execute(Live, Dst) {
       // Long complicated code to a create a binary.
       .
       .
       .
    }

The full definitions of those instructions can be found in `bs_instrs.tab`.
The generated code can be found in `beam_warm.h`.
