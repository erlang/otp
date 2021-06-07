Invariants on the Structure and Format of BEAM SSA
==================================================

Function Calls
--------------

All function calls not in a tail call position must be followed by a
succeeded:body-instruction unless one of the following exceptions
apply:

* The function call can statically be proven to always fail.

* The function call is to the `erlang`-module and can statically be
  proven to always succeed or fail.

Variable Naming
---------------

A variable name in BEAM SSA is either an atom, a non-negative integer
or a tuple: `atom() | non_neg_integer() | {atom() | non_neg_integer(),
non_neg_integer()}`. In order to generate fresh unused variable names,
all compiler transforms maintain a counter, the `cnt`-field in the
`opt_st`-record, which is incremented each time a new variable or
label is created. In the following description the value of the
`cnt`-field is called `Cnt`.

Due to peculiarities in the BEAM SSA code generator, a compiler
transformation unfortunately cannot just use the `cnt`-value directly
as a fresh name. There are three basic strategies for creating fresh
variable names which can by used by a compiler pass:

1) A name can be derived from an existing name of the form `V ::
  atom() | non_neg_integer()` by selecting an atom, which is unique to
  the compiler pass, to form a new name `{A, V}`. The same `A` cannot
  be used by strategy 3) below.

2) A name can be derived from an existing name of the form `V ::
  non_neg_integer()` by combining it with the `cnt`-field into `{V,
  Cnt}`.

3) A fresh name can be created by selecting an atom `A`, which is
  unique to the compiler pass, to form the new name `{A, Cnt}`. The
  same `A` cannot be used by strategy 1) above.
