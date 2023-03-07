Invariants on the Structure and Format of BEAM SSA
==================================================

Exception Handling
------------------

The translation of a `try`-`catch` expression into BEAM SSA has the
following structure:

    @tag = new_try_tag `try`
	br @tag, ^protected_block0, ^landing_pad_block

	protected_block0:
	  @success0 = ... % Something that could raise an exception
	  br @success0, ^protected_block1, ^landing_pad_block

	...

	protected_blockN:
	  % The end of the protected code
	  @ignored0 = kill_try_tag @tag
      br ^after_try_catch

	landing_pad_block:
	  @aggregate = landingpad try, @tag
	  @class  = extract @aggregate, `0` % The error class
	  @reason = extract @aggregate, `1` % The reason
	  @stk    = extract @aggregate, `2` % The stack trace
	  @ignored1 = kill_try_tag @tag
	  %% Pattern matching on @class, @reason, and @stk is done here
	  %% to send control to the appropriate catch clause
      br ^after_try_catch

    after_try_catch:
      % Normal execution continues

The following invariants must hold for the SSA:

 * All code that can cause an exception in one of the protected blocks
   must have explicit control flow edges to the landing pad block. If
   there are no edges to the landing pad block except from the block
   containing the `new_try_tag`, the compiler will remove the
   redundant exception handler.
 * The extraction of the class, reason and stack trace from the result
   of the `landingpad` instruction must be done in that
   order. Omitting the extraction of elements which are unused is
   allowed.
 * Both the landing pad block and the final protected block must end
   with a `kill_try_tag` instruction. Trying to share the
   `kill_try_tag` epilogue between the last protected block and the
   landing pad is unlikely to work.

The translation of an old-style `catch` expression into BEAM SSA has
the following structure:

    @tag = new_try_tag `try`
	br @tag, ^protected_block0, ^landing_pad_block

	protected_block0:
	  @success0 = ... % Something that could raise an exception
	  br @success0, ^protected_block1, ^landing_pad_block

	...

	protected_blockN:
	  % The end of the protected code
	  @successful_result = .... % The result of a successful computation
	  br ^common_end_of_catch

	landing_pad_block:
	   @aggregate = landingpad catch, @tag
	   @catched_val = extract @ssa_agg, `0`
	   br ^common_end_of_catch

	common_end_of_catch:
	  @tmp = phi { @catched_val, ^landing_pad_block },
	             { @successful_result, ^protected_blockN }
	  @result_of_catch_expr = catch_end @tag, @tmp

Just as for a `try`-`catch` expression all code that can cause an
exception in one of the protected blocks must have explicit control
flow edges to the landing pad block.

Exception Re-issuing
--------------------

A typical user-written `try`-`catch` expression will catch a subset of
all possible exception classes and reasons and leave unhandled
exceptions to a handler further up the call stack. Re-issuing an
exception is done with the `resume` instruction. The `resume` must
come after the `kill_try_tag` instruction in the program flow. For
example, if the [example in the Exception Handling Section](#exception-handling)
was to only handle user `throws`, the relevant blocks would look like this:

	landing_pad_block:
	  @aggregate = landingpad `try`, @tag
	  @class  = extract @aggregate, `0` % The error class
	  @reason = extract @aggregate, `1` % The reason
	  @stk    = extract @aggregate, `2` % The stack trace
	  @ignored1 = kill_try_tag @tag
	  @is_throw = bif:'=:=' @class, `throw`
      br @is_throw ^first_block_of_throw_handler, ^reissue

	first_block_of_throw_handler:
	  %% Handle the user-defined throw

	reissue:
	  @tmp = resume @stk, @reason
	  ret @tmp

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

A variable name in BEAM SSA is either an atom or a non-negative
integer:

    atom() | non_neg_integer()

In order to generate fresh unused variable names, all compiler
transforms maintain a counter, the `cnt`-field in the `b_function` and
`opt_st` records, which is incremented each time a new variable or
label is created. In the following description the value of the
`cnt`-field is called `Cnt`. The `Cnt` value is guaranteed to never
clash with a previously defined variable name. Therefore, value of
`Cnt` can directly be used as a variable name in the SSA passes.

Note that the rules were more complicated before Erlang/OTP 27, because
the `Cnt` value could clash with other variables.
