BEAM SSA Checks
===============

While developing optimizations operating on the BEAM SSA it is often
hard to check that various transforms have the intended effect. For
example, unless a transform produces crashing code, it is hard to
detect that the transform is broken. Likewise missing an optimization
opportunity is also hard to detect.

To simplify the creation of tests on BEAM SSA the compiler has an
internal mode in which it parses and checks assertions on the
structure and content of the produced BEAM SSA code. This is a short
introduction to the syntax and semantics of the SSA checker
functionality.

Syntax
------

SSA checks are embedded in the source code as comments starting with
with one of `%ssa%`, `%%ssa%` or `%%%ssa%`. This is a short
introduction the syntax, for the full syntax please refer to the
`ssa_check_when_clause` production in `erl_parse.yrl`.

SSA checks can be placed inside any Erlang function, for example:

    t0() ->
    %ssa% () when post_ssa_opt ->
    %ssa%   ret(#{}).
      #{}.

will check that `t0/0` returns the literal `#{}`. If we want to check
that a function returns its first formal parameter, we can write:

    t1(A, _B) ->
    %ssa% (X, _) when post_ssa_opt ->
    %ssa%   ret(X).
      A.

Note how we match the first formal parameter using `X`. The reason for
having our own formal parameters for the SSA check, is that we don't
want to introduce new identifiers at the Erlang level to support
SSA-level checks. Consider if `t1/2` had been defined as `t1([A|As],
B)` we would have had to introduce a new identifier for the aggregate
value `[A|As]`.

The full syntax for a SSA check clause is:

    <expected-result>? (<formals>) when <pipeline-location> -> <checks> '.'

where `<expected-result>` can be one of `pass` (the check must
succeed), `fail` and `xfail` (the check must fail). Omitting
`<expected-result>` is parsed as an implicit `pass`.

`<formals>` is a comma-separated list of variables.

`<pipeline-location>` specifies when in the compiler pipeline to run
the checks. For now the only supported value for `<pipeline-location>`
is `post_ssa_opt` which runs the checks after the `ssa_opt` pass.

`<checks>` is a comma-separated list of matches against the BEAM SSA
code. For non-flow-control operations the syntax is:

    <variable> = <operation> ( <arguments> ) <annotation>?

where `<operation>` is the `#b_set.op` field from the internal SSA
representation. BIFs are written as `bif:<atom>`.

`<arguments>` is a comma-separated list of variables or literals.

For flow control operations and labels, the syntax is as follows:

    br(<bool>, <true-label>, <false-label>)

    switch(<value>, <fail-label>, [{<label>,<value>},...])

	ret(<value>)

	label <value>

where `<value>` is a literal or a variable.

A check can also include an assertion on operation annotations. The
assertion is written as a map-like pattern following the argument
list, for example:

    t0() ->
    %ssa% () when post_ssa_opt ->
    %ssa% _ = call(fun return_int/0) { result_type => {t_integer,{17,17}},
    %ssa%                              location => {_,32} },
    %ssa% _ = call(fun return_tuple/0) {
    %ssa%    result_type => {t_tuple,2,true,#{1 => {t_integer,{1,1}},
    %ssa%                                     2 => {t_integer,{2,2}}}}
    %ssa% }.
        X = return_int(),
        Y = return_tuple(),
        {X, Y}.

Semantics
---------

When an SSA assertion is matched against the BEAM SSA for a function,
patterns are applied sequentially. If the current pattern doesn't
match, the checker tries with the next instruction. If the checker
reaches the end of the SSA representation without having matched all
patterns, the check is considered failed otherwise the assertion is
considered successful. When a pattern is matched against an SSA
operation, the values of variables already bound are considered and if
the patterns matches, free variables introduced in the successfully
matched pattern are bound to the values they have in the matched
operation.

Compiler integration
--------------------

The BEAM SSA checker is enabled by the compiler option
`{check_ssa,post_ssa_opt}`. When enabled, a failed SSA assertion will
be reported using the same mechanisms as an ordinary compilation
error.

Limitations
-----------

* Unbound variables are not allowed in the key-part of map literals nor
in annotation assertions.
