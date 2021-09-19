# Erlang/OTP + EEP49

Our entry for Spawnfest is the implementation of [EEP-49](https://github.com/erlang/eep/blob/master/eeps/eep-0049.md) onto regular Erlang/OTP.

EEP-49 extends the `begin ... end` expression to make it a construct usable for control flow and value-based error handling based on pattern matching.

This introduces `begin ... else ... end` along with a new contextual use of the `<-` operator to replace or simplify deeply-nested `case ... end` expressions, and prevent using exceptions for control flow.

This branch is forked off `master` branch as recommended by the contributing guides.

## Team Members

- @peerst
- @ferd
- The whole batch of contributors in OTP before Spawnfest.

## Achievments

While implementing we figured out that the choice of `else` in EEP-49 was more trouble than expected. Not only would it collide with usage of atom `else` but also with the `-else` attribute of epp.

So we decide to go with `begin ... cond ... end` instead.  Also trying out the alternative `begin ... catch ... end` in not merged PR [Switch from 'cond' to 'catch' in maybe clauses](https://github.com/spawnfest/eep49ers/pull/10). Keyword `cond` was always reserved but not used so far and `catch` is used for exceptions but that usage wouldn't collide.  We also discussed `cond ... catch ... end` which might or might not collide with the initially indended `cond` expression.

## Building

Nothing should change from regular builds of Erlang/OTP. See the [HOWTO/](https://github.com/spawnfest/eep49ers/tree/eep-49/HOWTO) section.

## Running Tests

You can follow the standard instructions [in the official wiki](https://github.com/erlang/otp/wiki/Running-tests).

We specifically require compiler change for the tests, so until we add a new suite, just building the project itself might be considered a successful test.

To run only the tests needed, assuming bash and `xterm` being installed (for the compiler suite):

```bash
export ERL_TOP=$PWD
export PATH=$ERL_TOP/bin:$PATH

## The Erlang makefile system does not always catch changes made
## in files, so if the below steps do not work, commit your work
## and try again after running `git clean -xfdq`, which clears all
## unchecked files.
./otp_build setup -a --prefix=$PWD/tests_install
make install
./otp_build tests
export PATH=$PWD/tests_install/bin:$PATH
unset ERL_LIBS

cd release/tests/test_server
$ERL_TOP/tests_install/bin/erl

ts:install().
ts:run(stdlib, [erl_scan_SUITE, erl_lint_SUITE, begin_maybe_SUITE, erl_eval_SUITE]).
ts:run(syntax_tools, [syntax_tools_SUITE]).
% ts:run(stdlib, [batch]). % this takes many minutes to run and tests a lot of unrelated stuff
% ts:run(compiler, [batch]). % requires a bunch of external deps and remote displays?
^C
```

Re-runing tests after requires re-building things and starting afresh. You may have to delete the .beam files you modified to get the rebuild step to work.

```bash
cd $ERL_TOP
rm -rf tests_install/lib/erlang/lib/stdlib-3.15.2/ebin/* tests_install/lib/erlang/lib/syntax_tools-2.6/ebin/*
```

