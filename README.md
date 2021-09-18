# Erlang/OTP + EEP49

Our entry for Spawnfest is the implementation of [EEP-49](https://github.com/erlang/eep/blob/master/eeps/eep-0049.md) onto regular Erlang/OTP.

EEP-49 extends the `begin ... end` expression to make it a construct usable for control flow and value-based error handling based on pattern matching.

This introduces `begin ... else ... end` along with a new contextual use of the `<-` operator to replace or simplify deeply-nested `case ... end` expressions, and prevent using exceptions for control flow.

This branch is forked off `master` branch as recommended by the contributing guides.

## Team Members

- @peerst
- @ferd
- The whole batch of contributors in OTP before Spawnfest.

## Building

Nothing should change from regular builds of Erlang/OTP. See the [HOWTO/](https://github.com/spawnfest/eep49ers/tree/eep-49/HOWTO) section.

## Running Tests

You can follow the standard instructions [in the official wiki](https://github.com/erlang/otp/wiki/Running-tests).

We specifically require compiler change for the tests, so until we add a new suite, just building the project itself might be considered a successful test.
