<!--
%CopyrightBegin%

SPDX-License-Identifier: Apache-2.0

Copyright Ericsson AB 2025. All Rights Reserved.

%CopyrightEnd%
-->

### Distribution Control Messages

#### `ALIAS_SEND` and `ALIAS_SEND_TT`

The [`ALIAS_SEND`](`e:erts:erl_dist_protocol.md#ALIAS_SEND`) and
[`ALIAS_SEND_TT`](`e:erts:erl_dist_protocol.md#ALIAS_SEND_TT`) distribution
control messages are as of OTP 28 deprecated and have been scheduled for removal in
OTP 30. The support for these control messages are indicated by the
[`DFLAG_ALIAS`](`e:erts:erl_dist_protocol.md#DFLAG_ALIAS`) distribution flag.

The `ALIAS_SEND` and `ALIAS_SEND_TT` control messages are as of OTP 28 replaced
by the [`ALTACT_SIG_SEND`](`e:erts:erl_dist_protocol.md#ALTACT_SIG_SEND`)
control message. Support for the `ALTACT_SIG_SEND` control message is indicated
by the [`DFLAG_ALTACT_SIG`](`e:erts:erl_dist_protocol.md#DFLAG_ALTACT_SIG`)
distribution flag.
