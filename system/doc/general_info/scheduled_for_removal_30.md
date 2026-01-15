<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
-->

### Old aliases for type tests in guards

The type test aliases `atom/1`, `binary/1`, `float/1`, `function/1`,
`integer/1`, `list/1`, `number/1`, `pid/1`, `port/1`, `record/2`,
`reference/1`, and `tuple/1`, for the respective tests `is_atom/1`,
`is_binary/1`, `is_float/1`, ..., `is_tuple/2`, have been deprecated since
before OTP R13 and have been scheduled for removal in OTP 30. Note that
these aliases could only be used as guard tests at the top level. For
example, in `{X, Y} when float(X), X =:= float(Y) -> ...` the first use of
`float/1` is a type test alias, but the second use is the normal
integer-to-float conversion BIF, because it occurs inside the `=:=`. The
aliases were never recognized outside guards.



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
