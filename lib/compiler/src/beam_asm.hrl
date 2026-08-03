%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2021-2025. All Rights Reserved.
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
%%

-include("beam_types.hrl").

-type reg_num() :: 0 .. 1023.

-type xreg() :: {'x', reg_num()}.
-type yreg() :: {'y', reg_num()}.
-type freg() :: {'fr', reg_num()}.
-type zreg() :: {'z', reg_num()}.

-type beam_reg() :: xreg() | yreg() | freg().

-type beam_literal() :: {atom, [] | atom()} |
                        {float, [] | float()} |
                        {integer, [] | integer()} |
                        {literal, term()} |
                        nil.

%% Type-tagged beam register. Assembly passes that care about registers (e.g.
%% beam_trim) must be prepared to handle these whenever they inspect a
%% register.
%%
%% To aid in the above, the validator will explode upon encountering them in an
%% unfamiliar context.
-record(tr, {r :: beam_reg(), t :: type()}).
