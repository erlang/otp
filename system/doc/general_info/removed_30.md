<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
### erlang:fun_info(Fun, pid)

`erlang:fun_info(Fun, pid)` will now raise an exception.

### Old aliases for type tests in guards

The type test aliases `atom/1`, `binary/1`, `float/1`, `function/1`,
`integer/1`, `list/1`, `number/1`, `pid/1`, `port/1`, `record/2`,
`reference/1`, and `tuple/1`, for the respective tests `is_atom/1`,
`is_binary/1`, `is_float/1`, ..., `is_tuple/2`, have been deprecated
since before OTP R13 and was removed in OTP 30.
