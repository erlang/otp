<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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
### erlang:now/0

New time functionality and a new time API was introduced. For more information
see the [Time and Time Correction](`e:erts:time_correction.md`) chapter in the
ERTS User's guide and specifically the
[Dos and Donts](`e:erts:time_correction.md#Dos_and_Donts`) section on how to
replace usage of `erlang:now/0`.

### httpd_conf module

API functions in the module `httpd_conf` was deprecated in favor of standard
modules such as `lists`, `string`, `filelib`, and `erlang`.
