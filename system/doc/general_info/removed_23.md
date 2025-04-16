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
### VxWorks Support

Some parts of OTP has had limited VxWorks support, such as
[`erl_interface`](`e:erl_interface:index.html`). This support was removed in
OTP 23. This limited support was formally deprecated as of OTP 22.

### Legacy parts of erl_interface

The old legacy [`erl_interface`](`e:erl_interface:index.html`) library
(functions with prefix `erl_`) was removed in OTP 23. These parts of
`erl_interface` has been informally deprecated for a very long time, and was
formally deprecated in OTP 22. You typically want to replace the usage of the
`erl_interface` library with the use of the `ei` library which also is part of
the `erl_interface` application.

### httpd_conf module

API functions in the module called `httpd_conf` was deprecated in favor of
standard modules such as `lists`, `string`, `filelib`, and `erlang`. Formally
deprecated as of OTP 18.

### inets - httpd Apache config files

Support for the Apache-compatible config files was removed in OTP 23. A new
config file format was introduced in OTP 12.

### SSL/TLS

For security reasons SSL-3.0 is no longer supported at all.
