<!--
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2025. All Rights Reserved.
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

# Erlang RYU

To work with the Erlang VM, Ryu has been changed in three important ways. These
changes have been marked with a `//CHANGE_FOR_ERLANG` comment explaining them in
the code.

1. We only kept the bare minimum files needed to generate a double to string with the shortest algorithm, with the widest lookup table
2. We deleted the code producing the final string, this is handled using a modified version of to_chars from the MS STL. <https://github.com/microsoft/STL/blob/8f4c8163775d665d80642044ce27c4bc696127ce/stl/inc/xcharconv_ryu.h#L1302>
3. All other unneeded code has been deleted

This is build with our own makefile.

Some of the more minor difference:

- the Zero case in common.h is changed to correspond to erlang fixed point version
- the MS STL pointer check are not here. Erlang generate a 256 bytes buffer, we only need 30 maximum. Beware what this mean when refactoring.

# How to update the Ryu version used by Erlang

To update run the `update.sh` script in this folder. It uses
<https://github.com/erlang/ryu> as a base and tries to merge
<https://github.com/ulfjack/ryu> into it. If there are no merge conflicts it then
checks that the STL file we use had changed. If it has changed you need to manually
check if there are any changes in the `__to_chars` function and if so see if they
need to be uplifted.

The update script will also bump the versions in vendor.info so that the SBOM
contains the latest versions.