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
### Erlang Distribution Without Large Node Container Support

Communication over the Erlang distribution without support for large
[node container data types (version 4)](`e:erts:erl_dist_protocol.md#DFLAG_V4_NC`)
is as of OTP 24 deprecated and is scheduled for removal in OTP 26. That is, as
of OTP 26, support for large node container data types will become mandatory.

### Old Link Protocol

The old link protocol used when communicating over the Erlang distribution is as
of OTP 24 deprecated and support for it is scheduled for removal in OTP 26. As
of OTP 26, the
[new link protocol](`e:erts:erl_dist_protocol.md#new_link_protocol`) will become
mandatory. That is, Erlang nodes will then refuse to connect to nodes not
implementing the new link protocol. If you implement the Erlang distribution
yourself, you are, however, encouraged to implement the new link protocol as
soon as possible since the old protocol can cause links to enter an inconsistent
state.

### ?NO_APP macro

The ?NO_APP macro in the edoc include file `edoc_doclet.hrl` has been
deprecated.

### Functions Deprecated in OTP 24

-   `erlang:phash/2` (use erlang:phash2/2 instead)
-   `zlib:adler32/2` (use erlang:adler32/1 instead)
-   `zlib:adler32/3` (use erlang:adler32/2 instead)
-   `zlib:adler32_combine/4` (use erlang:adler_combine/3 instead)
-   `zlib:crc32/1` (use erlang:crc32/1 on the uncompressed data instead)
-   `zlib:crc32/2` (use erlang:crc32/1 instead)
-   `zlib:crc32/3` (use erlang:crc32/2 instead)
-   `zlib:crc32_combine/4` (use erlang:crc32_combine/3 instead)
-   `zlib:getBufSize/1` (this function will be removed in a future release)
-   `zlib:inflateChunk/1` (use safeInflate/2 instead)
-   `zlib:inflateChunk/2` (use safeInflate/2 instead)
-   `zlib:setBufSize/2` (this function will be removed in a future release)
