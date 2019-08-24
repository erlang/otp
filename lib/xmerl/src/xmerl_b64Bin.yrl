%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

%% The base64Binary grammar is as follows:
%% Base64Binary  ::=  ((B64S B64S B64S B64S)*
%%                      ((B64S B64S B64S B64) |
%%                       (B64S B64S B16S '=') |
%%                       (B64S B04S '=' #x20? '=')))?
%%
%% B64S         ::= B64 #x20?
%%
%% B16S         ::= B16 #x20?
%%
%% B04S         ::= B04 #x20?
%%
%% B04         ::=  [AQgw]
%% (B16         ::=  [AEIMQUYcgkosw048])
%% (B64         ::=  [A-Za-z0-9+/])

%% Changes
%% B16x       ::=  [EIMUYckos048]
%% B64x       ::=  [B-DF-HJ-LN-PR-TV-XZabd-fh-jl-np-rt-vx-z1-35-79+/]

%% B16         ::=  B04 | B16x
%% B64         ::=  B04 | B16x | B64x
%% Handle whitespace in scanner

Nonterminals base64Binary base64Binary2 b64 b16.

Terminals '=' b04 b16x b64x.

Rootsymbol base64Binary.

Endsymbol '$end'.




base64Binary -> base64Binary2.
base64Binary -> '$empty'.

base64Binary2 -> b64 b64 b64 b64 base64Binary2.
base64Binary2 -> b64 b64 b64 b64.
base64Binary2 -> b64 b64 b16 '='.
base64Binary2 -> b64 b04 '=' '='.

b64          -> b04.
b64          -> b16x.
b64          -> b64x.

b16          -> b04.
b16          -> b16x.
