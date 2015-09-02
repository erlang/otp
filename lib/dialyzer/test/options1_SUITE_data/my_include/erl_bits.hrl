%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Original Code is Erlang-4.7.3, December, 1998.
%%
%% The Initial Developer of the Original Code is Ericsson Telecom
%% AB. Portions created by Ericsson are Copyright (C), 1998, Ericsson
%% Telecom AB. All Rights Reserved.
%%
%% Contributor(s): ______________________________________.''
%%
%% This is an -*- erlang -*- file.
%% Generic compiler options, passed from the erl_compile module.

-record(bittype, {
          type,       %% integer/float/binary
	  unit,       %% element unit
          sign,       %% signed/unsigned
          endian      %% big/little
         }).

-record(bitdefault, {
	  integer,    %% default type for integer
	  float,      %% default type for float
	  binary      %% default type for binary
	 }).

%%% (From config.hrl in the bitsyntax branch.)
-define(SYS_ENDIAN, big).
-define(SIZEOF_CHAR, 1).
-define(SIZEOF_DOUBLE, 8).
-define(SIZEOF_FLOAT, 4).
-define(SIZEOF_INT, 4).
-define(SIZEOF_LONG, 4).
-define(SIZEOF_LONG_LONG, 8).
-define(SIZEOF_SHORT, 2).
