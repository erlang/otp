%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% This is an -*- erlang -*- file.
%% Generic compiler options, passed from the erl_compile module.

-type bt_endian():: 'big' | 'little' | 'native'.
-type bt_sign()  :: 'signed' | 'unsigned'.
-type bt_type()  :: 'integer' | 'float' | 'binary' | 'utf8' | 'utf16' | 'utf32'.
-type bt_unit()  :: 1..256.

-record(bittype, {
          type   :: bt_type(),
	  unit   :: bt_unit(),       %% element unit
          sign   :: bt_sign(),
          endian :: bt_endian()
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
