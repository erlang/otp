%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2010. All Rights Reserved.
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

-define(EPMD_ALIVE2_REQ, $x).
-define(EPMD_PORT_PLEASE2_REQ, $z).
-define(EPMD_ALIVE2_RESP, $y).
-define(EPMD_PORT2_RESP, $w).
-define(EPMD_NAMES, $n).

%% Commands used only by interactive client
-define(EPMD_DUMP, $d).
-define(EPMD_KILL, $k).
-define(EPMD_STOP, $s).
