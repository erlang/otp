%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2011. All Rights Reserved.
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

-ifndef(snmpc_lib).
-define(snmpc_lib, true).

-define(vwarning(F, A),
	case get(warnings_as_errors) of
	    true -> snmpc_lib:error(F, A);
	    _ -> ?verbosity(warning, F, A, ignore)
	end).

-define(vwarning2(F, A, MibLine),
	 case get(warnings_as_errors) of
	     true -> snmpc_lib:error(F, A, MibLine);
	     _ -> ?verbosity(warning, F, A, MibLine)
	 end).
-define(vinfo(F, A),              ?verbosity(info,    F, A, ignore)).
-define(vinfo2(F, A, MibLine),    ?verbosity(info,    F, A, MibLine)).
-define(vlog(F, A),               ?verbosity(log,     F, A, ignore)).
-define(vlog2(F, A, MibLine),     ?verbosity(log,     F, A, MibLine)).
-define(vdebug(F, A),             ?verbosity(debug,   F, A, ignore)).
-define(vdebug2(F, A, MibLine),   ?verbosity(debug,   F, A, MibLine)).
-define(vtrace(F, A),             ?verbosity(trace,   F, A, ignore)).
-define(vtrace2(F, A, MibLine),   ?verbosity(trace,   F, A, MibLine)).

-define(verbosity(Severity, F, A, MibLine), 
	snmpc_lib:vprint(Severity, ?MODULE, ?LINE, MibLine, F, A)).

-endif. % -ifndef(snmpc_lib).
