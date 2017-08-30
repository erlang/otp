%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
