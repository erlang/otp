%% -*- erlang-indent-level: 4 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%% Purpose: Exposes type definitions used also in other parts of
%%	    the system (e.g. in the translation from Beam to Icode).

%%
%% XXX: THE FOLLOWING TYPE DECLARATION DOES NOT BELONG HERE.
%%      IT SHOULD BE MOVED TO A FILE THAT DEFINES (AND EXPORTS)
%%      PROPER TYPES FOR THE SET OF BEAM INSTRUCTIONS.
%%
-type beam_instr() :: 'bs_init_writable' | 'build_stacktrace'
                    | 'fclearerror' | 'if_end' | 'raw_raise'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple().  %% XXX: Very underspecified - FIX THIS

%%-----------------------------------------------------------------------
%% Record definitions
%%-----------------------------------------------------------------------

-record(function, {name      :: atom(),
		   arity     :: byte(),
		   entry     :: beam_lib:label(),    %% unnecessary ?
		   code = [] :: [beam_instr()]}).

-record(beam_file, {module               :: module(),
		    labeled_exports = [] :: [beam_lib:labeled_entry()],
		    attributes      = [] :: [beam_lib:attrib_entry()],
		    compile_info    = [] :: [beam_lib:compinfo_entry()],
		    code            = [] :: [#function{}]}).
