%% -*- erlang-indent-level: 4 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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
%% Purpose: Exposes type definitions used also in other parts of
%%	    the system (e.g. in the translation from Beam to Icode).

%%
%% XXX: THE FOLLOWING TYPE DECLARATION DOES NOT BELONG HERE...
%%
-type beam_instr() :: 'bs_init_writable' | 'fclearerror' | 'if_end'
                    | 'remove_message' | 'return' | 'send' | 'timeout'
                    | tuple().  %% XXX: Very underspecified - FIX THIS

%%-----------------------------------------------------------------------
%% Record definitions
%%-----------------------------------------------------------------------

-record(function, {name      :: atom(),
		   arity     :: byte(),
		   entry,    %% unused ??
		   code = [] :: [beam_instr()]}).

-record(beam_file, {module               :: module(),
		    labeled_exports = [] :: [beam_lib:labeled_entry()],
		    attributes      = [] :: [beam_lib:attrib_entry()],
		    compile_info    = [] :: [beam_lib:compinfo_entry()],
		    code            = [] :: [#function{}]}).
