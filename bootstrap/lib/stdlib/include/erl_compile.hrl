%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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

%% Generic compiler options, passed from the erl_compile module.

-record(options,
	 {includes=[] :: [file:filename()],	% Include paths (list of
						% absolute directory names).
	  outdir="."  :: file:filename(),	% Directory for result
						% (absolute path).
	  output_type=undefined :: atom(),	% Type of output file.
	  defines=[]  :: [atom() | {atom(),_}],	% Preprocessor defines.  Each
						% element is an atom
						% (the name to define), or 
						% a {Name, Value} tuple.
	  warning=1   :: non_neg_integer(),	% Warning level (0 - no
						% warnings, 1 - standard level,
						% 2, 3, ... - more warnings).
	  verbose=false :: boolean(),		% Verbose (true/false).
	  optimize=999,				% Optimize options.
	  specific=[] :: [_],			% Compiler specific options.
	  outfile=""  :: file:filename(),	% Name of output file (internal
						% use in erl_compile.erl).
	  cwd	      :: file:filename()	% Current working directory
						% for erlc.
	 }).

