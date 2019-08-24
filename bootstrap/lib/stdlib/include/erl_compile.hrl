%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

