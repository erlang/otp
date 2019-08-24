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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: erl_compile.hrl,v 1.1 2008/12/17 09:53:40 mikpe Exp $
%%

%% Generic compiler options, passed from the erl_compile module.

-record(options,
	 {includes=[],				% Include paths (list of absolute
						% directory names).
	  outdir=".",				% Directory for result (absolute
						% path).
	  output_type=undefined,		% Type of output file (atom).
	  defines=[],				% Preprocessor defines.  Each
						% element is an atom (the name to
						% define), or a {Name, Value}
						% tuple.
	  warning=1,				% Warning level (0 - no
						% warnings, 1 - standard level,
						% 2, 3, ... - more warnings).
	  verbose=false,			% Verbose (true/false).
	  optimize=999,				% Optimize options.
	  specific=[],				% Compiler specific options.
	  outfile="",				% Name of output file (internal
						% use in erl_compile.erl).
	  cwd					% Current working directory
						% for erlc.
	 }).
