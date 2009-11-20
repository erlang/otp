%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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

-define(SHOW, show).
-define(SHOW_EXPORTED, show_exported).
-define(ANNOTATE, annotate).
-define(ANNOTATE_INC_FILES, annotate_inc_files).

-type mode() :: ?SHOW | ?SHOW_EXPORTED | ?ANNOTATE | ?ANNOTATE_INC_FILES.

-record(typer_analysis,
	{mode					:: mode(),
	 macros      = []			:: [{atom(), _}], % {macro_name, value}
	 includes    = []			:: [string()],
	 
	 %% Esp for Dialyzer
	 %% ----------------------
	 code_server = dialyzer_codeserver:new():: dialyzer_codeserver:codeserver(),
	 callgraph   = dialyzer_callgraph:new() :: dialyzer_callgraph:callgraph(),
	 ana_files   = []			:: [string()],   % absolute filenames
	 plt         = none			:: 'none' | string(),
	 
	 %% Esp for TypEr
	 %% ----------------------
	 t_files     = []			:: [string()], 
	 
	 %% For choosing between contracts or comments
	 contracts   = true			:: boolean(),
	 
	 %% Any file in 'final_files' is compilable.
	 %% And we need to keep it as {FileName,ModuleName}
	 %% in case filename does NOT match with moduleName
	 final_files = []			:: [{string(), atom()}],  
	 
	 ex_func     = typer_map:new()		:: dict(),
	 record      = typer_map:new()		:: dict(),
	 
	 %% Functions: the line number of the function 
	 %%            should be kept as well
	 func        = typer_map:new()		:: dict(),
	 inc_func    = typer_map:new()		:: dict(),
	 trust_plt   = dialyzer_plt:new()	:: dialyzer_plt:plt()}).

-record(args,
	{analyze = []        :: [string()],
	 analyzed_dir_r = [] :: [string()],
	 trust = []          :: [string()]}).
