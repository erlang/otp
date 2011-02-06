%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2011. All Rights Reserved.
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
	 macros      = []			:: [{atom(), term()}], % {macro_name, value}
	 includes    = []			:: [file:filename()],
	 %% --- for dialyzer ---
	 code_server = dialyzer_codeserver:new():: dialyzer_codeserver:codeserver(),
	 callgraph   = dialyzer_callgraph:new() :: dialyzer_callgraph:callgraph(),
	 ana_files   = []			:: [file:filename()],   % absolute filenames
	 plt         = none			:: 'none' | file:filename(),
	 no_spec     = false                    :: boolean(),
	 %% --- for typer ---
	 t_files     = []			:: [file:filename()], 
	 %% For choosing between contracts or comments
	 contracts   = true			:: boolean(),
	 %% Files in 'final_files' are compilable with option 'to_pp'; we keep
	 %% them as {FileName, ModuleName} in case the ModuleName is different
	 final_files = []			:: [{file:filename(), module()}],
	 ex_func     = typer_map:new()		:: dict(),
	 record      = typer_map:new()		:: dict(),
	 func        = typer_map:new()		:: dict(),
	 inc_func    = typer_map:new()		:: dict(),
	 trust_plt   = dialyzer_plt:new()	:: dialyzer_plt:plt()}).

-record(args, {files   = [] :: [file:filename()],
	       files_r = [] :: [file:filename()],
	       trusted = [] :: [file:filename()]}).
