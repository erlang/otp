%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2015. All Rights Reserved.
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

%%% This file is meant to be included by xref_* only.

-define(VAR_EXPR, '$F_EXPR').
-define(MOD_EXPR, '$M_EXPR').

-define(XREF_END_LINE, (1 bsl 23)).

%%% Filenames are stored as directory and basename. A lot of heap can
%%% be saved by keeping only one (or few) copy of the directory name.

%% 'data' in xref_mod holds "raw" data (as sets) for each module. The
%% data in 'variables' is derived from raw data.
-record(xref, {
    % version of the xref record
    version = 1,
    mode = functions,
    % table of variables
    variables = not_set_up,

    % dict-of(xref_mod())
    modules = dict:new(),
    % dict-of(xref_app())
    applications = dict:new(),
    % dict-of(xref_rel())
    releases = dict:new(),

    % [string()] | code_path
    library_path = [],
    % dict-of(xref_lib())
    libraries = dict:new(),

    % Default value of the 'builtins' option.
    builtins_default = false,
    % Default value of the 'recurse' option.
    recurse_default = false,
    % Default value of the 'verbose' option.
    verbose_default = false,
    % Default value of the 'warnings' option.
    warnings_default = true
}).

-record(xref_mod, {
    name = '',
    % [] or [AppName]
    app_name = [],
    % string(), directory where the BEAM file resides
    dir = "",
    % modification time for file
    mtime,
    % whether calls to built-in functions are included
    builtins,
    % number of exports, locals etc.
    info,
    % number of unresolved calls
    no_unresolved = 0,
    data
    %% Data has been read from the BEAM file, and is represented here
    %% as a list of sets.
    %% If xref.mode = functions:
    %% [
    %% DefAt,         M -> P(V * N)
    %% L,             M -> P(V)
    %% X,             M -> P(V)
    %% LCallAt,       M -> P(V * V -> P(N))
    %% XCallAt,       M -> P(V * V -> P(N))
    %% CallAt,        M -> P(V * V -> P(N))
    %% LC,            M -> P(V * V)
    %% XC,            M -> P(V * V)
    %% LU,            M -> P(V)
    %% EE,            M -> P(EV * EV)
    %% ECallAt,       M -> P(EV * EV -> P(N))
    %% Unres,         M -> P(V * V)
    %% LPredefined    M -> P(V)
    %% ]
    %%
    %% If xref.mode = modules:
    %% [
    %% X,             M -> P(V)
    %% I              M -> P(V)
    %% ]
}).

-record(xref_app, {
    name = '',
    % [] or [RelName]
    rel_name = [],
    vsn = [],
    % where BEAM files are read from
    dir = ""
}).

-record(xref_rel, {
    name = '',
    % where application directories reside
    dir = ""
}).

-record(xref_lib, {
    % atom(), module name
    name = '',
    % string(), directory where the file resides
    dir = ""
}).

-record(xref_var, {
    % atom(), variable name
    name = '',
    % set or pair of sets, variable value
    value,
    % VarType (predef, tmp, user)
    vtype,
    % ObjectType (vertex, edge, etc.)
    otype,
    % Type (function, module, etc.)
    type
}).
