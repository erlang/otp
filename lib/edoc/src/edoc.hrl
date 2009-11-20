%% =====================================================================
%% Header file for EDoc
%% 
%% Copyright (C) 2001-2004 Richard Carlsson
%%
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% Author contact: richardc@it.uu.se
%% =====================================================================

%% Note: Documentation in this file is included by edoc_extract.erl

-define(APPLICATION, edoc).
-define(INFO_FILE, "edoc-info").
-define(PACKAGE_FILE, "package.edoc").
-define(OVERVIEW_FILE, "overview.edoc").
-define(PACKAGE_SUMMARY, "package-summary").
-define(DEFAULT_SOURCE_SUFFIX, ".erl").
-define(DEFAULT_FILE_SUFFIX, ".html").
-define(DEFAULT_DOCLET, edoc_doclet).
-define(DEFAULT_LAYOUT, edoc_layout).
-define(APP_DEFAULT, "http://www.erlang.org/edoc/doc").
-define(CURRENT_DIR, ".").
-define(SOURCE_DIR, "src").
-define(EBIN_DIR, "ebin").
-define(EDOC_DIR, "doc").

-include("edoc_doclet.hrl").

%% Module information

%% @type module() = #module{name = [] | atom(),
%%                          parameters = none | [atom()],
%%                          functions = ordset(function_name()),
%%                          exports = ordset(function_name()),
%%                          attributes = ordset({atom(), term()}),
%%                          records = [{atom(), [{atom(), term()}]}]}
%%  ordset(T) = sets:ordset(T)
%%  function_name(T) = {atom(), integer()}

-record(module, {name = [],
		 parameters = none,
		 functions = [],
		 exports = [],
		 attributes = [],
		 records = []
		}).

%% Environment for generating documentation data

-record(env, {module = [],
	      package = [],
	      root = "",
	      file_suffix,
	      package_summary,
	      apps,
	      modules,
	      packages,
	      app_default,
	      macros = [],
	      includes = []
	     }).

%% Simplified comment data

%% @type comment() = #comment{line = integer(),
%%                            text = string()}

-record(comment, {line = 0, text}).

%% Module Entries (one per function, plus module header and footer)

%% @type entry() = #entry{name = atom(),
%%                        args = [string()],
%%                        line = integer(),
%%                        export = bool(),
%%                        data = term()}

-record(entry, {name, args = [], line = 0, export, data}).

%% Generic tag information

%% @type tag() = #tag{name = atom(),
%%                    line = integer(),
%%                    data = term()}

-record(tag, {name, line = 0, data}).
