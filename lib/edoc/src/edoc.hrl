%% =====================================================================
%% Header file for EDoc
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2001-2004 Richard Carlsson
%% Copyright Ericsson AB 2009-2025. All Rights Reserved.
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
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% %CopyrightEnd%
%%
%% Author contact: carlsson.richard@gmail.com
%% =====================================================================

%% Note: Documentation in this file is included by edoc_extract.erl

-define(APPLICATION, edoc).
-define(INFO_FILE, "edoc-info").
-define(OVERVIEW_FILE, "overview.edoc").
-define(DEFAULT_SOURCE_SUFFIX, ".erl").
-define(DEFAULT_FILE_SUFFIX, ".html").
-define(DEFAULT_DOCLET, edoc_doclet).
-define(DEFAULT_LAYOUT, edoc_layout).
-define(APP_DEFAULT, "http://www.erlang.org/edoc/doc").
-define(CURRENT_DIR, ".").
-define(SOURCE_DIR, "src").
-define(EBIN_DIR, "ebin").
-define(EDOC_DIR, "doc").
-define(REPORT_MISSING_TYPES, false).

-include("edoc_doclet.hrl").

-record(module, {name = [],
		 parameters = none,
		 functions = [],
		 exports = [],
		 attributes = [],
		 records = [],
		 encoding = latin1,
		 file}).

-record(env, {module = [],
	      root = "",
	      file_suffix,
	      apps,
	      modules,
	      app_default,
	      macros = [],
	      includes = []}).

-record(comment, {line = 0, text}).

-record(entry, {name, args = [], line = 0, export, data}).

-record(tag, {name, line = 0, origin = comment, data, form}).
