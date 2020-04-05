%% =====================================================================
%% Header file for EDoc doclet modules.
%%
%% Copyright (C) 2001-2004 Richard Carlsson
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
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
%% Author contact: carlsson.richard@gmail.com
%% =====================================================================

-define(NO_APP, []).

%% Context for doclets

%% @type edoc_context() = #context{dir = string(),
%%                                 env = edoc_lib:edoc_env(),
%%                                 opts = [term()]}

-record(context, {dir = "",
		  env,
		  opts = []}).

%% Doclet commands

%% @type no_app().
%%    A value used to mark absence of an Erlang application
%%    context. Use the macro `NO_APP' defined in
%%    <a href="edoc_doclet.hrl">`edoc_doclet.hrl'</a>
%%    to produce this value.

%% @type doclet_gen() = #doclet_gen{sources = [string()],
%%                                  app = no_app() | atom(),
%%                                  modules = [atom()]}

-record(doclet_gen, {sources = [],
		     app = ?NO_APP,
		     modules = []
		    }).

%% @type doclet_toc() = #doclet_gen{paths = [string()],
%%                                  indir = string()}

-record(doclet_toc, {paths,
		     indir
		    }).
