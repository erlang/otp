%% =====================================================================
%% Header file for EDoc doclet modules.
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
%%    <a href="../include/edoc_doclet.hrl">`edoc_doclet.hrl'</a>
%%    to produce this value.

%% @type doclet_gen() = #doclet_gen{sources = [string()],
%%                                  app = no_app() | atom(),
%%                                  packages = [atom()],
%%                                  modules = [atom()],
%%                                  modules = [atom()],
%%                                  filemap = function()}

-record(doclet_gen, {sources = [],
		     app = ?NO_APP,
		     packages = [],
		     modules = [],
		     filemap
		    }).

%% @type doclet_toc() = #doclet_gen{paths = [string()],
%%                                  indir = string()}

-record(doclet_toc, {paths,
		     indir
		    }).
