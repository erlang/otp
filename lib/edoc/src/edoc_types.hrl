%% =====================================================================
%% Header file for EDoc Type Representations
%%
%% Copyright (C) 2001-2005 Richard Carlsson
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

%% Type specification data structures

%% @type t_spec() = #t_spec{name = t_name(),
%%                          type = t_type(),
%%                          defs = [t_def()]}

-record(t_spec, {name, type, defs=[]}).		% function specification

%% @type type() = t_atom() | t_binary() | t_float() | t_fun() | t_integer()
%%              | t_integer_range() | t_list() | t_nil()| t_nonempty_list()
%%              | t_record() | t_tuple() | t_type() | t_union() | t_var()
%%              | t_paren()

%% @type t_typedef() = #t_typedef{name = t_name(),
%%                                args = [type()],
%%                                type = type() | undefined,
%%                                defs = [t_def()]}.

-record(t_typedef, {name, args, type,
		    defs=[]}).			% type declaration/definition

%% @type t_throws() = #t_throws{type = type(),
%%                              defs = [t_def()]}

-record(t_throws, {type, defs=[]}).		% exception declaration

%% @type t_def() = #t_def{name = t_type() | t_var(),
%%                        type = type()}

-record(t_def, {name, type}).			% local definition 'name = type'
%% @type t_name() = #t_name{app = [] | atom(),
%%                          module = [] | atom(),
%%                          name = [] | atom()}

-record(t_name, {app = [],			% app = [] if module = []
		 module=[],			% unqualified if module = []
		 name=[]}).

%% The following records all have 'a=[]' as their first field.
%% This is used for name and usage annotations; in particular, the
%% fun-argument types of a function specification (t_spec) are often
%% annotated with the names of the corresponding formal parameters,
%% and/or usage summaries.

-define(t_ann(X), element(2, X)).
-define(set_t_ann(X, Y), setelement(2, X, Y)).
-define(add_t_ann(X, Y), ?set_t_ann(X, [Y | ?t_ann(X)])).

%% @type t_var() = #t_var{a = list(), name = [] | atom()}

-record(t_var, {a=[], name=[]}).	% type variable

%% @type t_type() = #t_type{a = list(),
%%                          name = t_name(),
%%                          args = [type()]}

-record(t_type, {a=[],                     % abstract type 'name(...)'
                 name,
                 args = []}).

%% @type t_union() = #t_union{a = list(),
%%                            types = [type()]}

-record(t_union, {a=[], types = []}).	% union type 't1|...|tN'

%% @type t_fun() = #t_fun{a = list(),
%%                        args = [type()],
%%                        range = type()}

-record(t_fun, {a=[], args, range}).	% function '(t1,...,tN) -> range'

%% @type t_tuple() = #t_tuple{a = list(),
%%                            types = [type()]}

-record(t_tuple, {a=[], types = []}).	% tuple type '{t1,...,tN}'

%% @type t_list() = #t_list{a = list(),
%%                          type = type()}

-record(t_list, {a=[], type}).		% list type '[type]'

%% @type t_nil() = #t_nil{a = list()}

-record(t_nil, {a=[]}).			% empty-list constant '[]'

%% @type t_nonempty_list() = #t_nonempty_list{a = list(),
%%                                            type = type()}

-record(t_nonempty_list, {a=[], type}).	% list type '[type, ...]'

%% @type t_atom() = #t_atom{a = list(),
%%                          val = atom()}

-record(t_atom, {a=[], val}).		% atom constant

%% @type t_integer() = #t_integer{a = list(),
%%                                val = integer()}

-record(t_integer, {a=[], val}).	% integer constant

%% @type t_integer_range() = #t_integer_range{a = list(),
%%                                            from = integer(),
%%                                            to = integer()}

-record(t_integer_range, {a=[], from, to}).

%% @type t_binary() = #t_binary{a = list(),
%%                              base_size = integer(),
%%                              unit_size = integer()}

-record(t_binary, {a=[], base_size = 0, unit_size = 0}).

%% @type t_float() = #t_float{a = list(),
%%                            val = float()}

-record(t_float, {a=[], val}).		% floating-point constant

%% @type t_record() = #t_list{a = list(),
%%                            name = t_atom(),
%%                            fields = [field()]}

-record(t_record, {a=[],                 % record "type" '#r{f1,...,fN}'
                   name,
                   fields = []}).

%% @type t_field() = #t_field{a = list(),
%%                            name = type(),
%%                            type = type()}

-record(t_field, {a=[], name, type}).	% named field 'n1=t1'

%% @type t_paren() = #t_paren{a = list(), type = type()}

-record(t_paren, {a=[], type}).		% parentheses

-record(t_map, {a=[], types=[]}).
-record(t_map_field, {a=[], assoc_type, k_type, v_type}).

