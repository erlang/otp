%% =====================================================================
%% Header file for EDoc Type Representations
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2001-2005 Richard Carlsson
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

%% Type specification data structures

-record(t_spec, {name, type, defs=[]}).		% function specification

-record(t_typedef, {name, args, type,
		    defs=[]}).			% type declaration/definition

-record(t_throws, {type, defs=[]}).		% exception declaration

-record(t_def, {name, type}).			% local definition 'name = type'

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

-record(t_var, {a=[], name=[]}).	% type variable

-record(t_type, {a=[],                     % abstract type 'name(...)'
                 name,
                 args = []}).

-record(t_union, {a=[], types = []}).	% union type 't1|...|tN'

-record(t_fun, {a=[], args, range}).	% function '(t1,...,tN) -> range'

-record(t_tuple, {a=[], types = []}).	% tuple type '{t1,...,tN}'

-record(t_list, {a=[], type}).		% list type '[type]'

-record(t_nil, {a=[]}).			% empty-list constant '[]'

-record(t_nonempty_list, {a=[], type}).	% list type '[type, ...]'

-record(t_atom, {a=[], val}).		% atom constant

-record(t_integer, {a=[], val}).	% integer constant

-record(t_integer_range, {a=[], from, to}).

-record(t_binary, {a=[], base_size = 0, unit_size = 0}).

-record(t_float, {a=[], val}).		% floating-point constant

-record(t_record, {a=[],                 % record "type" '#r{f1,...,fN}'
                   name,
                   fields = []}).

-record(t_field, {a=[], name, type}).	% named field 'n1=t1'

-record(t_paren, {a=[], type}).		% parentheses

-record(t_map, {a=[], types=[]}).

-record(t_map_field, {a=[], assoc_type, k_type, v_type}).

