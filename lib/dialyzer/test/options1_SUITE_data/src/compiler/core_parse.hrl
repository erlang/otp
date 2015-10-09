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
%%     $Id: core_parse.hrl,v 1.1 2008/12/17 09:53:42 mikpe Exp $
%%
%% Purpose : Core Erlang syntax trees as records.

%% It would be nice to incorporate some generic functions as well but
%% this could make including this file difficult.

%% Note: the annotation list is *always* the first record field.
%% Thus it is possible to define the macros:
%% -define(get_ann(X), element(2, X)).
%% -define(set_ann(X, Y), setelement(2, X, Y)).

-record(c_int, {anno=[], val}).		% val :: integer()

-record(c_float, {anno=[], val}).	% val :: float()

-record(c_atom, {anno=[], val}).	% val :: atom()

-record(c_char, {anno=[], val}).	% val :: char()

-record(c_string, {anno=[], val}).	% val :: string()

-record(c_nil, {anno=[]}).

-record(c_binary, {anno=[], segments}).	% segments :: [#ce_bitstr{}]

-record(c_bitstr, {anno=[],val,	% val :: Tree,
		   size,		% size :: Tree,
		   unit,		% unit :: integer(),
		   type,		% type :: atom(),
		   flags}).		% flags :: [atom()],

-record(c_cons, {anno=[], hd,		% hd :: Tree,
		 tl}).			% tl :: Tree

-record(c_tuple, {anno=[], es}).	% es :: [Tree]

-record(c_var, {anno=[], name}).	% name :: integer() | atom()

-record(c_fname, {anno=[], id,		% id :: atom(),
		  arity}).		% arity :: integer()

-record(c_values, {anno=[], es}).	% es :: [Tree]

-record(c_fun, {anno=[], vars,		% vars :: [Tree],
		body}).			% body :: Tree

-record(c_seq, {anno=[], arg,		% arg :: Tree,
		body}).			% body :: Tree

-record(c_let, {anno=[], vars,		% vars :: [Tree],
		arg,			% arg :: Tree,
		body}).			% body :: Tree

-record(c_letrec, {anno=[], defs,	% defs :: [#ce_def{}],
		   body}).		% body :: Tree

-record(c_def, {anno=[], name,		% name :: Tree,
		val}).			% val :: Tree,

-record(c_case, {anno=[], arg,		% arg :: Tree,
		 clauses}).		% clauses :: [Tree]

-record(c_clause, {anno=[], pats,	% pats :: [Tree],
		   guard,		% guard :: Tree,
		   body}).		% body :: Tree

-record(c_alias, {anno=[], var,		% var :: Tree,
		  pat}).		% pat :: Tree

-record(c_receive, {anno=[], clauses,	% clauses :: [Tree],
		    timeout,		% timeout :: Tree,
		    action}).		% action :: Tree

-record(c_apply, {anno=[], op,		% op :: Tree,
		  args}).		% args :: [Tree]

-record(c_call, {anno=[], module,	% module :: Tree,
		 name,			% name :: Tree,
		 args}).		% args :: [Tree]

-record(c_primop, {anno=[], name,	% name :: Tree,
		   args}).		% args :: [Tree]

-record(c_try, {anno=[], arg,		% arg :: Tree,
		vars,			% vars :: [Tree],
		body,			% body :: Tree
		evars,			% evars :: [Tree],
		handler}).		% handler :: Tree

-record(c_catch, {anno=[], body}).	% body :: Tree

-record(c_module, {anno=[], name,	% name :: Tree,
		   exports,		% exports :: [Tree],
		   attrs,		% attrs :: [#ce_def{}],
		   defs}).		% defs :: [#ce_def{}]
