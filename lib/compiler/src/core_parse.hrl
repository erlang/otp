%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%% Purpose : Core Erlang syntax trees as records.

%% It would be nice to incorporate some generic functions as well but
%% this could make including this file difficult.

%% Note: the annotation list is *always* the first record field.
%% Thus it is possible to define the macros:
%% -define(get_ann(X), element(2, X)).
%% -define(set_ann(X, Y), setelement(2, X, Y)).

%% The record definitions appear alphabetically

-record(c_alias, {anno=[], var,		% var :: Tree,
		  pat}).		% pat :: Tree

-record(c_apply, {anno=[], op,		% op :: Tree,
		  args}).		% args :: [Tree]

-record(c_binary, {anno=[], segments}).	% segments :: [#c_bitstr{}]

-record(c_bitstr, {anno=[], val,        % val :: Tree,
		   size,		% size :: Tree,
		   unit,		% unit :: Tree,
		   type,		% type :: Tree,
		   flags}).		% flags :: Tree

-record(c_call, {anno=[], module,	% module :: Tree,
		 name,			% name :: Tree,
		 args}).		% args :: [Tree]

-record(c_case, {anno=[], arg,		% arg :: Tree,
		 clauses}).		% clauses :: [Tree]

-record(c_catch, {anno=[], body}).	% body :: Tree

-record(c_clause, {anno=[], pats,	% pats :: [Tree],
		   guard,		% guard :: Tree,
		   body}).		% body :: Tree

-record(c_cons, {anno=[], hd,		% hd :: Tree,
		 tl}).			% tl :: Tree

-record(c_fun, {anno=[], vars,		% vars :: [Tree],
		body}).			% body :: Tree

-record(c_let, {anno=[], vars,		% vars :: [Tree],
		arg,			% arg :: Tree,
		body}).			% body :: Tree

-record(c_letrec, {anno=[], defs,	% defs :: [#c_def{}],
		   body}).		% body :: Tree

-record(c_literal, {anno=[], val}).	% val :: literal()

-record(c_module, {anno=[], name,	% name :: Tree,
		   exports,		% exports :: [Tree],
		   attrs,		% attrs :: [#c_def{}],
		   defs}).		% defs :: [#c_def{}]

-record(c_primop, {anno=[], name,	% name :: Tree,
		   args}).		% args :: [Tree]

-record(c_receive, {anno=[], clauses,	% clauses :: [Tree],
		    timeout,		% timeout :: Tree,
		    action}).		% action :: Tree

-record(c_seq, {anno=[], arg,		% arg :: Tree,
		body}).			% body :: Tree

-record(c_try, {anno=[], arg,		% arg :: Tree,
		vars,			% vars :: [Tree],
		body,			% body :: Tree
		evars,			% evars :: [Tree],
		handler}).		% handler :: Tree

-record(c_tuple, {anno=[], es}).	% es :: [Tree]

-record(c_values, {anno=[], es}).	% es :: [Tree]

-record(c_var, {anno=[], name :: cerl:var_name()}).
