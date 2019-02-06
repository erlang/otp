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
%% Purpose : Core Erlang syntax trees as records.

%% It would be nice to incorporate some generic functions as well but
%% this could make including this file difficult.

%% Note: the annotation list is *always* the first record field.
%% Thus it is possible to define the macros:
%% -define(get_ann(X), element(2, X)).
%% -define(set_ann(X, Y), setelement(2, X, Y)).

%% The record definitions appear alphabetically

-record(c_alias, {anno=[] :: cerl:anns(),
		  var     :: cerl:c_var(),
		  pat     :: cerl:cerl()}).

-record(c_apply, {anno=[] :: cerl:anns(),
		  op      :: cerl:c_var(),
		  args    :: [cerl:cerl()]}).

-record(c_binary, {anno=[]  :: cerl:anns(),
		   segments :: [cerl:c_bitstr()]}).

-record(c_bitstr, {anno=[], val,	% val :: Tree,
		   size,	       	% size :: Tree,
		   unit,	       	% unit :: Tree,
		   type,	       	% type :: Tree,
		   flags}).	       	% flags :: Tree

-record(c_call, {anno=[], module,	% module :: cerl:cerl(),
		 name,			% name :: cerl:cerl(),
		 args}).		% args :: [cerl:cerl()]

-record(c_case, {anno=[] :: cerl:anns(),
		 arg     :: cerl:cerl(),
		 clauses :: [cerl:cerl()]}).

-record(c_catch, {anno=[] :: cerl:anns(), body :: cerl:cerl()}).

-record(c_clause, {anno=[] :: cerl:anns(),
		   pats,    % :: [cerl:cerl()],	% pats :: [Tree],
		   guard,   % :: cerl:cerl(),		% guard :: Tree,
		   body}).  % :: cerl:cerl()}).		% body :: Tree

-record(c_cons, {anno=[] :: cerl:anns(),
		 hd      :: cerl:cerl(),
		 tl      :: cerl:cerl()}).

-record(c_fun, {anno=[] :: cerl:anns(),
		vars    :: [cerl:c_var()],
		body    :: cerl:cerl()}).

-record(c_let, {anno=[] :: cerl:anns(),
		vars    :: [cerl:c_var()],
		arg     :: cerl:cerl(),
		body    :: cerl:cerl()}).

-record(c_letrec, {anno=[] :: cerl:anns(),
		   defs    :: cerl:defs(),
		   body    :: cerl:cerl()}).

-record(c_literal, {anno=[] :: cerl:anns(), val :: cerl:litval()}).

-record(c_map, {anno=[] :: cerl:anns(),
		arg=#c_literal{val=#{}} :: cerl:c_var() | cerl:c_literal(),
		es :: [cerl:c_map_pair()],
		is_pat=false :: boolean()}).

-record(c_map_pair, {anno=[] :: cerl:anns(),
	             op, %:: #c_literal{val::'assoc'} | #c_literal{val::'exact'},
		     key,
		     val}).

-record(c_module, {anno=[] :: cerl:anns(),
		   name    :: cerl:c_literal(),
		   exports :: [cerl:c_var()],
		   attrs   :: cerl:attrs(),
		   defs    :: cerl:defs()}).

-record(c_primop, {anno=[] :: cerl:anns(),
		   name    :: cerl:c_literal(),
		   args    :: [cerl:cerl()]}).

-record(c_receive, {anno=[]:: cerl:anns(),
                    clauses,	% clauses :: [Tree],
		    timeout,		% timeout :: Tree,
		    action}).		% action :: Tree

-record(c_seq, {anno=[] :: cerl:anns(),
                arg,		% arg :: cerl:cerl(),
		body}).			% body :: cerl:cerl()

-record(c_try, {anno=[], arg,		% arg :: cerl:cerl(),
		vars,			% vars :: [cerl:c_var()],
		body,			% body :: cerl:cerl(),
		evars,			% evars :: [cerl:c_var()],
		handler}).		% handler :: cerl:cerl()

-record(c_tuple, {anno=[] :: cerl:anns(), es :: [cerl:cerl()]}).

-record(c_values, {anno=[] :: cerl:anns(), es :: [cerl:cerl()]}).

-record(c_var, {anno=[] :: cerl:anns(), name :: cerl:var_name()}).
