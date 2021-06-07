%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2019. All Rights Reserved.
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
%% Purpose : Core Erlang syntax trees as records.

%% It would be nice to incorporate some generic functions as well but
%% this could make including this file difficult.

%% Note: the annotation list is *always* the first record field.
%% Thus it is possible to define the macros:
%% -define(get_ann(X), element(2, X)).
%% -define(set_ann(X, Y), setelement(2, X, Y)).

%% The record definitions appear alphabetically

-record(c_alias, {anno=[] :: list(), var :: cerl:cerl(),
		  pat :: cerl:cerl()}).

-record(c_apply, {anno=[] :: list(), op :: cerl:cerl(),
		  args :: [cerl:cerl()]}).

-record(c_binary, {anno=[] :: list(), segments :: [cerl:c_bitstr()]}).

-record(c_bitstr, {anno=[] :: list(), val :: cerl:cerl(),
		   size :: cerl:cerl(),
		   unit :: cerl:cerl(),
		   type :: cerl:cerl(),
		   flags :: cerl:cerl()}).

-record(c_call, {anno=[] :: list(), module :: cerl:cerl(),
		 name :: cerl:cerl(),
		 args :: [cerl:cerl()]}).

-record(c_case, {anno=[] :: list(), arg :: cerl:cerl(),
		 clauses :: [cerl:cerl()]}).

-record(c_catch, {anno=[] :: list(), body :: cerl:cerl()}).

-record(c_clause, {anno=[] :: list(), pats :: [cerl:cerl()],
		   guard :: cerl:cerl(),
		   body :: cerl:cerl() | any()}). % TODO

-record(c_cons, {anno=[] :: list(), hd :: cerl:cerl(),
		 tl :: cerl:cerl()}).

-record(c_fun, {anno=[] :: list(), vars :: [cerl:cerl()],
		body :: cerl:cerl()}).

-record(c_let, {anno=[] :: list(), vars :: [cerl:cerl()],
		arg :: cerl:cerl(),
		body :: cerl:cerl()}).

-record(c_letrec, {anno=[] :: list(),
                   defs :: [{cerl:cerl(), cerl:cerl()}],
		   body :: cerl:cerl()}).

-record(c_literal, {anno=[] :: list(), val :: any()}).

-record(c_map, {anno=[] :: list(),
		arg=#c_literal{val=#{}} :: cerl:c_var() | cerl:c_literal(),
		es :: [cerl:c_map_pair()],
		is_pat=false :: boolean()}).

-record(c_map_pair, {anno=[] :: list(),
	             op :: #c_literal{val::'assoc'} | #c_literal{val::'exact'},
		     key :: any(),              % TODO
		     val :: any()}).            % TODO

-record(c_module, {anno=[] :: list(), name :: cerl:cerl(),
		   exports :: [cerl:cerl()],
		   attrs :: [{cerl:cerl(), cerl:cerl()}],
		   defs :: [{cerl:cerl(), cerl:cerl()}]}).

-record(c_primop, {anno=[] :: list(), name :: cerl:cerl(),
		   args :: [cerl:cerl()]}).

-record(c_receive, {anno=[] :: list(), clauses :: [cerl:cerl()],
		    timeout :: cerl:cerl(),
		    action :: cerl:cerl()}).

-record(c_seq, {anno=[] :: list(), arg :: cerl:cerl() | any(), % TODO
		body :: cerl:cerl()}).

-record(c_try, {anno=[] :: list(), arg :: cerl:cerl(),
		vars :: [cerl:cerl()],
		body :: cerl:cerl(),
		evars :: [cerl:cerl()],
		handler :: cerl:cerl()}).

-record(c_tuple, {anno=[] :: list(), es :: [cerl:cerl()]}).

-record(c_values, {anno=[] :: list(), es :: [cerl:cerl()]}).

-record(c_var, {anno=[] :: list(), name :: cerl:var_name()}).
