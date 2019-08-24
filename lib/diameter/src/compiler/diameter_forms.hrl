%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
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

%%
%% Macros used when building abstract code.
%%

%% Generated functions that could have no generated clauses will have
%% a trailing ?BADARG clause that should never execute as called
%% by diameter.
-define(BADARG(N), {?clause, [?VAR('_') || _ <- lists:seq(1,N)],
                    [],
                    [?APPLY(erlang, error, [?ATOM(badarg)])]}).

-define(ANNO(L), erl_anno:new(L)).

%% Form tag with line number.
-define(F(T), T, ?ANNO(?LINE)).
%% Yes, that's right. The replacement is to the first unmatched ')'.

-define(attribute,    ?F(attribute)).
-define(clause,       ?F(clause)).
-define(function,     ?F(function)).
-define(call,         ?F(call)).
-define(cons,         ?F(cons)).
-define('fun',        ?F('fun')).
-define(generate,     ?F(generate)).
-define(lc,           ?F(lc)).
-define(match,        ?F(match)).
-define(remote,       ?F(remote)).
-define(record,       ?F(record)).
-define(record_field, ?F(record_field)).
-define(record_index, ?F(record_index)).
-define(tuple,        ?F(tuple)).

-define(ATOM(T),      {atom, ?ANNO(?LINE), T}).
-define(INTEGER(N),   {integer, ?ANNO(?LINE), N}).
-define(VAR(V),       {var, ?ANNO(?LINE), V}).
-define(NIL,          {nil, ?ANNO(?LINE)}).

-define(CALL(F,A),    {?call, ?ATOM(F), A}).
-define(APPLY(M,F,A), {?call, {?remote, ?ATOM(M), ?ATOM(F)}, A}).
-define(FIELDS(Fs),   [{?record_field, ?ATOM(F), V} || {F,V} <- Fs]).

%% Literal term.
-define(TERM(T), erl_parse:abstract(T, [
                     {line, ?LINE},
                     {encoding, fun diameter_codegen:is_printable_ascii/1}])).
