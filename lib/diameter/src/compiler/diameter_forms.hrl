%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2014. All Rights Reserved.
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

%%
%% Macros used when building abstract code.
%%

%% Generated functions that could have no generated clauses will have
%% a trailing ?BADARG clause that should never execute as called
%% by diameter.
-define(BADARG(N), {?clause, [?VAR('_') || _ <- lists:seq(1,N)],
                    [],
                    [?APPLY(erlang, error, [?ATOM(badarg)])]}).

%% Form tag with line number.
-define(F(T), T, ?LINE).
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

-define(ATOM(T),      {atom, ?LINE, T}).
-define(INTEGER(N),   {integer, ?LINE, N}).
-define(VAR(V),       {var, ?LINE, V}).
-define(NIL,          {nil, ?LINE}).

-define(CALL(F,A),    {?call, ?ATOM(F), A}).
-define(APPLY(M,F,A), {?call, {?remote, ?ATOM(M), ?ATOM(F)}, A}).
-define(FIELDS(Fs),   [{?record_field, ?ATOM(F), V} || {F,V} <- Fs]).

%% Literal term.
-define(TERM(T), erl_parse:abstract(T, [
                     {line, ?LINE},
                     {encoding, fun diameter_codegen:is_printable_ascii/1}])).
