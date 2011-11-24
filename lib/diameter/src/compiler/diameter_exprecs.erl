%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
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
%% Parse transform for generating record access functions
%%
%% This parse transform can be used to reduce compile-time
%% dependencies in large systems.
%%
%% In the old days, before records, Erlang programmers often wrote
%% access functions for tuple data. This was tedious and error-prone.
%% The record syntax made this easier, but since records were implemented
%% fully in the pre-processor, a nasty compile-time dependency was
%% introduced.
%%
%% This module automates the generation of access functions for
%% records. While this method cannot fully replace the utility of
%% pattern matching, it does allow a fair bit of functionality on
%% records without the need for compile-time dependencies.
%%
%% Whenever record definitions need to be exported from a module,
%% inserting a compiler attribute,
%%
%%   export_records([RecName, ...])
%%
%% causes this transform to lay out access functions for the exported
%% records:
%%
%%   -module(foo)
%%   -compile({parse_transform, diameter_exprecs}).
%%
%%   -record(r, {a, b, c}).
%%   -export_records([a]).
%%
%%   -export(['#info-'/1, '#info-'/2,
%%            '#new-'/1, '#new-'/2,
%%            '#get-'/2, '#set-'/2,
%%            '#new-a'/0, '#new-a'/1,
%%            '#get-a'/2, '#set-a'/2,
%%            '#info-a'/1]).
%%
%%   '#info-'(RecName) ->
%%       '#info-'(RecName, fields).
%%
%%   '#info-'(r, Info) ->
%%       '#info-r'(Info).
%%
%%   '#new-'(r) -> #r{}.
%%   '#new-'(r, Vals) -> '#new-r'(Vals)
%%
%%   '#new-r'() -> #r{}.
%%   '#new-r'(Vals) -> '#set-r'(Vals, #r{}).
%%
%%   '#get-'(Attrs, #r{} = Rec) ->
%%       '#get-r'(Attrs, Rec).
%%
%%   '#get-r'(Attrs, Rec) when is_list(Attrs) ->
%%       ['#get-r'(A, Rec) || A <- Attrs];
%%   '#get-r'(a, Rec) -> Rec#r.a;
%%   '#get-r'(b, Rec) -> Rec#r.b;
%%   '#get-r'(c, Rec) -> Rec#r.c.
%%
%%   '#set-'(Vals, #r{} = Rec) ->
%%       '#set-r'(Vals, Rec).
%%
%%   '#set-r'(Vals, Rec) when is_list(Vals) ->
%%       lists:foldl(fun '#set-r'/2, Rec, Vals);
%%   '#set-r'({a,V}, Rec) -> Rec#r{a = V};
%%   '#set-r'({b,V}, Rec) -> Rec#r{b = V};
%%   '#set-r'({c,V}, Rec) -> Rec#r{c = V}.
%%
%%   '#info-r'(fields) -> record_info(fields, r);
%%   '#info-r'(size) -> record_info(size, r);
%%   '#info-r'({index, a}) -> 1;
%%   '#info-r'({index, b}) -> 2;
%%   '#info-r'({index, c}) -> 3;
%%

-module(diameter_exprecs).

-export([parse_transform/2]).

%% Form tag with line number.
-define(F(T), T, ?LINE).
%% Yes, that's right. The replacement is to the first unmatched ')'.

-define(attribute,    ?F(attribute)).
-define(clause,       ?F(clause)).
-define(function,     ?F(function)).
-define(call,         ?F(call)).
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
-define(VAR(V),       {var, ?LINE, V}).

-define(CALL(F,A),    {?call, ?ATOM(F), A}).
-define(APPLY(M,F,A), {?call, {?remote, ?ATOM(M), ?ATOM(F)}, A}).

%% parse_transform/2

parse_transform(Forms, _Options) ->
    Rs = [R || {attribute, _, record, R} <- Forms],
    case lists:append([E || {attribute, _, export_records, E} <- Forms]) of
        [] ->
            Forms;
        Es ->
            {H,T} = lists:splitwith(fun is_head/1, Forms),
            H ++ [a_export(Es) | f_accessors(Es, Rs)] ++ T
    end.

is_head(T) ->
    not lists:member(element(1,T), [function, eof]).

%% a_export/1

a_export(Exports) ->
    {?attribute, export, [{fname(info), 1},
                          {fname(info), 2},
                          {fname(new), 1},
                          {fname(new), 2},
                          {fname(get), 2},
                          {fname(set), 2}
                          | lists:flatmap(fun export/1, Exports)]}.

export(Rname) ->
    New = fname(new, Rname),
    [{New, 0},
     {New, 1},
     {fname(get, Rname), 2},
     {fname(set, Rname), 2},
     {fname(info, Rname), 1}].

%% f_accessors/2

f_accessors(Es, Rs) ->
    ['#info-/1'(),
     '#info-/2'(Es),
     '#new-/1'(Es),
     '#new-/2'(Es),
     '#get-/2'(Es),
     '#set-/2'(Es)
     | lists:flatmap(fun(N) -> accessors(N, fields(N, Rs)) end, Es)].

accessors(Rname, Fields) ->
    ['#new-X/0'(Rname),
     '#new-X/1'(Rname),
     '#get-X/2'(Rname, Fields),
     '#set-X/2'(Rname, Fields),
     '#info-X/1'(Rname, Fields)].

fields(Rname, Recs) ->
    {Rname, Fields} = lists:keyfind(Rname, 1, Recs),
    lists:map(fun({record_field, _, {atom, _, N}})    -> N;
                 ({record_field, _, {atom, _, N}, _}) -> N
              end,
              Fields).

fname_prefix(Op) ->
    "#" ++ atom_to_list(Op) ++ "-".

fname(Op) ->
    list_to_atom(fname_prefix(Op)).

fname(Op, Rname) ->
    Prefix = fname_prefix(Op),
    list_to_atom(Prefix ++ atom_to_list(Rname)).

%% Generated functions.

'#info-/1'() ->
    Fname = fname(info),
    {?function, Fname, 1,
     [{?clause, [?VAR('RecName')],
       [],
       [?CALL(Fname, [?VAR('RecName'), ?ATOM(fields)])]}]}.

'#info-/2'(Exports) ->
    {?function, fname(info), 2,
     lists:map(fun 'info-'/1, Exports)}.

'info-'(R) ->
    {?clause, [?ATOM(R), ?VAR('Info')],
     [],
     [?CALL(fname(info, R), [?VAR('Info')])]}.

'#new-/1'(Exports) ->
    {?function, fname(new), 1,
     lists:map(fun 'new-'/1, Exports)}.

'new-'(R) ->
    {?clause, [?ATOM(R)],
     [],
     [{?record, R, []}]}.

'#new-/2'(Exports) ->
    {?function, fname(new), 2,
     lists:map(fun 'new--'/1, Exports)}.

'new--'(R) ->
    {?clause, [?ATOM(R), ?VAR('Vals')],
     [],
     [?CALL(fname(new, R), [?VAR('Vals')])]}.

'#get-/2'(Exports) ->
    {?function, fname(get), 2,
     lists:map(fun 'get-'/1, Exports)}.

'get-'(R) ->
    {?clause, [?VAR('Attrs'),
               {?match, {?record, R, []}, ?VAR('Rec')}],
     [],
     [?CALL(fname(get, R), [?VAR('Attrs'), ?VAR('Rec')])]}.

'#set-/2'(Exports) ->
    {?function, fname(set), 2,
     lists:map(fun 'set-'/1, Exports)}.

'set-'(R) ->
    {?clause, [?VAR('Vals'), {?match, {?record, R, []}, ?VAR('Rec')}],
     [],
     [?CALL(fname(set, R), [?VAR('Vals'), ?VAR('Rec')])]}.

'#new-X/0'(Rname) ->
    {?function, fname(new, Rname), 0,
     [{?clause, [],
       [],
       [{?record, Rname, []}]}]}.

'#new-X/1'(Rname) ->
    {?function, fname(new, Rname), 1,
     [{?clause, [?VAR('Vals')],
       [],
       [?CALL(fname(set, Rname), [?VAR('Vals'), {?record, Rname, []}])]}]}.

'#set-X/2'(Rname, Fields) ->
    {?function, fname(set, Rname), 2,
     [{?clause, [?VAR('Vals'), ?VAR('Rec')],
       [[?CALL(is_list, [?VAR('Vals')])]],
       [?APPLY(lists, foldl, [{?'fun', {function, fname(set, Rname), 2}},
                              ?VAR('Rec'),
                              ?VAR('Vals')])]}
      | lists:map(fun(A) -> 'set-X'(Rname, A) end, Fields)]}.

'set-X'(Rname, Attr) ->
    {?clause, [{?tuple, [?ATOM(Attr), ?VAR('V')]}, ?VAR('Rec')],
     [],
     [{?record, ?VAR('Rec'), Rname,
       [{?record_field, ?ATOM(Attr), ?VAR('V')}]}]}.

'#get-X/2'(Rname, Fields) ->
    FName = fname(get, Rname),
    {?function, FName, 2,
     [{?clause, [?VAR('Attrs'), ?VAR('Rec')],
       [[?CALL(is_list, [?VAR('Attrs')])]],
       [{?lc, ?CALL(FName, [?VAR('A'), ?VAR('Rec')]),
	 [{?generate, ?VAR('A'), ?VAR('Attrs')}]}]}
      | lists:map(fun(A) -> 'get-X'(Rname, A) end, Fields)]}.

'get-X'(Rname, Attr) ->
    {?clause, [?ATOM(Attr), ?VAR('Rec')],
     [],
     [{?record_field, ?VAR('Rec'), Rname, ?ATOM(Attr)}]}.

'#info-X/1'(Rname, Fields) ->
    {?function, fname(info, Rname), 1,
     [{?clause, [?ATOM(fields)],
       [],
       [?CALL(record_info, [?ATOM(fields), ?ATOM(Rname)])]},
      {?clause, [?ATOM(size)],
       [],
       [?CALL(record_info, [?ATOM(size), ?ATOM(Rname)])]}
      | lists:map(fun(A) -> 'info-X'(Rname, A) end, Fields)]}.

'info-X'(Rname, Attr) ->
    {?clause, [{?tuple, [?ATOM(index), ?ATOM(Attr)]}],
     [],
     [{?record_index, Rname, ?ATOM(Attr)}]}.
