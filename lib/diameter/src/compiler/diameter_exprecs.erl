%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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
%% Parse transform for generating record access functions.
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
%% causes this transform to insert functions for the exported records:
%%
%%   -module(foo)
%%   -compile({parse_transform, diameter_exprecs}).
%%
%%   -record(r, {a, b, c}).
%%   -export_records([r]).
%%
%%   -export(['#info-'/1, '#info-'/2,
%%            '#new-'/1,  '#new-'/2,
%%            '#get-'/1', '#get-'/2,
%%            '#set-'/2,
%%            '#new-r'/0, '#new-r'/1,
%%            '#get-r'/2, '#set-r'/2,
%%            '#info-r'/1]).
%%
%%   '#info-'(RecName) ->
%%       '#info-'(RecName, fields).
%%
%%   '#info-'(r, Info) ->
%%       '#info-r'(Info).
%%
%%   '#new-'([r | Vals]) -> '#new-r'(Vals);
%%   '#new-'(r) -> #r{}.
%%
%%   '#new-'(r, Vals) -> '#new-r'(Vals).
%%
%%   '#new-r'() -> #r{}.
%%   '#new-r'(Vals) -> '#set-r'(Vals, #r{}).
%%
%%   '#get-'(#r{} = Rec) ->
%%       [r | '#get-r'(Rec)].
%%
%%   '#get-'(Attrs, #r{} = Rec) ->
%%       '#get-r'(Attrs, Rec).
%%
%%   '#get-r'(#r{} = Rec) ->
%%       lists:zip([a,b,c], tl(tuple_to_list(Rec))).
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

-include("diameter_forms.hrl").

%% parse_transform/2

parse_transform(Forms, _Options) ->
    {H,T} = lists:splitwith(fun is_head/1, Forms),
    Rs = [R || {attribute, _, record, R} <- H],
    Es = lists:append([E || {attribute, _, export_records, E} <- H]),
    H ++ [a_export(Es) | f_accessors(Es, Rs)] ++ T.

is_head(T) ->
    not lists:member(element(1,T), [function, eof]).

%% a_export/1

a_export(Exports) ->
    {?attribute, export, [{fname(info), 1},
                          {fname(info), 2},
                          {fname(new), 1},
                          {fname(new), 2},
                          {fname(get), 1},
                          {fname(get), 2},
                          {fname(set), 2}
                          | lists:flatmap(fun export/1, Exports)]}.

export(Rname) ->
    New = fname(new, Rname),
    [{New, 0},
     {New, 1},
     {fname(get, Rname), 1},
     {fname(get, Rname), 2},
     {fname(set, Rname), 2},
     {fname(info, Rname), 1}].

%% f_accessors/2

f_accessors(Es, Rs) ->
    ['#info-/1'(),
     '#info-/2'(Es),
     '#new-/1'(Es),
     '#new-/2'(Es),
     '#get-/1'(Es),
     '#get-/2'(Es),
     '#set-/2'(Es)
     | lists:flatmap(fun(N) -> accessors(N, fields(N, Rs)) end, Es)].

accessors(Rname, Fields) ->
    ['#new-X/0'(Rname),
     '#new-X/1'(Rname),
     '#get-X/1'(Rname, Fields),
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
     lists:map(fun 'info-'/1, Exports) ++ [?BADARG(2)]}.

'info-'(R) ->
    {?clause, [?ATOM(R), ?VAR('Info')],
     [],
     [?CALL(fname(info, R), [?VAR('Info')])]}.

'#new-/1'(Exports) ->
    {?function, fname(new), 1,
     lists:flatmap(fun 'new-'/1, Exports) ++ [?BADARG(1)]}.

'new-'(R) ->
    [{?clause, [?ATOM(R)],
      [],
      [{?record, R, []}]},
     {?clause, [{?cons, ?ATOM(R), ?VAR('Vals')}],
      [],
      [?CALL(fname(new, R), [?VAR('Vals')])]}].

'#new-/2'(Exports) ->
    {?function, fname(new), 2,
     lists:map(fun 'new--'/1, Exports) ++ [?BADARG(2)]}.

'new--'(R) ->
    {?clause, [?ATOM(R), ?VAR('Vals')],
     [],
     [?CALL(fname(new, R), [?VAR('Vals')])]}.

'#get-/1'(Exports) ->
    {?function, fname(get), 1,
     lists:map(fun 'get--'/1, Exports) ++ [?BADARG(1)]}.

'get--'(R) ->
    {?clause, [{?match, {?record, R, []}, ?VAR('Rec')}],
     [],
     [{?cons, ?ATOM(R), ?CALL(fname(get, R), [?VAR('Rec')])}]}.

'#get-/2'(Exports) ->
    {?function, fname(get), 2,
     lists:map(fun 'get-'/1, Exports) ++ [?BADARG(2)]}.

'get-'(R) ->
    {?clause, [?VAR('Attrs'),
               {?match, {?record, R, []}, ?VAR('Rec')}],
     [],
     [?CALL(fname(get, R), [?VAR('Attrs'), ?VAR('Rec')])]}.

'#set-/2'(Exports) ->
    {?function, fname(set), 2,
     lists:map(fun 'set-'/1, Exports) ++ [?BADARG(2)]}.

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

'#get-X/1'(Rname, Fields) ->
    FName = fname(get, Rname),
    Values = ?CALL(tl, [?CALL(tuple_to_list, [?VAR('Rec')])]),
    {?function, FName, 1,
     [{?clause, [?VAR('Rec')],
       [],
       [?APPLY(lists, zip, [?TERM(Fields), Values])]}]}.

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
