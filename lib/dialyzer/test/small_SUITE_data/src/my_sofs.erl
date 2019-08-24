%% Program showing the problems with record field accesses.

-module(my_sofs).
-export([ordset_of_sets/3, is_equal/2]).

-define(TAG, 'Set').
-define(ORDTAG, 'OrdSet').

-record(?TAG, {data = [], type = type}).
-record(?ORDTAG, {orddata = {}, ordtype = type}).

-define(LIST(S), (S)#?TAG.data).
-define(TYPE(S), (S)#?TAG.type).
-define(SET(L, T), #?TAG{data = L, type = T}).
-define(IS_SET(S), record(S, ?TAG)).

%% Ordered sets and atoms:
-define(ORDDATA(S), (S)#?ORDTAG.orddata).
-define(ORDTYPE(S), (S)#?ORDTAG.ordtype).
-define(ORDSET(L, T), #?ORDTAG{orddata = L, ordtype = T}).
-define(IS_ORDSET(S), record(S, ?ORDTAG)).

%% When IS_SET is true:
-define(ANYTYPE, '_').
-define(REL_TYPE(I, R), element(I, R)).
-define(SET_OF(X), [X]).

is_equal(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
        true  -> ?LIST(S1) == ?LIST(S2);
        false -> erlang:error(type_mismatch, [S1, S2])
    end;
is_equal(S1, S2) when ?IS_ORDSET(S1), ?IS_ORDSET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
        true  -> ?ORDDATA(S1) == ?ORDDATA(S2);
        false -> erlang:error(type_mismatch, [S1, S2])
    end;
is_equal(S1, S2) when ?IS_SET(S1), ?IS_ORDSET(S2) ->
    erlang:error(type_mismatch, [S1, S2]);
is_equal(S1, S2) when ?IS_ORDSET(S1), ?IS_SET(S2) ->
    erlang:error(type_mismatch, [S1, S2]).

%% Type = OrderedSetType
%%      | SetType
%%      | atom() except '_'
%% OrderedSetType = {Type, ..., Type}
%% SetType = [ElementType]           % list of exactly one element
%% ElementType = '_'                 % any type (implies empty set)
%%             | Type

ordset_of_sets([S | Ss], L, T) when ?IS_SET(S) ->
    ordset_of_sets(Ss, [?LIST(S) | L], [[?TYPE(S)] | T]);
ordset_of_sets([S | Ss], L, T) when ?IS_ORDSET(S) ->
    ordset_of_sets(Ss, [?LIST(S) | L], [?ORDTYPE(S) | T]);
ordset_of_sets([], L, T) ->
    ?ORDSET(list_to_tuple(lists:reverse(L)), list_to_tuple(lists:reverse(T)));
ordset_of_sets(_, _L, _T) ->
    error.

%% inlined.
match_types(T, T) -> true;
match_types(Type1, Type2) -> match_types1(Type1, Type2).

match_types1(Atom, Atom) when is_atom(Atom) ->
    true;
match_types1(?ANYTYPE, _) ->
    true;
match_types1(_, ?ANYTYPE) ->
    true;
match_types1(?SET_OF(Type1), ?SET_OF(Type2)) ->
    match_types1(Type1, Type2);
match_types1(T1, T2) when tuple(T1), tuple(T2), size(T1) =:= size(T2) ->
    match_typesl(size(T1), T1, T2);
match_types1(_T1, _T2) ->
    false.

match_typesl(0, _T1, _T2) ->
    true;
match_typesl(N, T1, T2) ->
    case match_types1(?REL_TYPE(N, T1), ?REL_TYPE(N, T2)) of
        true  -> match_typesl(N-1, T1, T2);
        false -> false
    end.
