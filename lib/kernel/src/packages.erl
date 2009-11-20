%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2009. All Rights Reserved.
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
-module(packages).

-export([to_string/1, concat/1, concat/2, is_valid/1, is_segmented/1,
	 split/1, last/1, first/1, strip_last/1, find_modules/1,
	 find_modules/2]).

%% A package name (or a package-qualified module name) may be an atom or
%% a string (list of nonnegative integers) - not a deep list, and not a
%% list containing atoms. A name may be empty, but may not contain two
%% consecutive period (`.') characters or end with a period character.

-type package_name() :: atom() | string().

-spec to_string(package_name()) -> string().
to_string(Name) when is_atom(Name) ->
    atom_to_list(Name);
to_string(Name) ->
    Name.

%% `concat' does not insert a leading period if the first segment is
%% empty. However, the result may contain leading, consecutive or
%% dangling period characters, if any of the segments after the first
%% are empty. Use 'is_valid' to check the result if necessary.

-spec concat(package_name(), package_name()) -> string().
concat(A, B) ->
    concat([A, B]).

-spec concat([package_name()]) -> string().
concat([H | T]) when is_atom(H) ->
    concat([atom_to_list(H) | T]);
concat(["" | T]) ->
    concat_1(T);
concat(L) ->
    concat_1(L).

concat_1([H | T]) when is_atom(H) ->
    concat_1([atom_to_list(H) | T]);
concat_1([H]) ->
    H;
concat_1([H | T]) ->
    H ++ "." ++ concat_1(T);
concat_1([]) ->
    "";
concat_1(Name) ->
    erlang:error({badarg, Name}).

-spec is_valid(package_name()) -> boolean().
is_valid(Name) when is_atom(Name) ->
    is_valid_1(atom_to_list(Name));
is_valid([$. | _]) ->
    false;
is_valid(Name) ->
    is_valid_1(Name).

is_valid_1([$.]) -> false;
is_valid_1([$., $. | _]) -> false;
is_valid_1([H | T]) when is_integer(H), H >= 0 ->
    is_valid_1(T);
is_valid_1([]) -> true;
is_valid_1(_) -> false.

-spec split(package_name()) -> [string()].
split(Name) when is_atom(Name) ->
    split_1(atom_to_list(Name), []);
split(Name) ->
    split_1(Name, []).

split_1([$. | T], Cs) ->
    [lists:reverse(Cs) | split_1(T, [])];
split_1([H | T], Cs) when is_integer(H), H >= 0 ->
    split_1(T, [H | Cs]);
split_1([], Cs) ->
    [lists:reverse(Cs)];
split_1(_, _) ->
    erlang:error(badarg).

%% This is equivalent to testing if `split(Name)' yields a list of
%% length larger than one (i.e., if the name can be split into two or
%% more segments), but is cheaper.

-spec is_segmented(package_name()) -> boolean().
is_segmented(Name) when is_atom(Name) ->
    is_segmented_1(atom_to_list(Name));
is_segmented(Name) ->
    is_segmented_1(Name).

is_segmented_1([$. | _]) -> true;
is_segmented_1([H | T]) when is_integer(H), H >= 0 ->
    is_segmented_1(T);
is_segmented_1([]) -> false;
is_segmented_1(_) ->
    erlang:error(badarg).

-spec last(package_name()) -> string().
last(Name) ->
    last_1(split(Name)).

last_1([H]) -> H;
last_1([_ | T]) -> last_1(T).

-spec first(package_name()) -> [string()].
first(Name) ->
    first_1(split(Name)).

first_1([H | T]) when T =/= [] -> [H | first_1(T)];
first_1(_) -> [].

-spec strip_last(package_name()) -> string().
strip_last(Name) ->
    concat(first(Name)).

%% This finds all modules available for a given package, using the
%% current code server search path. (There is no guarantee that the
%% modules are loadable; only that the object files exist.)

-spec find_modules(package_name()) -> [string()].
find_modules(P) ->
    find_modules(P, code:get_path()).

-spec find_modules(package_name(), [string()]) -> [string()].
find_modules(P, Paths) ->
    P1 = filename:join(packages:split(P)),
    find_modules(P1, Paths, code:objfile_extension(), sets:new()).

find_modules(P, [Path | Paths], Ext, S0) ->
    case file:list_dir(filename:join(Path, P)) of
	{ok, Fs} ->
	    Fs1 = [F || F <- Fs, filename:extension(F) =:= Ext],
	    S1 = lists:foldl(fun (F, S) ->
				     F1 = filename:rootname(F, Ext),
				     sets:add_element(F1, S)
			     end,
			     S0, Fs1),
	    find_modules(P, Paths, Ext, S1);
	_ ->
	    find_modules(P, Paths, Ext, S0)
    end;   
find_modules(_P, [], _Ext, S) ->
    sets:to_list(S).
