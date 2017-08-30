%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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
%% @author Richard Carlsson <richardc@it.uu.se>
%% @author Dan Gudmundsson <dgud@erix.ericsson.se>
%% @version 1.0

%% @doc Functional, extendible arrays. Arrays can have fixed size, or
%% can grow automatically as needed. A default value is used for entries
%% that have not been explicitly set.
%%
%% Arrays uses <b>zero</b> based indexing. This is a deliberate design
%% choice and differs from other erlang datastructures, e.g. tuples. 
%%
%% Unless specified by the user when the array is created, the default
%% value is the atom `undefined'. There is no difference between an
%% unset entry and an entry which has been explicitly set to the same
%% value as the default one (cf. {@link reset/2}). If you need to
%% differentiate between unset and set entries, you must make sure that
%% the default value cannot be confused with the values of set entries.
%%
%% The array never shrinks automatically; if an index `I' has been used
%% successfully to set an entry, all indices in the range [0,`I'] will
%% stay accessible unless the array size is explicitly changed by
%% calling {@link resize/2}.
%% 
%% Examples:
%% ```
%% %% Create a fixed-size array with entries 0-9 set to 'undefined'
%% A0 = array:new(10).
%% 10 = array:size(A0).
%%
%% %% Create an extendible array and set entry 17 to 'true',
%% %% causing the array to grow automatically
%% A1 = array:set(17, true, array:new()).
%% 18 = array:size(A1).
%%
%% %% Read back a stored value
%% true = array:get(17, A1).
%%
%% %% Accessing an unset entry returns the default value
%% undefined = array:get(3, A1).
%%
%% %% Accessing an entry beyond the last set entry also returns the
%% %% default value, if the array does not have fixed size
%% undefined = array:get(18, A1).
%%
%% %% "sparse" functions ignore default-valued entries
%% A2 = array:set(4, false, A1).
%% [{4, false}, {17, true}] = array:sparse_to_orddict(A2).
%%
%% %% An extendible array can be made fixed-size later
%% A3 = array:fix(A2).
%%
%% %% A fixed-size array does not grow automatically and does not
%% %% allow accesses beyond the last set entry
%% {'EXIT',{badarg,_}} = (catch array:set(18, true, A3)).
%% {'EXIT',{badarg,_}} = (catch array:get(18, A3)).
%% '''

%% @type array(). A functional, extendible array. The representation is
%% not documented and is subject to change without notice. Note that
%% arrays cannot be directly compared for equality.

-module(array).

-export([new/0, new/1, new/2, is_array/1, set/3, get/2, size/1,
	 sparse_size/1, default/1, reset/2, to_list/1, sparse_to_list/1,
	 from_list/1, from_list/2, to_orddict/1, sparse_to_orddict/1,
	 from_orddict/1, from_orddict/2, map/2, sparse_map/2, foldl/3,
	 foldr/3, sparse_foldl/3, sparse_foldr/3, fix/1, relax/1, is_fix/1,
	 resize/1, resize/2]).

-export_type([array/0, array/1]).

%%-define(TEST,1).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% Developers: 
%% 
%% For OTP devs: Both tests and documentation is extracted from this
%% file, keep and update this file, 
%% test are extracted with array_SUITE:extract_tests().
%% Doc with docb_gen array.erl
%%  
%% The key to speed is to minimize the number of tests, on
%% large input. Always make the most probable path as short as possible.
%% In particular, keep in mind that for large trees, the probability of
%% a leaf node is small relative to that of an internal node.
%%
%% If you try to tweak the set_1 and get_1 loops: Measure, look at the
%% generated Beam code, and measure again! The argument order matters!


%% Representation:
%%
%% A tree is either a leaf, with LEAFSIZE elements (the "base"), an
%% internal node with LEAFSIZE+1 elements, or an unexpanded tree,
%% represented by a single integer: the number of elements that may be
%% stored in the tree when it is expanded. The last element of an
%% internal node caches the number of elements that may be stored in
%% each of its subtrees.
%% 
%% Note that to update an entry in a tree of height h = log[b] n, the
%% total number of written words is (b+1)+(h-1)*(b+2), since tuples use
%% a header word on the heap. 4 is the optimal base for minimizing the
%% number of words written, but causes higher trees, which takes time.
%% The best compromise between speed and memory usage seems to lie
%% around 8-10. Measurements indicate that the optimum base for speed is
%% 24 - above that, it gets slower again due to the high memory usage.
%% Base 10 is a good choice, giving 2/3 of the possible speedup from
%% base 4, but only using 1/3 more memory. (Base 24 uses 65% more memory
%% per write than base 10, but the speedup is only 21%.)

-define(DEFAULT, undefined).
-define(LEAFSIZE, 10).		% the "base"
-define(NODESIZE, ?LEAFSIZE).   % (no reason to have a different size)
-define(NODEPATTERN(S), {_,_,_,_,_,_,_,_,_,_,S}). % NODESIZE+1 elements!
-define(NEW_NODE(S),  % beware of argument duplication!
	setelement((?NODESIZE+1),erlang:make_tuple((?NODESIZE+1),(S)),(S))).
-define(NEW_LEAF(D), erlang:make_tuple(?LEAFSIZE,(D))).
-define(NODELEAFS, ?NODESIZE*?LEAFSIZE).

%% These make the code a little easier to experiment with.
%% It turned out that using shifts (when NODESIZE=2^n) was not faster.
-define(reduce(X), ((X) div (?NODESIZE))).
-define(extend(X), ((X) * (?NODESIZE))).

%%--------------------------------------------------------------------------

-type element_tuple(T) ::
        {T, T, T, T, T, T, T, T, T, T}
      | {element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), element_tuple(T), element_tuple(T),
         element_tuple(T), non_neg_integer()}.

-type elements(T) :: non_neg_integer()
                   | element_tuple(T)
                   | nil(). % kill reference, for GC

-record(array, {size :: non_neg_integer(),	%% number of defined entries
		max  :: non_neg_integer(),	%% maximum number of entries
						%% in current tree
		default,	%% the default value (usually 'undefined')
                elements :: elements(_)         %% the tuple tree
	       }).

-type array() :: array(term()).

-opaque array(Type) ::
          #array{default :: Type, elements :: elements(Type)}.

%%
%% Types
%%

-type array_indx() :: non_neg_integer().

-type array_opt()  :: {'fixed', boolean()} | 'fixed'
                    | {'default', Type :: term()}
                    | {'size', N :: non_neg_integer()}
                    | (N :: non_neg_integer()).
-type array_opts() :: array_opt() | [array_opt()].

-type indx_pair(Type)  :: {Index :: array_indx(), Type}.
-type indx_pairs(Type) :: [indx_pair(Type)].

%%--------------------------------------------------------------------------

%% @doc Create a new, extendible array with initial size zero.
%% @equiv new([])
%%
%% @see new/1
%% @see new/2

-spec new() -> array().

new() ->
    new([]).

%% @doc Create a new array according to the given options. By default,
%% the array is extendible and has initial size zero. Array indices
%% start at 0.
%% 
%% `Options' is a single term or a list of terms, selected from the
%% following:
%% <dl>
%%   <dt>`N::integer()' or `{size, N::integer()}'</dt>
%%   <dd>Specifies the initial size of the array; this also implies
%%   `{fixed, true}'. If `N' is not a nonnegative integer, the call
%%   fails with reason `badarg'.</dd>
%%   <dt>`fixed' or `{fixed, true}'</dt>
%%   <dd>Creates a fixed-size array; see also {@link fix/1}.</dd>
%%   <dt>`{fixed, false}'</dt>
%%   <dd>Creates an extendible (non fixed-size) array.</dd>
%%   <dt>`{default, Value}'</dt>
%%   <dd>Sets the default value for the array to `Value'.</dd>
%% </dl>
%% Options are processed in the order they occur in the list, i.e.,
%% later options have higher precedence.
%%
%% The default value is used as the value of uninitialized entries, and
%% cannot be changed once the array has been created.
%%
%% Examples:
%% ```array:new(100)''' creates a fixed-size array of size 100.
%% ```array:new({default,0})''' creates an empty, extendible array
%% whose default value is 0.
%% ```array:new([{size,10},{fixed,false},{default,-1}])''' creates an
%% extendible array with initial size 10 whose default value is -1.
%%
%% @see new/0
%% @see new/2
%% @see set/3
%% @see get/2
%% @see from_list/2
%% @see fix/1

-spec new(Options :: array_opts()) -> array().

new(Options) ->
    new_0(Options, 0, false).

%% @doc Create a new array according to the given size and options. If
%% `Size' is not a nonnegative integer, the call fails with reason
%% `badarg'. By default, the array has fixed size. Note that any size
%% specifications in `Options' will override the `Size' parameter.
%%
%% If `Options' is a list, this is simply equivalent to `new([{size,
%% Size} | Options]', otherwise it is equivalent to `new([{size, Size} |
%% [Options]]'. However, using this function directly is more efficient.
%% 
%% Example:
%% ```array:new(100, {default,0})''' creates a fixed-size array of size
%% 100, whose default value is 0.
%%
%% @see new/1

-spec new(Size :: non_neg_integer(), Options :: array_opts()) -> array().

new(Size, Options) when is_integer(Size), Size >= 0 ->
    new_0(Options, Size, true);
new(_, _) ->
    erlang:error(badarg).

new_0(Options, Size, Fixed) when is_list(Options) ->
    new_1(Options, Size, Fixed, ?DEFAULT);
new_0(Options, Size, Fixed) ->
    new_1([Options], Size, Fixed, ?DEFAULT).

new_1([fixed | Options], Size, _, Default) ->
    new_1(Options, Size, true, Default);
new_1([{fixed, Fixed} | Options], Size, _, Default)
  when is_boolean(Fixed) ->
    new_1(Options, Size, Fixed, Default);
new_1([{default, Default} | Options], Size, Fixed, _) ->
    new_1(Options, Size, Fixed, Default);
new_1([{size, Size} | Options], _, _, Default)
  when is_integer(Size), Size >= 0 ->
    new_1(Options, Size, true, Default);
new_1([Size | Options], _, _, Default)
  when is_integer(Size), Size >= 0 ->
    new_1(Options, Size, true, Default);
new_1([], Size, Fixed, Default) ->
    new(Size, Fixed, Default);
new_1(_Options, _Size, _Fixed, _Default) ->
    erlang:error(badarg).

new(0, false, undefined) ->
    %% Constant empty array
    #array{size=0, max=?LEAFSIZE, elements=?LEAFSIZE};
new(Size, Fixed, Default) ->
    E = find_max(Size - 1, ?LEAFSIZE),
    M = if Fixed -> 0;
	   true -> E
	end,
    #array{size = Size, max = M, default = Default, elements = E}.

-spec find_max(integer(), integer()) -> integer().

find_max(I, M) when I >= M ->
    find_max(I, ?extend(M));
find_max(_I, M) ->
    M.


%% @doc Returns `true' if `X' appears to be an array, otherwise `false'.
%% Note that the check is only shallow; there is no guarantee that `X'
%% is a well-formed array representation even if this function returns
%% `true'.

-spec is_array(X :: term()) -> boolean().

is_array(#array{size = Size, max = Max})
  when is_integer(Size), is_integer(Max) ->
    true;
is_array(_) ->
    false.


%% @doc Get the number of entries in the array. Entries are numbered
%% from 0 to `size(Array)-1'; hence, this is also the index of the first
%% entry that is guaranteed to not have been previously set.
%% @see set/3
%% @see sparse_size/1

-spec size(Array :: array()) -> non_neg_integer().

size(#array{size = N}) -> N;
size(_) -> erlang:error(badarg).


%% @doc Get the value used for uninitialized entries.
%%
%% @see new/2

-spec default(Array :: array(Type)) -> Value :: Type.

default(#array{default = D}) -> D;
default(_) -> erlang:error(badarg).


-ifdef(EUNIT).
new_test_() ->
    N0 = ?LEAFSIZE,
    N01 = N0+1,
    N1 = ?NODESIZE*N0,
    N11 = N1+1,
    N2 = ?NODESIZE*N1,
    [?_test(new()),

     ?_test(new([])),
     ?_test(new(10)),
     ?_test(new({size,10})),
     ?_test(new(fixed)),
     ?_test(new({fixed,true})),
     ?_test(new({fixed,false})),
     ?_test(new({default,undefined})),
     ?_test(new([{size,100},{fixed,false},{default,undefined}])),
     ?_test(new([100,fixed,{default,0}])),

     ?_assert(new() =:= new([])),
     ?_assert(new() =:= new([{size,0},{default,undefined},{fixed,false}])),
     ?_assert(new() =:= new(0, {fixed,false})),
     ?_assert(new(fixed) =:= new(0)),
     ?_assert(new(fixed) =:= new(0, [])),
     ?_assert(new(10) =:= new([{size,0},{size,5},{size,10}])),
     ?_assert(new(10) =:= new(0, {size,10})),
     ?_assert(new(10, []) =:= new(10, [{default,undefined},{fixed,true}])),

     ?_assertError(badarg, new(-1)),
     ?_assertError(badarg, new(10.0)),
     ?_assertError(badarg, new(undefined)),
     ?_assertError(badarg, new([undefined])),
     ?_assertError(badarg, new([{default,0} | fixed])),

     ?_assertError(badarg, new(-1, [])),
     ?_assertError(badarg, new(10.0, [])),
     ?_assertError(badarg, new(undefined, [])),

     ?_assertMatch(#array{size=0,max=N0,default=undefined,elements=N0},
		   new()),
     ?_assertMatch(#array{size=0,max=0,default=undefined,elements=N0},
		   new(fixed)),
     ?_assertMatch(#array{size=N0,max=N0,elements=N0},
		   new(N0, {fixed,false})),
     ?_assertMatch(#array{size=N01,max=N1,elements=N1},
		   new(N01, {fixed,false})),
     ?_assertMatch(#array{size=N1,max=N1,elements=N1},
		   new(N1, {fixed,false})),
     ?_assertMatch(#array{size=N11,max=N2,elements=N2},
		   new(N11, {fixed,false})),
     ?_assertMatch(#array{size=N2, max=N2, default=42,elements=N2},
		   new(N2, [{fixed,false},{default,42}])),

     ?_assert(0 =:= array:size(new())),
     ?_assert(17 =:= array:size(new(17))),
     ?_assert(100 =:= array:size(array:set(99,0,new()))),
     ?_assertError(badarg, array:size({bad_data,gives_error})),

     ?_assert(undefined =:= default(new())),
     ?_assert(4711 =:= default(new({default,4711}))),
     ?_assert(0 =:= default(new(10, {default,0}))),
     ?_assertError(badarg, default({bad_data,gives_error})),

     ?_assert(is_array(new())),
     ?_assert(false =:= is_array({foobar, 23, 23})),
     ?_assert(false =:= is_array(#array{size=bad})),
     ?_assert(false =:= is_array(#array{max=bad})),
     ?_assert(is_array(new(10))),
     ?_assert(is_array(new(10, {fixed,false})))
    ].
-endif.


%% @doc Fix the size of the array. This prevents it from growing
%% automatically upon insertion; see also {@link set/3}.
%% @see relax/1

-spec fix(Array :: array(Type)) -> array(Type).

fix(#array{}=A) ->
    A#array{max = 0}.


%% @doc Check if the array has fixed size. 
%% Returns `true' if the array is fixed, otherwise `false'.
%% @see fix/1

-spec is_fix(Array :: array()) -> boolean().

is_fix(#array{max = 0}) -> true;
is_fix(#array{}) -> false.


-ifdef(EUNIT).
fix_test_() ->
    [?_assert(is_array(fix(new()))),
     ?_assert(fix(new()) =:= new(fixed)),

     ?_assertNot(is_fix(new())),
     ?_assertNot(is_fix(new([]))),
     ?_assertNot(is_fix(new({fixed,false}))),
     ?_assertNot(is_fix(new(10, {fixed,false}))),
     ?_assert(is_fix(new({fixed,true}))),
     ?_assert(is_fix(new(fixed))),
     ?_assert(is_fix(new(10))),
     ?_assert(is_fix(new(10, []))),
     ?_assert(is_fix(new(10, {fixed,true}))),
     ?_assert(is_fix(fix(new()))),
     ?_assert(is_fix(fix(new({fixed,false})))),

     ?_test(set(0, 17, new())),
     ?_assertError(badarg, set(0, 17, new(fixed))),
     ?_assertError(badarg, set(1, 42, fix(set(0, 17, new())))),

     ?_test(set(9, 17, new(10))),
     ?_assertError(badarg, set(10, 17, new(10))),
     ?_assertError(badarg, set(10, 17, fix(new(10, {fixed,false}))))
    ].
-endif.


%% @doc Make the array resizable. (Reverses the effects of {@link
%% fix/1}.)
%% @see fix/1

-spec relax(Array :: array(Type)) -> array(Type).

relax(#array{size = N}=A) ->
    A#array{max = find_max(N-1, ?LEAFSIZE)}.


-ifdef(EUNIT).
relax_test_() ->
    [?_assert(is_array(relax(new(fixed)))),
     ?_assertNot(is_fix(relax(fix(new())))),
     ?_assertNot(is_fix(relax(new(fixed)))),

     ?_assert(new() =:= relax(new(fixed))),
     ?_assert(new() =:= relax(new(0))),
     ?_assert(new(17, {fixed,false}) =:= relax(new(17))),
     ?_assert(new(100, {fixed,false})
	      =:= relax(fix(new(100, {fixed,false}))))
    ].
-endif.


%% @doc Change the size of the array. If `Size' is not a nonnegative
%% integer, the call fails with reason `badarg'. If the given array has
%% fixed size, the resulting array will also have fixed size.

-spec resize(Size :: non_neg_integer(), Array :: array(Type)) ->
                    array(Type).

resize(Size, #array{size = N, max = M, elements = E}=A)
  when is_integer(Size), Size >= 0 ->
    if Size > N ->
   	    {E1, M1} = grow(Size-1, E,
			    if M > 0 -> M;
			       true -> find_max(N-1, ?LEAFSIZE)
			    end),
	    A#array{size = Size,
		    max = if M > 0 -> M1;
			     true -> M
			  end,
		    elements = E1};
       Size < N ->
	    %% TODO: shrink physical representation when shrinking the array
	    A#array{size = Size};
       true ->
	    A
    end;
resize(_Size, _) ->
    erlang:error(badarg).


%% @doc Change the size of the array to that reported by {@link
%% sparse_size/1}. If the given array has fixed size, the resulting
%% array will also have fixed size.
%% @equiv resize(sparse_size(Array), Array)
%% @see resize/2
%% @see sparse_size/1

-spec resize(Array :: array(Type)) -> array(Type).

resize(Array) ->
    resize(sparse_size(Array), Array).


-ifdef(EUNIT).
resize_test_() ->
    [?_assert(resize(0, new()) =:= new()),
     ?_assert(resize(99, new(99)) =:= new(99)),
     ?_assert(resize(99, relax(new(99))) =:= relax(new(99))),
     ?_assert(is_fix(resize(100, new(10)))),
     ?_assertNot(is_fix(resize(100, relax(new(10))))),

     ?_assert(array:size(resize(100, new())) =:= 100),
     ?_assert(array:size(resize(0, new(100))) =:= 0),
     ?_assert(array:size(resize(99, new(10))) =:= 99),
     ?_assert(array:size(resize(99, new(1000))) =:= 99),

     ?_assertError(badarg, set(99, 17, new(10))),
     ?_test(set(99, 17, resize(100, new(10)))),
     ?_assertError(badarg, set(100, 17, resize(100, new(10)))),

     ?_assert(array:size(resize(new())) =:= 0),
     ?_assert(array:size(resize(new(8))) =:= 0),
     ?_assert(array:size(resize(array:set(7, 0, new()))) =:= 8),
     ?_assert(array:size(resize(array:set(7, 0, new(10)))) =:= 8),
     ?_assert(array:size(resize(array:set(99, 0, new(10,{fixed,false}))))
	      =:= 100),
     ?_assert(array:size(resize(array:set(7, undefined, new()))) =:= 0),
     ?_assert(array:size(resize(array:from_list([1,2,3,undefined])))
	      =:= 3),
     ?_assert(array:size(
		resize(array:from_orddict([{3,0},{17,0},{99,undefined}])))
	      =:= 18),
     ?_assertError(badarg, resize(foo, bad_argument))
    ].
-endif.


%% @doc Set entry `I' of the array to `Value'. If `I' is not a
%% nonnegative integer, or if the array has fixed size and `I' is larger
%% than the maximum index, the call fails with reason `badarg'.
%%
%% If the array does not have fixed size, and `I' is greater than
%% `size(Array)-1', the array will grow to size `I+1'.
%%
%% @see get/2
%% @see reset/2

-spec set(I :: array_indx(), Value :: Type, Array :: array(Type)) -> array(Type).

set(I, Value, #array{size = N, max = M, default = D, elements = E}=A)
  when is_integer(I), I >= 0 ->
    if I < N ->
	    A#array{elements = set_1(I, E, Value, D)};
       I < M ->
	    %% (note that this cannot happen if M == 0, since N >= 0)
	    A#array{size = I+1, elements = set_1(I, E, Value, D)};
       M > 0 ->
	    {E1, M1} = grow(I, E, M),
	    A#array{size = I+1, max = M1,
		    elements = set_1(I, E1, Value, D)};
       true ->
	    erlang:error(badarg)
    end;
set(_I, _V, _A) ->
    erlang:error(badarg).

%% See get_1/3 for details about switching and the NODEPATTERN macro.

set_1(I, E=?NODEPATTERN(S), X, D) ->
    I1 = I div S + 1,
    setelement(I1, E, set_1(I rem S, element(I1, E), X, D));
set_1(I, E, X, D) when is_integer(E) ->
    expand(I, E, X, D);
set_1(I, E, X, _D) ->
    setelement(I+1, E, X).


%% Enlarging the array upwards to accommodate an index `I'

grow(I, E, _M) when is_integer(E) ->
    M1 = find_max(I, E),
    {M1, M1};
grow(I, E, M) ->
    grow_1(I, E, M).

grow_1(I, E, M) when I >= M ->
    grow(I, setelement(1, ?NEW_NODE(M), E), ?extend(M));
grow_1(_I, E, M) ->
    {E, M}.


%% Insert an element in an unexpanded node, expanding it as necessary.

expand(I, S, X, D) when S > ?LEAFSIZE ->
    S1 = ?reduce(S),
    setelement(I div S1 + 1, ?NEW_NODE(S1),
	       expand(I rem S1, S1, X, D));
expand(I, _S, X, D) ->
    setelement(I+1, ?NEW_LEAF(D), X).


%% @doc Get the value of entry `I'. If `I' is not a nonnegative
%% integer, or if the array has fixed size and `I' is larger than the
%% maximum index, the call fails with reason `badarg'.
%%
%% If the array does not have fixed size, this function will return the
%% default value for any index `I' greater than `size(Array)-1'.
 
%% @see set/3

-spec get(I :: array_indx(), Array :: array(Type)) -> Value :: Type.

get(I, #array{size = N, max = M, elements = E, default = D})
  when is_integer(I), I >= 0 ->
    if I < N ->
	    get_1(I, E, D);
       M > 0 ->
	    D;
       true ->
	    erlang:error(badarg)
    end;
get(_I, _A) ->
    erlang:error(badarg).

%% The use of NODEPATTERN(S) to select the right clause is just a hack,
%% but it is the only way to get the maximum speed out of this loop
%% (using the Beam compiler in OTP 11).

get_1(I, E=?NODEPATTERN(S), D) ->
    get_1(I rem S, element(I div S + 1, E), D);
get_1(_I, E, D) when is_integer(E) ->
    D;
get_1(I, E, _D) ->
    element(I+1, E).


%% @doc Reset entry `I' to the default value for the array. 
%% If the value of entry `I' is the default value the array will be
%% returned unchanged. Reset will never change size of the array. 
%% Shrinking can be done explicitly by calling {@link resize/2}. 
%%
%% If `I' is not a nonnegative integer, or if the array has fixed size
%% and `I' is larger than the maximum index, the call fails with reason
%% `badarg'; cf. {@link set/3}
%%
%% @see new/2
%% @see set/3

%% TODO: a reset_range function

-spec reset(I :: array_indx(), Array :: array(Type)) -> array(Type).

reset(I, #array{size = N, max = M, default = D, elements = E}=A) 
    when is_integer(I), I >= 0 ->
    if I < N ->
	    try A#array{elements = reset_1(I, E, D)} 
	    catch throw:default -> A
	    end;
       M > 0 ->
	    A;
       true ->
	    erlang:error(badarg)
    end;
reset(_I, _A) ->
    erlang:error(badarg).

reset_1(I, E=?NODEPATTERN(S), D) ->
    I1 = I div S + 1,
    setelement(I1, E, reset_1(I rem S, element(I1, E), D));
reset_1(_I, E, _D) when is_integer(E) ->
    throw(default);
reset_1(I, E, D) ->
    Indx = I+1,
    case element(Indx, E) of
	D -> throw(default);
	_ -> setelement(I+1, E, D)
    end.


-ifdef(EUNIT).
set_get_test_() ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    [?_assert(array:get(0, new()) =:= undefined),
     ?_assert(array:get(1, new()) =:= undefined),
     ?_assert(array:get(99999, new()) =:= undefined),

     ?_assert(array:get(0, new(1)) =:= undefined),
     ?_assert(array:get(0, new(1,{default,0})) =:= 0),
     ?_assert(array:get(9, new(10)) =:= undefined),

     ?_assertError(badarg, array:get(0, new(fixed))),
     ?_assertError(badarg, array:get(1, new(1))),
     ?_assertError(badarg, array:get(-1, new(1))),
     ?_assertError(badarg, array:get(10, new(10))),
     ?_assertError(badarg, array:set(-1, foo, new(10))),
     ?_assertError(badarg, array:set(10, foo, no_array)),

     ?_assert(array:size(set(0, 17, new())) =:= 1),
     ?_assert(array:size(set(N1-1, 17, new())) =:= N1),
     ?_assert(array:size(set(0, 42, set(0, 17, new()))) =:= 1),
     ?_assert(array:size(set(9, 42, set(0, 17, new()))) =:= 10),

     ?_assert(array:get(0, set(0, 17, new())) =:= 17),
     ?_assert(array:get(0, set(1, 17, new())) =:= undefined),
     ?_assert(array:get(1, set(1, 17, new())) =:= 17),

     ?_assert(array:get(0, fix(set(0, 17, new()))) =:= 17),
     ?_assertError(badarg, array:get(1, fix(set(0, 17, new())))),

     ?_assert(array:get(N1-2, set(N1-1, 17, new())) =:= undefined),
     ?_assert(array:get(N1-1, set(N1-1, 17, new())) =:= 17),
     ?_assertError(badarg, array:get(N1, fix(set(N1-1, 17, new())))),

     ?_assert(array:get(0, set(0, 42, set(0, 17, new()))) =:= 42),

     ?_assertError(badarg, array:get(0, reset(11, new([{size,10}])))),
     ?_assertError(badarg, array:get(0, reset(-1, new([{size,10}])))),
     ?_assert(array:get(0, reset(0,  new())) =:= undefined),
     ?_assert(array:get(0, reset(0,  set(0,  17, new()))) =:= undefined),
     ?_assert(array:get(0, reset(9,  set(9,  17, new()))) =:= undefined),
     ?_assert(array:get(0, reset(11, set(11, 17, new()))) =:= undefined),
     ?_assert(array:get(0, reset(11, set(12, 17, new()))) =:= undefined),
     ?_assert(array:get(0, reset(1,  set(12, 17, new()))) =:= undefined),
     ?_assert(array:get(0, reset(11, new())) =:= undefined),
     ?_assert(array:get(0, reset(0,  set(0,  17, new({default,42})))) =:= 42),
     ?_assert(array:get(0, reset(0,  new({default,42}))) =:= 42)
    ].
-endif.


%% @doc Converts the array to a list.
%%
%% @see from_list/2
%% @see sparse_to_list/1

-spec to_list(Array :: array(Type)) -> list(Value :: Type).

to_list(#array{size = 0}) ->
    [];
to_list(#array{size = N, elements = E, default = D}) ->
    to_list_1(E, D, N - 1);
to_list(_) ->
    erlang:error(badarg).

%% this part handles the rightmost subtrees

to_list_1(E=?NODEPATTERN(S), D, I) ->
    N = I div S,
    to_list_3(N, D, to_list_1(element(N+1, E), D, I rem S), E);
to_list_1(E, D, I) when is_integer(E) ->
    push(I+1, D, []);
to_list_1(E, _D, I) ->
    push_tuple(I+1, E, []).

%% this part handles full trees only

to_list_2(E=?NODEPATTERN(_S), D, L) ->
    to_list_3(?NODESIZE, D, L, E);
to_list_2(E, D, L) when is_integer(E) ->
    push(E, D, L);
to_list_2(E, _D, L) ->
    push_tuple(?LEAFSIZE, E, L).

to_list_3(0, _D, L, _E) ->
    L;
to_list_3(N, D, L, E) ->
    to_list_3(N-1, D, to_list_2(element(N, E), D, L), E).

push(0, _E, L) ->
    L;
push(N, E, L) ->
    push(N - 1, E, [E | L]).

push_tuple(0, _T, L) ->
    L;
push_tuple(N, T, L) ->
    push_tuple(N - 1, T, [element(N, T) | L]).


-ifdef(EUNIT).
to_list_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] =:= to_list(new())),
     ?_assert([undefined] =:= to_list(new(1))),
     ?_assert([undefined,undefined] =:= to_list(new(2))),
     ?_assert(lists:duplicate(N0,0) =:= to_list(new(N0,{default,0}))),
     ?_assert(lists:duplicate(N0+1,1) =:= to_list(new(N0+1,{default,1}))),
     ?_assert(lists:duplicate(N0+2,2) =:= to_list(new(N0+2,{default,2}))),
     ?_assert(lists:duplicate(666,6) =:= to_list(new(666,{default,6}))),
     ?_assert([1,2,3] =:= to_list(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([3,2,1] =:= to_list(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([1|lists:duplicate(N0-2,0)++[1]] =:= 
	      to_list(set(N0-1,1,set(0,1,new({default,0}))))),
     ?_assert([1|lists:duplicate(N0-1,0)++[1]] =:= 
	      to_list(set(N0,1,set(0,1,new({default,0}))))),
     ?_assert([1|lists:duplicate(N0,0)++[1]] =:= 
	      to_list(set(N0+1,1,set(0,1,new({default,0}))))),
     ?_assert([1|lists:duplicate(N0*3,0)++[1]] =:= 
	      to_list(set((N0*3)+1,1,set(0,1,new({default,0}))))),
     ?_assertError(badarg, to_list(no_array))
    ].
-endif.


%% @doc Converts the array to a list, skipping default-valued entries.
%%
%% @see to_list/1

-spec sparse_to_list(Array :: array(Type)) -> list(Value :: Type).

sparse_to_list(#array{size = 0}) ->
    [];
sparse_to_list(#array{size = N, elements = E, default = D}) ->
    sparse_to_list_1(E, D, N - 1);
sparse_to_list(_) ->
    erlang:error(badarg).

%% see to_list/1 for details

sparse_to_list_1(E=?NODEPATTERN(S), D, I) ->
    N = I div S,
    sparse_to_list_3(N, D,
		     sparse_to_list_1(element(N+1, E), D, I rem S),
		     E);
sparse_to_list_1(E, _D, _I) when is_integer(E) ->
    [];
sparse_to_list_1(E, D, I) ->
    sparse_push_tuple(I+1, D, E, []).

sparse_to_list_2(E=?NODEPATTERN(_S), D, L) ->
    sparse_to_list_3(?NODESIZE, D, L, E);
sparse_to_list_2(E, _D, L) when is_integer(E) ->
    L;
sparse_to_list_2(E, D, L) ->
    sparse_push_tuple(?LEAFSIZE, D, E, L).

sparse_to_list_3(0, _D, L, _E) ->
    L;
sparse_to_list_3(N, D, L, E) ->
    sparse_to_list_3(N-1, D, sparse_to_list_2(element(N, E), D, L), E).

sparse_push_tuple(0, _D, _T, L) ->
    L;
sparse_push_tuple(N, D, T, L) ->
    case element(N, T) of
	D -> sparse_push_tuple(N - 1, D, T, L);
	E -> sparse_push_tuple(N - 1, D, T, [E | L])
    end.


-ifdef(EUNIT).
sparse_to_list_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] =:= sparse_to_list(new())),
     ?_assert([] =:= sparse_to_list(new(1))),
     ?_assert([] =:= sparse_to_list(new(1,{default,0}))),
     ?_assert([] =:= sparse_to_list(new(2))),
     ?_assert([] =:= sparse_to_list(new(2,{default,0}))),
     ?_assert([] =:= sparse_to_list(new(N0,{default,0}))),
     ?_assert([] =:= sparse_to_list(new(N0+1,{default,1}))),
     ?_assert([] =:= sparse_to_list(new(N0+2,{default,2}))),
     ?_assert([] =:= sparse_to_list(new(666,{default,6}))),
     ?_assert([1,2,3] =:= sparse_to_list(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([3,2,1] =:= sparse_to_list(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([0,1] =:= sparse_to_list(set(N0-1,1,set(0,0,new())))),
     ?_assert([0,1] =:= sparse_to_list(set(N0,1,set(0,0,new())))),
     ?_assert([0,1] =:= sparse_to_list(set(N0+1,1,set(0,0,new())))),
     ?_assert([0,1,2] =:= sparse_to_list(set(N0*10+1,2,set(N0*2+1,1,set(0,0,new()))))),
     ?_assertError(badarg, sparse_to_list(no_array))
    ].
-endif.


%% @equiv from_list(List, undefined)

-spec from_list(List :: list(Value :: Type)) -> array(Type).

from_list(List) ->
    from_list(List, undefined).

%% @doc Convert a list to an extendible array. `Default' is used as the value
%% for uninitialized entries of the array. If `List' is not a proper list,
%% the call fails with reason `badarg'.
%%
%% @see new/2
%% @see to_list/1

-spec from_list(List :: list(Value :: Type), Default :: term()) -> array(Type).

from_list([], Default) ->
    new({default,Default});
from_list(List, Default) when is_list(List) ->
    {E, N, M} = from_list_1(?LEAFSIZE, List, Default, 0, [], []),
    #array{size = N, max = M, default = Default, elements = E};
from_list(_, _) ->
    erlang:error(badarg).

%% Note: A cleaner but slower algorithm is to first take the length of
%% the list and compute the max size of the final tree, and then
%% decompose the list. The below algorithm is almost twice as fast,
%% however.

%% Building the leaf nodes (padding the last one as necessary) and
%% counting the total number of elements.
from_list_1(0, Xs, D, N, As, Es) ->
    E = list_to_tuple(lists:reverse(As)),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?LEAFSIZE};
		_ ->
		    from_list_2_0(N, [E | Es], ?LEAFSIZE)
	    end;
	[_|_] ->
	    from_list_1(?LEAFSIZE, Xs, D, N, [], [E | Es]);
	_ ->
	    erlang:error(badarg)
    end;
from_list_1(I, Xs, D, N, As, Es) ->
    case Xs of
	[X | Xs1] ->
	    from_list_1(I-1, Xs1, D, N+1, [X | As], Es);
	_ ->
	    from_list_1(I-1, Xs, D, N, [D | As], Es)
    end.

%% Building the internal nodes (note that the input is reversed).
from_list_2_0(N, Es, S) ->
    from_list_2(?NODESIZE, pad((N-1) div S + 1, ?NODESIZE, S, Es),
		S, N, [S], []).

from_list_2(0, Xs, S, N, As, Es) ->
    E = list_to_tuple(As),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?extend(S)};
		_ ->
		    from_list_2_0(N, lists:reverse([E | Es]),
				  ?extend(S))
	    end;
	_ ->
	    from_list_2(?NODESIZE, Xs, S, N, [S], [E | Es])
    end;
from_list_2(I, [X | Xs], S, N, As, Es) ->
    from_list_2(I-1, Xs, S, N, [X | As], Es).


%% left-padding a list Es with elements P to the nearest multiple of K
%% elements from N (adding 0 to K-1 elements).
pad(N, K, P, Es) ->
    push((K - (N rem K)) rem K, P, Es).


-ifdef(EUNIT).
from_list_test_() ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?_assert(array:size(from_list([])) =:= 0),
     ?_assert(array:is_fix(from_list([])) =:= false),
     ?_assert(array:size(from_list([undefined])) =:= 1),
     ?_assert(array:is_fix(from_list([undefined])) =:= false),
     ?_assert(array:size(from_list(lists:seq(1,N1))) =:= N1),
     ?_assert(to_list(from_list(lists:seq(1,N0))) =:= lists:seq(1,N0)),
     ?_assert(to_list(from_list(lists:seq(1,N0+1))) =:= lists:seq(1,N0+1)),
     ?_assert(to_list(from_list(lists:seq(1,N0+2))) =:= lists:seq(1,N0+2)),
     ?_assert(to_list(from_list(lists:seq(1,N2))) =:= lists:seq(1,N2)),
     ?_assert(to_list(from_list(lists:seq(1,N2+1))) =:= lists:seq(1,N2+1)),
     ?_assert(to_list(from_list(lists:seq(0,N3))) =:= lists:seq(0,N3)),
     ?_assert(to_list(from_list(lists:seq(0,N4))) =:= lists:seq(0,N4)),
     ?_assertError(badarg, from_list([a,b,a,c|d])),
     ?_assertError(badarg, from_list(no_array))     
    ].
-endif.


%% @doc Convert the array to an ordered list of pairs `{Index, Value}'.
%%
%% @see from_orddict/2
%% @see sparse_to_orddict/1

-spec to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).

to_orddict(#array{size = 0}) ->
    [];
to_orddict(#array{size = N, elements = E, default = D}) ->
    I = N - 1,
    to_orddict_1(E, I, D, I);
to_orddict(_) ->
    erlang:error(badarg).

%% see to_list/1 for comparison

to_orddict_1(E=?NODEPATTERN(S), R, D, I) ->
    N = I div S,
    I1 = I rem S,
    to_orddict_3(N, R - I1 - 1, D,
 		 to_orddict_1(element(N+1, E), R, D, I1),
 		 E, S);
to_orddict_1(E, R, D, I) when is_integer(E) ->
    push_pairs(I+1, R, D, []);
to_orddict_1(E, R, _D, I) ->
    push_tuple_pairs(I+1, R, E, []).

to_orddict_2(E=?NODEPATTERN(S), R, D, L) ->
    to_orddict_3(?NODESIZE, R, D, L, E, S);
to_orddict_2(E, R, D, L) when is_integer(E) ->
    push_pairs(E, R, D, L);
to_orddict_2(E, R, _D, L) ->
    push_tuple_pairs(?LEAFSIZE, R, E, L).

to_orddict_3(0, _R, _D, L, _E, _S) -> %% when is_integer(R) ->
    L;
to_orddict_3(N, R, D, L, E, S) ->
    to_orddict_3(N-1, R - S, D,
 		 to_orddict_2(element(N, E), R, D, L),
 		 E, S).

-spec push_pairs(non_neg_integer(), array_indx(), term(), indx_pairs(Type)) ->
	  indx_pairs(Type).

push_pairs(0, _I, _E, L) ->
    L;
push_pairs(N, I, E, L) ->
    push_pairs(N-1, I-1, E, [{I, E} | L]).

-spec push_tuple_pairs(non_neg_integer(), array_indx(), term(), indx_pairs(Type)) ->
	  indx_pairs(Type).

push_tuple_pairs(0, _I, _T, L) ->
    L;
push_tuple_pairs(N, I, T, L) ->
    push_tuple_pairs(N-1, I-1, T, [{I, element(N, T)} | L]).


-ifdef(EUNIT).
to_orddict_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] =:= to_orddict(new())),
     ?_assert([{0,undefined}] =:= to_orddict(new(1))),
     ?_assert([{0,undefined},{1,undefined}] =:= to_orddict(new(2))),
     ?_assert([{N,0}||N<-lists:seq(0,N0-1)]
	      =:= to_orddict(new(N0,{default,0}))),
     ?_assert([{N,1}||N<-lists:seq(0,N0)]
	      =:= to_orddict(new(N0+1,{default,1}))),
     ?_assert([{N,2}||N<-lists:seq(0,N0+1)]
	      =:= to_orddict(new(N0+2,{default,2}))),
     ?_assert([{N,6}||N<-lists:seq(0,665)]
	      =:= to_orddict(new(666,{default,6}))),
     ?_assert([{0,1},{1,2},{2,3}] =:=
	      to_orddict(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([{0,3},{1,2},{2,1}] =:=
	      to_orddict(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([{0,1}|[{N,0}||N<-lists:seq(1,N0-2)]++[{N0-1,1}]]
	      =:= to_orddict(set(N0-1,1,set(0,1,new({default,0}))))),
     ?_assert([{0,1}|[{N,0}||N<-lists:seq(1,N0-1)]++[{N0,1}]]
	      =:= to_orddict(set(N0,1,set(0,1,new({default,0}))))),
     ?_assert([{0,1}|[{N,0}||N<-lists:seq(1,N0)]++[{N0+1,1}]]
	      =:= to_orddict(set(N0+1,1,set(0,1,new({default,0}))))),
     ?_assert([{0,0} | [{N,undefined}||N<-lists:seq(1,N0*2)]] ++ 
	      [{N0*2+1,1} | [{N,undefined}||N<-lists:seq(N0*2+2,N0*10)]] ++
	      [{N0*10+1,2}] =:= 
	      to_orddict(set(N0*10+1,2,set(N0*2+1,1,set(0,0,new()))))),
     ?_assertError(badarg, to_orddict(no_array))     
    ].
-endif.


%% @doc Convert the array to an ordered list of pairs `{Index, Value}',
%% skipping default-valued entries.
%% 
%% @see to_orddict/1

-spec sparse_to_orddict(Array :: array(Type)) -> indx_pairs(Value :: Type).

sparse_to_orddict(#array{size = 0}) ->
    [];
sparse_to_orddict(#array{size = N, elements = E, default = D}) ->
    I = N - 1,
    sparse_to_orddict_1(E, I, D, I);
sparse_to_orddict(_) ->
    erlang:error(badarg).

%% see to_orddict/1 for details

sparse_to_orddict_1(E=?NODEPATTERN(S), R, D, I) ->
    N = I div S,
    I1 = I rem S,
    sparse_to_orddict_3(N, R - I1 - 1, D,
 		 sparse_to_orddict_1(element(N+1, E), R, D, I1),
 		 E, S);
sparse_to_orddict_1(E, _R, _D, _I) when is_integer(E) ->
    [];
sparse_to_orddict_1(E, R, D, I) ->
    sparse_push_tuple_pairs(I+1, R, D, E, []).

sparse_to_orddict_2(E=?NODEPATTERN(S), R, D, L) ->
    sparse_to_orddict_3(?NODESIZE, R, D, L, E, S);
sparse_to_orddict_2(E, _R, _D, L) when is_integer(E) ->
    L;
sparse_to_orddict_2(E, R, D, L) ->
    sparse_push_tuple_pairs(?LEAFSIZE, R, D, E, L).

sparse_to_orddict_3(0, _R, _D, L, _E, _S) -> % when is_integer(R) ->
    L;
sparse_to_orddict_3(N, R, D, L, E, S) ->
    sparse_to_orddict_3(N-1, R - S, D,
 		 sparse_to_orddict_2(element(N, E), R, D, L),
 		 E, S).

-spec sparse_push_tuple_pairs(non_neg_integer(), array_indx(),
			      _, _, indx_pairs(Type)) -> indx_pairs(Type).

sparse_push_tuple_pairs(0, _I, _D, _T, L) ->
    L;
sparse_push_tuple_pairs(N, I, D, T, L) ->
    case element(N, T) of
	D -> sparse_push_tuple_pairs(N-1, I-1, D, T, L);
	E -> sparse_push_tuple_pairs(N-1, I-1, D, T, [{I, E} | L])
    end.


-ifdef(EUNIT).
sparse_to_orddict_test_() ->
    N0 = ?LEAFSIZE,
    [?_assert([] =:= sparse_to_orddict(new())),
     ?_assert([] =:= sparse_to_orddict(new(1))),
     ?_assert([] =:= sparse_to_orddict(new(1,{default,0}))),
     ?_assert([] =:= sparse_to_orddict(new(2))),
     ?_assert([] =:= sparse_to_orddict(new(2,{default,0}))),
     ?_assert([] =:= sparse_to_orddict(new(N0,{default,0}))),
     ?_assert([] =:= sparse_to_orddict(new(N0+1,{default,1}))),
     ?_assert([] =:= sparse_to_orddict(new(N0+2,{default,2}))),
     ?_assert([] =:= sparse_to_orddict(new(666,{default,6}))),
     ?_assert([{0,1},{1,2},{2,3}] =:=
	      sparse_to_orddict(set(2,3,set(1,2,set(0,1,new()))))),
     ?_assert([{0,3},{1,2},{2,1}] =:=
	      sparse_to_orddict(set(0,3,set(1,2,set(2,1,new()))))),
     ?_assert([{0,1},{N0-1,1}] =:=
	      sparse_to_orddict(set(N0-1,1,set(0,1,new({default,0}))))),
     ?_assert([{0,1},{N0,1}] =:=
	      sparse_to_orddict(set(N0,1,set(0,1,new({default,0}))))),
     ?_assert([{0,1},{N0+1,1}] =:=
	      sparse_to_orddict(set(N0+1,1,set(0,1,new({default,0}))))),
     ?_assert([{0,0},{N0*2+1,1},{N0*10+1,2}] =:= 
	      sparse_to_orddict(set(N0*10+1,2,set(N0*2+1,1,set(0,0,new()))))),
     ?_assertError(badarg, sparse_to_orddict(no_array))     
    ].
-endif.


%% @equiv from_orddict(Orddict, undefined)

-spec from_orddict(Orddict :: indx_pairs(Value :: Type)) -> array(Type).

from_orddict(Orddict) ->
    from_orddict(Orddict, undefined).

%% @doc Convert an ordered list of pairs `{Index, Value}' to a
%% corresponding extendible array. `Default' is used as the value for
%% uninitialized entries of the array. If `List' is not a proper,
%% ordered list of pairs whose first elements are nonnegative
%% integers, the call fails with reason `badarg'.
%%
%% @see new/2
%% @see to_orddict/1

-spec from_orddict(Orddict :: indx_pairs(Value :: Type), Default :: Type) ->
                          array(Type).

from_orddict([], Default) ->
    new({default,Default});
from_orddict(List, Default) when is_list(List) ->
    {E, N, M} = from_orddict_0(List, 0, ?LEAFSIZE, Default, []),
    #array{size = N, max = M, default = Default, elements = E};
from_orddict(_, _) ->
    erlang:error(badarg).

%% 2 pass implementation, first pass builds the needed leaf nodes
%% and adds hole sizes.
%% (inserts default elements for missing list entries in the leafs 
%%  and pads the last tuple if necessary).
%% Second pass builds the tree from the leafs and the holes.
%%
%% Doesn't build/expand unnecessary leaf nodes which costs memory
%% and time for sparse arrays.

from_orddict_0([], N, _Max, _D, Es) ->
    %% Finished, build the resulting tree
    case Es of
	[E] -> 
	    {E, N, ?LEAFSIZE};
	_ -> 
	    collect_leafs(N, Es, ?LEAFSIZE)
    end;

from_orddict_0(Xs=[{Ix1, _}|_], Ix, Max0, D, Es0) 
  when Ix1 > Max0, is_integer(Ix1) ->
    %% We have a hole larger than a leaf
    Hole = Ix1-Ix,
    Step = Hole - (Hole rem ?LEAFSIZE),
    Next = Ix+Step,
    from_orddict_0(Xs, Next, Next+?LEAFSIZE, D, [Step|Es0]);
from_orddict_0(Xs0=[{_, _}|_], Ix0, Max, D, Es) ->
    %% Fill a leaf 
    {Xs,E,Ix} = from_orddict_1(Ix0, Max, Xs0, Ix0, D, []),
    from_orddict_0(Xs, Ix, Ix+?LEAFSIZE, D, [E|Es]);
from_orddict_0(Xs, _, _, _,_) ->
    erlang:error({badarg, Xs}).

from_orddict_1(Ix, Ix, Xs, N, _D, As) ->
    %% Leaf is full
    E = list_to_tuple(lists:reverse(As)),
    {Xs, E, N};
from_orddict_1(Ix, Max, Xs, N0, D, As) ->
    case Xs of
	[{Ix, Val} | Xs1] ->
	    N = Ix+1,
	    from_orddict_1(N, Max, Xs1, N, D, [Val | As]);
	[{Ix1, _} | _] when is_integer(Ix1), Ix1 > Ix ->
	    N = Ix+1,
	    from_orddict_1(N, Max, Xs, N, D, [D | As]);
	[_ | _] ->
	    erlang:error({badarg, Xs});
	_ ->
	    from_orddict_1(Ix+1, Max, Xs, N0, D, [D | As])
    end.

%% Es is reversed i.e. starting from the largest leafs
collect_leafs(N, Es, S) -> 
    I = (N-1) div S + 1,
    Pad = ((?NODESIZE - (I rem ?NODESIZE)) rem ?NODESIZE) * S,
    case Pad of
	0 -> 
	    collect_leafs(?NODESIZE, Es, S, N, [S], []);
	_ ->  %% Pad the end
	    collect_leafs(?NODESIZE, [Pad|Es], S, N, [S], [])
    end.

collect_leafs(0, Xs, S, N, As, Es) ->
    E = list_to_tuple(As),
    case Xs of
	[] ->
	    case Es of
		[] ->
		    {E, N, ?extend(S)};
		_ ->
		    collect_leafs(N, lists:reverse([E | Es]),
			       ?extend(S))
	    end;
	_ ->
	    collect_leafs(?NODESIZE, Xs, S, N, [S], [E | Es])		
    end;
collect_leafs(I, [X | Xs], S, N, As0, Es0) 
  when is_integer(X) ->
    %% A hole, pad accordingly.
    Step0 = (X div S),
    if 
	Step0 < I ->
	    As = push(Step0, S, As0),
	    collect_leafs(I-Step0, Xs, S, N, As, Es0);
	I =:= ?NODESIZE ->
	    Step = Step0 rem ?NODESIZE,
	    As = push(Step, S, As0),
	    collect_leafs(I-Step, Xs, S, N, As, [X|Es0]);
	I =:= Step0 ->
	    As = push(I, S, As0),
	    collect_leafs(0, Xs, S, N, As, Es0);
	true ->
	    As = push(I, S, As0),
	    Step = Step0 - I,
	    collect_leafs(0, [Step*S|Xs], S, N, As, Es0)
    end;
collect_leafs(I, [X | Xs], S, N, As, Es) ->
    collect_leafs(I-1, Xs, S, N, [X | As], Es);
collect_leafs(?NODESIZE, [], S, N, [_], Es) ->
    collect_leafs(N, lists:reverse(Es), ?extend(S)).

-ifdef(EUNIT).
from_orddict_test_() ->
    N0 = ?LEAFSIZE,
    N1 = ?NODESIZE*N0,
    N2 = ?NODESIZE*N1,
    N3 = ?NODESIZE*N2,
    N4 = ?NODESIZE*N3,
    [?_assert(array:size(from_orddict([])) =:= 0),
     ?_assert(array:is_fix(from_orddict([])) =:= false),
     ?_assert(array:size(from_orddict([{0,undefined}])) =:= 1),
     ?_assert(array:is_fix(from_orddict([{0,undefined}])) =:= false),
     ?_assert(array:size(from_orddict([{N0-1,undefined}])) =:= N0),
     ?_assert(array:size(from_orddict([{N,0}||N<-lists:seq(0,N1-1)]))
	      =:= N1),
     ?_assertError({badarg,_}, from_orddict([foo])),
     ?_assertError({badarg,_}, from_orddict([{200,foo},{1,bar}])),
     ?_assertError({badarg,_}, from_orddict([{N,0}||N<-lists:seq(0,N0-1)] ++ not_a_list)),
     ?_assertError(badarg, from_orddict(no_array)),


     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N0-1)],
		   L =:= to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N0)],
		   L =:= to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N2-1)],
		   L =:= to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N2)],
		   L =:= to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N3-1)],
		   L =:= to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N,0}||N<-lists:seq(0,N4-1)],
		   L =:= to_orddict(from_orddict(L)))),

     %% Hole in the begining
     ?_assert(?LET(L, [{0,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N0,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N3,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N4,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N0-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N1-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N3-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{N4-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),

     %% Hole in middle 
     
     ?_assert(?LET(L, [{0,0},{N0,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N3,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N4,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N0-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N1-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N3-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L)))),
     ?_assert(?LET(L, [{0,0},{N4-1,0}],
		   L =:= sparse_to_orddict(from_orddict(L))))
     
    ].
-endif.


%%    Function = (Index::integer(), Value::term()) -> term()
%% @doc Map the given function onto each element of the array. The
%% elements are visited in order from the lowest index to the highest.
%% If `Function' is not a function, the call fails with reason `badarg'.
%%
%% @see foldl/3
%% @see foldr/3
%% @see sparse_map/2

-spec map(Function, Array :: array(Type1)) -> array(Type2) when
      Function :: fun((Index :: array_indx(), Type1) -> Type2).

map(Function, Array=#array{size = N, elements = E, default = D})
  when is_function(Function, 2) ->
    if N > 0 ->
	    A = Array#array{elements = []}, % kill reference, for GC
	    A#array{elements = map_1(N-1, E, 0, Function, D)};
       true ->
	    Array
    end;
map(_, _) ->
    erlang:error(badarg).

%% It might be simpler to traverse the array right-to-left, as done e.g.
%% in the to_orddict/1 function, but it is better to guarantee
%% left-to-right application over the elements - that is more likely to
%% be a generally useful property.

map_1(N, E=?NODEPATTERN(S), Ix, F, D) ->
    list_to_tuple(lists:reverse([S | map_2(1, E, Ix, F, D, [],
					   N div S + 1, N rem S, S)]));
map_1(N, E, Ix, F, D) when is_integer(E) ->
    map_1(N, unfold(E, D), Ix, F, D);
map_1(N, E, Ix, F, D) ->
    list_to_tuple(lists:reverse(map_3(1, E, Ix, F, D, N+1, []))).

map_2(I, E, Ix, F, D, L, I, R, _S) ->
    map_2_1(I+1, E, [map_1(R, element(I, E), Ix, F, D) | L]);
map_2(I, E, Ix, F, D, L, N, R, S) ->
    map_2(I+1, E, Ix + S, F, D, 
	  [map_1(S-1, element(I, E), Ix, F, D) | L],
	  N, R, S).

map_2_1(I, E, L) when I =< ?NODESIZE ->
    map_2_1(I+1, E, [element(I, E) | L]);
map_2_1(_I, _E, L) ->
    L.

-spec map_3(pos_integer(), _, array_indx(),
	    fun((array_indx(),_) -> _), _, non_neg_integer(), [X]) -> [X].

map_3(I, E, Ix, F, D, N, L) when I =< N ->
    map_3(I+1, E, Ix+1, F, D, N, [F(Ix, element(I, E)) | L]);
map_3(I, E, Ix, F, D, N, L) when I =< ?LEAFSIZE ->
    map_3(I+1, E, Ix+1, F, D, N, [D | L]);
map_3(_I, _E, _Ix, _F, _D, _N, L) ->
    L.


unfold(S, _D) when S > ?LEAFSIZE ->
    ?NEW_NODE(?reduce(S));
unfold(_S, D) ->
    ?NEW_LEAF(D).
    

-ifdef(EUNIT).
map_test_() ->
    N0 = ?LEAFSIZE,
    Id = fun (_,X) -> X end,
    Plus = fun(N) -> fun (_,X) -> X+N end end,
    Default = fun(_K,undefined) ->  no_value;
		 (K,V) -> K+V
	      end,
    [?_assertError(badarg, map([], new())),
     ?_assertError(badarg, map([], new(10))),
     ?_assert(to_list(map(Id, new())) =:= []),
     ?_assert(to_list(map(Id, new(1))) =:= [undefined]),
     ?_assert(to_list(map(Id, new(5,{default,0}))) =:= [0,0,0,0,0]),
     ?_assert(to_list(map(Id, from_list([1,2,3,4]))) =:= [1,2,3,4]),
     ?_assert(to_list(map(Plus(1), from_list([0,1,2,3]))) =:= [1,2,3,4]),
     ?_assert(to_list(map(Plus(-1), from_list(lists:seq(1,11))))
	      =:= lists:seq(0,10)),
     ?_assert(to_list(map(Plus(11), from_list(lists:seq(0,99999))))
	      =:= lists:seq(11,100010)),
     ?_assert([{0,0},{N0*2+1,N0*2+1+1},{N0*100+1,N0*100+1+2}] =:= 
	      sparse_to_orddict((map(Default, 
				     set(N0*100+1,2,
					 set(N0*2+1,1,
					     set(0,0,new())))))#array{default = no_value}))
    ].
-endif.


%%    Function = (Index::integer(), Value::term()) -> term()
%% @doc Map the given function onto each element of the array, skipping
%% default-valued entries. The elements are visited in order from the
%% lowest index to the highest. If `Function' is not a function, the
%% call fails with reason `badarg'.
%%
%% @see map/2

-spec sparse_map(Function, Array :: array(Type1)) -> array(Type2) when
      Function :: fun((Index :: array_indx(), Type1) -> Type2).

sparse_map(Function, Array=#array{size = N, elements = E, default = D})
  when is_function(Function, 2) ->
    if N > 0 ->
	    A = Array#array{elements = []}, % kill reference, for GC
	    A#array{elements = sparse_map_1(N-1, E, 0, Function, D)};
       true ->
	    Array
    end;
sparse_map(_, _) ->
    erlang:error(badarg).

%% see map/2 for details
%% TODO: we can probably optimize away the use of div/rem here

sparse_map_1(N, E=?NODEPATTERN(S), Ix, F, D) ->
    list_to_tuple(lists:reverse([S | sparse_map_2(1, E, Ix, F, D, [],
						  N div S + 1,
						  N rem S, S)]));
sparse_map_1(_N, E, _Ix, _F, _D) when is_integer(E) ->
    E;
sparse_map_1(_N, E, Ix, F, D) ->
    list_to_tuple(lists:reverse(sparse_map_3(1, E, Ix, F, D, []))).

sparse_map_2(I, E, Ix, F, D, L, I, R, _S) ->
    sparse_map_2_1(I+1, E,
		   [sparse_map_1(R, element(I, E), Ix, F, D) | L]);
sparse_map_2(I, E, Ix, F, D, L, N, R, S) ->
    sparse_map_2(I+1, E, Ix + S, F, D, 
	  [sparse_map_1(S-1, element(I, E), Ix, F, D) | L],
	  N, R, S).

sparse_map_2_1(I, E, L) when I =< ?NODESIZE ->
    sparse_map_2_1(I+1, E, [element(I, E) | L]);
sparse_map_2_1(_I, _E, L) ->
    L.

-spec sparse_map_3(pos_integer(), _, array_indx(),
		   fun((array_indx(),_) -> _), _, [X]) -> [X].

sparse_map_3(I, T, Ix, F, D, L) when I =< ?LEAFSIZE ->
    case element(I, T) of
	D -> sparse_map_3(I+1, T, Ix+1, F, D, [D | L]);
	E -> sparse_map_3(I+1, T, Ix+1, F, D, [F(Ix, E) | L])
    end;
sparse_map_3(_I, _E, _Ix, _F, _D, L) ->
    L.


-ifdef(EUNIT).
sparse_map_test_() ->
    N0 = ?LEAFSIZE,
    Id = fun (_,X) -> X end,
    Plus = fun(N) -> fun (_,X) -> X+N end end,
    KeyPlus = fun (K,X) -> K+X end,
    [?_assertError(badarg, sparse_map([], new())),
     ?_assertError(badarg, sparse_map([], new(10))),
     ?_assert(to_list(sparse_map(Id, new())) =:= []),
     ?_assert(to_list(sparse_map(Id, new(1))) =:= [undefined]),
     ?_assert(to_list(sparse_map(Id, new(5,{default,0}))) =:= [0,0,0,0,0]),
     ?_assert(to_list(sparse_map(Id, from_list([1,2,3,4]))) =:= [1,2,3,4]),
     ?_assert(to_list(sparse_map(Plus(1), from_list([0,1,2,3])))
	      =:= [1,2,3,4]),
     ?_assert(to_list(sparse_map(Plus(-1), from_list(lists:seq(1,11))))
	      =:= lists:seq(0,10)),
     ?_assert(to_list(sparse_map(Plus(11), from_list(lists:seq(0,99999))))
	      =:= lists:seq(11,100010)),
     ?_assert(to_list(sparse_map(Plus(1), set(1,1,new({default,0}))))
	      =:= [0,2]),
     ?_assert(to_list(sparse_map(Plus(1),
				 set(3,4,set(0,1,new({default,0})))))
	      =:= [2,0,0,5]),
     ?_assert(to_list(sparse_map(Plus(1),
				 set(9,9,set(1,1,new({default,0})))))
	      =:= [0,2,0,0,0,0,0,0,0,10]),
     ?_assert([{0,0},{N0*2+1,N0*2+1+1},{N0*100+1,N0*100+1+2}] =:= 
	      sparse_to_orddict(sparse_map(KeyPlus, 
					   set(N0*100+1,2,
					       set(N0*2+1,1,
						   set(0,0,new()))))))

    ].
-endif.


%% @doc Fold the elements of the array using the given function and
%% initial accumulator value. The elements are visited in order from the
%% lowest index to the highest. If `Function' is not a function, the
%% call fails with reason `badarg'.
%%
%% @see foldr/3
%% @see map/2
%% @see sparse_foldl/3

-spec foldl(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

foldl(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function, 3) ->
    if N > 0 ->
	    foldl_1(N-1, E, A, 0, Function, D);
       true ->
	    A
    end;
foldl(_, _, _) ->
    erlang:error(badarg).

foldl_1(N, E=?NODEPATTERN(S), A, Ix, F, D) ->
    foldl_2(1, E, A, Ix, F, D, N div S + 1, N rem S, S);
foldl_1(N, E, A, Ix, F, D) when is_integer(E) ->
    foldl_1(N, unfold(E, D), A, Ix, F, D);
foldl_1(N, E, A, Ix, F, _D) ->
    foldl_3(1, E, A, Ix, F, N+1).

foldl_2(I, E, A, Ix, F, D, I, R, _S) ->
    foldl_1(R, element(I, E), A, Ix, F, D);
foldl_2(I, E, A, Ix, F, D, N, R, S) ->
    foldl_2(I+1, E, foldl_1(S-1, element(I, E), A, Ix, F, D),
	    Ix + S, F, D, N, R, S).

-spec foldl_3(pos_integer(), _, A, array_indx(),
	      fun((array_indx, _, A) -> B), integer()) -> B.

foldl_3(I, E, A, Ix, F, N) when I =< N ->
    foldl_3(I+1, E, F(Ix, element(I, E), A), Ix+1, F, N);
foldl_3(_I, _E, A, _Ix, _F, _N) ->
    A.


-ifdef(EUNIT).
foldl_test_() ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    Reverse = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
	      (K,X,{C,L}) -> {C,[K+X|L]} 
	   end,
    [?_assertError(badarg, foldl([], 0, new())),
     ?_assertError(badarg, foldl([], 0, new(10))),
     ?_assert(foldl(Count, 0, new()) =:= 0),
     ?_assert(foldl(Count, 0, new(1)) =:= 1),
     ?_assert(foldl(Count, 0, new(10)) =:= 10),
     ?_assert(foldl(Count, 0, from_list([1,2,3,4])) =:= 4),
     ?_assert(foldl(Count, 10, from_list([0,1,2,3,4,5,6,7,8,9])) =:= 20),
     ?_assert(foldl(Count, 1000, from_list(lists:seq(0,999))) =:= 2000),
     ?_assert(foldl(Sum, 0, from_list(lists:seq(0,10))) =:= 55),
     ?_assert(foldl(Reverse, [], from_list(lists:seq(0,1000)))
	      =:= lists:reverse(lists:seq(0,1000))),
     ?_assert({999,[N0*100+1+2,N0*2+1+1,0]} =:= 
	      foldl(Vals, {0,[]}, 
		    set(N0*100+1,2,
			set(N0*2+1,1,
			    set(0,0,new())))))
     
    ].
-endif.


%% @doc Fold the elements of the array using the given function and
%% initial accumulator value, skipping default-valued entries. The
%% elements are visited in order from the lowest index to the highest.
%% If `Function' is not a function, the call fails with reason `badarg'.
%%
%% @see foldl/3
%% @see sparse_foldr/3

-spec sparse_foldl(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

sparse_foldl(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function, 3) ->
    if N > 0 ->
	    sparse_foldl_1(N-1, E, A, 0, Function, D);
       true ->
	    A
    end;
sparse_foldl(_, _, _) ->
    erlang:error(badarg).

%% see foldl/3 for details
%% TODO: this can be optimized

sparse_foldl_1(N, E=?NODEPATTERN(S), A, Ix, F, D) ->
    sparse_foldl_2(1, E, A, Ix, F, D, N div S + 1, N rem S, S);
sparse_foldl_1(_N, E, A, _Ix, _F, _D) when is_integer(E) ->
    A;
sparse_foldl_1(N, E, A, Ix, F, D) ->
    sparse_foldl_3(1, E, A, Ix, F, D, N+1).

sparse_foldl_2(I, E, A, Ix, F, D, I, R, _S) ->
    sparse_foldl_1(R, element(I, E), A, Ix, F, D);
sparse_foldl_2(I, E, A, Ix, F, D, N, R, S) ->
    sparse_foldl_2(I+1, E, sparse_foldl_1(S-1, element(I, E), A, Ix, F, D),
	    Ix + S, F, D, N, R, S).

sparse_foldl_3(I, T, A, Ix, F, D, N) when I =< N ->
    case element(I, T) of
	D -> sparse_foldl_3(I+1, T, A, Ix+1, F, D, N);
	E -> sparse_foldl_3(I+1, T, F(Ix, E, A), Ix+1, F, D, N)
    end;
sparse_foldl_3(_I, _T, A, _Ix, _F, _D, _N) ->
    A.


-ifdef(EUNIT).
sparse_foldl_test_() ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    Reverse = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
	      (K,X,{C,L}) -> {C,[K+X|L]} 
	   end,
    [?_assertError(badarg, sparse_foldl([], 0, new())),
     ?_assertError(badarg, sparse_foldl([], 0, new(10))),
     ?_assert(sparse_foldl(Count, 0, new()) =:= 0),
     ?_assert(sparse_foldl(Count, 0, new(1)) =:= 0),
     ?_assert(sparse_foldl(Count, 0, new(10,{default,1})) =:= 0),
     ?_assert(sparse_foldl(Count, 0, from_list([0,1,2,3,4],0)) =:= 4),
     ?_assert(sparse_foldl(Count, 0, from_list([0,1,2,3,4,5,6,7,8,9,0],0))
	      =:= 9),
     ?_assert(sparse_foldl(Count, 0, from_list(lists:seq(0,999),0))
	      =:= 999),
     ?_assert(sparse_foldl(Sum, 0, from_list(lists:seq(0,10), 5)) =:= 50),
     ?_assert(sparse_foldl(Reverse, [], from_list(lists:seq(0,1000), 0))
	      =:= lists:reverse(lists:seq(1,1000))),
     ?_assert({0,[N0*100+1+2,N0*2+1+1,0]} =:= 
	      sparse_foldl(Vals, {0,[]}, 
			   set(N0*100+1,2,
			       set(N0*2+1,1,
				   set(0,0,new())))))
    ].
-endif.


%% @doc Fold the elements of the array right-to-left using the given
%% function and initial accumulator value. The elements are visited in
%% order from the highest index to the lowest. If `Function' is not a
%% function, the call fails with reason `badarg'.
%%
%% @see foldl/3
%% @see map/2

-spec foldr(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

foldr(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function, 3) ->
    if N > 0 ->
	    I = N - 1,
	    foldr_1(I, E, I, A, Function, D);
       true ->
	    A
    end;
foldr(_, _, _) ->
    erlang:error(badarg).

%% this is based on to_orddict/1

foldr_1(I, E=?NODEPATTERN(S), Ix, A, F, D) ->
    foldr_2(I div S + 1, E, Ix, A, F, D, I rem S, S-1);
foldr_1(I, E, Ix, A, F, D) when is_integer(E) ->
    foldr_1(I, unfold(E, D), Ix, A, F, D);
foldr_1(I, E, Ix, A, F, _D) ->
    I1 = I+1,
    foldr_3(I1, E, Ix-I1, A, F).

foldr_2(0, _E, _Ix, A, _F, _D, _R, _R0) ->
    A;
foldr_2(I, E, Ix, A, F, D, R, R0) ->
    foldr_2(I-1, E, Ix - R - 1,
	    foldr_1(R, element(I, E), Ix, A, F, D),
	    F, D, R0, R0).

-spec foldr_3(array_indx(), term(), integer(), A,
	      fun((array_indx(), _, A) -> B)) -> B.

foldr_3(0, _E, _Ix, A, _F) ->
    A;
foldr_3(I, E, Ix, A, F) ->
    foldr_3(I-1, E, Ix, F(Ix+I, element(I, E), A), F).


-ifdef(EUNIT).
foldr_test_() ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    List = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
	      (K,X,{C,L}) -> {C,[K+X|L]} 
	   end,
    [?_assertError(badarg, foldr([], 0, new())),
     ?_assertError(badarg, foldr([], 0, new(10))),
     ?_assert(foldr(Count, 0, new()) =:= 0),
     ?_assert(foldr(Count, 0, new(1)) =:= 1),
     ?_assert(foldr(Count, 0, new(10)) =:= 10),
     ?_assert(foldr(Count, 0, from_list([1,2,3,4])) =:= 4),
     ?_assert(foldr(Count, 10, from_list([0,1,2,3,4,5,6,7,8,9])) =:= 20),
     ?_assert(foldr(Count, 1000, from_list(lists:seq(0,999))) =:= 2000),
     ?_assert(foldr(Sum, 0, from_list(lists:seq(0,10))) =:= 55),
     ?_assert(foldr(List, [], from_list(lists:seq(0,1000)))
 	      =:= lists:seq(0,1000)),
     ?_assert({999,[0,N0*2+1+1,N0*100+1+2]} =:= 
	      foldr(Vals, {0,[]}, 
		    set(N0*100+1,2,
			set(N0*2+1,1,
			    set(0,0,new())))))
     
    ].
-endif.


%% @doc Fold the elements of the array right-to-left using the given
%% function and initial accumulator value, skipping default-valued
%% entries. The elements are visited in order from the highest index to
%% the lowest. If `Function' is not a function, the call fails with
%% reason `badarg'.
%%
%% @see foldr/3
%% @see sparse_foldl/3

-spec sparse_foldr(Function, InitialAcc :: A, Array :: array(Type)) -> B when
      Function :: fun((Index :: array_indx(), Value :: Type, Acc :: A) -> B).

sparse_foldr(Function, A, #array{size = N, elements = E, default = D})
  when is_function(Function, 3) ->
    if N > 0 ->
	    I = N - 1,
	    sparse_foldr_1(I, E, I, A, Function, D);
       true ->
	    A
    end;
sparse_foldr(_, _, _) ->
    erlang:error(badarg).

%% see foldr/3 for details
%% TODO: this can be optimized

sparse_foldr_1(I, E=?NODEPATTERN(S), Ix, A, F, D) ->
    sparse_foldr_2(I div S + 1, E, Ix, A, F, D, I rem S, S-1);
sparse_foldr_1(_I, E, _Ix, A, _F, _D) when is_integer(E) ->
    A;
sparse_foldr_1(I, E, Ix, A, F, D) ->
    I1 = I+1,
    sparse_foldr_3(I1, E, Ix-I1, A, F, D).

sparse_foldr_2(0, _E, _Ix, A, _F, _D, _R, _R0) ->
    A;
sparse_foldr_2(I, E, Ix, A, F, D, R, R0) ->
    sparse_foldr_2(I-1, E, Ix - R - 1,
	    sparse_foldr_1(R, element(I, E), Ix, A, F, D),
	    F, D, R0, R0).

-spec sparse_foldr_3(array_indx(), _, array_indx(), A,
		     fun((array_indx(), _, A) -> B), _) -> B.

sparse_foldr_3(0, _T, _Ix, A, _F, _D) ->
    A;
sparse_foldr_3(I, T, Ix, A, F, D) ->
    case element(I, T) of
	D -> sparse_foldr_3(I-1, T, Ix, A, F, D);
	E -> sparse_foldr_3(I-1, T, Ix, F(Ix+I, E, A), F, D)
    end.


%% @doc Get the number of entries in the array up until the last
%% non-default valued entry. In other words, returns `I+1' if `I' is the
%% last non-default valued entry in the array, or zero if no such entry
%% exists.
%% @see size/1
%% @see resize/1

-spec sparse_size(Array :: array()) -> non_neg_integer().

sparse_size(A) ->
    F = fun (I, _V, _A) -> throw({value, I}) end,
    try sparse_foldr(F, [], A) of
	[] -> 0
    catch
	{value, I} ->
	    I + 1
    end.


-ifdef(EUNIT).
sparse_foldr_test_() ->
    N0 = ?LEAFSIZE,
    Count = fun (_,_,N) -> N+1 end,
    Sum = fun (_,X,N) -> N+X end,
    List = fun (_,X,L) -> [X|L] end,
    Vals = fun(_K,undefined,{C,L}) -> {C+1,L};
	      (K,X,{C,L}) -> {C,[K+X|L]} 
	   end,
    [?_assertError(badarg, sparse_foldr([], 0, new())),
     ?_assertError(badarg, sparse_foldr([], 0, new(10))),
     ?_assert(sparse_foldr(Count, 0, new()) =:= 0),
     ?_assert(sparse_foldr(Count, 0, new(1)) =:= 0),
     ?_assert(sparse_foldr(Count, 0, new(10,{default,1})) =:= 0),
     ?_assert(sparse_foldr(Count, 0, from_list([0,1,2,3,4],0)) =:= 4),
     ?_assert(sparse_foldr(Count, 0, from_list([0,1,2,3,4,5,6,7,8,9,0],0))
	      =:= 9),
     ?_assert(sparse_foldr(Count, 0, from_list(lists:seq(0,999),0))
	      =:= 999),
     ?_assert(sparse_foldr(Sum, 0, from_list(lists:seq(0,10),5)) =:= 50),
     ?_assert(sparse_foldr(List, [], from_list(lists:seq(0,1000),0))
 	      =:= lists:seq(1,1000)),

     ?_assert(sparse_size(new()) =:= 0),
     ?_assert(sparse_size(new(8)) =:= 0),
     ?_assert(sparse_size(array:set(7, 0, new())) =:= 8),
     ?_assert(sparse_size(array:set(7, 0, new(10))) =:= 8),
     ?_assert(sparse_size(array:set(99, 0, new(10,{fixed,false})))
	      =:= 100),
     ?_assert(sparse_size(array:set(7, undefined, new())) =:= 0),
     ?_assert(sparse_size(array:from_list([1,2,3,undefined])) =:= 3),
     ?_assert(sparse_size(array:from_orddict([{3,0},{17,0},{99,undefined}]))
			  =:= 18),
     ?_assert({0,[0,N0*2+1+1,N0*100+1+2]} =:= 
	      sparse_foldr(Vals, {0,[]}, 
			   set(N0*100+1,2,
			       set(N0*2+1,1,
				   set(0,0,new())))))     
    ].
-endif.
