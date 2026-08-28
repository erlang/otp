%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(versions).
-moduledoc """
Utility functions for versions using the
[OTP Versions Scheme](`e:system:versions.md#version-scheme`).
""".
-moduledoc(#{since => "OTP @OTP-20352@"}).

-export_type([vsn_string/0, vsn_list/0, vsn_branch_string/0]).

-doc """
A version string formatted according to the
[OTP Versions Scheme](`e:system:versions.md#version-scheme`).

The string should be formatted as `<V(1)>.<V(2)> ... <V(N)>` where:
*   each `<V(X)>` component is the string representation of a non-negative
    decimal integer. No leading `0` digits except for the number zero which
    should be exactly one `0` digit.
*   each `<V(X)>` component is separated by a dot (`.`) character. No leading
    or trailing dot characters are allowed.
*   at least the `<V(1)>` and `<V(2)>` components exist.
*   trailing `0` are only allowed in the `<V(1)>` and `<V(2)>` components.

See the [OTP Versions Scheme](`e:system:versions.md#version-scheme`) for
more information about versions.
""".
-type vsn_string() :: unicode:chardata().

-doc """
A branch identifier identifying a specific branch in a version tree.

The branch identifier format differs from the `t:vsn_string/0` type in that:
*   trailing `0` components are allowed.
*   it is always ended by a trailing dot (`.`) after the last component.
*   no branch identifiers with less than three components exist except for
    `~"0."` which identifies the trunk of the tree.

See the [OTP Versions Scheme](`e:system:versions.md#version-scheme`) for
more information about branches.
""".
-type vsn_branch_string() :: unicode:chardata().

-doc """
A list representation of the `t:vsn_string/0` type.

A list of components of actual non-negative integers as elements in the list
separated as elements instead of by the dot character. The order of the
components, amount of components, and content of the components is the same as
 for the `t:vsn_string/0` type.
""".
-type vsn_list() :: [non_neg_integer()].

-export([branch/1, branch_base/1, check/1, compare/2,
         list_check/1, list_compare/2, list_to_string/1, string_to_list/1]).

-doc """
Calculates the branch identifier of the branch that the version `V` exists on.

Returns the branch identifier, of the type described in the
 [OTP Versions Scheme](`e:system:versions.md#version-scheme`),
that the version `V` exists on. If the version `V` exists on the trunk of the
tree, `~"0."` is returned.

Note that the returned branch identifier does not correspond to any of the
branch names used in the OTP git repository.

A `badarg` `error` exception will be thrown if the version is not a valid
version adhering to the description of the `t:vsn_string/0` type.

## Examples
```erlang
1> versions:branch(~"18.0").
<<"0.">>
2> versions:branch(~"18.2.4").
<<"0.">>
3> versions:branch(~"18.2.4.1").
<<"18.2.4.">>
4> versions:branch(~"18.2.4.0.1").
<<"18.2.4.0.">>
```
""".
-spec branch(V :: vsn_string()) -> vsn_branch_string().
branch(V) ->
    try
        case vs2vl(V) of
            [_, _] -> ~"0.";
            [_, _, _] -> ~"0.";
            VL -> c2b([vl2vs(lists:reverse(tl(lists:reverse(VL))), true), $.])
        end
    catch _:_ ->
            error(badarg, [V])
    end.

-doc """
Calculates the base version of the branch `B`.

Returns the version which is the base version of the branch identified by the
argument. If the passed argument identifies the trunk of the version tree
(`"~0."`), the base version returned is `~"0.0"`.

A `badarg` `error` exception will be thrown if the branch identifier is not a
valid branch identifier adhering to the description of the
`t:vsn_branch_string/0` type.

## Examples
```erlang
1> versions:branch_base(~"0.").
<<"0.0">>
2> versions:branch_base(~"18.2.4.").
<<"18.2.4">>
3> versions:branch_base(~"18.2.4.0.").
<<"18.2.4">>
```
""".
-spec branch_base(B :: vsn_branch_string()) -> vsn_string().
branch_base(B) ->
    try
        case bs2rbl(B) of
            [0] ->
                ~"0.0";
            RBL ->
                vl2vs(case lists:reverse(drop_zeros(RBL)) of
                          [X] -> [X,0];
                          Xs -> Xs
                      end,
                      false)
        end
    catch _:_ ->
            error(badarg, [B])
    end.

-doc """
Checks whether or not the version `V` is a valid version.

Returns true if the version is a valid version adhering to the description
of the `t:vsn_string/0` type; otherwise `false`.

Examples:
```erlang
1> versions:check(~"30.0").
true
2> versions:check(~"30.0.1").
true
3> versions:check(~"30.0.1.2").
true
4> versions:check(~"30.0.1.").
false
5> versions:check(~"30.0-rc1").
false
```
""".
-spec check(V :: vsn_string()) -> 'true' | 'false'.
check(V) ->
    try _ = vs2vl(V), true
    catch _:_ -> false
    end.

-doc """
Compares the versions `V1` and `V2`. The return value tells you how `V1`
compares to `V2`.

The return value:
*   `same` tells you that `V1` is the *same* as `V2`.
*   `ancestor` tells you that `V1` is an *ancestor* of `V2`.
*   `descendant` tells you that `V1` is a *descendant* of `V2`.
*   `undefined` tells you that the order between `V1` and `V2` is *undefined*.

A `badarg` `error` exception will be thrown if a version is not a valid
version adhering to the description of the `t:vsn_string/0` type.

See the description of the
[order between versions](`e:system:versions.md#order-of-versions`) in the OTP
version scheme for more information.

## Examples
```erlang
1> versions:compare(~"23.3", ~"23.3").
same
2> versions:compare(~"23.3", ~"23.2.7").
descendant
3> versions:compare(~"23.2.7", ~"23.3").
ancestor
4> versions:compare(~"23.2.7.1", ~"23.3").
undefined
```
""".
-spec compare(V1 :: vsn_string(), V2 :: vsn_string()) ->
          'same' | 'ancestor' | 'descendant' | 'undefined'.
compare(V1, V2) ->
    try cmp(vs2vl(V1), vs2vl(V2))
    catch _:_ -> error(badarg, [V1, V2])
    end.

-doc """
Checks whether or not the version `VL` is a valid version list representation.

Returns true if the version is a valid version adhering to the description
of the `t:vsn_list/0` type; otherwise `false`.
""".
-spec list_check(VL :: vsn_list()) -> 'true' | 'false'.
list_check(VL) ->
    try _ = chk_vl(VL), true
    catch _:_ -> false
    end.

-doc """
Compare two versions on the version list format.

Works exactly the same way as `compare/2` with the only difference that the
versions passed as input are represented using the `t:vsn_list/0` type instead
of using the `t:vsn_string/0` type.

A `badarg` `error` exception will be thrown if a version is not a valid
version adhering to the description of the `t:vsn_list/0` type.

## Examples
```erlang
1> versions:list_compare([23, 3], [23, 3]).
same
2> versions:list_compare([23, 3], [23, 2, 7]).
descendant
3> versions:list_compare([23, 2, 7], [23, 3]).
ancestor
4> versions:list_compare([23, 2, 7, 1], [23, 3]).
undefined
```
""".
-spec list_compare(VL1 :: vsn_list(), VL2 :: vsn_list()) ->
          'same' | 'ancestor' | 'descendant' | 'undefined'.
list_compare(VL1, VL2) ->
    try cmp(chk_vl(VL1), chk_vl(VL2))
    catch _:_ -> error(badarg, [VL1, VL2])
    end.

-doc """
Convert a version from the `t:vsn_list/0` type to the `t:vsn_string/0` type.

A `badarg` `error` exception will be thrown if the version is not a valid
version adhering to the description of the `t:vsn_list/0` type.
""".
-spec list_to_string(VL :: vsn_list()) -> vsn_string().
list_to_string(VL) ->
    try vl2vs(VL, false)
    catch _:_ -> error(badarg, [VL])
    end.

-doc """
Convert a version from the `t:vsn_string/0` type to the `t:vsn_list/0` type.

A `badarg` `error` exception will be thrown if the version is not a valid
version adhering to the description of the `t:vsn_string/0` type.
""".
-spec string_to_list(V :: vsn_string()) -> vsn_list().
string_to_list(V) ->
    try vs2vl(V)
    catch _:_ -> error(badarg, [V])
    end.

%%%
%%% Internal helper functions
%%%

%% Branch string to reversed branch list
bs2rbl(B) -> bsplit2bl(string:split(B, ~".", all), 0, []).

%% Branch split list to branch list
bsplit2bl([X], N, RXs) ->
    true = string:is_empty(X), %% from the trailing dot
    %% The only valid branch identifier for the trunk is "0."
    _ = if N == 1 -> [0] = RXs; %% The trunk
           N == 2 -> error(invalid_branch_id);
           true -> ok
        end,
    RXs;
bsplit2bl([X|Xs], N, RXs) ->
    I = str2int(X),
    true = I >= 0,
    bsplit2bl(Xs, N+1, [I | RXs]).

%% Version string to version list
vs2vl(V) -> il2vl([str2int(X) || X <:- string:split(V, ~".", all)]).

%% String to integer
str2int(S) ->
    BS = c2b(S),
    case BS of
        <<"0"/utf8, _/utf8, _/binary>> -> error(leading_zero_in_integer);
        _ -> ok
    end,
    {I, R} = string:to_integer(BS),
    true = string:is_empty(R),
    I.

%% Integer list to version list: Check syntax and drop all trailing zeros
il2vl([X, Y] = VL) when X >= 0, Y >= 0 -> VL;
il2vl([X, Y, Z] = VL) when X >= 0, Y >= 0, Z > 0 -> VL;
il2vl([X, Y, Z | [_|_] = Vs] = VL) when X >= 0, Y >= 0, Z >= 0 -> chk_i(Vs), VL.

%% Check components past normal versions
chk_i([]) -> ok;
chk_i([0]) -> error(trailing_zero);
chk_i([X|Xs]) when X >= 0 -> chk_i(Xs).

%% Compare version lists (the core of compare/2 and list_compare/2)
%% Input must be valid version lists; otherwise, incorrect result
%% will be produced.
cmp(V1, V2) -> cmp(V1, V2, 1).

cmp([XY|Xs], [XY|Ys], N) -> cmp(Xs, Ys, N+1);
cmp([], [], _N) -> same;
cmp([], [_Y|_Ys], _N) -> ancestor;
cmp([_X|_Xs], [], _N) -> descendant;
%% X on trunk and X < Y
cmp([X, _X2], [Y|_Ys], 1) when X < Y -> ancestor;
cmp([X, _X2, _X3], [Y|_Ys], 1) when X < Y -> ancestor;
cmp([X], [Y|_Ys], 2) when X < Y -> ancestor;
cmp([X, _X2], [Y|_Ys], 2) when X < Y -> ancestor;
%% Y on trunk and X > Y
cmp([X|_Xs], [Y, _Y1], 1) when X > Y -> descendant;
cmp([X|_Xs], [Y, _Y1, _Y2], 1) when X > Y -> descendant;
cmp([X|_Xs], [Y], 2) when X > Y -> descendant;
cmp([X|_Xs], [Y, _Y1], 2) when X > Y -> descendant;
%% X last component (perhaps also on trunk) and X < Y
cmp([X], [Y|_Ys], N) when X < Y, N >= 3 -> ancestor;
%% Y last component (perhaps also on trunk) and X > Y
cmp([X|_Xs], [Y], N) when X > Y, N >= 3 -> descendant;
cmp(_Xs, _Ys, _N) -> undefined.

%% Check that list is a valid version list
chk_vl([V1, V2 | Vs] = VL) when is_integer(V1), V1 >= 0,
                                is_integer(V2), V2 >= 0 ->
    true = chk_vl_tail(Vs),
    VL.

chk_vl_tail([]) -> true;
chk_vl_tail([V]) -> true = (is_integer(V) andalso V > 0);
chk_vl_tail([V|Vs]) -> true = (is_integer(V) andalso V >= 0), chk_vl_tail(Vs).

%% Version list to version string
vl2vs([V1, V2 | Vs], TZ) when is_integer(V1), V1 >= 0,
                               is_integer(V2), V2 >= 0 ->
    c2b([integer_to_binary(V1), $., integer_to_binary(V2) | vl2vs_tail(Vs,TZ)]).

vl2vs_tail([], _TZ) ->
    [];
vl2vs_tail([V], false) ->
    true = (is_integer(V) andalso V > 0),
    [$., integer_to_binary(V)];
vl2vs_tail([V], true) ->
    true = (is_integer(V) andalso V >= 0),
    [$., integer_to_binary(V)];
vl2vs_tail([V|Vs], TZ) ->
    true = (is_integer(V) andalso V >= 0),
    [$., integer_to_binary(V) | vl2vs_tail(Vs, TZ)].

%% drop all zeros in the head
drop_zeros([0|Xs]) ->
    drop_zeros(Xs);
drop_zeros(Xs) ->
    Xs.

% Characters to binary
c2b(L) -> unicode:characters_to_binary(L).
