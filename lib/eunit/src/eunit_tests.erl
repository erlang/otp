%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2007 Richard Carlsson
%% @private
%% @see eunit
%% @doc External tests for eunit.erl

-module(eunit_tests).

-include("eunit.hrl").

-ifdef(TEST).
id(X) -> X.  % for suppressing compiler warnings
-endif.

under_eunit_test() -> ?assert(?UNDER_EUNIT).

let_test() -> ?assertEqual(42, ?LET(X, 17, X+25)).

if_test_() ->
    [?_assertEqual(17, ?IF(id(1) > 0, 17, 42)),
     ?_assertEqual(42, ?IF(id(1) < 0, 17, 42))].

matches_test_() ->
    [?_assert(?MATCHES("hel"++_, "hello")),
     ?_assertNot(?MATCHES("hal"++_, "hello"))].
