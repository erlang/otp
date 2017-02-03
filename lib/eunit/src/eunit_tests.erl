%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @copyright 2007 Richard Carlsson
%% @private
%% @see eunit
%% @doc External tests for eunit.erl

-module(eunit_tests).

-include("eunit.hrl").

-dialyzer(no_match).

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
