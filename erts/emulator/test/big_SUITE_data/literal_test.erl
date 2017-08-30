%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(literal_test).

-export([t/0]).

t() ->
     2639222 = do_band(-30710410, 11032439),
    -104896167137483835127591520601167100453480347078199925156632915223228188306305878154109985624943277357501787279310034030156370067160844817777591157023073455111626047495778039507502639061242015835277440456218702874565483838389693116456108032 = do_bsl(-283388912239613, 746),
    899396154689163167548626101 = do_plus(899396154689163167641847368, -93221267),
    ok.

do_plus(A, B) ->
    A + B.

do_band(A, B) ->
    A band B.

do_bsl(X, S) ->
    X bsl S.

