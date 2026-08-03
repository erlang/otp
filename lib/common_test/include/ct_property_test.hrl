%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2026. All Rights Reserved.
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

-ifndef(CT_PROPERTY_TEST_HRL).
  -define(CT_PROPERTY_TEST_HRL, true).

  -define(CT_BYTE(), choose(0, 255)).
  -define(CT_NEG_INT(), ?LET(N, int(), -abs(N)-1)).
  -define(CT_RANGE(Min,Max), choose(Min, Max)).
  -define(CT_INT(Min,Max), choose(Min, Max)).

  -ifdef(EQC).
    -define(MOD_eqc, eqc).
    -include_lib("eqc/include/eqc.hrl").
    -define(CT_SAFE_ANY(), ct_quickcheck_ext:safe_any()).
    -define(CT_SAFE_ATOM(), ct_quickcheck_ext:safe_atom()).
    -define(CT_SAFE_LIST(), ct_quickcheck_ext:safe_list()).
    -define(CT_SAFE_MAP(), ct_quickcheck_ext:safe_map()).
    -define(CT_SAFE_TERM(), ct_quickcheck_ext:safe_term()).
    -define(CT_SAFE_TUPLE(), ct_quickcheck_ext:safe_tuple()).
    -define(CT_EXISTING_ATOM(), ct_quickcheck_ext:existing_atom()).
  -else.
    -ifdef(PROPER).
      -define(MOD_eqc, proper).
      -include_lib("proper/include/proper.hrl").
      -define(CT_SAFE_ANY(), ct_proper_ext:safe_any()).
      -define(CT_SAFE_ATOM(), ct_proper_ext:safe_atom()).
      -define(CT_SAFE_LIST(), ct_proper_ext:safe_list()).
      -define(CT_SAFE_MAP(), ct_proper_ext:safe_map()).
      -define(CT_SAFE_TERM(), ct_proper_ext:safe_term()).
      -define(CT_SAFE_TUPLE(), ct_proper_ext:safe_tuple()).
      -define(CT_EXISTING_ATOM(), ct_proper_ext:existing_atom()).
    -else.
      -ifdef(TRIQ).
        -define(MOD_eqc, triq).
        -include_lib("triq/include/triq.hrl").
      -endif.
    -endif.
  -endif.

-endif.
