%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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

  -ifdef(EQC).
    -define(MOD_eqc, eqc).
    -include_lib("eqc/include/eqc.hrl").
  -else.
    -ifdef(PROPER).
      -define(MOD_eqc, proper).
      -include_lib("proper/include/proper.hrl").
    -else.
      -ifdef(TRIQ).
        -define(MOD_eqc, triq).
        -include_lib("triq/include/triq.hrl").
      -endif.
    -endif.
  -endif.

-endif.
