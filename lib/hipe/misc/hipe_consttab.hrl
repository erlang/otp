%% -*- erlang-indent-level: 2 -*-
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
%%-----------------------------------------------------------------------------

-type ct_alignment()  :: 4 | 8 | 16 | 32.

-type hipe_constlbl() :: non_neg_integer().
-type hipe_consttab() :: {dict:dict(), [hipe_constlbl()], hipe_constlbl()}.

%%-----------------------------------------------------------------------------
