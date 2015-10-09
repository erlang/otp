%% ---------------------------------------------------------------------
%% Header file for merl
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-ifndef(MERL_HRL).


%% Quoting a piece of code
-define(Q(Text), merl:quote(?LINE, Text)).

%% Quasi-quoting code, substituting metavariables listed in Env
-define(Q(Text, Env), merl:qquote(?LINE, Text, Env)).


-ifndef(MERL_NO_TRANSFORM).
-compile({parse_transform, merl_transform}).
-endif.


-endif.
