%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2010-2015 Richard Carlsson
%% Copyright Ericsson AB 2025. All Rights Reserved.
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
%% %CopyrightEnd%
%%
%% NOTE: An object file that uses the macros in this header file shall
%% never be considered a derived work under the the LGPL; these macros
%% shall be regarded as "small" regardless of the exact line count.
%%

-ifndef(MERL_HRL).


%% Quoting a piece of code
-define(Q(Text), merl:quote(?LINE, Text)).

%% Quasi-quoting code, substituting metavariables listed in Env
-define(Q(Text, Env), merl:qquote(?LINE, Text, Env)).


-ifndef(MERL_NO_TRANSFORM).
-compile({parse_transform, merl_transform}).
-endif.


-endif.
