%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
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
%% =====================================================================

%% @doc Test visibility of various kinds of items in the generated docs.
-module(eep48_visibility).

-export([public_function/0, hidden_function/0, private_function/0]).

-export_type([public_type/0]).

%% Note: There is no such thing as a hidden or private type.

-type public_type() :: {}.
%% This type should be visible in the generated docs.
-type non_exported_type() :: ok.
%% This type should not be visible in the generated docs.

%% @doc This function should be visible in the generated docs.
-spec public_function() -> ok.
public_function() -> ok.

%% @doc This function should not be visible in the generated docs.
%% @hidden
-spec hidden_function() -> ok.
hidden_function() -> ok.

%% @doc This function should not be visible in the generated docs.
%% @private
-spec private_function() -> non_exported_type().
private_function() -> non_exported_function().

%% @doc This type should not be visible in the generated docs.
-spec non_exported_function() -> non_exported_type().
non_exported_function() -> ok.