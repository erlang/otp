%% =====================================================================
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
%%
%% Copyright 2019-2021 Radek Szymczyszyn
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
%%
%% @author Radek Szymczyszyn <lavrin@gmail.com>
%% @end
%% =====================================================================
-module(eep48_redundant).

-export([f_redundant_spec/0,
         f_only_attr/0,
         f_only_tag/0,
         f_prefixed_spec/0]).

-export_type([t_only_attr/0,
	      t_only_tag/0,
	      t_redundant/0]).

-type t_only_attr() :: any().
%% Type `t_only_attr' defined with an attribute.

%% @type t_only_tag() = any().
%% Type `t_only_tag' defined with a tag.

-type t_redundant() :: any().
%% Type `t_redundant' defined with an attribute, redundant with a tag.

%% @type t_redundant() = any().
%% Type `t_redundant' defined with a tag, redundant with an attribute.

-spec f_redundant_spec() -> atom().
%% @doc Function with a redundant spec.
%% @spec f_redundant_spec() -> tag()
f_redundant_spec() -> ok.

-spec f_only_attr() -> atom().
%% @doc Function with only a spec attribute.
f_only_attr() -> ok.

%% @spec f_only_tag() -> atom()
%% @doc Function with only a spec tag.
f_only_tag() -> ok.

-spec eep48_redundant:f_prefixed_spec() -> any().
f_prefixed_spec() -> ok.
