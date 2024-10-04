%% Extracted from lib/syntax_tools/src/erl_recomment.erl to test
%% omissions in handling of Phi instructions.

%% =====================================================================
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
%% @copyright 1997-2006 Richard Carlsson
%% @author Richard Carlsson <carlsson.richard@gmail.com>
%% @end
%% =====================================================================

-module(phis).

-export([filter_forms/1]).

-record(filter, {file = undefined :: file:filename() | 'undefined',
		 line = 0         :: integer()}).

filter_forms(Fs) ->
    filter_forms(Fs, #filter{}).

filter_forms([{A1, A2} | Fs], S) ->
%ssa% (_, Rec0) when post_ssa_opt ->
%ssa% Rec = update_record(inplace, 3, Rec0, ...),
%ssa% Phi = phi({Rec0, _}, {Rec, _}),
%ssa% _ = update_record(inplace, 3, Phi, ...).
    S1 = case ex:f() of
	     undefined ->
		 S#filter{file = A1, line = A2};
	     _ ->
		 S
	 end,
    if S1#filter.file =:= A1 ->
	    filter_forms(Fs, S1#filter{line = A2});
       true ->
	    filter_forms(Fs, S1)
    end;
filter_forms([], _) ->
    [].
