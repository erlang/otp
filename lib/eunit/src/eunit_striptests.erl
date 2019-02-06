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
%% @author Eric Merritt <ericbmerritt@gmail.com>
%% @copyright 2006 Richard Carlsson, Eric Merritt
%% @private
%% @see eunit
%% @doc Parse transform for stripping EUnit test functions.

-module(eunit_striptests).

-include("eunit_internal.hrl").

-export([parse_transform/2]).

parse_transform(Forms, Options) ->
    TestSuffix = proplists:get_value(eunit_test_suffix, Options,
				     ?DEFAULT_TEST_SUFFIX),
    GeneratorSuffix = proplists:get_value(eunit_generator_suffix,
					  Options,
					  ?DEFAULT_GENERATOR_SUFFIX),
    ExportSuffix = proplists:get_value(eunit_export_suffix, Options,
				       ?DEFAULT_EXPORT_SUFFIX),
    Exports = lists:foldl(fun ({attribute,_,export,Es}, S) ->
				  sets:union(sets:from_list(Es), S);
			      (_F, S) -> S
			  end,
			  sets:new(), Forms),
    F = fun (Form, Acc) ->
		form(Form, Acc, Exports, TestSuffix, GeneratorSuffix,
		     ExportSuffix)
	end,
    lists:reverse(lists:foldl(F, [], Forms)).

form({function, _L, Name, 0, _Cs}=Form, Acc, Exports, TestSuffix,
     GeneratorSuffix, ExportSuffix) ->
    N = atom_to_list(Name),
    case not sets:is_element({Name, 0}, Exports)
	andalso (lists:suffix(TestSuffix, N)
		 orelse lists:suffix(GeneratorSuffix, N)
		 orelse lists:suffix(ExportSuffix, N))
	of
	true ->
	    Acc;
	false ->
	    [Form | Acc]
    end;
form({function, _L, ?DEFAULT_MODULE_WRAPPER_NAME, 1, _Cs}, Acc, _, _, _,
     _) ->
    Acc;
form(Form, Acc, _, _, _, _) ->
    [Form | Acc].
