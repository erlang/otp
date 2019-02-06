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
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Parse transform for automatic exporting of test functions.

-module(eunit_autoexport).

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
    F = fun (Form, Set) ->
		form(Form, Set, TestSuffix, GeneratorSuffix,
		     ExportSuffix)
	end,
    Exports = sets:to_list(lists:foldl(F, sets:new(), Forms)),
    rewrite(Forms, Exports).

form({function, _L, Name, 0, _Cs}, S, TestSuffix, GeneratorSuffix,
     ExportSuffix) ->
    N = atom_to_list(Name),
    case lists:suffix(TestSuffix, N) of
	true ->
	    sets:add_element({Name, 0}, S);
	false ->
	    case lists:suffix(GeneratorSuffix, N) of
		true ->
		    sets:add_element({Name, 0}, S);
		false ->
		    case lists:suffix(ExportSuffix, N) of
			true ->
			    sets:add_element({Name, 0}, S);
			false ->
			    S
		    end
	    end
    end;
form({function, _L, ?DEFAULT_MODULE_WRAPPER_NAME, 1, _Cs}, S, _, _, _) ->
    sets:add_element({?DEFAULT_MODULE_WRAPPER_NAME,1}, S);
form(_, S, _, _, _) ->
    S.

rewrite([{attribute,_,module,{Name,_Ps}}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
rewrite([{attribute,_,module,Name}=M | Fs], Exports) ->
    module_decl(Name, M, Fs, Exports);
rewrite([F | Fs], Exports) ->
    [F | rewrite(Fs, Exports)];
rewrite([], _Exports) ->
    [].    %% fail-safe, in case there is no module declaration

rewrite([{function,_,test,0,_}=F | Fs], As, Module, _Test) ->
    rewrite(Fs, [F | As], Module, false);
rewrite([F | Fs], As, Module, Test) ->
    rewrite(Fs, [F | As], Module, Test);
rewrite([], As, Module, Test) ->
    L = erl_anno:new(0),
    {if Test ->
	     [{function,L,test,0,
	       [{clause,L,[],[],
		 [{call,L,{remote,L,{atom,L,eunit},{atom,L,test}},
		   [{atom,L,Module}]}]}]}
	      | As];
	true ->
	     As
     end,
     Test}.

module_decl(Name, M, Fs, Exports) ->
    Module = Name,
    {Fs1, Test} = rewrite(Fs, [], Module, true),
    Es = if Test -> [{test,0} | Exports];
	    true -> Exports
	 end,
    [M, {attribute,erl_anno:new(0),export,Es} | lists:reverse(Fs1)].
