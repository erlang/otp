%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: eunit_striptests.erl 329 2009-03-01 11:23:32Z rcarlsson $ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @author Eric Merritt <cyberlync@gmail.com>
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
