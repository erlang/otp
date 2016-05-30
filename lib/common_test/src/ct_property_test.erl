%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2016. All Rights Reserved.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                             %%%
%%%                       WARNING                               %%%
%%%                                                             %%%
%%% This is experimental code which may be changed or removed   %%%
%%%               anytime without any warning.                  %%%
%%%                                                             %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% @doc EXPERIMENTAL support in common-test for calling property based tests.
%%%
%%% <p>This module is a first step towards running Property Based testing in the
%%% Common Test framework.  A property testing tool like QuickCheck or PropEr is
%%% assumed to be installed.</p>
%%%
%%% <p>The idea is to have a common_test testsuite calling a property testing
%%% tool with special property test suites as defined by that tool. In this manual
%%% we assume the usual Erlang Application directory structure.  The tests are
%%% collected in the application's <c>test</c> directory. The test directory
%%% has a sub-directory called <c>property_test</c> where everything needed for
%%% the property tests are collected.</p>
%%%
%%% <p>A typical ct test suite using <c>ct_property_test</c> is organized as follows:
%%% </p>
%%% ```
%%% -include_lib("common_test/include/ct.hrl").
%%% 
%%% all() -> [prop_ftp_case].
%%% 
%%% init_per_suite(Config) ->
%%%     ct_property_test:init_per_suite(Config).
%%% 
%%% %%%---- test case
%%% prop_ftp_case(Config) ->
%%%     ct_property_test:quickcheck(
%%%       ftp_simple_client_server:prop_ftp(Config),
%%%       Config
%%%      ).
%%% '''
%%%
%%% <warning>
%%% <p>
%%% This is experimental code which may be changed or removed
%%% anytime without any warning.
%%% </p>
%%% </warning>
%%%
%%% @end

-module(ct_property_test).

%% API
-export([init_per_suite/1,
	 quickcheck/2]).

-include_lib("common_test/include/ct.hrl").

%%%-----------------------------------------------------------------
%%% @spec init_per_suite(Config) -> Config | {skip,Reason}
%%%
%%% @doc Initializes Config for property testing.
%%%
%%% <p>The function investigates if support is available for either Quickcheck, PropEr,
%%% or Triq.
%%% The options <c>{property_dir,AbsPath}</c> and
%%% <c>{property_test_tool,Tool}</c> is set in the Config returned.</p>
%%% <p>The function is intended to be called in the init_per_suite in the test suite.</p>
%%% <p>The property tests are assumed to be in the subdirectory <c>property_test</c>.</p>
%%% @end

init_per_suite(Config) ->
    case which_module_exists([eqc,proper,triq]) of
	{ok,ToolModule} ->
 	    ct:pal("Found property tester ~p",[ToolModule]),
	    Path = property_tests_path("property_test", Config),
	    case compile_tests(Path,ToolModule) of
		error -> 
		    {fail, "Property test compilation failed in "++Path};
		up_to_date ->
		    add_code_pathz(Path),
		    [{property_dir,Path},
		     {property_test_tool,ToolModule} | Config]
	    end;

	not_found ->
 	    ct:pal("No property tester found",[]),
	    {skip, "No property testing tool found"}
    end.
	
%%%-----------------------------------------------------------------
%%% @spec quickcheck(Property, Config) -> true | {fail,Reason}
%%%
%%% @doc Call quickcheck and return the result in a form suitable for common_test.
%%%
%%% <p>The function is intended to be called in the test cases in the test suite.</p>
%%% @end

quickcheck(Property, Config) ->
    Tool = proplists:get_value(property_test_tool,Config),
    F = function_name(quickcheck, Tool),
    mk_ct_return( Tool:F(Property), Tool ).


%%%================================================================
%%%
%%% Local functions
%%% 

%%% Make return values back to the calling Common Test suite
mk_ct_return(true, _Tool) ->
    true;
mk_ct_return(Other, Tool) ->
    try lists:last(hd(Tool:counterexample()))
    of
	{set,{var,_},{call,M,F,Args}} ->
	    {fail, io_lib:format("~p:~p/~p returned bad result",[M,F,length(Args)])}
    catch
	_:_ ->
	    {fail, Other}
    end.

%%% Check if a property testing tool is found
which_module_exists([Module|Modules]) ->
    case module_exists(Module) of
	true -> {ok,Module};
	false -> which_module_exists(Modules)
    end;
which_module_exists(_) ->
    not_found.

module_exists(Module) ->
    is_list(catch Module:module_info()).

%%% The path to the property tests
property_tests_path(Dir, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    filename:join(lists:droplast(filename:split(DataDir))++[Dir]).

%%% Extend the code path with Dir if it not already present
add_code_pathz(Dir) ->
    case lists:member(Dir, code:get_path()) of
	true ->  ok;
	false ->
	    true = code:add_pathz(Dir),
	    ok
    end.

compile_tests(Path, ToolModule) ->
    MacroDefs = macro_def(ToolModule),
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Path),
    {ok,FileNames} = file:list_dir("."),
    BeamFiles = [F || F<-FileNames,
		      filename:extension(F) == ".beam"],
    _ = [file:delete(F) || F<-BeamFiles],
    ct:pal("Compiling in ~p:~n  Deleted ~p~n  MacroDefs=~p",[Path,BeamFiles,MacroDefs]),
    Result = make:all([load|MacroDefs]),
    ok = file:set_cwd(Cwd),
    Result.
    

macro_def(eqc) -> [{d, 'EQC'}];
macro_def(proper) -> [{d, 'PROPER'}];
macro_def(triq) -> [{d, 'TRIQ'}].
    
function_name(quickcheck, triq) -> check;
function_name(F, _) -> F.
    
