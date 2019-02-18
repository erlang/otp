%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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


-module(ct_property_test).

%% API
-export([init_per_suite/1,
         init_tool/1,
         quickcheck/2]).

-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    case init_tool(Config) of
        {skip, _}=Skip ->
            Skip;
        Config1 ->
            Path = property_tests_path("property_test", Config1),
            case compile_tests(Path, Config1) of
                error ->
                    {fail, "Property test compilation failed in "++Path};
                up_to_date ->
                    add_code_pathz(Path),
                    [{property_dir, Path} | Config1]
            end
    end.

init_tool(Config) ->
    case which_module_exists([eqc,proper,triq]) of
        {ok, ToolModule} ->
            ct:pal("Found property tester ~p",[ToolModule]),
            [{property_test_tool, ToolModule} | Config];
        not_found ->
            ct:pal("No property tester found",[]),
            {skip, "No property testing tool found"}
    end.
	
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
	    {fail, io_lib:format("~p:~tp/~p returned bad result",[M,F,length(Args)])}
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

compile_tests(Path, Config) ->
    ToolModule = proplists:get_value(property_test_tool, Config),
    MacroDefs = macro_def(ToolModule),
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(Path),
    {ok,FileNames} = file:list_dir("."),
    BeamFiles = [F || F<-FileNames,
		      filename:extension(F) == ".beam"],
    _ = [file:delete(F) || F<-BeamFiles],
    ct:pal("Compiling in ~tp:~n  Deleted ~p~n  MacroDefs=~p",[Path,BeamFiles,MacroDefs]),
    Result = make:all([load|MacroDefs]),
    ok = file:set_cwd(Cwd),
    Result.
    

macro_def(eqc) -> [{d, 'EQC'}];
macro_def(proper) -> [{d, 'PROPER'}];
macro_def(triq) -> [{d, 'TRIQ'}].
    
function_name(quickcheck, triq) -> check;
function_name(F, _) -> F.
    
