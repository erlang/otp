%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
%%
%%-----------------------------------------------------------------
%% File: ic_pragma_SUITE.erl
%% 
%% Description:
%% Test suite for the IFR object registration when
%% pragmas are engaged
%%
%%-----------------------------------------------------------------
-module(ic_pragma_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1]).
-export([ifr_pragma_reg/1, pragma_error/1, uggly_pragmas/1]).


%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(REMAP_EXCEPT(F), case catch F of
			     {'EXCEPTION', E} -> exit(E);
			     R -> R
			 end).
%% Standard options to the ic compiler, NOTE unholy use of OutDir

-define(OUT(X), filename:join([proplists:get_value(priv_dir, Config), gen, to_list(X)])).


%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [ifr_pragma_reg, pragma_error, uggly_pragmas].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_suite(Config) ->
    io:format("Setting up.....~n"),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    orber:install([node()]),
    orber:start(),
    if
	is_list(Config) ->
	    Config;
	true ->
	    exit("Config not a list")
    end.

end_per_suite(Config) ->
    io:format("Setting down.....~n"),
    orber:stop(),
    orber:uninstall(),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    Config.




%%-----------------------------------------------------------------
%% Test Case: IFR registration with pragmas
%%-----------------------------------------------------------------
%% Checks that IFR object is correctly registered under pragma engagement.
ifr_pragma_reg(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(ifr_pragma_reg_run(Config)).

ifr_pragma_reg_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(ifr_pragma_reg),
    File0 = filename:join(DataDir, reg_m0),
    ok = ic:gen(File0, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}]),
    ok = compile(OutDir, ifr_pragma_files()),
    code:add_pathz(OutDir),

    %% OE_register for all files
    ok = 'oe_reg_m0':'oe_register'(),
    
    %% Pragma registration test
    OE_IFR = orber_ifr:find_repository(),
    io:format("~n##### Starting the test case #####~n"), 
    check_pragma_effect(OE_IFR,"IDL:M1/T1:1.0"),
    check_pragma_effect(OE_IFR,"DCE:d62207a2-011e-11ce-88b4-0800090b5d3e:3"),
    check_pragma_effect(OE_IFR,"IDL:P2/T3:1.0"),
    check_pragma_effect(OE_IFR,"IDL:P1/M2/T4:2.4"),
    
    %% OE_unregister for all files
    ok = 'oe_reg_m0':'oe_unregister'(),
    code:del_path(OutDir),
    ok.


ifr_pragma_files() -> ['oe_reg_m0'].


check_pragma_effect(OE_IFR,ID) ->
    io:format("Checking for existance of : ~s~n",[ID]),
    case orber_ifr:lookup_id(OE_IFR,ID) of
        [] ->
	    test_server:fail(ID ++ " does not exist"),
	    false;
	{Def,_} ->
	    io:format("Id refers to = {~p,#Bin}~n",[Def]),
	    true
    end.




%%-----------------------------------------------------------------
%% Test Case: Syntactical / Semantical error pragma definitions 
%%-----------------------------------------------------------------
%% Finds errornous pragma definitions under compilation.
pragma_error(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(pragma_error_run(Config)).

pragma_error_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(pragma_error),
    File1 = filename:join(DataDir, reg_m1),
    File2 = filename:join(DataDir, reg_m2),
    File3 = filename:join(DataDir, reg_m3),
    File4 = filename:join(DataDir, reg_m4),
    File5 = filename:join(DataDir, reg_m5),
    File6 = filename:join(DataDir, reg_m6),

    error = ic:gen(File1, stdopts(OutDir)++[{preproc_flags,
						   "-I" ++ DataDir}] ),
    
    error = ic:gen(File2, stdopts(OutDir)++[{preproc_flags,
						   "-I" ++ DataDir}] ),
    
    error = ic:gen(File3, stdopts(OutDir)++[{preproc_flags,
						   "-I" ++ DataDir}] ),
    
    ok = ic:gen(File4, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    
    error = ic:gen(File5, stdopts(OutDir)++[{preproc_flags,
						   "-I" ++ DataDir}] ),
    
    error = ic:gen(File6, stdopts(OutDir)++[{preproc_flags,
						   "-I" ++ DataDir}] ),
    ok.




%%-----------------------------------------------------------------
%% Test Case: IFR registration with realy uggly placed pragmas
%%-----------------------------------------------------------------
%% Checks that IFR object is correctly registered under really uggly pragma engagement.
uggly_pragmas(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(uggly_pragmas_run(Config)).

uggly_pragmas_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(ifr_pragma_reg),
    File0 = filename:join(DataDir, uggly),

    ok = ic:gen(File0, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}]),

    ok = compile(OutDir, uggly_pragma_files()),
    code:add_pathz(OutDir),

    %% OE_register for all files
    ok = 'oe_uggly':'oe_register'(),
    
    %% Pragma registration test
    OE_IFR = orber_ifr:find_repository(),
    io:format("~n##### Starting the test case #####~n"),
 
    check_pragma_effect(OE_IFR, "IDL:M:1.0"),
    check_pragma_effect(OE_IFR, "LOCAL:SomeLocalId:10"),
    check_pragma_effect(OE_IFR, "LOCAL:SomeLocalId:11"),
    check_pragma_effect(OE_IFR, "LOCAL:SomeLocalId:17"),
    check_pragma_effect(OE_IFR, "LOCAL:SomeLocalId:34"),
    check_pragma_effect(OE_IFR, "IDL:Exc1:2.2"),
    check_pragma_effect(OE_IFR, "IDL:Exc2:2.2"),
    check_pragma_effect(OE_IFR, "IDL:S:1.0"),
    check_pragma_effect(OE_IFR, "IDL:U:1.0"),
    check_pragma_effect(OE_IFR, "LOCAL:SomeLocalId:23"),
    
    %% OE_unregister for all files
    ok = 'oe_uggly':'oe_unregister'(),

    code:del_path(OutDir),
    ok.


uggly_pragma_files() -> ['oe_uggly'].




%%----------------------------


stdopts(OutDir) ->
    [{outdir, OutDir}, {maxerrs, infinity}].


compile(Dir, Files) ->
    compile(Dir, Files, []).

compile(Dir, Files, Opts) ->
    {ok, Cwd} = file:get_cwd(),
    file:set_cwd(Dir),
    io:format("Changing to ~p~n", [Dir]),
    case catch do_compile(Files, Opts) of
	ok ->
	    file:set_cwd(Cwd);
	Err ->
	    file:set_cwd(Cwd),
	    test_server:fail(Err)
    end.

do_compile([], _Opts) -> ok;
do_compile([F | Fs], Opts) ->
    io:format("Compiling ~p", [F]),
    case compile:file(F, Opts) of
	ok ->
	    io:format(" ok~n", []),
	    do_load(F, Opts),
	    do_compile(Fs, Opts);
	{ok, _} ->
	    io:format(" ok~n", []),
	    do_load(F, Opts),
	    do_compile(Fs, Opts);
	{ok, _, _} ->
	    io:format(" ok~n", []),
	    do_load(F, Opts),
	    do_compile(Fs, Opts);
	Err -> 
	    io:format(" error: ~p~n", [Err]),
	    Err
    end.

do_load(File, Opts) ->
    case lists:member(load, Opts) of
	true ->
	    io:format("Loading file ~p", [File]),
	    code:purge(File),
	    R = code:load_abs(File),
	    io:format("Loaded: ~p", [R]);
	false ->
	    ok
    end.


to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) -> X.



