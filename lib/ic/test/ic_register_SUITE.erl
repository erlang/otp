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
%% File: ic_register_SUITE.erl
%% 
%% Description:
%% Test suite for the IFR object registration
%%
%%-----------------------------------------------------------------
-module(ic_register_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, ifr_reg_unreg/1]).
-export([ifr_inheritence_reg/1,ifr_reg_unreg_with_inheritence/1]).
-export([ifr_reg_unreg_with_inheritence_bad_order/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

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
    [ifr_reg_unreg, ifr_reg_unreg_with_inheritence,
     ifr_reg_unreg_with_inheritence_bad_order,
     ifr_inheritence_reg].

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
%% Test Case: IFR type registration
%%-----------------------------------------------------------------
%% Checks that the generated register/unregister 
%% code for the IFR is correct.
ifr_reg_unreg(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(ifr_reg_unregt_run(Config)).

ifr_reg_unregt_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(ifr_reg_unreg),
    File0 = filename:join(DataDir, reg_m8),
    File1 = filename:join(DataDir, reg_m9),
    File2 = filename:join(DataDir, reg_m10),
    ok = ic:gen(File0, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File0, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File1, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File1, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File2, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File2, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = compile(OutDir, ifr_reg_unreg_files()),
    code:add_pathz(OutDir),
    ok = 'oe_reg_m8':'oe_register'(),
    ok = 'oe_reg_m9':'oe_register'(),
    ok = 'oe_reg_m10':'oe_register'(),
    ok = 'oe_reg_m10':'oe_unregister'(),
    ok = 'oe_reg_m9':'oe_unregister'(),
    ok = 'oe_reg_m8':'oe_unregister'(),
    code:del_path(OutDir),
    ok.

ifr_reg_unreg_files() -> ['oe_reg_m8', 'oe_reg_m9', 'oe_reg_m10'].



%%-----------------------------------------------------------------
%% Test Case: IFR registration when object inheritence 
%%            is applied and registered.
%%-----------------------------------------------------------------
%% Checks that the generated register/unregister 
%% code for the IFR is correct, and works even when
%% the object inheritence is registered. This fixes
%% two bugs in ifr that caused crash when trying to
%% use OE_register/OE_unregister in a sequence of
%% compiled files that contained interfaces who
%% inherited others in sequence.
ifr_reg_unreg_with_inheritence(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(ifr_reg_unreg_with_inheritence_run(Config)).

ifr_reg_unreg_with_inheritence_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(ifr_reg_unreg),
    File0 = filename:join(DataDir, reg_m8),
    File1 = filename:join(DataDir, reg_m9),
    File2 = filename:join(DataDir, reg_m10),
    File3 = filename:join(DataDir, reg_m11),
    File4 = filename:join(DataDir, reg_m12),
    ok = ic:gen(File0, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File0, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File1, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File1, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File2, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File2, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File3, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File3, stdopts(OutDir)++[silent2, {preproc_flags,
						"-I" ++ DataDir}]),
    ok = ic:gen(File4, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File4, stdopts(OutDir)++[silent2, {preproc_flags,
						"-I" ++ DataDir}]),
    ok = compile(OutDir, ifr_reg_unreg_with_inheritence_files()),
    code:add_pathz(OutDir),
    ok = 'oe_reg_m8':'oe_register'(),
    ok = 'oe_reg_m9':'oe_register'(),
    ok = 'oe_reg_m10':'oe_register'(),
    ok = 'oe_reg_m11':'oe_register'(),
    ok = 'oe_reg_m12':'oe_register'(),
    ok = 'oe_reg_m8':'oe_unregister'(),
    ok = 'oe_reg_m9':'oe_unregister'(),
    ok = 'oe_reg_m10':'oe_unregister'(),
    ok = 'oe_reg_m11':'oe_unregister'(),
    ok = 'oe_reg_m12':'oe_unregister'(),
    code:del_path(OutDir),
    ok.

ifr_reg_unreg_with_inheritence_files() -> 
    ['oe_reg_m8', 'oe_reg_m9', 'oe_reg_m10', 'oe_reg_m11', 'oe_reg_m12'].





%%-----------------------------------------------------------------
%% Test Case: IFR registration when object inheritence 
%%            is applied and registered in a bad order.
%%            Modules included and used from an ifr object
%%            are not allready registered when the current 
%%            object is getting registered.
%%-----------------------------------------------------------------
ifr_reg_unreg_with_inheritence_bad_order(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(ifr_reg_unreg_with_inheritence_bad_order_run(Config)).

ifr_reg_unreg_with_inheritence_bad_order_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(ifr_reg_unreg),
    File1 = filename:join(DataDir, reg_m9),
    File2 = filename:join(DataDir, reg_m10),
    File4 = filename:join(DataDir, reg_m12),
    ok = ic:gen(File1, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File1, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File2, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File2, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File4, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File4, stdopts(OutDir)++[silent2, {preproc_flags,
						"-I" ++ DataDir}]),
    ok = compile(OutDir, ifr_reg_unreg_with_inheritence_files()),
    code:add_pathz(OutDir),
    case catch 'oe_reg_m12':'oe_register'() of
	{'EXIT',Reason1} ->
	    io:format("IFR object missing detected : ~p~n",[Reason1]),
	    true;
	_ ->
	    test_server:fail("Failed to detect object missing : IDL:M1:1.0~n")
    end,
    ok = 'oe_reg_m9':'oe_register'(),
    case catch 'oe_reg_m10':'oe_register'() of
	{'EXIT',Reason2} ->
	    io:format("IFR object missing detected : ~p~n",[Reason2]),
	    true;
	_ ->
	    test_server:fail("Failed to detect object missing : IDL:M0:1.0~n")
    end,
    ok = 'oe_reg_m9':'oe_unregister'(),
    code:del_path(OutDir),
    ok.

%%-----------------------------------------------------------------
%% Test Case: IFR registration with inheritence is correctly registered
%%-----------------------------------------------------------------
ifr_inheritence_reg(Config) when is_list(Config) ->
    ?REMAP_EXCEPT(ifr_inh_reg_run(Config)).

ifr_inh_reg_run(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(ifr_reg_unreg),
    File0 = filename:join(DataDir, reg_m8),
    File1 = filename:join(DataDir, reg_m9),
    File2 = filename:join(DataDir, reg_m10),
    File3 = filename:join(DataDir, reg_m11),
    File4 = filename:join(DataDir, reg_m12),
    ok = ic:gen(File0, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File0, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File1, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File1, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File2, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File2, stdopts(OutDir)++[silent2, {preproc_flags,
					       "-I" ++ DataDir}]),
    ok = ic:gen(File3, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File3, stdopts(OutDir)++[silent2, {preproc_flags,
						"-I" ++ DataDir}]),
    ok = ic:gen(File4, stdopts(OutDir)++[{preproc_flags,
						"-I" ++ DataDir}] ),
    {ok, []} = ic:gen(File4, stdopts(OutDir)++[silent2, {preproc_flags,
						"-I" ++ DataDir}]),
    ok = compile(OutDir, ifr_reg_unreg_with_inheritence_files()),
    code:add_pathz(OutDir),
    %% OE_register for all files
    ok = 'oe_reg_m8':'oe_register'(),
    ok = 'oe_reg_m9':'oe_register'(),
    ok = 'oe_reg_m10':'oe_register'(),
    ok = 'oe_reg_m11':'oe_register'(),
    ok = 'oe_reg_m12':'oe_register'(),
    
    %% Inheritence registration test
    OE_IFR = orber_ifr:find_repository(),
    %% Interfaces that not inherit from other interfaces
    [] = get_inh(OE_IFR, "IDL:m0/i0:1.0"),
    [] = get_inh(OE_IFR, "IDL:m1/i1:1.0"),
    [] = get_inh(OE_IFR, "IDL:m3/i3:1.0"),
    %% Interfaces that inherit from other interfaces
    ["IDL:m1/i1:1.0"] = get_inh(OE_IFR, "IDL:m2/i2:1.0"),
    ["IDL:m1/i1:1.0","IDL:m2/i2:1.0"] = get_inh(OE_IFR, "IDL:m4/i4:1.0"),
    ["IDL:m3/i3:1.0"] = get_inh(OE_IFR, "IDL:m4/i5:1.0"),
    
    %% OE_unregister for all files
    ok = 'oe_reg_m8':'oe_unregister'(),
    ok = 'oe_reg_m9':'oe_unregister'(),
    ok = 'oe_reg_m10':'oe_unregister'(),
    ok = 'oe_reg_m11':'oe_unregister'(),
    ok = 'oe_reg_m12':'oe_unregister'(),
    code:del_path(OutDir),
    ok.


get_inh(OE_IFR,ID) ->
    OE_CURRENT = orber_ifr:lookup_id(OE_IFR,ID),
    INH_LIST = orber_ifr:get_base_interfaces(OE_CURRENT),
    case INH_LIST of
	[] ->
	    io:format("~nInterface ~p inherits from nobody.~n",[ID]),
	    [];
	_ ->
	    print_inh_list_ids(ID, INH_LIST, [])
    end.

print_inh_list_ids(_ID, [], Acc) ->
    lists:reverse(Acc);
print_inh_list_ids(ID, [H|T], Acc) ->
    io:format("~n"),
    Parent = orber_ifr:get_id(H),
    io:format("Interface ~p inherits from ~p.~n", [ID, Parent]),
    print_inh_list_ids(ID, T, [Parent|Acc]).




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


















