%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%%----------------------------------------------------------------------
%%% Purpose : Test suite for the IDL compiler
%%%----------------------------------------------------------------------

-module(ic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).


-include_lib("orber/src/orber_ifr.hrl").
-include_lib("orber/src/ifr_objects.hrl").
-include_lib("orber/include/ifr_types.hrl").


%% The type cases
-export([ type_norm/1]).

%% The syntax case
-export([]).
-export([syntax1/1, syntax2/1, syntax3/1, syntax4/1, syntax5/1, syntax6/1]).

%% The constant cases
-export([]).
-export([const_norm/1, const_bad_tk/1, const_bad_type/1]).
-export([const_bad_comb/1]).

%% The union cases
-export([]).
-export([union_norm/1, union_type/1, union_mult_err/1, union_case_mult/1]).
-export([union_default/1]).

%% The enum cases
-export([]).
-export([enum_norm/1]).

%% The struct cases
-export([]).
-export([struct_norm/1]).

%% The oneway cases
-export([]).                                                  
-export([oneway_norm/1, oneway_raises/1, oneway_out/1, oneway_void/1, oneway_followed/1]).

%% The attributes cases
-export([]).
-export([attr_norm/1]).

%% The raises registration case
-export([raises_reg/1]).     


%% The typeID case

%% general stuff
-export([]).
-export([typeid/1, undef_id/1, dir/1, nasty_names/1, coss/1, mult_ids/1]).
-export([forward/1, include/1, app_test/1]).

%% inheritance stuff
-export([ inherit_norm/1, inherit_warn/1, inherit_err/1]).

%% Standard options to the ic compiler, NOTE unholy use of OutDir

-define(OUT(X), filename:join([proplists:get_value(priv_dir, Config), gen, to_list(X)])).


%% Top of cases

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [app_test, {group, const}, {group, union},
     {group, enum}, {group, attr}, {group, type},
     {group, struct}, {group, general}, {group, inherit},
     {group, oneway}, {group, syntax}, raises_reg].

groups() -> 
    [{const, [],
      [const_norm, const_bad_tk, const_bad_type,
       const_bad_comb]},
     {union, [],
      [union_norm, union_type, union_mult_err,
       union_case_mult, union_default]},
     {enum, [], [enum_norm]}, {struct, [], [struct_norm]},
     {general, [],
      [typeid, undef_id, mult_ids, forward, include,
       nasty_names]},
     {inherit, [],
      [inherit_norm, inherit_warn, inherit_err]},
     {oneway, [],
      [oneway_norm, oneway_out, oneway_raises, oneway_void,
       oneway_followed]},
     {attr, [], [attr_norm]}, {type, [], [type_norm]},
     {syntax, [],
      [syntax1, syntax2, syntax3, syntax4, syntax5, syntax6]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


app_test(_Config) ->
    ok=test_server:app_test(ic),
    ok.

%%---------------------------------------------------------------------
%%
%% Test of constant expressions.
%%

%% Checks normal constant types and values
const_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(const_norm),
    File = filename:join(DataDir, c_norm),
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, const_norm_files()),
    ok.

%% Checks when the constant value doesn't match the declared type
const_bad_tk(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, c_err1),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(18, bad_tk_match, R),
    ok.    

%% Checks operands of ops are of correct type
const_bad_type(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, c_err2),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(4, bad_type, R),
    ok.    

%% Checks operands of ops are of conflicting types
const_bad_comb(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, c_err3),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(3, bad_type_combination, R),
    ok.    


%% Checks that normal union declarations works.
union_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(union_norm),
    File = filename:join(DataDir, u_norm),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, union_norm_files()),
    ok.


%% Checks OTP-2007
%% Checks that default cases are correct in type code.
union_default(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(union_default),
    File = filename:join(DataDir, u_default),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, union_default_files(), [load]),
    TkList = i1:oe_get_interface(),
    check_label("op0", 0, TkList),
    check_label("op1", 1, TkList),
    check_label("op2", 2, TkList),
    check_label("op3", -1, TkList),
    ok.

check_label(Id, N, List) ->
    case lists:keysearch(Id, 1, List) of
	{value, {_, {{_, _, _, _, D, L}, _, _}}} ->
	    if  D /= N ->
		    test_server:fail({bad_default_num, D, N});
		D /= -1 ->
		    case lists:nth(D+1, L) of
			T when element(1, T) == default ->
			    ok;
			_Que ->
			    test_server:fail({bad_default_list, D, L})
		    end;
		true ->
		    %% D = N = -1, just check that there is no default label
		    case lists:keysearch(default, 1, L) of
			false ->
			    ok;
			_ ->
			    test_server:fail({bad_default_label, D, L})
		    end
	    end;
	_ ->
	    test_server:fail({'no_such_op!', Id, List})
    end.

%% Checks that errors are detected. Check that mismatch between case 
%% value and declared discriminator type is detected.
union_type(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, u_type),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(28, bad_case_type, R),
    ok.    


%% Check that multiple declared declarators are caught.
%% Also check that if the discriminator is an enum, then the enum name
%% must not be used as a declarator in the union switch (declarator
%% as opposed to label).
union_mult_err(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, u_mult),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(8, multiply_defined, R),
    ok.    

%% Check that multiply defined case labels are found  in the
%% correct order 
union_case_mult(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, u_case_mult),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(7, multiple_cases, R),
    ok.    


%%--------------------------------------------------------------------
%%
%% Enum cases
%%
%%Checks that normal enum declarations works.
enum_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(enum_norm),
    File = filename:join(DataDir, enum),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, enum_norm_files()),
    ok.


%%--------------------------------------------------------------------
%%
%% Struct cases
%%
%% Checks that normal struct declarations works.
struct_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(struct_norm),
    File = filename:join(DataDir, struct),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, struct_norm_files()),
    Mod = ridiculous_name_to_avoid_clash_svenne,
    TestFile = filename:join(OutDir, Mod),
    ok = gen_struct_file(TestFile, Mod),
    ok = compile(OutDir, [Mod], [load]),
%%    {ok, Mod, []} = compile:file(TestFile, 
%%				       [{i, OutDir}, {outdir, OutDir},
%%					return, load]),
    ok = Mod:test(),
    ok.


%%--------------------------------------------------------------------
%%
%% General cases
%%

%% coss (add sometimes, takes 440 seconds!)
%% Check that type id's are generated correctly
typeid(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(typeid),
    File = filename:join(DataDir, typeid),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, typeid_files(), [load]),
    "IDL:I1:1.0" = 'I1':'typeID'(),
    "IDL:M1/I1:1.0" = 'M1_I1':'typeID'(),
    "IDL:M2/M1/I1:1.0" = 'M2_M1_I1':'typeID'(),
    "IDL:M3/M2/M1/I1:1.0" = 'M3_M2_M1_I1':'typeID'(),
    ok.


%%% This test case is removed because there's no way to test this from
%%% an automated test suite.
%% Check that relative directories work, absolute is used in
%% all other cases in the suite.
dir(Config) when is_list(Config) ->
    ok;
dir(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    %% Needs a unique directory (any better way?)
    OutDir = mk_unique("oe_the_dir"),
    
    %% More unique names
    File = filename:join(DataDir, mk_unique("oe_the_file")),
    Const = mk_unique("oe_the_constant"),
    Mod  = list_to_atom(File),
    Func = list_to_atom(Const),
    
    %% Generate a unique IDL file with a single constant
    gen_file(File, Const),
    
    ok = ic:gen(File, stdopts(OutDir)),
    ok = compile(OutDir, [load]),
    19955 = Mod:Func(),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, [load]),
    19955 = Mod:Func(),

    ok = ic:gen(File),
%%%    ok = compile(".", [load]),
    ok.

%% Check that various undefied id's are detected correctly
undef_id(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, undef_id),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(16, tk_not_found, R),
    ok.

%% Check that multiply defined ids are caught.
mult_ids(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, mult_ids),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(22, multiply_defined, R),
    ok.


%% Check that various nasty names can be generated.
%% Try to provoke name clashes and name conflicts with
%% Erlang and IDL
nasty_names(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(nasty_names),
    File = filename:join(DataDir, nasty),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, nasty_names_files(), [load]),
    ok.

%% Check that the Coss standard specification works.
coss(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(coss),
    File = filename:join(DataDir, 'Coss'),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, [_W1]} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, []),
    ok.

%% Check that forward declaratios work.
forward(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(forward),
    File = filename:join(DataDir, forward),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, forward_files(), [load]),
    ok.

%% Check that various undefied id's are detected correctly
include(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, include),
    error = ic:gen(File,  stdopts(OutDir)++[{preproc_flags,"-I" ++ DataDir}]),
    {error, [], R} = 
	ic:gen(File, stdopts(OutDir)++[{preproc_flags,"-I" ++ DataDir},silent2]),
    case lists:map(fun(D) ->
			   filename:rootname(filename:basename(element(3, D)))
		   end,
		   lists:sort(R)) of
	["include",
	 "include2",
	 "include2",
	 "include3"] ->
	    ok;
	RRR ->
	    test_server:fail({bad_include_file, RRR})
    end,
    ok.




%%--------------------------------------------------------------------
%%
%% Inhertit cases
%%

%% Checks that normal inheritance works.
inherit_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(inherit_norm),
    File = filename:join(DataDir, inherit),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, _Ws} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, inherit_norm_files(), [load]),
    
    %% Now check constant values:
    9 = m1_I1:c1(),

    9 = m1_I2:c1(),
    14 = m1_I2:c2(),
    27 = m1_I2:c3(),

    50 = m1_I3:c1(),
    14 = m1_I3:c2(),
    27 = m1_I3:c3(),
    91 = m1_I3:c4(),
    100 = m1_I3:c5(),
    ok.

%% Check that various inheritance shadowing is detected
inherit_warn(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, inherit_warn),
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(7, inherit_name_shadow, R),
    ok.

%% Check that various inheritance errors is detected
inherit_err(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, inherit_err),
    error = ic:gen(File, stdopts(OutDir)),
    {error, _Ws, R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(21, inherit_name_collision, R),
    ok.


%% Checks that normal oneway operations works.
oneway_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(oneway_norm),
    File = filename:join(DataDir, one),
    
    ok = ic:gen(File, stdopts(OutDir)),
    ok = compile(OutDir, oneway_norm_files(), [load]),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, oneway_norm_files(), [load]),
    ok.

%% Check that non-void oneways are detected.
oneway_void(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, one_void),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(2, bad_oneway_type, R),
    ok.

%% Check that oneways cannot raise exceptions.
oneway_raises(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, one_raises),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(3, oneway_raises, R),
    ok.

%% Check that illegal out parameters are detected
oneway_out(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, one_out),
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(2, oneway_outparams, R),
    ok.

%% Checks that normal oneways, followed by other operations.
oneway_followed(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(oneway_followed),
    File = filename:join(DataDir, one_followed),
    
    ok = ic:gen(File, stdopts(OutDir)),
    ok = compile(OutDir, oneway_followed_files(), [load]),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, oneway_followed_files(), [load]),
    ok.


%% Checks that normal attr operations works.
attr_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(attr_norm),
    File = filename:join(DataDir, attr),
    
    ok = ic:gen(File, stdopts(OutDir)),
    ok = compile(OutDir, attr_norm_files(), [load]),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, attr_norm_files(), [load]),
    ok.


%% Checks all types are handled.
type_norm(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(type_norm),
    File = filename:join(DataDir, type),
    
    ok = ic:gen(File, stdopts(OutDir)),
    ok = compile(OutDir, type_norm_files(), [load]),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, type_norm_files(), [load]),
    ok.

syntax1(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, syntax1),

    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(1, parse_error, R),
    ok.

syntax2(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, syntax2),

    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(1, parse_error, R),
    ok.

syntax3(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, syntax3),

    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(1, parse_error, R),
    ok.

syntax4(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, syntax4),

    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(1, parse_error, R),
    ok.

syntax5(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, syntax5),

    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(1, parse_error, R),
    ok.

syntax6(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(slask),
    File = filename:join(DataDir, syntax6),
    
    error = ic:gen(File, stdopts(OutDir)),
    {error, [], R} =
	ic:gen(File, stdopts(OutDir)++[silent2]),
    check_errors(1, parse_error, R),
    ok.



%%--------------------------------------------------------------------
%%
%%  Checks RAISES to be registered under IFR operation registration
%% ( OTP-2102 )
%%

%% Check that exceptions are really registered to operations.
raises_reg(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    OutDir = ?OUT(raises_reg_check),
    File = filename:join(DataDir, raises_reg),
    
    ok = ic:gen(File, stdopts(OutDir)),
    {ok, []} = ic:gen(File, stdopts(OutDir)++[silent2]),
    ok = compile(OutDir, raises_reg_files(), [load]),

    set_up('oe_raises_reg'),

    io:format("~n##### Starting the test case #####~n"), 
    io:format("Checking for existance of exception : ~s~n",["IDL:Raises_RegModule/Exception_1:1.0"]),
    raises_register_check("IDL:Raises_RegModule/R_R/op:1.0","IDL:Raises_RegModule/Exception_1:1.0"),

    io:format("Checking for existance of exception : ~s~n",["IDL:Raises_RegModule/Exception_2:1.0"]),
    raises_register_check("IDL:Raises_RegModule/R_R/op:1.0","IDL:Raises_RegModule/Exception_2:1.0"),

    io:format("Checking for existance of exception : ~s~n",["IDL:Raises_RegModule/XXXXXXXX:1.0"]),
    raises_register_check("IDL:Raises_RegModule/R_R/op:1.0","IDL:RaisesModule/XXXXXXXX:1.0"),

    set_down('oe_raises_reg'),

    ok.

set_up(Register) ->
    io:format("Setting up.....~n"),
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    orber:install([node()]),
    orber:start(),
    io:format("Running OE_register()~n"),
    Register:'oe_register'().

set_down(Register) ->
    io:format("Running OE_unregister()~n"),
    Register:'oe_unregister'(),
    io:format("Setting down.....~n"),
    orber:stop(),
    orber:uninstall(),
    mnesia:stop(),
    mnesia:delete_schema([node()]).


raises_register_check(OpId,ExcId) ->
    case is_valid_exc(OpId,ExcId) of
	true ->
	    ok;     %   Because right exception where found, 
                    %   the test succeeds for normal cases.
  	false ->
	    ok;     %   Because the exception tested, is not 
                    %   registered for that operation.
        FailReason ->
	    test_server:fail({FailReason, OpId, ExcId})
		    % Because the test descovered errors in a previous 
                    % stage, or no exceptions where registered att all. 
                    % ( This testcase assumes that operations to be 
                    %   checked allways raise excption(s) )
    end.
    
is_valid_exc(OpId,ExcId) ->
    OE_IFR = orber_ifr:find_repository(),
    OpDef = orber_ifr:'Repository_lookup_id'(OE_IFR,OpId),
    ExcDefList = orber_ifr:get_exceptions(OpDef),
    case ExcDefList of
	[] ->
	    no_exceptions_registered;
	_ ->
	    ExcDef=orber_ifr:lookup_id(OE_IFR,ExcId),  
	    lists:member(ExcDef,ExcDefList)
    end.

%%--------------------------------------------------------------------
%%
%% Utilities


stdopts(OutDir) ->
    [{outdir, OutDir},{maxerrs, infinity}].

mk_unique(Prefix) ->
    {A,B,C} = now(),
    Prefix++"_"++integer_to_list(A)++"_"++integer_to_list(B)++"_"++
	integer_to_list(C).

gen_file(File, Const) ->
    {ok, Fd} = file:open(File++".idl", [write]),
    io:format(Fd, "interface ~s {~n", [File]),
    io:format(Fd, "    const long ~s = 19955;~n", [Const]),
    io:format(Fd, "};~n", []),
    file:close(Fd).


%% Compile all files in Dir. Used for checking that valid Erlang has
%% been generated.
%%compile(Dir) ->
%%    compile(Dir, []).
%%compile(Dir, Opts) ->
%%    {ok, Cwd} = file:get_cwd(),
%%    catch do_compile(Dir, Opts),
%%    file:set_cwd(Cwd).

%%do_compile(Dir, Opts) ->
%%    ok = file:set_cwd(Dir),
%%    up_to_date = ts_make_erl:all(Opts),
%%    ok.

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


%% Check that ErrList consists of exactly Num errors of type ErrType
check_errors(Num, ErrType, ErrList) ->
    Num = length(ErrList),
    lists:foreach(fun(T) ->
			  case catch element(1, element(4, T)) of
			      ErrType -> ok;
			      Else -> 
				  test_server:fail({bad, ErrType, Else})
			  end end, ErrList).

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) -> X.


%% File must be an atom
gen_struct_file(File, Mod) ->
    
    {ok, Fd} = file:open(to_list(File)++".erl", [write]),
    io:format(Fd, "~n", []),
    io:format(Fd, "-module(~p).~n", [Mod]),
    io:format(Fd, "-export([test/0]).~n", []),
    io:format(Fd, "-include(\"oe_struct.hrl\").~n", []),
    io:format(Fd, "test() ->~n", []),
    io:format(Fd, "    A = #'S1'{a=99, b=$a, s=\"123456789\"},~n", []),
    io:format(Fd, "    B = #'S2'{a=9, b=#'S2_S3'{a=1, b=9, b1=5, c=$2},~n", []),
    io:format(Fd, "	      c=[#'S1'{a=1}, #'S1'{a=2}],~n", []),
    io:format(Fd, 
"	      c2=[#'S1'{a=2}, #'S1'{a=3}, #'S1'{a=2}, #'S1'{a=3}]},~n", []),
    io:format(Fd, "    C = #'S2_S3'{a=11, b=999, b1=19},~n", []),
    io:format(Fd, "    D = #s4{a=7},~n", []),
    io:format(Fd, "    E = {1, #'U1_S5'{a=3}},~n", []),
    io:format(Fd, "    F = {2, {$b, #'U1_U2_s6'{a=6, b=false}}},~n", []),
    io:format(Fd, "    ok.~n", []),
    file:close(Fd).


union_norm_files() -> ['oe_u_norm'].
union_default_files() -> ['oe_u_default', i1].

typeid_files() -> ['oe_typeid', 'M3_M2_M1_I1', 'M2_M1_I1', 'M1_I1', 'I1'].

struct_norm_files() -> ['oe_struct'].
oneway_norm_files() -> ['oe_one', 'I1'].
oneway_followed_files() -> ['oe_one_followed', 'I1'].
nasty_names_files() -> ['oe_nasty', 'I2', 'I1'].

inherit_norm_files() -> [m1_I3, m1_I2, m1_I1, 'oe_inherit', 'I4', 'I3',
			  'I2', 'I1'].

forward_files() -> [i1, 'oe_forward'].
enum_norm_files() -> ['oe_enum'].
const_norm_files() -> ['oe_c_norm'].
attr_norm_files() -> ['oe_attr', 'I1', 'I2'].
type_norm_files() -> ['oe_type'].

raises_reg_files() -> ['oe_raises_reg'].
















