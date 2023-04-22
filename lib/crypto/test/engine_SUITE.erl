%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017-2022. All Rights Reserved.
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

-module(engine_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
         suite/0,
         all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         get_all_possible_methods/0,
         get_all_possible_methods/1,
         engine_load_register_method/0,
         engine_load_register_method/1,
         engine_list/0,
         engine_list/1,
         get_id_and_name/0,
         get_id_and_name/1,
         engine_by_id/0,
         engine_by_id/1,
         bad_arguments/0,
         bad_arguments/1,
         unknown_engine/0,
         unknown_engine/1,
         pre_command_fail_bad_value/0,
         pre_command_fail_bad_value/1,
         pre_command_fail_bad_key/0,
         pre_command_fail_bad_key/1,
         failed_engine_init/0,
         failed_engine_init/1,
         ctrl_cmd_string/0,
         ctrl_cmd_string/1,
         ctrl_cmd_string_optional/0,
         ctrl_cmd_string_optional/1,
         ensure_load/0,
         ensure_load/1,
	 gc_clean/0,
	 gc_clean/1,
         sign_verify_rsa/1,
         sign_verify_rsa_fake/1,
         sign_verify_dsa/1,
         sign_verify_ecdsa/1,
         sign_verify_eddsa/1,
         sign_verify_rsa_pwd/1,
         sign_verify_rsa_pwd_bad_pwd/1,
         priv_encrypt_pub_decrypt_rsa/1,
         priv_encrypt_pub_decrypt_rsa_pwd/1,
         pub_encrypt_priv_decrypt_rsa/1,
         pub_encrypt_priv_decrypt_rsa_pwd/1,
         get_pub_from_priv_key_rsa/1,
         get_pub_from_priv_key_rsa_pwd/1,
         get_pub_from_priv_key_rsa_pwd_no_pwd/1,
         get_pub_from_priv_key_rsa_pwd_bad_pwd/1,
         get_pub_from_priv_key_dsa/1,
         get_pub_from_priv_key_ecdsa/1
        ]).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds, 10}}
    ].

all() ->
    [
     get_all_possible_methods,
     engine_load_register_method,
     engine_list,
     get_id_and_name,
     engine_by_id,
     bad_arguments,
     unknown_engine,
     pre_command_fail_bad_value,
     pre_command_fail_bad_key,
     failed_engine_init,
     ctrl_cmd_string,
     ctrl_cmd_string_optional,
     ensure_load,
     gc_clean,
     {group, engine_stored_key},
     {group, engine_fakes_rsa}
    ].

groups() ->
    [{engine_stored_key, [],
      [
       sign_verify_rsa,
       sign_verify_dsa,
       sign_verify_ecdsa,
       sign_verify_eddsa,
       sign_verify_rsa_pwd,
       sign_verify_rsa_pwd_bad_pwd,
       priv_encrypt_pub_decrypt_rsa,
       priv_encrypt_pub_decrypt_rsa_pwd,
       pub_encrypt_priv_decrypt_rsa,
       pub_encrypt_priv_decrypt_rsa_pwd,
       get_pub_from_priv_key_rsa,
       get_pub_from_priv_key_rsa_pwd,
       get_pub_from_priv_key_rsa_pwd_no_pwd,
       get_pub_from_priv_key_rsa_pwd_bad_pwd,
       get_pub_from_priv_key_dsa,
       get_pub_from_priv_key_ecdsa
      ]},
    {engine_fakes_rsa, [],
     [
      sign_verify_rsa_fake
     ]}
    ].


init_per_suite(Config) ->
    try {os:type(), crypto:info_lib()} of
        {_, [{_,_, <<"OpenSSL 1.0.1s-freebsd  1 Mar 2016">>}]} ->
            {skip, "Problem with engine on OpenSSL 1.0.1s-freebsd"};

        {_, [{_,_,<<"LibreSSL 2.1.",_/binary>>}]} ->
            {skip, "Problem with engine on older LibreSSL 2.1.*"};

        {{unix,darwin}, _} ->
            {skip, "Engine unsupported on Darwin"};
        
        {{win32,_}, _} ->
            {skip, "Engine unsupported on Windows"};
        
        {OS, Res} ->
            ct:log("crypto:info_lib() -> ~p\nos:type() -> ~p", [Res,OS]),
            try crypto:start() of
                ok ->
                    Config;
                {error,{already_started,crypto}} ->
                    Config
            catch _:_ ->
                    {skip, "Crypto did not start"}
            end
    catch _:_ ->
	    {skip, "Crypto not loaded"}
    end.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
init_per_group(engine_stored_key, Config) ->
    group_load_engine(Config,  [engine_method_rsa]);
init_per_group(engine_fakes_rsa, Config) ->
    case crypto:info_lib() of
        [{<<"OpenSSL">>,LibVer,_}] when is_integer(LibVer), LibVer >= 16#10100000 ->
            CryptoInfo = crypto:info(),
            ct:log("~p:~p  crypto:info() = ~p",[?MODULE,?LINE,CryptoInfo]),
            case CryptoInfo of
                #{link_type := static} ->
                    ct:log("~p:~p  Statically linked",[?MODULE,?LINE]),
                    {skip, "Statically linked"};
                _Info ->
                    %% Dynamically linked; use fake engine rsa implementation
                    group_load_engine(Config,  [])
            end;
        _ ->
            {skip, "Too low OpenSSL cryptolib version"}
    end;
init_per_group(_Group, Config0) ->
    Config0.

group_load_engine(Config, ExcludeMthds) ->
    case load_storage_engine(Config) of
        {ok, E} ->
	    ok = crypto:engine_register(E, crypto:engine_get_all_methods() -- ExcludeMthds),
            KeyDir = key_dir(Config),
            ct:log("storage engine ~p loaded.~nKeyDir = ~p", [E,KeyDir]),
            [{storage_engine,E}, {storage_dir,KeyDir} | Config];
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {error, notsup} ->
            {skip, "Engine not supported on this SSL version"};
        {error, bad_engine_id} ->
            {skip, "Dynamic Engine not supported"};
        Other ->
            ct:log("Engine load failed: ~p",[Other]),
            {fail, "Engine load failed"}
    end.

end_per_group(_, Config) ->
    case proplists:get_value(storage_engine, Config) of
        undefined ->
            ok;
        E ->
            ok = crypto:engine_unload(E)
    end.

%%--------------------------------------------------------------------
init_per_testcase(Case, Config) ->
    HasMD5 = lists:member(md5, crypto:supports(hashs)),
    case Case of
        ensure_load                 when HasMD5==false -> {skip, "md5 not available"};
	engine_load_register_method when HasMD5==false -> {skip, "md5 not available"};
	gc_clean                    when HasMD5==false -> {skip, "md5 not available"};
        _ ->
            case string:tokens(atom_to_list(Case),"_") of
                ["sign","verify",Type|_] ->
                    skip_if_unsup(list_to_atom(Type), Config);
                ["priv","encrypt","pub","decrypt",Type|_] ->
                    skip_if_unsup(list_to_atom(Type), Config);
                ["get","pub","from","priv","key",Type|_] ->
                    skip_if_unsup(list_to_atom(Type), Config);
                _ ->
                    Config
            end
    end.

end_per_testcase(_Case, _Config) ->
    ok.

%%-------------------------------------------------------------------------
%% Test cases starts here.
%%-------------------------------------------------------------------------
get_all_possible_methods() ->
    [{doc, "Just fetch all possible engine methods supported."}].

get_all_possible_methods(Config) when is_list(Config) ->
    try
        List = crypto:engine_get_all_methods(),
        true = erlang:is_list(List),
        ct:log("crypto:engine_get_all_methods() -> ~p\n", [List]),
        ok
    catch
        error:notsup ->
            {skip, "Engine not supported on this SSL version"}
    end.

%%-------------------------------------------------------------------------
engine_load_register_method()->
    [{doc, "Use a dummy md5 engine that does not implement md5"
      "but rather returns a static binary to test that crypto:engine_load "
      "functions works."}].

engine_load_register_method(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try
                Md5Hash1 =  <<106,30,3,246,166,222,229,158,244,217,241,179,50,232,107,109>>,
                Md5Hash1 = crypto:hash(md5, "Don't panic"),
                Md5Hash2 =  <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         <<"LOAD">>],
                                        []) of		    
                    {ok, E} ->
			ok = crypto:engine_register(E, [engine_method_digests]),

                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_to_load_still_original_engine);
                            Md5Hash2 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,

			ok = crypto:engine_unregister(E, [engine_method_digests]),
                        ok = crypto:engine_unload(E),

                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash2 ->
                                ct:fail(fail_to_unload_still_test_engine);
                            Md5Hash1 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_unload_engine)
                        end;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                  {skip, "Engine not supported on this SSL version"}
           end
    end.

%%-------------------------------------------------------------------------
engine_list()->
    [{doc, "Test add and remove engine ID to the SSL internal engine list."}].

engine_list(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->
                        EngineList0 = crypto:engine_list(),
                        false = lists:member(<<"MD5">>, EngineList0),
                        ok = crypto:engine_add(E),
                        [<<"MD5">>] = lists:subtract(crypto:engine_list(), EngineList0),
                        ok = crypto:engine_remove(E),
                        EngineList0 = crypto:engine_list(),
                        ok = crypto:engine_unload(E);
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                  {skip, "Engine not supported on this SSL version"}
           end
    end.

%%-------------------------------------------------------------------------
get_id_and_name()->
    [{doc, "Test fetching id and name from an engine."}].

get_id_and_name(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->
                        <<"MD5">> = crypto:engine_get_id(E),
                        <<"MD5 test engine">> = crypto:engine_get_name(E),
                        ok = crypto:engine_unload(E);
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                  {skip, "Engine not supported on this SSL version"}
           end
    end.

%%-------------------------------------------------------------------------
engine_by_id()->
    [{doc, "Test fetching a new reference the the engine when the"
     "engine id is added to the SSL engine list."}].

engine_by_id(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->
                        case crypto:engine_by_id(<<"MD5">>) of
                            {error,bad_engine_id} ->
                                ok;
                            {ok, _} ->
                                ct:fail(fail_engine_found)
                        end,
                        ok = crypto:engine_add(E),
                        {ok, _E1} = crypto:engine_by_id(<<"MD5">>),
                        ok = crypto:engine_remove(E),
                        ok = crypto:engine_unload(E);
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                  {skip, "Engine not supported on this SSL version"}
           end
    end.

%%-------------------------------------------------------------------------
%% Error cases
bad_arguments()->
    [{doc, "Test different arguments in bad format."}].

bad_arguments(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try
                try
                    crypto:engine_load(fail_engine, [], [])
                of
                    X1 ->
                        ct:fail("1 Got ~p",[X1])
                catch
                    error:badarg ->
                       ok
                end,
                try
                    crypto:engine_load(<<"dynamic">>,
                                       [{<<"SO_PATH">>, Engine},
                                        1,
                                        {<<"ID">>, <<"MD5">>},
                                        <<"LOAD">>],
                                       [])
                of
                    {error,bad_engine_id} ->
                        throw(dynamic_engine_unsupported);
                    X2 ->
                        ct:fail("2 Got ~p",[X2])
                catch
                    error:badarg ->
                       ok
                end,
                try
                    crypto:engine_load(<<"dynamic">>,
                                       [{<<"SO_PATH">>, Engine},
                                        {'ID', <<"MD5">>},
                                        <<"LOAD">>],
                                       [])
                of
                    {error,bad_engine_id} ->    % should have happened in the previous try...catch end!
                        throw(dynamic_engine_unsupported);
                    X3 ->
                        ct:fail("3 Got ~p",[X3])
                catch
                    error:badarg ->
                       ok
                end
          catch
              error:notsup ->
                  {skip, "Engine not supported on this SSL version"};
              throw:dynamic_engine_unsupported ->
                  {skip, "Dynamic Engine not supported"}
          end
    end.

%%-------------------------------------------------------------------------
unknown_engine() ->
    [{doc, "Try to load a non existent engine."}].

unknown_engine(Config) when is_list(Config) ->
    try
        {error, bad_engine_id} = crypto:engine_load(<<"fail_engine">>, [], []),
        ok
    catch
        error:notsup ->
           {skip, "Engine not supported on this SSL version"}
    end.

%%-------------------------------------------------------------------------
pre_command_fail_bad_value() ->
    [{doc, "Test pre command due to bad value"}].

pre_command_fail_bad_value(Config) when is_list(Config) ->
    DataDir = unicode:characters_to_binary(code:priv_dir(crypto)),
    try
        case crypto:engine_load(<<"dynamic">>,
                                [{<<"SO_PATH">>,
                                  <<DataDir/binary, <<"/libfail_engine.so">>/binary >>},
                                 {<<"ID">>, <<"MD5">>},
                                 <<"LOAD">>],
                                []) of
            {error, ctrl_cmd_failed} ->
                ok;
            {error, bad_engine_id} ->
                {skip, "Dynamic Engine not supported"}
        end
    catch
        error:notsup ->
           {skip, "Engine not supported on this SSL version"}
    end.

%%-------------------------------------------------------------------------
pre_command_fail_bad_key() ->
    [{doc, "Test pre command due to bad key"}].

pre_command_fail_bad_key(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_WRONG_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {error, ctrl_cmd_failed} ->
                        ok;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch
       error:notsup ->
          {skip, "Engine not supported on this SSL version"}
   end.

%%-------------------------------------------------------------------------
failed_engine_init()->
    [{doc, "Test failing engine init due to missed pre command"}].

failed_engine_init(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>}],
                                        []) of
                    {error, engine_init_failed} ->
                        ok;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch
       error:notsup ->
          {skip, "Engine not supported on this SSL version"}
   end.

%%-------------------------------------------------------------------------
%% Test the optional flag in ctrl comands
ctrl_cmd_string()->
    [{doc, "Test that a not known optional ctrl command do not fail"}].
ctrl_cmd_string(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->
                        case crypto:engine_ctrl_cmd_string(E, <<"TEST">>, <<"17">>) of
                            ok ->
                                ok = crypto:engine_unload(E),
                                ct:fail(fail_ctrl_cmd_should_fail);
                            {error,ctrl_cmd_failed} ->
                                ok = crypto:engine_unload(E)
                        end;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch
       error:notsup ->
          {skip, "Engine not supported on this SSL version"}
   end.

%%-------------------------------------------------------------------------
ctrl_cmd_string_optional()->
    [{doc, "Test that a not known optional ctrl command do not fail"}].
ctrl_cmd_string_optional(Config) when is_list(Config) ->
    try
        case crypto:get_test_engine() of
            {error, notexist} ->
                {skip, "OTP Test engine not found"};
            {ok, Engine} ->
                case crypto:engine_load(<<"dynamic">>,
                                        [{<<"SO_PATH">>, Engine},
                                         {<<"ID">>, <<"MD5">>},
                                         <<"LOAD">>],
                                        []) of
                    {ok, E} ->
                        case crypto:engine_ctrl_cmd_string(E, <<"TEST">>, <<"17">>, true) of
                            ok ->
                                ok = crypto:engine_unload(E);
                            Err ->
                                ct:log("Error: ~p",[Err]),
                                ok = crypto:engine_unload(E),
                                ct:fail(fail_ctrl_cmd_string)
                        end;
                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
        end
   catch
       error:notsup ->
          {skip, "Engine not supported on this SSL version"}
   end.

%%-------------------------------------------------------------------------
%
%
ensure_load()->
    [{doc, "Test the special ensure load function."}].

ensure_load(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->
            try
                Md5Hash1 =  <<106,30,3,246,166,222,229,158,244,217,241,179,50,232,107,109>>,
                Md5Hash1 = crypto:hash(md5, "Don't panic"),
                Md5Hash2 =  <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,
                case crypto:ensure_engine_loaded(<<"MD5">>, Engine) of
                    {ok, E} ->
			ok = crypto:engine_register(E, [engine_method_digests]),

                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_to_load_still_original_engine);
                            Md5Hash2 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,

                        {ok, E1} = crypto:ensure_engine_loaded(<<"MD5">>, Engine),
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_to_load_still_original_engine);
                            Md5Hash2 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,

                        ok = crypto:ensure_engine_unloaded(E1),
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_test_engine_unloaded);
                            Md5Hash2 ->
				ok;
                            _ ->
                                ct:fail(fail_to_unload_engine)
                        end,

                        {ok, E2} = crypto:ensure_engine_loaded(<<"MD5">>, Engine),
			case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_test_engine_not_loaded);
                            Md5Hash2 ->
				ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,
			ok = crypto:ensure_engine_unloaded(E2),

                        {ok, E3} = crypto:ensure_engine_loaded(<<"MD5">>, Engine),
			case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_test_engine_not_loaded);
                            Md5Hash2 ->
				ok;
                            _ ->
                                ct:fail(fail_to_load_engine)
                        end,

                        ok = crypto:ensure_engine_unloaded(E),
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ct:fail(fail_test_engine_not_loaded);
                            Md5Hash2 ->
                                ok;
                            _ ->
                                ct:fail(fail_to_unload_engine)
                        end,

			ok = crypto:engine_unregister(E3, [engine_method_digests]),
			ok = crypto:ensure_engine_unloaded(E3),
                        case crypto:hash(md5, "Don't panic") of
                            Md5Hash1 ->
                                ok;
                            Md5Hash2 ->
                                ct:fail(fail_to_unload_still_test_engine);
                            _ ->
                                ct:fail(fail_to_unload_engine)
                        end;

                    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
                end
           catch
               error:notsup ->
                   {skip, "Engine not supported on this SSL version"}
           end
    end.

%%-------------------------------------------------------------------------
gc_clean()->
    [{doc, "Test the special ensure load function."}].

gc_clean(Config) when is_list(Config) ->
    case crypto:get_test_engine() of
        {error, notexist} ->
            {skip, "OTP Test engine not found"};
        {ok, Engine} ->

	    Md5Hash1 = <<106,30,3,246,166,222,229,158,244,217,241,179,50,232,107,109>>,
	    Md5Hash1 = crypto:hash(md5, "Don't panic"),
	    Md5Hash2 =  <<0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15>>,

	    load_without_unload(Engine, Md5Hash1, Md5Hash2),

	    erlang:garbage_collect(),
	    timer:sleep(1000),

	    case crypto:hash(md5, "Don't panic") of
		Md5Hash2 ->
		    ct:fail(fail_to_unload_still_test_engine);
		Md5Hash1 ->
		    ok;
		_ ->
		    ct:fail(fail_to_unload_engine)
	    end,
	    
	    try
		case crypto:ensure_engine_loaded(<<"MD5">>, Engine) of
		    {ok, E} ->
			ok = crypto:engine_register(E, [engine_method_digests]),
			case crypto:hash(md5, "Don't panic") of
			    Md5Hash1 ->
				ct:fail(fail_to_load_still_original_engine);
			    Md5Hash2 ->
				ok;
			    _ ->
				ct:fail(fail_to_load_engine)
			end,
			
			ok = crypto:engine_unregister(E, [engine_method_digests]),
			ok = crypto:engine_remove(E),
			ok = crypto:ensure_engine_unloaded(E),
			
			case crypto:hash(md5, "Don't panic") of
			    Md5Hash2 ->
				ct:fail(fail_to_unload_still_test_engine);
			    Md5Hash1 ->
				ok;
			    _ ->
				ct:fail(fail_to_unload_engine)
			end;
		    {error, bad_engine_id} ->
                        {skip, "Dynamic Engine not supported"}
		end
	     catch
		 error:notsup ->
			     {skip, "Engine not supported on this SSL version"}
	     end
    end.
    
load_without_unload(Engine, Md5Hash1, Md5Hash2) ->
    case crypto:ensure_engine_loaded(<<"MD5">>, Engine) of
	{ok, E} ->
	    ok = crypto:engine_register(E, [engine_method_digests]),
	    case crypto:hash(md5, "Don't panic") of
		Md5Hash1 ->
		    ct:fail(fail_to_load_still_original_engine);
		Md5Hash2 ->
		    ok = crypto:engine_unregister(E, [engine_method_digests]),
		    ok;
		_ ->
		    ct:fail(fail_to_load_engine)
	    end;
	 {error, bad_engine_id} ->
	    io:format("load_without_unload: bad_engine_id\n", [])
    end.

%%%----------------------------------------------------------------
%%% Pub/priv key storage tests.  Those are for testing the crypto.erl
%%% support for using priv/pub keys stored in an engine.

sign_verify_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    sign_verify(rsa, sha, Priv, Pub).

sign_verify_rsa_fake(Config) ->
    %% Use fake engine rsa implementation
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    sign_verify_fake(rsa, sha256, Priv, Pub).

sign_verify_dsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "dsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "dsa_public_key.pem")},
    sign_verify(dss, sha, Priv, Pub).

sign_verify_ecdsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ecdsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ecdsa_public_key.pem")},
    sign_verify(ecdsa, sha, Priv, Pub).

sign_verify_eddsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "eddsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "eddsa_public_key.pem")},
    sign_verify(eddsa, sha, Priv, Pub).

sign_verify_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    sign_verify(rsa, sha, Priv, Pub).

sign_verify_rsa_pwd_bad_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "Bad password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    try sign_verify(rsa, sha, Priv, Pub) of
        _ -> {fail, "PWD prot pubkey sign succeeded with no pwd!"}
    catch
        error:{badarg,_,_} -> ok
    end.

priv_encrypt_pub_decrypt_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    priv_enc_pub_dec(rsa, Priv, Pub,  rsa_pkcs1_padding).

priv_encrypt_pub_decrypt_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    priv_enc_pub_dec(rsa, Priv, Pub,  rsa_pkcs1_padding).

pub_encrypt_priv_decrypt_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key.pem")},
    pub_enc_priv_dec(rsa, Pub, Priv,  rsa_pkcs1_padding).

pub_encrypt_priv_decrypt_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_public_key_pwd.pem")},
    pub_enc_priv_dec(rsa, Pub, Priv,  rsa_pkcs1_padding).

get_pub_from_priv_key_rsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key.pem")},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("rsa Pub = ~p",[Pub]),
            sign_verify(rsa, sha, Priv, Pub)
    end.

get_pub_from_priv_key_rsa_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "password"},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("rsa Pub = ~p",[Pub]),
            sign_verify(rsa, sha, Priv, Pub)
    end.

get_pub_from_priv_key_rsa_pwd_no_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem")},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            ok;
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("rsa Pub = ~p",[Pub]),
            {fail, "PWD prot pubkey fetch succeeded although no pwd!"}
    end.

get_pub_from_priv_key_rsa_pwd_bad_pwd(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_private_key_pwd.pem"),
             password => "Bad password"},
    case crypto:privkey_to_pubkey(rsa, Priv) of
        {error, not_found} ->
            ok;
        {error, notsup} ->
            {skip, "RSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("rsa Pub = ~p",[Pub]),
            {fail, "PWD prot pubkey fetch succeeded with bad pwd!"}
    end.

get_pub_from_priv_key_dsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "dsa_private_key.pem")},
    case crypto:privkey_to_pubkey(dss, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "DSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("dsa Pub = ~p",[Pub]),
            sign_verify(dss, sha, Priv, Pub)
    end.

get_pub_from_priv_key_ecdsa(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ecdsa_private_key.pem")},
    case crypto:privkey_to_pubkey(ecdsa, Priv) of
        {error, not_found} ->
            {fail, "Key not found"};
        {error, notsup} ->
            {skip, "ECDSA not supported"};
        {error, Error} ->
            {fail, {wrong_error,Error}};
        Pub ->
            ct:log("ecdsa Pub = ~p",[Pub]),
            sign_verify(ecdsa, sha, Priv, Pub)
    end.

%%%================================================================
%%% Help for engine_stored_pub_priv_keys* test cases
%%%
skip_if_unsup(Type, Config) ->
    case pkey_supported(Type) of
        false ->
            {skip, "Unsupported in this cryptolib"};
        true ->
            Config
    end.


pkey_supported(Type) ->
    lists:member(Type, proplists:get_value(public_keys, crypto:supports(), [])).


load_storage_engine(_Config) ->
    case crypto:get_test_engine() of
        {ok, EngineLibPath} ->
            try crypto:engine_load(<<"dynamic">>,
				   [{<<"SO_PATH">>, EngineLibPath},
				    <<"LOAD">>],
				   [])
	    catch
                error:notsup ->
                    {error, notsup}
            end;

        {error, Error} ->
            {error, Error}
    end.


key_dir(Config) ->
    DataDir = unicode:characters_to_binary(proplists:get_value(data_dir, Config)),
    filename:join(DataDir, "pkcs8").


engine_ref(Config) ->
    proplists:get_value(storage_engine, Config).

key_id(Config, File) ->
    filename:join(proplists:get_value(storage_dir,Config), File).

pubkey_alg_supported(Alg) ->
    lists:member(Alg,
                 proplists:get_value(public_keys, crypto:supports())).


pub_enc_priv_dec(Alg, KeyEnc, KeyDec, Padding) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            CryptoText = crypto:public_encrypt(Alg, PlainText, KeyEnc, Padding),
            case crypto:private_decrypt(Alg, CryptoText, KeyDec, Padding) of
                PlainText -> ok;
                _ -> {fail, "Encrypt-decrypt error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.

priv_enc_pub_dec(Alg, KeyEnc, KeyDec, Padding) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            CryptoText = crypto:private_encrypt(Alg, PlainText, KeyEnc, Padding),
            case crypto:public_decrypt(Alg, CryptoText, KeyDec, Padding) of
                PlainText -> ok;
                _ -> {fail, "Encrypt-decrypt error"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.

sign_verify(Alg, Sha, KeySign, KeyVerify) ->
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Hej på dig">>,
            Signature = crypto:sign(Alg, Sha, PlainText, KeySign),
            case is_fake(Signature) of
                true ->
                    ct:pal("SIG ~p ~p size ~p~n~p",[Alg,Sha,size(Signature),Signature]),
                    {fail, "Faked RSA impl used!!"};
                false ->
                    case crypto:verify(Alg, Sha, PlainText, Signature, KeyVerify) of
                        true -> ok;
                        _ -> {fail, "Sign-verify error"}
                    end
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.


%%% Use fake engine rsa implementation
sign_verify_fake(Alg, Sha, KeySign, KeyVerify) ->
    ct:log("~p:~p  sign_verify_fake ~p~n Sha = ~p~n KeySign = ~p~n KeyVerify = ~p~n",
           [?MODULE, ?LINE, Alg, Sha, KeySign, KeyVerify]),
    case pubkey_alg_supported(Alg) of
        true ->
            PlainText = <<"Fake me!">>,
            Signature = crypto:sign(Alg, Sha, PlainText, KeySign),
            case is_fake(Signature) of
                true ->
                    case crypto:verify(Alg, Sha, PlainText, Signature, KeyVerify) of
                        true -> ok;
                        _ -> {fail, "Sign-verify error"}
                    end;
                false ->
                    ct:pal("SIG ~p ~p size ~p~n~p",[Alg,Sha,size(Signature),Signature]),
                    {fail, "Faked impl not used"}
            end;
        false ->
            {skip, lists:concat([Alg," is not supported by cryptolib"])}
    end.


is_fake(Sig) -> is_fake(Sig, 0).

is_fake(<<>>, _) -> true;
is_fake(<<B,Rest/binary>>, B) -> is_fake(Rest, B+1);
is_fake(_, _) -> false.


  
