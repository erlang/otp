-module(public_key_engine_SUITE).

-include_lib("common_test/include/ct.hrl").


%% Common test
-export([all/0,
         init_per_suite/1,
         init_per_testcase/2,
         end_per_suite/1,
         end_per_testcase/2
        ]).
%% Test cases
-export([sign_verify_ec/1,
         sign_verify_rsa/1
        ]).


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     sign_verify_ec,
     sign_verify_rsa
    ].

init_per_suite(Config) ->
    application:stop(crypto),
    try crypto:start() of
	ok ->
            case crypto:info_lib() of
                [{_,_, <<"OpenSSL 1.0.1s-freebsd  1 Mar 2016">>}] ->
                    {skip, "Problem with engine on OpenSSL 1.0.1s-freebsd"};
                _ ->
                    case crypto:get_test_engine() of
                         {ok, EngineName} ->
                            try
                                %% The test engine has it's own fake rsa sign/verify that
                                %% you don't want to use, so exclude it from methods to load:
                                Methods =
                                crypto:engine_get_all_methods() -- [engine_method_rsa],
                                crypto:engine_load(<<"dynamic">>,
                                                   [{<<"SO_PATH">>, EngineName},
                                                    <<"LOAD">>],
                                                   [],
                                                   Methods)
                            of
                                {ok, Engine} ->
                                    KeyDir = key_dir(Config),
                                    [{storage_engine, Engine}, {storage_dir, KeyDir} |Config];
                                {error, Reason} ->
                                    ct:pal("Reason ~p", [Reason]),
                                    {skip, "No dynamic engine support"}
                            catch error:notsup ->
                                    {skip, "No engine support in OpenSSL"}
                            end;
                        {error, notexist} ->
                            {skip, "Test engine not found"}
                    end
            end
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(Config) ->
    Engine = proplists:get_value(storage_engine, Config),
    crypto:engine_unload(Engine),
    application:stop(crypto).


init_per_testcase(_TestCase, Config) ->
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------

sign_verify_ec(Config) when is_list(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ec_key.pem"),
             algorithm => ecdsa},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "ec_pub.pem"),
             algorithm => ecdsa},
    sign_verify(Priv, Pub).

sign_verify_rsa(Config) when is_list(Config) ->
    Priv = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_key.pem"),
             algorithm => ecdsa},
    Pub  = #{engine => engine_ref(Config),
             key_id => key_id(Config, "rsa_pub.pem"),
             algorithm => ecdsa},
    sign_verify(Priv, Pub).

%%%================================================================
%%% Help for public_key_engine_stored_pub_priv_keys* test cases
%%%

key_dir(Config) ->
    unicode:characters_to_binary(proplists:get_value(data_dir, Config)).

engine_ref(Config) ->
    proplists:get_value(storage_engine, Config).

key_id(Config, File) ->
    filename:join(proplists:get_value(storage_dir,Config), File).

sign_verify(PrivateKey, PublicKey) ->
    Msg = list_to_binary(lists:duplicate(5, "Foo bar 100")),
    Sign = public_key:sign(Msg, sha256, PrivateKey),
    true = public_key:verify(Msg, sha256, Sign, PublicKey),
    false = public_key:verify(<<1:8, Msg/binary>>, sha256, Sign, PublicKey).

