%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2023. All Rights Reserved.
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

-module(crypto_collect_labmachine_info_SUITE).

%% This test suite collects information about the OpenSSL version
%% used by crypto application.

%% It requires common_test configuration data 'collect_host_info'
%% to be set to the root target directory where the collected
%% information will be stored.

%% It can either be a local path
%%   {collect_host_info, "/my/local/dir"}.
%% or a remote path usable by the scp command
%%   {collect_host_info, "hostname:/its/local/dir"}.
%%   {collect_host_info, "user@hostname:/its/local/dir"}.

%% The collected data will be store as
%% <collect_host_info>/crypto_info/<hostname>.data
%% Sub directory crypto_info must exist.

-include_lib("common_test/include/ct.hrl").

-export([
         suite/0,
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         crypto_info_lib/1
        ]).

-define(DAYS_TO_KEEP, 5).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> [{require, collect_host_info},
            {timetrap,{seconds,40}}].

all() -> [crypto_info_lib].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Root = ct:get_config(collect_host_info),
    RemoteFile = filename:join([Root, "crypto_info", hostname()++".data"]),
    CryptoStarted =
        try crypto:start() of
            ok -> true;
            {error, already_started} -> true;
            _ -> false
        catch
            _:_ -> false
        end,

    case CryptoStarted of
        true ->
            LocalFile = priv_file(Config, hostname()++".cryptodata"),

            case usable_file(LocalFile) of
                false -> ct:fail(no_local_file);
                true -> ok
            end,

            ct:log("os:type() = ~p", [os:type()]),

            TransferType =
                case {path_type(RemoteFile), os:type()} of
                    {local, {unix,_}} ->
                             case usable_file(RemoteFile) of
                                 true -> filesystem;
                                 false -> ssh
                             end;
                          _ ->
                             ssh
                     end,

            case TransferType of
                filesystem ->
                    %% 'filesystem' was concluded since it was possible
                    %% to open the file in append mode
                    {ok,B} = file:read_file(RemoteFile),
                    ok = file:write_file(LocalFile, B);
                ssh ->
                    SCP = wsl_ify("scp "++RemoteFile++" "++remove_drive_letter(LocalFile)),
                    ct:pal("Run command: \"~s\"", [SCP]),
                    Result = os:cmd(SCP),
                    ct:pal("Command result: \"~s\"",[Result])
            end,

            crypto:enable_fips_mode(true),

            [{transfer_type, TransferType},
             {local_file,LocalFile},
             {remote_file,RemoteFile} | Config];

        false ->
            {skip, "Crypto did not start"}
    end.

end_per_suite(Config) ->
    LocalFile = proplists:get_value(local_file,Config),
    RemoteFile = proplists:get_value(remote_file,Config),
    case proplists:get_value(transfer_type,Config) of
        filesystem ->
            {ok,B} = file:read_file(LocalFile),
            ok = file:write_file(RemoteFile, B);
        ssh ->
            SCP = wsl_ify("scp "++remove_drive_letter(LocalFile)++" "++RemoteFile),
            ct:pal("Run command: \"~s\"", [SCP]),
            Result = os:cmd(SCP),
            ct:pal("Command result: \"~s\"",[Result])
    end,
    file:delete(LocalFile).

path_type(Path) ->
    case string:lexemes(Path, ":") of
        [_] ->
            local;
        [Host | _] ->
            case string:find(Host, "/") of
                nomatch -> remote;
                _ -> local
            end
    end.

%%--------------------------------------------------------------------
crypto_info_lib(Config) ->
    LocalFile = proplists:get_value(local_file, Config),

    NewEntry = new_entry(),
    ct:pal("New entry:~n~p",[NewEntry]),

    YoungEntries =
        case file:consult(LocalFile) of
            {ok, Consulted} when is_list(Consulted) ->
                lists:filter(fun(E) -> is_young(E) end,
                             Consulted);
            Other ->
                ct:log("Strange result of consult:~n~p", [Other]),
                ct:fail("Consult failed")
        end,

    {ok,D} = file:open(LocalFile, [write]),
    lists:foreach(fun(E) ->
                          io:format(D, '~p.~n', [E])
                  end, lists:usort([NewEntry|YoungEntries])),
    file:close(D).

is_young(E) ->
    try
        Days = days_ago(proplists:get_value(date, E)),
        Days >= 0 andalso Days =< ?DAYS_TO_KEEP
    catch
        _:_ -> false                     % No or illegal date property
    end.

days_ago(D={_,_,_})->
     calendar:date_to_gregorian_days(date()) - calendar:date_to_gregorian_days(D).

new_entry() ->
    [{hostname,  hostname()},
     {date,      date()},
     {time,      time()},
     {os_type,   os:type()},
     {os_version,os:version()}
    ]
    ++ [{cwd,PWD} || {ok,PWD} <- [file:get_cwd()]]
    ++ crypto_info([info_lib,info_fips,compile_type,link_type,supports,ec_curves])
    ++ try_rsa().

crypto_info(Tags) ->
    [{Tag, Info} || Tag <- Tags,
                    Info <-
                        try crypto:Tag()
                        of V -> [V]
                        catch _:_ ->
                                try maps:get(Tag, crypto:info()) of
                                    V -> [V]
                                catch  _:_ -> []
                                end
                        end
    ].

%%--------------------------------------------------------------------
try_rsa() ->
    try
        {Pub,Pri} = rsa_keys(), %% Precomputed {4096,65537}
        RsaOpts = proplists:get_value(rsa_opts, crypto:supports(), []),
        [{sign_verify, test_sign_verify(Pub, Pri, RsaOpts)},
         {pub_enc_priv_dec, pk_enc_dec(RsaOpts, Pub, fun crypto:public_encrypt/4,  Pri, fun crypto:private_decrypt/4)},
         {priv_enc_pub_dec, pk_enc_dec(RsaOpts, Pri, fun crypto:private_encrypt/4, Pub, fun crypto:public_decrypt/4)}
        ]
    catch
        _:_ ->
            []
    end.

md_opts() ->
    [rsa_mgf1_md, rsa_oaep_md].

%%----------------
pk_enc_dec(PadOpts, KeyEnc, Fenc, KeyDec, Fdec) ->
    Ptxt = <<1:32/unit:8>>,
    HashOpts = [O || O <- md_opts(),
                     lists:member(O, PadOpts)],
    NonHashOpts = PadOpts -- HashOpts,
    lists:sort(
      pk_enc_dec_hash_opts(HashOpts, KeyEnc, Fenc, KeyDec, Fdec, Ptxt) ++
      pk_enc_dec_non_hash_opts(NonHashOpts, KeyEnc, Fenc, KeyDec, Fdec, Ptxt)
     ).

%%----
pk_enc_dec_hash_opts(PadOpts, KeyEnc, Fenc, KeyDec, Fdec, Ptxt) ->
    SupHash = proplists:get_value(hashs, crypto:supports(), []),
    [begin
         [H|T] = lists:reverse(tuple_to_list(LogOpt)),
         list_to_tuple(lists:reverse([{H,OkMDs}|T]))
     end ||
        Opt <- PadOpts,
        {LogOpt, OptsF} <- gen_logopt_opts(Opt),
        OkMDs <- [
                    [MD || MD <- SupHash,
                             pk_enc_dec_1(Fenc, Ptxt, KeyEnc, Fdec, KeyDec, OptsF(MD))]
                   ],
        OkMDs =/= []
    ].

pk_enc_dec_non_hash_opts(PadOpts, KeyEnc, Fenc, KeyDec, Fdec, Ptxt) ->
    [LogOpt ||
        Opt <- PadOpts,
        {LogOpt, OptsF} <- gen_logopt_opts(Opt),
        pk_enc_dec_1(Fenc, Ptxt, KeyEnc, Fdec, KeyDec, OptsF(undefined))
    ].

%%----
pk_enc_dec_1(Fenc, Ptxt, KeyEnc, Fdec, KeyDec, Opts) ->
    case
        begin
            R = try Fdec(rsa, Fenc(rsa,Ptxt,KeyEnc,Opts), KeyDec, Opts)
                catch _:_ -> no
                end,
            ct:pal("enc/dec ~p -> ~p",[Opts,R]),
            R
        end
    of
        Ptxt ->
            true;
        _ ->
            false
    end.

%%----------------
test_sign_verify(Pub, Pri, RsaOpts) ->
    MDs = Hashs = proplists:get_value(hashs, crypto:supports(), []),
    HashOpts = [O || O <- md_opts(),
                     lists:member(O, RsaOpts)],
    NonHashOpts = RsaOpts -- HashOpts,
    Rm1 =
        lists:sort(
          [{{LogOpt,OkHashs},MD} ||
              Opt <- HashOpts,
              {LogOpt, OptsF}  <- gen_logopt_opts(Opt),
              MD <- MDs,
              OkHashs <- [test_sign_verify_hashs(Pub, Pri, OptsF(MD), Hashs)],
              OkHashs =/= []
          ]),

    Rm2 =
        lists:sort(
          [{LogOpt,OkHashs} ||
              Opt <- NonHashOpts,
              {LogOpt, OptsF}  <- gen_logopt_opts(Opt),
              OkHashs <- [test_sign_verify_hashs(Pub, Pri, OptsF(undefined), Hashs)],
              OkHashs =/= []
          ]),

    lists:sort(
      Rm2 ++ 
          lists:foldr(fun({{LogOpt,MD0s},_OkHashs}, Acc) ->
                              [H|T] = lists:reverse(tuple_to_list(LogOpt)),
                              [list_to_tuple(lists:reverse([{H,MD0s}|T])) | Acc]
                      end,
                      [],
                      lists:foldr(fun({{LogOpt,OkHashs},MD1}, [{{LogOpt,MD1s},OkHashs}|T]) ->
                                          [{{LogOpt,[MD1|MD1s]},OkHashs}|T];
                                     ({{LogOpt,OkHashs},MD1}, T) ->
                                          [{{LogOpt,[MD1]},OkHashs}|T]
                                  end, [], Rm1)
                     )
     ).

test_sign_verify_hashs(Pub, Pri, Opts, Hashs) ->
    [Hash || Hash <- Hashs,
             test_sign_verify_hash(Pub, Pri, Opts, Hash)
    ].

test_sign_verify_hash(Pub, Pri, Opts, Hash) ->
    R = try
            Sig = crypto:sign(rsa, Hash, <<"hej">>, Pri, Opts),
            true == crypto:verify(rsa, Hash, <<"hej">>, Sig, Pub, Opts)
        catch
            _:_ -> false
        end,
    ct:pal("sign/verify ~p ~p -> ~p",[Opts,Hash,R]),
    R.

%%----------------
gen_logopt_opts(rsa_mgf1_md = Opt) ->
    [{{rsa_pkcs1_pss_padding, Opt},
      fun(Hash) ->
              [{rsa_padding,rsa_pkcs1_pss_padding},
               {rsa_mgf1_md,Hash}]
      end
     },

     {{rsa_pkcs1_oaep_padding,Opt},
      fun(Hash) ->
              [{rsa_padding,rsa_pkcs1_oaep_padding},
               {rsa_mgf1_md,Hash}]
      end
     }
    ];

gen_logopt_opts(rsa_pss_saltlen = Opt) -> 
    [{{rsa_pkcs1_pss_padding,Opt},
      fun(_Hash) ->
              [{rsa_padding,rsa_pkcs1_pss_padding},
               {rsa_pss_saltlen,3}]
      end
     }];

gen_logopt_opts(rsa_oaep_label = Opt) -> 
    [{{rsa_pkcs1_oaep_padding,Opt},
       fun(_Hash) ->
               [{rsa_padding,rsa_pkcs1_oaep_padding},
                {rsa_oaep_label,<<"putte">>}]
       end
     }];

gen_logopt_opts(rsa_oaep_md = Opt) ->
    [{{rsa_pkcs1_oaep_padding,Opt},
      fun(Hash) ->
              [{rsa_padding,rsa_pkcs1_oaep_padding},
               {rsa_oaep_md,Hash}]
      end
     }];

gen_logopt_opts(signature_md) ->
    [];

gen_logopt_opts(Opt) -> 
    [{Opt,
      fun(_Hash) ->
              [{rsa_padding,Opt}]
      end
     }].
        
        

rsa_keys() ->
    {[<<1,0,1>>,
      <<184,150,67,22,76,107,137,142,167,255,194,1,215,181,17,212,132,236,4,125,13,115,247,242,189,131,107,101,85,44,86,194,140,47,9,223,187,8,149,120,79,189,228,126,124,174,244,3,164,165,48,50,32,28,251,226,109,27,132,254,210,71,48,86,110,101,77,172,88,148,39,124,235,88,63,225,142,192,255,168,105,177,37,238,148,38,198,188,224,8,82,34,255,203,212,12,1,164,191,222,94,102,252,82,155,33,234,106,207,220,68,84,120,86,189,33,107,162,48,56,9,99,179,3,114,179,107,160,113,246,128,72,89,243,152,31,107,145,151,12,214,79,57,120,210,225,59,221,145,70,120,167,115,66,62,12,250,53,171,0,253,53,117,40,33,98,212,0,135,29,205,119,70,76,17,226,94,108,166,119,181,54,47,152,162,179,123,170,233,43,195,19,191,95,124,245,133,154,46,221,89,162,153,104,26,189,0,122,21,23,98,105,251,52,220,2,168,31,246,247,170,194,141,76,79,104,48,57,24,126,150,43,92,199,27,204,124,62,187,234,59,88,131,126,147,8,176,106,216,186,185,180,184,71,223,4,123,190,138,46,236,27,138,81,34,204,57,229,222,59,68,23,199,153,90,1,41,190,170,190,131,163,215,144,43,238,203,134,85,150,22,40,234,86,142,16,232,205,172,231,242,11,113,85,89,55,236,12,91,9,107,157,93,86,51,86,164,233,169,113,33,175,182,188,142,91,95,177,162,157,110,172,84,239,230,234,133,26,162,198,193,246,66,23,13,16,25,45,59,174,175,0,96,83,241,137,102,24,54,77,189,14,191,108,44,143,60,239,45,231,236,116,66,188,77,227,234,94,169,114,97,220,38,153,222,77,77,102,81,138,51,18,46,137,177,99,155,85,10,213,82,221,6,35,162,177,110,164,79,197,233,242,100,159,105,91,200,234,5,120,142,237,216,118,60,245,172,29,182,168,227,125,17,159,8,143,233,136,55,130,253,7,150,94,206,106,231,68,199,254,25,85,22,57,74,236,144,19,242,179,57,57,9,14,146,28,52,54,170,109,200,195,152,170,214,81,249,238,9,38,215,73,86,241,116,26,139,152,99,64,165,192,69,84,214,96,18,28,141,118,63,35,114,122,87,43,96,4,199,224,157,239>>],

     [<<1,0,1>>,
      <<184,150,67,22,76,107,137,142,167,255,194,1,215,181,17,212,132,236,4,125,13,115,247,242,189,131,107,101,85,44,86,194,140,47,9,223,187,8,149,120,79,189,228,126,124,174,244,3,164,165,48,50,32,28,251,226,109,27,132,254,210,71,48,86,110,101,77,172,88,148,39,124,235,88,63,225,142,192,255,168,105,177,37,238,148,38,198,188,224,8,82,34,255,203,212,12,1,164,191,222,94,102,252,82,155,33,234,106,207,220,68,84,120,86,189,33,107,162,48,56,9,99,179,3,114,179,107,160,113,246,128,72,89,243,152,31,107,145,151,12,214,79,57,120,210,225,59,221,145,70,120,167,115,66,62,12,250,53,171,0,253,53,117,40,33,98,212,0,135,29,205,119,70,76,17,226,94,108,166,119,181,54,47,152,162,179,123,170,233,43,195,19,191,95,124,245,133,154,46,221,89,162,153,104,26,189,0,122,21,23,98,105,251,52,220,2,168,31,246,247,170,194,141,76,79,104,48,57,24,126,150,43,92,199,27,204,124,62,187,234,59,88,131,126,147,8,176,106,216,186,185,180,184,71,223,4,123,190,138,46,236,27,138,81,34,204,57,229,222,59,68,23,199,153,90,1,41,190,170,190,131,163,215,144,43,238,203,134,85,150,22,40,234,86,142,16,232,205,172,231,242,11,113,85,89,55,236,12,91,9,107,157,93,86,51,86,164,233,169,113,33,175,182,188,142,91,95,177,162,157,110,172,84,239,230,234,133,26,162,198,193,246,66,23,13,16,25,45,59,174,175,0,96,83,241,137,102,24,54,77,189,14,191,108,44,143,60,239,45,231,236,116,66,188,77,227,234,94,169,114,97,220,38,153,222,77,77,102,81,138,51,18,46,137,177,99,155,85,10,213,82,221,6,35,162,177,110,164,79,197,233,242,100,159,105,91,200,234,5,120,142,237,216,118,60,245,172,29,182,168,227,125,17,159,8,143,233,136,55,130,253,7,150,94,206,106,231,68,199,254,25,85,22,57,74,236,144,19,242,179,57,57,9,14,146,28,52,54,170,109,200,195,152,170,214,81,249,238,9,38,215,73,86,241,116,26,139,152,99,64,165,192,69,84,214,96,18,28,141,118,63,35,114,122,87,43,96,4,199,224,157,239>>,
      <<6,96,106,128,226,178,244,85,145,72,44,147,147,184,21,157,153,217,237,109,196,129,53,23,149,164,108,153,91,143,228,143,99,248,92,6,246,151,29,82,194,174,116,174,73,240,97,77,158,234,117,247,197,84,58,255,95,117,139,234,161,169,3,234,120,117,215,113,20,190,73,126,239,66,101,50,141,147,237,65,12,37,170,129,203,108,107,129,42,220,30,186,70,203,235,207,94,95,42,161,171,241,50,214,71,181,172,59,116,98,207,153,123,110,195,169,219,230,6,116,105,30,160,152,133,229,220,217,246,21,229,232,153,242,232,86,109,136,226,6,247,128,85,35,239,101,8,132,102,248,96,66,168,160,169,52,25,144,177,15,159,175,177,20,105,210,216,48,12,122,227,22,51,191,124,254,172,244,99,235,206,190,200,118,37,104,189,228,241,153,41,108,13,56,103,64,180,20,215,121,158,23,23,229,152,198,222,181,88,245,45,217,43,108,112,13,71,113,147,62,139,15,157,129,253,92,68,14,52,226,106,157,164,222,219,254,252,107,206,98,17,26,197,176,33,180,51,35,253,200,181,145,160,172,26,240,37,163,4,195,29,56,108,116,12,23,232,64,225,80,249,223,3,119,251,59,192,43,120,132,98,193,108,88,160,83,183,24,210,143,125,12,78,248,44,137,55,191,250,226,84,45,184,107,35,65,221,107,13,145,179,84,64,190,211,39,13,52,253,37,231,12,67,138,218,140,215,57,181,178,154,64,22,196,57,233,183,182,34,229,214,100,252,39,83,1,166,26,238,91,82,70,224,170,46,250,215,247,237,44,61,245,153,198,50,119,158,157,26,196,168,56,238,136,3,233,137,186,165,143,228,19,14,52,106,61,97,85,50,212,160,24,73,246,145,226,163,176,16,156,45,16,34,122,143,142,223,50,240,224,12,234,119,122,65,167,150,190,63,118,10,250,233,173,206,90,44,36,176,32,110,235,189,56,68,151,29,43,72,41,4,177,112,119,144,243,92,120,51,157,232,113,57,186,7,21,152,22,244,56,92,44,63,213,110,197,204,22,19,69,10,172,136,114,172,19,128,4,149,50,130,238,212,139,78,164,219,187,2,2,23,197,0,40,22,65,197,129,55,206,221,4,208,60,173,157,138,105,67,147,192,193>>,
      <<236,240,239,180,144,149,110,219,221,16,232,109,235,113,46,241,245,63,172,130,16,207,32,9,108,91,17,129,164,182,0,67,167,223,65,148,51,163,132,226,195,45,22,71,86,45,48,115,229,0,104,114,192,215,190,50,98,38,56,205,247,193,186,213,138,127,210,154,130,26,7,206,42,70,61,208,223,99,179,2,2,201,148,49,53,57,154,209,20,125,8,45,215,107,39,198,228,176,12,161,14,57,84,247,236,147,134,1,124,83,146,218,235,65,176,143,205,166,10,177,127,238,94,7,240,194,161,222,154,27,75,10,144,155,8,100,128,1,220,108,98,58,152,21,42,133,212,127,161,155,175,36,142,140,107,236,37,232,86,200,251,43,121,91,51,60,67,248,26,113,168,110,134,161,244,42,162,72,39,111,85,62,169,237,53,153,241,191,178,236,139,9,17,40,120,77,187,198,216,143,179,179,77,117,199,226,8,196,238,65,97,163,62,2,150,122,1,240,12,25,226,211,84,119,186,172,218,171,155,202,201,178,142,36,232,249,44,135,235,81,146,148,106,249,83,253,233,198,152,29,205,18,166,147,240,195>>,
      <<199,111,65,34,99,67,225,246,145,69,58,195,193,187,63,170,49,114,63,158,95,149,1,57,178,55,176,29,96,188,238,6,201,184,175,16,48,80,9,237,111,171,37,74,199,72,121,197,230,136,217,226,137,95,181,202,104,75,148,142,16,52,84,127,250,114,137,197,254,184,140,132,213,238,67,140,165,0,209,138,63,100,169,185,150,94,73,156,232,46,135,226,216,63,214,148,131,169,92,166,109,68,132,152,102,111,126,164,26,81,64,156,178,158,168,183,94,93,114,246,250,179,80,142,112,196,58,157,93,82,43,173,153,212,154,74,217,44,239,247,127,91,23,207,204,125,61,24,119,108,186,197,102,109,77,203,218,172,254,118,183,95,196,158,112,100,41,171,92,105,127,190,21,54,252,107,75,155,108,129,43,252,246,239,176,137,26,26,224,93,46,153,200,60,75,48,184,40,45,168,9,23,202,103,234,8,192,198,161,174,44,152,236,132,193,240,149,234,56,201,68,83,27,1,106,65,46,60,112,67,186,173,25,71,220,138,127,89,79,65,162,121,15,123,119,216,244,46,27,249,171,129,72,136,203,101>>,
      <<94,223,6,11,181,140,175,235,198,243,91,45,144,5,44,183,226,236,16,229,175,87,79,203,150,32,76,189,45,55,86,153,192,96,203,64,77,0,223,87,112,176,223,231,198,96,167,178,239,241,158,224,187,200,102,182,131,200,44,28,132,100,132,171,182,30,2,52,173,125,64,188,210,254,119,7,232,58,98,85,158,202,25,213,46,43,151,138,185,250,106,72,248,165,70,51,4,36,93,154,43,56,215,133,193,171,60,15,226,57,16,103,41,165,142,205,181,153,19,85,9,21,103,127,36,110,186,35,136,110,255,56,6,184,156,190,168,230,149,220,171,228,44,58,229,96,218,186,19,250,99,65,110,246,88,204,32,77,140,90,66,255,160,66,221,27,247,87,35,55,121,42,112,43,238,185,23,107,110,166,204,106,31,222,4,5,84,223,222,3,138,148,33,211,163,127,244,124,228,255,125,3,129,51,116,202,83,154,142,143,141,73,176,173,150,148,217,171,197,28,164,78,219,214,104,223,135,91,9,49,68,91,131,64,210,43,219,33,189,30,87,99,168,5,151,20,231,3,6,171,128,14,68,182,157,195>>,
      <<7,89,91,46,24,56,85,40,79,220,28,57,64,119,84,246,65,146,51,51,243,211,186,210,212,117,102,224,254,66,152,6,218,2,27,57,94,126,100,143,66,81,45,51,159,58,85,122,108,111,88,144,23,125,214,102,26,7,198,246,161,52,60,116,230,214,183,170,251,34,243,210,165,75,125,99,190,100,212,83,226,216,35,196,249,252,137,123,229,182,105,61,123,184,134,128,188,133,78,43,183,213,17,137,200,36,141,167,32,109,36,19,98,37,164,147,245,63,62,80,73,150,183,254,55,187,147,105,95,25,251,16,15,158,28,253,107,25,83,210,107,248,0,93,160,236,112,134,233,71,177,110,58,195,6,93,223,182,6,62,123,173,100,194,109,227,211,1,68,139,130,176,143,121,146,61,165,240,153,67,253,73,250,19,133,14,216,78,107,60,35,206,120,233,119,223,216,185,106,77,73,224,125,141,236,186,219,215,181,247,132,31,50,128,5,225,134,180,181,216,252,210,143,41,51,48,41,179,2,116,12,121,250,119,130,113,252,21,95,109,76,190,103,74,166,116,241,50,141,199,90,129,142,25,180,93>>,
      <<150,52,171,50,46,79,167,168,5,191,161,144,81,14,216,185,45,107,92,238,220,196,190,14,89,8,217,1,206,175,171,46,59,0,117,183,68,66,186,93,198,237,143,4,10,80,186,176,32,4,27,67,211,137,193,66,9,163,109,188,250,25,11,73,82,91,172,10,149,69,25,169,123,56,174,72,2,1,24,61,5,231,214,164,163,65,121,178,147,24,250,154,193,105,148,223,164,116,179,91,183,58,178,125,188,237,234,104,195,67,144,163,49,219,49,254,13,66,125,185,171,201,244,1,70,6,76,251,4,162,17,246,180,29,20,121,38,248,76,165,250,16,59,42,11,114,89,223,171,28,35,194,84,223,136,63,63,205,172,254,237,195,90,232,181,189,218,133,238,47,95,218,105,32,10,140,81,97,214,238,2,191,107,0,106,107,219,132,123,204,151,252,87,170,139,181,53,60,188,243,230,8,145,184,79,177,225,170,198,250,122,140,253,160,69,226,136,185,69,10,43,97,19,214,68,7,126,194,114,241,207,51,244,35,180,20,136,27,84,155,61,213,189,98,197,120,31,71,209,129,105,98,114,102,77,147>>]}.

%%--------------------------------------------------------------------
hostname() ->
    case inet:gethostname() of
	{ok,Name} -> string:to_lower(Name);
	_ -> "undefined"
    end.
	    
priv_dir(Config) -> proplists:get_value(priv_dir, Config).

priv_file(Config, Name) -> filename:join(priv_dir(Config), Name).


remove_drive_letter([_DriveLetter,$:|FileName]) -> "/mnt/c" ++ FileName;
remove_drive_letter(FileName) -> FileName.


usable_file(FileName) ->
    case file:open(FileName, [append]) of
        {ok,D} ->
            ok == file:close(D);
        _ ->
            false
    end.

%%%----------------------------------------------------------------
wsl_ify(Cmnd) ->
    case os:getenv("WSLENV") of
        false -> Cmnd;
        _ -> "wsl " ++ Cmnd
    end.
