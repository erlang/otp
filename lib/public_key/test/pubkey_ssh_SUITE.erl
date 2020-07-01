%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
-module(pubkey_ssh_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

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

         ssh_rsa_public_key/1,
         ssh_dsa_public_key/1,
         ssh_ecdsa_public_key/1,
         ssh_rfc4716_rsa_comment/1,
         ssh_rfc4716_dsa_comment/1,
         ssh_rfc4716_rsa_subject/1,
         ssh_known_hosts/1,
         ssh1_known_hosts/1,
         ssh_auth_keys/1,
         ssh1_auth_keys/1,
         ssh_openssh_public_key_with_comment/1,
         ssh_openssh_public_key_long_header/1,

         ssh_hostkey_fingerprint_md5_implicit/1,
         ssh_hostkey_fingerprint_md5/1,
         ssh_hostkey_fingerprint_sha/1,
         ssh_hostkey_fingerprint_sha256/1,
         ssh_hostkey_fingerprint_sha384/1,
         ssh_hostkey_fingerprint_sha512/1,
         ssh_hostkey_fingerprint_list/1,

         openssh_private_key_decode_ed25519/1,
         openssh_private_key_decode_rsa/1,
         openssh_private_key_decode_dsa/1
        ]).


-define(TIMEOUT, 120000). % 2 min


%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------

suite() -> 
    [].

all() -> 
    [{group, ssh_public_key_decode_encode},
     {group, ssh_hostkey_fingerprint},
     {group, ssh_private_key_decode}
    ].

groups() -> 
    [{ssh_public_key_decode_encode, [],
      [ssh_rsa_public_key, ssh_dsa_public_key, ssh_ecdsa_public_key,
       ssh_rfc4716_rsa_comment, ssh_rfc4716_dsa_comment,
       ssh_rfc4716_rsa_subject,
       ssh_known_hosts,
       ssh_auth_keys, ssh1_known_hosts, ssh1_auth_keys, ssh_openssh_public_key_with_comment,
       ssh_openssh_public_key_long_header]},

     {ssh_hostkey_fingerprint, [],
      [ssh_hostkey_fingerprint_md5_implicit,
       ssh_hostkey_fingerprint_md5,
       ssh_hostkey_fingerprint_sha,
       ssh_hostkey_fingerprint_sha256,
       ssh_hostkey_fingerprint_sha384,
       ssh_hostkey_fingerprint_sha512,
       ssh_hostkey_fingerprint_list]},

     {openssh_private_key_decode, [],
      [openssh_private_key_decode_ed25519,
       openssh_private_key_decode_rsa,
       openssh_private_key_decode_dsa]}
    ].
%%-------------------------------------------------------------------
init_per_suite(Config) ->
    application:stop(crypto),
    try crypto:start() of
	ok ->
	    application:start(asn1),
	    Config
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(asn1),
    application:stop(crypto).

%%-------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.
%%-------------------------------------------------------------------
init_per_testcase(ssh_hostkey_fingerprint_md5_implicit, Config) ->
    init_fingerprint_testcase([md5], Config);

init_per_testcase(ssh_hostkey_fingerprint_md5, Config) ->
    init_fingerprint_testcase([md5], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha, Config) ->
    init_fingerprint_testcase([sha], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha256, Config) ->
    init_fingerprint_testcase([sha256], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha384, Config) ->
    init_fingerprint_testcase([sha384], Config);

init_per_testcase(ssh_hostkey_fingerprint_sha512, Config) ->
    init_fingerprint_testcase([sha512], Config);

init_per_testcase(ssh_hostkey_fingerprint_list  , Config) ->
    init_fingerprint_testcase([sha,md5], Config);

init_per_testcase(_, Config) ->
    init_common_per_testcase(Config).

	
init_fingerprint_testcase(Algs, Config) ->
    Hashs = proplists:get_value(hashs, crypto:supports(), []),
    case Algs -- Hashs of
        [] -> init_common_per_testcase(Config);
        UnsupportedAlgs ->  {skip,{UnsupportedAlgs,not_supported}}
    end.

init_common_per_testcase(Config0) ->
    Config = lists:keydelete(watchdog, 1, Config0),
    Dog = ct:timetrap(?TIMEOUT),
    [{watchdog, Dog} | Config].


end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
ssh_rsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(RSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(RSARawSsh2, rfc4716_public_key),

    {ok, RSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_rsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(RSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(RSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == RSARawSsh2 and EncodedOpenSsh
    %% = RSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_dsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(DSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(DSARawSsh2, rfc4716_public_key),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(DSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == DSARawSsh2 and EncodedOpenSsh
    %% = DSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_ecdsa_public_key(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, ECDSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_ecdsa_pub")),
    [{PubKey, Attributes1}] = public_key:ssh_decode(ECDSARawSsh2, public_key),
    [{PubKey, Attributes1}] = public_key:ssh_decode(ECDSARawSsh2, rfc4716_public_key),

    {ok, ECDSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_ecdsa_pub")),
    [{PubKey, Attributes2}] = public_key:ssh_decode(ECDSARawOpenSsh, public_key),
    [{PubKey, Attributes2}] = public_key:ssh_decode(ECDSARawOpenSsh, openssh_public_key),

    %% Can not check EncodedSSh == ECDSARawSsh2 and EncodedOpenSsh
    %% = ECDSARawOpenSsh as line breakpoints may differ

    EncodedSSh = public_key:ssh_encode([{PubKey, Attributes1}], rfc4716_public_key),
    EncodedOpenSsh = public_key:ssh_encode([{PubKey, Attributes2}], openssh_public_key),

    [{PubKey, Attributes1}] =
	public_key:ssh_decode(EncodedSSh, public_key),
    [{PubKey, Attributes2}] =
	public_key:ssh_decode(EncodedOpenSsh, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_rsa_comment_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,
    RSARawSsh2 = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_dsa_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_dsa_comment_pub")),
    [{{_, #'Dss-Parms'{}} = PubKey, Attributes}] =
        public_key:ssh_decode(DSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Comment", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == DSARawSsh2 as line continuation breakpoints may differ
    Encoded  = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key),
    [{PubKey, Attributes}] =
        public_key:ssh_decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_rfc4716_rsa_subject(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, RSARawSsh2} = file:read_file(filename:join(Datadir, "ssh2_subject_pub")),
    [{#'RSAPublicKey'{} = PubKey, Attributes}] =
        public_key:ssh_decode(RSARawSsh2, public_key),

    Headers = proplists:get_value(headers, Attributes),

    Value = proplists:get_value("Subject", Headers, undefined),
    true = Value =/= undefined,

    %% Can not check Encoded == RSARawSsh2 as line continuation breakpoints may differ
    Encoded  = public_key:ssh_encode([{PubKey, Attributes}], rfc4716_public_key),
    [{PubKey, Attributes}] =
        public_key:ssh_decode(Encoded, public_key).

%%--------------------------------------------------------------------
ssh_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {#'RSAPublicKey'{}, Attributes4}] = Decoded =
        public_key:ssh_decode(SshKnownHosts, known_hosts),

    Comment1 = undefined,
    Comment2 = "foo@bar.com",
    Comment3 = "Comment with whitespaces",
    Comment4 = "foo@bar.com Comment with whitespaces",
    	
    Comment1 = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),	

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh1_known_hosts(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshKnownHosts} = file:read_file(filename:join(Datadir, "ssh1_known_hosts")),
    [{#'RSAPublicKey'{}, Attributes1}, {#'RSAPublicKey'{}, Attributes2},{#'RSAPublicKey'{}, Attributes3}] 
	= Decoded = public_key:ssh_decode(SshKnownHosts, known_hosts),

    Value1 = proplists:get_value(hostnames, Attributes1, undefined),
    Value2 = proplists:get_value(hostnames, Attributes2, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment ="dhopson@VMUbuntu-DSH comment with whitespaces",
    Comment = proplists:get_value(comment, Attributes3),

    Encoded = public_key:ssh_encode(Decoded, known_hosts),
    Decoded = public_key:ssh_decode(Encoded, known_hosts).

%%--------------------------------------------------------------------
ssh_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1}, {{_, #'Dss-Parms'{}}, Attributes2},
     {#'RSAPublicKey'{}, Attributes3}, {{_, #'Dss-Parms'{}}, Attributes4}
    ] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(options, Attributes1, undefined),
    true = Value1 =/= undefined,

    Comment1 = Comment2 = "dhopson@VMUbuntu-DSH",
    Comment3 = Comment4 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    Comment1 = proplists:get_value(comment, Attributes1),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh1_auth_keys(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, SshAuthKeys} = file:read_file(filename:join(Datadir, "ssh1_auth_keys")),
    [{#'RSAPublicKey'{}, Attributes1},
     {#'RSAPublicKey'{}, Attributes2}, {#'RSAPublicKey'{}, Attributes3},
     {#'RSAPublicKey'{}, Attributes4}, {#'RSAPublicKey'{}, Attributes5}] = Decoded =
        public_key:ssh_decode(SshAuthKeys, auth_keys),

    Value1 = proplists:get_value(bits, Attributes2, undefined),
    Value2 = proplists:get_value(bits, Attributes3, undefined),
    true = (Value1 =/= undefined) and (Value2 =/= undefined),

    Comment2 = Comment3 = "dhopson@VMUbuntu-DSH",
    Comment4 = Comment5 ="dhopson@VMUbuntu-DSH comment with whitespaces",
    
    undefined = proplists:get_value(comment, Attributes1, undefined),
    Comment2 = proplists:get_value(comment, Attributes2),
    Comment3 = proplists:get_value(comment, Attributes3),
    Comment4 = proplists:get_value(comment, Attributes4),
    Comment5 = proplists:get_value(comment, Attributes5),

    Encoded = public_key:ssh_encode(Decoded, auth_keys),
    Decoded = public_key:ssh_decode(Encoded, auth_keys).

%%--------------------------------------------------------------------
ssh_openssh_public_key_with_comment(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok, DSARawOpenSsh} = file:read_file(filename:join(Datadir, "openssh_dsa_with_comment_pub")),
    [{{_, #'Dss-Parms'{}}, _}] = public_key:ssh_decode(DSARawOpenSsh, openssh_public_key).

%%--------------------------------------------------------------------
ssh_openssh_public_key_long_header(Config) when is_list(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    {ok,RSARawOpenSsh} = file:read_file(filename:join(Datadir, "ssh_rsa_long_header_pub")),
    [{#'RSAPublicKey'{}, _}] = Decoded = public_key:ssh_decode(RSARawOpenSsh, public_key),

    Encoded = public_key:ssh_encode(Decoded, rfc4716_public_key),
    Decoded = public_key:ssh_decode(Encoded, rfc4716_public_key).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5_implicit(_Config) ->
    Expected = "4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = public_key:ssh_hostkey_fingerprint(ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Check of different host keys left to later
ssh_hostkey_fingerprint_md5(_Config) ->
    Expected = "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a",
    Expected = public_key:ssh_hostkey_fingerprint(md5, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead. The Expected is generated with:
%%       $ openssh-7.3p1/ssh-keygen -E sha1 -lf <file>
%%       2048 SHA1:Soammnaqg06jrm2jivMSnzQGlmk none@example.org (RSA)
ssh_hostkey_fingerprint_sha(_Config) ->
    Expected = "SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
    Expected = public_key:ssh_hostkey_fingerprint(sha, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha256(_Config) ->
    Expected = "SHA256:T7F1BahkJWR7iJO8+rpzWOPbp7LZP4MlNrDExdNYOvY",
    Expected = public_key:ssh_hostkey_fingerprint(sha256, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha384(_Config) ->
    Expected = "SHA384:QhkLoGNI4KXdPvC//HxxSCP3uTQVADqxdajbgm+Gkx9zqz8N94HyP1JmH8C4/aEl",
    Expected = public_key:ssh_hostkey_fingerprint(sha384, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_sha512(_Config) ->
    Expected = "SHA512:ezUismvm3ADQQb6Nm0c1DwQ6ydInlJNfsnSQejFkXNmABg1Aenk9oi45CXeBOoTnlfTsGG8nFDm0smP10PBEeA",
    Expected = public_key:ssh_hostkey_fingerprint(sha512, ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
%% Since this kind of fingerprint is not available yet on standard
%% distros, we do like this instead.
ssh_hostkey_fingerprint_list(_Config) ->
    Expected = ["SHA1:Soammnaqg06jrm2jivMSnzQGlmk",
                "MD5:4b:0b:63:de:0f:a7:3a:ab:2c:cc:2d:d1:21:37:1d:3a"],
    Expected = public_key:ssh_hostkey_fingerprint([sha,md5], ssh_hostkey(rsa)).

%%--------------------------------------------------------------------
openssh_private_key_decode_ed25519(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    PrvKey = <<122,222,138,131,85,60,85,254,150,28,71,212,104,168,26,53,
               173,100,101,218,68,164,92,198,81,84,245,110,252,94,77,104>>,

    {ok, PubBin} = file:read_file(filename:join(Datadir, "openssh_auth_ed25519_pub")),
    {ok, PrvBin} = file:read_file(filename:join(Datadir, "openssh_auth_ed25519_priv")),

    [{{ed_pub,ed25519,PubKey},_Headers}] = public_key:ssh_decode(PubBin, public_key),
    [Entry] = public_key:pem_decode(PrvBin),
    {ed_pri,ed25519,PubKey,PrvKey} = public_key:pem_entry_decode(Entry),

    ok.

openssh_private_key_decode_rsa(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    PrvKey =
        #'RSAPrivateKey'{version = 'two-prime',
                         modulus = 27398778494932030090255443996525597419695854838583340211613977140127443653630377441227848913029771405632239323938356225457977441643905756190557534829982488767853157707904204754174477809198592686491333673268818291946756964899974207402578737115111356262899172855087326372137746169409182246711032655574954037493950809542198033586509024885622048258182104795500887291937953485657373294050622580928029614197684597076405345451402530156286497776751479292737484553817780299354222630648064739189144802997126990095881745686419244380124268441814270070690588170117387629026988384479560206927301918009391295378403969842814842169411,
                         publicExponent = 65537,
                         privateExponent = 802268274894709335843877458982446884178347276122517507149964480093757181001826362355863742070954290361296172584001489184031290881710410090936111041682353417848089012946399269470082898452051503202418013015592143403662459612784389032234441560094277929543670181865397856296936614417752120656094597953039302957875008119349228843736116309320742350773163027230587913232710455280834409740074528699424028602735098044572456057794004286551037301633023490008306570679207205999915891344474317623028275309045767902874345621296931019808606685736880955281160041228243426788194606061399315328559566129125172490274026663991876181897,
                         prime1 = 178863139785975262303934050424656525746399818092115379258930662790502623430620782172817698646071740411707474167549541019081008083552799036026443006052744586450876116958917222191432881564197764107912734838054177683126606736527691484932720224704895834275453027952132120744123642735881672266635843498509695991447,
                         prime2 = 153182922583808854724186945695832877500433838476494648722720004867889580919499251921364827204843643247738490012542067340595103222576762349116258992995258402003665549101959734788017247681531367180946609085500570477510086737523177248466890549906817244768257661141327550129083285484829878716203308481858252309813,
                         exponent1 = 40757772396718717171169737843383440735717760705977555790665891299775183153224754886077475495986013569562833501963544952911420643602506993057645297349461947511442146126073360028790738869336854131064418299151640897810591650568450564352735765075345444391254032370067917225273394885601368595296392675995907654245,
                         exponent2 = 90233405959197717878865938913707191905400740564644031828442037138179612606577484786064195099607684934906150829824168174980148006585225300023394089210399478300067267697348910714427054133474501884743638612474625376264963280316191734090121758383091684609893449629384165780143434031796656359598833085770447692453,
                         coefficient = 11561797597069097096214582320411054712775135020239267336303820856654931581641251175935182859267368805934208755764957367231144649130638010827262644208307551070487352599345455918063435917741486960007008834069607532788619680452202904989216125714393673072333712179721484770141087400643496661118463612067958456553,
                         otherPrimeInfos = asn1_NOVALUE},

    PubKey = #'RSAPublicKey'{modulus = PrvKey#'RSAPrivateKey'.modulus,
                             publicExponent = PrvKey#'RSAPrivateKey'.publicExponent},

    {ok, PubBin} = file:read_file(filename:join(Datadir, "openssh_auth_rsa_pub")),
    {ok, PrvBin} = file:read_file(filename:join(Datadir, "openssh_auth_rsa_priv")),

    [{#'RSAPublicKey'{}=PubKey,_Headers}] = public_key:ssh_decode(PubBin, public_key),
    [Entry] = public_key:pem_decode(PrvBin),
    PrvKey = public_key:pem_entry_decode(Entry),

    ok.

openssh_private_key_decode_dsa(Config) ->
    Datadir = proplists:get_value(data_dir, Config),

    PrvKey =
        #'DSAPrivateKey'{version = undefined,
                         p = 129895791080698497518780501924918484927351596703374117774720101506125690448509356347180861746786715322888608347349248204185194659357357464902961340041660601225983851077648028948221171542896742400322526082649641714323526861324374988273476481805949369114517862362512557795740725957408701917756555760862537284721,
                         q = 826926494655098715994195225312451596198899523159,
                         g = 101759483125956109452088255285757179274813134108709528878385941562967679410124801717522246674288536644831783779456349503897338927456328105730495814173135979471102799518633559276947205268034208552334498748311732254478629975888299784487202154149752072089868046301494997785932150806569394947232732807340671132734,
                         y = 123106021906266088416688498477704339796187572246466754195960490213756208574625816584259082040467529760605585104222614262677769993822078029948673485887182928621382166415347351064151106721417255654419950864429660149694821757769379224147255058192381040047257331075970886790811801325032003814325781769634759807209, 
                         x = 658941921020344611967113479553890866227248487441},
    Y = PrvKey#'DSAPrivateKey'.y,
    DssParms = #'Dss-Parms'{p = PrvKey#'DSAPrivateKey'.p,
                            q = PrvKey#'DSAPrivateKey'.q,
                            g = PrvKey#'DSAPrivateKey'.g},
    PubKey = {Y, DssParms},

    {ok, PubBin} = file:read_file(filename:join(Datadir, "openssh_auth_dsa_pub")),
    {ok, PrvBin} = file:read_file(filename:join(Datadir, "openssh_auth_dsa_priv")),

    [{PubKey,_Headers}] = public_key:ssh_decode(PubBin, public_key),
    [Entry] = public_key:pem_decode(PrvBin),
    PrvKey = public_key:pem_entry_decode(Entry),

    ok.

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
ssh_hostkey(rsa) ->
    [{PKdecoded,_}] =
	public_key:ssh_decode(
	  <<"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDYXcYmsyJBstl4EfFYzfQJmSiUE162zvSGSoMYybShYOI6rnnyvvihfw8Aml+2gZ716F2tqG48FQ/yPZEGWNPMrCejPpJctaPWhpNdNMJ8KFXSEgr5bY2mEpa19DHmuDeXKzeJJ+X7s3fVdYc4FMk5731KIW6Huf019ZnTxbx0VKG6b1KAJBg3vpNsDxEMwQ4LFMB0JHVklOTzbxmpaeULuIxvl65A+eGeFVeo2Q+YI9UnwY1vSgmc9Azwy8Ie9Z0HpQBN5I7Uc5xnknT8V6xDhgNfXEfzsgsRdDfZLECt1WO/1gP9wkosvAGZWt5oG8pbNQWiQdFq536ck8WQD9WD none@example.org">>,
	  public_key),
    PKdecoded.

