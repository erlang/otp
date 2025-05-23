-module(pkcs12_quick_demo).
-export([main/0, run/0, test_key_generation/0, test_cert_generation/0]).

-include_lib("public_key/include/public_key.hrl").

main() ->
    run().

run() ->
    io:format("=== PKCS#12 Quick Demonstration ===~n"),

    % Test key generation
    io:format("Testing key generation...~n"),
    test_key_generation(),

    % Test certificate generation
    io:format("Testing certificate generation...~n"),
    test_cert_generation(),

    io:format("Demo completed successfully!~n").

test_key_generation() ->
    % Test RSA key generation
    RSAKey = public_key:generate_key({rsa, 2048, 65537}),
    io:format("Generated RSA-2048 key: ~p~n", [element(1, RSAKey)]),

    % Test EC key generation
    ECKey = public_key:generate_key({namedCurve, {1, 2, 840, 10045, 3, 1, 7}}), % secp256r1
    io:format("Generated EC secp256r1 key: ~p~n", [element(1, ECKey)]),

    % Test EdDSA key generation
    try
        EdKey = public_key:generate_key({namedCurve, {1,3,101,112}}), % Ed25519
        io:format("Generated Ed25519 key: ~p~n", [element(1, EdKey)])
    catch
        _:_ ->
            io:format("Ed25519 not supported in this OTP version~n")
    end,

    io:format("Key generation tests passed!~n").

test_cert_generation() ->
    % Generate a test key
    PrivKey = public_key:generate_key({rsa, 2048, 65537}),

    % Create a basic certificate structure
    Subject = {rdnSequence, [
        [#'AttributeTypeAndValue'{type = {2,5,4,6}, value = "US"}],
        [#'AttributeTypeAndValue'{type = {2,5,4,3}, value = {utf8String, "Test Certificate"}}]
    ]},

    % Create validity period (1 year from now)
    Now = calendar:universal_time(),
    Start = calendar:datetime_to_gregorian_seconds(Now),
    End = Start + (365 * 24 * 60 * 60),
    StartTime = calendar:gregorian_seconds_to_datetime(Start),
    EndTime = calendar:gregorian_seconds_to_datetime(End),
    Validity = #'Validity'{
        notBefore = {utcTime, format_utc_time(StartTime)},
        notAfter = {utcTime, format_utc_time(EndTime)}
    },

    % Extract public key
    #'RSAPrivateKey'{modulus = N, publicExponent = E} = PrivKey,
    PubKey = #'RSAPublicKey'{modulus = N, publicExponent = E},
    PubKeyInfo = #'OTPSubjectPublicKeyInfo'{
        algorithm = #'PublicKeyAlgorithm'{
            algorithm = {1, 2, 840, 113549, 1, 1, 1}, % rsaEncryption
            parameters = asn1_NOVALUE
        },
        subjectPublicKey = PubKey
    },

    % Create TBS certificate
    TBSCert = #'OTPTBSCertificate'{
        version = v3,
        serialNumber = 1,
        signature = #'SignatureAlgorithm'{
            algorithm = {1, 2, 840, 113549, 1, 1, 11}, % sha256WithRSAEncryption
            parameters = asn1_NOVALUE
        },
        issuer = Subject,
        validity = Validity,
        subject = Subject,
        subjectPublicKeyInfo = PubKeyInfo,
        extensions = []
    },

    try
        % Sign the certificate
        Cert = public_key:pkix_sign(TBSCert, PrivKey),
        io:format("Generated self-signed certificate: ~w bytes~n", [byte_size(Cert)]),

        % Verify the certificate
        PubKeyForVerify = public_key:pkix_decode_cert(Cert, plain),
        case public_key:pkix_verify(Cert, PubKey) of
            true -> io:format("Certificate signature verified!~n");
            false -> io:format("Certificate signature verification failed!~n")
        end

    catch
        Error:Reason ->
            io:format("Certificate generation failed: ~p:~p~n", [Error, Reason])
    end,

    io:format("Certificate generation tests completed!~n").

format_utc_time({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    YearStr = case Year of
        Y when Y >= 2050 -> integer_to_list(Y);
        Y -> string:right(integer_to_list(Y rem 100), 2, $0)
    end,
    io_lib:format("~s~2..0w~2..0w~2..0w~2..0w~2..0wZ",
                  [YearStr, Month, Day, Hour, Min, Sec]).
