<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Getting Started

This section describes examples of how to use the Public Key API. Keys and
certificates used in the following sections are generated only for testing the
Public Key application.

Some shell printouts in the following examples are abbreviated for increased
readability.

## PEM Files

Public-key data (keys, certificates, and so on) can be stored in Privacy
Enhanced Mail (PEM) format. The PEM files have the following structure:

```text
    <text>
    -----BEGIN <SOMETHING>-----
    <Attribute> : <Value>
    <Base64 encoded DER data>
    -----END <SOMETHING>-----
    <text>
```

A file can contain several `BEGIN/END` blocks. Text lines between blocks are
ignored. Attributes, if present, are ignored except for `Proc-Type` and
`DEK-Info`, which are used when `DER` data is encrypted.

### DSA Private Key

A DSA private key can look as follows:

> #### Note {: .info }
>
> File handling is not done by the Public Key application.

```erlang
1> {ok, PemBin} = file:read_file("dsa.pem").
{ok,<<"-----BEGIN DSA PRIVATE KEY-----\nMIIBuw"...>>}
```

The following PEM file has only one entry, a private DSA key:

```erlang
2> [DSAEntry] =  public_key:pem_decode(PemBin).
[{'DSAPrivateKey',<<48,130,1,187,2,1,0,2,129,129,0,183,
                    179,230,217,37,99,144,157,21,228,204,
		    162,207,61,246,...>>,
		    not_encrypted}]
```

```c
3> Key = public_key:pem_entry_decode(DSAEntry).
#'DSAPrivateKey'{version = 0,
                 p = 12900045185019966618...6593,
                 q = 1216700114794736143432235288305776850295620488937,
                 g = 10442040227452349332...47213,
                 y = 87256807980030509074...403143,
                 x = 510968529856012146351317363807366575075645839654}
```

### RSA Private Key with Password

An RSA private key encrypted with a password can look as follows:

```erlang
1> {ok, PemBin} = file:read_file("rsa.pem").
{ok,<<"Bag Attribute"...>>}
```

The following PEM file has only one entry, a private RSA key:

```erlang
2>[RSAEntry] = public_key:pem_decode(PemBin).
[{'RSAPrivateKey',<<224,108,117,203,152,40,15,77,128,126,
                    221,195,154,249,85,208,202,251,109,
                    119,120,57,29,89,19,9,...>>,
                  {"DES-EDE3-CBC",<<"kÙeø¼pµL">>}}]
```

In this following example, the password is `"abcd1234"`:

```erlang
3> Key = public_key:pem_entry_decode(RSAEntry, "abcd1234").
    #'RSAPrivateKey'{version = 'two-prime',
                 modulus = 1112355156729921663373...2737107,
                 publicExponent = 65537,
                 privateExponent = 58064406231183...2239766033,
                 prime1 = 11034766614656598484098...7326883017,
                 prime2 = 10080459293561036618240...77738643771,
                 exponent1 = 77928819327425934607...22152984217,
                 exponent2 = 36287623121853605733...20588523793,
                 coefficient = 924840412626098444...41820968343,
                 otherPrimeInfos = asn1_NOVALUE}
```

### X509 Certificates

The following is an example of X509 certificates:

```erlang
1> {ok, PemBin} = file:read_file("cacerts.pem").
{ok,<<"-----BEGIN CERTIFICATE-----\nMIIC7jCCAl"...>>}
```

The following file includes two certificates:

```erlang
2> [CertEntry1, CertEntry2] = public_key:pem_decode(PemBin).
[{'Certificate',<<48,130,2,238,48,130,2,87,160,3,2,1,2,2,
                  9,0,230,145,97,214,191,2,120,150,48,13,
                  ...>>,
                not_encrypted},
 {'Certificate',<<48,130,3,200,48,130,3,49,160,3,2,1,2,2,1,
                  1,48,13,6,9,42,134,72,134,247,...>>,
                not_encrypted}]
```

Certificates can be decoded as usual:

```erlang
2> Cert = public_key:pem_entry_decode(CertEntry1).
#'Certificate'{
    tbsCertificate =
        #'TBSCertificate'{
            version = v3,serialNumber = 16614168075301976214,
            signature =
                #'AlgorithmIdentifier'{
                    algorithm = {1,2,840,113549,1,1,5},
                    parameters = <<5,0>>},
            issuer =
                {rdnSequence,
                    [[#'AttributeTypeAndValue'{
                          type = {2,5,4,3},
                          value = <<19,8,101,114,108,97,110,103,67,65>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,11},
                          value = <<19,10,69,114,108,97,110,103,32,79,84,80>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,10},
                          value = <<19,11,69,114,105,99,115,115,111,110,32,65,66>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,7},
                          value = <<19,9,83,116,111,99,107,104,111,108,109>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,6},
                          value = <<19,2,83,69>>}],
                     [#'AttributeTypeAndValue'{
                          type = {1,2,840,113549,1,9,1},
                          value = <<22,22,112,101,116,101,114,64,101,114,...>>}]]},
            validity =
                #'Validity'{
                    notBefore = {utcTime,"080109082929Z"},
                    notAfter = {utcTime,"080208082929Z"}},
            subject =
                {rdnSequence,
                    [[#'AttributeTypeAndValue'{
                          type = {2,5,4,3},
                          value = <<19,8,101,114,108,97,110,103,67,65>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,11},
                          value = <<19,10,69,114,108,97,110,103,32,79,84,80>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,10},
                          value = <<19,11,69,114,105,99,115,115,111,110,32,...>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,7},
                          value = <<19,9,83,116,111,99,107,104,111,108,...>>}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,6},
                          value = <<19,2,83,69>>}],
                     [#'AttributeTypeAndValue'{
                          type = {1,2,840,113549,1,9,1},
                          value = <<22,22,112,101,116,101,114,64,...>>}]]},
            subjectPublicKeyInfo =
                #'SubjectPublicKeyInfo'{
                    algorithm =
                        #'AlgorithmIdentifier'{
                            algorithm = {1,2,840,113549,1,1,1},
                            parameters = <<5,0>>},
                    subjectPublicKey =
                        {0,<<48,129,137,2,129,129,0,203,209,187,77,73,231,90,...>>}},
            issuerUniqueID = asn1_NOVALUE,
            subjectUniqueID = asn1_NOVALUE,
            extensions =
                [#'Extension'{
                     extnID = {2,5,29,19},
                     critical = true,
                     extnValue = [48,3,1,1,255]},
                 #'Extension'{
                     extnID = {2,5,29,15},
                     critical = false,
                     extnValue = [3,2,1,6]},
                 #'Extension'{
                     extnID = {2,5,29,14},
                     critical = false,
                     extnValue = [4,20,27,217,65,152,6,30,142|...]},
                 #'Extension'{
                     extnID = {2,5,29,17},
                     critical = false,
                     extnValue = [48,24,129,22,112,101,116,101|...]}]},
    signatureAlgorithm =
        #'AlgorithmIdentifier'{
            algorithm = {1,2,840,113549,1,1,5},
            parameters = <<5,0>>},
    signature =
    <<163,186,7,163,216,152,63,47,154,234,139,73,154,96,120,
    165,2,52,196,195,109,167,192,...>>}
```

Parts of certificates can be decoded with `public_key:der_decode/2`, using the
ASN.1 type of that part. However, an application-specific certificate extension
requires application-specific ASN.1 decode/encode-functions. In the recent
example, the first value of `rdnSequence` is of ASN.1 type
`'X520CommonName'. ({2,5,4,3} = ?id-at-commonName)`:

```erlang
public_key:der_decode('X520CommonName', <<19,8,101,114,108,97,110,103,67,65>>).
{printableString,"erlangCA"}
```

However, certificates can also be decoded using `pkix_decode_cert/2`, which can
customize and recursively decode standard parts of a certificate:

```text
3>{_, DerCert, _} = CertEntry1.
```

```text
4> public_key:pkix_decode_cert(DerCert, otp).
#'OTPCertificate'{
    tbsCertificate =
        #'OTPTBSCertificate'{
            version = v3,serialNumber = 16614168075301976214,
            signature =
                #'SignatureAlgorithm'{
                    algorithm = {1,2,840,113549,1,1,5},
                    parameters = 'NULL'},
            issuer =
                {rdnSequence,
                    [[#'AttributeTypeAndValue'{
                          type = {2,5,4,3},
                          value = {printableString,"erlangCA"}}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,11},
                          value = {printableString,"Erlang OTP"}}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,10},
                          value = {printableString,"Ericsson AB"}}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,7},
                          value = {printableString,"Stockholm"}}],
                     [#'AttributeTypeAndValue'{type = {2,5,4,6},value = "SE"}],
                     [#'AttributeTypeAndValue'{
                          type = {1,2,840,113549,1,9,1},
                          value = "peter@erix.ericsson.se"}]]},
            validity =
                #'Validity'{
                    notBefore = {utcTime,"080109082929Z"},
                    notAfter = {utcTime,"080208082929Z"}},
            subject =
                {rdnSequence,
                    [[#'AttributeTypeAndValue'{
                          type = {2,5,4,3},
                          value = {printableString,"erlangCA"}}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,11},
                          value = {printableString,"Erlang OTP"}}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,10},
                          value = {printableString,"Ericsson AB"}}],
                     [#'AttributeTypeAndValue'{
                          type = {2,5,4,7},
                          value = {printableString,"Stockholm"}}],
                     [#'AttributeTypeAndValue'{type = {2,5,4,6},value = "SE"}],
                     [#'AttributeTypeAndValue'{
                          type = {1,2,840,113549,1,9,1},
                          value = "peter@erix.ericsson.se"}]]},
            subjectPublicKeyInfo =
                #'OTPSubjectPublicKeyInfo'{
                    algorithm =
                        #'PublicKeyAlgorithm'{
                            algorithm = {1,2,840,113549,1,1,1},
                            parameters = 'NULL'},
                    subjectPublicKey =
                        #'RSAPublicKey'{
                            modulus =
                                1431267547247997...37419,
                            publicExponent = 65537}},
            issuerUniqueID = asn1_NOVALUE,
            subjectUniqueID = asn1_NOVALUE,
            extensions =
                [#'Extension'{
                     extnID = {2,5,29,19},
                     critical = true,
                     extnValue =
                         #'BasicConstraints'{
                             cA = true,pathLenConstraint = asn1_NOVALUE}},
                 #'Extension'{
                     extnID = {2,5,29,15},
                     critical = false,
                     extnValue = [keyCertSign,cRLSign]},
                 #'Extension'{
                     extnID = {2,5,29,14},
                     critical = false,
                     extnValue = [27,217,65,152,6,30,142,132,245|...]},
                 #'Extension'{
                     extnID = {2,5,29,17},
                     critical = false,
                     extnValue = [{rfc822Name,"peter@erix.ericsson.se"}]}]},
    signatureAlgorithm =
        #'SignatureAlgorithm'{
            algorithm = {1,2,840,113549,1,1,5},
            parameters = 'NULL'},
    signature =
         <<163,186,7,163,216,152,63,47,154,234,139,73,154,96,120,
           165,2,52,196,195,109,167,192,...>>}
```

This call is equivalent to `public_key:pem_entry_decode(CertEntry1)`:

```text
5> public_key:pkix_decode_cert(DerCert, plain).
#'Certificate'{ ...}
```

### Encoding Public-Key Data to PEM Format

If you have public-key data and want to create a PEM file this can be done by
calling functions `public_key:pem_entry_encode/2` and `pem_encode/1` and saving
the result to a file. For example, assume that you have
`PubKey = 'RSAPublicKey'{}`. Then you can create a PEM-"RSA PUBLIC KEY" file
(ASN.1 type `'RSAPublicKey'`) or a PEM-"PUBLIC KEY" file
(`'SubjectPublicKeyInfo'` ASN.1 type).

The second element of the PEM-entry is the ASN.1 `DER` encoded key data:

```erlang
1> PemEntry = public_key:pem_entry_encode('RSAPublicKey', RSAPubKey).
{'RSAPublicKey', <<48,72,...>>, not_encrypted}

2> PemBin = public_key:pem_encode([PemEntry]).
<<"-----BEGIN RSA PUBLIC KEY-----\nMEgC...>>

3> file:write_file("rsa_pub_key.pem", PemBin).
ok
```

or:

```erlang
1> PemEntry = public_key:pem_entry_encode('SubjectPublicKeyInfo', RSAPubKey).
{'SubjectPublicKeyInfo', <<48,92...>>, not_encrypted}

2> PemBin = public_key:pem_encode([PemEntry]).
<<"-----BEGIN PUBLIC KEY-----\nMFw...>>

3> file:write_file("pub_key.pem", PemBin).
ok
```

## RSA Public-Key Cryptography

Suppose you have the following private key and a corresponding public key:

- `PrivateKey = #'RSAPrivateKey{}'` and the plaintext `Msg = binary()`
- `PublicKey = #'RSAPublicKey'{}`

Then you can proceed as follows:

Encrypt with the private key:

```erlang
RsaEncrypted = public_key:encrypt_private(Msg, PrivateKey),
Msg = public_key:decrypt_public(RsaEncrypted, PublicKey),
```

Encrypt with the public key:

```erlang
RsaEncrypted = public_key:encrypt_public(Msg, PublicKey),
Msg = public_key:decrypt_private(RsaEncrypted, PrivateKey),
```

> #### Note {: .info }
>
> You normally do only one of the encrypt or decrypt operations, and the peer
> does the other. This normally used in legacy applications as a primitive
> digital signature.

## Digital Signatures

Suppose you have the following private key and a corresponding public key:

- `PrivateKey = #'RSAPrivateKey{}'` or `#'DSAPrivateKey'{}` and the plaintext
  `Msg = binary()`
- `PublicKey = #'RSAPublicKey'{}` or `{integer(), #'DssParams'{}}`

Then you can proceed as follows:

```erlang
Signature = public_key:sign(Msg, sha, PrivateKey),
true = public_key:verify(Msg, sha, Signature, PublicKey),
```

> #### Note {: .info }
>
> You normally do only one of the sign or verify operations, and the peer does
> the other.

It can be appropriate to calculate the message digest before calling `sign` or
`verify`, and then use `none` as second argument:

```erlang
Digest = crypto:sha(Msg),
Signature = public_key:sign(Digest, none, PrivateKey),
true = public_key:verify(Digest, none, Signature, PublicKey),
```

[](){: #verify_hostname }

## Verifying a certificate hostname

### Background

When a client checks a server certificate there are a number of checks available
like checks that the certificate is not revoked, not forged or not out-of-date.

There are however attacks that are not detected by those checks. Suppose a bad
guy has succeeded with a DNS infection. Then the client could believe it is
connecting to one host but ends up at another but evil one. Though it is evil,
it could have a perfectly legal certificate\! The certificate has a valid
signature, it is not revoked, the certificate chain is not faked and has a
trusted root and so on.

To detect that the server is not the intended one, the client must additionally
perform a _hostname verification_. This procedure is described in
[RFC 6125](https://tools.ietf.org/html/rfc6125). The idea is that the
certificate lists the hostnames it could be fetched from. This is checked by the
certificate issuer when the certificate is signed. So if the certificate is
issued by a trusted root the client could trust the host names signed in it.

There is a default hostname matching procedure defined in
[RFC 6125, section 6](https://tools.ietf.org/html/rfc6125#section/6) as well as
protocol dependent variations defined in
[RFC 6125 appendix B](https://tools.ietf.org/html/rfc6125#appendix-B). The
default procedure is implemented in
[public_key:pkix_verify_hostname/2,3](`public_key:pkix_verify_hostname/2`). It
is possible for a client to hook in modified rules using the options list.

Some terminology is needed: the certificate presents hostname(s) on which it is
valid. Those are called _Presented IDs_. The hostname(s) the client believes it
connects to are called _Reference IDs_. The matching rules aims to verify that
there is at least one of the Reference IDs that matches one of the Presented
IDs. If not, the verification fails.

The IDs contains normal fully qualified domain names like e.g `foo.example.com`,
but IP addresses are not recommended. The rfc describes why this is not
recommended as well as security considerations about how to acquire the
Reference IDs.

Internationalized domain names are not supported.

### The verification process

Traditionally the Presented IDs were found in the `Subject` certificate field as
`CN` names. This is still quite common. When printing a certificate they show up
as:

```text
 $ openssl x509 -text < cert.pem
 ...
 Subject: C=SE, CN=example.com, CN=*.example.com, O=erlang.org
 ...
```

The example `Subject` field has one C, two CN and one O part. It is only the CN
(Common Name) that is used by hostname verification. The two other (C and O) is
not used here even when they contain a domain name like the O part. The C and O
parts are defined elsewhere and meaningful only for other functions.

In the example the Presented IDs are `example.com` as well as hostnames matching
`*.example.com`. For example `foo.example.com` and `bar.example.com` both
matches but not `foo.bar.example.com`. The name `erlang.org` matches neither
since it is not a CN.

In case where the Presented IDs are fetched from the `Subject` certificate
field, the names may contain wildcard characters. The function handles this as
defined in
[chapter 6.4.3 in RFC 6125](https://tools.ietf.org/html/rfc6125#section-6.4.3).

There may only be one wildcard character and that is in the first label, for
example: `*.example.com`. This matches `foo.example.com` but neither
`example.com` nor `foo.bar.example.com`.

There may be label characters before or/and after the wildcard. For example:
`a*d.example.com` matches `abcd.example.com` and `ad.example.com`, but not
`ab.cd.example.com`.

In the previous example there is no indication of which protocols are expected.
So a client has no indication of whether it is a web server, an ldap server or
maybe a sip server it is connected to. There are fields in the certificate that
can indicate this. To be more exact, the rfc introduces the usage of the
`X509v3 Subject Alternative Name` in the `X509v3 extensions` field:

```text
 $ openssl x509 -text < cert.pem
 ...
 X509v3 extensions:
     X509v3 Subject Alternative Name:
         DNS:kb.example.org, URI:https://www.example.org
 ...
```

Here `kb.example.org` serves any protocol while `www.example.org` presents a
secure web server.

The next example has both `Subject` and `Subject Alternate Name` present:

```text
 $ openssl x509 -text < cert.pem
 ...
 Subject: C=SE, CN=example.com, CN=*.example.com, O=erlang.org
 ...
 X509v3 extensions:
     X509v3 Subject Alternative Name:
         DNS:kb.example.org, URI:https://www.example.org
 ...
```

The RFC states that if a certificate defines Reference IDs in a
`Subject Alternate Name` field, the `Subject` field MUST NOT be used for host
name checking, even if it contains valid CN names. Therefore only
`kb.example.org` and `https://www.example.org` matches. The match fails both for
`example.com` and `foo.example.com` because they are in the `Subject` field
which is not checked because the `Subject Alternate Name` field is present.

[](){: #verify_hostname_examples }

### Function call examples

> #### Note {: .info }
>
> Other applications like ssl/tls or https might have options that are passed
> down to the `public_key:pkix_verify_hostname`. You will probably not have to
> call it directly

Suppose our client expects to connect to the web server https://www.example.net.
This URI is therefore the Reference IDs of the client. The call will be:

```erlang
 public_key:pkix_verify_hostname(CertFromHost,
                                 [{uri_id, "https://www.example.net"}
                                 ]).
```

The call will return `true` or `false` depending on the check. The caller do not
need to handle the matching rules in the rfc. The matching will proceed as:

- If there is a `Subject Alternate Name` field, the `{uri_id,string()}` in the
  function call will be compared to any `{uniformResourceIdentifier,string()}`
  in the Certificate field. If the two `strings()` are equal (case insensitive),
  there is a match. The same applies for any `{dns_id,string()}` in the call
  which is compared with all `{dNSName,string()}` in the Certificate field.
- If there is NO `Subject Alternate Name` field, the `Subject` field will be
  checked. All `CN` names will be compared to all hostnames _extracted_ from
  `{uri_id,string()}` and from `{dns_id,string()}`.

### Extending the search mechanism

The caller can use own extraction and matching rules. This is done with the two
options `fqdn_fun` and `match_fun`.

[](){: #hostname_extraction }

#### Hostname extraction

The `fqdn_fun` extracts hostnames (Fully Qualified Domain Names) from uri_id or
other ReferenceIDs that are not pre-defined in the public_key function. Suppose
you have some URI with a very special protocol-part: `myspecial://example.com"`.
Since this a non-standard URI there will be no hostname extracted for matching
CN-names in the `Subject`.

To "teach" the function how to extract, you can give a fun which replaces the
default extraction function. The `fqdn_fun` takes one argument and returns
either a `t:string/0` to be matched to each CN-name or the atom `default` which
will invoke the default fqdn extraction function. The return value `undefined`
removes the current URI from the fqdn extraction.

```erlang
 ...
 Extract = fun({uri_id, "myspecial://"++HostName}) -> HostName;
              (_Else) -> default
           end,
 ...
 public_key:pkix_verify_hostname(CertFromHost, RefIDs,
                                 [{fqdn_fun, Extract}])
 ...
```

[](){: #redefining_match_op }

#### Re-defining the match operation

The default matching handles dns_id and uri_id. In an uri_id the value is tested
for equality with a value from the `Subject Alternate Name`. If some other kind
of matching is needed, use the `match_fun` option.

The `match_fun` takes two arguments and returns either `true`, `false` or
`default`. The value `default` will invoke the default match function.

```erlang
 ...
 Match = fun({uri_id,"myspecial://"++A},
             {uniformResourceIdentifier,"myspecial://"++B}) ->
                                                    my_match(A,B);
            (_RefID, _PresentedID) ->
                                default
         end,
 ...
 public_key:pkix_verify_hostname(CertFromHost, RefIDs,
                                 [{match_fun, Match}]),
 ...
```

In case of a match operation between a ReferenceID and a CN value from the
`Subject` field, the first argument to the fun is the extracted hostname from
the ReferenceID, and the second argument is the tuple `{cn, string()}` taken
from the `Subject` field. That makes it possible to have separate matching rules
for Presented IDs from the `Subject` field and from the `Subject Alternate Name`
field.

The default matching transformes the ascii values in strings to lowercase before
comparing. The `match_fun` is however called without any transformation applied
to the strings. The reason is to enable the user to do unforeseen handling of
the strings where the original format is needed.

### "Pinning" a Certificate

The [RFC 6125](https://tools.ietf.org/html/rfc6125) defines _pinning_ as:

> "The act of establishing a cached name association between the application
> service's certificate and one of the client's reference identifiers, despite
> the fact that none of the presented identifiers matches the given reference
> identifier. ..."

The purpose is to have a mechanism for a human to accept an otherwise faulty
Certificate. In for example a web browser, you could get a question like

> Warning: you wanted to visit the site www.example.com, but the certificate is
> for shop.example.com. Accept anyway (yes/no)?"

This could be accomplished with the option `fail_callback` which will be called
if the hostname verification fails:

```erlang
 -include_lib("public_key/include/public_key.hrl"). % Record def
 ...
 Fail = fun(#'OTPCertificate'{}=C) ->
              case in_my_cache(C) orelse my_accept(C) of
                  true ->
                       enter_my_cache(C),
                       true;
                  false ->
                       false
         end,
 ...
 public_key:pkix_verify_hostname(CertFromHost, RefIDs,
                                 [{fail_callback, Fail}]),
 ...
```
