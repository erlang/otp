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
# Uniform Resource Identifiers

## Basics

At the time of writing this document, in October 2020, there are two major
standards concerning Universal Resource Identifiers and Universal Resource
Locators:

- [RFC 3986 - Uniform Resource Identifier (URI): Generic Syntax](https://www.ietf.org/rfc/rfc3986.txt)
- [WHAT WG URL - Living standard](https://url.spec.whatwg.org/)

The former is a classical standard with a proper formal syntax, using the so
called [Augmented Backus-Naur Form (ABNF)](https://www.ietf.org/rfc/rfc2234.txt)
for describing the grammar, while the latter is a living document describing the
current pratice, that is, how a majority of Web browsers work with URIs. WHAT WG
URL is Web focused and it has no formal grammar but a plain english description
of the algorithms that should be followed.

What is the difference between them, if any? They provide an overlapping
definition for resource identifiers and they are not compatible. The
`m:uri_string` module implements
[RFC 3986](https://www.ietf.org/rfc/rfc3986.txt) and the term URI will be used
throughout this document. A URI is an identifier, a string of characters that
identifies a particular resource.

For a more complete problem statement regarding the URIs check the
[URL Problem Statement and Directions](https://tools.ietf.org/html/draft-ruby-url-problem-01).

## What is a URI?

Let's start with what it is not. It is not the text that you type in the address
bar in your Web browser. Web browsers do all possible heuristics to convert the
input into a valid URI that could be sent over the network.

A URI is an identifier consisting of a sequence of characters matching the
syntax rule named `URI` in [RFC 3986](https://www.ietf.org/rfc/rfc3986.txt).

It is crucial to clarify that a _character_ is a symbol that is displayed on a
terminal or written to paper and should not be confused with its internal
representation.

A URI more specifically, is a sequence of characters from a subset of the US
ASCII character set. The generic URI syntax consists of a hierarchical sequence
of components referred to as the scheme, authority, path, query, and fragment.
There is a formal description for each of these components in
[ABNF](https://www.ietf.org/rfc/rfc2234.txt) notation in
[RFC 3986](https://www.ietf.org/rfc/rfc3986.txt):

```text
    URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
    hier-part   = "//" authority path-abempty
                   / path-absolute
                   / path-rootless
                   / path-empty
    scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    authority   = [ userinfo "@" ] host [ ":" port ]
    userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

    reserved    = gen-delims / sub-delims
    gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
    sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
                / "*" / "+" / "," / ";" / "="

    unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
```

## The uri_string module

As producing and consuming standard URIs can get quite complex, Erlang/OTP
provides a module, `m:uri_string`, to handle all the most difficult operations
such as parsing, recomposing, normalizing and resolving URIs against a base URI.

The API functions in `m:uri_string` work on two basic data types
[`uri_string()`](`t:uri_string:uri_string/0`) and
[`uri_map()`](`t:uri_string:uri_map/0`).
[`uri_string()`](`t:uri_string:uri_string/0`) represents a standard URI, while
[`uri_map()`](`t:uri_string:uri_map/0`) is a wider datatype, that can represent
URI components using [Unicode](unicode_usage.md#what-unicode-is) characters.
[`uri_map()`](`t:uri_string:uri_map/0`) is a convenient choice for enabling
operations such as producing standard compliant URIs out of components that have
special or [Unicode](unicode_usage.md#what-unicode-is) characters. It is easier
to explain this by an example.

Let's say that we would like to create the following URI and send it over the
network: `http://cities/örebro?foo bar`. This is not a valid URI as it contains
characters that are not allowed in a URI such as "ö" and the space. We can
verify this by parsing the URI:

```erlang
  1> uri_string:parse("http://cities/örebro?foo bar").
  {error,invalid_uri,":"}
```

The URI parser tries all possible combinations to interpret the input and fails
at the last attempt when it encounters the colon character `":"`. Note, that the
inital fault occurs when the parser attempts to interpret the character `"ö"`
and after a failure back-tracks to the point where it has another possible
parsing alternative.

The proper way to solve this problem is to use `uri_string:recompose/1` with a
[`uri_map()`](`t:uri_string:uri_map/0`) as input:

```erlang
  2> uri_string:recompose(#{scheme => "http", host => "cities", path => "/örebro",
  query => "foo bar"}).
  "http://cities/%C3%B6rebro?foo%20bar"
```

The result is a valid URI where all the special characters are encoded as
defined by the standard. Applying `uri_string:parse/1` and
`uri_string:percent_decode/1` on the URI returns the original input:

```erlang
  3> uri_string:percent_decode(uri_string:parse("http://cities/%C3%B6rebro?foo%20bar")).
  #{host => "cities",path => "/örebro",query => "foo bar",
  scheme => "http"}
```

This symmetric property is heavily used in our property test suite.

## Percent-encoding

As you have seen in the previous chapter, a standard URI can only contain a
strict subset of the US ASCII character set, moreover the allowed set of
characters is not the same in the different URI components. Percent-encoding is
a mechanism to represent a data octet in a component when that octet's
corresponding character is outside of the allowed set or is being used as a
delimiter. This is what you see when `"ö"` is encoded as `%C3%B6` and `space` as
`%20`. Most of the API functions are expecting UTF-8 encoding when handling
percent-encoded triplets. The UTF-8 encoding of the
[Unicode](unicode_usage.md#what-unicode-is) character `"ö"` is two octets:
`OxC3 0xB6`. The character `space` is in the first 128 characters of
[Unicode](unicode_usage.md#what-unicode-is) and it is encoded using a single
octet `0x20`.

> #### Note {: .info }
>
> [Unicode](unicode_usage.md#what-unicode-is) is backward compatible with ASCII,
> the encoding of the first 128 characters is the same binary value as in ASCII.

[](){: #percent_encoding } It is a major source of confusion exactly which
characters will be percent-encoded. In order to make it easier to answer this
question the library provides a utility function,
[`uri_string:allowed_characters/0 `](`uri_string:allowed_characters/0`), that
lists the allowed set of characters in each major URI component, and also in the
most important standard character sets.

```erlang
    1> uri_string:allowed_characters().
    [{scheme,
     "+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"},
    {userinfo,
     "!$%&'()*+,-.0123456789:;=ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"},
    {host,
     "!$&'()*+,-.0123456789:;=ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"},
    {ipv4,".0123456789"},
    {ipv6,".0123456789:ABCDEFabcdef"},
    {regname,
     "!$%&'()*+,-.0123456789;=ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"},
    {path,
     "!$%&'()*+,-./0123456789:;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"},
    {query,
     "!$%&'()*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"},
    {fragment,
     "!$%&'()*+,-./0123456789:;=?@ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"},
    {reserved,"!#$&'()*+,/:;=?@[]"},
    {unreserved,
     "-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz~"}]
```

If a URI component has a character that is not allowed, it will be
percent-encoded when the URI is produced:

```erlang
    2> uri_string:recompose(#{scheme => "https", host => "local#host", path => ""}).
    "https://local%23host"
```

Consuming a URI containing percent-encoded triplets can take many steps. The
following example shows how to handle an input URI that is not normalized and
contains multiple percent-encoded triplets. First, the input
[`uri_string()`](`t:uri_string:uri_string/0`) is to be parsed into a
[`uri_map()`](`t:uri_string:uri_map/0`). The parsing only splits the URI into
its components without doing any decoding:

```text
    3> uri_string:parse("http://%6C%6Fcal%23host/%F6re%26bro%20").
    #{host => "%6C%6Fcal%23host",path => "/%F6re%26bro%20",
      scheme => "http"}}
```

The input is a valid URI but how can you decode those percent-encoded octets?
You can try to normalize the input with `uri_string:normalize/1`. The normalize
operation decodes those percent-encoded triplets that correspond to a character
in the unreserved set. Normalization is a safe, idempotent operation that
converts a URI into its canonical form:

```erlang
    4> uri_string:normalize("http://%6C%6Fcal%23host/%F6re%26bro%20").
    "http://local%23host/%F6re%26bro%20"
    5> uri_string:normalize("http://%6C%6Fcal%23host/%F6re%26bro%20", [return_map]).
    #{host => "local%23host",path => "/%F6re%26bro%20",
      scheme => "http"}
```

There are still a few percent-encoded triplets left in the output. At this
point, when the URI is already parsed, it is safe to apply application specific
decoding on the remaining character triplets. Erlang/OTP provides a function,
`uri_string:percent_decode/1` for raw percent decoding that you can use on the
host and path components, or on the whole map:

```erlang
    6> uri_string:percent_decode("local%23host").
    "local#host"
    7> uri_string:percent_decode("/%F6re%26bro%20").
    {error,invalid_utf8,<<"/öre&bro ">>}
    8> uri_string:percent_decode(#{host => "local%23host",path => "/%F6re%26bro%20",
    scheme => "http"}).
    {error,{invalid,{path,{invalid_utf8,<<"/öre&bro ">>}}}}
```

The `host` was successfully decoded but the path contains at least one character
with non-UTF-8 encoding. In order to be able to decode this, you have to make
assumptions about the encoding used in these triplets. The most obvious choice
is _latin-1_, so you can try `uri_string:transcode/2`, to transcode the path to
UTF-8 and run the percent-decode operation on the transcoded string:

```erlang
    9> uri_string:transcode("/%F6re%26bro%20", [{in_encoding, latin1}]).
    "/%C3%B6re%26bro%20"
    10> uri_string:percent_decode("/%C3%B6re%26bro%20").
    "/öre&bro "
```

It is important to emphasize that it is not safe to apply
`uri_string:percent_decode/1` directly on an input URI:

```erlang
    11> uri_string:percent_decode("http://%6C%6Fcal%23host/%C3%B6re%26bro%20").
    "http://local#host/öre&bro "
    12> uri_string:parse("http://local#host/öre&bro ").
    {error,invalid_uri,":"}
```

> #### Note {: .info }
>
> Percent-encoding is implemented in `uri_string:recompose/1` and it happens
> when converting a [`uri_map()`](`t:uri_string:uri_map/0`) into a
> [`uri_string()`](`t:uri_string:uri_string/0`). Applying any percent-encoding
> directly on an input URI would not be safe just as in the case of
> `uri_string:percent_decode/1`, the output could be an invalid URI. Quoting
> functions allow users to perform raw percent encoding and decoding on
> application data which cannot be handled automatically by
> `uri_string:recompose/1`. For example in scenario when user would need to use
> '/' or sub-delimeter as data rather than delimeter in a path component.

## Normalization

Normalization is the operation of converting the input URI into a _canonical_
form and keeping the reference to the same underlying resource. The most common
application of normalization is determining whether two URIs are equivalent
without accessing their referenced resources.

Normalization has 6 distinct steps. First the input URI is parsed into an
intermediate form that can handle [Unicode](unicode_usage.md#what-unicode-is)
characters. This datatype is the [`uri_map()`](`t:uri_string:uri_map/0`), that
can hold the components of the URI in map elements of type
`t:unicode:chardata/0`. After having the intermediate form, a sequence of
normalization algorithms are applied to the individual URI components:

- **Case normalization** - Converts the `scheme` and `host` components to lower
  case as they are not case sensitive.

- **Percent-encoding normalization** - Decodes percent-encoded triplets that
  correspond to characters in the unreserved set.

- **Scheme-based normalization** - Applying rules for the schemes http, https,
  ftp, ssh, sftp and tftp.

- **Path segment normalization** - Converts the path into a canonical form.

After these steps, the intermediate data structure, an
[`uri_map()`](`t:uri_string:uri_map/0`), is fully normalized. The last step is
applying `uri_string:recompose/1` that converts the intermediate structure into
a valid canonical URI string.

Notice the order, the
[`uri_string:normalize(URIMap, [return_map])`](`uri_string:normalize/2`) that we
used many times in this user guide is a shortcut in the normalization process
returning the intermediate datastructure, and allowing us to inspect and apply
further decoding on the remaining percent-encoded triplets.

```text
    13> uri_string:normalize("hTTp://LocalHost:80/%c3%B6rebro/a/../b").
    "http://localhost/%C3%B6rebro/b"
    14> uri_string:normalize("hTTp://LocalHost:80/%c3%B6rebro/a/../b", [return_map]).
    #{host => "localhost",path => "/%C3%B6rebro/b",
      scheme => "http"}
```

## Special considerations

The current URI implementation provides support for producing and consuming
standard URIs. The API is not meant to be directly exposed in a Web browser's
address bar where users can basically enter free text. Application designers
shall implement proper heuristics to map the input into a parsable URI.
