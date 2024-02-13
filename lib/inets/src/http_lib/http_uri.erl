%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2024. All Rights Reserved.
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
-module(http_uri).
-moduledoc """
Old URI utility module, use uri_string instead

This module is deprecated since OTP 23. Use the module `m:uri_string` to
properly handle URIs, this is the recommended module since OTP 21.

## DATA TYPES

Type definitions that are related to URI:

- **`uri_part() = [byte()] | binary()`** - Syntax according to the URI
  definition in RFC 3986, for example, "http://www.erlang.org/"

For more information about URI, see
[RFC 3986](http://www.ietf.org/rfc/rfc3986.txt).
""".
-moduledoc(#{since => "OTP R15B01"}).

-export([encode/1, decode/1]).

-deprecated({encode, 1, "use uri_string:quote function instead"}).
-deprecated({decode, 1, "use uri_string:unquote function instead"}).
-removed({parse, 1, "use uri_string functions instead"}).
-removed({parse, 2, "use uri_string functions instead"}).
-removed({scheme_defaults, 0, "use uri_string functions instead"}).

-removed_type({uri, 0, "use uri_string instead"}).
-removed_type({user_info, 0, "use uri_string instead"}).
-removed_type({scheme, 0, "use uri_string instead"}).
-removed_type({host, 0, "use uri_string instead"}).
-removed_type({path, 0, "use uri_string instead"}).
-removed_type({query, 0, "use uri_string instead"}).
-removed_type({fragment, 0, "use uri_string instead"}).
-removed_type({default_scheme_port_number, 0, "use uri_string instead"}).

%%%=========================================================================
%%%  API
%%%=========================================================================
reserved() ->
    sets:from_list([$;, $:, $@, $&, $=, $+, $,, $/, $?,
            $#, $[, $], $<, $>, $\", ${, $}, $|, %"
			       $\\, $', $^, $%, $ ]).

-doc """
encode(DecodedPart) -> EncodedPart

> #### Warning {: .warning }
>
> Do not use will be removed use `uri_string:quote/1` instead

Performes prrcent encoding.

[](){: #decode }
""".
-doc(#{since => <<"OTP R15B01">>}).
encode(URI) when is_list(URI) ->
    Reserved = reserved(), 
    lists:append([uri_encode(Char, Reserved) || Char <- URI]);
encode(URI) when is_binary(URI) ->
    Reserved = reserved(),
    << <<(uri_encode_binary(Char, Reserved))/binary>> || <<Char>> <= URI >>.

-doc """
decode(EncodedPart) -> DecodePart

> #### Warning {: .warning }
>
> Do not use will be removed use `uri_string:unquote/1` instead

Decodes a possibly percent encoded URI part
""".
-doc(#{since => <<"OTP R15B01">>}).
decode(String) when is_list(String) ->
    do_decode(String);
decode(String) when is_binary(String) ->
    do_decode_binary(String).

do_decode([$%,Hex1,Hex2|Rest]) ->
    [hex2dec(Hex1)*16+hex2dec(Hex2)|do_decode(Rest)];
do_decode([First|Rest]) ->
    [First|do_decode(Rest)];
do_decode([]) ->
    [].

do_decode_binary(<<$%, Hex:2/binary, Rest/bits>>) ->
    <<(binary_to_integer(Hex, 16)), (do_decode_binary(Rest))/binary>>;
do_decode_binary(<<First:1/binary, Rest/bits>>) ->
    <<First/binary, (do_decode_binary(Rest))/binary>>;
do_decode_binary(<<>>) ->
    <<>>.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%% In this version of the function, we no longer need 
%% the Scheme argument, but just in case...
uri_encode(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
	true ->
	    [ $% | http_util:integer_to_hexlist(Char)];
	false ->
	    [Char]
    end.

uri_encode_binary(Char, Reserved) ->
    case sets:is_element(Char, Reserved) of
        true ->
            << $%, (integer_to_binary(Char, 16))/binary >>;
        false ->
            <<Char>>
    end.

hex2dec(X) when (X>=$0) andalso (X=<$9) -> X-$0;
hex2dec(X) when (X>=$A) andalso (X=<$F) -> X-$A+10;
hex2dec(X) when (X>=$a) andalso (X=<$f) -> X-$a+10.
