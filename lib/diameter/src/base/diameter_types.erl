%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2024. All Rights Reserved.
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

-module(diameter_types).
-moduledoc false.

%%
%% Encode/decode of RFC 3588 Data Formats, Basic (section 4.2) and
%% Derived (section 4.3).
%%

%% Basic types.
-export(['OctetString'/3,
         'Integer32'/3,
         'Integer64'/3,
         'Unsigned32'/3,
         'Unsigned64'/3,
         'Float32'/3,
         'Float64'/3]).

%% Derived types.
-export(['Address'/3,
         'Time'/3,
         'UTF8String'/3,
         'DiameterIdentity'/3,
         'DiameterURI'/3,
         'IPFilterRule'/3,
         'QoSFilterRule'/3]).

-include_lib("diameter/include/diameter.hrl").

-define(UINT(N,X), ((0 =< X) andalso (X < 1 bsl N))).
-define(SINT(N,X), ((-1*(1 bsl (N-1)) < X) andalso (X < 1 bsl (N-1)))).

%% The Grouped and Enumerated types are dealt with directly in
%% generated decode modules by way of diameter_gen.hrl and
%% diameter_codec.erl. Padding and the setting of Length and other
%% fields are also dealt with there.

%% 3588:
%%
%%   DIAMETER_INVALID_AVP_LENGTH        5014
%%      The request contained an AVP with an invalid length.  A Diameter
%%      message indicating this error MUST include the offending AVPs
%%      within a Failed-AVP AVP.
%%
-define(INVALID_LENGTH(Bitstr), erlang:error({'DIAMETER', 5014, Bitstr})).

%% -------------------------------------------------------------------------
%% 3588, 4.2.  Basic AVP Data Formats
%%
%%    The Data field is zero or more octets and contains information
%%    specific to the Attribute.  The format and length of the Data field
%%    is determined by the AVP Code and AVP Length fields.  The format of
%%    the Data field MUST be one of the following base data types or a data
%%    type derived from the base data types.  In the event that a new Basic
%%    AVP Data Format is needed, a new version of this RFC must be created.
%% --------------------

'OctetString'(decode, Bin, #{string_decode := true})
  when is_binary(Bin) ->
    binary_to_list(Bin);

'OctetString'(decode, Bin, _)
  when is_binary(Bin) ->
    Bin;

'OctetString'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'OctetString'(encode, zero, _) ->
    <<>>;

'OctetString'(encode, Str, _) ->
    iolist_to_binary(Str).

%% --------------------

'Integer32'(decode, <<X:32/signed>>, _) ->
    X;

'Integer32'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Integer32'(encode, zero, _) ->
    <<0:32/signed>>;

'Integer32'(encode, I, _)
  when ?SINT(32,I) ->
    <<I:32/signed>>.

%% --------------------

'Integer64'(decode, <<X:64/signed>>, _) ->
    X;

'Integer64'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Integer64'(encode, zero, _) ->
    <<0:64/signed>>;

'Integer64'(encode, I, _)
  when ?SINT(64,I) ->
    <<I:64/signed>>.

%% --------------------

'Unsigned32'(decode, <<X:32>>, _) ->
    X;

'Unsigned32'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Unsigned32'(encode, zero, _) ->
    <<0:32>>;

'Unsigned32'(encode, I, _)
  when ?UINT(32,I) ->
    <<I:32>>.

%% --------------------

'Unsigned64'(decode, <<X:64>>, _) ->
    X;

'Unsigned64'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Unsigned64'(encode, zero, _) ->
    <<0:64>>;

'Unsigned64'(encode, I, _)
  when ?UINT(64,I) ->
    <<I:64>>.

%% --------------------

%% Decent summaries of the IEEE floating point formats can be
%% found at http://en.wikipedia.org/wiki/IEEE_754-1985 and
%% http://www.psc.edu/general/software/packages/ieee/ieee.php.
%%
%% That the bit syntax uses these formats isn't well documented but
%% this does indeed appear to be the case. However, the bit syntax
%% only encodes numeric values, not the standard's (signed) infinity
%% or NaN. It also encodes any large value as 'infinity', never 'NaN'.
%% Treat these equivalently on decode for this reason.
%%
%% An alternative would be to decode infinity/NaN to the largest
%% possible float but could likely lead to misleading results if
%% arithmetic is performed on the decoded value. Better to be explicit
%% that precision has been lost.

'Float32'(decode, <<S:1, 255:8, _:23>>, _) ->
    choose(S, infinity, '-infinity');

'Float32'(decode, <<X:32/float>>, _) ->
    X;

'Float32'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Float32'(encode, zero, _) ->
    <<0.0:32/float>>;

'Float32'(encode, infinity, _) ->
    <<0:1, 255:8, 0:23>>;

'Float32'(encode, '-infinity', _) ->
    <<1:1, 255:8, 0:23>>;

'Float32'(encode, X, _)
  when is_float(X) ->
    <<X:32/float>>.
%% Note that this could also encode infinity/-infinity for large
%% (signed) numeric values. Note also that precision is lost just in
%% using the floating point syntax. For example:
%%
%% 1> B = <<3.14159:32/float>>.
%% <<64,73,15,208>>
%% 2> <<F:32/float>> = B.
%% <<64,73,15,208>>
%% 3> F.
%% 3.141590118408203
%%
%% (The 64 bit type does better.)

%% --------------------

%% The 64 bit format is entirely analogous to the 32 bit format.

'Float64'(decode, <<S:1, 2047:11, _:52>>, _) ->
    choose(S, infinity, '-infinity');

'Float64'(decode, <<X:64/float>>, _) ->
    X;

'Float64'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Float64'(encode, infinity, _) ->
    <<0:1, 2047:11, 0:52>>;

'Float64'(encode, '-infinity', _) ->
    <<1:1, 2047:11, 0:52>>;

'Float64'(encode, zero, _) ->
    <<0.0:64/float>>;

'Float64'(encode, X, _)
  when is_float(X) ->
    <<X:64/float>>.

%% -------------------------------------------------------------------------
%% 3588, 4.3.  Derived AVP Data Formats
%%
%%    In addition to using the Basic AVP Data Formats, applications may
%%    define data formats derived from the Basic AVP Data Formats.  An
%%    application that defines new AVP Derived Data Formats MUST include
%%    them in a section entitled "AVP Derived Data Formats", using the same
%%    format as the definitions below.  Each new definition must be either
%%    defined or listed with a reference to the RFC that defines the
%%    format.
%% --------------------

'Address'(encode, zero, _) ->
    <<0:48>>;

'Address'(decode, <<A:16, B/binary>>, _)
  when 1 == A,  4 == byte_size(B);
       2 == A, 16 == byte_size(B) ->
    list_to_tuple([N || <<N:A/unit:8>> <= B]);

%% Bytes for non-IP address types are left for the user to interpret.
'Address'(decode, <<A:16, B/binary>>, _)
  when 2 < A, A < 65535 ->
    {A, B};

'Address'(decode, B, _) ->
    ?INVALID_LENGTH(B);

%% Allow IP address types as both 2- and 4/8-tuples.
'Address'(encode, {A, B}, _)
  when 0 < A, A < 65535 ->
    <<A:16, B/binary>>;

'Address'(encode, T, _) ->
    Ns = tuple_to_list(diameter_lib:ipaddr(T)),  %% length 4 or 8
    A = length(Ns) div 4,                        %% 1 or 2
    B = << <<N:A/unit:8>> || N <- Ns >>,
    <<A:16, B/binary>>.

%% --------------------

%% A DiameterIdentity is a FQDN as definined in RFC 1035, which is at
%% least one character.

'DiameterIdentity'(encode, zero, _) ->
    <<0>>;

'DiameterIdentity'(encode = M, X, Opts) ->
    <<_,_/binary>> = 'OctetString'(M, X, Opts);

'DiameterIdentity'(decode = M, <<_,_/binary>> = X, Opts) ->
    'OctetString'(M, X, Opts);

'DiameterIdentity'(decode, X, _) ->
    ?INVALID_LENGTH(X).

%% --------------------

'DiameterURI'(decode, Bin, Opts)
  when is_binary(Bin) ->
    scan_uri(Bin, Opts);

'DiameterURI'(decode, B, _) ->
    ?INVALID_LENGTH(B);

%% The minimal DiameterURI is "aaa://x", 7 characters.
'DiameterURI'(encode, zero, _) ->
    <<0:7/unit:8>>;

'DiameterURI'(encode,
              #diameter_uri{type = Type,
                            fqdn = DN,
                            port = PN,
                            transport = T,
                            protocol = P},
              _)
  when (Type == 'aaa' orelse Type == 'aaas'),
       is_integer(PN),
       0 =< PN,
       (T == tcp orelse T == sctp orelse T == udp),
       (P == diameter orelse P == radius orelse P == 'tacacs+'),
       (P /= diameter orelse T /= udp) ->
    iolist_to_binary([atom_to_list(Type), "://", DN,
                      ":", integer_to_list(PN),
                      ";transport=", atom_to_list(T),
                      ";protocol=", atom_to_list(P)]);
%% Don't omit defaults since they're dependent on whether RFC 3588 or
%% 6733 is being followed. For one, we don't know this at encode; for
%% two (more importantly), we don't know how the peer will interpret
%% defaults, so it's best to be explicit. Interpret defaults on decode
%% since there's no choice.

'DiameterURI'(encode, Str, Opts) ->
    Bin = iolist_to_binary(Str),
    #diameter_uri{} = scan_uri(Bin, Opts),  %% assert
    Bin.

%% --------------------

%% This minimal rule is "deny in 0 from 0.0.0.0 to 0.0.0.0", 33 characters.
'IPFilterRule'(encode, zero, _) ->
    <<0:33/unit:8>>;

'IPFilterRule'(M, X, Opts) ->
    'OctetString'(M, X, Opts).

%% --------------------

%% This minimal rule is the same as for an IPFilterRule.
'QoSFilterRule'(encode, zero, _) ->
    <<0:33/unit:8>>;

'QoSFilterRule'(M, X, Opts) ->
    'OctetString'(M, X, Opts).

%% --------------------

'UTF8String'(decode, Bin, #{string_decode := true})
  when is_binary(Bin) ->
    %% assert list return
    tl([0|_] = unicode:characters_to_list([0, Bin]));

'UTF8String'(decode, Bin, _)
  when is_binary(Bin) ->
    <<_/binary>> = unicode:characters_to_binary(Bin);

'UTF8String'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'UTF8String'(encode, zero, _) ->
    <<>>;

'UTF8String'(encode, S, _) ->
    <<_/binary>> = unicode:characters_to_binary(S).   %% assert binary return

%% --------------------

%% RFC 3588, 4.3:
%%
%%    Time
%%       The Time format is derived from the OctetString AVP Base Format.
%%       The string MUST contain four octets, in the same format as the
%%       first four bytes are in the NTP timestamp format.  The NTP
%%       Timestamp format is defined in chapter 3 of [SNTP].
%%
%%       This represents the number of seconds since 0h on 1 January 1900
%%       with respect to the Coordinated Universal Time (UTC).
%%
%%       On 6h 28m 16s UTC, 7 February 2036 the time value will overflow.
%%       SNTP [SNTP] describes a procedure to extend the time to 2104.
%%       This procedure MUST be supported by all DIAMETER nodes.

%% RFC 2030, 3:
%%
%%       As the NTP timestamp format has been in use for the last 17 years,
%%       it remains a possibility that it will be in use 40 years from now
%%       when the seconds field overflows. As it is probably inappropriate
%%       to archive NTP timestamps before bit 0 was set in 1968, a
%%       convenient way to extend the useful life of NTP timestamps is the
%%       following convention: If bit 0 is set, the UTC time is in the
%%       range 1968-2036 and UTC time is reckoned from 0h 0m 0s UTC on 1
%%       January 1900. If bit 0 is not set, the time is in the range 2036-
%%       2104 and UTC time is reckoned from 6h 28m 16s UTC on 7 February
%%       2036. Note that when calculating the correspondence, 2000 is not a
%%       leap year. Note also that leap seconds are not counted in the
%%       reckoning.
%%
%% The statement regarding year 2000 is wrong: errata id 518 at
%% http://www.rfc-editor.org/errata_search.php?rfc=2030 notes this.

-define(TIME_1900, 59958230400).  %% {{1900,1,1},{0,0,0}}
-define(TIME_2036, 64253197696).  %% {{2036,2,7},{6,28,16}}
%% TIME_2036 = TIME_1900 + (1 bsl 32)

%% Time maps [0, 1 bsl 31) onto [TIME_1900 + 1 bsl 31, TIME_2036 + 1 bsl 31)
%% by taking integers with the high-order bit set relative to TIME_1900
%% and those without relative to TIME_2036. This corresponds to the
%% following dates.
-define(TIME_MIN, {{1968,1,20},{3,14,8}}).  %% TIME_1900 + 1 bsl 31
-define(TIME_MAX, {{2104,2,26},{9,42,24}}). %% TIME_2036 + 1 bsl 31

'Time'(decode, <<Time:32>>, _) ->
    Offset = msb(1 == Time bsr 31),
    calendar:gregorian_seconds_to_datetime(Time + Offset);

'Time'(decode, B, _) ->
    ?INVALID_LENGTH(B);

'Time'(encode, {{_Y,_M,_D},{_HH,_MM,_SS}} = Datetime, _)
  when ?TIME_MIN =< Datetime, Datetime < ?TIME_MAX ->
    S = calendar:datetime_to_gregorian_seconds(Datetime),
    T = S - msb(S < ?TIME_2036),
    0 = T bsr 32,  %% sanity check
    <<T:32>>;

'Time'(encode, zero, _) ->
    <<0:32>>.

%% ===========================================================================
%% ===========================================================================

choose(0, X, _) -> X;
choose(1, _, X) -> X.

msb(true)  -> ?TIME_1900;
msb(false) -> ?TIME_2036.

%% RFC 3588, 4.3:
%%
%%       The DiameterURI MUST follow the Uniform Resource Identifiers (URI)
%%       syntax [URI] rules specified below:
%%
%%       "aaa://" FQDN [ port ] [ transport ] [ protocol ]
%%
%%                       ; No transport security
%%
%%       "aaas://" FQDN [ port ] [ transport ] [ protocol ]
%%
%%                       ; Transport security used
%%
%%       FQDN               = Fully Qualified Host Name
%%
%%       port               = ":" 1*DIGIT
%%
%%                       ; One of the ports used to listen for
%%                       ; incoming connections.
%%                       ; If absent,
%%                       ; the default Diameter port (3868) is
%%                       ; assumed.
%%
%%       transport          = ";transport=" transport-protocol
%%
%%                       ; One of the transports used to listen
%%                       ; for incoming connections.  If absent,
%%                       ; the default SCTP [SCTP] protocol is
%%                       ; assumed.  UDP MUST NOT be used when
%%                       ; the aaa-protocol field is set to
%%                       ; diameter.
%%
%%       transport-protocol = ( "tcp" / "sctp" / "udp" )
%%
%%       protocol           = ";protocol=" aaa-protocol
%%
%%                       ; If absent, the default AAA protocol
%%                       ; is diameter.
%%
%%       aaa-protocol       = ( "diameter" / "radius" / "tacacs+" )

%% RFC 6733, 4.3.1, changes the defaults:
%%
%%       "aaa://" FQDN [ port ] [ transport ] [ protocol ]
%%
%%                       ; No transport security
%%
%%       "aaas://" FQDN [ port ] [ transport ] [ protocol ]
%%
%%                       ; Transport security used
%%
%%       FQDN               = < Fully Qualified Domain Name >
%%
%%       port               = ":" 1*DIGIT
%%
%%                       ; One of the ports used to listen for
%%                       ; incoming connections.
%%                       ; If absent, the default Diameter port
%%                       ; (3868) is assumed if no transport
%%                       ; security is used and port 5658 when
%%                       ; transport security (TLS/TCP and DTLS/SCTP)
%%                       ; is used.
%%
%%       transport          = ";transport=" transport-protocol
%%
%%                       ; One of the transports used to listen
%%                       ; for incoming connections.  If absent,
%%                       ; the default protocol is assumed to be TCP.
%%                       ; UDP MUST NOT be used when the aaa-protocol
%%                       ; field is set to diameter.
%%
%%       transport-protocol = ( "tcp" / "sctp" / "udp" )
%%
%%       protocol           = ";protocol=" aaa-protocol
%%
%%                       ; If absent, the default AAA protocol
%%                       ; is Diameter.
%%
%%       aaa-protocol       = ( "diameter" / "radius" / "tacacs+" )

scan_uri(Bin, Opts) ->
    RE = "^(aaas?)://"
         "([-a-zA-Z0-9.]{1,255})"
         "(:0{0,5}([0-9]{1,5}))?"
         "(;transport=(tcp|sctp|udp))?"
         "(;protocol=(diameter|radius|tacacs\\+))?$",
    %% A port number is 16-bit, so an arbitrary number of digits is
    %% just a vulnerability, but provide a little slack with leading
    %% zeros in a port number just because the regexp was previously
    %% [0-9]+ and it's not inconceivable that a value might be padded.
    %% Don't fantasize about this padding being more than the number
    %% of digits in the port number proper.
    %%
    %% Similarly, a FQDN can't be arbitrarily long: at most 255
    %% octets.
    {match, [A, DN, PN, T, P]} = re:run(Bin,
                                        RE,
                                        [{capture, [1,2,4,6,8], binary}]),
    Type = to_atom(A),
    #diameter_uri{type = Type,
                  fqdn = 'OctetString'(decode, DN, Opts),
                  port = portnr(PN, Type, Opts),
                  transport = transport(T, Opts),
                  protocol = to_atom(P, diameter)}.

%% Choose defaults based on the RFC, since 6733 has changed them.

portnr(<<>>, aaa, #{rfc := 6733}) ->
    3868;
portnr(<<>>, aaas, #{rfc := 6733}) ->
    5868;
portnr(<<>>, _, _) ->
    3868;
portnr(B, _, _) ->
    binary_to_integer(B).

transport(<<>>, #{rfc := 6733}) ->
    tcp;
transport(<<>>, #{rfc := 3588}) ->
    sctp;
transport(<<>>, _) ->
    tcp;
transport(B, _) ->
    to_atom(B).

to_atom(<<>>, A) ->
    A;
to_atom(B, _) ->
    to_atom(B).

to_atom(B) ->
    binary_to_atom(B, latin1).
