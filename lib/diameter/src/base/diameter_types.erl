%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(diameter_types).

%%
%% Encode/decode of RFC 3588 Data Formats, Basic (section 4.2) and
%% Derived (section 4.3).
%%

%% Basic types.
-export(['OctetString'/2,
         'Integer32'/2,
         'Integer64'/2,
         'Unsigned32'/2,
         'Unsigned64'/2,
         'Float32'/2,
         'Float64'/2]).

%% Derived types.
-export(['Address'/2,
         'Time'/2,
         'UTF8String'/2,
         'DiameterIdentity'/2,
         'DiameterURI'/2,
         'IPFilterRule'/2,
         'QoSFilterRule'/2]).

%% Functions taking the AVP name in question as second parameter.
-export(['OctetString'/3,
         'Integer32'/3,
         'Integer64'/3,
         'Unsigned32'/3,
         'Unsigned64'/3,
         'Float32'/3,
         'Float64'/3,
         'Address'/3,
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

'OctetString'(decode, Bin)
  when is_binary(Bin) ->
    case diameter_codec:getopt(string_decode) of
        true ->
            binary_to_list(Bin);
        false ->
            Bin
    end;

'OctetString'(decode, B) ->
    ?INVALID_LENGTH(B);

'OctetString'(encode = M, zero) ->
    'OctetString'(M, []);

'OctetString'(encode, Str) ->
    iolist_to_binary(Str).

%% --------------------

'Integer32'(decode, <<X:32/signed>>) ->
    X;

'Integer32'(decode, B) ->
    ?INVALID_LENGTH(B);

'Integer32'(encode = M, zero) ->
    'Integer32'(M, 0);

'Integer32'(encode, I)
  when ?SINT(32,I) ->
    <<I:32/signed>>.

%% --------------------

'Integer64'(decode, <<X:64/signed>>) ->
    X;

'Integer64'(decode, B) ->
    ?INVALID_LENGTH(B);

'Integer64'(encode = M, zero) ->
    'Integer64'(M, 0);

'Integer64'(encode, I)
  when ?SINT(64,I) ->
    <<I:64/signed>>.

%% --------------------

'Unsigned32'(decode, <<X:32>>) ->
    X;

'Unsigned32'(decode, B) ->
    ?INVALID_LENGTH(B);

'Unsigned32'(encode = M, zero) ->
    'Unsigned32'(M, 0);

'Unsigned32'(encode, I)
  when ?UINT(32,I) ->
    <<I:32>>.

%% --------------------

'Unsigned64'(decode, <<X:64>>) ->
    X;

'Unsigned64'(decode, B) ->
    ?INVALID_LENGTH(B);

'Unsigned64'(encode = M, zero) ->
    'Unsigned64'(M, 0);

'Unsigned64'(encode, I)
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

'Float32'(decode, <<S:1, 255:8, _:23>>) ->
    choose(S, infinity, '-infinity');

'Float32'(decode, <<X:32/float>>) ->
    X;

'Float32'(decode, B) ->
    ?INVALID_LENGTH(B);

'Float32'(encode = M, zero) ->
    'Float32'(M, 0.0);

'Float32'(encode, infinity) ->
    <<0:1, 255:8, 0:23>>;

'Float32'(encode, '-infinity') ->
    <<1:1, 255:8, 0:23>>;

'Float32'(encode, X)
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

'Float64'(decode, <<S:1, 2047:11, _:52>>) ->
    choose(S, infinity, '-infinity');

'Float64'(decode, <<X:64/float>>) ->
    X;

'Float64'(decode, B) ->
    ?INVALID_LENGTH(B);

'Float64'(encode, infinity) ->
    <<0:1, 2047:11, 0:52>>;

'Float64'(encode, '-infinity') ->
    <<1:1, 2047:11, 0:52>>;

'Float64'(encode = M, zero) ->
    'Float64'(M, 0.0);

'Float64'(encode, X)
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

'Address'(encode, zero) ->
    <<0:48>>;

'Address'(decode, <<A:16, B/binary>>)
  when 1 == A,  4 == size(B);
       2 == A, 16 == size(B) ->
    list_to_tuple([N || <<N:A/unit:8>> <= B]);

'Address'(decode, B) ->
    ?INVALID_LENGTH(B);

'Address'(encode, T) ->
    Ns = tuple_to_list(diameter_lib:ipaddr(T)),  %% length 4 or 8
    A = length(Ns) div 4,                        %% 1 or 2
    B = << <<N:A/unit:8>> || N <- Ns >>,
    <<A:16, B/binary>>.

%% --------------------

%% A DiameterIdentity is a FQDN as definined in RFC 1035, which is at
%% least one character.

'DiameterIdentity'(encode = M, zero) ->
    'OctetString'(M, [0]);

'DiameterIdentity'(encode = M, X) ->
    <<_,_/binary>> = 'OctetString'(M, X);

'DiameterIdentity'(decode = M, <<_,_/binary>> = X) ->
    'OctetString'(M, X);

'DiameterIdentity'(decode, X) ->
    ?INVALID_LENGTH(X).

%% --------------------

'DiameterURI'(decode, Bin)
  when is_binary(Bin) ->
    scan_uri(Bin);

'DiameterURI'(decode, B) ->
    ?INVALID_LENGTH(B);

%% The minimal DiameterURI is "aaa://x", 7 characters.
'DiameterURI'(encode = M, zero) ->
    'OctetString'(M, lists:duplicate(0,7));

'DiameterURI'(encode, #diameter_uri{type = Type,
                                    fqdn = DN,
                                    port = PN,
                                    transport = T,
                                    protocol = P})
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

'DiameterURI'(encode, Str) ->
    Bin = iolist_to_binary(Str),
    #diameter_uri{} = scan_uri(Bin),  %% assert
    Bin.

%% --------------------

%% This minimal rule is "deny in 0 from 0.0.0.0 to 0.0.0.0", 33 characters.
'IPFilterRule'(encode = M, zero) ->
    'OctetString'(M, lists:duplicate(0,33));

'IPFilterRule'(M, X) ->
    'OctetString'(M, X).

%% --------------------

%% This minimal rule is the same as for an IPFilterRule.
'QoSFilterRule'(encode = M, zero = X) ->
    'IPFilterRule'(M, X);

'QoSFilterRule'(M, X) ->
    'OctetString'(M, X).

%% --------------------

'UTF8String'(decode, Bin)
  when is_binary(Bin) ->
    case diameter_codec:getopt(string_decode) of
        true ->
            %% assert list return
            tl([0|_] = unicode:characters_to_list([0, Bin]));
        false ->
            <<_/binary>> = unicode:characters_to_binary(Bin)
    end;

'UTF8String'(decode, B) ->
    ?INVALID_LENGTH(B);

'UTF8String'(encode = M, zero) ->
    'UTF8String'(M, []);

'UTF8String'(encode, S) ->
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

'Time'(decode, <<Time:32>>) ->
    Offset = msb(1 == Time bsr 31),
    calendar:gregorian_seconds_to_datetime(Time + Offset);

'Time'(decode, B) ->
    ?INVALID_LENGTH(B);

'Time'(encode, {{_Y,_M,_D},{_HH,_MM,_SS}} = Datetime)
  when ?TIME_MIN =< Datetime, Datetime < ?TIME_MAX ->
    S = calendar:datetime_to_gregorian_seconds(Datetime),
    T = S - msb(S < ?TIME_2036),
    0 = T bsr 32,  %% sanity check
    <<T:32>>;

'Time'(encode, zero) ->
    <<0:32>>.

%% -------------------------------------------------------------------------

'OctetString'(M, _, Data) ->
    'OctetString'(M, Data).

'Integer32'(M, _, Data) ->
    'Integer32'(M, Data).

'Integer64'(M, _, Data) ->
    'Integer64'(M, Data).

'Unsigned32'(M, _, Data) ->
    'Unsigned32'(M, Data).

'Unsigned64'(M, _, Data) ->
    'Unsigned64'(M, Data).

'Float32'(M, _, Data) ->
    'Float32'(M, Data).

'Float64'(M, _, Data) ->
    'Float64'(M, Data).

'Address'(M, _, Data) ->
    'Address'(M, Data).

'Time'(M, _, Data) ->
    'Time'(M, Data).

'UTF8String'(M, _, Data) ->
    'UTF8String'(M, Data).

'DiameterIdentity'(M, _, Data) ->
    'DiameterIdentity'(M, Data).

'DiameterURI'(M, _, Data) ->
    'DiameterURI'(M, Data).

'IPFilterRule'(M, _, Data) ->
    'IPFilterRule'(M, Data).

'QoSFilterRule'(M, _, Data) ->
    'QoSFilterRule'(M, Data).

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

scan_uri(Bin) ->
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
    {PN0, T0} = defaults(diameter_codec:getopt(rfc), Type),
    PortNr = to_int(PN, PN0),
    0 = PortNr bsr 16,  %% assert
    #diameter_uri{type = Type,
                  fqdn = 'OctetString'(decode, DN),
                  port = PortNr,
                  transport = to_atom(T, T0),
                  protocol = to_atom(P, diameter)}.

%% Choose defaults based on the RFC, since 6733 has changed them.
defaults(3588, _) ->
    {3868, sctp};
defaults(6733, aaa) ->
    {3868, tcp};
defaults(6733, aaas) ->
    {5658, tcp}.

to_int(<<>>, N) ->
    N;
to_int(B, _) ->
    binary_to_integer(B).

to_atom(<<>>, A) ->
    A;
to_atom(B, _) ->
    to_atom(B).

to_atom(B) ->
    binary_to_atom(B, latin1).
